
/*
 * Copyright (C) Roman Arutyunyan
 */


#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>
#include <ngx_cycle.h>
#include <nginx.h>
#include "ngx_rtmp.h"
#include "ngx_rtmp_version.h"
#include "ngx_rtmp_live_module.h"
#include "ngx_rtmp_play_module.h"
#include "ngx_rtmp_codec_module.h"
#include "ngx_rtmp_stats.h"



#define DEBUG_LEVEL NGX_LOG_DEBUG
#define DBG(fmt, args...) ngx_log_error(DEBUG_LEVEL, ngx_cycle->log, 0, "JUJU | " fmt, ##args)

int ngx_ipc_broadcast_alert(ngx_str_t *name, ngx_str_t *data);
static ngx_int_t ngx_rtmp_stat_handler(ngx_http_request_t *r);
static void ngx_rtmp_stat_tick_handler(ngx_event_t *ev);

static ngx_int_t ngx_rtmp_stat_init_process(ngx_cycle_t *cycle);
static char *ngx_rtmp_stat(ngx_conf_t *cf, ngx_command_t *cmd, void *conf);
static ngx_int_t ngx_rtmp_stat_postconfiguration(ngx_conf_t *cf);
static void * ngx_rtmp_stat_create_loc_conf(ngx_conf_t *cf);
static char * ngx_rtmp_stat_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child);


static time_t                       start_time;


#define NGX_RTMP_STAT_ALL           0xff
#define NGX_RTMP_STAT_GLOBAL        0x01
#define NGX_RTMP_STAT_LIVE          0x02
#define NGX_RTMP_STAT_CLIENTS       0x04
#define NGX_RTMP_STAT_PLAY          0x08

#define NGX_RTMP_STAT_BUFSIZE       256

/*
 * global: stat-{bufs-{total,free,used}, total bytes in/out, bw in/out} - cscf
*/

ngx_rtmp_stat_request_t ngx_rtmp_stat_request_map[NGX_RTMP_STAT_MAX_REQUESTS];


typedef struct {
    ngx_uint_t                      stat;
    ngx_str_t                       stylesheet;
    ngx_int_t                       state;
} ngx_rtmp_stat_loc_conf_t;


static ngx_conf_bitmask_t           ngx_rtmp_stat_masks[] = {
    { ngx_string("all"),            NGX_RTMP_STAT_ALL           },
    { ngx_string("global"),         NGX_RTMP_STAT_GLOBAL        },
    { ngx_string("live"),           NGX_RTMP_STAT_LIVE          },
    { ngx_string("clients"),        NGX_RTMP_STAT_CLIENTS       },
    { ngx_null_string,              0 }
};


static ngx_command_t  ngx_rtmp_stat_commands[] = {

    { ngx_string("rtmp_stat"),
        NGX_HTTP_MAIN_CONF|NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_CONF_1MORE,
        ngx_rtmp_stat,
        NGX_HTTP_LOC_CONF_OFFSET,
        offsetof(ngx_rtmp_stat_loc_conf_t, stat),
        ngx_rtmp_stat_masks },

    { ngx_string("rtmp_stat_stylesheet"),
        NGX_HTTP_MAIN_CONF|NGX_HTTP_SRV_CONF|NGX_HTTP_LOC_CONF|NGX_CONF_TAKE1,
        ngx_conf_set_str_slot,
        NGX_HTTP_LOC_CONF_OFFSET,
        offsetof(ngx_rtmp_stat_loc_conf_t, stylesheet),
        NULL },

    ngx_null_command
};


static ngx_http_module_t  ngx_rtmp_stat_module_ctx = {
    NULL,                               /* preconfiguration */
    ngx_rtmp_stat_postconfiguration,    /* postconfiguration */

    NULL,                               /* create main configuration */
    NULL,                               /* init main configuration */

    NULL,                               /* create server configuration */
    NULL,                               /* merge server configuration */

    ngx_rtmp_stat_create_loc_conf,      /* create location configuration */
    ngx_rtmp_stat_merge_loc_conf,       /* merge location configuration */
};


ngx_module_t  ngx_rtmp_stat_module = {
    NGX_MODULE_V1,
    &ngx_rtmp_stat_module_ctx,          /* module context */
    ngx_rtmp_stat_commands,             /* module directives */
    NGX_HTTP_MODULE,                    /* module type */
    NULL,                               /* init master */
    NULL,                               /* init module */
    ngx_rtmp_stat_init_process,         /* init process */
    NULL,                               /* init thread */
    NULL,                               /* exit thread */
    NULL,                               /* exit process */
    NULL,                               /* exit master */
    NGX_MODULE_V1_PADDING
};

void
ngx_rtmp_stat_clear_stat_request(ngx_int_t conn_id) {
    int                        i;
    ngx_rtmp_stat_request_t   *r = NULL;

    for (i = 0; i < NGX_RTMP_STAT_MAX_REQUESTS; i++) {
        if (ngx_rtmp_stat_request_map[i].conn_id == conn_id) {
            r = &ngx_rtmp_stat_request_map[i];
            break;
        }
    }
    if (r == NULL) {
        return;
    }

    r->active = 0;
    r->conn_id = -1;
    for (i = 0; i < r->got_responses; i++) {
        ngx_free(r->responses[i]->data);
        ngx_free(r->responses[i]);
    }
    ngx_free(r->responses);
    r->got_responses = 0;
    r->responses = NULL;
    r->r = NULL;
    r->total_responses = 0;
    r->error = 0;
}

void
ngx_rtmp_stat_handle_stat(ngx_int_t conn_id, ngx_str_t *data) {
    int                                 i;
    ngx_rtmp_stat_request_t            *r = NULL;
    ngx_str_t                          *s;

    DBG("got a stat from con_id=%d, stat.len:%ui", conn_id, data->len);

    for (i = 0; i < NGX_RTMP_STAT_MAX_REQUESTS; i++) {
        if (ngx_rtmp_stat_request_map[i].active && ngx_rtmp_stat_request_map[i].conn_id == conn_id) {
            r = &ngx_rtmp_stat_request_map[i];
            break;
        }
    }

    if (r == NULL) {
        DBG("no active request with conn_id=%d was found", conn_id);
        r->error = NGX_ERROR;
        return;
    }

    s = ngx_alloc(sizeof(ngx_str_t), r->r->connection->log);
    if (s == NULL) {
        DBG("can't allocate %ui bytes for ngx_str_t", sizeof(ngx_str_t));
        r->error = NGX_ERROR;
        return;// TODO: handle error
    }
    s->data = ngx_alloc(sizeof(u_char) * data->len, r->r->connection->log);
    if (s->data == NULL) {
        r->error = NGX_ERROR;
        DBG("can't allocate %ui bytes to store worker stats", data->len);
        return;// TODO: handle error
    }
    ngx_memcpy(s->data, data->data, data->len);
    s->len = data->len;

    r->responses[r->got_responses++] = s;
    DBG("got %d/%d responses for connection %d", r->got_responses, r->total_responses, conn_id);
}

ngx_rtmp_stat_request_ctx_t *
ngx_rtmp_stat_create_request_ctx(ngx_http_request_t *r) {
    ngx_rtmp_stat_request_ctx_t  *ctx;

    ctx = ngx_pcalloc(r->pool, sizeof(ngx_rtmp_stat_request_ctx_t));
    if (ctx == NULL) {
        return NULL;
    }

    ctx->timer_begin  = (ngx_timeofday())->msec;
    ctx->state        = state_rtmp_stat_start;
    ctx->tick.handler = ngx_rtmp_stat_tick_handler;
    ctx->tick.data    = r;
    ctx->tick.log     = r->connection->log;

    return ctx;
}

static ngx_int_t
ngx_rtmp_stat_init_process(ngx_cycle_t *cycle)
{
    /*
     * HTTP process initializer is called
     * after event module initializer
     * so we can run posted events here
     */

    ngx_event_process_posted(cycle, &ngx_rtmp_init_queue);

    return NGX_OK;
}


/* ngx_escape_html does not escape characters out of ASCII range
 * which are bad for xslt */

static void *
ngx_rtmp_stat_escape(ngx_pool_t *pool, void *data, size_t len)
{
    u_char *p, *np;
    void   *new_data;
    size_t  n;

    p = data;

    for (n = 0; n < len; ++n, ++p) {
        if (*p < 0x20 || *p >= 0x7f) {
            break;
        }
    }

    if (n == len) {
        return data;
    }

    new_data = ngx_palloc(pool, len);
    if (new_data == NULL) {
        return NULL;
    }

    p  = data;
    np = new_data;

    for (n = 0; n < len; ++n, ++p, ++np) {
        *np = (*p < 0x20 || *p >= 0x7f) ? (u_char) ' ' : *p;
    }

    return new_data;
}

#if (NGX_WIN32)
/*
 * Fix broken MSVC memcpy optimization for 4-byte data
 * when this function is inlined
 */
__declspec(noinline)
#endif

static void
ngx_rtmp_stat_output(ngx_pool_t *pool, ngx_chain_t ***lll,
        void *data, size_t len, ngx_uint_t escape)
{
    ngx_chain_t        *cl;
    ngx_buf_t          *b;
    size_t              real_len;

    if (len == 0) {
        return;
    }

    if (escape) {
        data = ngx_rtmp_stat_escape(pool, data, len);
        if (data == NULL) {
            return;
        }
    }

    real_len = escape
        ? len + ngx_escape_html(NULL, data, len)
        : len;

    cl = **lll;
    if (cl && cl->buf->last + real_len > cl->buf->end) {
        *lll = &cl->next;
    }

    if (**lll == NULL) {
        cl = ngx_alloc_chain_link(pool);
        if (cl == NULL) {
            DBG("couldn't allocate chain_link");
            return;
        }
        b = ngx_create_temp_buf(pool,
                ngx_max(NGX_RTMP_STAT_BUFSIZE, real_len));
        if (b == NULL || b->pos == NULL) {
            DBG("couldn't allocate temp_buf");
            return;
        }
        cl->next = NULL;
        cl->buf = b;
        **lll = cl;
    }

    b = (**lll)->buf;

    if (escape) {
        b->last = (u_char *)ngx_escape_html(b->last, data, len);
    } else {
        b->last = ngx_cpymem(b->last, data, len);
    }
}


/* These shortcuts assume 2 variables exist in current context:
 *   ngx_http_request_t    *r
 *   ngx_chain_t         ***lll */

/* plain data */
#define NGX_RTMP_STAT(data, len)    ngx_rtmp_stat_output(pool, lll, data, len, 0)

/* escaped data */
#define NGX_RTMP_STAT_E(data, len)  ngx_rtmp_stat_output(pool, lll, data, len, 1)

/* literal */
#define NGX_RTMP_STAT_L(s)          NGX_RTMP_STAT((s), sizeof(s) - 1)

/* ngx_str_t */
#define NGX_RTMP_STAT_S(s)          NGX_RTMP_STAT((s)->data, (s)->len)

/* escaped ngx_str_t */
#define NGX_RTMP_STAT_ES(s)         NGX_RTMP_STAT_E((s)->data, (s)->len)

/* C string */
#define NGX_RTMP_STAT_CS(s)         NGX_RTMP_STAT((s), ngx_strlen(s))

/* escaped C string */
#define NGX_RTMP_STAT_ECS(s)        NGX_RTMP_STAT_E((s), ngx_strlen(s))


#define NGX_RTMP_STAT_BW            0x01
#define NGX_RTMP_STAT_BYTES         0x02
#define NGX_RTMP_STAT_BW_BYTES      0x03


static void
ngx_rtmp_stat_bw(ngx_pool_t *pool, ngx_chain_t ***lll,
                 ngx_rtmp_bandwidth_t *bw, char *name,
                 ngx_uint_t flags)
{
    u_char  buf[NGX_INT64_LEN + 9];

    ngx_rtmp_update_bandwidth(bw, 0);

    if (flags & NGX_RTMP_STAT_BW) {
        NGX_RTMP_STAT_L("<bw_");
        NGX_RTMP_STAT_CS(name);
        NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), ">%uL</bw_",
                                        bw->bandwidth * 8)
                           - buf);
        NGX_RTMP_STAT_CS(name);
        NGX_RTMP_STAT_L(">\r\n");
    }

    if (flags & NGX_RTMP_STAT_BYTES) {
        NGX_RTMP_STAT_L("<bytes_");
        NGX_RTMP_STAT_CS(name);
        NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), ">%uL</bytes_",
                                        bw->bytes)
                           - buf);
        NGX_RTMP_STAT_CS(name);
        NGX_RTMP_STAT_L(">\r\n");
    }
}


#ifdef NGX_RTMP_POOL_DEBUG
static void
ngx_rtmp_stat_get_pool_size(ngx_pool_t *pool, ngx_uint_t *nlarge,
        ngx_uint_t *size)
{
    ngx_pool_large_t       *l;
    ngx_pool_t             *p, *n;

    *nlarge = 0;
    for (l = pool->large; l; l = l->next) {
        ++*nlarge;
    }

    *size = 0;
    for (p = pool, n = pool->d.next; /* void */; p = n, n = n->d.next) {
        *size += (p->d.last - (u_char *)p);
        if (n == NULL) {
            break;
        }
    }
}


static void
ngx_rtmp_stat_dump_pool(ngx_http_request_t *r, ngx_chain_t ***lll,
        ngx_pool_t *pool)
{
    ngx_uint_t  nlarge, size;
    u_char      buf[NGX_INT_T_LEN];

    size = 0;
    nlarge = 0;
    ngx_rtmp_stat_get_pool_size(pool, &nlarge, &size);
    NGX_RTMP_STAT_L("<pool><nlarge>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), "%ui", nlarge) - buf);
    NGX_RTMP_STAT_L("</nlarge><size>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), "%ui", size) - buf);
    NGX_RTMP_STAT_L("</size></pool>\r\n");
}
#endif



static void
ngx_rtmp_stat_client(ngx_pool_t *pool, ngx_chain_t ***lll,
    ngx_rtmp_session_t *s)
{
    u_char  buf[NGX_INT_T_LEN];

#ifdef NGX_RTMP_POOL_DEBUG
    ngx_rtmp_stat_dump_pool(pool, lll, s->connection->pool);
#endif
    NGX_RTMP_STAT_L("<id>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), "%ui",
                  (ngx_uint_t) s->connection->number) - buf);
    NGX_RTMP_STAT_L("</id>");

    NGX_RTMP_STAT_L("<address>");
    NGX_RTMP_STAT_ES(&s->connection->addr_text);
    NGX_RTMP_STAT_L("</address>");

    NGX_RTMP_STAT_L("<time>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), "%i",
                  (ngx_int_t) (ngx_current_msec - s->epoch)) - buf);
    NGX_RTMP_STAT_L("</time>");

    if (s->flashver.len) {
        NGX_RTMP_STAT_L("<flashver>");
        NGX_RTMP_STAT_ES(&s->flashver);
        NGX_RTMP_STAT_L("</flashver>");
    }

    if (s->page_url.len) {
        NGX_RTMP_STAT_L("<pageurl>");
        NGX_RTMP_STAT_ES(&s->page_url);
        NGX_RTMP_STAT_L("</pageurl>");
    }

    if (s->swf_url.len) {
        NGX_RTMP_STAT_L("<swfurl>");
        NGX_RTMP_STAT_ES(&s->swf_url);
        NGX_RTMP_STAT_L("</swfurl>");
    }
}

static ngx_int_t
ngx_rtmp_stat_collect_client(ngx_rtmp_session_t *s, ngx_rtmp_stats_t *st)
{
    //st->ids[st->idx++] = s->connection->number;
    return NGX_OK;
}


static char *
ngx_rtmp_stat_get_aac_profile(ngx_uint_t p, ngx_uint_t sbr, ngx_uint_t ps) {
    switch (p) {
        case 1:
            return "Main";
        case 2:
            if (ps) {
                return "HEv2";
            }
            if (sbr) {
                return "HE";
            }
            return "LC";
        case 3:
            return "SSR";
        case 4:
            return "LTP";
        case 5:
            return "SBR";
        default:
            return "";
    }
}


static char *
ngx_rtmp_stat_get_avc_profile(ngx_uint_t p) {
    switch (p) {
        case 66:
            return "Baseline";
        case 77:
            return "Main";
        case 100:
            return "High";
        default:
            return "";
    }
}


static void
ngx_rtmp_stat_live(ngx_pool_t *pool, ngx_chain_t ***lll,
        ngx_rtmp_live_app_conf_t *lacf)
{
    ngx_rtmp_live_stream_t         *stream;
    ngx_rtmp_codec_ctx_t           *codec;
    ngx_rtmp_live_ctx_t            *ctx;
    ngx_rtmp_session_t             *s;
    ngx_int_t                       n;
    ngx_uint_t                      nclients, total_nclients;
    u_char                          buf[NGX_INT_T_LEN];
    u_char                          bbuf[NGX_INT32_LEN];
//    ngx_rtmp_stat_loc_conf_t       *slcf;
    u_char                         *cname;

    if (!lacf->live) {
        return;
    }

//    slcf = ngx_http_get_module_loc_conf(r, ngx_rtmp_stat_module);

    NGX_RTMP_STAT_L("<live>\r\n");

    total_nclients = 0;
    for (n = 0; n < lacf->nbuckets; ++n) {
        for (stream = lacf->streams[n]; stream; stream = stream->next) {
            NGX_RTMP_STAT_L("<stream>\r\n");

            NGX_RTMP_STAT_L("<name>");
            NGX_RTMP_STAT_ECS(stream->name);
            NGX_RTMP_STAT_L("</name>\r\n");

            NGX_RTMP_STAT_L("<time>");
            NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf), "%i",
                          (ngx_int_t) (ngx_current_msec - stream->epoch))
                          - buf);
            NGX_RTMP_STAT_L("</time>");

            ngx_rtmp_stat_bw(pool, lll, &stream->bw_in, "in",
                             NGX_RTMP_STAT_BW_BYTES);
            ngx_rtmp_stat_bw(pool, lll, &stream->bw_out, "out",
                             NGX_RTMP_STAT_BW_BYTES);
            ngx_rtmp_stat_bw(pool, lll, &stream->bw_in_audio, "audio",
                             NGX_RTMP_STAT_BW);
            ngx_rtmp_stat_bw(pool, lll, &stream->bw_in_video, "video",
                             NGX_RTMP_STAT_BW);

            nclients = 0;
            codec = NULL;
            for (ctx = stream->ctx; ctx; ctx = ctx->next, ++nclients) {
                s = ctx->session;
                //TODO: get flags from stat request initator
//                if (slcf->stat & NGX_RTMP_STAT_CLIENTS) {
                    NGX_RTMP_STAT_L("<client>");

                    ngx_rtmp_stat_client(pool, lll, s);

                    NGX_RTMP_STAT_L("<dropped>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%ui", ctx->ndropped) - buf);
                    NGX_RTMP_STAT_L("</dropped>");

                    NGX_RTMP_STAT_L("<avsync>");
                    if (!lacf->interleave) {
                        NGX_RTMP_STAT(bbuf, ngx_snprintf(bbuf, sizeof(bbuf),
                                      "%D", ctx->cs[1].timestamp -
                                      ctx->cs[0].timestamp) - bbuf);
                    }
                    NGX_RTMP_STAT_L("</avsync>");

                    NGX_RTMP_STAT_L("<timestamp>");
                    NGX_RTMP_STAT(bbuf, ngx_snprintf(bbuf, sizeof(bbuf),
                                  "%D", s->current_time) - bbuf);
                    NGX_RTMP_STAT_L("</timestamp>");

                    if (ctx->publishing) {
                        NGX_RTMP_STAT_L("<publishing/>");
                    }

                    if (ctx->active) {
                        NGX_RTMP_STAT_L("<active/>");
                    }

                    NGX_RTMP_STAT_L("</client>\r\n");
//                }
                if (ctx->publishing) {
                    codec = ngx_rtmp_get_module_ctx(s, ngx_rtmp_codec_module);
                }
            }
            total_nclients += nclients;

            if (codec) {
                NGX_RTMP_STAT_L("<meta>");

                NGX_RTMP_STAT_L("<video>");
                NGX_RTMP_STAT_L("<width>");
                NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                              "%ui", codec->width) - buf);
                NGX_RTMP_STAT_L("</width><height>");
                NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                              "%ui", codec->height) - buf);
                NGX_RTMP_STAT_L("</height><frame_rate>");
                NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                              "%ui", codec->frame_rate) - buf);
                NGX_RTMP_STAT_L("</frame_rate>");

                cname = ngx_rtmp_get_video_codec_name(codec->video_codec_id);
                if (*cname) {
                    NGX_RTMP_STAT_L("<codec>");
                    NGX_RTMP_STAT_ECS(cname);
                    NGX_RTMP_STAT_L("</codec>");
                }
                if (codec->avc_profile) {
                    NGX_RTMP_STAT_L("<profile>");
                    NGX_RTMP_STAT_CS(
                            ngx_rtmp_stat_get_avc_profile(codec->avc_profile));
                    NGX_RTMP_STAT_L("</profile>");
                }
                if (codec->avc_level) {
                    NGX_RTMP_STAT_L("<compat>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%ui", codec->avc_compat) - buf);
                    NGX_RTMP_STAT_L("</compat>");
                }
                if (codec->avc_level) {
                    NGX_RTMP_STAT_L("<level>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%.1f", codec->avc_level / 10.) - buf);
                    NGX_RTMP_STAT_L("</level>");
                }
                NGX_RTMP_STAT_L("</video>");

                NGX_RTMP_STAT_L("<audio>");
                cname = ngx_rtmp_get_audio_codec_name(codec->audio_codec_id);
                if (*cname) {
                    NGX_RTMP_STAT_L("<codec>");
                    NGX_RTMP_STAT_ECS(cname);
                    NGX_RTMP_STAT_L("</codec>");
                }
                if (codec->aac_profile) {
                    NGX_RTMP_STAT_L("<profile>");
                    NGX_RTMP_STAT_CS(
                            ngx_rtmp_stat_get_aac_profile(codec->aac_profile,
                                                          codec->aac_sbr,
                                                          codec->aac_ps));
                    NGX_RTMP_STAT_L("</profile>");
                }
                if (codec->aac_chan_conf) {
                    NGX_RTMP_STAT_L("<channels>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%ui", codec->aac_chan_conf) - buf);
                    NGX_RTMP_STAT_L("</channels>");
                } else if (codec->audio_channels) {
                    NGX_RTMP_STAT_L("<channels>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%ui", codec->audio_channels) - buf);
                    NGX_RTMP_STAT_L("</channels>");
                }
                if (codec->sample_rate) {
                    NGX_RTMP_STAT_L("<sample_rate>");
                    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                                  "%ui", codec->sample_rate) - buf);
                    NGX_RTMP_STAT_L("</sample_rate>");
                }
                NGX_RTMP_STAT_L("</audio>");

                NGX_RTMP_STAT_L("</meta>\r\n");
            }

            NGX_RTMP_STAT_L("<nclients>");
            NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                          "%ui", nclients) - buf);
            NGX_RTMP_STAT_L("</nclients>\r\n");

            if (stream->publishing) {
                NGX_RTMP_STAT_L("<publishing/>\r\n");
            }

            if (stream->active) {
                NGX_RTMP_STAT_L("<active/>\r\n");
            }

            NGX_RTMP_STAT_L("</stream>\r\n");
        }
    }

    NGX_RTMP_STAT_L("<nclients>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                  "%ui", total_nclients) - buf);
    NGX_RTMP_STAT_L("</nclients>\r\n");

    NGX_RTMP_STAT_L("</live>\r\n");
}

static ngx_int_t
ngx_rtmp_stat_collect_live(ngx_rtmp_live_app_conf_t *lacf, ngx_rtmp_stats_t *st)
{
    ngx_rtmp_live_stream_t         *stream;
    ngx_rtmp_live_ctx_t            *ctx;
    ngx_rtmp_session_t             *s;
    ngx_int_t                       n;
    ngx_uint_t                      nclients, total_nclients;


    //DBG("dump | ngx_rtmp_stat_collect_live lacf->live: %ui", lacf->live);
    if (!lacf->live) {
        return NGX_OK;
    }

    total_nclients = 0;
    for (n = 0; n < lacf->nbuckets; ++n) {
        for (stream = lacf->streams[n]; stream; stream = stream->next) {
//            DBG("dump | \tstream bw_in.bandwidth=%uL", stream->bw_in.bandwidth);
//            DBG("dump | \tstream bw_in.bytes=%uL", stream->bw_in.bytes);
//            DBG("dump | \tstream bw_out.bandwidth=%uL", stream->bw_out.bandwidth);
//            DBG("dump | \tstream bw_out.bytes=%uL", stream->bw_out.bytes);
            nclients = 0;
            for (ctx = stream->ctx; ctx; ctx = ctx->next, ++nclients) {
                s = ctx->session;
                //DBG("dump | \tclient with id=%d", s->connection->number);
                ngx_rtmp_stat_collect_client(s, st);
            }
            total_nclients += nclients;
        }
    }
//    st->stream_nclients = total_nclients;
    return NGX_OK;
}


static void
ngx_rtmp_stat_play(ngx_pool_t *pool, ngx_chain_t ***lll,
        ngx_rtmp_play_app_conf_t *pacf)
{
    ngx_rtmp_play_ctx_t            *ctx, *sctx;
    ngx_rtmp_session_t             *s;
    ngx_uint_t                      n, nclients, total_nclients;
    u_char                          buf[NGX_INT_T_LEN];
    u_char                          bbuf[NGX_INT32_LEN];
//    ngx_rtmp_stat_loc_conf_t       *slcf;

    if (pacf->entries.nelts == 0) {
        return;
    }

    //TODO: get flags from stats request initiator
//    slcf = ngx_http_get_module_loc_conf(r, ngx_rtmp_stat_module);

    NGX_RTMP_STAT_L("<play>\r\n");

    total_nclients = 0;
    for (n = 0; n < pacf->nbuckets; ++n) {
        for (ctx = pacf->ctx[n]; ctx; ) {
            NGX_RTMP_STAT_L("<stream>\r\n");

            NGX_RTMP_STAT_L("<name>");
            NGX_RTMP_STAT_ECS(ctx->name);
            NGX_RTMP_STAT_L("</name>\r\n");

            nclients = 0;
            sctx = ctx;
            for (; ctx; ctx = ctx->next) {
                if (ngx_strcmp(ctx->name, sctx->name)) {
                    break;
                }

                nclients++;

                s = ctx->session;
                //if (slcf->stat & NGX_RTMP_STAT_CLIENTS) {
                    NGX_RTMP_STAT_L("<client>");

                    ngx_rtmp_stat_client(pool, lll, s);

                    NGX_RTMP_STAT_L("<timestamp>");
                    NGX_RTMP_STAT(bbuf, ngx_snprintf(bbuf, sizeof(bbuf),
                                  "%D", s->current_time) - bbuf);
                    NGX_RTMP_STAT_L("</timestamp>");

                    NGX_RTMP_STAT_L("</client>\r\n");
                //}
            }
            total_nclients += nclients;

            NGX_RTMP_STAT_L("<active/>");
            NGX_RTMP_STAT_L("<nclients>");
            NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                          "%ui", nclients) - buf);
            NGX_RTMP_STAT_L("</nclients>\r\n");

            NGX_RTMP_STAT_L("</stream>\r\n");
        }
    }

    NGX_RTMP_STAT_L("<nclients>");
    NGX_RTMP_STAT(buf, ngx_snprintf(buf, sizeof(buf),
                  "%ui", total_nclients) - buf);
    NGX_RTMP_STAT_L("</nclients>\r\n");

    NGX_RTMP_STAT_L("</play>\r\n");
}


static ngx_int_t
ngx_rtmp_stat_collect_play(ngx_rtmp_play_app_conf_t *pacf, ngx_rtmp_stats_t *st)
{
    ngx_rtmp_play_ctx_t            *ctx, *sctx;
    ngx_uint_t                      n, nclients, total_nclients;
    ngx_rtmp_session_t             *s;

    if (pacf->entries.nelts == 0) {
        return NGX_OK;
    }

    total_nclients = 0;
    for (n = 0; n < pacf->nbuckets; ++n) {
        for (ctx = pacf->ctx[n]; ctx; ) {
            nclients = 0;
            sctx = ctx;
            sctx = ctx;
            for (; ctx; ctx = ctx->next) {
                if (ngx_strcmp(ctx->name, sctx->name)) {
                    break;
                }

                nclients++;

                s = ctx->session;
                ngx_rtmp_stat_collect_client(s, st);
            }
            total_nclients += nclients;
        }
    }
//    st->nclients = total_nclients;
    return NGX_OK;
}


static void
ngx_rtmp_stat_application(ngx_pool_t *pool, ngx_chain_t ***lll,
        ngx_rtmp_core_app_conf_t *cacf)
{
//    ngx_rtmp_stat_loc_conf_t       *slcf;

    NGX_RTMP_STAT_L("<application>\r\n");
    NGX_RTMP_STAT_L("<name>");
    NGX_RTMP_STAT_ES(&cacf->name);
    NGX_RTMP_STAT_L("</name>\r\n");

    //TODO: get flags from stat reqeust initiator
//    slcf = ngx_http_get_module_loc_conf(r, ngx_rtmp_stat_module);

//    if (slcf->stat & NGX_RTMP_STAT_LIVE) {
        ngx_rtmp_stat_live(pool, lll,
                cacf->app_conf[ngx_rtmp_live_module.ctx_index]);
//    }

//    if (slcf->stat & NGX_RTMP_STAT_PLAY) {
        ngx_rtmp_stat_play(pool, lll,
                cacf->app_conf[ngx_rtmp_play_module.ctx_index]);
//    }

    NGX_RTMP_STAT_L("</application>\r\n");
}

static ngx_int_t
ngx_rtmp_stat_collect_application(ngx_rtmp_core_app_conf_t *cacf, ngx_rtmp_stats_t *st) {
    ngx_int_t      rc;

    if ((rc = ngx_rtmp_stat_collect_live(
             cacf->app_conf[ngx_rtmp_live_module.ctx_index], st)) != NGX_OK) {
        return rc;
    }

    if ((rc = ngx_rtmp_stat_collect_play(
             cacf->app_conf[ngx_rtmp_play_module.ctx_index], st)) != NGX_OK) {
        return rc;
    }

    return NGX_OK;
}

static void
ngx_rtmp_stat_server(ngx_pool_t *pool, ngx_chain_t ***lll,
        ngx_rtmp_core_srv_conf_t *cscf)
{
    ngx_rtmp_core_app_conf_t      **cacf;
    size_t                          n;

    NGX_RTMP_STAT_L("<server>\r\n");

#ifdef NGX_RTMP_POOL_DEBUG
    ngx_rtmp_stat_dump_pool(pool, lll, cscf->pool);
#endif

    cacf = cscf->applications.elts;
    for (n = 0; n < cscf->applications.nelts; ++n, ++cacf) {
        ngx_rtmp_stat_application(pool, lll, *cacf);
    }

    NGX_RTMP_STAT_L("</server>\r\n");
}

static ngx_int_t
ngx_rtmp_stat_collect_server(ngx_rtmp_core_srv_conf_t *cscf,
         ngx_rtmp_stats_t *st) {
    ngx_rtmp_core_app_conf_t      **cacf;
    size_t                          n;
    ngx_int_t                       rc;

//    st->applications = ngx_array_create(r->pool, cscf->applications.nelts,
//                                        sizeof(ngx_rtmp_core_app_conf_t));
//    if (st->applications == NULL) {
//        return NGX_ERROR;
//    }

    cacf = cscf->applications.elts;
    for (n = 0; n < cscf->applications.nelts; ++n, ++cacf) {
        if ((rc = ngx_rtmp_stat_collect_application(*cacf, st)) != NGX_OK) {
            return rc;
        }
    }
    return NGX_OK;
}

ngx_rtmp_stats_t*
ngx_rtmp_stat_collect(ngx_log_t *log) {
    ngx_rtmp_core_main_conf_t      *cmcf;
    ngx_rtmp_core_srv_conf_t      **cscf;
    ngx_int_t                       rc;
    ngx_uint_t                      n;
    ngx_rtmp_stats_t               *stats;

    cmcf = ngx_rtmp_core_main_conf;
    if (cmcf == NULL) {
        return NULL;
    }

    stats = ngx_calloc(sizeof(ngx_rtmp_stats_t), log);
    if (stats == NULL) {
        return NULL;
    }

    stats->bw_in = ngx_rtmp_bw_in;
    stats->bw_out = ngx_rtmp_bw_out;
    stats->naccepted = ngx_rtmp_naccepted;

    cscf = cmcf->servers.elts;
    for (n = 0; n < cmcf->servers.nelts; ++n, ++cscf) {
        if ((rc = ngx_rtmp_stat_collect_server(*cscf, stats)) != NGX_OK) {
            ngx_free(stats);
            return NULL;
        }
    }
//    DBG("dump | end ngx_rtmp_stat_collect: stream_clients=%ui", stats->stream_nclients);
    return stats;
}

static ngx_int_t
ngx_rtmp_stat_create_stat_request(ngx_http_request_t *r) {
    ngx_rtmp_stat_request_t   *sr = NULL;
    ngx_core_conf_t           *ccf;
    int                        i, workers;

    for (i = 0; i < NGX_RTMP_STAT_MAX_REQUESTS; i++) {
        if (!ngx_rtmp_stat_request_map[i].active) {
            sr = &ngx_rtmp_stat_request_map[i];
        }
    }
    if (sr == NULL) {
        return NGX_HTTP_SERVICE_UNAVAILABLE;
    }

    ccf = (ngx_core_conf_t *) ngx_get_conf(ngx_cycle->conf_ctx, ngx_core_module);
    if (ccf == NULL) {
        return NGX_ERROR;
    }

    workers = ccf->worker_processes;

    sr->active = 1;
    sr->error = 0;
    sr->r = r;
    sr->conn_id = r->connection->fd;
    sr->got_responses = 0;
    if ((sr->responses = ngx_alloc(workers * sizeof(ngx_str_t*), r->connection->log)) == NULL) {
        return NGX_ERROR;
    }
    sr->total_responses = workers;

    return NGX_OK;
}

static ngx_int_t
ngx_rtmp_stat_send_getstats_broadcast(ngx_http_request_t *r, ngx_rtmp_stat_request_ctx_t *ctx) {
    ngx_int_t                       rc;
    static u_char                   nbuf[NGX_INT_T_LEN];
    u_char                         *end;

    rc = ngx_rtmp_stat_create_stat_request(r);
    if (rc != NGX_OK) {
        return rc;
    }

    end = ngx_snprintf(nbuf, NGX_INT_T_LEN, "%d", r->connection->fd);
    ngx_str_t name = ngx_string("collect");
    ngx_str_t data = ngx_string(nbuf);
    data.len = end - data.data;
    ngx_ipc_broadcast_alert(&name, &data);

    ctx->state = state_rtmp_stat_awaiting_responses;
    ngx_add_timer(&ctx->tick, (ngx_msec_t) 100);

    DBG("send broadcast from c:%d pid:%ui", r->connection->fd, (ngx_uint_t)ngx_getpid());

    return NGX_AGAIN;
}

ngx_chain_t *
ngx_rtmp_stat_create_stats(ngx_pool_t *pool) {
    ngx_rtmp_core_srv_conf_t      **cscf;
    ngx_rtmp_core_main_conf_t      *cmcf;
    ngx_chain_t                    *cl, **ll, ***lll;
    static u_char                   tbuf[NGX_TIME_T_LEN];
    static u_char                   nbuf[NGX_INT_T_LEN];
    size_t                          n;

    cmcf = ngx_rtmp_core_main_conf;
    if (cmcf == NULL) {
        return NULL;
    }

    cl = NULL;
    ll = &cl;
    lll = &ll;

    NGX_RTMP_STAT_L("<?xml version=\"1.0\" encoding=\"utf-8\" ?>\r\n");
    NGX_RTMP_STAT_L("<rtmp>\r\n");

#ifdef NGINX_VERSION
    NGX_RTMP_STAT_L("<nginx_version>" NGINX_VERSION "</nginx_version>\r\n");
#endif

#ifdef NGINX_RTMP_VERSION
    NGX_RTMP_STAT_L("<nginx_rtmp_version>" NGINX_RTMP_VERSION "</nginx_rtmp_version>\r\n");
#endif

#ifdef NGX_COMPILER
    NGX_RTMP_STAT_L("<compiler>" NGX_COMPILER "</compiler>\r\n");
#endif
    NGX_RTMP_STAT_L("<built>" __DATE__ " " __TIME__ "</built>\r\n");

    NGX_RTMP_STAT_L("<pid>");
    NGX_RTMP_STAT(nbuf, ngx_snprintf(nbuf, sizeof(nbuf),
                  "%ui", (ngx_uint_t) ngx_getpid()) - nbuf);
    NGX_RTMP_STAT_L("</pid>\r\n");

    NGX_RTMP_STAT_L("<uptime>");
    NGX_RTMP_STAT(tbuf, ngx_snprintf(tbuf, sizeof(tbuf),
                  "%T", ngx_cached_time->sec - start_time) - tbuf);
    NGX_RTMP_STAT_L("</uptime>\r\n");

    NGX_RTMP_STAT_L("<naccepted>");
    NGX_RTMP_STAT(nbuf, ngx_snprintf(nbuf, sizeof(nbuf),
                  "%ui", ngx_rtmp_naccepted) - nbuf);
    NGX_RTMP_STAT_L("</naccepted>\r\n");

    ngx_rtmp_stat_bw(pool, lll, &ngx_rtmp_bw_in, "in", NGX_RTMP_STAT_BW_BYTES);
    ngx_rtmp_stat_bw(pool, lll, &ngx_rtmp_bw_out, "out", NGX_RTMP_STAT_BW_BYTES);

    cscf = cmcf->servers.elts;
    for (n = 0; n < cmcf->servers.nelts; ++n, ++cscf) {
        ngx_rtmp_stat_server(pool, lll, *cscf);
    }

    NGX_RTMP_STAT_L("</rtmp>\r\n");

    (*ll)->buf->last_buf = 1;

    return cl;
}

static void ngx_rtmp_stat_tick_handler(ngx_event_t *ev) {
    ngx_http_request_t              *r;
    ngx_rtmp_stat_request_ctx_t     *ctx;
    ngx_rtmp_stat_request_t         *sr = NULL;
    ngx_int_t                        i, rc;
    ngx_pool_t                      *pool;

    ngx_rtmp_core_main_conf_t       *cmcf;
    ngx_chain_t                     *cl;
    ngx_buf_t                       *b;
    off_t                            len;
    ngx_str_t                       *s;

    r = ev->data;
    if (r == NULL) {
        goto error;
    }
    pool = r->pool;

    ctx = ngx_http_get_module_ctx(r, ngx_rtmp_stat_module);
    if (ctx == NULL) {
        goto error;
    }

    for (i = 0; i < NGX_RTMP_STAT_MAX_REQUESTS; i++) {
        if (ngx_rtmp_stat_request_map[i].active && ngx_rtmp_stat_request_map[i].conn_id == r->connection->fd) {
            sr = &ngx_rtmp_stat_request_map[i];
            break;
        }
    }

    if (sr == NULL) {
        goto error;
    }

    if (sr->error) {
        goto error;
    }

    //TODO: replace hardcoded value with loc conf value
    if ((ngx_timeofday())->msec - ctx->timer_begin > 1000) {
        ctx->state = state_rtmp_stat_timedout;
    }
    DBG("\t state %d", ctx->state);
    if (sr->got_responses < sr->total_responses && ctx->state != state_rtmp_stat_timedout) {
        ngx_add_timer(&ctx->tick, (ngx_msec_t)100);
        return;// NGX_AGAIN;
    }

    sr->active = 0;

    cmcf = ngx_rtmp_core_main_conf;
    if (cmcf == NULL) {
        goto error;
    }

    len = 0;
    DBG("adding %d responses to out chain", sr->got_responses);
    for (i = 0; i < sr->got_responses; i++) {
        len += sr->responses[i]->len;
    }

    cl = ngx_alloc_chain_link(pool);
    if (cl == NULL) {
        DBG("can't alloc chain link");
        goto error;
    }

    b = ngx_create_temp_buf(pool, len);
    if (b == NULL || b->pos == NULL) {
        DBG("can't alloc temp buf");
        goto error;
    }
    cl->next = NULL;
    cl->buf = b;
    cl->buf->last_buf = 1;

    for (i = 0; i < sr->got_responses; i++) {
        s = sr->responses[i];
        DBG("copying %ui bytes to response buffer", s->len);
        b->last = ngx_cpymem(b->last, s->data, s->len);
    }

    ngx_rtmp_stat_clear_stat_request(r->connection->fd);

    ngx_str_set(&r->headers_out.content_type, "text/xml");
    r->headers_out.content_length_n = len;
    r->headers_out.status = NGX_HTTP_OK;
    rc = ngx_http_send_header(r);
    if (rc != NGX_OK) {
        ngx_http_finalize_request(r, rc);
        return;
    }
    rc = ngx_http_output_filter(r, cl);
    ngx_http_finalize_request(r, rc);
    return;

error:
    ngx_rtmp_stat_clear_stat_request(r->connection->fd);

    r->headers_out.status = NGX_HTTP_INTERNAL_SERVER_ERROR;
    r->headers_out.content_length_n = 0;
    rc = ngx_http_send_header(r);
    ngx_http_finalize_request(r, rc);
}



static ngx_int_t
ngx_rtmp_stat_handler(ngx_http_request_t *r)
{
    ngx_rtmp_stat_loc_conf_t       *slcf;
    ngx_rtmp_stat_request_ctx_t    *ctx;
    ngx_int_t                       rc;


    slcf = ngx_http_get_module_loc_conf(r, ngx_rtmp_stat_module);
    if (slcf->stat == 0) {
        return NGX_DECLINED;
    }

    ctx = ngx_http_get_module_ctx(r, ngx_rtmp_stat_module);
    if (ctx == NULL) {
        ctx = ngx_rtmp_stat_create_request_ctx(r);
        if (ctx == NULL) {
            return NGX_HTTP_INTERNAL_SERVER_ERROR;
        }

        ngx_http_set_ctx(r, ctx, ngx_rtmp_stat_module);
    }

    rc = ngx_rtmp_stat_send_getstats_broadcast(r, ctx);

    if (rc == NGX_ERROR ||
            rc == NGX_OK ||
            rc == NGX_DONE ||
            rc == NGX_DECLINED) {
        return rc;
    }

    if (rc >= NGX_HTTP_SPECIAL_RESPONSE) {
        if (ctx && ctx->header_sent) {
            return NGX_ERROR;
        }

        return rc;
    }

    /* rc == NGX_AGAIN */
#if defined(nginx_version) && nginx_version >= 8011
    r->main->count++;
#endif

    if (ctx) {
        ctx->waiting = 1;
        ctx->done = 0;
    }

    return NGX_DONE;
}


static void *
ngx_rtmp_stat_create_loc_conf(ngx_conf_t *cf)
{
    ngx_rtmp_stat_loc_conf_t       *conf;

    conf = ngx_pcalloc(cf->pool, sizeof(ngx_rtmp_stat_loc_conf_t));
    if (conf == NULL) {
        return NULL;
    }

    conf->stat = 0;
    conf->state = 0;

    return conf;
}


static char *
ngx_rtmp_stat_merge_loc_conf(ngx_conf_t *cf, void *parent, void *child)
{
    ngx_rtmp_stat_loc_conf_t       *prev = parent;
    ngx_rtmp_stat_loc_conf_t       *conf = child;

    ngx_conf_merge_bitmask_value(conf->stat, prev->stat, 0);
    ngx_conf_merge_str_value(conf->stylesheet, prev->stylesheet, "");

    return NGX_CONF_OK;
}


static char *
ngx_rtmp_stat(ngx_conf_t *cf, ngx_command_t *cmd, void *conf)
{
    ngx_http_core_loc_conf_t  *clcf;

    clcf = ngx_http_conf_get_module_loc_conf(cf, ngx_http_core_module);
    clcf->handler = ngx_rtmp_stat_handler;

    return ngx_conf_set_bitmask_slot(cf, cmd, conf);
}


static ngx_int_t
ngx_rtmp_stat_postconfiguration(ngx_conf_t *cf)
{
    //ngx_http_handler_pt       *h;
    ngx_http_core_main_conf_t *cmcf;

    cmcf = ngx_http_conf_get_module_main_conf(cf, ngx_http_core_module);
//    h = ngx_array_push(&cmcf->phases[NGX_HTTP_CONTENT_PHASE].handlers);
//    if (h == NULL) {
//        return NGX_ERROR;
//    }
//    *h = ngx_rtmp_stat_handler;
    start_time = ngx_cached_time->sec;

    return NGX_OK;
}
