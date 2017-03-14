#ifndef _NGX_RTMP_STATS_H_INCLUDED_
#define _NGX_RTMP_STATS_H_INCLUDED_


#include <ngx_config.h>
#include <ngx_core.h>

#include <ngx_rtmp.h>

typedef struct ngx_rtmp_stats_s               ngx_rtmp_stats_t;
typedef struct ngx_rtmp_stats_application_s   ngx_rtmp_stats_application_t;
typedef struct ngx_rtmp_stats_live_s          ngx_rtmp_stats_live_t;
typedef struct ngx_rtmp_stats_play_s          ngx_rtmp_stats_play_t;
typedef struct ngx_rtmp_stat_request_ctx_s    ngx_rtmp_stat_request_ctx_t;

struct ngx_rtmp_stat_request_ctx_s {
    ngx_int_t    responses;
    ngx_event_t  tick;
    ngx_time_t   timer_begin;
    ngx_int_t    result;

    ngx_int_t    state;

    unsigned     waiting:1;
    unsigned     done:1;
    unsigned     header_sent:1;
};

struct ngx_rtmp_stats_s {
    //TODO: built, pid, uptime etc.
    ngx_rtmp_bandwidth_t      bw_in;
    ngx_rtmp_bandwidth_t      bw_out;

    ngx_uint_t                naccepted;

    ngx_array_t              *servers;
    ngx_uint_t                nclients;
    ngx_uint_t                stream_nclients;

    //temporary solution
    ngx_uint_t                ids[512];
    ngx_uint_t                idx;
};

struct ngx_rtmp_stats_application_s {
    ngx_str_t                 name;
    //ngx_array_t              *lives;
    //ngx_array_t              *
    //lives
    //plays
};

struct ngx_rtmp_stats_live_s {
    ngx_str_t                  name;
};

struct ngx_rtmp_stats_play_s {

};

struct ngx_rtmp_stats_stream_s {
    ngx_str_t                 name;
    ngx_array_t              *clients;
};


ngx_rtmp_stats_t* ngx_rtmp_stat_collect(ngx_log_t *log);

//ngx_rtmp_stats_t     ngx_rtmp_stats;

#endif /* _NGX_RTMP_STATS_H_INCLUDED_ */
