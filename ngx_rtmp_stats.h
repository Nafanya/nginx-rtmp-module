#ifndef _NGX_RTMP_STATS_H_INCLUDED_
#define _NGX_RTMP_STATS_H_INCLUDED_


#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_http.h>

#include <ngx_rtmp.h>

ngx_chain_t * ngx_rtmp_stat_create_stats(ngx_pool_t *pool);
void ngx_rtmp_stat_handle_stat(ngx_int_t conn_id, ngx_str_t *data);

typedef struct ngx_rtmp_stats_s               ngx_rtmp_stats_t;
typedef struct ngx_rtmp_stats_server_s        ngx_rtmp_stats_server_t;
typedef struct ngx_rtmp_stats_application_s   ngx_rtmp_stats_application_t;
typedef struct ngx_rtmp_stats_live_s          ngx_rtmp_stats_live_t;
typedef struct ngx_rtmp_stats_play_s          ngx_rtmp_stats_play_t;

typedef struct ngx_rtmp_stat_request_ctx_s    ngx_rtmp_stat_request_ctx_t;

#define NGX_RTMP_STAT_MAX_REQUESTS 16

typedef struct {
    ngx_http_request_t   *r;

    ngx_socket_t          conn_id;
    ngx_int_t             total_responses;
    ngx_int_t             got_responses;
    ngx_str_t           **responses;

    unsigned              active:1;
} ngx_rtmp_stat_request_t;

extern ngx_rtmp_stat_request_t ngx_rtmp_stat_request_map[NGX_RTMP_STAT_MAX_REQUESTS];

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
    ngx_uint_t                naccepted;
    ngx_rtmp_bandwidth_t      bw_in;
    ngx_rtmp_bandwidth_t      bw_out;

    ngx_array_t              *servers; // ngx_rtmp_stats_server_t
};

struct ngx_rtmp_stats_server_s {
    ngx_array_t              *applications; // ngx_rtmp_stats_application_t
};

struct ngx_rtmp_stats_application_s {
    ngx_str_t                 name;
    ngx_array_t              *lives; // ngx_rtmp_stats_stream_t
//    ngx_array_t              *
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
    ngx_msec_t                time;
    ngx_rtmp_bandwidth_t      bw_in;
    ngx_rtmp_bandwidth_t      bw_out;
    ngx_rtmp_bandwidth_t      bw_in_audio;
    ngx_rtmp_bandwidth_t      bw_out_audio;

    ngx_array_t              *clients;  // ngx_rtmp_stats_client_t
};


ngx_rtmp_stats_t* ngx_rtmp_stat_collect(ngx_log_t *log);

//ngx_rtmp_stats_t     ngx_rtmp_stats;

#endif /* _NGX_RTMP_STATS_H_INCLUDED_ */
