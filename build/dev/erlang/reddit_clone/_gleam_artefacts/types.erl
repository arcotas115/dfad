-module(types).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/types.gleam").
-export_type([user/0, subreddit/0, post/0, comment/0, vote/0, direct_message/0, engine_state/0, performance_metrics/0, engine_message/0, engine_response/0, response_data/0, user_info/0, client_message/0, engine_stats/0, client_state/0, simulator_config/0, api_request/0]).

-type user() :: {user,
        binary(),
        binary(),
        binary(),
        integer(),
        list(binary()),
        integer(),
        gleam@option:option(binary())}.

-type subreddit() :: {subreddit,
        binary(),
        binary(),
        binary(),
        list(binary()),
        integer(),
        integer()}.

-type post() :: {post,
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        integer(),
        integer(),
        integer(),
        boolean(),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type comment() :: {comment,
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary(),
        binary(),
        integer(),
        integer(),
        integer(),
        list(binary())}.

-type vote() :: upvote | downvote.

-type direct_message() :: {direct_message,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary()),
        integer(),
        boolean()}.

-type engine_state() :: {engine_state,
        gleam@erlang@process:subject(engine_message()),
        gleam@dict:dict(binary(), user()),
        gleam@dict:dict(binary(), subreddit()),
        gleam@dict:dict(binary(), post()),
        gleam@dict:dict(binary(), comment()),
        gleam@dict:dict(binary(), direct_message()),
        gleam@dict:dict(binary(), vote()),
        gleam@dict:dict(binary(), gleam@erlang@process:subject(client_message())),
        integer(),
        integer(),
        integer(),
        performance_metrics(),
        integer(),
        gleam@option:option(engine_stats()),
        integer()}.

-type performance_metrics() :: {performance_metrics,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        float(),
        float(),
        float()}.

-type engine_message() :: {register_user,
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {login_user, binary(), gleam@erlang@process:subject(client_message())} |
    {logout_user, binary()} |
    {create_subreddit, binary(), binary(), binary()} |
    {join_subreddit, binary(), binary()} |
    {leave_subreddit, binary(), binary()} |
    {create_post,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {create_repost, binary(), binary(), binary()} |
    {vote_post, binary(), binary(), vote()} |
    {create_comment,
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary()} |
    {vote_comment, binary(), binary(), vote()} |
    {get_feed,
        binary(),
        integer(),
        gleam@erlang@process:subject(engine_response())} |
    {get_subreddit_feed,
        binary(),
        integer(),
        gleam@erlang@process:subject(engine_response())} |
    {get_post, binary(), gleam@erlang@process:subject(engine_response())} |
    {get_comments, binary(), gleam@erlang@process:subject(engine_response())} |
    {send_direct_message,
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {get_messages, binary(), gleam@erlang@process:subject(engine_response())} |
    {get_stats, gleam@erlang@process:subject(engine_response())} |
    {get_user_karma, binary(), gleam@erlang@process:subject(engine_response())} |
    update_stats_cache |
    {authenticate_user,
        binary(),
        binary(),
        gleam@erlang@process:subject(engine_response())} |
    {get_all_subreddits, gleam@erlang@process:subject(engine_response())} |
    {search_posts,
        binary(),
        integer(),
        gleam@erlang@process:subject(engine_response())} |
    {search_subreddits,
        binary(),
        integer(),
        gleam@erlang@process:subject(engine_response())} |
    {search_users,
        binary(),
        integer(),
        gleam@erlang@process:subject(engine_response())} |
    {get_user_public_key,
        binary(),
        gleam@erlang@process:subject(engine_response())} |
    {get_active_clients, gleam@erlang@process:subject(engine_response())}.

-type engine_response() :: {success, response_data()} | {engine_error, binary()}.

-type response_data() :: {user_registered, binary()} |
    {user_authenticated, binary(), binary()} |
    {subreddit_created, binary()} |
    {joined_subreddit, binary()} |
    {left_subreddit, binary()} |
    {post_created, binary()} |
    {comment_created, binary()} |
    vote_cast |
    {feed_data, list(post())} |
    {post_data, post(), list(comment())} |
    {comments_data, list(comment())} |
    {messages_data, list(direct_message())} |
    {message_sent, binary()} |
    {stats_data, engine_stats()} |
    {karma_data, integer()} |
    {subreddits_data, list(subreddit())} |
    {post_search_results, list(post())} |
    {subreddit_search_results, list(subreddit())} |
    {user_search_results, list(user_info())} |
    {public_key_data, gleam@option:option(binary())} |
    {active_clients_data, integer(), list(binary())}.

-type user_info() :: {user_info,
        binary(),
        binary(),
        integer(),
        integer(),
        integer(),
        boolean()}.

-type client_message() :: {new_post, post()} |
    {new_comment, comment()} |
    {post_vote_update, binary(), integer(), integer()} |
    {new_direct_message, direct_message()}.

-type engine_stats() :: {engine_stats,
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        list({binary(), integer()}),
        performance_metrics(),
        integer(),
        integer(),
        integer(),
        integer(),
        integer(),
        {integer(), integer(), integer()},
        float(),
        float(),
        float(),
        integer(),
        integer()}.

-type client_state() :: {client_state,
        binary(),
        binary(),
        boolean(),
        list(binary()),
        float(),
        list(binary()),
        list({binary(), binary()}),
        list(direct_message())}.

-type simulator_config() :: {simulator_config,
        integer(),
        integer(),
        integer(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float()}.

-type api_request() :: {api_register_user,
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {api_login_user, binary(), binary()} |
    {api_create_subreddit, binary(), binary(), binary()} |
    {api_join_subreddit, binary(), binary()} |
    {api_leave_subreddit, binary(), binary()} |
    api_get_subreddits |
    {api_get_subreddit, binary()} |
    {api_create_post,
        binary(),
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {api_get_feed, binary(), integer()} |
    {api_get_subreddit_feed, binary(), integer()} |
    {api_get_post, binary()} |
    {api_vote_post, binary(), binary(), vote()} |
    {api_create_comment,
        binary(),
        binary(),
        gleam@option:option(binary()),
        binary()} |
    {api_get_comments, binary()} |
    {api_vote_comment, binary(), binary(), vote()} |
    {api_send_message,
        binary(),
        binary(),
        binary(),
        gleam@option:option(binary())} |
    {api_get_messages, binary()} |
    api_get_stats |
    {api_get_user_karma, binary()} |
    {api_search_posts, binary(), integer()} |
    {api_search_subreddits, binary(), integer()} |
    {api_search_users, binary(), integer()} |
    {api_get_user_public_key, binary()}.


