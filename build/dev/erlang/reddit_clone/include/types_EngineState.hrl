-record(engine_state, {
    self_subject :: gleam@erlang@process:subject(types:engine_message()),
    users :: gleam@dict:dict(binary(), types:user()),
    subreddits :: gleam@dict:dict(binary(), types:subreddit()),
    posts :: gleam@dict:dict(binary(), types:post()),
    comments :: gleam@dict:dict(binary(), types:comment()),
    messages :: gleam@dict:dict(binary(), types:direct_message()),
    user_votes :: gleam@dict:dict(binary(), types:vote()),
    user_sessions :: gleam@dict:dict(binary(), gleam@erlang@process:subject(types:client_message())),
    next_post_id :: integer(),
    next_comment_id :: integer(),
    next_message_id :: integer(),
    metrics :: types:performance_metrics(),
    start_time :: integer(),
    cached_stats :: gleam@option:option(types:engine_stats()),
    last_stats_update :: integer()
}).
