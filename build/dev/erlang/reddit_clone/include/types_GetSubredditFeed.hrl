-record(get_subreddit_feed, {
    subreddit :: binary(),
    limit :: integer(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
