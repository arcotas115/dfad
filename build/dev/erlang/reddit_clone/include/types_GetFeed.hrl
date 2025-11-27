-record(get_feed, {
    user_id :: binary(),
    limit :: integer(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
