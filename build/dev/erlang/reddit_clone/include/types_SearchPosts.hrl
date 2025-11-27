-record(search_posts, {
    'query' :: binary(),
    limit :: integer(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
