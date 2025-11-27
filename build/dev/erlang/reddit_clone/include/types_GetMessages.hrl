-record(get_messages, {
    user_id :: binary(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
