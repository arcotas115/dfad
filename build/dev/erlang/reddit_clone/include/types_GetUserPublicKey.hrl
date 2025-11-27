-record(get_user_public_key, {
    user_id :: binary(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
