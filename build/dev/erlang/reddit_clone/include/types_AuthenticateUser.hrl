-record(authenticate_user, {
    username :: binary(),
    password :: binary(),
    reply_to :: gleam@erlang@process:subject(types:engine_response())
}).
