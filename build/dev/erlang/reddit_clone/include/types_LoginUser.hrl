-record(login_user, {
    user_id :: binary(),
    session :: gleam@erlang@process:subject(types:client_message())
}).
