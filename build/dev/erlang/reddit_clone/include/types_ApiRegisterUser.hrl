-record(api_register_user, {
    username :: binary(),
    password :: binary(),
    public_key :: gleam@option:option(binary())
}).
