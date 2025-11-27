-record(user, {
    id :: binary(),
    username :: binary(),
    password_hash :: binary(),
    karma :: integer(),
    joined_subreddits :: list(binary()),
    created_at :: integer(),
    public_key :: gleam@option:option(binary())
}).
