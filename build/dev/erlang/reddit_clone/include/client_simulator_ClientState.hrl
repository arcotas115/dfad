-record(client_state, {
    user_id :: binary(),
    username :: binary(),
    password :: binary(),
    public_key :: binary(),
    private_key :: binary(),
    joined_subreddits :: list(binary()),
    created_posts :: list(binary()),
    is_connected :: boolean(),
    actions_remaining :: integer()
}).
