-record(client_state, {
    user_id :: binary(),
    username :: binary(),
    is_connected :: boolean(),
    joined_subreddits :: list(binary()),
    activity_level :: float(),
    seen_posts :: list(binary()),
    seen_comments :: list({binary(), binary()}),
    received_messages :: list(types:direct_message())
}).
