-record(user_info, {
    id :: binary(),
    username :: binary(),
    karma :: integer(),
    joined_subreddits_count :: integer(),
    created_at :: integer(),
    has_public_key :: boolean()
}).
