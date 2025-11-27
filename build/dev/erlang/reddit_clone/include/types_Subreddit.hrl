-record(subreddit, {
    name :: binary(),
    description :: binary(),
    creator :: binary(),
    members :: list(binary()),
    post_count :: integer(),
    created_at :: integer()
}).
