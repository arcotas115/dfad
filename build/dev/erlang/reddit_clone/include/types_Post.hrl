-record(post, {
    id :: binary(),
    subreddit :: binary(),
    author :: binary(),
    title :: binary(),
    content :: binary(),
    upvotes :: integer(),
    downvotes :: integer(),
    created_at :: integer(),
    is_repost :: boolean(),
    original_post_id :: gleam@option:option(binary()),
    signature :: gleam@option:option(binary())
}).
