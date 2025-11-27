-record(create_post, {
    user_id :: binary(),
    subreddit :: binary(),
    title :: binary(),
    content :: binary(),
    signature :: gleam@option:option(binary())
}).
