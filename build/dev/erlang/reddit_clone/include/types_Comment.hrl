-record(comment, {
    id :: binary(),
    post_id :: binary(),
    parent_id :: gleam@option:option(binary()),
    author :: binary(),
    content :: binary(),
    upvotes :: integer(),
    downvotes :: integer(),
    created_at :: integer(),
    replies :: list(binary())
}).
