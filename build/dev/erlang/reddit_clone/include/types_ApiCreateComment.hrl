-record(api_create_comment, {
    user_id :: binary(),
    post_id :: binary(),
    parent_id :: gleam@option:option(binary()),
    content :: binary()
}).
