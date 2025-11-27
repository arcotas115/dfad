-record(direct_message, {
    id :: binary(),
    sender :: binary(),
    recipient :: binary(),
    content :: binary(),
    reply_to :: gleam@option:option(binary()),
    created_at :: integer(),
    is_read :: boolean()
}).
