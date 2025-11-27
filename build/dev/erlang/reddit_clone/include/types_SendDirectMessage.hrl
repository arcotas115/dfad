-record(send_direct_message, {
    sender :: binary(),
    recipient :: binary(),
    content :: binary(),
    reply_to :: gleam@option:option(binary())
}).
