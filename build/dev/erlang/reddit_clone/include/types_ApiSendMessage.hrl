-record(api_send_message, {
    sender :: binary(),
    recipient :: binary(),
    content :: binary(),
    reply_to :: gleam@option:option(binary())
}).
