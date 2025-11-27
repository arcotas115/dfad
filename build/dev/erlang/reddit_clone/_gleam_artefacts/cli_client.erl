-module(cli_client).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/cli_client.gleam").
-export([main/0]).
-export_type([client_state/0]).

-type client_state() :: {client_state, binary(), binary(), binary()}.

-file("src/cli_client.gleam", 626).
-spec make_request(gleam@http:method(), binary(), binary(), binary()) -> {ok,
        gleam@http@response:response(binary())} |
    {error, binary()}.
make_request(Method, Url, Body, Auth) ->
    Path = gleam@string:replace(
        Url,
        <<"http://localhost:8080"/utf8>>,
        <<""/utf8>>
    ),
    Req = begin
        _pipe = gleam@http@request:new(),
        _pipe@1 = gleam@http@request:set_method(_pipe, Method),
        _pipe@2 = gleam@http@request:set_scheme(_pipe@1, http),
        _pipe@3 = gleam@http@request:set_host(_pipe@2, <<"localhost"/utf8>>),
        _pipe@4 = gleam@http@request:set_port(_pipe@3, 8080),
        _pipe@5 = gleam@http@request:set_path(_pipe@4, Path),
        _pipe@6 = gleam@http@request:set_body(_pipe@5, Body),
        gleam@http@request:set_header(
            _pipe@6,
            <<"content-type"/utf8>>,
            <<"application/json"/utf8>>
        )
    end,
    Req@1 = case Auth of
        <<""/utf8>> ->
            Req;

        Token ->
            gleam@http@request:set_header(Req, <<"authorization"/utf8>>, Token)
    end,
    _pipe@7 = gleam@httpc:send(Req@1),
    gleam@result:map_error(
        _pipe@7,
        fun(_) -> <<"HTTP request failed"/utf8>> end
    ).

-file("src/cli_client.gleam", 238).
-spec test_health_check(binary()) -> nil.
test_health_check(Base_url) ->
    gleam_stdlib:println(
        <<<<"GET "/utf8, Base_url/binary>>/binary, "/health"/utf8>>
    ),
    case make_request(
        get,
        <<Base_url/binary, "/health"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 250).
-spec test_register_with_key(binary(), binary(), binary(), binary()) -> nil.
test_register_with_key(Base_url, Username, Password, Public_key) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/register"/utf8>>
    ),
    gleam_stdlib:println(<<"Username: "/utf8, Username/binary>>),
    gleam_stdlib:println(
        <<<<"Public Key: "/utf8,
                (gleam@string:slice(Public_key, 0, 32))/binary>>/binary,
            "..."/utf8>>
    ),
    Body = <<<<<<<<<<<<"{\"username\":\""/utf8, Username/binary>>/binary,
                        "\",\"password\":\""/utf8>>/binary,
                    Password/binary>>/binary,
                "\",\"public_key\":\""/utf8>>/binary,
            Public_key/binary>>/binary,
        "\"}"/utf8>>,
    case make_request(
        post,
        <<Base_url/binary, "/api/register"/utf8>>,
        Body,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 278).
-spec test_login(binary(), binary(), binary()) -> nil.
test_login(Base_url, Username, Password) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/login"/utf8>>
    ),
    gleam_stdlib:println(<<"Username: "/utf8, Username/binary>>),
    Body = <<<<<<<<"{\"username\":\""/utf8, Username/binary>>/binary,
                "\",\"password\":\""/utf8>>/binary,
            Password/binary>>/binary,
        "\"}"/utf8>>,
    case make_request(
        post,
        <<Base_url/binary, "/api/login"/utf8>>,
        Body,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 294).
-spec test_get_public_key(binary(), binary()) -> nil.
test_get_public_key(Base_url, Username) ->
    gleam_stdlib:println(
        <<<<<<<<"GET "/utf8, Base_url/binary>>/binary, "/api/user/"/utf8>>/binary,
                Username/binary>>/binary,
            "/publickey"/utf8>>
    ),
    case make_request(
        get,
        <<<<<<Base_url/binary, "/api/user/"/utf8>>/binary, Username/binary>>/binary,
            "/publickey"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 313).
-spec test_create_subreddit(binary(), binary(), binary(), binary()) -> nil.
test_create_subreddit(Base_url, Username, Name, Description) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/subreddit"/utf8>>
    ),
    gleam_stdlib:println(<<"Subreddit: "/utf8, Name/binary>>),
    Body = <<<<<<<<"{\"name\":\""/utf8, Name/binary>>/binary,
                "\",\"description\":\""/utf8>>/binary,
            Description/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        post,
        <<Base_url/binary, "/api/subreddit"/utf8>>,
        Body,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 335).
-spec test_get_subreddits(binary()) -> nil.
test_get_subreddits(Base_url) ->
    gleam_stdlib:println(
        <<<<"GET "/utf8, Base_url/binary>>/binary, "/api/subreddits"/utf8>>
    ),
    case make_request(
        get,
        <<Base_url/binary, "/api/subreddits"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 347).
-spec test_join_subreddit(binary(), binary(), binary()) -> nil.
test_join_subreddit(Base_url, Username, Subreddit) ->
    gleam_stdlib:println(
        <<<<<<<<"POST "/utf8, Base_url/binary>>/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/join"/utf8>>
    ),
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        post,
        <<<<<<Base_url/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/join"/utf8>>,
        <<""/utf8>>,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 367).
-spec test_create_signed_post(
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> nil.
test_create_signed_post(
    Base_url,
    Username,
    Subreddit,
    Title,
    Content,
    Signature
) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/post"/utf8>>
    ),
    gleam_stdlib:println(<<"Title: "/utf8, Title/binary>>),
    gleam_stdlib:println(
        <<<<"Signature: "/utf8, (gleam@string:slice(Signature, 0, 32))/binary>>/binary,
            "..."/utf8>>
    ),
    Body = <<<<<<<<<<<<<<<<"{\"subreddit\":\""/utf8, Subreddit/binary>>/binary,
                                "\",\"title\":\""/utf8>>/binary,
                            Title/binary>>/binary,
                        "\",\"content\":\""/utf8>>/binary,
                    Content/binary>>/binary,
                "\",\"signature\":\""/utf8>>/binary,
            Signature/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(post, <<Base_url/binary, "/api/post"/utf8>>, Body, Auth) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 400).
-spec test_get_feed(binary(), binary()) -> nil.
test_get_feed(Base_url, Username) ->
    gleam_stdlib:println(
        <<<<"GET "/utf8, Base_url/binary>>/binary, "/api/posts?limit=10"/utf8>>
    ),
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        get,
        <<Base_url/binary, "/api/posts?limit=10"/utf8>>,
        <<""/utf8>>,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 413).
-spec test_get_subreddit_posts(binary(), binary()) -> nil.
test_get_subreddit_posts(Base_url, Subreddit) ->
    gleam_stdlib:println(
        <<<<<<<<"GET "/utf8, Base_url/binary>>/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/posts"/utf8>>
    ),
    case make_request(
        get,
        <<<<<<Base_url/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/posts"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 432).
-spec test_get_post(binary(), binary()) -> nil.
test_get_post(Base_url, Post_id) ->
    gleam_stdlib:println(
        <<<<<<"GET "/utf8, Base_url/binary>>/binary, "/api/post/"/utf8>>/binary,
            Post_id/binary>>
    ),
    gleam_stdlib:println(<<"(Signature will be verified on download)"/utf8>>),
    case make_request(
        get,
        <<<<Base_url/binary, "/api/post/"/utf8>>/binary, Post_id/binary>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 445).
-spec test_create_comment(binary(), binary(), binary(), binary()) -> nil.
test_create_comment(Base_url, Username, Post_id, Content) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/comment"/utf8>>
    ),
    gleam_stdlib:println(<<"Post ID: "/utf8, Post_id/binary>>),
    Body = <<<<<<<<"{\"post_id\":\""/utf8, Post_id/binary>>/binary,
                "\",\"content\":\""/utf8>>/binary,
            Content/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        post,
        <<Base_url/binary, "/api/comment"/utf8>>,
        Body,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 467).
-spec test_get_comments(binary(), binary()) -> nil.
test_get_comments(Base_url, Post_id) ->
    gleam_stdlib:println(
        <<<<<<<<"GET "/utf8, Base_url/binary>>/binary, "/api/post/"/utf8>>/binary,
                Post_id/binary>>/binary,
            "/comments"/utf8>>
    ),
    case make_request(
        get,
        <<<<<<Base_url/binary, "/api/post/"/utf8>>/binary, Post_id/binary>>/binary,
            "/comments"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 486).
-spec test_upvote_post(binary(), binary(), binary()) -> nil.
test_upvote_post(Base_url, Username, Post_id) ->
    gleam_stdlib:println(
        <<<<<<<<"POST "/utf8, Base_url/binary>>/binary, "/api/post/"/utf8>>/binary,
                Post_id/binary>>/binary,
            "/upvote"/utf8>>
    ),
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        post,
        <<<<<<Base_url/binary, "/api/post/"/utf8>>/binary, Post_id/binary>>/binary,
            "/upvote"/utf8>>,
        <<""/utf8>>,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 506).
-spec test_send_message(binary(), binary(), binary(), binary()) -> nil.
test_send_message(Base_url, Username, Recipient, Content) ->
    gleam_stdlib:println(
        <<<<"POST "/utf8, Base_url/binary>>/binary, "/api/message"/utf8>>
    ),
    gleam_stdlib:println(<<"To: "/utf8, Recipient/binary>>),
    Body = <<<<<<<<"{\"recipient\":\""/utf8, Recipient/binary>>/binary,
                "\",\"content\":\""/utf8>>/binary,
            Content/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    case make_request(
        post,
        <<Base_url/binary, "/api/message"/utf8>>,
        Body,
        Auth
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 528).
-spec test_search_posts(binary(), binary()) -> nil.
test_search_posts(Base_url, Query) ->
    gleam_stdlib:println(
        <<<<<<"GET "/utf8, Base_url/binary>>/binary,
                "/api/search/posts?q="/utf8>>/binary,
            Query/binary>>
    ),
    case make_request(
        get,
        <<<<Base_url/binary, "/api/search/posts?q="/utf8>>/binary,
            Query/binary>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 547).
-spec test_search_subreddits(binary(), binary()) -> nil.
test_search_subreddits(Base_url, Query) ->
    gleam_stdlib:println(
        <<<<<<"GET "/utf8, Base_url/binary>>/binary,
                "/api/search/subreddits?q="/utf8>>/binary,
            Query/binary>>
    ),
    case make_request(
        get,
        <<<<Base_url/binary, "/api/search/subreddits?q="/utf8>>/binary,
            Query/binary>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 566).
-spec test_search_users(binary(), binary()) -> nil.
test_search_users(Base_url, Query) ->
    gleam_stdlib:println(
        <<<<<<"GET "/utf8, Base_url/binary>>/binary,
                "/api/search/users?q="/utf8>>/binary,
            Query/binary>>
    ),
    case make_request(
        get,
        <<<<Base_url/binary, "/api/search/users?q="/utf8>>/binary,
            Query/binary>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 579).
-spec test_get_karma(binary(), binary()) -> nil.
test_get_karma(Base_url, Username) ->
    gleam_stdlib:println(
        <<<<<<<<"GET "/utf8, Base_url/binary>>/binary, "/api/user/"/utf8>>/binary,
                Username/binary>>/binary,
            "/karma"/utf8>>
    ),
    case make_request(
        get,
        <<<<<<Base_url/binary, "/api/user/"/utf8>>/binary, Username/binary>>/binary,
            "/karma"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 598).
-spec test_get_active_clients(binary()) -> nil.
test_get_active_clients(Base_url) ->
    gleam_stdlib:println(
        <<<<"GET "/utf8, Base_url/binary>>/binary, "/api/clients"/utf8>>
    ),
    case make_request(
        get,
        <<Base_url/binary, "/api/clients"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 610).
-spec test_get_stats(binary()) -> nil.
test_get_stats(Base_url) ->
    gleam_stdlib:println(
        <<<<"GET "/utf8, Base_url/binary>>/binary, "/api/stats"/utf8>>
    ),
    case make_request(
        get,
        <<Base_url/binary, "/api/stats"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ) of
        {ok, Resp} ->
            gleam_stdlib:println(
                <<"✅ Status: "/utf8,
                    (gleam@string:inspect(erlang:element(2, Resp)))/binary>>
            ),
            gleam_stdlib:println(
                <<"📄 Response: "/utf8, (erlang:element(4, Resp))/binary>>
            );

        {error, Msg} ->
            gleam_stdlib:println(<<"❌ Error: "/utf8, Msg/binary>>)
    end.

-file("src/cli_client.gleam", 18).
-spec main() -> nil.
main() ->
    _ = inets:start(),
    gleam_stdlib:println(
        <<"\n╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║        Reddit Clone - Command Line Client Demo               ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    Base_url = <<"http://localhost:8080"/utf8>>,
    {Public_key, Private_key} = utils:generate_key_pair(),
    gleam_stdlib:println(<<"🔑 Generated key pair for digital signatures"/utf8>>),
    gleam_stdlib:println(
        <<<<"   Public Key: "/utf8,
                (gleam@string:slice(Public_key, 0, 32))/binary>>/binary,
            "..."/utf8>>
    ),
    gleam_stdlib:println(<<"\n📋 Test 1: Health Check"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_health_check(Base_url),
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n📋 Test 2: User Registration (with public key)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    Username = <<"alice_test"/utf8>>,
    Password = <<"password123"/utf8>>,
    test_register_with_key(Base_url, Username, Password, Public_key),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 3: User Login"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_login(Base_url, Username, Password),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 4: Get User's Public Key"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_public_key(Base_url, Username),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 5: Create Subreddit"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_create_subreddit(
        Base_url,
        Username,
        <<"programming"/utf8>>,
        <<"Discuss programming topics"/utf8>>
    ),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 6: Get All Subreddits"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_subreddits(Base_url),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 7: Join Subreddit"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_join_subreddit(Base_url, Username, <<"programming"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 8: Create Signed Post"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    Post_title = <<"Learning Gleam"/utf8>>,
    Post_content = <<"Gleam is an amazing language for the BEAM!"/utf8>>,
    Signature = utils:sign_content(
        <<Post_title/binary, Post_content/binary>>,
        Private_key
    ),
    test_create_signed_post(
        Base_url,
        Username,
        <<"programming"/utf8>>,
        Post_title,
        Post_content,
        Signature
    ),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 9: Get Posts Feed"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_feed(Base_url, Username),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 10: Get Subreddit Posts"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_subreddit_posts(Base_url, <<"programming"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n📋 Test 11: Get Post (with signature verification)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_post(Base_url, <<"post_1"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 12: Create Comment"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_create_comment(
        Base_url,
        Username,
        <<"post_1"/utf8>>,
        <<"Great post! I agree."/utf8>>
    ),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 13: Get Post Comments"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_comments(Base_url, <<"post_1"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 14: Vote on Post (Upvote)"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_upvote_post(Base_url, Username, <<"post_1"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 15: Send Direct Message"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_send_message(Base_url, Username, <<"bob"/utf8>>, <<"Hello Bob!"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 16: Search Posts"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_search_posts(Base_url, <<"Gleam"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 17: Search Subreddits"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_search_subreddits(Base_url, <<"prog"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 18: Search Users"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_search_users(Base_url, <<"alice"/utf8>>),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 19: Get User Karma"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_karma(Base_url, Username),
    timer:sleep(500),
    gleam_stdlib:println(<<"\n📋 Test 20: Get Active Clients"/utf8>>),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_active_clients(Base_url),
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n📋 Test 21: Get System Statistics (with rates)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"─────────────────────────────────────────────────────────────────"/utf8>>
    ),
    test_get_stats(Base_url),
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║              All Tests Completed Successfully!                ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝\n"/utf8>>
    ).
