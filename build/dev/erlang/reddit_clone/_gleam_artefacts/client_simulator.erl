-module(client_simulator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/client_simulator.gleam").
-export([default_config/0, main/0]).
-export_type([sim_config/0, client_state/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type sim_config() :: {sim_config,
        integer(),
        integer(),
        integer(),
        integer(),
        float(),
        float(),
        float(),
        float(),
        float(),
        float(),
        binary()}.

-type client_state() :: {client_state,
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        list(binary()),
        list(binary()),
        boolean(),
        integer()}.

-file("src/client_simulator.gleam", 38).
-spec default_config() -> sim_config().
default_config() ->
    {sim_config,
        10,
        5,
        30000,
        5,
        1.0,
        0.8,
        0.3,
        0.4,
        0.5,
        0.2,
        <<"http://localhost:8080"/utf8>>}.

-file("src/client_simulator.gleam", 275).
-spec wait_for_completions(
    gleam@erlang@process:subject(binary()),
    integer(),
    list(binary())
) -> list(binary()).
wait_for_completions(Subject, Remaining, Completed) ->
    case Remaining of
        0 ->
            gleam_stdlib:println(
                <<<<"   ✅ All "/utf8,
                        (erlang:integer_to_binary(erlang:length(Completed)))/binary>>/binary,
                    " clients completed"/utf8>>
            ),
            Completed;

        N ->
            case gleam@erlang@process:'receive'(Subject, 60000) of
                {ok, Username} ->
                    wait_for_completions(Subject, N - 1, [Username | Completed]);

                {error, _} ->
                    gleam_stdlib:println(
                        <<<<"   ⚠️ Timeout waiting for clients, "/utf8,
                                (erlang:integer_to_binary(N))/binary>>/binary,
                            " remaining"/utf8>>
                    ),
                    Completed
            end
    end.

-file("src/client_simulator.gleam", 352).
-spec choose_action(sim_config()) -> binary().
choose_action(Config) ->
    Rand = utils:random_float(),
    Post_threshold = 0.25,
    Comment_threshold = Post_threshold + 0.25,
    Vote_threshold = Comment_threshold + 0.30,
    case Rand of
        R when R < Post_threshold ->
            <<"post"/utf8>>;

        R@1 when R@1 < Comment_threshold ->
            <<"comment"/utf8>>;

        R@2 when R@2 < Vote_threshold ->
            <<"vote"/utf8>>;

        _ ->
            <<"dm"/utf8>>
    end.

-file("src/client_simulator.gleam", 669).
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

-file("src/client_simulator.gleam", 527).
-spec check_health(binary()) -> {ok, binary()} | {error, binary()}.
check_health(Base_url) ->
    _pipe = make_request(
        get,
        <<Base_url/binary, "/health"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 532).
-spec register_user(binary(), binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
register_user(Base_url, Username, Password, Public_key) ->
    Body = <<<<<<<<<<<<"{\"username\":\""/utf8, Username/binary>>/binary,
                        "\",\"password\":\""/utf8>>/binary,
                    Password/binary>>/binary,
                "\",\"public_key\":\""/utf8>>/binary,
            Public_key/binary>>/binary,
        "\"}"/utf8>>,
    _pipe = make_request(
        post,
        <<Base_url/binary, "/api/register"/utf8>>,
        Body,
        <<""/utf8>>
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 550).
-spec create_subreddit(binary(), binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
create_subreddit(Base_url, Username, Name, Description) ->
    Body = <<<<<<<<"{\"name\":\""/utf8, Name/binary>>/binary,
                "\",\"description\":\""/utf8>>/binary,
            Description/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<Base_url/binary, "/api/subreddit"/utf8>>,
        Body,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 563).
-spec join_subreddit(binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
join_subreddit(Base_url, Username, Subreddit) ->
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<<<<<Base_url/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/join"/utf8>>,
        <<""/utf8>>,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 601).
-spec get_subreddit_posts(binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
get_subreddit_posts(Base_url, Subreddit) ->
    _pipe = make_request(
        get,
        <<<<<<Base_url/binary, "/api/subreddit/"/utf8>>/binary,
                Subreddit/binary>>/binary,
            "/posts"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 631).
-spec vote_on_post(binary(), binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
vote_on_post(Base_url, Username, Post_id, Vote_type) ->
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<<<<<<<Base_url/binary, "/api/post/"/utf8>>/binary, Post_id/binary>>/binary,
                "/"/utf8>>/binary,
            Vote_type/binary>>,
        <<""/utf8>>,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 664).
-spec get_stats(binary()) -> {ok, binary()} | {error, binary()}.
get_stats(Base_url) ->
    _pipe = make_request(
        get,
        <<Base_url/binary, "/api/stats"/utf8>>,
        <<""/utf8>>,
        <<""/utf8>>
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 700).
-spec escape_json_string(binary()) -> binary().
escape_json_string(S) ->
    _pipe = S,
    _pipe@1 = gleam@string:replace(_pipe, <<"\\"/utf8>>, <<"\\\\"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"\""/utf8>>, <<"\\\""/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"\n"/utf8>>, <<"\\n"/utf8>>),
    _pipe@4 = gleam@string:replace(_pipe@3, <<"\r"/utf8>>, <<"\\r"/utf8>>),
    gleam@string:replace(_pipe@4, <<"\t"/utf8>>, <<"\\t"/utf8>>).

-file("src/client_simulator.gleam", 578).
-spec create_post_with_signature(
    binary(),
    binary(),
    binary(),
    binary(),
    binary(),
    binary()
) -> {ok, binary()} | {error, binary()}.
create_post_with_signature(
    Base_url,
    Username,
    Subreddit,
    Title,
    Content,
    Signature
) ->
    Body = <<<<<<<<<<<<<<<<"{\"subreddit\":\""/utf8, Subreddit/binary>>/binary,
                                "\",\"title\":\""/utf8>>/binary,
                            (escape_json_string(Title))/binary>>/binary,
                        "\",\"content\":\""/utf8>>/binary,
                    (escape_json_string(Content))/binary>>/binary,
                "\",\"signature\":\""/utf8>>/binary,
            Signature/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<Base_url/binary, "/api/post"/utf8>>,
        Body,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 614).
-spec create_comment(binary(), binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
create_comment(Base_url, Username, Post_id, Content) ->
    Body = <<<<<<<<"{\"post_id\":\""/utf8, Post_id/binary>>/binary,
                "\",\"content\":\""/utf8>>/binary,
            (escape_json_string(Content))/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<Base_url/binary, "/api/comment"/utf8>>,
        Body,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 647).
-spec send_dm(binary(), binary(), binary(), binary()) -> {ok, binary()} |
    {error, binary()}.
send_dm(Base_url, Username, Recipient, Content) ->
    Body = <<<<<<<<"{\"recipient\":\""/utf8, Recipient/binary>>/binary,
                "\",\"content\":\""/utf8>>/binary,
            (escape_json_string(Content))/binary>>/binary,
        "\"}"/utf8>>,
    Auth = <<"Bearer "/utf8, Username/binary>>,
    _pipe = make_request(
        post,
        <<Base_url/binary, "/api/message"/utf8>>,
        Body,
        Auth
    ),
    gleam@result:map(_pipe, fun(Resp) -> erlang:element(4, Resp) end).

-file("src/client_simulator.gleam", 709).
-spec truncate_string(binary(), integer()) -> binary().
truncate_string(S, Max_len) ->
    case string:length(S) > Max_len of
        true ->
            <<(gleam@string:slice(S, 0, Max_len))/binary, "..."/utf8>>;

        false ->
            S
    end.

-file("src/client_simulator.gleam", 718).
?DOC(
    " Extract a post ID from the JSON response\n"
    " Looks for \"post_\" pattern in the response\n"
).
-spec extract_post_id(binary()) -> {ok, binary()} | {error, nil}.
extract_post_id(Json_response) ->
    case gleam@string:split(Json_response, <<"\"id\":\"post_"/utf8>>) of
        [_, Rest | _] ->
            case gleam@string:split(Rest, <<"\""/utf8>>) of
                [Id_num | _] ->
                    {ok, <<"post_"/utf8, Id_num/binary>>};

                _ ->
                    {error, nil}
            end;

        _ ->
            {error, nil}
    end.

-file("src/client_simulator.gleam", 370).
-spec execute_action(binary(), client_state(), sim_config(), list(binary())) -> client_state().
execute_action(Action, Client, Config, _) ->
    case Action of
        <<"post"/utf8>> ->
            case erlang:element(7, Client) of
                [] ->
                    Client;

                Subs ->
                    Sub_index = utils:random_int(erlang:length(Subs)),
                    case gleam@list:drop(Subs, Sub_index - 1) of
                        [Subreddit | _] ->
                            Title = utils:random_post_title(),
                            Content = utils:random_post_content(),
                            Signature = utils:sign_content(
                                <<Title/binary, Content/binary>>,
                                erlang:element(6, Client)
                            ),
                            case create_post_with_signature(
                                erlang:element(12, Config),
                                erlang:element(3, Client),
                                Subreddit,
                                Title,
                                Content,
                                Signature
                            ) of
                                {ok, _} ->
                                    gleam_stdlib:println(
                                        <<<<<<"   ["/utf8,
                                                    (erlang:element(3, Client))/binary>>/binary,
                                                "] Posted in r/"/utf8>>/binary,
                                            Subreddit/binary>>
                                    ),
                                    Client;

                                {error, _} ->
                                    gleam_stdlib:println(
                                        <<<<<<"   ["/utf8,
                                                    (erlang:element(3, Client))/binary>>/binary,
                                                "] Failed to post in r/"/utf8>>/binary,
                                            Subreddit/binary>>
                                    ),
                                    Client
                            end;

                        _ ->
                            Client
                    end
            end;

        <<"comment"/utf8>> ->
            case erlang:element(7, Client) of
                [] ->
                    Client;

                [Sub | _] ->
                    case get_subreddit_posts(erlang:element(12, Config), Sub) of
                        {ok, Posts_json} ->
                            case extract_post_id(Posts_json) of
                                {ok, Post_id} ->
                                    Content@1 = utils:random_comment(),
                                    case create_comment(
                                        erlang:element(12, Config),
                                        erlang:element(3, Client),
                                        Post_id,
                                        Content@1
                                    ) of
                                        {ok, _} ->
                                            gleam_stdlib:println(
                                                <<<<<<"   ["/utf8,
                                                            (erlang:element(
                                                                3,
                                                                Client
                                                            ))/binary>>/binary,
                                                        "] Commented on "/utf8>>/binary,
                                                    Post_id/binary>>
                                            );

                                        {error, _} ->
                                            gleam_stdlib:println(
                                                <<<<"   ["/utf8,
                                                        (erlang:element(
                                                            3,
                                                            Client
                                                        ))/binary>>/binary,
                                                    "] Failed to comment"/utf8>>
                                            )
                                    end,
                                    Client;

                                {error, _} ->
                                    gleam_stdlib:println(
                                        <<<<"   ["/utf8,
                                                (erlang:element(3, Client))/binary>>/binary,
                                            "] No posts to comment on"/utf8>>
                                    ),
                                    Client
                            end;

                        {error, _} ->
                            Client
                    end
            end;

        <<"vote"/utf8>> ->
            case erlang:element(7, Client) of
                [] ->
                    Client;

                [Sub@1 | _] ->
                    case get_subreddit_posts(erlang:element(12, Config), Sub@1) of
                        {ok, Posts_json@1} ->
                            case extract_post_id(Posts_json@1) of
                                {ok, Post_id@1} ->
                                    Vote_type = case utils:random_float() < 0.7 of
                                        true ->
                                            <<"upvote"/utf8>>;

                                        false ->
                                            <<"downvote"/utf8>>
                                    end,
                                    case vote_on_post(
                                        erlang:element(12, Config),
                                        erlang:element(3, Client),
                                        Post_id@1,
                                        Vote_type
                                    ) of
                                        {ok, _} ->
                                            gleam_stdlib:println(
                                                <<<<<<<<<<"   ["/utf8,
                                                                    (erlang:element(
                                                                        3,
                                                                        Client
                                                                    ))/binary>>/binary,
                                                                "] "/utf8>>/binary,
                                                            Vote_type/binary>>/binary,
                                                        "d "/utf8>>/binary,
                                                    Post_id@1/binary>>
                                            );

                                        {error, _} ->
                                            gleam_stdlib:println(
                                                <<<<"   ["/utf8,
                                                        (erlang:element(
                                                            3,
                                                            Client
                                                        ))/binary>>/binary,
                                                    "] Failed to vote"/utf8>>
                                            )
                                    end,
                                    Client;

                                {error, _} ->
                                    gleam_stdlib:println(
                                        <<<<"   ["/utf8,
                                                (erlang:element(3, Client))/binary>>/binary,
                                            "] No posts to vote on"/utf8>>
                                    ),
                                    Client
                            end;

                        {error, _} ->
                            Client
                    end
            end;

        <<"dm"/utf8>> ->
            Recipient_num = utils:random_int(erlang:element(2, Config)),
            Recipient_num_safe = case Recipient_num of
                0 ->
                    1;

                N ->
                    N
            end,
            Recipient = <<"user_"/utf8,
                (erlang:integer_to_binary(Recipient_num_safe))/binary>>,
            Content@2 = utils:random_dm_content(),
            case send_dm(
                erlang:element(12, Config),
                erlang:element(3, Client),
                Recipient,
                Content@2
            ) of
                {ok, _} ->
                    gleam_stdlib:println(
                        <<<<<<"   ["/utf8, (erlang:element(3, Client))/binary>>/binary,
                                "] Sent DM to "/utf8>>/binary,
                            Recipient/binary>>
                    );

                {error, _} ->
                    gleam_stdlib:println(
                        <<<<"   ["/utf8, (erlang:element(3, Client))/binary>>/binary,
                            "] Failed to send DM"/utf8>>
                    )
            end,
            Client;

        _ ->
            Client
    end.

-file("src/client_simulator.gleam", 307).
-spec run_client_actions(client_state(), sim_config(), list(binary())) -> nil.
run_client_actions(Client, Config, All_subreddits) ->
    case erlang:element(10, Client) of
        0 ->
            gleam_stdlib:println(
                <<<<"   ["/utf8, (erlang:element(3, Client))/binary>>/binary,
                    "] Completed all actions"/utf8>>
            );

        _ ->
            Is_connected = utils:random_float() < erlang:element(7, Config),
            case Is_connected of
                false ->
                    gleam_stdlib:println(
                        <<<<"   ["/utf8, (erlang:element(3, Client))/binary>>/binary,
                            "] Disconnected, skipping action"/utf8>>
                    ),
                    timer:sleep(100),
                    run_client_actions(
                        {client_state,
                            erlang:element(2, Client),
                            erlang:element(3, Client),
                            erlang:element(4, Client),
                            erlang:element(5, Client),
                            erlang:element(6, Client),
                            erlang:element(7, Client),
                            erlang:element(8, Client),
                            erlang:element(9, Client),
                            erlang:element(10, Client) - 1},
                        Config,
                        All_subreddits
                    );

                true ->
                    Action = choose_action(Config),
                    New_client = execute_action(
                        Action,
                        Client,
                        Config,
                        All_subreddits
                    ),
                    timer:sleep(utils:random_int(500) + 100),
                    run_client_actions(
                        {client_state,
                            erlang:element(2, New_client),
                            erlang:element(3, New_client),
                            erlang:element(4, New_client),
                            erlang:element(5, New_client),
                            erlang:element(6, New_client),
                            erlang:element(7, New_client),
                            erlang:element(8, New_client),
                            erlang:element(9, New_client),
                            erlang:element(10, Client) - 1},
                        Config,
                        All_subreddits
                    )
            end
    end.

-file("src/client_simulator.gleam", 119).
-spec run_simulation(sim_config()) -> nil.
run_simulation(Config) ->
    Start_time = utils:current_timestamp(),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(<<"📌 Phase 1: Creating Subreddits"/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    Admin_username = <<"admin_"/utf8,
        (erlang:integer_to_binary(utils:random_int(9999)))/binary>>,
    {Admin_pub, _} = utils:generate_key_pair(),
    _ = register_user(
        erlang:element(12, Config),
        Admin_username,
        <<"admin123"/utf8>>,
        Admin_pub
    ),
    Subreddit_names = begin
        _pipe = gleam@list:range(1, erlang:element(3, Config)),
        gleam@list:map(
            _pipe,
            fun(I) ->
                Name = <<"subreddit_"/utf8,
                    (erlang:integer_to_binary(I))/binary>>,
                Desc = <<"Test subreddit number "/utf8,
                    (erlang:integer_to_binary(I))/binary>>,
                _ = create_subreddit(
                    erlang:element(12, Config),
                    Admin_username,
                    Name,
                    Desc
                ),
                gleam_stdlib:println(<<"   Created: r/"/utf8, Name/binary>>),
                Name
            end
        )
    end,
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(<<"👥 Phase 2: Registering Users"/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    Clients = begin
        _pipe@1 = gleam@list:range(1, erlang:element(2, Config)),
        gleam@list:map(
            _pipe@1,
            fun(I@1) ->
                Username = <<"user_"/utf8,
                    (erlang:integer_to_binary(I@1))/binary>>,
                Password = <<"pass_"/utf8,
                    (erlang:integer_to_binary(I@1))/binary>>,
                {Pub_key, Priv_key} = utils:generate_key_pair(),
                _ = register_user(
                    erlang:element(12, Config),
                    Username,
                    Password,
                    Pub_key
                ),
                gleam_stdlib:println(
                    <<"   Registered: "/utf8, Username/binary>>
                ),
                Num_to_join = utils:zipf_sample(
                    erlang:element(3, Config),
                    erlang:element(6, Config)
                ),
                Subs_to_join = gleam@list:take(Subreddit_names, Num_to_join),
                gleam@list:each(
                    Subs_to_join,
                    fun(Sub) ->
                        _ = join_subreddit(
                            erlang:element(12, Config),
                            Username,
                            Sub
                        )
                    end
                ),
                {client_state,
                    Username,
                    Username,
                    Password,
                    Pub_key,
                    Priv_key,
                    Subs_to_join,
                    [],
                    true,
                    erlang:element(5, Config)}
            end
        )
    end,
    timer:sleep(500),
    gleam_stdlib:println(
        <<"\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(<<"📝 Phase 2.5: Creating Seed Posts"/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    _pipe@2 = gleam@list:take(Clients, 3),
    gleam@list:each(_pipe@2, fun(Client) -> case erlang:element(7, Client) of
                [Sub@1 | _] ->
                    Title = utils:random_post_title(),
                    Content = utils:random_post_content(),
                    Signature = utils:sign_content(
                        <<Title/binary, Content/binary>>,
                        erlang:element(6, Client)
                    ),
                    case create_post_with_signature(
                        erlang:element(12, Config),
                        erlang:element(3, Client),
                        Sub@1,
                        Title,
                        Content,
                        Signature
                    ) of
                        {ok, _} ->
                            gleam_stdlib:println(
                                <<<<<<"   "/utf8,
                                            (erlang:element(3, Client))/binary>>/binary,
                                        " created seed post in r/"/utf8>>/binary,
                                    Sub@1/binary>>
                            );

                        {error, _} ->
                            gleam_stdlib:println(
                                <<"   Failed to create seed post"/utf8>>
                            )
                    end;

                _ ->
                    nil
            end end),
    timer:sleep(300),
    gleam_stdlib:println(
        <<"\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(
        <<"🚀 Phase 3: Running Concurrent Client Simulation"/utf8>>
    ),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"   Spawning "/utf8,
                (erlang:integer_to_binary(erlang:element(2, Config)))/binary>>/binary,
            " concurrent client processes...\n"/utf8>>
    ),
    Completion_subject = gleam@erlang@process:new_subject(),
    gleam@list:each(
        Clients,
        fun(Client@1) ->
            proc_lib:spawn(
                fun() ->
                    run_client_actions(Client@1, Config, Subreddit_names),
                    gleam@erlang@process:send(
                        Completion_subject,
                        erlang:element(3, Client@1)
                    )
                end
            )
        end
    ),
    gleam_stdlib:println(<<"   Waiting for all clients to complete..."/utf8>>),
    wait_for_completions(Completion_subject, erlang:element(2, Config), []),
    End_time = utils:current_timestamp(),
    Duration_ms = End_time - Start_time,
    gleam_stdlib:println(
        <<"\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(<<"📊 Phase 4: Simulation Results"/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    case get_stats(erlang:element(12, Config)) of
        {ok, Stats_body} ->
            gleam_stdlib:println(<<"   Server Statistics:"/utf8>>),
            gleam_stdlib:println(
                <<"   "/utf8, (truncate_string(Stats_body, 500))/binary>>
            );

        {error, _} ->
            gleam_stdlib:println(<<"   Failed to fetch final statistics"/utf8>>)
    end,
    gleam_stdlib:println(
        <<<<"\n   Simulation Duration: "/utf8,
                (erlang:integer_to_binary(Duration_ms))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<"   Total Client Processes: "/utf8,
            (erlang:integer_to_binary(erlang:element(2, Config)))/binary>>
    ),
    gleam_stdlib:println(
        <<"   Actions per Client: "/utf8,
            (erlang:integer_to_binary(erlang:element(5, Config)))/binary>>
    ),
    gleam_stdlib:println(
        <<"\n╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║              Simulation Completed Successfully!               ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝\n"/utf8>>
    ).

-file("src/client_simulator.gleam", 76).
-spec main() -> nil.
main() ->
    _ = inets:start(),
    gleam_stdlib:println(
        <<"\n╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║     Reddit Clone - Multi-Client Concurrent Simulator         ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    Config = default_config(),
    gleam_stdlib:println(<<"📋 Simulation Configuration:"/utf8>>),
    gleam_stdlib:println(
        <<"   - Users: "/utf8,
            (erlang:integer_to_binary(erlang:element(2, Config)))/binary>>
    ),
    gleam_stdlib:println(
        <<"   - Subreddits: "/utf8,
            (erlang:integer_to_binary(erlang:element(3, Config)))/binary>>
    ),
    gleam_stdlib:println(
        <<"   - Actions per user: "/utf8,
            (erlang:integer_to_binary(erlang:element(5, Config)))/binary>>
    ),
    gleam_stdlib:println(
        <<<<"   - Duration: "/utf8,
                (erlang:integer_to_binary(erlang:element(4, Config)))/binary>>/binary,
            "ms"/utf8>>
    ),
    gleam_stdlib:println(
        <<<<"   - Zipf alpha: "/utf8,
                (gleam_stdlib:float_to_string(erlang:element(6, Config)))/binary>>/binary,
            "\n"/utf8>>
    ),
    gleam_stdlib:println(<<"🔍 Checking server health..."/utf8>>),
    case check_health(erlang:element(12, Config)) of
        {ok, _} ->
            gleam_stdlib:println(<<"✅ Server is healthy\n"/utf8>>),
            run_simulation(Config);

        {error, Msg} ->
            gleam_stdlib:println(
                <<"❌ Server health check failed: "/utf8, Msg/binary>>
            ),
            gleam_stdlib:println(
                <<"   Make sure the server is running with: gleam run"/utf8>>
            )
    end.
