-module(api_server).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api_server.gleam").
-export([start/2]).
-export_type([api_context/0]).

-type api_context() :: {api_context,
        gleam@erlang@process:subject(types:engine_message())}.

-file("src/api_server.gleam", 92).
-spec log_request(binary(), binary(), binary()) -> nil.
log_request(Method, Path, Status) ->
    Timestamp = utils:current_timestamp(),
    gleam_stdlib:println(
        <<<<<<<<<<<<<<"["/utf8, (erlang:integer_to_binary(Timestamp))/binary>>/binary,
                                "] "/utf8>>/binary,
                            Status/binary>>/binary,
                        " "/utf8>>/binary,
                    Method/binary>>/binary,
                " "/utf8>>/binary,
            Path/binary>>
    ).

-file("src/api_server.gleam", 106).
-spec log_response(binary(), binary(), integer(), integer()) -> nil.
log_response(Method, Path, Status, Duration_ms) ->
    Timestamp = utils:current_timestamp(),
    Status_emoji = case Status of
        S when (S >= 200) andalso (S < 300) ->
            <<"✅"/utf8>>;

        S@1 when (S@1 >= 400) andalso (S@1 < 500) ->
            <<"⚠️"/utf8>>;

        S@2 when S@2 >= 500 ->
            <<"❌"/utf8>>;

        _ ->
            <<"📋"/utf8>>
    end,
    gleam_stdlib:println(
        <<<<<<<<<<<<<<<<<<<<<<<<"["/utf8,
                                                        (erlang:integer_to_binary(
                                                            Timestamp
                                                        ))/binary>>/binary,
                                                    "] "/utf8>>/binary,
                                                Status_emoji/binary>>/binary,
                                            " "/utf8>>/binary,
                                        Method/binary>>/binary,
                                    " "/utf8>>/binary,
                                Path/binary>>/binary,
                            " -> "/utf8>>/binary,
                        (erlang:integer_to_binary(Status))/binary>>/binary,
                    " ("/utf8>>/binary,
                (erlang:integer_to_binary(Duration_ms))/binary>>/binary,
            "ms)"/utf8>>
    ).

-file("src/api_server.gleam", 131).
-spec http_method_to_string(gleam@http:method()) -> binary().
http_method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        connect ->
            <<"CONNECT"/utf8>>;

        trace ->
            <<"TRACE"/utf8>>;

        {other, S} ->
            S
    end.

-file("src/api_server.gleam", 146).
-spec handle_cors_preflight() -> gleam@http@response:response(mist:response_data()).
handle_cors_preflight() ->
    _pipe = gleam@http@response:new(204),
    gleam@http@response:set_body(_pipe, {bytes, gleam@bytes_tree:new()}).

-file("src/api_server.gleam", 484).
-spec verify_post_signature(types:post(), api_context()) -> gleam@option:option(boolean()).
verify_post_signature(Post, Context) ->
    case erlang:element(12, Post) of
        {some, Signature} ->
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {get_user_public_key, erlang:element(4, Post), Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 2000) of
                {ok, {success, {public_key_data, {some, Public_key}}}} ->
                    Content_to_verify = <<(erlang:element(5, Post))/binary,
                        (erlang:element(6, Post))/binary>>,
                    Is_valid = utils:verify_signature(
                        Content_to_verify,
                        Signature,
                        Public_key
                    ),
                    {some, Is_valid};

                _ ->
                    none
            end;

        none ->
            none
    end.

-file("src/api_server.gleam", 790).
-spec json_response(integer(), binary()) -> gleam@http@response:response(mist:response_data()).
json_response(Status, Body) ->
    _pipe = gleam@http@response:new(Status),
    _pipe@1 = gleam@http@response:set_body(
        _pipe,
        {bytes, gleam_stdlib:wrap_list(Body)}
    ),
    gleam@http@response:prepend_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        <<"application/json"/utf8>>
    ).

-file("src/api_server.gleam", 796).
-spec error_response(integer(), binary()) -> gleam@http@response:response(mist:response_data()).
error_response(Status, Message) ->
    Body = json_codec:encode_error_response(Message),
    json_response(Status, Body).

-file("src/api_server.gleam", 277).
-spec handle_get_user_karma(binary(), api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_user_karma(Username, Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_user_karma, Username, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {karma_data, Karma}}} ->
            Response_body = json_codec:encode_karma_response(Karma),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(404, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 295).
-spec handle_get_user_public_key(binary(), api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_user_public_key(Username, Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_user_public_key, Username, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {public_key_data, Public_key}}} ->
            Response_body = json_codec:encode_public_key_response(Public_key),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(404, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 320).
-spec handle_get_subreddits(api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_subreddits(Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_all_subreddits, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {subreddits_data, Subreddits}}} ->
            Response_body = json_codec:encode_subreddits_response(Subreddits),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 335).
-spec handle_get_subreddit(binary(), api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_subreddit(Name, Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_all_subreddits, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {subreddits_data, Subreddits}}} ->
            case gleam@list:find(
                Subreddits,
                fun(S) -> erlang:element(2, S) =:= Name end
            ) of
                {ok, Subreddit} ->
                    Response_body = json_codec:encode_subreddits_response(
                        [Subreddit]
                    ),
                    json_response(200, Response_body);

                {error, _} ->
                    error_response(404, <<"Subreddit not found"/utf8>>)
            end;

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 459).
-spec handle_get_post(binary(), api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_post(Post_id, Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_post, Post_id, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {post_data, Post, Comments}}} ->
            Signature_valid = verify_post_signature(Post, Context),
            Response_body = json_codec:encode_post_with_comments_verified(
                Post,
                Comments,
                Signature_valid
            ),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(404, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 550).
-spec handle_get_post_comments(binary(), api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_post_comments(Post_id, Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_comments, Post_id, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {comments_data, Comments}}} ->
            Response_body = json_codec:encode_comments_response(Comments),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 743).
-spec handle_get_stats(api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_stats(Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_stats, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {stats_data, Stats}}} ->
            Response_body = json_codec:encode_stats_response(Stats),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 762).
-spec handle_get_active_clients(api_context()) -> gleam@http@response:response(mist:response_data()).
handle_get_active_clients(Context) ->
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_active_clients, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {active_clients_data, Count, Users}}} ->
            Response_body = json_codec:encode_active_clients_response(
                Count,
                Users
            ),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 782).
-spec handle_not_found() -> gleam@http@response:response(mist:response_data()).
handle_not_found() ->
    error_response(404, <<"Endpoint not found"/utf8>>).

-file("src/api_server.gleam", 801).
-spec with_body(
    gleam@http@request:request(mist@internal@http:connection()),
    fun((binary()) -> gleam@http@response:response(mist:response_data()))
) -> gleam@http@response:response(mist:response_data()).
with_body(Req, Handler) ->
    case mist:read_body(Req, 1048576) of
        {ok, Req_with_body} ->
            case gleam@bit_array:to_string(erlang:element(4, Req_with_body)) of
                {ok, Body_string} ->
                    Handler(Body_string);

                {error, _} ->
                    error_response(
                        400,
                        <<"Invalid UTF-8 in request body"/utf8>>
                    )
            end;

        {error, _} ->
            error_response(400, <<"Failed to read request body"/utf8>>)
    end.

-file("src/api_server.gleam", 227).
-spec handle_register(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_register(Req, Context) ->
    with_body(Req, fun(Body) -> case json_codec:decode_register_request(Body) of
                {ok, {Username, Password, Public_key}} ->
                    gleam@erlang@process:send(
                        erlang:element(2, Context),
                        {register_user, Username, Password, Public_key}
                    ),
                    Response_body = json_codec:encode_success_response(
                        <<"User registered successfully"/utf8>>
                    ),
                    json_response(201, Response_body);

                {error, Msg} ->
                    error_response(400, Msg)
            end end).

-file("src/api_server.gleam", 249).
-spec handle_login(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_login(Req, Context) ->
    with_body(Req, fun(Body) -> case json_codec:decode_login_request(Body) of
                {ok, {Username, Password}} ->
                    Response_subject = gleam@erlang@process:new_subject(),
                    gleam@erlang@process:send(
                        erlang:element(2, Context),
                        {authenticate_user,
                            Username,
                            Password,
                            Response_subject}
                    ),
                    case gleam@erlang@process:'receive'(Response_subject, 5000) of
                        {ok, {success, {user_authenticated, User_id, Uname}}} ->
                            Response_body = json_codec:encode_user_response(
                                User_id,
                                Uname
                            ),
                            json_response(200, Response_body);

                        {ok, {success, _}} ->
                            error_response(
                                500,
                                <<"Unexpected response type"/utf8>>
                            );

                        {ok, {engine_error, Msg}} ->
                            error_response(401, Msg);

                        {error, _} ->
                            error_response(500, <<"Request timeout"/utf8>>)
                    end;

                {error, Msg@1} ->
                    error_response(400, Msg@1)
            end end).

-file("src/api_server.gleam", 816).
-spec with_auth_header(
    gleam@http@request:request(mist@internal@http:connection()),
    fun((binary()) -> gleam@http@response:response(mist:response_data()))
) -> gleam@http@response:response(mist:response_data()).
with_auth_header(Req, Handler) ->
    case gleam@http@request:get_header(Req, <<"authorization"/utf8>>) of
        {ok, Auth} ->
            case gleam_stdlib:string_starts_with(Auth, <<"Bearer "/utf8>>) of
                true ->
                    User_id = gleam@string:drop_start(Auth, 7),
                    Handler(User_id);

                false ->
                    error_response(401, <<"Invalid authorization header"/utf8>>)
            end;

        {error, _} ->
            error_response(401, <<"Missing authorization header"/utf8>>)
    end.

-file("src/api_server.gleam", 360).
-spec handle_create_subreddit(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_create_subreddit(Req, Context) ->
    with_body(
        Req,
        fun(Body) ->
            with_auth_header(
                Req,
                fun(User_id) ->
                    case json_codec:decode_create_subreddit_request(Body) of
                        {ok, {Name, Description}} ->
                            gleam@erlang@process:send(
                                erlang:element(2, Context),
                                {create_subreddit, User_id, Name, Description}
                            ),
                            Response_body = json_codec:encode_success_response(
                                <<<<"Subreddit '"/utf8, Name/binary>>/binary,
                                    "' created"/utf8>>
                            ),
                            json_response(201, Response_body);

                        {error, Msg} ->
                            error_response(400, Msg)
                    end
                end
            )
        end
    ).

-file("src/api_server.gleam", 382).
-spec handle_join_subreddit(
    binary(),
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_join_subreddit(Subreddit, Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {join_subreddit, User_id, Subreddit}
            ),
            Response_body = json_codec:encode_success_response(
                <<"Joined r/"/utf8, Subreddit/binary>>
            ),
            json_response(200, Response_body)
        end
    ).

-file("src/api_server.gleam", 396).
-spec handle_leave_subreddit(
    binary(),
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_leave_subreddit(Subreddit, Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {leave_subreddit, User_id, Subreddit}
            ),
            Response_body = json_codec:encode_success_response(
                <<"Left r/"/utf8, Subreddit/binary>>
            ),
            json_response(200, Response_body)
        end
    ).

-file("src/api_server.gleam", 511).
-spec handle_create_post(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_create_post(Req, Context) ->
    with_body(
        Req,
        fun(Body) ->
            with_auth_header(
                Req,
                fun(User_id) ->
                    case json_codec:decode_create_post_request(Body) of
                        {ok, {Subreddit, Title, Content, Signature}} ->
                            gleam@erlang@process:send(
                                erlang:element(2, Context),
                                {create_post,
                                    User_id,
                                    Subreddit,
                                    Title,
                                    Content,
                                    Signature}
                            ),
                            Response_body = json_codec:encode_success_response(
                                <<"Post created"/utf8>>
                            ),
                            json_response(201, Response_body);

                        {error, Msg} ->
                            error_response(400, Msg)
                    end
                end
            )
        end
    ).

-file("src/api_server.gleam", 532).
-spec handle_vote_post(
    binary(),
    types:vote(),
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_vote_post(Post_id, Vote, Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {vote_post, User_id, Post_id, Vote}
            ),
            Response_body = json_codec:encode_success_response(
                <<"Vote cast"/utf8>>
            ),
            json_response(200, Response_body)
        end
    ).

-file("src/api_server.gleam", 568).
-spec handle_create_comment(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_create_comment(Req, Context) ->
    with_body(
        Req,
        fun(Body) ->
            with_auth_header(
                Req,
                fun(User_id) ->
                    case json_codec:decode_create_comment_request(Body) of
                        {ok, {Post_id, Parent_id, Content}} ->
                            gleam@erlang@process:send(
                                erlang:element(2, Context),
                                {create_comment,
                                    User_id,
                                    Post_id,
                                    Parent_id,
                                    Content}
                            ),
                            Response_body = json_codec:encode_success_response(
                                <<"Comment created"/utf8>>
                            ),
                            json_response(201, Response_body);

                        {error, Msg} ->
                            error_response(400, Msg)
                    end
                end
            )
        end
    ).

-file("src/api_server.gleam", 589).
-spec handle_vote_comment(
    binary(),
    types:vote(),
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_vote_comment(Comment_id, Vote, Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {vote_comment, User_id, Comment_id, Vote}
            ),
            Response_body = json_codec:encode_success_response(
                <<"Vote cast"/utf8>>
            ),
            json_response(200, Response_body)
        end
    ).

-file("src/api_server.gleam", 607).
-spec handle_get_messages(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_get_messages(Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {get_messages, User_id, Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 5000) of
                {ok, {success, {messages_data, Messages}}} ->
                    Response_body = json_codec:encode_messages_response(
                        Messages
                    ),
                    json_response(200, Response_body);

                {ok, {success, _}} ->
                    error_response(500, <<"Unexpected response type"/utf8>>);

                {ok, {engine_error, Msg}} ->
                    error_response(500, Msg);

                {error, _} ->
                    error_response(500, <<"Request timeout"/utf8>>)
            end
        end
    ).

-file("src/api_server.gleam", 627).
-spec handle_send_message(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_send_message(Req, Context) ->
    with_body(
        Req,
        fun(Body) ->
            with_auth_header(
                Req,
                fun(User_id) ->
                    case json_codec:decode_send_message_request(Body) of
                        {ok, {Recipient, Content, Reply_to}} ->
                            gleam@erlang@process:send(
                                erlang:element(2, Context),
                                {send_direct_message,
                                    User_id,
                                    Recipient,
                                    Content,
                                    Reply_to}
                            ),
                            Response_body = json_codec:encode_success_response(
                                <<"Message sent"/utf8>>
                            ),
                            json_response(201, Response_body);

                        {error, Msg} ->
                            error_response(400, Msg)
                    end
                end
            )
        end
    ).

-file("src/api_server.gleam", 835).
-spec get_query_param(
    gleam@http@request:request(mist@internal@http:connection()),
    binary()
) -> gleam@option:option(binary()).
get_query_param(Req, Key) ->
    case gleam@http@request:get_query(Req) of
        {ok, Query} ->
            _pipe = gleam@list:find(
                Query,
                fun(Pair) -> erlang:element(1, Pair) =:= Key end
            ),
            _pipe@1 = gleam@result:map(
                _pipe,
                fun(Pair@1) -> erlang:element(2, Pair@1) end
            ),
            gleam@option:from_result(_pipe@1);

        {error, _} ->
            none
    end.

-file("src/api_server.gleam", 846).
-spec parse_limit(gleam@option:option(binary()), integer()) -> integer().
parse_limit(Opt, Default) ->
    case Opt of
        {some, Limit_str} ->
            case gleam_stdlib:parse_int(Limit_str) of
                {ok, Limit} ->
                    Limit;

                {error, _} ->
                    Default
            end;

        none ->
            Default
    end.

-file("src/api_server.gleam", 413).
-spec handle_get_feed(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_get_feed(Req, Context) ->
    with_auth_header(
        Req,
        fun(User_id) ->
            Limit = begin
                _pipe = get_query_param(Req, <<"limit"/utf8>>),
                parse_limit(_pipe, 25)
            end,
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {get_feed, User_id, Limit, Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 5000) of
                {ok, {success, {feed_data, Posts}}} ->
                    Response_body = json_codec:encode_posts_response(Posts),
                    json_response(200, Response_body);

                {ok, {success, _}} ->
                    error_response(500, <<"Unexpected response type"/utf8>>);

                {ok, {engine_error, Msg}} ->
                    error_response(500, Msg);

                {error, _} ->
                    error_response(500, <<"Request timeout"/utf8>>)
            end
        end
    ).

-file("src/api_server.gleam", 435).
-spec handle_get_subreddit_feed(
    binary(),
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_get_subreddit_feed(Subreddit, Req, Context) ->
    Limit = begin
        _pipe = get_query_param(Req, <<"limit"/utf8>>),
        parse_limit(_pipe, 25)
    end,
    Response_subject = gleam@erlang@process:new_subject(),
    gleam@erlang@process:send(
        erlang:element(2, Context),
        {get_subreddit_feed, Subreddit, Limit, Response_subject}
    ),
    case gleam@erlang@process:'receive'(Response_subject, 5000) of
        {ok, {success, {feed_data, Posts}}} ->
            Response_body = json_codec:encode_posts_response(Posts),
            json_response(200, Response_body);

        {ok, {success, _}} ->
            error_response(500, <<"Unexpected response type"/utf8>>);

        {ok, {engine_error, Msg}} ->
            error_response(500, Msg);

        {error, _} ->
            error_response(500, <<"Request timeout"/utf8>>)
    end.

-file("src/api_server.gleam", 652).
-spec handle_search_posts(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_search_posts(Req, Context) ->
    Query = begin
        _pipe = get_query_param(Req, <<"q"/utf8>>),
        gleam@option:unwrap(_pipe, <<""/utf8>>)
    end,
    Limit = begin
        _pipe@1 = get_query_param(Req, <<"limit"/utf8>>),
        parse_limit(_pipe@1, 25)
    end,
    case gleam@string:is_empty(Query) of
        true ->
            error_response(400, <<"Search query 'q' is required"/utf8>>);

        false ->
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {search_posts, Query, Limit, Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 5000) of
                {ok, {success, {post_search_results, Posts}}} ->
                    Response_body = json_codec:encode_posts_response(Posts),
                    json_response(200, Response_body);

                {ok, {success, _}} ->
                    error_response(500, <<"Unexpected response type"/utf8>>);

                {ok, {engine_error, Msg}} ->
                    error_response(500, Msg);

                {error, _} ->
                    error_response(500, <<"Request timeout"/utf8>>)
            end
    end.

-file("src/api_server.gleam", 681).
-spec handle_search_subreddits(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_search_subreddits(Req, Context) ->
    Query = begin
        _pipe = get_query_param(Req, <<"q"/utf8>>),
        gleam@option:unwrap(_pipe, <<""/utf8>>)
    end,
    Limit = begin
        _pipe@1 = get_query_param(Req, <<"limit"/utf8>>),
        parse_limit(_pipe@1, 25)
    end,
    case gleam@string:is_empty(Query) of
        true ->
            error_response(400, <<"Search query 'q' is required"/utf8>>);

        false ->
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {search_subreddits, Query, Limit, Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 5000) of
                {ok, {success, {subreddit_search_results, Subreddits}}} ->
                    Response_body = json_codec:encode_subreddits_response(
                        Subreddits
                    ),
                    json_response(200, Response_body);

                {ok, {success, _}} ->
                    error_response(500, <<"Unexpected response type"/utf8>>);

                {ok, {engine_error, Msg}} ->
                    error_response(500, Msg);

                {error, _} ->
                    error_response(500, <<"Request timeout"/utf8>>)
            end
    end.

-file("src/api_server.gleam", 710).
-spec handle_search_users(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_search_users(Req, Context) ->
    Query = begin
        _pipe = get_query_param(Req, <<"q"/utf8>>),
        gleam@option:unwrap(_pipe, <<""/utf8>>)
    end,
    Limit = begin
        _pipe@1 = get_query_param(Req, <<"limit"/utf8>>),
        parse_limit(_pipe@1, 25)
    end,
    case gleam@string:is_empty(Query) of
        true ->
            error_response(400, <<"Search query 'q' is required"/utf8>>);

        false ->
            Response_subject = gleam@erlang@process:new_subject(),
            gleam@erlang@process:send(
                erlang:element(2, Context),
                {search_users, Query, Limit, Response_subject}
            ),
            case gleam@erlang@process:'receive'(Response_subject, 5000) of
                {ok, {success, {user_search_results, Users}}} ->
                    Response_body = json_codec:encode_user_search_response(
                        Users
                    ),
                    json_response(200, Response_body);

                {ok, {success, _}} ->
                    error_response(500, <<"Unexpected response type"/utf8>>);

                {ok, {engine_error, Msg}} ->
                    error_response(500, Msg);

                {error, _} ->
                    error_response(500, <<"Request timeout"/utf8>>)
            end
    end.

-file("src/api_server.gleam", 151).
-spec route_request(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
route_request(Req, Context) ->
    Path_segments = gleam@http@request:path_segments(Req),
    case {erlang:element(2, Req), Path_segments} of
        {get, [<<"health"/utf8>>]} ->
            json_response(200, <<"{\"status\":\"ok\"}"/utf8>>);

        {post, [<<"api"/utf8>>, <<"register"/utf8>>]} ->
            handle_register(Req, Context);

        {post, [<<"api"/utf8>>, <<"login"/utf8>>]} ->
            handle_login(Req, Context);

        {get, [<<"api"/utf8>>, <<"user"/utf8>>, Username, <<"karma"/utf8>>]} ->
            handle_get_user_karma(Username, Context);

        {get,
            [<<"api"/utf8>>, <<"user"/utf8>>, Username@1, <<"publickey"/utf8>>]} ->
            handle_get_user_public_key(Username@1, Context);

        {get, [<<"api"/utf8>>, <<"subreddits"/utf8>>]} ->
            handle_get_subreddits(Context);

        {get, [<<"api"/utf8>>, <<"subreddit"/utf8>>, Name]} ->
            handle_get_subreddit(Name, Context);

        {post, [<<"api"/utf8>>, <<"subreddit"/utf8>>]} ->
            handle_create_subreddit(Req, Context);

        {post, [<<"api"/utf8>>, <<"subreddit"/utf8>>, Name@1, <<"join"/utf8>>]} ->
            handle_join_subreddit(Name@1, Req, Context);

        {post, [<<"api"/utf8>>, <<"subreddit"/utf8>>, Name@2, <<"leave"/utf8>>]} ->
            handle_leave_subreddit(Name@2, Req, Context);

        {get, [<<"api"/utf8>>, <<"posts"/utf8>>]} ->
            handle_get_feed(Req, Context);

        {get,
            [<<"api"/utf8>>, <<"subreddit"/utf8>>, Subreddit, <<"posts"/utf8>>]} ->
            handle_get_subreddit_feed(Subreddit, Req, Context);

        {get, [<<"api"/utf8>>, <<"post"/utf8>>, Post_id]} ->
            handle_get_post(Post_id, Context);

        {post, [<<"api"/utf8>>, <<"post"/utf8>>]} ->
            handle_create_post(Req, Context);

        {post, [<<"api"/utf8>>, <<"post"/utf8>>, Post_id@1, <<"upvote"/utf8>>]} ->
            handle_vote_post(Post_id@1, upvote, Req, Context);

        {post,
            [<<"api"/utf8>>, <<"post"/utf8>>, Post_id@2, <<"downvote"/utf8>>]} ->
            handle_vote_post(Post_id@2, downvote, Req, Context);

        {get, [<<"api"/utf8>>, <<"post"/utf8>>, Post_id@3, <<"comments"/utf8>>]} ->
            handle_get_post_comments(Post_id@3, Context);

        {post, [<<"api"/utf8>>, <<"comment"/utf8>>]} ->
            handle_create_comment(Req, Context);

        {post,
            [<<"api"/utf8>>, <<"comment"/utf8>>, Comment_id, <<"upvote"/utf8>>]} ->
            handle_vote_comment(Comment_id, upvote, Req, Context);

        {post,
            [<<"api"/utf8>>,
                <<"comment"/utf8>>,
                Comment_id@1,
                <<"downvote"/utf8>>]} ->
            handle_vote_comment(Comment_id@1, downvote, Req, Context);

        {get, [<<"api"/utf8>>, <<"messages"/utf8>>]} ->
            handle_get_messages(Req, Context);

        {post, [<<"api"/utf8>>, <<"message"/utf8>>]} ->
            handle_send_message(Req, Context);

        {get, [<<"api"/utf8>>, <<"search"/utf8>>, <<"posts"/utf8>>]} ->
            handle_search_posts(Req, Context);

        {get, [<<"api"/utf8>>, <<"search"/utf8>>, <<"subreddits"/utf8>>]} ->
            handle_search_subreddits(Req, Context);

        {get, [<<"api"/utf8>>, <<"search"/utf8>>, <<"users"/utf8>>]} ->
            handle_search_users(Req, Context);

        {get, [<<"api"/utf8>>, <<"stats"/utf8>>]} ->
            handle_get_stats(Context);

        {get, [<<"api"/utf8>>, <<"clients"/utf8>>]} ->
            handle_get_active_clients(Context);

        {_, _} ->
            handle_not_found()
    end.

-file("src/api_server.gleam", 55).
-spec handle_request(
    gleam@http@request:request(mist@internal@http:connection()),
    api_context()
) -> gleam@http@response:response(mist:response_data()).
handle_request(Req, Context) ->
    Start_time = utils:current_timestamp(),
    Method_str = http_method_to_string(erlang:element(2, Req)),
    Path = erlang:element(8, Req),
    log_request(Method_str, Path, <<"RECEIVED"/utf8>>),
    Response = case erlang:element(2, Req) of
        options ->
            handle_cors_preflight();

        _ ->
            route_request(Req, Context)
    end,
    End_time = utils:current_timestamp(),
    Duration_ms = End_time - Start_time,
    log_response(Method_str, Path, erlang:element(2, Response), Duration_ms),
    _pipe = Response,
    _pipe@1 = gleam@http@response:prepend_header(
        _pipe,
        <<"access-control-allow-origin"/utf8>>,
        <<"*"/utf8>>
    ),
    _pipe@2 = gleam@http@response:prepend_header(
        _pipe@1,
        <<"access-control-allow-methods"/utf8>>,
        <<"GET, POST, PUT, DELETE, OPTIONS"/utf8>>
    ),
    gleam@http@response:prepend_header(
        _pipe@2,
        <<"access-control-allow-headers"/utf8>>,
        <<"Content-Type, Authorization"/utf8>>
    ).

-file("src/api_server.gleam", 33).
-spec start(gleam@erlang@process:subject(types:engine_message()), integer()) -> {ok,
        nil} |
    {error, nil}.
start(Engine, Port) ->
    Context = {api_context, Engine},
    gleam_stdlib:println(
        <<"Starting Reddit Clone REST API on port "/utf8,
            (erlang:integer_to_binary(Port))/binary>>
    ),
    case begin
        _pipe = mist:new(fun(Req) -> handle_request(Req, Context) end),
        _pipe@1 = mist:port(_pipe, Port),
        mist:start(_pipe@1)
    end of
        {ok, _} -> nil;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"api_server"/utf8>>,
                        function => <<"start"/utf8>>,
                        line => 41,
                        value => _assert_fail,
                        start => 1196,
                        'end' => 1367,
                        pattern_start => 1207,
                        pattern_end => 1212})
    end,
    {ok, nil}.
