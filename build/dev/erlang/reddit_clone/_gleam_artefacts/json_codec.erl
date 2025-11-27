-module(json_codec).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/json_codec.gleam").
-export([decode_register_request/1, decode_login_request/1, decode_create_subreddit_request/1, decode_create_post_request/1, decode_create_comment_request/1, decode_vote_request/1, decode_send_message_request/1, decode_search_request/1, encode_success_response/1, encode_error_response/1, encode_user_response/2, encode_subreddit/1, encode_subreddits_response/1, encode_stats_response/1, encode_karma_response/1, encode_user_info/1, encode_user_search_response/1, encode_active_clients_response/2, encode_post/1, encode_posts_response/1, encode_comment/1, encode_comments_response/1, encode_post_with_comments/2, encode_message/1, encode_messages_response/1, encode_public_key_response/1, encode_post_with_verification/2, encode_post_with_comments_verified/3]).

-file("src/json_codec.gleam", 15).
-spec decode_register_request(binary()) -> {ok,
        {binary(), binary(), gleam@option:option(binary())}} |
    {error, binary()}.
decode_register_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"username"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Username) ->
                gleam@dynamic@decode:field(
                    <<"password"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Password) ->
                        gleam@dynamic@decode:optional_field(
                            <<"public_key"/utf8>>,
                            none,
                            begin
                                _pipe = {decoder,
                                    fun gleam@dynamic@decode:decode_string/1},
                                gleam@dynamic@decode:map(
                                    _pipe,
                                    fun(Field@0) -> {some, Field@0} end
                                )
                            end,
                            fun(Public_key) ->
                                gleam@dynamic@decode:success(
                                    {Username, Password, Public_key}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    _pipe@1 = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe@1,
        fun(_) -> <<"Invalid JSON for register request"/utf8>> end
    ).

-file("src/json_codec.gleam", 33).
-spec decode_login_request(binary()) -> {ok, {binary(), binary()}} |
    {error, binary()}.
decode_login_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"username"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Username) ->
                gleam@dynamic@decode:field(
                    <<"password"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Password) ->
                        gleam@dynamic@decode:success({Username, Password})
                    end
                )
            end
        )
    end,
    _pipe = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe,
        fun(_) -> <<"Invalid JSON for login request"/utf8>> end
    ).

-file("src/json_codec.gleam", 46).
-spec decode_create_subreddit_request(binary()) -> {ok, {binary(), binary()}} |
    {error, binary()}.
decode_create_subreddit_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"name"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Name) ->
                gleam@dynamic@decode:field(
                    <<"description"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Description) ->
                        gleam@dynamic@decode:success({Name, Description})
                    end
                )
            end
        )
    end,
    _pipe = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe,
        fun(_) -> <<"Invalid JSON for create subreddit request"/utf8>> end
    ).

-file("src/json_codec.gleam", 59).
-spec decode_create_post_request(binary()) -> {ok,
        {binary(), binary(), binary(), gleam@option:option(binary())}} |
    {error, binary()}.
decode_create_post_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"subreddit"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Subreddit) ->
                gleam@dynamic@decode:field(
                    <<"title"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Title) ->
                        gleam@dynamic@decode:field(
                            <<"content"/utf8>>,
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            fun(Content) ->
                                gleam@dynamic@decode:optional_field(
                                    <<"signature"/utf8>>,
                                    none,
                                    begin
                                        _pipe = {decoder,
                                            fun gleam@dynamic@decode:decode_string/1},
                                        gleam@dynamic@decode:map(
                                            _pipe,
                                            fun(Field@0) -> {some, Field@0} end
                                        )
                                    end,
                                    fun(Signature) ->
                                        gleam@dynamic@decode:success(
                                            {Subreddit,
                                                Title,
                                                Content,
                                                Signature}
                                        )
                                    end
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    _pipe@1 = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe@1,
        fun(_) -> <<"Invalid JSON for create post request"/utf8>> end
    ).

-file("src/json_codec.gleam", 78).
-spec decode_create_comment_request(binary()) -> {ok,
        {binary(), gleam@option:option(binary()), binary()}} |
    {error, binary()}.
decode_create_comment_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"post_id"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Post_id) ->
                gleam@dynamic@decode:optional_field(
                    <<"parent_id"/utf8>>,
                    none,
                    begin
                        _pipe = {decoder,
                            fun gleam@dynamic@decode:decode_string/1},
                        gleam@dynamic@decode:map(
                            _pipe,
                            fun(Field@0) -> {some, Field@0} end
                        )
                    end,
                    fun(Parent_id) ->
                        gleam@dynamic@decode:field(
                            <<"content"/utf8>>,
                            {decoder, fun gleam@dynamic@decode:decode_string/1},
                            fun(Content) ->
                                gleam@dynamic@decode:success(
                                    {Post_id, Parent_id, Content}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    _pipe@1 = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe@1,
        fun(_) -> <<"Invalid JSON for create comment request"/utf8>> end
    ).

-file("src/json_codec.gleam", 96).
-spec decode_vote_request(binary()) -> {ok, types:vote()} | {error, binary()}.
decode_vote_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"vote"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Vote_str) -> case Vote_str of
                    <<"up"/utf8>> ->
                        gleam@dynamic@decode:success(upvote);

                    <<"down"/utf8>> ->
                        gleam@dynamic@decode:success(downvote);

                    _ ->
                        gleam@dynamic@decode:success(upvote)
                end end
        )
    end,
    _pipe = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe,
        fun(_) -> <<"Invalid JSON for vote request"/utf8>> end
    ).

-file("src/json_codec.gleam", 110).
-spec decode_send_message_request(binary()) -> {ok,
        {binary(), binary(), gleam@option:option(binary())}} |
    {error, binary()}.
decode_send_message_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"recipient"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Recipient) ->
                gleam@dynamic@decode:field(
                    <<"content"/utf8>>,
                    {decoder, fun gleam@dynamic@decode:decode_string/1},
                    fun(Content) ->
                        gleam@dynamic@decode:optional_field(
                            <<"reply_to"/utf8>>,
                            none,
                            begin
                                _pipe = {decoder,
                                    fun gleam@dynamic@decode:decode_string/1},
                                gleam@dynamic@decode:map(
                                    _pipe,
                                    fun(Field@0) -> {some, Field@0} end
                                )
                            end,
                            fun(Reply_to) ->
                                gleam@dynamic@decode:success(
                                    {Recipient, Content, Reply_to}
                                )
                            end
                        )
                    end
                )
            end
        )
    end,
    _pipe@1 = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe@1,
        fun(_) -> <<"Invalid JSON for send message request"/utf8>> end
    ).

-file("src/json_codec.gleam", 128).
-spec decode_search_request(binary()) -> {ok, {binary(), integer()}} |
    {error, binary()}.
decode_search_request(Json_string) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"query"/utf8>>,
            {decoder, fun gleam@dynamic@decode:decode_string/1},
            fun(Query) ->
                gleam@dynamic@decode:optional_field(
                    <<"limit"/utf8>>,
                    25,
                    {decoder, fun gleam@dynamic@decode:decode_int/1},
                    fun(Limit) ->
                        gleam@dynamic@decode:success({Query, Limit})
                    end
                )
            end
        )
    end,
    _pipe = gleam@json:parse(Json_string, Decoder),
    gleam@result:map_error(
        _pipe,
        fun(_) -> <<"Invalid JSON for search request"/utf8>> end
    ).

-file("src/json_codec.gleam", 145).
-spec encode_success_response(binary()) -> binary().
encode_success_response(Message) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"message"/utf8>>, gleam@json:string(Message)}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 153).
-spec encode_error_response(binary()) -> binary().
encode_error_response(Message) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
            {<<"message"/utf8>>, gleam@json:string(Message)}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 161).
-spec encode_user_response(binary(), binary()) -> binary().
encode_user_response(User_id, Username) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"user_id"/utf8>>, gleam@json:string(User_id)},
                        {<<"username"/utf8>>, gleam@json:string(Username)}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 175).
-spec encode_subreddit(types:subreddit()) -> gleam@json:json().
encode_subreddit(Subreddit) ->
    gleam@json:object(
        [{<<"name"/utf8>>, gleam@json:string(erlang:element(2, Subreddit))},
            {<<"description"/utf8>>,
                gleam@json:string(erlang:element(3, Subreddit))},
            {<<"creator"/utf8>>,
                gleam@json:string(erlang:element(4, Subreddit))},
            {<<"member_count"/utf8>>,
                gleam@json:int(erlang:length(erlang:element(5, Subreddit)))},
            {<<"post_count"/utf8>>,
                gleam@json:int(erlang:element(6, Subreddit))},
            {<<"created_at"/utf8>>,
                gleam@json:int(erlang:element(7, Subreddit))}]
    ).

-file("src/json_codec.gleam", 186).
-spec encode_subreddits_response(list(types:subreddit())) -> binary().
encode_subreddits_response(Subreddits) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"subreddits"/utf8>>,
                            gleam@json:array(Subreddits, fun encode_subreddit/1)},
                        {<<"count"/utf8>>,
                            gleam@json:int(erlang:length(Subreddits))}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 341).
-spec encode_stats_response(types:engine_stats()) -> binary().
encode_stats_response(Stats) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"total_users"/utf8>>,
                            gleam@json:int(erlang:element(2, Stats))},
                        {<<"total_subreddits"/utf8>>,
                            gleam@json:int(erlang:element(3, Stats))},
                        {<<"total_posts"/utf8>>,
                            gleam@json:int(erlang:element(4, Stats))},
                        {<<"total_comments"/utf8>>,
                            gleam@json:int(erlang:element(5, Stats))},
                        {<<"total_messages"/utf8>>,
                            gleam@json:int(erlang:element(6, Stats))},
                        {<<"active_sessions"/utf8>>,
                            gleam@json:int(erlang:element(7, Stats))},
                        {<<"uptime_seconds"/utf8>>,
                            gleam@json:int(erlang:element(10, Stats))},
                        {<<"comments_with_replies"/utf8>>,
                            gleam@json:int(erlang:element(11, Stats))},
                        {<<"max_comment_depth"/utf8>>,
                            gleam@json:int(erlang:element(12, Stats))},
                        {<<"threaded_messages"/utf8>>,
                            gleam@json:int(erlang:element(13, Stats))},
                        {<<"repost_count"/utf8>>,
                            gleam@json:int(erlang:element(14, Stats))},
                        {<<"start_time"/utf8>>,
                            gleam@json:int(erlang:element(19, Stats))},
                        {<<"last_update_time"/utf8>>,
                            gleam@json:int(erlang:element(20, Stats))},
                        {<<"rates"/utf8>>,
                            gleam@json:object(
                                [{<<"posts_per_second"/utf8>>,
                                        gleam@json:float(
                                            erlang:element(16, Stats)
                                        )},
                                    {<<"comments_per_second"/utf8>>,
                                        gleam@json:float(
                                            erlang:element(17, Stats)
                                        )},
                                    {<<"votes_per_second"/utf8>>,
                                        gleam@json:float(
                                            erlang:element(18, Stats)
                                        )}]
                            )},
                        {<<"performance"/utf8>>,
                            gleam@json:object(
                                [{<<"total_posts_created"/utf8>>,
                                        gleam@json:int(
                                            erlang:element(
                                                2,
                                                erlang:element(9, Stats)
                                            )
                                        )},
                                    {<<"total_comments_created"/utf8>>,
                                        gleam@json:int(
                                            erlang:element(
                                                3,
                                                erlang:element(9, Stats)
                                            )
                                        )},
                                    {<<"total_votes_cast"/utf8>>,
                                        gleam@json:int(
                                            erlang:element(
                                                4,
                                                erlang:element(9, Stats)
                                            )
                                        )},
                                    {<<"messages_processed"/utf8>>,
                                        gleam@json:int(
                                            erlang:element(
                                                6,
                                                erlang:element(9, Stats)
                                            )
                                        )}]
                            )}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 395).
-spec encode_karma_response(integer()) -> binary().
encode_karma_response(Karma) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object([{<<"karma"/utf8>>, gleam@json:int(Karma)}])}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 414).
-spec encode_user_info(types:user_info()) -> gleam@json:json().
encode_user_info(User) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, User))},
            {<<"username"/utf8>>, gleam@json:string(erlang:element(3, User))},
            {<<"karma"/utf8>>, gleam@json:int(erlang:element(4, User))},
            {<<"joined_subreddits_count"/utf8>>,
                gleam@json:int(erlang:element(5, User))},
            {<<"created_at"/utf8>>, gleam@json:int(erlang:element(6, User))},
            {<<"has_public_key"/utf8>>,
                gleam@json:bool(erlang:element(7, User))}]
    ).

-file("src/json_codec.gleam", 425).
-spec encode_user_search_response(list(types:user_info())) -> binary().
encode_user_search_response(Users) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"users"/utf8>>,
                            gleam@json:array(Users, fun encode_user_info/1)},
                        {<<"count"/utf8>>, gleam@json:int(erlang:length(Users))}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 439).
-spec encode_active_clients_response(integer(), list(binary())) -> binary().
encode_active_clients_response(Count, Users) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"active_clients"/utf8>>, gleam@json:int(Count)},
                        {<<"users"/utf8>>,
                            gleam@json:array(Users, fun gleam@json:string/1)}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 457).
-spec encode_optional_string(gleam@option:option(binary())) -> gleam@json:json().
encode_optional_string(Opt) ->
    case Opt of
        {some, Value} ->
            gleam@json:string(Value);

        none ->
            gleam@json:null()
    end.

-file("src/json_codec.gleam", 200).
-spec encode_post(types:post()) -> gleam@json:json().
encode_post(Post) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Post))},
            {<<"subreddit"/utf8>>, gleam@json:string(erlang:element(3, Post))},
            {<<"author"/utf8>>, gleam@json:string(erlang:element(4, Post))},
            {<<"title"/utf8>>, gleam@json:string(erlang:element(5, Post))},
            {<<"content"/utf8>>, gleam@json:string(erlang:element(6, Post))},
            {<<"upvotes"/utf8>>, gleam@json:int(erlang:element(7, Post))},
            {<<"downvotes"/utf8>>, gleam@json:int(erlang:element(8, Post))},
            {<<"score"/utf8>>,
                gleam@json:int(
                    erlang:element(7, Post) - erlang:element(8, Post)
                )},
            {<<"created_at"/utf8>>, gleam@json:int(erlang:element(9, Post))},
            {<<"is_repost"/utf8>>, gleam@json:bool(erlang:element(10, Post))},
            {<<"signature"/utf8>>,
                encode_optional_string(erlang:element(12, Post))},
            {<<"signature_valid"/utf8>>, gleam@json:null()}]
    ).

-file("src/json_codec.gleam", 237).
-spec encode_posts_response(list(types:post())) -> binary().
encode_posts_response(Posts) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"posts"/utf8>>,
                            gleam@json:array(Posts, fun encode_post/1)},
                        {<<"count"/utf8>>, gleam@json:int(erlang:length(Posts))}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 251).
-spec encode_comment(types:comment()) -> gleam@json:json().
encode_comment(Comment) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Comment))},
            {<<"post_id"/utf8>>, gleam@json:string(erlang:element(3, Comment))},
            {<<"parent_id"/utf8>>,
                encode_optional_string(erlang:element(4, Comment))},
            {<<"author"/utf8>>, gleam@json:string(erlang:element(5, Comment))},
            {<<"content"/utf8>>, gleam@json:string(erlang:element(6, Comment))},
            {<<"upvotes"/utf8>>, gleam@json:int(erlang:element(7, Comment))},
            {<<"downvotes"/utf8>>, gleam@json:int(erlang:element(8, Comment))},
            {<<"score"/utf8>>,
                gleam@json:int(
                    erlang:element(7, Comment) - erlang:element(8, Comment)
                )},
            {<<"created_at"/utf8>>, gleam@json:int(erlang:element(9, Comment))},
            {<<"reply_count"/utf8>>,
                gleam@json:int(erlang:length(erlang:element(10, Comment)))}]
    ).

-file("src/json_codec.gleam", 266).
-spec encode_comments_response(list(types:comment())) -> binary().
encode_comments_response(Comments) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"comments"/utf8>>,
                            gleam@json:array(Comments, fun encode_comment/1)},
                        {<<"count"/utf8>>,
                            gleam@json:int(erlang:length(Comments))}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 280).
-spec encode_post_with_comments(types:post(), list(types:comment())) -> binary().
encode_post_with_comments(Post, Comments) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"post"/utf8>>, encode_post(Post)},
                        {<<"comments"/utf8>>,
                            gleam@json:array(Comments, fun encode_comment/1)}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 315).
-spec encode_message(types:direct_message()) -> gleam@json:json().
encode_message(Message) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Message))},
            {<<"sender"/utf8>>, gleam@json:string(erlang:element(3, Message))},
            {<<"recipient"/utf8>>,
                gleam@json:string(erlang:element(4, Message))},
            {<<"content"/utf8>>, gleam@json:string(erlang:element(5, Message))},
            {<<"reply_to"/utf8>>,
                encode_optional_string(erlang:element(6, Message))},
            {<<"created_at"/utf8>>, gleam@json:int(erlang:element(7, Message))},
            {<<"is_read"/utf8>>, gleam@json:bool(erlang:element(8, Message))}]
    ).

-file("src/json_codec.gleam", 327).
-spec encode_messages_response(list(types:direct_message())) -> binary().
encode_messages_response(Messages) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"messages"/utf8>>,
                            gleam@json:array(Messages, fun encode_message/1)},
                        {<<"count"/utf8>>,
                            gleam@json:int(erlang:length(Messages))}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 403).
-spec encode_public_key_response(gleam@option:option(binary())) -> binary().
encode_public_key_response(Public_key) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"public_key"/utf8>>, encode_optional_string(Public_key)}]
                )}]
    ),
    gleam@json:to_string(_pipe).

-file("src/json_codec.gleam", 464).
-spec encode_optional_bool(gleam@option:option(boolean())) -> gleam@json:json().
encode_optional_bool(Opt) ->
    case Opt of
        {some, Value} ->
            gleam@json:bool(Value);

        none ->
            gleam@json:null()
    end.

-file("src/json_codec.gleam", 217).
-spec encode_post_with_verification(
    types:post(),
    gleam@option:option(boolean())
) -> gleam@json:json().
encode_post_with_verification(Post, Signature_valid) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:string(erlang:element(2, Post))},
            {<<"subreddit"/utf8>>, gleam@json:string(erlang:element(3, Post))},
            {<<"author"/utf8>>, gleam@json:string(erlang:element(4, Post))},
            {<<"title"/utf8>>, gleam@json:string(erlang:element(5, Post))},
            {<<"content"/utf8>>, gleam@json:string(erlang:element(6, Post))},
            {<<"upvotes"/utf8>>, gleam@json:int(erlang:element(7, Post))},
            {<<"downvotes"/utf8>>, gleam@json:int(erlang:element(8, Post))},
            {<<"score"/utf8>>,
                gleam@json:int(
                    erlang:element(7, Post) - erlang:element(8, Post)
                )},
            {<<"created_at"/utf8>>, gleam@json:int(erlang:element(9, Post))},
            {<<"is_repost"/utf8>>, gleam@json:bool(erlang:element(10, Post))},
            {<<"signature"/utf8>>,
                encode_optional_string(erlang:element(12, Post))},
            {<<"signature_valid"/utf8>>, encode_optional_bool(Signature_valid)}]
    ).

-file("src/json_codec.gleam", 297).
-spec encode_post_with_comments_verified(
    types:post(),
    list(types:comment()),
    gleam@option:option(boolean())
) -> binary().
encode_post_with_comments_verified(Post, Comments, Signature_valid) ->
    _pipe = gleam@json:object(
        [{<<"status"/utf8>>, gleam@json:string(<<"success"/utf8>>)},
            {<<"data"/utf8>>,
                gleam@json:object(
                    [{<<"post"/utf8>>,
                            encode_post_with_verification(Post, Signature_valid)},
                        {<<"comments"/utf8>>,
                            gleam@json:array(Comments, fun encode_comment/1)}]
                )}]
    ),
    gleam@json:to_string(_pipe).
