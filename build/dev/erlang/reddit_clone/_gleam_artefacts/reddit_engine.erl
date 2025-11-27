-module(reddit_engine).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/reddit_engine.gleam").
-export([start/0]).

-file("src/reddit_engine.gleam", 337).
-spec calculate_comment_depth(
    gleam@dict:dict(binary(), types:comment()),
    binary(),
    integer()
) -> integer().
calculate_comment_depth(Comments, Comment_id, Current_depth) ->
    case gleam_stdlib:map_get(Comments, Comment_id) of
        {ok, Comment} ->
            case erlang:element(10, Comment) of
                [] ->
                    Current_depth;

                Replies ->
                    Depths = gleam@list:map(
                        Replies,
                        fun(Reply_id) ->
                            calculate_comment_depth(
                                Comments,
                                Reply_id,
                                Current_depth + 1
                            )
                        end
                    ),
                    case gleam@list:reduce(Depths, fun gleam@int:max/2) of
                        {ok, Max} ->
                            Max;

                        {error, _} ->
                            Current_depth
                    end
            end;

        {error, _} ->
            Current_depth
    end.

-file("src/reddit_engine.gleam", 314).
-spec get_max_comment_depth(gleam@dict:dict(binary(), types:comment())) -> integer().
get_max_comment_depth(Comments) ->
    Top_level_comments = begin
        _pipe = Comments,
        _pipe@1 = maps:values(_pipe),
        gleam@list:filter(
            _pipe@1,
            fun(C) -> gleam@option:is_none(erlang:element(4, C)) end
        )
    end,
    case Top_level_comments of
        [] ->
            0;

        _ ->
            Depths = gleam@list:map(
                Top_level_comments,
                fun(C@1) ->
                    calculate_comment_depth(Comments, erlang:element(2, C@1), 1)
                end
            ),
            case gleam@list:reduce(Depths, fun gleam@int:max/2) of
                {ok, Max} ->
                    Max;

                {error, _} ->
                    0
            end
    end.

-file("src/reddit_engine.gleam", 248).
-spec calculate_stats(types:engine_state()) -> types:engine_stats().
calculate_stats(State) ->
    Popular_subreddits = begin
        _pipe = erlang:element(4, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:map(
            _pipe@1,
            fun(S) ->
                {erlang:element(2, S), erlang:length(erlang:element(5, S))}
            end
        ),
        _pipe@3 = gleam@list:sort(
            _pipe@2,
            fun(A, B) ->
                gleam@int:compare(erlang:element(2, B), erlang:element(2, A))
            end
        ),
        gleam@list:take(_pipe@3, 10)
    end,
    Current_time = utils:current_timestamp(),
    Uptime = Current_time - erlang:element(14, State),
    Uptime_seconds = Uptime div 1000,
    Comments_with_replies = begin
        _pipe@4 = erlang:element(6, State),
        _pipe@5 = maps:values(_pipe@4),
        _pipe@6 = gleam@list:filter(
            _pipe@5,
            fun(C) -> erlang:element(10, C) /= [] end
        ),
        erlang:length(_pipe@6)
    end,
    Max_depth = get_max_comment_depth(erlang:element(6, State)),
    Threaded_messages = begin
        _pipe@7 = erlang:element(7, State),
        _pipe@8 = maps:values(_pipe@7),
        _pipe@9 = gleam@list:filter(
            _pipe@8,
            fun(M) -> gleam@option:is_some(erlang:element(6, M)) end
        ),
        erlang:length(_pipe@9)
    end,
    Repost_count = begin
        _pipe@10 = erlang:element(5, State),
        _pipe@11 = maps:values(_pipe@10),
        _pipe@12 = gleam@list:filter(
            _pipe@11,
            fun(P) -> erlang:element(10, P) end
        ),
        erlang:length(_pipe@12)
    end,
    Users_list = maps:values(erlang:element(3, State)),
    Low_karma = begin
        _pipe@13 = gleam@list:filter(
            Users_list,
            fun(U) -> erlang:element(5, U) < 10 end
        ),
        erlang:length(_pipe@13)
    end,
    Medium_karma = begin
        _pipe@14 = gleam@list:filter(
            Users_list,
            fun(U@1) ->
                (erlang:element(5, U@1) >= 10) andalso (erlang:element(5, U@1) < 50)
            end
        ),
        erlang:length(_pipe@14)
    end,
    High_karma = begin
        _pipe@15 = gleam@list:filter(
            Users_list,
            fun(U@2) -> erlang:element(5, U@2) >= 50 end
        ),
        erlang:length(_pipe@15)
    end,
    {engine_stats,
        maps:size(erlang:element(3, State)),
        maps:size(erlang:element(4, State)),
        maps:size(erlang:element(5, State)),
        maps:size(erlang:element(6, State)),
        maps:size(erlang:element(7, State)),
        maps:size(erlang:element(9, State)),
        Popular_subreddits,
        erlang:element(13, State),
        Uptime_seconds,
        Comments_with_replies,
        Max_depth,
        Threaded_messages,
        Repost_count,
        {Low_karma, Medium_karma, High_karma},
        erlang:element(11, erlang:element(13, State)),
        erlang:element(12, erlang:element(13, State)),
        erlang:element(13, erlang:element(13, State)),
        erlang:element(14, State),
        erlang:element(16, State)}.

-file("src/reddit_engine.gleam", 187).
-spec update_stats_cache(types:engine_state()) -> types:engine_state().
update_stats_cache(State) ->
    Current_time = utils:current_timestamp(),
    Time_elapsed_ms = Current_time - erlang:element(
        7,
        erlang:element(13, State)
    ),
    Time_elapsed_sec = erlang:float(Time_elapsed_ms) / 1000.0,
    Posts_per_sec = case Time_elapsed_sec > +0.0 of
        true ->
            case Time_elapsed_sec of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> erlang:float(
                    erlang:element(8, erlang:element(13, State))
                )
                / Gleam@denominator
            end;

        false ->
            +0.0
    end,
    Comments_per_sec = case Time_elapsed_sec > +0.0 of
        true ->
            case Time_elapsed_sec of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@1 -> erlang:float(
                    erlang:element(9, erlang:element(13, State))
                )
                / Gleam@denominator@1
            end;

        false ->
            +0.0
    end,
    Votes_per_sec = case Time_elapsed_sec > +0.0 of
        true ->
            case Time_elapsed_sec of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator@2 -> erlang:float(
                    erlang:element(10, erlang:element(13, State))
                )
                / Gleam@denominator@2
            end;

        false ->
            +0.0
    end,
    New_metrics = begin
        _record = erlang:element(13, State),
        {performance_metrics,
            erlang:element(2, _record),
            erlang:element(3, _record),
            erlang:element(4, _record),
            erlang:element(5, _record),
            erlang:element(6, _record),
            Current_time,
            0,
            0,
            0,
            Posts_per_sec,
            Comments_per_sec,
            Votes_per_sec}
    end,
    State_with_metrics = {engine_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        erlang:element(6, State),
        erlang:element(7, State),
        erlang:element(8, State),
        erlang:element(9, State),
        erlang:element(10, State),
        erlang:element(11, State),
        erlang:element(12, State),
        New_metrics,
        erlang:element(14, State),
        erlang:element(15, State),
        erlang:element(16, State)},
    Stats = calculate_stats(State_with_metrics),
    gleam@erlang@process:send_after(
        erlang:element(2, State),
        3000,
        update_stats_cache
    ),
    {engine_state,
        erlang:element(2, State_with_metrics),
        erlang:element(3, State_with_metrics),
        erlang:element(4, State_with_metrics),
        erlang:element(5, State_with_metrics),
        erlang:element(6, State_with_metrics),
        erlang:element(7, State_with_metrics),
        erlang:element(8, State_with_metrics),
        erlang:element(9, State_with_metrics),
        erlang:element(10, State_with_metrics),
        erlang:element(11, State_with_metrics),
        erlang:element(12, State_with_metrics),
        erlang:element(13, State_with_metrics),
        erlang:element(14, State_with_metrics),
        {some, Stats},
        Current_time}.

-file("src/reddit_engine.gleam", 236).
-spec handle_get_stats_cached(
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_stats_cached(Reply_to, State) ->
    Stats = case erlang:element(15, State) of
        {some, Cached} ->
            Cached;

        none ->
            calculate_stats(State)
    end,
    gleam@erlang@process:send(Reply_to, {success, {stats_data, Stats}}).

-file("src/reddit_engine.gleam", 366).
-spec handle_register_user(
    binary(),
    binary(),
    gleam@option:option(binary()),
    types:engine_state()
) -> types:engine_state().
handle_register_user(Username, Password, Public_key, State) ->
    User_id = Username,
    Timestamp = utils:current_timestamp(),
    case gleam@dict:has_key(erlang:element(3, State), User_id) of
        true ->
            State;

        false ->
            Password_hash = utils:hash_password(Password),
            User = {user,
                User_id,
                Username,
                Password_hash,
                0,
                [],
                Timestamp,
                Public_key},
            New_users = gleam@dict:insert(
                erlang:element(3, State),
                User_id,
                User
            ),
            {engine_state,
                erlang:element(2, State),
                New_users,
                erlang:element(4, State),
                erlang:element(5, State),
                erlang:element(6, State),
                erlang:element(7, State),
                erlang:element(8, State),
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State),
                erlang:element(12, State),
                erlang:element(13, State),
                erlang:element(14, State),
                erlang:element(15, State),
                erlang:element(16, State)}
    end.

-file("src/reddit_engine.gleam", 396).
-spec handle_authenticate_user(
    binary(),
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_authenticate_user(Username, Password, Reply_to, State) ->
    case gleam_stdlib:map_get(erlang:element(3, State), Username) of
        {ok, User} ->
            case utils:verify_password(Password, erlang:element(4, User)) of
                true ->
                    gleam@erlang@process:send(
                        Reply_to,
                        {success,
                            {user_authenticated,
                                erlang:element(2, User),
                                erlang:element(3, User)}}
                    );

                false ->
                    gleam@erlang@process:send(
                        Reply_to,
                        {engine_error, <<"Invalid credentials"/utf8>>}
                    )
            end;

        {error, _} ->
            gleam@erlang@process:send(
                Reply_to,
                {engine_error, <<"User not found"/utf8>>}
            )
    end.

-file("src/reddit_engine.gleam", 418).
-spec handle_login_user(
    binary(),
    gleam@erlang@process:subject(types:client_message()),
    types:engine_state()
) -> types:engine_state().
handle_login_user(User_id, Session, State) ->
    New_sessions = gleam@dict:insert(erlang:element(9, State), User_id, Session),
    {engine_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        erlang:element(6, State),
        erlang:element(7, State),
        erlang:element(8, State),
        New_sessions,
        erlang:element(10, State),
        erlang:element(11, State),
        erlang:element(12, State),
        erlang:element(13, State),
        erlang:element(14, State),
        erlang:element(15, State),
        erlang:element(16, State)}.

-file("src/reddit_engine.gleam", 427).
-spec handle_logout_user(binary(), types:engine_state()) -> types:engine_state().
handle_logout_user(User_id, State) ->
    New_sessions = gleam@dict:delete(erlang:element(9, State), User_id),
    {engine_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        erlang:element(6, State),
        erlang:element(7, State),
        erlang:element(8, State),
        New_sessions,
        erlang:element(10, State),
        erlang:element(11, State),
        erlang:element(12, State),
        erlang:element(13, State),
        erlang:element(14, State),
        erlang:element(15, State),
        erlang:element(16, State)}.

-file("src/reddit_engine.gleam", 432).
-spec handle_get_user_karma(
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_user_karma(User_id, Reply_to, State) ->
    case gleam_stdlib:map_get(erlang:element(3, State), User_id) of
        {ok, User} ->
            gleam@erlang@process:send(
                Reply_to,
                {success, {karma_data, erlang:element(5, User)}}
            );

        {error, _} ->
            gleam@erlang@process:send(
                Reply_to,
                {engine_error, <<"User not found"/utf8>>}
            )
    end.

-file("src/reddit_engine.gleam", 445).
-spec handle_get_user_public_key(
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_user_public_key(User_id, Reply_to, State) ->
    case gleam_stdlib:map_get(erlang:element(3, State), User_id) of
        {ok, User} ->
            gleam@erlang@process:send(
                Reply_to,
                {success, {public_key_data, erlang:element(8, User)}}
            );

        {error, _} ->
            gleam@erlang@process:send(
                Reply_to,
                {engine_error, <<"User not found"/utf8>>}
            )
    end.

-file("src/reddit_engine.gleam", 462).
-spec handle_create_subreddit(
    binary(),
    binary(),
    binary(),
    types:engine_state()
) -> types:engine_state().
handle_create_subreddit(User_id, Name, Description, State) ->
    case gleam_stdlib:map_get(erlang:element(3, State), User_id) of
        {ok, _} ->
            case gleam_stdlib:map_get(erlang:element(4, State), Name) of
                {ok, _} ->
                    State;

                {error, _} ->
                    Timestamp = utils:current_timestamp(),
                    Subreddit = {subreddit,
                        Name,
                        Description,
                        User_id,
                        [User_id],
                        0,
                        Timestamp},
                    New_subreddits = gleam@dict:insert(
                        erlang:element(4, State),
                        Name,
                        Subreddit
                    ),
                    New_users = case gleam_stdlib:map_get(
                        erlang:element(3, State),
                        User_id
                    ) of
                        {ok, User} ->
                            Updated_user = {user,
                                erlang:element(2, User),
                                erlang:element(3, User),
                                erlang:element(4, User),
                                erlang:element(5, User),
                                [Name | erlang:element(6, User)],
                                erlang:element(7, User),
                                erlang:element(8, User)},
                            gleam@dict:insert(
                                erlang:element(3, State),
                                User_id,
                                Updated_user
                            );

                        {error, _} ->
                            erlang:element(3, State)
                    end,
                    {engine_state,
                        erlang:element(2, State),
                        New_users,
                        New_subreddits,
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        erlang:element(9, State),
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State),
                        erlang:element(14, State),
                        erlang:element(15, State),
                        erlang:element(16, State)}
            end;

        {error, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 503).
-spec handle_join_subreddit(binary(), binary(), types:engine_state()) -> types:engine_state().
handle_join_subreddit(User_id, Subreddit_name, State) ->
    case {gleam_stdlib:map_get(erlang:element(3, State), User_id),
        gleam_stdlib:map_get(erlang:element(4, State), Subreddit_name)} of
        {{ok, User}, {ok, Subreddit}} ->
            case gleam@list:contains(erlang:element(5, Subreddit), User_id) of
                true ->
                    State;

                false ->
                    Updated_subreddit = {subreddit,
                        erlang:element(2, Subreddit),
                        erlang:element(3, Subreddit),
                        erlang:element(4, Subreddit),
                        [User_id | erlang:element(5, Subreddit)],
                        erlang:element(6, Subreddit),
                        erlang:element(7, Subreddit)},
                    Updated_user = {user,
                        erlang:element(2, User),
                        erlang:element(3, User),
                        erlang:element(4, User),
                        erlang:element(5, User),
                        [Subreddit_name | erlang:element(6, User)],
                        erlang:element(7, User),
                        erlang:element(8, User)},
                    {engine_state,
                        erlang:element(2, State),
                        gleam@dict:insert(
                            erlang:element(3, State),
                            User_id,
                            Updated_user
                        ),
                        gleam@dict:insert(
                            erlang:element(4, State),
                            Subreddit_name,
                            Updated_subreddit
                        ),
                        erlang:element(5, State),
                        erlang:element(6, State),
                        erlang:element(7, State),
                        erlang:element(8, State),
                        erlang:element(9, State),
                        erlang:element(10, State),
                        erlang:element(11, State),
                        erlang:element(12, State),
                        erlang:element(13, State),
                        erlang:element(14, State),
                        erlang:element(15, State),
                        erlang:element(16, State)}
            end;

        {_, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 540).
-spec handle_leave_subreddit(binary(), binary(), types:engine_state()) -> types:engine_state().
handle_leave_subreddit(User_id, Subreddit_name, State) ->
    case {gleam_stdlib:map_get(erlang:element(3, State), User_id),
        gleam_stdlib:map_get(erlang:element(4, State), Subreddit_name)} of
        {{ok, User}, {ok, Subreddit}} ->
            Updated_subreddit = {subreddit,
                erlang:element(2, Subreddit),
                erlang:element(3, Subreddit),
                erlang:element(4, Subreddit),
                gleam@list:filter(
                    erlang:element(5, Subreddit),
                    fun(Id) -> Id /= User_id end
                ),
                erlang:element(6, Subreddit),
                erlang:element(7, Subreddit)},
            Updated_user = {user,
                erlang:element(2, User),
                erlang:element(3, User),
                erlang:element(4, User),
                erlang:element(5, User),
                gleam@list:filter(
                    erlang:element(6, User),
                    fun(S) -> S /= Subreddit_name end
                ),
                erlang:element(7, User),
                erlang:element(8, User)},
            {engine_state,
                erlang:element(2, State),
                gleam@dict:insert(
                    erlang:element(3, State),
                    User_id,
                    Updated_user
                ),
                gleam@dict:insert(
                    erlang:element(4, State),
                    Subreddit_name,
                    Updated_subreddit
                ),
                erlang:element(5, State),
                erlang:element(6, State),
                erlang:element(7, State),
                erlang:element(8, State),
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State),
                erlang:element(12, State),
                erlang:element(13, State),
                erlang:element(14, State),
                erlang:element(15, State),
                erlang:element(16, State)};

        {_, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 577).
-spec handle_get_all_subreddits(
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_all_subreddits(Reply_to, State) ->
    Subreddits = maps:values(erlang:element(4, State)),
    gleam@erlang@process:send(
        Reply_to,
        {success, {subreddits_data, Subreddits}}
    ).

-file("src/reddit_engine.gleam", 785).
-spec handle_get_feed(
    binary(),
    integer(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_feed(User_id, Limit, Reply_to, State) ->
    case gleam_stdlib:map_get(erlang:element(3, State), User_id) of
        {ok, User} ->
            Posts = begin
                _pipe = erlang:element(5, State),
                _pipe@1 = maps:values(_pipe),
                _pipe@2 = gleam@list:filter(
                    _pipe@1,
                    fun(P) ->
                        gleam@list:contains(
                            erlang:element(6, User),
                            erlang:element(3, P)
                        )
                    end
                ),
                _pipe@3 = gleam@list:sort(
                    _pipe@2,
                    fun(A, B) ->
                        gleam@int:compare(
                            erlang:element(9, B),
                            erlang:element(9, A)
                        )
                    end
                ),
                gleam@list:take(_pipe@3, Limit)
            end,
            gleam@erlang@process:send(Reply_to, {success, {feed_data, Posts}});

        {error, _} ->
            gleam@erlang@process:send(
                Reply_to,
                {engine_error, <<"User not found"/utf8>>}
            )
    end.

-file("src/reddit_engine.gleam", 808).
-spec handle_get_subreddit_feed(
    binary(),
    integer(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_subreddit_feed(Subreddit, Limit, Reply_to, State) ->
    Posts = begin
        _pipe = erlang:element(5, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(P) -> erlang:element(3, P) =:= Subreddit end
        ),
        _pipe@3 = gleam@list:sort(
            _pipe@2,
            fun(A, B) ->
                gleam@int:compare(erlang:element(9, B), erlang:element(9, A))
            end
        ),
        gleam@list:take(_pipe@3, Limit)
    end,
    gleam@erlang@process:send(Reply_to, {success, {feed_data, Posts}}).

-file("src/reddit_engine.gleam", 824).
-spec handle_get_post(
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_post(Post_id, Reply_to, State) ->
    case gleam_stdlib:map_get(erlang:element(5, State), Post_id) of
        {ok, Post} ->
            Comments = begin
                _pipe = erlang:element(6, State),
                _pipe@1 = maps:values(_pipe),
                gleam@list:filter(
                    _pipe@1,
                    fun(C) ->
                        (erlang:element(3, C) =:= Post_id) andalso (erlang:element(
                            4,
                            C
                        )
                        =:= none)
                    end
                )
            end,
            gleam@erlang@process:send(
                Reply_to,
                {success, {post_data, Post, Comments}}
            );

        {error, _} ->
            gleam@erlang@process:send(
                Reply_to,
                {engine_error, <<"Post not found"/utf8>>}
            )
    end.

-file("src/reddit_engine.gleam", 975).
-spec handle_get_comments(
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_comments(Post_id, Reply_to, State) ->
    Comments = begin
        _pipe = erlang:element(6, State),
        _pipe@1 = maps:values(_pipe),
        gleam@list:filter(
            _pipe@1,
            fun(C) -> erlang:element(3, C) =:= Post_id end
        )
    end,
    gleam@erlang@process:send(Reply_to, {success, {comments_data, Comments}}).

-file("src/reddit_engine.gleam", 992).
-spec handle_send_dm(
    binary(),
    binary(),
    binary(),
    gleam@option:option(binary()),
    types:engine_state()
) -> types:engine_state().
handle_send_dm(Sender, Recipient, Content, Reply_msg_id, State) ->
    Message_id = <<"msg_"/utf8,
        (erlang:integer_to_binary(erlang:element(12, State)))/binary>>,
    Timestamp = utils:current_timestamp(),
    Message = {direct_message,
        Message_id,
        Sender,
        Recipient,
        Content,
        Reply_msg_id,
        Timestamp,
        false},
    New_messages = gleam@dict:insert(
        erlang:element(7, State),
        Message_id,
        Message
    ),
    case gleam_stdlib:map_get(erlang:element(9, State), Recipient) of
        {ok, Session} ->
            gleam@erlang@process:send(Session, {new_direct_message, Message});

        {error, _} ->
            nil
    end,
    {engine_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        erlang:element(6, State),
        New_messages,
        erlang:element(8, State),
        erlang:element(9, State),
        erlang:element(10, State),
        erlang:element(11, State),
        erlang:element(12, State) + 1,
        begin
            _record = erlang:element(13, State),
            {performance_metrics,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, erlang:element(13, State)) + 1,
                erlang:element(6, _record),
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record),
                erlang:element(10, _record),
                erlang:element(11, _record),
                erlang:element(12, _record),
                erlang:element(13, _record)}
        end,
        erlang:element(14, State),
        erlang:element(15, State),
        erlang:element(16, State)}.

-file("src/reddit_engine.gleam", 1031).
-spec handle_get_messages(
    binary(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_messages(User_id, Reply_to, State) ->
    Messages = begin
        _pipe = erlang:element(7, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(M) ->
                (erlang:element(4, M) =:= User_id) orelse (erlang:element(3, M)
                =:= User_id)
            end
        ),
        gleam@list:sort(
            _pipe@2,
            fun(A, B) ->
                gleam@int:compare(erlang:element(7, B), erlang:element(7, A))
            end
        )
    end,
    gleam@erlang@process:send(Reply_to, {success, {messages_data, Messages}}).

-file("src/reddit_engine.gleam", 1049).
-spec handle_search_posts(
    binary(),
    integer(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_search_posts(Query, Limit, Reply_to, State) ->
    Matching_posts = begin
        _pipe = erlang:element(5, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(P) ->
                utils:contains_lowercase(erlang:element(5, P), Query) orelse utils:contains_lowercase(
                    erlang:element(6, P),
                    Query
                )
            end
        ),
        _pipe@3 = gleam@list:sort(
            _pipe@2,
            fun(A, B) ->
                gleam@int:compare(erlang:element(9, B), erlang:element(9, A))
            end
        ),
        gleam@list:take(_pipe@3, Limit)
    end,
    gleam@erlang@process:send(
        Reply_to,
        {success, {post_search_results, Matching_posts}}
    ).

-file("src/reddit_engine.gleam", 1068).
-spec handle_search_subreddits(
    binary(),
    integer(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_search_subreddits(Query, Limit, Reply_to, State) ->
    Matching_subreddits = begin
        _pipe = erlang:element(4, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(S) ->
                utils:contains_lowercase(erlang:element(2, S), Query) orelse utils:contains_lowercase(
                    erlang:element(3, S),
                    Query
                )
            end
        ),
        _pipe@3 = gleam@list:sort(
            _pipe@2,
            fun(A, B) ->
                gleam@int:compare(
                    erlang:length(erlang:element(5, B)),
                    erlang:length(erlang:element(5, A))
                )
            end
        ),
        gleam@list:take(_pipe@3, Limit)
    end,
    gleam@erlang@process:send(
        Reply_to,
        {success, {subreddit_search_results, Matching_subreddits}}
    ).

-file("src/reddit_engine.gleam", 1092).
-spec handle_search_users(
    binary(),
    integer(),
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_search_users(Query, Limit, Reply_to, State) ->
    Matching_users = begin
        _pipe = erlang:element(3, State),
        _pipe@1 = maps:values(_pipe),
        _pipe@2 = gleam@list:filter(
            _pipe@1,
            fun(U) -> utils:contains_lowercase(erlang:element(3, U), Query) end
        ),
        _pipe@3 = gleam@list:map(
            _pipe@2,
            fun(U@1) ->
                {user_info,
                    erlang:element(2, U@1),
                    erlang:element(3, U@1),
                    erlang:element(5, U@1),
                    erlang:length(erlang:element(6, U@1)),
                    erlang:element(7, U@1),
                    gleam@option:is_some(erlang:element(8, U@1))}
            end
        ),
        _pipe@4 = gleam@list:sort(
            _pipe@3,
            fun(A, B) ->
                gleam@int:compare(erlang:element(4, B), erlang:element(4, A))
            end
        ),
        gleam@list:take(_pipe@4, Limit)
    end,
    gleam@erlang@process:send(
        Reply_to,
        {success, {user_search_results, Matching_users}}
    ).

-file("src/reddit_engine.gleam", 1122).
-spec handle_get_active_clients(
    gleam@erlang@process:subject(types:engine_response()),
    types:engine_state()
) -> nil.
handle_get_active_clients(Reply_to, State) ->
    Count = maps:size(erlang:element(9, State)),
    Users = maps:keys(erlang:element(9, State)),
    gleam@erlang@process:send(
        Reply_to,
        {success, {active_clients_data, Count, Users}}
    ).

-file("src/reddit_engine.gleam", 1135).
-spec update_vote_counts(
    integer(),
    integer(),
    {ok, types:vote()} | {error, nil},
    gleam@option:option(types:vote())
) -> {integer(), integer()}.
update_vote_counts(Upvotes, Downvotes, Previous, New) ->
    case {Previous, New} of
        {{ok, upvote}, {some, downvote}} ->
            {Upvotes - 1, Downvotes + 1};

        {{ok, downvote}, {some, upvote}} ->
            {Upvotes + 1, Downvotes - 1};

        {{error, _}, {some, upvote}} ->
            {Upvotes + 1, Downvotes};

        {{error, _}, {some, downvote}} ->
            {Upvotes, Downvotes + 1};

        {_, _} ->
            {Upvotes, Downvotes}
    end.

-file("src/reddit_engine.gleam", 1150).
-spec update_user_karma(
    gleam@dict:dict(binary(), types:user()),
    binary(),
    integer()
) -> gleam@dict:dict(binary(), types:user()).
update_user_karma(Users, User_id, Karma_change) ->
    case gleam_stdlib:map_get(Users, User_id) of
        {ok, User} ->
            Updated_user = {user,
                erlang:element(2, User),
                erlang:element(3, User),
                erlang:element(4, User),
                erlang:element(5, User) + Karma_change,
                erlang:element(6, User),
                erlang:element(7, User),
                erlang:element(8, User)},
            gleam@dict:insert(Users, User_id, Updated_user);

        {error, _} ->
            Users
    end.

-file("src/reddit_engine.gleam", 924).
-spec handle_vote_comment(
    binary(),
    binary(),
    types:vote(),
    types:engine_state()
) -> types:engine_state().
handle_vote_comment(User_id, Comment_id, Vote, State) ->
    case gleam_stdlib:map_get(erlang:element(6, State), Comment_id) of
        {ok, Comment} ->
            Vote_key = <<<<User_id/binary, "_"/utf8>>/binary,
                Comment_id/binary>>,
            Previous_vote = gleam_stdlib:map_get(
                erlang:element(8, State),
                Vote_key
            ),
            {New_upvotes, New_downvotes} = update_vote_counts(
                erlang:element(7, Comment),
                erlang:element(8, Comment),
                Previous_vote,
                {some, Vote}
            ),
            Updated_comment = {comment,
                erlang:element(2, Comment),
                erlang:element(3, Comment),
                erlang:element(4, Comment),
                erlang:element(5, Comment),
                erlang:element(6, Comment),
                New_upvotes,
                New_downvotes,
                erlang:element(9, Comment),
                erlang:element(10, Comment)},
            New_comments = gleam@dict:insert(
                erlang:element(6, State),
                Comment_id,
                Updated_comment
            ),
            New_votes = gleam@dict:insert(
                erlang:element(8, State),
                Vote_key,
                Vote
            ),
            New_users = update_user_karma(
                erlang:element(3, State),
                erlang:element(5, Comment),
                ((New_upvotes - New_downvotes) - erlang:element(7, Comment)) + erlang:element(
                    8,
                    Comment
                )
            ),
            New_metrics = begin
                _record = erlang:element(13, State),
                {performance_metrics,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, erlang:element(13, State)) + 1,
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record),
                    erlang:element(9, _record),
                    erlang:element(10, erlang:element(13, State)) + 1,
                    erlang:element(11, _record),
                    erlang:element(12, _record),
                    erlang:element(13, _record)}
            end,
            {engine_state,
                erlang:element(2, State),
                New_users,
                erlang:element(4, State),
                erlang:element(5, State),
                New_comments,
                erlang:element(7, State),
                New_votes,
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State),
                erlang:element(12, State),
                New_metrics,
                erlang:element(14, State),
                erlang:element(15, State),
                erlang:element(16, State)};

        {error, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 1164).
-spec broadcast_to_subreddit_members(
    list(binary()),
    types:client_message(),
    types:engine_state()
) -> nil.
broadcast_to_subreddit_members(Members, Message, State) ->
    gleam@list:each(
        Members,
        fun(Member_id) ->
            case gleam_stdlib:map_get(erlang:element(9, State), Member_id) of
                {ok, Session} ->
                    gleam@erlang@process:send(Session, Message);

                {error, _} ->
                    nil
            end
        end
    ).

-file("src/reddit_engine.gleam", 589).
-spec handle_create_post(
    binary(),
    binary(),
    binary(),
    binary(),
    gleam@option:option(binary()),
    types:engine_state()
) -> types:engine_state().
handle_create_post(User_id, Subreddit, Title, Content, Signature, State) ->
    case gleam_stdlib:map_get(erlang:element(4, State), Subreddit) of
        {ok, Sub} ->
            case gleam@list:contains(erlang:element(5, Sub), User_id) of
                true ->
                    Post_id = <<"post_"/utf8,
                        (erlang:integer_to_binary(erlang:element(10, State)))/binary>>,
                    Timestamp = utils:current_timestamp(),
                    Post = {post,
                        Post_id,
                        Subreddit,
                        User_id,
                        Title,
                        Content,
                        1,
                        0,
                        Timestamp,
                        false,
                        none,
                        Signature},
                    New_posts = gleam@dict:insert(
                        erlang:element(5, State),
                        Post_id,
                        Post
                    ),
                    Vote_key = <<<<User_id/binary, "_"/utf8>>/binary,
                        Post_id/binary>>,
                    New_votes = gleam@dict:insert(
                        erlang:element(8, State),
                        Vote_key,
                        upvote
                    ),
                    Updated_sub = {subreddit,
                        erlang:element(2, Sub),
                        erlang:element(3, Sub),
                        erlang:element(4, Sub),
                        erlang:element(5, Sub),
                        erlang:element(6, Sub) + 1,
                        erlang:element(7, Sub)},
                    New_subreddits = gleam@dict:insert(
                        erlang:element(4, State),
                        Subreddit,
                        Updated_sub
                    ),
                    broadcast_to_subreddit_members(
                        erlang:element(5, Sub),
                        {new_post, Post},
                        State
                    ),
                    New_metrics = begin
                        _record = erlang:element(13, State),
                        {performance_metrics,
                            erlang:element(2, erlang:element(13, State)) + 1,
                            erlang:element(3, _record),
                            erlang:element(4, _record),
                            erlang:element(5, _record),
                            erlang:element(6, _record),
                            erlang:element(7, _record),
                            erlang:element(8, erlang:element(13, State)) + 1,
                            erlang:element(9, _record),
                            erlang:element(10, _record),
                            erlang:element(11, _record),
                            erlang:element(12, _record),
                            erlang:element(13, _record)}
                    end,
                    {engine_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        New_subreddits,
                        New_posts,
                        erlang:element(6, State),
                        erlang:element(7, State),
                        New_votes,
                        erlang:element(9, State),
                        erlang:element(10, State) + 1,
                        erlang:element(11, State),
                        erlang:element(12, State),
                        New_metrics,
                        erlang:element(14, State),
                        erlang:element(15, State),
                        erlang:element(16, State)};

                false ->
                    State
            end;

        {error, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 657).
-spec handle_create_repost(binary(), binary(), binary(), types:engine_state()) -> types:engine_state().
handle_create_repost(User_id, Original_post_id, Subreddit, State) ->
    case {gleam_stdlib:map_get(erlang:element(5, State), Original_post_id),
        gleam_stdlib:map_get(erlang:element(4, State), Subreddit)} of
        {{ok, Original_post}, {ok, Sub}} ->
            case gleam@list:contains(erlang:element(5, Sub), User_id) of
                true ->
                    Post_id = <<"post_"/utf8,
                        (erlang:integer_to_binary(erlang:element(10, State)))/binary>>,
                    Timestamp = utils:current_timestamp(),
                    Post = {post,
                        Post_id,
                        Subreddit,
                        User_id,
                        <<"[REPOST] "/utf8,
                            (erlang:element(5, Original_post))/binary>>,
                        erlang:element(6, Original_post),
                        1,
                        0,
                        Timestamp,
                        true,
                        {some, Original_post_id},
                        none},
                    New_posts = gleam@dict:insert(
                        erlang:element(5, State),
                        Post_id,
                        Post
                    ),
                    Vote_key = <<<<User_id/binary, "_"/utf8>>/binary,
                        Post_id/binary>>,
                    New_votes = gleam@dict:insert(
                        erlang:element(8, State),
                        Vote_key,
                        upvote
                    ),
                    Updated_sub = {subreddit,
                        erlang:element(2, Sub),
                        erlang:element(3, Sub),
                        erlang:element(4, Sub),
                        erlang:element(5, Sub),
                        erlang:element(6, Sub) + 1,
                        erlang:element(7, Sub)},
                    New_subreddits = gleam@dict:insert(
                        erlang:element(4, State),
                        Subreddit,
                        Updated_sub
                    ),
                    broadcast_to_subreddit_members(
                        erlang:element(5, Sub),
                        {new_post, Post},
                        State
                    ),
                    New_metrics = begin
                        _record = erlang:element(13, State),
                        {performance_metrics,
                            erlang:element(2, erlang:element(13, State)) + 1,
                            erlang:element(3, _record),
                            erlang:element(4, _record),
                            erlang:element(5, _record),
                            erlang:element(6, _record),
                            erlang:element(7, _record),
                            erlang:element(8, erlang:element(13, State)) + 1,
                            erlang:element(9, _record),
                            erlang:element(10, _record),
                            erlang:element(11, _record),
                            erlang:element(12, _record),
                            erlang:element(13, _record)}
                    end,
                    {engine_state,
                        erlang:element(2, State),
                        erlang:element(3, State),
                        New_subreddits,
                        New_posts,
                        erlang:element(6, State),
                        erlang:element(7, State),
                        New_votes,
                        erlang:element(9, State),
                        erlang:element(10, State) + 1,
                        erlang:element(11, State),
                        erlang:element(12, State),
                        New_metrics,
                        erlang:element(14, State),
                        erlang:element(15, State),
                        erlang:element(16, State)};

                false ->
                    State
            end;

        {_, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 725).
-spec handle_vote_post(binary(), binary(), types:vote(), types:engine_state()) -> types:engine_state().
handle_vote_post(User_id, Post_id, Vote, State) ->
    case gleam_stdlib:map_get(erlang:element(5, State), Post_id) of
        {ok, Post} ->
            Vote_key = <<<<User_id/binary, "_"/utf8>>/binary, Post_id/binary>>,
            Previous_vote = gleam_stdlib:map_get(
                erlang:element(8, State),
                Vote_key
            ),
            {New_upvotes, New_downvotes} = update_vote_counts(
                erlang:element(7, Post),
                erlang:element(8, Post),
                Previous_vote,
                {some, Vote}
            ),
            Updated_post = {post,
                erlang:element(2, Post),
                erlang:element(3, Post),
                erlang:element(4, Post),
                erlang:element(5, Post),
                erlang:element(6, Post),
                New_upvotes,
                New_downvotes,
                erlang:element(9, Post),
                erlang:element(10, Post),
                erlang:element(11, Post),
                erlang:element(12, Post)},
            New_posts = gleam@dict:insert(
                erlang:element(5, State),
                Post_id,
                Updated_post
            ),
            New_votes = gleam@dict:insert(
                erlang:element(8, State),
                Vote_key,
                Vote
            ),
            New_users = update_user_karma(
                erlang:element(3, State),
                erlang:element(4, Post),
                ((New_upvotes - New_downvotes) - erlang:element(7, Post)) + erlang:element(
                    8,
                    Post
                )
            ),
            case gleam_stdlib:map_get(
                erlang:element(4, State),
                erlang:element(3, Post)
            ) of
                {ok, Sub} ->
                    broadcast_to_subreddit_members(
                        erlang:element(5, Sub),
                        {post_vote_update, Post_id, New_upvotes, New_downvotes},
                        State
                    );

                {error, _} ->
                    nil
            end,
            New_metrics = begin
                _record = erlang:element(13, State),
                {performance_metrics,
                    erlang:element(2, _record),
                    erlang:element(3, _record),
                    erlang:element(4, erlang:element(13, State)) + 1,
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record),
                    erlang:element(9, _record),
                    erlang:element(10, erlang:element(13, State)) + 1,
                    erlang:element(11, _record),
                    erlang:element(12, _record),
                    erlang:element(13, _record)}
            end,
            {engine_state,
                erlang:element(2, State),
                New_users,
                erlang:element(4, State),
                New_posts,
                erlang:element(6, State),
                erlang:element(7, State),
                New_votes,
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State),
                erlang:element(12, State),
                New_metrics,
                erlang:element(14, State),
                erlang:element(15, State),
                erlang:element(16, State)};

        {error, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 848).
-spec handle_create_comment(
    binary(),
    binary(),
    gleam@option:option(binary()),
    binary(),
    types:engine_state()
) -> types:engine_state().
handle_create_comment(User_id, Post_id, Parent_id, Content, State) ->
    case gleam_stdlib:map_get(erlang:element(5, State), Post_id) of
        {ok, Post} ->
            Comment_id = <<"comment_"/utf8,
                (erlang:integer_to_binary(erlang:element(11, State)))/binary>>,
            Timestamp = utils:current_timestamp(),
            Comment = {comment,
                Comment_id,
                Post_id,
                Parent_id,
                User_id,
                Content,
                1,
                0,
                Timestamp,
                []},
            New_comments = gleam@dict:insert(
                erlang:element(6, State),
                Comment_id,
                Comment
            ),
            Vote_key = <<<<User_id/binary, "_"/utf8>>/binary,
                Comment_id/binary>>,
            New_votes = gleam@dict:insert(
                erlang:element(8, State),
                Vote_key,
                upvote
            ),
            Updated_comments = case Parent_id of
                {some, Pid} ->
                    case gleam_stdlib:map_get(New_comments, Pid) of
                        {ok, Parent_comment} ->
                            Updated_parent = {comment,
                                erlang:element(2, Parent_comment),
                                erlang:element(3, Parent_comment),
                                erlang:element(4, Parent_comment),
                                erlang:element(5, Parent_comment),
                                erlang:element(6, Parent_comment),
                                erlang:element(7, Parent_comment),
                                erlang:element(8, Parent_comment),
                                erlang:element(9, Parent_comment),
                                [Comment_id |
                                    erlang:element(10, Parent_comment)]},
                            gleam@dict:insert(New_comments, Pid, Updated_parent);

                        {error, _} ->
                            New_comments
                    end;

                none ->
                    New_comments
            end,
            case gleam_stdlib:map_get(
                erlang:element(4, State),
                erlang:element(3, Post)
            ) of
                {ok, Sub} ->
                    broadcast_to_subreddit_members(
                        erlang:element(5, Sub),
                        {new_comment, Comment},
                        State
                    );

                {error, _} ->
                    nil
            end,
            New_metrics = begin
                _record = erlang:element(13, State),
                {performance_metrics,
                    erlang:element(2, _record),
                    erlang:element(3, erlang:element(13, State)) + 1,
                    erlang:element(4, _record),
                    erlang:element(5, _record),
                    erlang:element(6, _record),
                    erlang:element(7, _record),
                    erlang:element(8, _record),
                    erlang:element(9, erlang:element(13, State)) + 1,
                    erlang:element(10, _record),
                    erlang:element(11, _record),
                    erlang:element(12, _record),
                    erlang:element(13, _record)}
            end,
            {engine_state,
                erlang:element(2, State),
                erlang:element(3, State),
                erlang:element(4, State),
                erlang:element(5, State),
                Updated_comments,
                erlang:element(7, State),
                New_votes,
                erlang:element(9, State),
                erlang:element(10, State),
                erlang:element(11, State) + 1,
                erlang:element(12, State),
                New_metrics,
                erlang:element(14, State),
                erlang:element(15, State),
                erlang:element(16, State)};

        {error, _} ->
            State
    end.

-file("src/reddit_engine.gleam", 80).
-spec handle_message(types:engine_state(), types:engine_message()) -> gleam@otp@actor:next(types:engine_state(), types:engine_message()).
handle_message(State, Message) ->
    Updated_metrics = case Message of
        update_stats_cache ->
            erlang:element(13, State);

        _ ->
            _record = erlang:element(13, State),
            {performance_metrics,
                erlang:element(2, _record),
                erlang:element(3, _record),
                erlang:element(4, _record),
                erlang:element(5, _record),
                erlang:element(6, erlang:element(13, State)) + 1,
                erlang:element(7, _record),
                erlang:element(8, _record),
                erlang:element(9, _record),
                erlang:element(10, _record),
                erlang:element(11, _record),
                erlang:element(12, _record),
                erlang:element(13, _record)}
    end,
    State@1 = {engine_state,
        erlang:element(2, State),
        erlang:element(3, State),
        erlang:element(4, State),
        erlang:element(5, State),
        erlang:element(6, State),
        erlang:element(7, State),
        erlang:element(8, State),
        erlang:element(9, State),
        erlang:element(10, State),
        erlang:element(11, State),
        erlang:element(12, State),
        Updated_metrics,
        erlang:element(14, State),
        erlang:element(15, State),
        erlang:element(16, State)},
    New_state = case Message of
        {register_user, Username, Password, Public_key} ->
            handle_register_user(Username, Password, Public_key, State@1);

        {authenticate_user, Username@1, Password@1, Reply_to} ->
            handle_authenticate_user(Username@1, Password@1, Reply_to, State@1),
            State@1;

        {login_user, User_id, Session} ->
            handle_login_user(User_id, Session, State@1);

        {logout_user, User_id@1} ->
            handle_logout_user(User_id@1, State@1);

        {create_subreddit, User_id@2, Name, Description} ->
            handle_create_subreddit(User_id@2, Name, Description, State@1);

        {join_subreddit, User_id@3, Subreddit} ->
            handle_join_subreddit(User_id@3, Subreddit, State@1);

        {leave_subreddit, User_id@4, Subreddit@1} ->
            handle_leave_subreddit(User_id@4, Subreddit@1, State@1);

        {create_post, User_id@5, Subreddit@2, Title, Content, Signature} ->
            handle_create_post(
                User_id@5,
                Subreddit@2,
                Title,
                Content,
                Signature,
                State@1
            );

        {create_repost, User_id@6, Original_post_id, Subreddit@3} ->
            handle_create_repost(
                User_id@6,
                Original_post_id,
                Subreddit@3,
                State@1
            );

        {vote_post, User_id@7, Post_id, Vote} ->
            handle_vote_post(User_id@7, Post_id, Vote, State@1);

        {create_comment, User_id@8, Post_id@1, Parent_id, Content@1} ->
            handle_create_comment(
                User_id@8,
                Post_id@1,
                Parent_id,
                Content@1,
                State@1
            );

        {vote_comment, User_id@9, Comment_id, Vote@1} ->
            handle_vote_comment(User_id@9, Comment_id, Vote@1, State@1);

        {get_feed, User_id@10, Limit, Reply_to@1} ->
            handle_get_feed(User_id@10, Limit, Reply_to@1, State@1),
            State@1;

        {get_subreddit_feed, Subreddit@4, Limit@1, Reply_to@2} ->
            handle_get_subreddit_feed(Subreddit@4, Limit@1, Reply_to@2, State@1),
            State@1;

        {get_post, Post_id@2, Reply_to@3} ->
            handle_get_post(Post_id@2, Reply_to@3, State@1),
            State@1;

        {get_comments, Post_id@3, Reply_to@4} ->
            handle_get_comments(Post_id@3, Reply_to@4, State@1),
            State@1;

        {send_direct_message, Sender, Recipient, Content@2, Reply_msg_id} ->
            handle_send_dm(Sender, Recipient, Content@2, Reply_msg_id, State@1);

        {get_messages, User_id@11, Reply_to@5} ->
            handle_get_messages(User_id@11, Reply_to@5, State@1),
            State@1;

        {get_stats, Reply_to@6} ->
            handle_get_stats_cached(Reply_to@6, State@1),
            State@1;

        {get_user_karma, User_id@12, Reply_to@7} ->
            handle_get_user_karma(User_id@12, Reply_to@7, State@1),
            State@1;

        {get_all_subreddits, Reply_to@8} ->
            handle_get_all_subreddits(Reply_to@8, State@1),
            State@1;

        {search_posts, Query, Limit@2, Reply_to@9} ->
            handle_search_posts(Query, Limit@2, Reply_to@9, State@1),
            State@1;

        {search_subreddits, Query@1, Limit@3, Reply_to@10} ->
            handle_search_subreddits(Query@1, Limit@3, Reply_to@10, State@1),
            State@1;

        {search_users, Query@2, Limit@4, Reply_to@11} ->
            handle_search_users(Query@2, Limit@4, Reply_to@11, State@1),
            State@1;

        {get_user_public_key, User_id@13, Reply_to@12} ->
            handle_get_user_public_key(User_id@13, Reply_to@12, State@1),
            State@1;

        {get_active_clients, Reply_to@13} ->
            handle_get_active_clients(Reply_to@13, State@1),
            State@1;

        update_stats_cache ->
            update_stats_cache(State@1)
    end,
    gleam@otp@actor:continue(New_state).

-file("src/reddit_engine.gleam", 20).
-spec start() -> {ok, gleam@erlang@process:subject(types:engine_message())} |
    {error, gleam@otp@actor:start_error()}.
start() ->
    Result = begin
        _pipe@3 = gleam@otp@actor:new_with_initialiser(
            1000,
            fun(Self_subject) ->
                Start_time = utils:current_timestamp(),
                State = {engine_state,
                    Self_subject,
                    maps:new(),
                    maps:new(),
                    maps:new(),
                    maps:new(),
                    maps:new(),
                    maps:new(),
                    maps:new(),
                    1,
                    1,
                    1,
                    {performance_metrics,
                        0,
                        0,
                        0,
                        0,
                        0,
                        Start_time,
                        0,
                        0,
                        0,
                        +0.0,
                        +0.0,
                        +0.0},
                    Start_time,
                    none,
                    Start_time},
                Initial_stats = calculate_stats(State),
                State_with_cache = {engine_state,
                    erlang:element(2, State),
                    erlang:element(3, State),
                    erlang:element(4, State),
                    erlang:element(5, State),
                    erlang:element(6, State),
                    erlang:element(7, State),
                    erlang:element(8, State),
                    erlang:element(9, State),
                    erlang:element(10, State),
                    erlang:element(11, State),
                    erlang:element(12, State),
                    erlang:element(13, State),
                    erlang:element(14, State),
                    {some, Initial_stats},
                    erlang:element(16, State)},
                gleam@erlang@process:send_after(
                    Self_subject,
                    3000,
                    update_stats_cache
                ),
                _pipe = State_with_cache,
                _pipe@1 = gleam@otp@actor:initialised(_pipe),
                _pipe@2 = gleam@otp@actor:returning(_pipe@1, Self_subject),
                {ok, _pipe@2}
            end
        ),
        _pipe@4 = gleam@otp@actor:on_message(_pipe@3, fun handle_message/2),
        gleam@otp@actor:start(_pipe@4)
    end,
    case Result of
        {ok, Actor_started} ->
            {ok, erlang:element(3, Actor_started)};

        {error, E} ->
            {error, E}
    end.
