-module(reddit_clone).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/reddit_clone.gleam").
-export([main/0]).

-file("src/reddit_clone.gleam", 9).
-spec main() -> nil.
main() ->
    gleam_stdlib:println(
        <<"\n╔═══════════════════════════════════════════════════════════════╗"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║          Reddit Clone - REST API Server Starting             ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"║       With Search, Digital Signatures & Request Logging      ║"/utf8>>
    ),
    gleam_stdlib:println(
        <<"╚═══════════════════════════════════════════════════════════════╝\n"/utf8>>
    ),
    gleam_stdlib:println(<<"🚀 Starting Reddit engine actor..."/utf8>>),
    Engine_subject@1 = case reddit_engine:start() of
        {ok, Engine_subject} -> Engine_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"reddit_clone"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 21,
                        value => _assert_fail,
                        start => 874,
                        'end' => 927,
                        pattern_start => 885,
                        pattern_end => 903})
    end,
    gleam_stdlib:println(<<"✅ Reddit engine started successfully\n"/utf8>>),
    gleam_stdlib:println(
        <<"🌐 Starting REST API server on http://localhost:8080..."/utf8>>
    ),
    case api_server:start(Engine_subject@1, 8080) of
        {ok, _} -> nil;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                        file => <<?FILEPATH/utf8>>,
                        module => <<"reddit_clone"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 26,
                        value => _assert_fail@1,
                        start => 1093,
                        'end' => 1150,
                        pattern_start => 1104,
                        pattern_end => 1109})
    end,
    gleam_stdlib:println(<<"✅ REST API server started successfully\n"/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(<<"  Server is ready to accept requests!"/utf8>>),
    gleam_stdlib:println(<<"  API Base URL: http://localhost:8080/api"/utf8>>),
    gleam_stdlib:println(
        <<"  Health Check: http://localhost:8080/health"/utf8>>
    ),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    gleam_stdlib:println(<<"📚 Available Endpoints:"/utf8>>),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  User Management:"/utf8>>),
    gleam_stdlib:println(
        <<"    POST   /api/register              - Register user (with optional public_key)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/login                 - Authenticate user"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/user/:username/karma  - Get user karma"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/user/:username/publickey - Get user's public key"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Subreddit Management:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/subreddits            - List all subreddits"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/subreddit/:name       - Get specific subreddit"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/subreddit             - Create subreddit"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/subreddit/:name/join  - Join subreddit"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/subreddit/:name/leave - Leave subreddit"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Posts:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/posts                 - Get user's feed"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/subreddit/:name/posts - Get subreddit posts"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/post/:id              - Get post (verifies signature)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/post                  - Create post (with optional signature)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/post/:id/upvote       - Upvote post"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/post/:id/downvote     - Downvote post"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Comments:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/post/:id/comments     - Get post comments"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/comment               - Create comment"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/comment/:id/upvote    - Upvote comment"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/comment/:id/downvote  - Downvote comment"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Messages:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/messages              - Get messages"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    POST   /api/message               - Send message"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Search:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/search/posts?q=       - Search posts"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/search/subreddits?q=  - Search subreddits"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/search/users?q=       - Search users"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(<<"  Statistics:"/utf8>>),
    gleam_stdlib:println(
        <<"    GET    /api/stats                 - Get system stats (with rates)"/utf8>>
    ),
    gleam_stdlib:println(
        <<"    GET    /api/clients               - Get active clients count"/utf8>>
    ),
    gleam_stdlib:println(<<""/utf8>>),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"/utf8>>
    ),
    gleam_stdlib:println(
        <<"📝 Request logging is ENABLED - all requests will be logged"/utf8>>
    ),
    gleam_stdlib:println(
        <<"🔐 Digital signatures are SUPPORTED for posts"/utf8>>
    ),
    gleam_stdlib:println(
        <<"━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"/utf8>>
    ),
    gleam_stdlib:println(<<"Press Ctrl+C to stop the server\n"/utf8>>),
    gleam_erlang_ffi:sleep_forever().
