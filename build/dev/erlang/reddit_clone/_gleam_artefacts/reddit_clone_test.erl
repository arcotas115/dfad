-module(reddit_clone_test).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "test/reddit_clone_test.gleam").
-export([main/0, hash_password_test/0, verify_password_test/0, generate_key_pair_test/0, sign_and_verify_test/0, random_int_test/0, contains_lowercase_test/0, current_timestamp_test/0]).

-file("test/reddit_clone_test.gleam", 5).
-spec main() -> nil.
main() ->
    gleeunit:main().

-file("test/reddit_clone_test.gleam", 10).
-spec hash_password_test() -> nil.
hash_password_test() ->
    Password = <<"test_password"/utf8>>,
    Hash = utils:hash_password(Password),
    gleeunit@should:not_equal(Hash, Password),
    Hash2 = utils:hash_password(Password),
    gleeunit@should:equal(Hash, Hash2).

-file("test/reddit_clone_test.gleam", 23).
-spec verify_password_test() -> nil.
verify_password_test() ->
    Password = <<"secret123"/utf8>>,
    Hash = utils:hash_password(Password),
    gleeunit@should:be_true(utils:verify_password(Password, Hash)),
    gleeunit@should:be_false(utils:verify_password(<<"wrong"/utf8>>, Hash)).

-file("test/reddit_clone_test.gleam", 35).
-spec generate_key_pair_test() -> nil.
generate_key_pair_test() ->
    {Pub_key, Priv_key} = utils:generate_key_pair(),
    gleeunit@should:not_equal(Pub_key, <<""/utf8>>),
    gleeunit@should:not_equal(Priv_key, <<""/utf8>>).

-file("test/reddit_clone_test.gleam", 44).
-spec sign_and_verify_test() -> nil.
sign_and_verify_test() ->
    {Pub_key, Priv_key} = utils:generate_key_pair(),
    Content = <<"Hello, World!"/utf8>>,
    Signature = utils:sign_content(Content, Priv_key),
    gleeunit@should:not_equal(Signature, <<""/utf8>>),
    gleeunit@should:be_true(utils:verify_signature(Content, Signature, Pub_key)),
    gleeunit@should:be_false(
        utils:verify_signature(<<"Wrong content"/utf8>>, Signature, Pub_key)
    ).

-file("test/reddit_clone_test.gleam", 62).
-spec random_int_test() -> nil.
random_int_test() ->
    Val = utils:random_int(100),
    gleeunit@should:be_true(Val >= 0),
    gleeunit@should:be_true(Val =< 100).

-file("test/reddit_clone_test.gleam", 71).
-spec contains_lowercase_test() -> nil.
contains_lowercase_test() ->
    gleeunit@should:be_true(
        utils:contains_lowercase(<<"Hello World"/utf8>>, <<"world"/utf8>>)
    ),
    gleeunit@should:be_true(
        utils:contains_lowercase(<<"HELLO"/utf8>>, <<"hello"/utf8>>)
    ),
    gleeunit@should:be_false(
        utils:contains_lowercase(<<"Hello"/utf8>>, <<"xyz"/utf8>>)
    ).

-file("test/reddit_clone_test.gleam", 78).
-spec current_timestamp_test() -> nil.
current_timestamp_test() ->
    Ts1 = utils:current_timestamp(),
    Ts2 = utils:current_timestamp(),
    gleeunit@should:be_true(Ts1 > 0),
    gleeunit@should:be_true(Ts2 >= Ts1).
