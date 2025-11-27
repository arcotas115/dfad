-module(utils).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/utils.gleam").
-export([generate_id/1, current_timestamp/0, random_float/0, random_int/1, zipf_sample/2, hash_password/1, verify_password/2, generate_key_pair/0, sign_content/2, verify_signature/3, random_username/0, random_subreddit_name/0, random_post_title/0, random_post_content/0, random_comment/0, random_dm_content/0, format_duration/1, format_timestamp/1, sleep/1, is_none/1, is_some/1, contains_lowercase/2]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/utils.gleam", 41).
-spec do_bit_array_to_list(bitstring(), list(integer())) -> list(integer()).
do_bit_array_to_list(Bits, Acc) ->
    case Bits of
        <<Byte:8, Rest/binary>> ->
            do_bit_array_to_list(Rest, [Byte | Acc]);

        <<>> ->
            Acc;

        _ ->
            Acc
    end.

-file("src/utils.gleam", 36).
-spec bit_array_to_list(bitstring()) -> list(integer()).
bit_array_to_list(Bits) ->
    _pipe = do_bit_array_to_list(Bits, []),
    lists:reverse(_pipe).

-file("src/utils.gleam", 23).
-spec encode_hex(bitstring()) -> binary().
encode_hex(Bytes) ->
    _pipe = Bytes,
    _pipe@1 = bit_array_to_list(_pipe),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Byte) ->
            Hex = gleam@int:to_base16(Byte),
            case string:length(Hex) of
                1 ->
                    <<"0"/utf8, Hex/binary>>;

                _ ->
                    Hex
            end
        end
    ),
    gleam@string:join(_pipe@2, <<""/utf8>>).

-file("src/utils.gleam", 17).
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Random_bytes = crypto:strong_rand_bytes(8),
    Hex = encode_hex(Random_bytes),
    <<<<Prefix/binary, "_"/utf8>>/binary, Hex/binary>>.

-file("src/utils.gleam", 56).
-spec current_timestamp() -> integer().
current_timestamp() ->
    erlang:system_time(erlang:binary_to_atom(<<"millisecond"/utf8>>)).

-file("src/utils.gleam", 70).
-spec random_float() -> float().
random_float() ->
    rand:uniform().

-file("src/utils.gleam", 74).
-spec random_int(integer()) -> integer().
random_int(Max) ->
    case Max of
        0 ->
            0;

        N ->
            rand:uniform(N)
    end.

-file("src/utils.gleam", 91).
-spec harmonic_number(integer(), float()) -> float().
harmonic_number(N, Alpha) ->
    case N of
        0 ->
            +0.0;

        _ ->
            _pipe = gleam@list:range(1, N),
            _pipe@1 = gleam@list:map(
                _pipe,
                fun(I) ->
                    I_float = erlang:float(I),
                    Power_result@1 = case gleam@float:power(I_float, Alpha) of
                        {ok, Power_result} -> Power_result;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                        file => <<?FILEPATH/utf8>>,
                                        module => <<"utils"/utf8>>,
                                        function => <<"harmonic_number"/utf8>>,
                                        line => 98,
                                        value => _assert_fail,
                                        start => 2495,
                                        'end' => 2552,
                                        pattern_start => 2506,
                                        pattern_end => 2522})
                    end,
                    case Power_result@1 of
                        +0.0 -> +0.0;
                        -0.0 -> -0.0;
                        Gleam@denominator -> 1.0 / Gleam@denominator
                    end
                end
            ),
            gleam@list:fold(_pipe@1, +0.0, fun gleam@float:add/2)
    end.

-file("src/utils.gleam", 106).
-spec find_zipf_value(integer(), integer(), float(), float(), float()) -> integer().
find_zipf_value(I, N, U, Alpha, Acc) ->
    case I > N of
        true ->
            N;

        false ->
            I_float = erlang:float(I),
            Power_result@1 = case gleam@float:power(I_float, Alpha) of
                {ok, Power_result} -> Power_result;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"utils"/utf8>>,
                                function => <<"find_zipf_value"/utf8>>,
                                line => 111,
                                value => _assert_fail,
                                start => 2804,
                                'end' => 2861,
                                pattern_start => 2815,
                                pattern_end => 2831})
            end,
            Prob = case Power_result@1 of
                +0.0 -> +0.0;
                -0.0 -> -0.0;
                Gleam@denominator -> 1.0 / Gleam@denominator
            end,
            New_acc = Acc + Prob,
            case New_acc >= U of
                true ->
                    I;

                false ->
                    find_zipf_value(I + 1, N, U, Alpha, New_acc)
            end
    end.

-file("src/utils.gleam", 85).
-spec zipf_sample(integer(), float()) -> integer().
zipf_sample(N, Alpha) ->
    H_n = harmonic_number(N, Alpha),
    U = random_float() * H_n,
    find_zipf_value(1, N, U, Alpha, +0.0).

-file("src/utils.gleam", 126).
-spec hash_password(binary()) -> binary().
hash_password(Password) ->
    _pipe = gleam@crypto:hash(sha256, <<Password/binary>>),
    encode_hex(_pipe).

-file("src/utils.gleam", 132).
-spec verify_password(binary(), binary()) -> boolean().
verify_password(Password, Hash) ->
    hash_password(Password) =:= Hash.

-file("src/utils.gleam", 145).
?DOC(
    " Generate a key pair (public/private) for signing\n"
    " For simplicity, we use the same key for both (symmetric)\n"
    " In real implementation, this would be RSA or ECDSA\n"
).
-spec generate_key_pair() -> {binary(), binary()}.
generate_key_pair() ->
    Key_bytes = crypto:strong_rand_bytes(32),
    Key_hex = encode_hex(Key_bytes),
    {Key_hex, Key_hex}.

-file("src/utils.gleam", 155).
?DOC(
    " Sign content with a private key\n"
    " Uses HMAC-SHA256 for demonstration\n"
).
-spec sign_content(binary(), binary()) -> binary().
sign_content(Content, Private_key) ->
    case gleam_stdlib:base16_decode(Private_key) of
        {ok, Key_bytes} ->
            Content_bytes = <<Content/binary>>,
            Signature = gleam_crypto_ffi:hmac(Content_bytes, sha256, Key_bytes),
            encode_hex(Signature);

        {error, _} ->
            Combined = <<Content/binary, Private_key/binary>>,
            _pipe = gleam@crypto:hash(sha256, <<Combined/binary>>),
            encode_hex(_pipe)
    end.

-file("src/utils.gleam", 173).
?DOC(" Verify a signature against content and public key\n").
-spec verify_signature(binary(), binary(), binary()) -> boolean().
verify_signature(Content, Signature, Public_key) ->
    Expected_signature = sign_content(Content, Public_key),
    string:lowercase(Signature) =:= string:lowercase(Expected_signature).

-file("src/utils.gleam", 203).
-spec list_at(list(binary()), integer()) -> binary().
list_at(Lst, Index) ->
    case gleam@list:drop(Lst, Index - 1) of
        [Head | _] ->
            Head;

        [] ->
            <<"Unknown"/utf8>>
    end.

-file("src/utils.gleam", 186).
-spec random_username() -> binary().
random_username() ->
    Adjectives = [<<"Happy"/utf8>>,
        <<"Clever"/utf8>>,
        <<"Swift"/utf8>>,
        <<"Brave"/utf8>>,
        <<"Wise"/utf8>>,
        <<"Noble"/utf8>>,
        <<"Mighty"/utf8>>,
        <<"Gentle"/utf8>>,
        <<"Bold"/utf8>>,
        <<"Bright"/utf8>>,
        <<"Cool"/utf8>>,
        <<"Epic"/utf8>>,
        <<"Fierce"/utf8>>,
        <<"Grand"/utf8>>],
    Nouns = [<<"Tiger"/utf8>>,
        <<"Eagle"/utf8>>,
        <<"Wolf"/utf8>>,
        <<"Bear"/utf8>>,
        <<"Lion"/utf8>>,
        <<"Hawk"/utf8>>,
        <<"Falcon"/utf8>>,
        <<"Dragon"/utf8>>,
        <<"Phoenix"/utf8>>,
        <<"Warrior"/utf8>>,
        <<"Knight"/utf8>>,
        <<"Wizard"/utf8>>,
        <<"Sage"/utf8>>,
        <<"Hero"/utf8>>],
    Adj = list_at(Adjectives, random_int(erlang:length(Adjectives))),
    Noun = list_at(Nouns, random_int(erlang:length(Nouns))),
    Number = erlang:integer_to_binary(random_int(9999)),
    <<<<Adj/binary, Noun/binary>>/binary, Number/binary>>.

-file("src/utils.gleam", 210).
-spec random_subreddit_name() -> binary().
random_subreddit_name() ->
    Prefixes = [<<"Ask"/utf8>>,
        <<"Learn"/utf8>>,
        <<"Share"/utf8>>,
        <<"Discuss"/utf8>>,
        <<"Show"/utf8>>,
        <<"Tell"/utf8>>,
        <<"Find"/utf8>>,
        <<"Explore"/utf8>>,
        <<"Discover"/utf8>>,
        <<"Master"/utf8>>,
        <<"Build"/utf8>>,
        <<"Create"/utf8>>],
    Topics = [<<"Programming"/utf8>>,
        <<"Science"/utf8>>,
        <<"Art"/utf8>>,
        <<"Music"/utf8>>,
        <<"Gaming"/utf8>>,
        <<"Books"/utf8>>,
        <<"Movies"/utf8>>,
        <<"Food"/utf8>>,
        <<"Travel"/utf8>>,
        <<"Tech"/utf8>>,
        <<"Sports"/utf8>>,
        <<"History"/utf8>>,
        <<"Philosophy"/utf8>>,
        <<"Nature"/utf8>>,
        <<"Space"/utf8>>,
        <<"Math"/utf8>>,
        <<"Design"/utf8>>],
    Prefix = list_at(Prefixes, random_int(erlang:length(Prefixes))),
    Topic = list_at(Topics, random_int(erlang:length(Topics))),
    <<Prefix/binary, Topic/binary>>.

-file("src/utils.gleam", 227).
-spec random_post_title() -> binary().
random_post_title() ->
    Templates = [<<"What's your opinion on "/utf8>>,
        <<"Just discovered "/utf8>>,
        <<"Tips for improving "/utf8>>,
        <<"My experience with "/utf8>>,
        <<"Anyone else think "/utf8>>,
        <<"Best practices for "/utf8>>,
        <<"How to master "/utf8>>,
        <<"The truth about "/utf8>>,
        <<"Why I love "/utf8>>,
        <<"Discussion: "/utf8>>,
        <<"Unpopular opinion: "/utf8>>,
        <<"PSA: Remember to "/utf8>>],
    Topics = [<<"this new framework"/utf8>>,
        <<"modern development"/utf8>>,
        <<"learning techniques"/utf8>>,
        <<"productivity tips"/utf8>>,
        <<"industry trends"/utf8>>,
        <<"recent updates"/utf8>>,
        <<"community guidelines"/utf8>>,
        <<"best practices"/utf8>>,
        <<"common mistakes"/utf8>>,
        <<"advanced features"/utf8>>,
        <<"beginner resources"/utf8>>,
        <<"career advice"/utf8>>],
    Template = list_at(Templates, random_int(erlang:length(Templates))),
    Topic = list_at(Topics, random_int(erlang:length(Topics))),
    <<Template/binary, Topic/binary>>.

-file("src/utils.gleam", 264).
-spec random_post_content() -> binary().
random_post_content() ->
    Paragraphs = [<<"I've been thinking about this topic for a while now, and I wanted to share my thoughts with the community."/utf8>>,
        <<"After extensive research and experimentation, I've come to some interesting conclusions."/utf8>>,
        <<"This might be controversial, but I believe it's important to discuss different perspectives."/utf8>>,
        <<"Based on my experience over the past few years, here's what I've learned."/utf8>>,
        <<"I'm curious to hear what others think about this. Please share your opinions!"/utf8>>,
        <<"This has been a game-changer for me, and I think it could help others too."/utf8>>,
        <<"Let's have a constructive discussion about the pros and cons."/utf8>>,
        <<"I've compiled some resources that might be helpful for anyone interested in this topic."/utf8>>],
    Num_paragraphs = random_int(3) + 1,
    _pipe = gleam@list:range(1, Num_paragraphs),
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(_) -> list_at(Paragraphs, random_int(erlang:length(Paragraphs))) end
    ),
    gleam@string:join(_pipe@1, <<"\n\n"/utf8>>).

-file("src/utils.gleam", 282).
-spec random_comment() -> binary().
random_comment() ->
    Comments = [<<"Great post! I completely agree with your points."/utf8>>,
        <<"Interesting perspective. Have you considered the alternative?"/utf8>>,
        <<"Thanks for sharing! This is really helpful."/utf8>>,
        <<"I had a similar experience. Here's what worked for me..."/utf8>>,
        <<"Could you elaborate on that last point?"/utf8>>,
        <<"This is exactly what I needed to read today!"/utf8>>,
        <<"I respectfully disagree. Here's why..."/utf8>>,
        <<"Adding to your point, I think it's also important to consider..."/utf8>>,
        <<"Has anyone tried this approach in production?"/utf8>>,
        <<"Sources? I'd love to read more about this."/utf8>>,
        <<"This deserves more upvotes!"/utf8>>,
        <<"Quality content. Thanks for taking the time to write this."/utf8>>],
    list_at(Comments, random_int(erlang:length(Comments))).

-file("src/utils.gleam", 301).
-spec random_dm_content() -> binary().
random_dm_content() ->
    Messages = [<<"Hey! I saw your post and wanted to reach out."/utf8>>,
        <<"Thanks for your contribution to the community!"/utf8>>,
        <<"Quick question about your recent comment..."/utf8>>,
        <<"I really appreciated your insights on that topic."/utf8>>,
        <<"Would you be interested in collaborating?"/utf8>>,
        <<"Just wanted to say thanks for the helpful advice!"/utf8>>,
        <<"Could you recommend any resources on this subject?"/utf8>>,
        <<"Your post inspired me to try something new!"/utf8>>],
    list_at(Messages, random_int(erlang:length(Messages))).

-file("src/utils.gleam", 320).
-spec format_duration(integer()) -> binary().
format_duration(Milliseconds) ->
    Seconds = Milliseconds div 1000,
    Minutes = Seconds div 60,
    Hours = Minutes div 60,
    case Hours of
        0 ->
            case Minutes of
                0 ->
                    <<(erlang:integer_to_binary(Seconds))/binary, "s"/utf8>>;

                M ->
                    <<<<<<(erlang:integer_to_binary(M))/binary, "m "/utf8>>/binary,
                            (erlang:integer_to_binary(Seconds rem 60))/binary>>/binary,
                        "s"/utf8>>
            end;

        H ->
            <<<<<<(erlang:integer_to_binary(H))/binary, "h "/utf8>>/binary,
                    (erlang:integer_to_binary(Minutes rem 60))/binary>>/binary,
                "m"/utf8>>
    end.

-file("src/utils.gleam", 335).
-spec format_timestamp(integer()) -> binary().
format_timestamp(Timestamp) ->
    Seconds = Timestamp div 1000,
    erlang:integer_to_binary(Seconds).

-file("src/utils.gleam", 346).
-spec sleep(integer()) -> nil.
sleep(Milliseconds) ->
    timer:sleep(Milliseconds).

-file("src/utils.gleam", 352).
-spec is_none(gleam@option:option(any())) -> boolean().
is_none(Opt) ->
    case Opt of
        none ->
            true;

        {some, _} ->
            false
    end.

-file("src/utils.gleam", 359).
-spec is_some(gleam@option:option(any())) -> boolean().
is_some(Opt) ->
    case Opt of
        none ->
            false;

        {some, _} ->
            true
    end.

-file("src/utils.gleam", 370).
-spec contains_lowercase(binary(), binary()) -> boolean().
contains_lowercase(Haystack, Needle) ->
    Haystack_lower = string:lowercase(Haystack),
    Needle_lower = string:lowercase(Needle),
    gleam_stdlib:contains_string(Haystack_lower, Needle_lower).
