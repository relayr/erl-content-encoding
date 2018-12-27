-module(content_encoding_tests).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

-ifdef(TEST).

-define(TEXT, <<"TO be OR not TO be\n">>).
-define(TEXT_GZIP, <<31,139,8,0,0,0,0,0,0,3,11,241,87,72,74,85,240,15,82,200,
    203,47,81,8,1,113,184,0,102,78,183,52,19,0,0,0>>).
-define(TEXT_DEFLATE, <<120,156,11,241,87,72,74,85,240,15,82,200,203,47,81,8,1,
    113,184,0,55,179,5,113>>).

encode_undefined_test() ->
    ?assertEqual({?TEXT, undefined}, content_encoding:encode(undefined, ?TEXT)).

encode_identity_test() ->
    ?assertEqual({?TEXT, undefined}, content_encoding:encode(<<"identity">>, ?TEXT)).

encode_gzip_test() ->
    ?assertEqual({?TEXT_GZIP, <<"gzip">>}, content_encoding:encode(<<"gzip">>, ?TEXT)).

encode_deflate_test() ->
    ?assertEqual({?TEXT_DEFLATE, <<"deflate">>}, content_encoding:encode(<<"deflate">>, ?TEXT)).

encode_choose_first_known_test() ->
    ?assertEqual({?TEXT_DEFLATE, <<"deflate">>}, content_encoding:encode(<<"unknown, deflate, gzip">>, ?TEXT)).

encode_identity_if_none_known_test() ->
    ?assertEqual({?TEXT, undefined}, content_encoding:encode(<<"unknown, unknowntoo">>, ?TEXT)).

decode_undefined_test() ->
    ?assertEqual(?TEXT, content_encoding:decode(undefined, ?TEXT)).

decode_identity_test() ->
    ?assertEqual(?TEXT, content_encoding:decode(<<"identity">>, ?TEXT)).

decode_gzip_test() ->
    ?assertEqual(?TEXT, content_encoding:decode(<<"gzip">>, ?TEXT_GZIP)).

decode_deflate_test() ->
    ?assertEqual(?TEXT, content_encoding:decode(<<"deflate">>, ?TEXT_DEFLATE)).

-endif.
