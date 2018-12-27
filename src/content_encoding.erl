-module(content_encoding).

%% API
-export([
    decode/2,
    encode/2
]).

decode(undefined, Content) -> Content;
decode(StringEncoding, Content) when is_list(StringEncoding) -> decode(list_to_binary(StringEncoding), Content);
decode(<<"gzip">>, Content) -> zlib:gunzip(Content);
decode(<<"identity">>, Content) -> Content;
decode(<<"deflate">>, Content) -> inflate(Content).

encode(undefined, Content) ->
    choose_encoding_and_encode([], Content);
encode(EncodingList, Content) when is_list(EncodingList) ->
    encode(list_to_binary(EncodingList), Content);
encode(EncodingList, Content) when is_binary(EncodingList) ->
    choose_encoding_and_encode(binary:split(EncodingList, [<<",">>, <<" ">>], [trim, global]), Content).

choose_encoding_and_encode([], Content) -> {Content, undefined};
choose_encoding_and_encode(_Encoding, <<>> = Content) -> {Content, undefined};
choose_encoding_and_encode([<<"gzip">> | _], Content) -> {zlib:gzip(Content), <<"gzip">>};
choose_encoding_and_encode([<<"identity">> | _], Content) -> {Content, undefined};
choose_encoding_and_encode([<<"deflate">> | _], Content) -> {deflate(Content), <<"deflate">>};
choose_encoding_and_encode([_ | Others], Content) -> choose_encoding_and_encode(Others, Content).

%%%-------------------------------------------------------------------
%%% Private
%%%-------------------------------------------------------------------
deflate(Data) ->
    Z = zlib:open(),
    ok = zlib:deflateInit(Z),
    Encoded = zlib:deflate(Z, Data),
    Last = zlib:deflate(Z, [], finish),
    ok = zlib:deflateEnd(Z),
    iolist_to_binary([Encoded | Last]).

inflate(Data) ->
    Z = zlib:open(),
    ok = zlib:inflateInit(Z),
    Decoded = zlib:inflate(Z, Data),
    ok = zlib:inflateEnd(Z),
    iolist_to_binary(Decoded).
