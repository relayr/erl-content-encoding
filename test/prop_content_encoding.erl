%%------------------------------------------------------------------------------
%% @author jodias
%% @copyright relayr GmbH 2019
%% @version 1.0
%%------------------------------------------------------------------------------
-module(prop_content_encoding).
-author("kuba.odias").

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Property based tests
%% =============================================================================
-ifdef(TEST).

prop_encode_decode() ->
    ?FORALL({Encoding, Bin}, {oneof(["gzip", "identity", "deflate", undefined]), binary()},
        begin
            {EncBin, Enc} = content_encoding:encode(Encoding, Bin),
            ?assert(is_binary(EncBin)),
            DecBin = content_encoding:decode(Enc, EncBin),
            Bin =:= DecBin
        end
    ).

-endif.
