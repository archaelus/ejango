%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Nonce generation and validation functions.
%% @end
%%%-------------------------------------------------------------------
-module(ej_tokens).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([id/0]).

%%====================================================================
%% API
%%====================================================================

id() ->
   to_hex(crypto:rand_bytes(20)).

to_hex(Bin) when is_binary(Bin) ->
    [lower_case(hd(erlang:integer_to_list(Nibble,16)))
     || <<Nibble:4>> <= Bin].

lower_case(C) when $A =< C, C =< $F -> C - $A + $a;
lower_case(C) -> C.

%%====================================================================
%% Internal functions
%%====================================================================

% @private
token_id_test() ->
    ?assert(case id() of
                X when is_list(X), length(X) == 40 -> true;
                _ -> false
            end),
    ?assert(lists:all(fun (C) when $a =< C, C =< $f;
                                   $0 =< C, C =< $9 ->
                              true;
                          (_) -> false
                      end, id())).
