%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Nonce generation and validation functions.
%% @end
%%%-------------------------------------------------------------------
-module(ejango.tokens).

-import(eunit).
-include_lib("eunit/include/eunit.hrl").

-import(crypto).
-import(mochihex).
-import(lists).

%% API
-export([id/0]).

%%====================================================================
%% API
%%====================================================================

id() ->
    mochihex:to_hex(crypto:rand_bytes(20)).

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
