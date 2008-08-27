%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Salted, hashed passwords
%% @end
%%%-------------------------------------------------------------------
-module(ejango.passwords).

-import(eunit).
-include_lib("eunit.hrl").

-import(crypto).

%% API
-export([new/1,
         verify/2]).

-record(password, {hash,
                   salt,
                   algo}).

-define(ALGO, sha).

%%====================================================================
%% API
%%====================================================================

new(Plaintext) when is_list(Plaintext); is_binary(Plaintext) ->
    Salt = salt(),
    #password{hash=hash(?ALGO, Salt, Plaintext),
              salt=Salt,
              algo=?ALGO}.

verify(P = #password{hash=Hash}, Plaintext) ->
    Hash =:= hash(P, Plaintext).

%%====================================================================
%% Internal functions
%%====================================================================

hash(#password{algo=Algo,salt=Salt}, Plaintext) ->
    hash(Algo, Salt, Plaintext).
hash(Algo, Salt, Plaintext) ->
    crypto:Algo(iolist_to_binary([Salt, Plaintext])).

salt() ->
    crypto:rand_bytes(8).

new_test() ->
    ?assertMatch(#password{}, new("Test password")),
    ?assertMatch(#password{}, new(<<"Test password">>)).

verify_test() ->
    P = new("Test password"),
    P2 = new("Other Test password"),
    ?assertMatch(true,
                 verify(P, "Test password")),
    ?assertMatch(false,
                 verify(P2, "Test password")),
    ?assertMatch(false,
                 verify(P, "Other Test password")),
    ?assertMatch(true,
                 verify(P2, "Other Test password")).
    
                 
