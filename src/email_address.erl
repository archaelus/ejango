%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Email address utility functions
%% @end
%%%-------------------------------------------------------------------
-module(ejango.email_address).

-import(eunit).
-include_lib("eunit.hrl").

-import(lists).

%% API
-export([is_valid/1,
         validate/1,
         errors/1]).

%%====================================================================
%% API
%%====================================================================

errors(Address) ->
    case validate(Address) of
        true -> ok;
        E -> E
    end.

is_valid(Address) ->
    validate(Address) =:= true.

validate(Addr) ->
    case split_email_address(Addr) of
        {_User, ""} -> {error, no_at_symbol};
        {_User, "@" ++ _} -> {error, too_many_at_symbols};
        {User, Host} ->
            is_valid_email_address(User, Host)
    end.

is_valid_email_address(User, Host) ->
    case sanitise_email_user(User) of 
        U when U =:= User -> is_valid_email_host(Host);
        _ -> {error, bad_user_name}
    end.

is_valid_email_host(Host) ->
    case domain:sanitise(Host) of 
        H when H =:= Host -> true;
        _ -> {error, bad_host_name}
    end.


sanitise_email_user(U) ->
    lists:filter(fun (C) when $A =< C, C =< $Z;
                     $a =< C, C =< $z;
                     $0 =< C, C =< $9;
                     C =:= $_;
                     C =:= $.;
                     C =:= $+;
                     C =:= $- ->
                         true;
                     (_) -> false
                 end,
                 U).

sanitise_host_user(U) ->
    lists:filter(fun (C) when $A =< C, C =< $Z;
                     $a =< C, C =< $z;
                     $0 =< C, C =< $9;
                     C =:= $_;
                     C =:= $.;
                     C =:= $- ->
                         true;
                     (_) -> false
                 end,
                 U).

is_valid_email_address_test() ->
    ?assertMatch(true, validate("nem@erlang.geek.nz")),
    ?assertMatch({error, too_many_at_symbols}, validate("nem@@erlang.geek.nz")),
    ?assertMatch({error, bad_user_name}, validate("n!em@erlang.geek.nz")),
    ?assertMatch({error, bad_host_name}, validate("nem@erl!ang.geek.nz")).

split_email_address(Addr) ->
    case lists:splitwith(fun ($@) -> false;
                             (_) -> true
                         end, Addr) of
        {User, "@" ++ Host} ->
            {User, Host};
        Else -> Else
    end.

split_email_address_test() ->
    ?assertMatch({"nem", "erlang.geek.nz"},
                 split_email_address("nem@erlang.geek.nz")),
    ?assertMatch({"nem", "@erlang.geek.nz"},
                 split_email_address("nem@@erlang.geek.nz")).


%%====================================================================
%% Internal functions
%%====================================================================
