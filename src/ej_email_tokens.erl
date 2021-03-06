%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Library of utilities to implement action confirmation via
%% email tokens
%% @end
%%%-------------------------------------------------------------------
-module(ej_email_tokens).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([create_token/1,
         verify_token/3,
         data/1,
         token/1,
         expired/2]).

-export([storage_init/1,
         store/2,
         delete/2,
         consume/2]).

-export([field_validation/1]).

-record(email_token, {token
                      ,data
                      ,issued
                     }).

%%====================================================================
%% API
%%====================================================================

%% @type token() = #email_token{}.
%% An opaque email confirmation token.

%% @type date() = {{Year, Month, Day},{Hours, Minutes, Seconds}}.
%% A universal datetime (UTC+0).

%% @spec create_token(Data::term()) -> token()
%% @doc Creates a new opqaue token (issued now) with a given Data.
%% @end
create_token(Data) ->
    create_token(Data, calendar:universal_time()).
%% @spec create_token(Data::term(), Issued::date()) -> token()
%% @doc Creates a new opqaue token (issued on the given date) with a given Data.
%% @end
create_token(Data, Issued) ->
    #email_token{data=Data,
                 issued=Issued,
                 token=token_id()}.

%% @spec verify_token(token(), TokenID::string(), Period::integer()) -> ok | {error, wrong_token} | {error, expired}
%% @doc Verifies a confirmation token (not supplied by the user)
%% against the confirmation string supplied by the user and an expiry
%% period in seconds. Elapsed expiry time is now() - the token issue time.
%% @end
verify_token(Token, TokenID, Period) ->
    verify_token(Token, TokenID, Period, calendar:universal_time()).

%% @spec verify_token(token() | [token()],
%%                    Token::string(),
%%                    Period::integer(),
%%                    Now::date()) -> ok | {error, wrong_token} | {error, expired}
%% @doc Verifies a confirmation token (not supplied by the user)
%% against the confirmation string supplied by the user and an expiry
%% period in seconds. Elapsed expiry time is Now - the token issue time.
%% @end
verify_token([Token = #email_token{}], TokenID, Period, Now) ->
    verify_token(Token, TokenID, Period, Now);
verify_token(Token = #email_token{token=TokenID}, TokenID, Period, Now) ->
    case expired(Token, Period, Now) of
        true -> {error, {expired, Token}};
        false -> {ok, Token}
    end;
verify_token(_, _, _, _) -> {error, wrong_token}.

%% @spec expired(token(), Period::integer()) -> bool()
%% @doc True if the token has expired (the time between the token
%% issue time and now() is greater than Period seconds).
%% @end
expired(T, Period) ->
    expired(T, Period, calendar:universal_time()).

%% @spec expired(token(), Period::integer(), Now::date()) -> bool()
%% @doc True if the token has expired (the time between the token
%% issue time and Now is greater than Period seconds).
%% @end
expired(#email_token{issued=I}, Period, Now) ->
    case (calendar:datetime_to_gregorian_seconds(Now) -
          calendar:datetime_to_gregorian_seconds(I)) of
        N when N >= 0, N =< Period ->
            false;
        N when N > Period ->
            true;
        N when N < 0 ->
            erlang:error(token_from_future)
    end.

%% @spec data(token()) -> term()
%% @doc Extracts the data from the opaque token
%% @end
data(#email_token{data=K}) -> K.
%% @spec token(token()) -> string()
%% @doc Extracts the token id token from the token.
%% @end
token(#email_token{token=V}) -> V.

storage_init(mnesia) ->
    mnesia:create_table(email_token,
                        [{attributes, record_info(fields, email_token)},
                         {type, set},
                         {disc_copies, [node()]}]).

store(mnesia, E = #email_token{}) ->
    mnesia:write(E).
delete(mnesia, E = #email_token{}) ->
    mnesia:delete({email_token, token(E)}).

consume(mnesia, {TokenID, Period}) ->
    case verify_token(mnesia:read({email_token, TokenID}), TokenID, Period) of
        {ok, Token} ->
            delete(mnesia, Token),
            {ok, data(Token)};
        {error, {expired, Token}} ->
            delete(mnesia, Token),
            {error, expired};
        {error, wrong_token} -> {error, never_issued}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec token_id() -> string()
%% @doc Creates a new unique token id - ascii hex of sha1({node,self,ref,now}).
%% @end
token_id() ->
    ej_tokens:id().
    
%% @spec field_validation(token) -> list()
%% @doc returns the ejango.form_validator validation predicates for
%%      token values.
%% @end
field_validation(token) ->
    [{regex, "^[a-f0-9]{40}\$"}].

%%====================================================================
%% Unit tests
%%====================================================================

% @private
create_token_test() ->
    T = create_token("data"),
    ?assertMatch(#email_token{}, T),
    ?assertMatch("data", data(T)).

% @private
expired_test() ->
    T = create_token("data", {{2008,8,12},{9,22,21}}),
    ?assert(expired(T, 10, {{2008,8,12},{9,22,22}}) =:= false),
    ?assert(expired(T, 10, {{2008,8,12},{9,22,32}}) =:= true),
    ?assert(expired(T, 10, {{2008,8,12},{9,22,21}}) =:= false).

% @private
verify_token_test() ->
    T = create_token("data", {{2008,8,12},{9,22,21}}),
    ?assertMatch({ok, #email_token{}}, verify_token(T, token(T), 10, {{2008,8,12},{9,22,22}})),
    ?assertMatch({error, {expired, T}}, verify_token(T, token(T), 10, {{2008,8,12},{9,22,32}})),
    ?assertMatch({error, wrong_token}, verify_token(T, "notdata", 10)).
