%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URL Routing
%% @end
%%%-------------------------------------------------------------------
-module(ejango.url_routes).

-import(eunit).
-include_lib("eunit/include/eunit.hrl").

-import(lists).
-import(string).
-import(proplists).

%% API
-export([route/2
         ,generate/3
         ,generate/4]).

%% @type routes() = [route(),...].
%% A list of routes.
%% @type route() = {[match_spec(),...], controller(), Options::proplist()}.
%% A single route specification.
%%
%% @type controller() = Module::atom().
%% A controller module name.
%%
%% @type match_spec() = [match_term(),...].
%% A match specification - a list of match terms to compare against
%% URL path components.
%%
%% @type match_term() = string() | '*' | 'root' | atom() | {'*', Var::term}.
%% A term to match against URL path components.
%%
%% Strings must match exactly in the same position. Atoms consume one
%% URL component and produce a variable binding of {Atom,
%% Component}. '*' and {'*', Var} consumes the remaining path (zero or
%% more path components) and can only appear as the last match_term()
%% in a match_spec(). {'*', Var} produces a variable binding of {Var,
%% RemainingPathComponents}. The atom 'root' will only match the path ""
%% and does not produce a binding.
%%
%% @type path() = [string(),...].
%% A list of URL path components. path() == string:tokens(PathString, "/").

%%====================================================================
%% API
%%====================================================================

%% @spec route(path_components(), routes()) -> Result
%%  Result = fail | not_found | controller()

route(_Path, []) ->
    not_found;
route(Path, [Route | Rest]) ->
    case match(Path, Route) of
        not_found -> route(Path, Rest);
        Else -> Else
    end.

match(_Path, []) ->
    not_found;
match(_Path, {[], _Mod, _Options}) ->
    not_found;
match([], {[root | _], Mod, Options}) ->
    {Mod, Options, []};
match(Path, {[root | Rest], Mod, Options}) ->
    match(Path, {Rest, Mod, Options});
match(Path, {[Pattern|Rest], Mod, Options}) ->
    case match_pattern(Path, Pattern, []) of
        {match, Vars} ->
            {Mod, Options, Vars};
        not_found ->
            match(Path, {Rest, Mod, Options})
    end.

match_pattern([], [], Vars) ->
    {match, Vars};
match_pattern([String | Rest], [String | Pattern], Vars) when is_list(String) ->
    match_pattern(Rest, Pattern, Vars);
match_pattern(_Path, ['*'], Vars) ->
    {match, Vars};
match_pattern(Path, [{'*', Var}], Vars) ->
    {match, [{Var,Path} | Vars]};
match_pattern([String | Rest], [Var | Pattern], Vars) when is_atom(Var) ->
    match_pattern(Rest, Pattern, [{Var, String} | Vars]);
match_pattern([String | Rest], [{String, Var} | Pattern], Vars) when is_list(String) ->
    match_pattern(Rest, Pattern, [{Var, String} | Vars]);

match_pattern(_Path, _Pattern, _Vars) ->
    not_found.

generate(Controller, Vars, Routes) ->
    generate(none, Controller, Vars, Routes).
%% @spec generate(BaseURL::string(), controller(), path_components(), routes()) -> URL::string()
generate(BaseUrl, Controller, Vars, Routes) ->
    case lists:keysearch(Controller, 2, Routes) of
        {value, {MatchSpecs, _Controler, _Options}} when is_list(BaseUrl) ->
            MS = best_match(MatchSpecs, Vars),
            string:join([BaseUrl | generate_path(Vars, MS)], "/");
        {value, {MatchSpecs, _Controler, _Options}} when BaseUrl =:= none ->
            MS = best_match(MatchSpecs, Vars),
            string:join(generate_path(Vars, MS), "/");
        false ->
            erlang:error({no_route, Controller})
    end.

generate_path(_Vars, []) -> [];
generate_path(_Vars, ['*']) -> [];
generate_path(Vars, [{'*', Var}]) -> proplists:get_value(Var, Vars,[]);
generate_path(Vars, [Var|Rest]) when is_atom(Var) ->
    [case proplists:get_value(Var, Vars) of
         undefined -> erlang:error({missing_var, Var});
         Val -> Val
     end | generate_path(Vars, Rest)];
generate_path(Vars, [String|Rest]) when is_list(String) ->
    [String | generate_path(Vars, Rest)].

%%====================================================================
%% Internal functions
%%====================================================================

best_match([], _Vars) -> erlang:error(no_possible_matches);
%% Only one possible choice, choose it even if we don't have enough
%% required variables - variable expansion will turn this into an
%% error.
best_match([MatchSpec], _Vars) -> MatchSpec;
best_match(MatchSpecs, Vars) ->
    [First | Rest] = annotate_matchspecs(MatchSpecs, Vars),    
    {_,MS} = lists:foldl(fun pick_best_match/2, First, Rest),
    MS.

pick_best_match(New = {NewScore, _MS},
                {CurrentScore, _MS2}) when NewScore > CurrentScore ->
    New;
pick_best_match(_, Current) -> Current.

annotate_matchspecs(MatchSpecs, Vars) ->
    %% Get the list of supplied variable keys
    Keys = [Key || {Key,_V} <- Vars],
    NumKeys = length(Keys),
    %% Calculate a score for each mspec,
    %% prefer having all required keys supplied,
    %% then number of required keys supplied,
    %% then number of optional keys supplied.
    lists:map(fun(MS) ->
                      {Req, Opt} = ms_vars(MS),
                      NumReqKeys = NumKeys - length(Keys -- Req),
                      NumOptKeys = NumKeys - length(Keys -- Opt),
                      {{NumReqKeys =:= length(Req),NumReqKeys,NumOptKeys},
                           MS}
              end, MatchSpecs).

%% @doc Find all the required and optional variables for a matchspec.    
ms_vars(MatchSpec) ->
    lists:foldl(fun ms_vars/2,
                {[],[]},
                MatchSpec).

ms_vars('*', Acc) -> Acc;
ms_vars(Atom, {Req, Opt}) when is_atom(Atom) ->
    {[Atom | Req], Opt};
ms_vars({'*', Var}, {Req, Opt}) ->
    {Req, [Var | Opt]};
ms_vars(_Other, Acc) -> Acc.

%%====================================================================
%% Unit Tests
%%====================================================================

best_match_test() ->
    ?assertMatch(["foo", bar, "baz", bobo,{'*',opt}],
                 best_match([ ["foo", bar, "baz", bobo,{'*',opt}],
                              ["foo", "bar"],
                              ["foo", bar, "baz"],
                              ["foo", bar, "baz", bobo],
                              ["zombies", ate, "my", baby]
                             ],
                            [{bar, "bar"},{bobo,"bobo"}, {opt, "opt"}])).

best_match_2_test() ->
    ?assertMatch(["test3", {'*', post_path}],
                 best_match([ ["test"],
                              ["test2", '*'],
                              ["test3", {'*', post_path}] ],
                            [{post_path, "foo"}])).

ms_vars_test() ->
    ?assertMatch({[test],["baz"]},
                 ms_vars(["foo", test, "bar", {'*', "baz"}])),
    ?assertMatch({[test],[]},
                 ms_vars(["foo", test, "bar"])),
    ?assertMatch({[],[]},
                 ms_vars(["foo", "bar"])).


route_1_test() ->
    ?assertMatch({test_web_account, [], []},
                 test_route(["test"])).
generate_1_test() ->
    ?assertMatch("test",
                 test_generate(test_web_account,[])).

route_2_test() ->
    ?assertMatch({test_web_account, [], []},
                 test_route(["test2"])).
route_3_test() ->
    ?assertMatch({test_web_account, [], []},
                 test_route(["test2", "foozbag"])).
route_4_test() ->
    ?assertMatch({test_web_account, [], [{post_path, ["foozbag"]}]},
                 test_route(["test3", "foozbag"])).
route_5_test() ->
    ?assertMatch(not_found,
                 test_route(["foozbag"])).
route_6_test() ->
    ?assertMatch({test_web_confirm, [], [{token, "98475932846572396"}]},
                 test_route(["account", "confirm", "98475932846572396"])).
generate_6_test() ->
    ?assertMatch("account/confirm/98475932846572396",
                 test_generate(test_web_confirm, [{token, "98475932846572396"}])).

route_7_test() ->
    ?assertMatch(not_found,
                 test_route(["account", "confirm", "984759328", "46572396"])).
route_8_test() ->
    ?assertMatch({test_web_setup, [], []},
                 test_route(["get_setup"])).
route_9_test() ->
    ?assertMatch({page, [{name,"welcome"}], []},
                 test_route([])).
route_10_test() ->
    ?assertMatch({page, [{name,"welcome"}], []},
                 test_route(["welcome"])).

generate_11_test() ->
    ?assertMatch("post_path/foo/bar",
                 test_generate(test_post_path, [{post_path, ["foo","bar"]}])).

generate_12_test() ->
    ?assertMatch("http://localhost:8000/test",
                 test_generate("http://localhost:8000",test_web_account,[])).

generate_13_test() ->
    ?assertMatch("http://localhost:8000/test3/foo/bar",
                 test_generate("http://localhost:8000",test_web_account,
                               [{post_path, ["foo","bar"]}])).
    
route_14_test() ->
    ?assertMatch({test_static_var, [], [{suffix, ["test"]},{prefix, "static"}]},
                 test_route(["static", "test"])).

test_route(Path) ->
    route(Path, test_routes()).

test_generate(Controller, Vars) ->
    generate(none, Controller, Vars, test_routes()).

test_generate(Base, Controller, Vars) ->
    generate(Base, Controller, Vars, test_routes()).

test_routes() ->
    [{[["test"],
       ["test2", '*'],
       ["test3", {'*', post_path}]], test_web_account, []},
     {[["account", "confirm", token]], test_web_confirm, []},
     {[["get_setup"]], test_web_setup, []},
     {[["login"]], test_web_login, []},
     {[["post_path", {'*', post_path}]], test_post_path, []},
     {[[{"static", prefix}, {'*', suffix}]], test_static_var, []},
     {[[], ["welcome"]], page, [{name, "welcome"}]}
    ].

test_url() ->
    "http://localhost:8000".
