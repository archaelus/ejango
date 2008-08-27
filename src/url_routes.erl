%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc URL Routing
%% @end
%%%-------------------------------------------------------------------
-module(ejango.url_routes).

-import(eunit).
-include_lib("eunit.hrl").

-import(lists).
-import(string).

%% API
-export([route/3
         ,generate/4]).

%% @type routes() = [route()].
%% A list of routes.
%% @type route() = {Method, [path_spec()], controller()}
%%       Method = any | atom().
%% A single route specification.
%%
%% Method can be any to match any HTTP method, or the atom of the
%% exact HTTP method (e.g. 'GET').
%% @type path_spec() = {exact, path_components()} | path_components().
%% A path specification - either an exact list of path components or a prefix list.
%%
%% An exact path matches only if RoutePath =:= RequestPath.
%% A prefix path matches for lists:prefix(RoutePath, RequestPath) =:= true.
%%
%% The leading "/" component isn't included in the route path specification.
%% @type controller() = Module::atom() | {page, Module::atom()}.
%% A controller module that will respond to either Module:render/0 for
%% {page, Module} or Module:serve_path(Method::atom(),
%% Path::[string()], Request::mochiweb_request()) for the plain
%% Module::atom form.
%% @type path_components() = [string()].
%% A list of URL path components (without "/" characters).
%%
%% string:tokens(Path, "/") ==> path().

%%====================================================================
%% API
%%====================================================================

%% @private
route(Method, Path) ->
    route(Method, tl(Path), test_routes()).

%% @spec route(Method::atom(), path_components(), routes()) -> Result
%%  Result = fail | not_found | controller()
route(Method, Path, [{RMethod, Paths, Controller}|Rest])
  when RMethod =:= any; RMethod =:= Method ->
    route_path(Method, Path, Paths, Controller, Rest);
route(Method, Path, [_Route|Rest]) ->
    route(Method, Path, Rest);
route(Method, _Path, []) when Method =:= 'GET'; Method =:= 'HEAD' ->
    not_found;
route(_Method, _Path, []) ->
    fail.

route_path(Method, Path, [], _Controller, Rest) ->
    route(Method, Path, Rest);
route_path(_Method, Path, [{exact, Path}|_Paths], Controller, _Rest) ->
    Controller;
route_path(Method, Path, [{exact, _OtherPath}|Paths], Controller, Rest) ->
    route_path(Method, Path, Paths, Controller, Rest);
route_path(Method, Path, [RPath|Paths], Controller, Rest) ->
    case lists:prefix(RPath, Path) of
        true -> Controller;
        false -> route_path(Method, Path, Paths, Controller, Rest)
    end.

%% @private
%% @spec generate(controller(), Extra_Path::path_components()) -> URL::string()
generate(Controller, Components) ->
    generate(test_url(), Controller, Components, test_routes()).
%% @spec generate(BaseURL::string(), controller(), path_components(), routes()) -> URL::string()
generate(BaseUrl, Controller, Components, Routes) ->
    case lists:keysearch(Controller, 3, Routes) of
        {value, {_, Paths, _Controler}} ->
            C = case hd(Paths) of
                    [] -> [BaseUrl | Components];
                    {exact, []} -> [BaseUrl | Components];
                    {exact, Path} -> [BaseUrl, Path | Components];
                    Path -> [BaseUrl | Path] ++ Components
                end,
            string:join(C, "/");
        false ->
            erlang:error({no_route, Controller})
    end.

%%====================================================================
%% Internal functions
%%====================================================================

route_test() ->
    ?assertMatch({page, vhreg_welcome},
                 route('GET', ["/", "welcome"])),
    ?assertMatch({page, vhreg_welcome},
                 route('GET', ["/", "welcome", "foo"])),
    ?assertMatch(vhreg_web_account,
                 route('GET', ["/", "account", "new"])),
    ?assertMatch(not_found,
                 route('GET', ["/", "fooz"])),
    ?assertMatch(fail,
                 route('POST', ["/", "fooz"])).

generate_test() ->
    ?assertMatch("http://bete.ran:8000/foo",
                 generate({page, vhreg_welcome}, ["foo"])),
    ?assertMatch("http://bete.ran:8000/account/confirm/foo/bar",
                 generate(vhreg_web_confirm, ["foo", "bar"])).

test_routes() ->
    [{any, [["get_started"],
            ["account", "new"]], vhreg_web_account},
     {any, [["account", "confirm"]], vhreg_web_confirm},
     {any, [["get_setup"]], vhreg_web_setup},
     {any, [["login"]], vhreg_web_login},
     {any, [{exact, []}, ["welcome"]], {page, vhreg_welcome}}
    ].

test_url() ->
    "http://bete.ran:8000".
