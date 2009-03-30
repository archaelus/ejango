%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Static file compression and serving for mochiweb.
%% @end
%%%-------------------------------------------------------------------
-module(ejango.static).

%% API
-export([compress_docroot/1
         ,serve_compressed/3
        ]).

-define(EXT_GZ, ".gz").
-define(EXT_MIN, ".min").
-define(INFO(Format, Args),
        error_logger:info_msg("(~p ~p:~p) " ++ Format,
                              [self(), ?MODULE, ?LINE | Args])).

-import(mochiweb_util).
-import(filename).
-import(filelib).
-import(zlib).
-import(string).
-import(lists).
-import(error_logger).
-import(calendar).
-import(httpd_util).
-import(file).
-import(os).
-import(code).

%%====================================================================
%% API
%%====================================================================

%% @spec serve_compressed(mochiweb_request(), RelPath::string(), DocRoot::string()) -> any()
%% @doc 
%% @end 
serve_compressed(Req, RelPath, DocRoot) ->
    case mochiweb_util:safe_relative_path(RelPath) of
        undefined ->
            Req:not_found();
        SafeRelPath ->
            serve_compressed2(Req, SafeRelPath, DocRoot)
    end.

serve_compressed2(Req, SafeRelPath, DocRoot) ->
    case have_compressed(DocRoot, SafeRelPath) of
        {regular, Regular} ->
            %%?INFO("Serving regular file ~p.", [Regular]),
            Req:serve_file(Regular, DocRoot, [{"Expires", far_future()}]);
        {gzip, Gzipped, Regular} ->
            case can_gzip(Req) of
                true ->
                    %%?INFO("Serving gzipped file instead ~p", [Gzipped]),
                    Req:serve_file(Gzipped, DocRoot,
                                   [{"Content-Encoding", "gzip"},
                                    {"Content-Type",
                                     mochiweb_util:guess_mime(filename:join([DocRoot, SafeRelPath]))},
                                    {"Expires", far_future()}]);
                false ->
                    %%?INFO("Serving regular file ~p.", [Regular]),
                    Req:serve_file(Regular, DocRoot, [{"Expires", far_future()}])
            end
    end.

have_compressed(DocRoot, SafeRelPath) ->
    GZMinName = SafeRelPath ++ ?EXT_MIN ++ ?EXT_GZ,
    GZName = SafeRelPath ++ ?EXT_GZ,
    MinName = SafeRelPath ++ ?EXT_MIN,
    case {filelib:is_regular(filename:join([DocRoot, GZMinName])),
          filelib:is_regular(filename:join([DocRoot, GZName])),
          filelib:is_regular(filename:join([DocRoot, MinName]))} of
        {true, _, true} ->
            {gzip, GZMinName, MinName};
        {true, _, false} ->
            {gzip, GZMinName, SafeRelPath};
        {false, true, true} ->
            {gzip, GZName, MinName};
        {false, true, false} ->
            {gzip, GZName, SafeRelPath};
        {false, false, true} ->
            {regular, MinName};
        {false, false, false} ->
            {regular, SafeRelPath}
    end.

can_gzip(Req) ->
    Header = case Req:get_header_value("Accept-Encoding") of
                 undefined -> [];
                 L when is_list(L) -> L
             end,
    Encodings = string:tokens(Header, ","),    
    lists:member("gzip", Encodings).

compress_docroot(DocRoot) ->
    filelib:fold_files(DocRoot,
                       ".*", true,
                       fun compress_static_file/2,
                       undefined).

compress_static_file(File, undefined) ->
    %%?INFO("Examining ~p", [File]),
    Files = case mochiweb_util:guess_mime(File) of
                "text/css" -> [yui_compress(File), File];
                "application/x-javascript" -> [yui_compress(File), File];
                _ -> [File]
            end,
    [gzip_file(F) || F <- Files, is_list(F)],
    undefined.

yui_compress(File) ->
    %%?INFO("YUI Compressing ~p", [File]),
    compress_file(File, fun (F, _Bin) ->
                                iolist_to_binary(yui_compress("--nomunge", F))
                        end,
                  ?EXT_MIN, "Minifying").

yui_compress(Args, File) ->
    Cmd = "java -jar " ++
        priv_dir() ++
        "bin/yuicompressor-2.4.2.jar " ++ Args ++ " " ++ File,
    %%?INFO("Running ~p", [Cmd]),
    os:cmd(Cmd).

priv_dir() ->
    RPath = lists:reverse(filename:split(code:which(?MODULE))),
    priv_dir(RPath).

priv_dir(["ebin" | Rest]) ->
    filename:join(lists:reverse(["priv" | Rest]));
priv_dir([_Else | Rest]) ->
    priv_dir(Rest);
priv_dir([]) -> erlang:error({cant_find_privdir, ?MODULE}).

gzip_file(File) ->
    compress_file(File, fun (_File,Bin) -> zlib:gzip(Bin) end,
                  ?EXT_GZ, "Gzipping").

compress_file(File, Compressor, Extension, Verb)
  when is_list(File), is_function(Compressor, 2),
       is_list(Extension), is_list(Verb) ->
    case maybe_compress(File, Extension) of
        {Tag, CName} when Tag =:= compress; Tag =:= recompress ->
            %%?INFO("Trying to compress ~p as ~p", [File, CName]),
            {ok, Bin} = file:read_file(File),
            CBin = Compressor(File, Bin),
            Dir = filename:dirname(File),
            case byte_size(Bin) - byte_size(CBin) of
                Savings when Savings > 0 ->
                    ?INFO("[~s] ~s ~p -> ~p to (saves ~w bytes).",
                          [Dir, Verb, filename:basename(File),
                           filename:basename(CName), Savings]),
                    file:write_file(CName, CBin),
                    CName;
                Increase -> 
                    ?INFO("[~s] Not ~s ~p -> ~p (~w bytes larger)",
                          [Dir, Verb, filename:basename(File),
                           filename:basename(CName), abs(Increase)]),
                    skip
            end;
        _ -> skip
    end.

maybe_compress(File, Extension) ->
    CName = File ++ Extension,
    case {filelib:is_regular(CName), filename:extension(File)} of
        {_, Extension} -> skip;
        {false, _} -> {compress, CName};
        {true, _} -> {recompress, CName};
        _ -> skip
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec far_future() -> RFC1123_date::string()
%% @doc Return a RFC1123 date far enough into the future to make yslow happy.
%% @end
far_future() ->
    Future = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) +
        86400 * 10, %% 10 days in the future
    httpd_util:rfc1123_date(calendar:gregorian_seconds_to_datetime(Future)).
