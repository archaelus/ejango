%% @author Justin Sheehy <justin@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @copyright 2007-2008 Basho Technologies
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc New webmachine style decision library.

-module(ejango.wm_dispatch).
-author('Justin Sheehy <justin@basho.com>').
-author('Andy Gross <andy@basho.com>').
-author('Geoff Cant <nem@erlang.geek.nz>').
-export([handle_request/3, cb/1]).

-import(sets).
-import(dict).
-import(lists).

-record(state, {req,cb,rp,mod_state}).

%#dispatch(Req, CB = #cb{}) ->
%   decision(v3b13, #state{cb=CB,req=Req}).

respond(Code, Headers) ->
    wrcall({add_response_headers, Headers}),
    respond(Code).

error_response(Code, Reason) ->
    ErrorHandler = webmachine_dispatcher:get_error_handler(),
    ErrorHTML = ErrorHandler:render_error(Code, get(req), Reason),
    wrcall({append_to_response_body, ErrorHTML}),
    respond(Code).
error_response(Reason) ->
    error_response(500, Reason).

decision_test(Test,TestVal,TrueFlow,FalseFlow) ->
    case Test of
	{error, Reason} -> error_response(Reason);
	{halt, Code} -> respond(Code);
	TestVal -> decision_flow(TrueFlow, Test);
	_ -> decision_flow(FalseFlow, Test)
    end.
	    
decision_flow(X, TestResult) when is_integer(X) ->
    if X >= 500 -> error_response(X, TestResult);
       true -> respond(X)
    end;
decision_flow(X, _TestResult) when is_atom(X) -> decision(X);
decision_flow({ErrCode, Reason}, _TestResult) when is_integer(ErrCode) ->
    error_response(ErrCode, Reason).

resource_call(Fun) ->
    webmachine_resource:do(Fun, get()).

resource_call(Fun, S = #state{rp=RP,mod_state=MS,cb=CB}) ->
    case dict:fetch(Fun, CB) of
        Fun when is_function(Fun, 2) ->
            {Result, NewMS} = Fun(RP, MS),
            {Result, S#state{mod_state=NewMS}};
        Value ->
            {Value, S}
    end.

wrcall(_) -> erlang:error(not_implemented).
respond(_) -> erlang:error(not_implemented).
method() -> erlang:error(not_implemented).
get_header_val(_) -> erlang:error(not_implemented).
handle_request(_,_,_) -> erlang:error(not_implemented).

%% "Service Available"
decision(v3b13) ->
    decision_test(resource_call(ping), pong, v3b13b, 503);
decision(v3b13b) ->
    decision_test(resource_call(service_available), true, v3b12, 503);
%% "Known method?"
decision(v3b12) ->
    decision_test(lists:member(method(), resource_call(known_methods)),
		  true, v3b11, 501);
%% "URI too long?"
decision(v3b11) ->
    decision_test(resource_call(uri_too_long), true, 414, v3b10);
%% "Method allowed?"
decision(v3b10) ->
    Methods = resource_call(allowed_methods),
    case lists:member(method(), Methods) of
	true ->
	    decision(v3b9);
	false ->
	    wrcall({add_response_headers, [{"Allow",
		     string:join([atom_to_list(M) || M <- Methods], ", ")}]}),
	    respond(405)
    end;
%% "Malformed?"
decision(v3b9) ->
    decision_test(resource_call(malformed_request), true, 400, v3b8);
%% "Authorized?"
decision(v3b8) ->
    case resource_call(is_authorized) of
	true -> decision(v3b7);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code}  ->
	    respond(Code);
        AuthHead ->
            wrcall({add_response_header, "WWW-Authenticate", AuthHead}),
            respond(401)
    end;
%% "Forbidden?"
decision(v3b7) ->
    decision_test(resource_call(forbidden), true, 403, v3b6);
%% "Okay Content-* Headers?"
decision(v3b6) ->
    decision_test(resource_call(valid_content_headers), true, v3b5, 501);
%% "Known Content-Type?"
decision(v3b5) ->
    decision_test(resource_call(known_content_type), true, v3b4, 415);
%% "Req Entity Too Large?"
decision(v3b4) ->
    decision_test(resource_call(valid_entity_length), true, v3b3, 413);
%% "OPTIONS?"
decision(v3b3) ->
    case method() of 
	'OPTIONS' ->
	    Hdrs = resource_call(options),
	    respond(200, Hdrs);
	_ ->
	    decision(v3c3)
    end;
%% Accept exists?
decision(v3c3) ->
    PTypes = [Type || {Type,_Fun} <- resource_call(content_types_provided)],
    case length(PTypes) of
	1 -> nop;
	0 -> error_response("No content types available.");
	_ -> wrcall({add_response_header, "Vary", "Accept"})
    end,
    case get_header_val("accept") of
	undefined ->
	    wrcall({add_response_header, "Content-Type", hd(PTypes)}),
	    wrcall({set_metadata, 'content-type', hd(PTypes)}),
	    decision(v3d4);
	_ ->
	    decision(v3c4)
    end;
%% Acceptable media type available?
decision(v3c4) ->
    PTypes = [Type || {Type,_Fun} <- resource_call(content_types_provided)],
    AcceptHdr = get_header_val("accept"),
    case webmachine_util:choose_media_type(PTypes, AcceptHdr) of
	none ->
	    respond(406);
	MType ->
	    wrcall({add_response_header, "Content-Type", MType}),
	    wrcall({set_metadata, 'content-type', MType}),
	    decision(v3d4)
    end;
%% Accept-Language exists?
decision(v3d4) ->
    decision_test(get_header_val("accept-language"),
		  undefined, v3e5, v3d5);
%% Acceptable Language available? %% WMACH-46 (do this as proper conneg)
decision(v3d5) ->
    decision_test(resource_call(language_available), true, v3e5, 406);
%% Accept-Charset exists?
decision(v3e5) ->
    decision_test(get_header_val("accept-charset"),
		  undefined, v3f6, v3e6);
%% Acceptable Charset available? %% WMACH-46 (do this as proper conneg)
decision(v3e6) ->
    decision_test(resource_call(charset_available), true, v3f6, 406);
%% Accept-Encoding exists?
decision(v3f6) ->
    decision_test(get_header_val("accept-encoding"),
		  undefined, v3g7, v3f7);
%% Acceptable encoding available? %% WMACH-46 (do this as proper conneg)
decision(v3f7) ->
    decision_test(resource_call(encoding_available), true, v3g7, 406);
%% "Resource exists?"
decision(v3g7) ->
    decision_test(resource_call(resource_exists), true, v3g8, v3h7);
%% "If-Match exists?"
decision(v3g8) ->
    decision_test(get_header_val("if-match"), undefined, v3h10, v3g9);
%% "If-Match: * exists"
decision(v3g9) ->
    decision_test(get_header_val("if-match"), "*", v3h10, v3g11);
%% "ETag in If-Match"
decision(v3g11) ->
    ReqETag = mochiweb_util:unquote_header(get_header_val("if-match")),
    decision_test(resource_call(generate_etag), ReqETag, v3h10, 412);
%% "If-Match: * exists"
decision(v3h7) ->
    decision_test(get_header_val("if-match"), "*", 412, v3i7);
%% "If-unmodified-since exists?"
decision(v3h10) ->
    decision_test(get_header_val("if-unmodified-since"),undefined,v3i12,v3h11);
%% "I-UM-S is valid date?"
decision(v3h11) ->
    IUMSDate = get_header_val("if-unmodified-since"),
    decision_test(webmachine_util:convert_request_date(IUMSDate),
		  bad_date, v3i12, v3h12);
%% "Last-Modified > I-UM-S?"
decision(v3h12) ->
    ReqDate = get_header_val("if-unmodified-since"),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    ResErlDate = resource_call(last_modified),
    decision_test(ResErlDate > ReqErlDate,
		  true, 412, v3i12);
%% "Moved permanently? (apply PUT to different URI)"
decision(v3i4) ->
    case resource_call(moved_permanently) of
	{true, MovedURI} ->
	    wrcall({add_response_header, "Location", MovedURI}),
	    respond(301);
	false ->
	    decision(v3p3);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% PUT?
decision(v3i7) ->
    decision_test(method(), 'PUT', v3i4, v3k7);
%% "If-none-match exists?"
decision(v3i12) ->
    decision_test(get_header_val("if-none-match"), undefined, v3l13, v3i13);
%% "If-None-Match: * exists?"
decision(v3i13) ->
    decision_test(get_header_val("if-none-match"), "*", v3j18, v3k13);
%% GET or HEAD?
decision(v3j18) ->
    decision_test(lists:member(method(),['GET','HEAD']),
		  true, 304, 412);
%% "Moved permanently?"
decision(v3k5) ->
    case resource_call(moved_permanently) of
	{true, MovedURI} ->
	    wrcall({add_response_header, "Location", MovedURI}),
	    respond(301);
	false ->
	    decision(v3l5);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% "Previously existed?"
decision(v3k7) ->
    decision_test(resource_call(previously_existed), true, v3k5, v3l7);
%% "Etag in if-none-match?"
decision(v3k13) ->
    ReqETag = mochiweb_util:unquote_header(get_header_val("if-none-match")),
    decision_test(resource_call(generate_etag), ReqETag, v3j18, v3l13);
%% "Moved temporarily?"
decision(v3l5) ->
    case resource_call(moved_temporarily) of
	{true, MovedURI} ->
	    wrcall({add_response_header, "Location", MovedURI}),
	    respond(307);
	false ->
	    decision(v3m5);
	{error, Reason} ->
	    error_response(Reason);
	{halt, Code} ->
	    respond(Code)
    end;
%% "POST?"
decision(v3l7) ->
    decision_test(method(), 'POST', v3m7, 404);
%% "IMS exists?"
decision(v3l13) ->
    decision_test(get_header_val("if-modified-since"), undefined, v3m16, v3l14);
%% "IMS is valid date?"
decision(v3l14) -> 
    IMSDate = get_header_val("if-modified-since"),
    decision_test(webmachine_util:convert_request_date(IMSDate),
		  bad_date, v3m16, v3l15);
%% "IMS > Now?"
decision(v3l15) ->
    NowDateTime = calendar:universal_time(),
    ReqDate = get_header_val("if-modified-since"),
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    decision_test(ReqErlDate > NowDateTime,
		  true, v3m16, v3l17);
%% "Last-Modified > IMS?"
decision(v3l17) ->
    ReqDate = get_header_val("if-modified-since"),    
    ReqErlDate = webmachine_util:convert_request_date(ReqDate),
    ResErlDate = resource_call(last_modified),
    decision_test(ResErlDate > ReqErlDate,
                  true, v3m16, 304);
%% "POST?"
decision(v3m5) ->
    decision_test(method(), 'POST', v3n5, 410);
%% "Server allows POST to missing resource?"
decision(v3m7) ->
    decision_test(resource_call(allow_missing_post), true, v3n11, 404);
%% "DELETE?"
decision(v3m16) ->
    decision_test(method(), 'DELETE', v3m20, v3n16);
%% DELETE enacted immediately?
%% Also where DELETE is forced.
decision(v3m20) ->
    decision_test(resource_call(delete_resource), true, v3m20b, 500);
decision(v3m20b) ->
    decision_test(resource_call(delete_completed), true, v3o20, 202);
%% "Server allows POST to missing resource?"
decision(v3n5) ->
    decision_test(resource_call(allow_missing_post), true, v3n11, 410);
%% "Redirect?"
decision(v3n11) ->
    case resource_call(post_is_create) of
        true ->
            case resource_call(create_path) of
                undefined -> respond(500);
                NewPath ->
                    put(path, NewPath),
                    accept_helper()
            end;
        _ ->
            case resource_call(process_post) of
                true -> nop;
                _ -> respond(500)
            end
    end,
    case wrcall({get_metadata, 'do_redirect'}) of
	true ->
            case wrcall({get_out_header, "Location"}) of
                undefined ->
                    respond(500, "Response had do_redirect but no Location");
                _ ->
                    respond(303)
            end;
	_ ->
	    decision(v3p11)
    end;
%% "POST?"
decision(v3n16) ->
    decision_test(method(), 'POST', v3n11, v3o16);
%% Conflict?
decision(v3o14) ->
    case resource_call(is_conflict) of
        true -> respond(409);
        _ -> accept_helper()
    end,
    decision(v3p11);
%% "PUT?"
decision(v3o16) ->
    decision_test(method(), 'PUT', v3o14, v3o18);
%% Multiple representations?
% (also where body generation for GET and HEAD is done)
decision(v3o18) ->    
    BuildBody = case method() of
        'GET' -> true;
        'HEAD' -> true;
        _ -> false
    end,
    case BuildBody of
        true ->
            case resource_call(generate_etag) of
                undefined -> nop;
                ETag -> wrcall({add_response_header, "ETag", ETag})
            end,
            CT = wrcall({get_metadata, 'content-type'}),
            wrcall({add_response_header, "Content-Type", CT}),
            case resource_call(last_modified) of
                undefined -> nop;
                LM ->
                    wrcall({add_response_header, "Last-Modified",
                           httpd_util:rfc1123_date(calendar:universal_time_to_local_time(LM))})
            end,
            case resource_call(expires) of
                undefined -> nop;
                Exp ->
                    wrcall({add_response_header, "Expires",
                           httpd_util:rfc1123_date(calendar:universal_time_to_local_time(Exp))})
            end,
            F = hd([Fun || {Type,Fun} <- resource_call(content_types_provided),
                           CT =:= Type]),
            case webmachine_resource:do(F, get()) of
                error ->
                    error_response("Failure in ~p~n.", [F]);
                Body ->
                    wrcall({append_to_response_body, Body})
            end;
        false -> nop
    end,
    decision(v3o18b);
decision(v3o18b) ->
    decision_test(resource_call(multiple_choices), true, 300, 200);
%% Response includes an entity?
decision(v3o20) ->
    decision_test(wrcall(has_response_body), true, v3o18, 204);
%% Conflict?
decision(v3p3) ->
    case resource_call(is_conflict) of
        true -> respond(409);
        _ -> accept_helper()
    end,
    decision(v3p11);
%% New resource?  (at this point boils down to "has location header")
decision(v3p11) ->
    case wrcall({get_out_header, "Location"}) of
        undefined -> decision(v3o20);
        _ -> respond(201)
    end.

accept_helper() ->
    CT = get_header_val("Content-Type"),
    F = hd([Fun || {Type,Fun} <-
            resource_call(content_types_accepted), CT =:= Type]),
    case webmachine_resource:do(F, get()) of
        true -> nop;
        _ -> respond(500)
    end.


cb(Module) ->
    Exports = sets:from_list([Call || {Call,2} <- Module:module_info(exports)]),
    Calls = dict:from_list(defaults()),
    CSet = sets:from_list([Call || {Call, _} <- defaults()]),
    Implemented = sets:intersection([Exports, CSet]),
    ImplFuns = [{Call, fun (ReqProps, ModState) -> Module:Call(ReqProps,ModState) end}
                || Call <- sets:to_list(Implemented)],
    D = dict:from_list(ImplFuns),
    dict:merge(fun (_Key, _V1, Fun) -> Fun end, Calls, D).

defaults() ->
    [{ping, no_default}
     ,{service_available, true}
     ,{resource_exists, true}
     ,{auth_required, true}
     ,{is_authorized, true}
     ,{forbidden, false}
     ,{allow_missing_post, false}
     ,{malformed_request, false}
     ,{uri_too_long, false}
     ,{known_content_type, true}
     ,{valid_content_headers, true}
     ,{valid_entity_length, true}
     ,{options, []}
     ,{allowed_methods, ['GET', 'HEAD']}
     ,{known_methods, ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'TRACE', 'CONNECT', 'OPTIONS']}
     ,{content_types_provided, [{"text/html", to_html}]}
     ,{content_types_accepted, []}
     ,{delete_resource, false}
     ,{delete_completed, true}
     ,{post_is_create, false}
     ,{create_path, undefined}
     ,{process_post, false}
     ,{language_available, true}
     ,{charset_available, true}
     ,{encoding_available, true}
     ,{is_conflict, false}
     ,{multiple_choices, false}
     ,{previously_existed, false}
     ,{moved_permanently, false}
     ,{moved_temporarily, false}
     ,{last_modified, undefined}
     ,{expires, undefined}
     ,{generate_etag, undefined}
     ,{finish_request, true}].
