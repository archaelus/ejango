%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc Form generation, validation and processing library
%% @end
%%%-------------------------------------------------------------------
-module(ejango.form).

-import(eunit).
-include_lib("eunit.hrl").

-import(lists).
-import(proplists).
-import(string).

%% API
-export([create/4,
         render/1,
         render_with_data/2,
         render_with_validation/3,
         render_with_fields/2,
         text/2,
         text/3,
         password/2,
         password/3,
         submit/1,
         hidden/2,
         validate/2,
         validate_field/3,
         valid_fields/3,
         valid_post/2,
         rules/1]).

-record(form, {title, action, fields = [], rules = []}).
%-record(vform, {form, data, validation_result}).
-record(field, {name, type, title, value, rules = []}).

%%====================================================================
%% API
%%====================================================================

create(Title, Action, Fields, Rules) ->
    #form{title=Title,
          action=Action,
          fields=Fields,
          rules=Rules}.

text(Title, Rules) ->
    text(Title, field_name("txt", Title), Rules).

text(Title, Name, Rules) ->
    #field{type=text, title=Title, name=Name, rules=Rules}.

password(Title, Rules) ->
    password(Title, field_name("pw", Title), Rules).

password(Title, Name, Rules) ->
    #field{type=password, title=Title, name=Name, rules=Rules}.

submit(Name) ->
    #field{type=submit, title=Name, name=Name}.

hidden(Name, Rules) ->
    #field{type=hidden, name=Name, rules=Rules}.

render(F = #form{}) ->
    render_form(F, []).

render_with_data(F = #form{}, Data) ->
    Result = validate(F, Data),
    render_with_validation(F, Result, Data).

render_with_validation(F, Result, Data) ->
    ValidFields = valid_fields(F, Result, Data),
    render_with_fields(F, ValidFields).

render_with_fields(F = #form{}, ValidFields) ->
    render_form(F, ValidFields).

validate(Form, Data) ->
    form_validator:validate(rules(Form), Data).

rules(#form{fields=Fields, rules=FormRules}) ->
    [{Name,Rules}
     || #field{type=Type,name=Name,rules=Rules} <- Fields,
        Type =/= submit]
        ++ FormRules.

validate_field(Form, Field, Data) ->
     case proplists:get_value(Field, rules(Form)) of
         undefined -> erlang:error({no_such_rule, Field});
         List when is_list(List) ->
             form_validator:validate_rule({Field, List}, Data)
     end.

valid_fields(F, Result, Data) ->
    Simple = simple_copy(Result, Data),
    % on a {Name, {duplication, ...} rule, put {Name, Field} into
    Complex = [ case {proplists:get_value(Field, Result),
                      proplists:get_value(Field, Simple)} of
                    {[], Value} ->
                        {Rule, Value};
                    _ ->
                        []
                end
                || {Rule, [{duplication, [Field|_Fields]}]} <- form_rules(F),
                   proplists:get_value(Rule, Result) =:= []],
    lists:flatten([Simple, Complex]).

valid_post(F = #form{}, Data) ->
    Result = form:validate(F, Data),
    Fields = valid_fields(F, Result, Data),
    case form_validator:is_valid(Result) of
        true ->
            {valid, Fields};
        false ->
            {invalid, Result, Fields}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

form_rules(#form{rules=R}) -> R.

type_description(text) ->
    "Text field: ";
type_description(password) ->
    "Password field: ".

simple_copy(Results, Data) ->
    lists:flatmap(fun ({Field, []}) ->
                          case proplists:get_value(Field, Data) of
                              undefined -> [];
                              V ->
                                  [{Field, V}]
                          end;
                      (_) -> []
                  end,
                  Results).

render_form(#form{title=Title,
                  action=Action,
                  fields=Fields}, ValidFields) ->
    form_template:render([{form_action, Action},
                          {form_title, Title},
                          {fields, [ T
                                     || {ok, T} <-
                                            lists:map(fun (F) -> render_field(F, ValidFields) end,
                                                      Fields)]}]).

render_field(#field{type=submit, name=Name, title=Title}, _ValidFields) ->
    field_template:render([{type, "submit"},
                           {description, "Submit button: " ++ Name},
                           {name, Name},
                           {value, Title}]);
render_field(#field{type=hidden, name=Name}, ValidFields) ->
    V = case proplists:get_value(Name, ValidFields) of
            undefined -> erlang:error({missing_hidden_field_value, Name});
            Vl -> Vl
        end,
    field_template:render([{type, "hidden"},
                           {description, ""},
                           {name, Name},
                           {value, V}]);
render_field(#field{type=Type, name=Name, title=Title}, ValidFields) ->
    case proplists:get_value(Name, ValidFields) of
        undefined ->
            named_field_template:render([{name, Name},
                                         {title, Title},
                                         {type, atom_to_list(Type)},
                                         {description, type_description(Type)}]);
        Value ->
            named_field_template:render([{name, Name},
                                         {title, Title},
                                         {type, atom_to_list(Type)},
                                         {description, type_description(Type)},
                                         {value, Value}])
    end.

field_name(Prefix, Title) ->
    S = lists:filter(fun (C) when $A =< C, C =< $Z;
                         $a =< C, C =< $z;
                         $0 =< C, C =< $9;
                         C =:= $_;
                         C =:= $.;
                         C =:= $- ->
                             true;
                         (_) -> false
                     end,
                     Title),
    Prefix ++ string:to_lower(S).

field_name_test() ->
    ?assertMatch("txtpassword", field_name("txt", "Password:")),
    ?assertMatch("txtpassword", field_name("txt", "pass word")),
    ?assertMatch("pwpassword", field_name("pw", "Password:")),
    ?assertMatch("pwpassword", field_name("pw", "pass word")).

create_test() ->
    ?assertMatch(#form{},
                 create("Setup Information", "",
                        [text("User Name:", [{length, [3,30]}]),
                         text("Email address:", [email_address]),
                         password("Password:", "txtpassword", [{length, [8,infinity]}]),
                         password("Confirm Password:", "txtpasswordc", []),
                         submit("Signup")],
                        [{"passwords", [{duplication, ["txtpassword", "txtpasswordc"]}]}])).

valid_fields_test() ->
    ?assertMatch([{"valid", foo}],
                 valid_fields(#form{},
                              [{"valid", []}],
                              [{"valid", foo}])),
    ?assertMatch([{"valid", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])),
    ?assertMatch([{"valid", foo},
                  {"Foo", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []},
                               {"Foo", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])),
    ?assertMatch([{"valid", foo},
                  {"other", baz},
                  {"Foo", foo}],
                 valid_fields(#form{rules=[{"Foo", [{duplication, ["valid", "other"]}]}]},
                              [{"valid", []},
                               {"other", []},
                               {"Foo", []}],
                              [{"valid", foo},
                               {"invalid", bar},
                               {"other", baz},
                               {"random", baz}])).

simple_copy_test() ->
    ?assertMatch([{"valid", foo}],
                 simple_copy([{"invalid", [error]},
                              {"valid", []}],
                             [{"invalid", invalid},
                              {"valid", foo}])).
