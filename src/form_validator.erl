%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc HTML Form validation functions
%% @end
%%%-------------------------------------------------------------------
-module(ejango.form_validator).

-import(eunit).
-include_lib("eunit.hrl").

-import(lists).
-import(proplists).
-import(error_logger).
-import(rvre).

%% API
-export([validate/2
         ,is_valid/1
         ,has_error/1
         ,invalid_fields/1
         ,field_errors/2
         ,errors/1
         ,valid_fields/2
         ,validate_rule/2
         ,rule_fields/1]).

%%====================================================================
%% API
%%====================================================================

%% @spec validate(Rules, Data) -> [Result]
%% Rules = [form_validation_rule()]
%% Data = [{Key::string(), Value::string()}]
%% Result = {Key::string(), [predicate_result()]}
validate(Rules,Data) when is_list(Rules), is_list(Data) ->
    lists:map(fun (Rule) -> validate_rule(Rule, Data) end,
              Rules).

invalid_fields(Results) ->
    [Field || {Field, Errors} <- Results,
              length(Errors) >= 1].

is_valid(Results) ->
    invalid_fields(Results) =:= [].

has_error(Results) ->
    is_valid(Results) =:= false.

field_errors(Name, Results) ->
    case lists:keysearch(Name, 1, Results) of
        false -> erlang:error(no_such_field);
        {value, V} -> V
    end.

errors(Results) ->
    [Field || Field = {_, Errors} <- Results,
              length(Errors) >= 1].

valid_fields(Results, Data) ->
    Fields = [ Field || {Field, []} <- Results],
    [ {K,V} || {K,V} <- Data,
               lists:member(K, Fields) ].

validate_rule({Name, Predicates}, Data) when is_list(Name), is_list(Predicates) ->
    {Name,
     lists:flatmap(fun (Predicate) ->
                           case validate_predicate(Predicate, Name, Data) of
                               true -> [];
                               false -> [{error, Predicate, false}];
                               {error, Reason} -> [{error, Predicate, Reason}]
                           end
                  end, normalize_predicates(Predicates))}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec validate_predicate(Predicate, Name, Data) -> true | false | {error, Reason}
%% @doc Checks a single condition in a rule.
%% @end
validate_predicate({duplication, [Field|Duplicates]}, _Name, Data)
  when is_list(Duplicates), length(Duplicates) >= 1 ->
    FieldValue = proplists:get_value(Field, Data),
    same_value(Field, FieldValue, Duplicates, Data);
    
validate_predicate(Predicate, Name, Data) ->
    validate_predicate_simple(Predicate, proplists:get_value(Name, Data)).

%% @private
validate_predicate_simple(not_empty, L) when is_list(L), length(L) > 0 -> true;
validate_predicate_simple(not_empty, _) -> false;
validate_predicate_simple(string, L) when is_list(L) -> true;
validate_predicate_simple(string, _) -> false;
validate_predicate_simple({length, [Exact]}, L) ->
    validate_predicate_simple({length, [Exact, Exact]}, L);
validate_predicate_simple({length, [Min,Max]}, L) when is_list(L) -> 
    case length(L) of
        Len when Min =< Len, Len =< Max -> true;
        _ -> false
    end;
validate_predicate_simple({length, [_Min,_Max]}, _L) -> false;
validate_predicate_simple({predicate, P}, L) -> P(L);
validate_predicate_simple({not_predicate, P}, L) -> P(L) =:= false;
validate_predicate_simple(email_address, L) when is_list(L) ->
    email_address:validate(L);
validate_predicate_simple(email_address, undefined) -> false;
validate_predicate_simple({regex, RE}, L) when is_list(L) ->
    case rvre:match(L, RE) of
        nomatch -> false;
        {match, _} -> true;
        {error, R} -> {error, R}
    end;
validate_predicate_simple({regex, _RE}, _L) ->
    {error, not_a_string};
validate_predicate_simple(P, V) -> erlang:error({not_implemented, {P, V}}).

%% @private
normalize_predicates(Predicates) when is_list(Predicates) ->
    proplists:normalize(Predicates, [{expand, [{password, string}]}]).

%% @private
same_value(_Field, _FieldValue, [], _Data) -> true;
same_value(Field, FieldValue, [Duplicate|Rest], Data) ->
    case proplists:get_value(Duplicate, Data) of
        undefined -> {error, {missing, Field}};
        V when V =:= FieldValue ->
            same_value(Field, FieldValue, Rest, Data);
        _ -> {error, {different_value, Field, Duplicate}}
    end.

rule_fields({Name, Rules}) ->
    lists:usort(lists:append([ predicate_fields(Name, Rule)
                               || Rule <- Rules ])).

%% @private
predicate_fields(_Name, {duplication, List}) ->
    List;
predicate_fields(Name, not_empty) -> [Name];
predicate_fields(Name, string) -> [Name];
predicate_fields(Name, {regex, _}) -> [Name];
predicate_fields(Name, {predicate, _}) -> [Name];
predicate_fields(Name, email_address) -> [Name];
predicate_fields(Name, {length, _}) -> [Name].
