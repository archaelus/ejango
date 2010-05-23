%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc EUnit tests for HTML Form validation functions
%% @end
%%%-------------------------------------------------------------------
-module(ej_form_validator_tests).

-include_lib("eunit/include/eunit.hrl").

-import(ej_form_validator, [is_valid/1, validate/2, valid_fields/2
                            ,rule_fields/1]).

not_empty_test() ->
    ?assertMatch(false, 
                 is_valid(validate([{"txtusername", [not_empty]}],
                                   [{"txtusername", []}]
                                  ))),
    ?assertMatch(true, 
                 is_valid(validate([{"txtusername", [not_empty]}],
                                   [{"txtusername", "foobar"}]
                                  ))).

string_test() ->
    ?assertMatch(false, 
                 is_valid(validate([{"txtusername", [not_empty,string]}],
                                   [{"txtusername", []}]
                                  ))),
    ?assertMatch(true, 
                 is_valid(validate([{"txtusername", [not_empty,string]}],
                                   [{"txtusername", "foobar"}]
                                  ))).

length_test() ->
    ?assertMatch(true, 
                 is_valid(validate([{"txtusername", [{length, [3,8]}]}],
                                   [{"txtusername", "foobar"}]
                                  ))),
    ?assertMatch(false, 
                 is_valid(validate([{"txtusername", [{length, [3,5]}]}],
                                   [{"txtusername", "foobar"}]
                                  ))),
    ?assertMatch(false, 
                 is_valid(validate([{"txtusername", [{length, [7,9]}]}],
                                   [{"txtusername", "foobar"}]
                                  ))),
    ?assertMatch(true, 
                 is_valid(validate([{"txtusername", [{length, [6]}]}],
                                   [{"txtusername", "foobar"}]
                                  ))).


duplication_test() ->
    ?assertMatch(true, 
                 is_valid(validate([{"passwords", [{duplication, ["p1", "p2"]}]}],
                                   [{"p1", "foobar"},
                                    {"p2", "foobar"}]
                                  ))),
    ?assertMatch(false, 
                 is_valid(validate([{"passwords", [{duplication, ["p1", "p2"]}]}],
                                   [{"p1", "foobar"},
                                    {"p2", "foobar2"}]
                                  ))),
    ?assertMatch(false, 
                 is_valid(validate([{"passwords", [{duplication, ["p1", "p2"]}]}],
                                   [{"p1", "foobar"}]
                                  ))),
    ?assertMatch(false, 
                 is_valid(validate([{"passwords", [{duplication, ["p1", "p2"]}]}],
                                   [{"p2", "foobar"}]
                                  ))).

predicate_test() ->
    ?assertMatch(true,
                 is_valid(validate([{"p", [{predicate, fun (_) -> true end}]}],
                                   [{"p", "foobar"}]
                                  ))),
    ?assertMatch(false,
                 is_valid(validate([{"p", [{predicate, fun (_) -> false end}]}],
                                   [{"p", "foobar"}]
                                  ))).
not_predicate_test() ->
    ?assertMatch(false,
                 is_valid(validate([{"p", [{not_predicate, fun (_) -> true end}]}],
                                   [{"p", "foobar"}]
                                  ))),
    ?assertMatch(true,
                 is_valid(validate([{"p", [{not_predicate, fun (_) -> false end}]}],
                                   [{"p", "foobar"}]
                                  ))).

regex_test() ->
    ?assertMatch(true,
                 is_valid(validate([{"p", [{regex, "^[abfro]+$"}]}],
                                   [{"p", "foobar"}]
                                  ))),
    ?assertMatch(false,
                 is_valid(validate([{"p", [{regex, "^[abfr]+$"}]}],
                                   [{"p", "foobar"}]
                                  ))).

member_test() ->
    ?assertMatch(true,
                 is_valid(validate([{"p", [{member, ["foobar"]}]}],
                                   [{"p", "foobar"}]
                                  ))),
    ?assertMatch(false,
                 is_valid(validate([{"p", [{member, []}]}],
                                   [{"p", "foobar"}]
                                  ))).


signup_validation_test() ->
    Post = [{"txtusername",[]},
            {"txtemailaddress",[]},
            {"txtpassword",[]},
            {"txtpasswordc",[]},
            {"Signup","Signup"}],
    Validation = [{"txtusername", [string, not_empty, {length, [3,30]},
                                   {predicate, fun (C) when $a =< C, C =< $z;
                                                            $A =< C, C =< $Z;
                                                            $0 =< C, C =< $9 -> true;
                                                   (_)-> false end}]},
                  {"txtemailaddress", [email_address, not_empty]},
                  {"txtpassword", [password, {length, [8, infinity]}]},
                  {"passwords", [{duplication, ["txtpassword", "txtpasswordc"]}]}
                 ],
    ?assertMatch(false,
                 is_valid(validate(Validation, Post))).

valid_fields_test() ->
    Data = [{"txtusername","Foobar"},
            {"txtemailaddress",
             "foo@bar.com"}],
    VResult = [{"txtusername", []},
               {"missingfield", []}],
    Valid = valid_fields(VResult, Data),
    ?assert(lists:member([], Valid) =:= false),
    ?assertMatch([{"txtusername", "Foobar"}], Valid).
    
rule_fields_test() ->
    ?assertMatch([a, b, c],
                 rule_fields({a, [not_empty,
                                  string,
                                  {duplication, [b, c]}]})).
