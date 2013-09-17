-module(test_utils).

-export([
    index_of/2,
    format_unique_str/1,
    random_character/1,
    unique_string/0,
    unique_url_with_scheme/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_character(Set) ->
    Len = length(Set),
    lists:nth(random:uniform(Len), Set).

unique_string() -> 
    Length = 8,
    [
        random_character(
          lists:seq($a, $z) ++
          lists:seq($A, $Z) ++
          lists:seq($0, $9) ++
          "-."
        )
        ||
        _ <- lists:seq(1, Length)
    ].

unique_url_with_scheme(Scheme) ->
    Scheme ++ "://" ++ unique_string() ++ "." ++ unique_string().

index_of(E, L) ->
    IdxList = lists:zip(lists:seq(1, length(L)), L),
    case lists:keyfind(E, 2, IdxList) of
        {Idx, _} -> Idx;
        false -> not_found
    end.

format_unique_str(Format) ->
  Fun = fun
    ($*) -> test_utils:unique_string();
    (C) -> C
  end,
  lists:flatten([Fun(C) || C <- Format]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unique_string_test_() ->
    [
        fun() ->
            ?assert(string:len(unique_string()) > 0)
        end,

        fun() ->
            S1 = unique_string(),
            S2 = unique_string(),
            ?assert(S1 =/= S2)
        end
    ].

index_of_test_() ->
    [
        fun() ->
            Result = index_of(unique_string(), []),
            ?assertEqual(not_found, Result)
        end,

        fun() ->
            Result = index_of(1, [1, 2, 3]),
            ?assertEqual(1, Result)
        end,

        fun() ->
            Result = index_of(2, [1, 2, 3]),
            ?assertEqual(2, Result)
        end,

        fun() ->
            Result = index_of(2, [1, 2, 2, 3]),
            ?assertEqual(2, Result)
        end
    ].

format_unique_str_test_() ->
  [
    fun() ->
        Result = format_unique_str(""),
        ?assertEqual("", Result)
    end,
    fun() ->
        Str = test_utils:unique_string(),
        Result = format_unique_str(Str),
        ?assertEqual(Str, Result)
    end,
    fun() ->
        Result = format_unique_str("*"),
        ?assert(length(Result) > 0),
        ?assertNotEqual("*", Result)
    end,
    fun() ->
        Prefix = test_utils:unique_string(),
        Suffix = test_utils:unique_string(),
        Result = format_unique_str(Prefix ++ "*" ++ Suffix),
        ?assert(lists:prefix(Prefix, Result)),
        ?assert(lists:suffix(Suffix, Result))
    end,
    fun() ->
        Prefix = test_utils:unique_string(),
        Suffix = test_utils:unique_string(),
        Infix  = test_utils:unique_string(),
        FormatString = Prefix ++ "*" ++ Infix ++ "*" ++ Suffix,
        Result = format_unique_str(FormatString),
        ?assert(length(Result) > length(FormatString)),
        ?assertEqual(0, string:chr(Result, $*))
    end
  ].

-endif.
