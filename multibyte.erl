-module(multibyte).
-export([binary_to_multibyte_integer/1, number_to_multibyte_integer/2]).
-compile(export_all).

% Will convert the binary into an integer by bitshifting each byte left.
binary_to_multibyte_integer(Binary) ->
  List = binary_to_list(Binary),
  list_to_multibyte_integer(List, 0).

list_to_multibyte_integer([], Result) ->
  Result;

list_to_multibyte_integer([H|T], Result) ->
  NewResult = (Result bsl 8) + H,
  list_to_multibyte_integer(T, NewResult).


% Will convert a number to a multibyte integer of specified length
number_to_multibyte_integer(Number, NumBytes) ->
    lists:reverse(lists:flatten(number_to_multibyte_integer(Number, NumBytes, []))).

number_to_multibyte_integer(_Number, 0, ByteList) ->
    ByteList;
number_to_multibyte_integer(Number, NumBytes, ByteList) ->
    BitShift = ((NumBytes - 1) * 8),
    Head = Number bsr BitShift,
    Tail = Number - (Head bsl BitShift),
    number_to_multibyte_integer(Tail, NumBytes - 1, [Head|ByteList]).
