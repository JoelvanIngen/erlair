% Usage: escript extract_ast.escript <file.erl> <include_dir>

main([Filename, IncludeDir]) ->
    Options = [{includes, [IncludeDir]}],

    case epp:parse_file(Filename, Options) of
        {ok, Forms} ->
            io:format("~s~n", [to_json(Forms)]);
        {error, Error} ->
            io:format(standard_error, "Error parsing file: ~p~n", [Error]),
            halt(1)
    end.

% Json encoder
% Only recent Erlang versions (OTP 27+) have the json module,
% and I don't want to introduce external dependencies either,
% so here's an encoder just complete enough for the Erlang Abstract Format
to_json(Atom) when is_atom(Atom) ->
    % Make atoms into string
    "\"" ++ atom_to_list(Atom) ++ "\"";
to_json(Int) when is_integer(Int) ->
    integer_to_list(Int);
to_json(Float) when is_float(Float) ->
    float_to_list(Float);
to_json(Tuple) when is_tuple(Tuple) ->
    % Tuples become JSON arrays
    to_json_array(tuple_to_list(Tuple));
to_json(List) when is_list(List) ->
    case is_string(List) of
        true  -> "\"" ++ escape_string(List) ++ "\"";
        false -> to_json_array(List)
    end.

to_json_array(List) ->
    "[" ++ string:join([to_json(E) || E <- List], ",") ++ "]".

% Distinguish between Erlang strings and literal strings
is_string([H|_] = List) when is_integer(H) ->
    io_lib:printable_list(List);
is_string(_) -> 
    false. % Empty list[] or list of tuples returns false and becomes a JSON array[]

% Escape special characters in strings
escape_string(Str) ->
    lists:flatmap(fun
        ($")  -> "\\\"";
        ($\\) -> "\\\\";
        ($\n) -> "\\n";
        ($\r) -> "\\r";
        ($\t) -> "\\t";
        (C)   -> [C]
    end, Str).
