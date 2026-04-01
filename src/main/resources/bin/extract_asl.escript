% Usage: escript extract_ast.escript <file.erl> <include_dir>

main([Filename, IncludeDir]) ->
    Options = [{includes, [IncludeDir]}],

    case epp:parse_file(Filename, Options) of
        {ok, Forms} ->
            io:format("~p~n", [Forms]);
        {error, Error} ->
            io:format(standard_error, "Error parsing file: ~p~n", [Error]),
            halt(1)
    end.
