module lang::erlang::ParseFile

import util::ShellExec;

loc PARSE_SCRIPT = |project://erlair/src/main/resources/bin/extract_asl.escript|;

// TODO: Not portable/compatible with other OS
str getRawAst(loc file, loc includeDir) = 
    exec(|file:///usr/bin/escript|, args=[PARSE_SCRIPT, file, includeDir]);
