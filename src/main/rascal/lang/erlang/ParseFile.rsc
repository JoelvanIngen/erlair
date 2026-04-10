module lang::erlang::ParseFile

import String;
import util::ShellExec;

loc PARSE_SCRIPT = |project://erlair/src/main/resources/bin/extract_ast.escript|;

// TODO: Not portable/compatible with other OS
str getRawAst(loc file, loc includeDir) = 
    trim(exec(|file:///usr/bin/escript|, args=[PARSE_SCRIPT, file, includeDir]));
