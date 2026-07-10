module lang::erlang::ParseFile

import String;
import util::ShellExec;

loc PARSE_SCRIPT = |project://erlair/src/main/resources/bin/extract_ast.escript|;

// TODO: Not portable/compatible with other OS
str getAstJSON(loc file, loc includeDir) {
    <result, code> = execWithCode(|file:///usr/bin/escript|, args=[PARSE_SCRIPT, file, includeDir]);
    if (code != 0) throw "Erlang Pre-processor Error";
    return trim(result);
}
