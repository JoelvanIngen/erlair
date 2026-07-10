module lang::erlang::Loader

import IO;
import Set;
import lang::erlang::AST;
import lang::erlang::ParseFile;
import lang::erlang::Parser;

set[str] EXTENSIONS = {"erl", "escript"};

// Recursively scans a folder and finds all Erlang source and script files
set[loc] findErlangFiles(loc dir) {
    set[loc] files = {};
    if (isDirectory(dir)) {
        for (str entry <- listEntries(dir)) {
            files += findErlangFiles(dir + entry);
        }
    } else if (dir.extension in EXTENSIONS) {
        files += dir;
    }
    return files;
}

list[EAF] loadProjectASTs(loc rootFolder, loc includeDir = |unknown:///|) {
    set[loc] sources = findErlangFiles(rootFolder);
    list[EAF] projectASTs = [];

    nTotal = size(sources);
    int i = 0;
    for (loc file <- sources) {
        i += 1;
        try {
            loc currentInclude = (includeDir == |unknown:///|) ? file.parent : includeDir;
            str rawAst = getAstJSON(file, currentInclude);
            projectASTs += [parseErlangAST(rawAst)];
            println("<i>/<nTotal> :: Parsed: <file.path>");
        } catch value e: {
            println("<i>/<nTotal> :: Error parsing <file.path>: <e>");
        }
    }
    return projectASTs;
}
