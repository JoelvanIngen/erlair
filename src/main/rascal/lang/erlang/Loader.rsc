module lang::erlang::Loader

import IO;
import List;
import lang::erlang::AST;
import lang::erlang::ParseFile;
import lang::erlang::Parser;

set[str] EXTENSIONS = {"erl", "escript"};

// Recursively scans a folder and finds all Erlang source and script files
list[loc] findErlangFiles(loc dir) {
    list[loc] files = [];
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
    list[loc] sources = findErlangFiles(rootFolder);
    list[EAF] projectASTs = [];

    nTotal = size(sources);
    println("Parsing <nTotal> files");
    int i = 0;
    for (loc file <- sources) {
        i += 1;
        try {
            loc currentInclude = (includeDir == |unknown:///|) ? file.parent : includeDir;
            str rawAst = getAstJSON(file, currentInclude);
            projectASTs += [parseErlangAST(rawAst)];
            // println("<i>/<nTotal> :: Parsed: <rootFolder + file.path>");
        } catch value e: {
            println("<i>/<nTotal> :: Error parsing <rootFolder + file.path>: <e>");
        }
    }
    return projectASTs;
}
