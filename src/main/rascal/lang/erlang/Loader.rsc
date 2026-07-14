module lang::erlang::Loader

import IO;
import List;
import lang::erlang::AST;
import lang::erlang::ParseFile;
import lang::erlang::Parser;

set[str] EXTENSIONS = {"erl", "escript"};

// Recursively walks up from the file to find the best project include
// Works some of the time
// Definitely better than nothing
loc findBestIncludeDir(loc file) {
    loc current = file.parent;
    // Avoid infinite loops
    while (current.path != "/" && current.path != "") {
        // If we find an 'apps' or 'lib' directory, probably best include root
        if (exists(current + "apps") && isDirectory(current + "apps")) {
            return current + "apps";
        }
        if (exists(current + "lib") && isDirectory(current + "lib")) {
            return current + "lib";
        }
        current = current.parent;
    }

    current = file.parent;
    // Avoid infinite loops
    while (current.path != "/" && current.path != "") {
        // If we find a rebar.config or standard configuration file, 
        // the sibling 'include' or the root itself might work
        if (exists(current + "rebar.config") || exists(current + "erlang.mk")) {
            if (exists(current + "include") && isDirectory(current + "include")) {
                return current + "include";
            }
            return current;
        }
        current = current.parent;
    }
    
    // Fall back to the sibling 'include' folder if it exists, otherwise the parent
    loc siblingInclude = file.parent.parent + "include";
    if (exists(siblingInclude) && isDirectory(siblingInclude)) {
        return siblingInclude;
    }
    
    return file.parent;
}

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
