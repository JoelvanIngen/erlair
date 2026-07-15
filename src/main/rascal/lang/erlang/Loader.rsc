module lang::erlang::Loader

import IO;
import List;
import Set;
import lang::erlang::AST;
import lang::erlang::ParseFile;
import lang::erlang::Parser;

set[str] EXTENSIONS = {"erl", "escript"};

bool existsDirectory(loc f) = exists(f) && isDirectory(f);

// Recursively walks up from the file to find the project include directories
list[loc] findBestIncludeDirs(loc file) {
    list[loc] dirs = [];
    loc current = file.parent;

    // Avoid infinite loops
    while (current.path != "/" && current.path != "") {
        appsInclude = current + "apps";
        libInclude = current + "lib";
        loc localInclude = current + "include";

        if (existsDirectory(appsInclude)) {
            dirs += appsInclude;
        }

        if (existsDirectory(libInclude)) {
            dirs += libInclude;
        }

        if (existsDirectory(localInclude)) {
            dirs += localInclude;
        }

        if (exists(current + "rebar.config") || exists(current + "erlang.mk")) {
            dirs += current;
        }

        current = current.parent;
    }
    
    // Explicitly add sibling include folder if exists
    loc siblingInclude = file.parent.parent + "include";
    if (exists(siblingInclude) && isDirectory(siblingInclude)) {
        dirs += siblingInclude;
    }

    // Always add file's parent directory
    dirs += file.parent;

    // Deduplication time
    return toList(toSet(dirs));
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

list[EAF] loadProjectASTs(loc rootFolder, list[loc] includeDirs = [|unknown:///|]) {
    list[loc] sources = findErlangFiles(rootFolder);
    list[EAF] projectASTs = [];

    nTotal = size(sources);
    println("Parsing <nTotal> files");
    int i = 0;
    for (loc file <- sources) {
        i += 1;
        try {
            list[loc] currentIncludes = (includeDirs == [|unknown:///|]) ? findBestIncludeDirs(file) : includeDirs;
            str rawAst = getAstJSON(file, currentIncludes);
            projectASTs += [parseErlangAST(rawAst)];
            // println("<i>/<nTotal> :: Parsed: <rootFolder + file.path>");
        } catch value e: {
            println("<i>/<nTotal> :: Error parsing <rootFolder + file.path>: <e>");
        }
    }
    return projectASTs;
}
