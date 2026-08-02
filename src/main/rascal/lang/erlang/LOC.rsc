module lang::erlang::LOC

import IO;
import List;
import String;
import lang::erlang::Loader;

// Counts lines in a single file
int countFileLOC(loc file) {
    list[str] lines = readFileLines(file);

    lines = [ l | str line <- lines, 
                  str l := trim(line), 
                  l != "", 
                  !startsWith(l, "%") ];

    return size(lines);
}

int countDirLOC(loc dir) {
    int total = 0;
    for (loc f <- findErlangFiles(dir)) {
        total += countFileLOC(f);
    }
    return total;
}
