module examples::Analyses

import IO;
import Set;
import lang::erlang::M3;
import lang::erlang::analysis::DeadFunctionAnalyser;

void reportDeadFunctions(M3 model) {
    dead = findDeadFunctions(model);
    if (isEmpty(dead)) {
        println("No dead functions found.");
    } else {
        println("Found <size(dead)> dead function(s):");
        for (f <- dead) {
            loc physLoc = getOneFrom(model.declarations[f]);
            println(" - <f.path> at <physLoc>");
        }
    }
}
