module lang::erlang::analysis::DeadFunctionAnalyser

/**
 * Detects dead functions
 * A function is considered "dead" when:
 * - It is private
 * - It is not reachable from any of the public functions in the module
 * Maybe not when any function in the module contains a dynamic function call (to avoid false positives)?
 */

import List;
import String;

import lang::erlang::M3;

loc normaliseFunctionLoc(loc l) {
    if (l.scheme == "erlang+function") {
        parts = [ p | p <- split("/", l.path), p != "" ];
        if (size(parts) >= 3) {
            return |erlang+function:///<parts[0]>/<parts[1]>/<parts[2]>|;
        }
    }
    return l;
}

set[loc] findDeadFunctions(M3 model) {
    // Get all declared functions in model
    declaredFunctions = { d | <d, _> <- model.declarations, d.scheme == "erlang+function" };

    // Find public functions (entrypoints)
    publicFunctions = { f | <f, \public()> <- model.modifiers, f.scheme == "erlang+function" };

    // Map callers and callees
    // Normalise logical locations to base function
    normalisedCalls = { <normaliseFunctionLoc(caller), normaliseFunctionLoc(callee)> 
                | <caller, callee> <- model.functionCalls};
    
    // Reachability analysis
    // "+" is transitive closure
    reachableFunctions = publicFunctions + (normalisedCalls+)[publicFunctions];

    // Anything not in reachableFunctions is dead
    return declaredFunctions - reachableFunctions;
}
