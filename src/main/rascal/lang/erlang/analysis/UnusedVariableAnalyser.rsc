module lang::erlang::analysis::UnusedVariableAnalyser

import String;
import List;
import lang::erlang::M3;

set[loc] findUnusedVariables(M3 model) {
    declaredVars = { d | <d, _> <- model.declarations, d.scheme == "erlang+variable" };
    usedVars = { u | <_, u> <- model.uses, u.scheme == "erlang+variable" };
    
    unusedVars = declaredVars - usedVars;
    
    // Filter out variables that start with underscore
    return { v | v <- unusedVars, !isIgnoredVar(v) };
}

// Extracts variable name and returns whether it starts with an underscord
private bool isIgnoredVar(loc l) {
    parts = [ p | p <- split("/", l.path), p != "" ];
    if (size(parts) > 0) {
        return startsWith(parts[size(parts)-1], "_");
    }
    return false;
}
