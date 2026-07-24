module lang::erlang::analysis::ShadowedBifsAnalyser

import List;
import String;
import lang::erlang::M3;

set[loc] findShadowedBifs(M3 model) {
    // Paths look like /module/function/arity
    declaredFunctions = { d | <d, _> <- model.declarations, d.scheme == "erlang+function" };

    // Paths look like /function/arity
    bifPaths = { b.path | b <- model.implicitDeclarations };

    // Normalise paths
    return { f | f <- declaredFunctions, 
                 parts := split("/", f.path), 
                 size(parts) >= 4, 
                 "/<parts[2]>/<parts[3]>" in bifPaths };
}
