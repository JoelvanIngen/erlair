module lang::erlang::analysis::CyclicTypeAnalyser

import lang::erlang::M3;
import List;

// Finds types that directly or transitively depend on themselves
set[loc] findCyclicTypes(M3 model) {
    set[loc] cyclic = {};
    
    // model.typeDependencies+ computes the transitive closure
    // If <T, T> is in the closure, T is cyclic
    for (<T, _> <- model.typeDependencies+) {
        if (<T, T> in model.typeDependencies+) {
            cyclic += { T };
        }
    }
    
    return cyclic;
}