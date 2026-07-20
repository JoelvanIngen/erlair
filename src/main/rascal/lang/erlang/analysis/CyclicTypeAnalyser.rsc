module lang::erlang::analysis::CyclicTypeAnalyser

import lang::erlang::M3;
import List;

// Finds types that directly or transitively depend on themselves
set[loc] findCyclicTypes(M3 model) = { T | <T, T> <- model.typeDependencies+ };
