module lang::erlang::analysis::NamingConventionsAnalyser

import lang::erlang::M3;

set[loc] findNonIdiomaticNames(M3 model) {
    // Only check functions/records/modules (variables need to be uppercase so they're excluded)
    checkSchemes = {"erlang+function", "erlang+record", "erlang+module"};

    // Inverted declarations
    physToLogical = model.declarations<1, 0>;
    
    return { logicalLoc | <str name, loc physLoc> <- model.names, 
                       /[A-Z]/ := name,
                       logicalLoc <- physToLogical[physLoc],
                       logicalLoc.scheme in checkSchemes };
}
