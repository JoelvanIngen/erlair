module lang::erlang::analysis::MissingSpecAnalyser

import lang::erlang::M3;

set[loc] findMissingSpecs(M3 model) {
    publicFunctions = { f | <f, \public()> <- model.modifiers, f.scheme == "erlang+function" };
    typedFunctions = model.types<0>;
    
    return publicFunctions - typedFunctions;
}
