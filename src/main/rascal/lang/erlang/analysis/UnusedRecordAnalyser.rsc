module lang::erlang::analysis::UnusedRecordAnalyser

import lang::erlang::M3;

set[loc] findUnusedRecords (M3 model) {
    declaredRecords = { d | <d, _> <- model.declarations, d.scheme == "erlang+record" };
    usedRecords = { u | <_, u> <- model.uses, u.scheme == "erlang+record" };
    return declaredRecords - usedRecords;
}

set[loc] findUnusedFields (M3 model) {
    declaredFields = { d | <d, _> <- model.declarations, d.scheme == "erlang+field" };
    usedFields = { u | <_, u> <- model.uses, u.scheme == "erlang+field" };
    return declaredFields - usedFields;
}

// Finds unused fields, but only from used records
// Reduces noise
set[loc] findUnusedFieldsOfUsedRecords(M3 model) {
    usedRecords = { u | <_, u> <- model.uses, u.scheme == "erlang+record" };
    declaredFieldsOfUsedRecords = { f | <r, f> <- model.containment, r in usedRecords, f.scheme == "erlang+field" };
    usedFields = { u | <_, u> <- model.uses, u.scheme == "erlang+field" };
    return declaredFieldsOfUsedRecords - usedFields;
}
