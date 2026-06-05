module examples::Analyses

import IO;
import Set;
import lang::erlang::M3;
import lang::erlang::analysis::DeadFunctionAnalyser;
import lang::erlang::analysis::UnusedRecordAnalyser;

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

void reportUnusedRecordsAndFields(M3 model) {
    // Unused Records
    unusedRecords = findUnusedRecords(model);
    if (isEmpty(unusedRecords)) {
        println("No unused records found.");
    } else {
        println("Found <size(unusedRecords)> unused record(s):");
        for (r <- unusedRecords) {
            loc physLoc = getOneFrom(model.declarations[r]);
            println(" - Record <r.path> at <physLoc>");
        }
    }

    println();

    // Unused Fields in Used Records
    unusedFields = findUnusedFieldsOfUsedRecords(model);
    if (isEmpty(unusedFields)) {
        println("No unused fields found in active records.");
    } else {
        println("Found <size(unusedFields)> unused field(s) within active records:");
        for (f <- unusedFields) {
            loc physLoc = getOneFrom(model.declarations[f]);
            println(" - Field <f.path> at <physLoc>");
        }
    }
}
