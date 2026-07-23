module Reports

import IO;
import List;
import Set;
import String;
import lang::erlang::AST;
import lang::erlang::M3;
import lang::erlang::analysis::CyclicTypeAnalyser;
import lang::erlang::analysis::DeadClauseAnalyser;
import lang::erlang::analysis::DeadFunctionAnalyser;
import lang::erlang::analysis::UnusedRecordAnalyser;
import lang::erlang::analysis::UnusedVariableAnalyser;

str reportDeadFunctions(M3 model) {
    dead = findDeadFunctions(model);
    res = "";
    if (isEmpty(dead)) {
        res += "No dead functions found.\n";
    } else {
        res += "Found <size(dead)> dead function(s):\n";
        for (f <- dead) {
            loc physLoc = getOneFrom(model.declarations[f]);
            res += " - <f.path> at <physLoc>\n";
        }
    }
    return res;
}

str reportUnusedRecordsAndFields(M3 model) {
    res = "";

    // Unused Records
    unusedRecords = findUnusedRecords(model);
    if (isEmpty(unusedRecords)) {
        res += "No unused records found.\n";
    } else {
        res += "Found <size(unusedRecords)> unused record(s):\n";
        for (r <- unusedRecords) {
            loc physLoc = getOneFrom(model.declarations[r]);
            res += " - Record <r.path> at <physLoc>\n";
        }
    }

    // Unused Fields in Used Records
    unusedFields = findUnusedFieldsOfUsedRecords(model);
    if (isEmpty(unusedFields)) {
        res += "No unused fields found in active records.\n";
    } else {
        res += "Found <size(unusedFields)> unused field(s) within active records:\n";
        for (f <- unusedFields) {
            loc physLoc = getOneFrom(model.declarations[f]);
            res += " - Field <f.path> at <physLoc>\n";
        }
    }

    return res;
}

str reportUnusedVariables(M3 model) {
    unused = findUnusedVariables(model);
    res = "";
    if (isEmpty(unused)) {
        res += "No unused variables found.\n";
    } else {
        res += "Found <size(unused)> unused variable(s):\n";
        for (v <- unused) {
            loc physLoc = getOneFrom(model.declarations[v]);
            parts = [ p | p <- split("/", v.path), p != "" ];
            str varName = size(parts) > 0 ? parts[size(parts)-1] : v.path;
            res += "- Variable \'<varName>\' at <physLoc>\n";
        }
    }
    return res;
}

str reportDeadClauses(EAF ast) {
    dead = findDeadClauses(ast);
    str res = "";
    if (isEmpty(dead)) {
        res += "No dead clauses found.\n";
    } else {
        res += "Found <size(dead)> dead clause(s):\n";
        for (d <- dead) {
            res += "- Dead clause at <d>\n";
        }
    }
    return res;
}

str reportCyclicTypes(M3 model) {
    cyclic = findCyclicTypes(model);
    str res = "";
    if (isEmpty(cyclic)) {
        res += "No cyclic type definitions found.\n";
    } else {
        res += "Found <size(cyclic)> cyclic type definition(s):\n";
        for (c <- cyclic) {
            res += "- Cyclic definition at <c>\n";
        }
    }
    return res;
}

str reportProcessSpawns(M3 model) {
    spawns = model.processSpawns;  // Linter complains, not sure why because it runs perfectly fine
    res = "";
    if (isEmpty(spawns)) {
        res += "No process spawns found.\n";
    } else {
        res += "Found <size(spawns)> process spawn(s):\n";
        for (<caller, entryPoint> <- spawns) {
            loc physLoc = |unknown:///|;
            if (caller in model.declarations<0>) {
                physLoc = getOneFrom(model.declarations[caller]);
            }
            res += " - <caller.path> at <physLoc> spawns <entryPoint>\n";
        }
    }
    return res;
}
