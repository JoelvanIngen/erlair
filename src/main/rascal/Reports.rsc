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
import lang::erlang::analysis::MissingSpecAnalyser;
import lang::erlang::analysis::NamingConventionsAnalyser;
import lang::erlang::analysis::ShadowedBifsAnalyser;
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

str reportMessageSends(M3 model) {
    sends = model.messageSends;
    res = "";
    if (isEmpty(sends)) {
        res += "No message sends to registered processes found.\n";
    } else {
        res += "Found <size(sends)> message send(s) to registered processes:\n";
        for (<caller, target> <- sends) {
            loc physLoc = |unknown:///|;
            if (caller in model.declarations<0>) {
                physLoc = getOneFrom(model.declarations[caller]);
            }
            
            // Clean up registration URI path
            str targetName = target.path;
            if (startsWith(targetName, "/")) {
                targetName = substring(targetName, 1);
            }
            
            res += " - <caller.path> at <physLoc> sends to registered process \'<targetName>\'\n";
        }
    }
    return res;
}

str reportMissingSpecs(M3 model) {
    missingSpecs = findMissingSpecs(model);
    str res = "";
    if (isEmpty(missingSpecs)) {
        res += "No public functions with missing specs found.\n";
    } else {
        res += "Found <size(missingSpecs)> public function(s) with missing specs:\n";
        for (m <- missingSpecs) {
            res += "- Missing spec at at <m>\n";
        }
    }
    return res;
}

str reportShadowedBifs(M3 model) {
    shadowedBifs = findShadowedBifs(model);
    str res = "";
    if (isEmpty(shadowedBifs)) {
        res += "No shadowed BIFs found.\n";
    } else {
        res += "Found <size(shadowedBifs)> shadowed BIF(s):\n";
        for (m <- shadowedBifs) {
            res += "- Shadowed BIF at <m>\n";
        }
    }
    return res;
}

str reportNonIdiomaticNames(M3 model) {
    badNames = findNonIdiomaticNames(model);
    str res = "";
    if (isEmpty(badNames)) {
        res += "No non-idiomatic names found.\n";
    } else {
        res += "Found <size(badNames)> non-idiomatic name(s):\n";
        for (n <- badNames) {
            res += "- Non-idiomatic name at <n>\n";
        }
    }
    return res;
}
