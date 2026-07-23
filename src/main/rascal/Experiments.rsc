module Experiments

import DateTime;
import IO;
import List;
import Message;
import Reports;
import String;
import lang::erlang::AST;
import lang::erlang::Loader;
import lang::erlang::M3;
import lang::erlang::ParseFile;
import lang::erlang::Parser;
import util::Progress;

loc REPORT_ROOT = |project://erlair/src/main/output/report/|;

/**
 * Creates report for a single file
 */
str createReport(loc fileLoc, EAF ast, M3 model) {
    res = "#######\nReport for file <fileLoc>\n#######\n";
    res += formatParseErrors(model);
    
    res += "Dead Functions:\n";
    res += reportDeadFunctions(model);
    res += "\n";

    res += "Unused Records/Fields:\n";
    res += reportUnusedRecordsAndFields(model);
    res += "\n";

    res += "Unused Variables:\n";
    res += reportUnusedVariables(model);
    res += "\n";

    res += "Unreachable Clauses:\n";
    res += reportDeadClauses(ast);
    res += "\n";

    res += "Cyclic Type Definitions:\n";
    res += reportCyclicTypes(model);
    res += "\n";

    res += "Process Spawns:\n";
    res += reportProcessSpawns(model);
    res += "\n";

    res += "Message Sends:\n";
    res += reportMessageSends(model);
    
    return res;
}

/**
 * Creates report for multiple files when asts and models have not yet been generated
 */
void createReport(loc rootFolder) {
    loc reportFn = REPORT_ROOT + getReportFileName() + "report.txt";
    writeFile(reportFn, "");  // Create empty
    
    list[loc] files = findErlangFiles(rootFolder);
    nFiles = size(files);

    nFailures = 0;

    <update, finish> = progressBar(nFiles, prefix="Processing:");
    for (i <- [0..nFiles]) {
        file = files[i];
        update("<i>/<nFiles>: <file>                                 ");
        try {
            appendToFile(reportFn, loop(file) + "\n\n");
        } catch value e: {
            println("Error processing <file>: <e>                                   ");
            nFailures += 1;
        }
    }
    finish();

    if (nFailures > 0) println("\nFailed files: <nFailures>.\n");
}

/**
 * Parses single file and returns report
 */
str loop(loc file) {
    EAF ast = parseErlangAST(getAstJSON(file, findBestIncludeDirs(file)));
    return createReport(
        file,
        ast,
        extractErlangM3(file, ast)
    );
}

str getReportFileName() {
    datetime dt = now();
    return "<dt.year><dt.month><dt.day>-<dt.hour><dt.minute><dt.second>";
}

/**
 * Detects whether any pre-processor errors occurred, and
 * extends the report entry with a warning if so
 */
str formatParseErrors(M3 model) {
    errors = [ m | m <- model.messages, Message::\error(_, _) := m ];
    if (isEmpty(errors)) return "";
    return "WARNING: PRE-PROCESSOR ERRORS IN FILE; RESULTS MAY NOT BE ACCURATE\n"; 
}
