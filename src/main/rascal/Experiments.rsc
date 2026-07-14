module Experiments

import DateTime;
import IO;
import List;
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
    
    return res;
}

/**
 * Creates report for multiple files when asts and models have already been generated
 */
str createReport(list[loc] fileLocs, list[EAF] asts, list[M3] models)
    = intercalate("\n\n", [ report(fileLocs[i], asts[i], models[i]) | i <- [0..size(fileLocs)] ]);

/**
 * Creates report for multiple files when asts and models have not yet been generated
 */
void createReport(loc rootFolder) {
    loc reportFn = REPORT_ROOT + getReportFileName() + "report.txt";
    writeFile(reportFn, "");  // Create empty
    
    list[loc] files = findErlangFiles(rootFolder);
    nFiles = size(files);

    nFailures = 0;

    // <update, finish> = progressBar(nFiles, prefix="Processing:");
    for (i <- [0..nFiles]) {
        file = files[i];
        // update("<i>/<nFiles>: <file>");
        try {
            appendToFile(reportFn, loop(file) + "\n\n");
        } catch value e: {
            println("Error processing <file>: <e>");
            nFailures += 1;
        }
    }
    // finish();

    if (nFailures > 0) println("\nFailed files: <nFailures>.\n");
}

/**
 * Parses single file and returns report
 */
str loop(loc file) {
    EAF ast = parseErlangAST(getAstJSON(file, file.parent));
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
