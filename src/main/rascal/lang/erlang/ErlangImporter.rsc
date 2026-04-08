module lang::erlang::ErlangImporter

import lang::json::IO;
import IO;

list[value] parseErlangJSON(str rawJSON) =
    parseJSON(#list[value], rawJSON);
