-module(test_analysis_cyclic_types).

-compile(export_all).

% Test that record types in -type declarations don't break analyser
-record(test_record, {
    key :: atom(),
    value :: term()
}).

% Depends on built-ins
-type test_int() :: integer().
-type test_binary() :: binary().

% Dependency on locally defined type
-type test_int_2() :: test_int().

% Dependency on both built-in and locally defined
-type list_of_ints() :: list(test_int()).

% Dependency on record
-type test_record_type() :: #test_record{}.

% Dependency on remote module type
-type remote_dict() :: dict:dict().

% Short cycle
-type x() :: y().
-type y() :: x().

% Long cycle
-type a() :: b().
-type b() :: c().
-type c() :: a().

% Opaque type
-opaque secret() :: reference().

% Non-cyclic type that uses cyclic type (should not be reported)
-type uses_cyclic() :: x().
