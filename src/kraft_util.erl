-module(kraft_util).

% API
-export([split_path/1]).
-export([attempt_atom/1]).

%--- API -----------------------------------------------------------------------

split_path(Path) -> string:lexemes(Path, [$/]).

attempt_atom(Binary) ->
    try
        binary_to_existing_atom(Binary)
    catch
        error:badarg -> Binary
    end.
