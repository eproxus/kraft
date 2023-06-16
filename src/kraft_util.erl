-module(kraft_util).

% API
-export([split_path/1]).

%--- API -----------------------------------------------------------------------

split_path(Path) -> string:lexemes(Path, [$/]).
