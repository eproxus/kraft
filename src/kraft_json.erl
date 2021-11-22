-module(kraft_json).

% API
-export([decode/1]).
-export([encode/1]).

%--- API -----------------------------------------------------------------------

decode(Binary) -> jsx:decode(Binary, [{labels, attempt_atom}]).

encode(Term) -> jsx:encode(Term).
