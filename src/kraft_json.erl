-module(kraft_json).

% API
-export([decode/1]).
-ignore_xref([{?MODULE, decode_stream, 1}]).
-export([decode_stream/1]).
-export([encode/1]).

%--- API -----------------------------------------------------------------------

decode(Binary) -> jsx:decode(Binary, [{labels, attempt_atom}]).

decode_stream(Binary) ->
    jsx:decode(Binary, [{labels, attempt_atom}, stream, return_tail]).

encode(Term) -> jsx:encode(Term).
