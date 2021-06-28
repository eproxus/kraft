# Kraft

An minimalistic Erlang web framework.

## Examples

Start an example shell:

```
rebar3 as example shell
```

Navigate to http://localhost:8090 to see a list of examples. All examples can be
found in the `examples/` folder.

## Development

### Formatting

Kraft uses [erlfmt][erlfmt] to format its code.

* `rebar3 fmt` to reformat all the code
* `rebar3 fmt path/to/file` to reformat a single file
* `rebar3 check` to check if formatting adheres

Kraft includes a Git hook that checks formatting before committing. To start
using the included Git hook in this repository, run:

```
git config core.hooksPath .githooks
```


[erlfmt]: https://github.com/WhatsApp/erlfmt
