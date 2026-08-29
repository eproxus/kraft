# Kraft REST API with Telemetry Example

This example is about how to build a REST API using the Kraft web framework,
while integrating telemetry monitoring through
[Cowboy Telemetry](https://github.com/beam-telemetry/cowboy_telemetry).

> [!TIP]
> [Cowboy Telemetry](https://github.com/beam-telemetry/cowboy_telemetry) is a
> prerequisite for using 
> [opentelemetry_cowboy](https://hexdocs.pm/opentelemetry_cowboy/readme.html),
> a Telemetry handler  that translates Cowboy events into OpenTelemetry spans,
> enabling HTTP request/response instrumentation.
