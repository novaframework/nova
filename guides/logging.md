# Logging

Nova ships with `nova_jsonlogger`, a structured JSON formatter for the standard OTP `logger`. It can emit log records in several common schemas so they drop straight into the backend you already use.

## Setup

Wire `nova_jsonlogger` as a formatter on the default handler in `sys.config`:

```erlang
{kernel, [
  {logger_level, info},
  {logger, [
    {handler, default, logger_std_h, #{
      formatter => {nova_jsonlogger, #{
        schema => ecs,
        new_line => true
      }}
    }}
  ]}
]}.
```

All logs emitted via the `?LOG_INFO`, `?LOG_ERROR`, etc. macros are then rendered as single-line JSON.

## Supported schemas

Pick the schema that matches your log pipeline. The default (`nova`) preserves Nova's historical shape and adds RFC 3339 timestamps.

| Schema    | Timestamp key   | Level key                          | Message key | Trace correlation keys                                          |
|-----------|-----------------|------------------------------------|-------------|------------------------------------------------------------------|
| `nova`    | `time`          | `level` (atom)                     | `text`      | `trace_id`, `span_id`                                            |
| `ecs`     | `@timestamp`    | `log.level` (lowercase)            | `message`   | `trace.id`, `span.id`                                            |
| `otel`    | `Timestamp`     | `SeverityText` + `SeverityNumber`  | `Body`      | `TraceId`, `SpanId`                                              |
| `gcp`     | `time`          | `severity` (uppercase)             | `message`   | `logging.googleapis.com/trace`, `logging.googleapis.com/spanId`  |
| `datadog` | `timestamp`     | `status`                           | `message`   | `dd.trace_id`, `dd.span_id`                                      |

### Severity numbers (OTel)

Erlang levels map to the lowest number in their OTel severity band:

| Erlang level | `SeverityNumber` |
|--------------|-------------------|
| `debug`      | 5                 |
| `info`       | 9                 |
| `notice`     | 10                |
| `warning`    | 13                |
| `error`      | 17                |
| `critical`   | 21                |
| `alert`      | 22                |
| `emergency`  | 24                |

### Source location

`logger` automatically passes `file`, `line`, and `mfa` meta when the `?LOG_*` macros are used. Each schema renders source location in its native form:

- **ECS**: `log.origin.file.name`, `log.origin.file.line`, `log.origin.function`
- **GCP**: a nested `logging.googleapis.com/sourceLocation` object with `file`, `line`, `function`

## Structured errors

When the `crash_report` SASL meta or any of `class`, `reason`, `stacktrace` are present, `nova_jsonlogger` builds a structured `error` object:

```json
{
  "error": {
    "type": "error",
    "reason": "badarg",
    "stacktrace": [
      {"mfa": "mymod:myfun/2", "file": "mymod.erl", "line": 17}
    ]
  }
}
```

The schema renders this under its own conventions:

| Schema | Type key           | Message key           | Stacktrace key             |
|--------|--------------------|------------------------|----------------------------|
| ECS    | `error.type`       | `error.message`        | `error.stack_trace`        |
| OTel   | `exception.type`   | `exception.message`    | `exception.stacktrace`     |

To attach a structured error from your own code, include the keys in the log report:

```erlang
?LOG_ERROR(#{
  text => <<"payment failed">>,
  class => error,
  reason => Reason,
  stacktrace => Stacktrace
}).
```

## Trace correlation

When `trace_id` and `span_id` are present in the process metadata (set via `logger:update_process_metadata/1`), the formatter renders them under each schema's conventional keys. Nothing else is required from the formatter side - it is the responsibility of upstream instrumentation to populate them.

If you use `opentelemetry_nova`, this is wired for you: each HTTP request runs through the plugin which writes the active span's hex trace and span ids into process metadata. Logs emitted during the request are then correlatable with traces in any backend that joins on those fields.

## Redaction

Use `redact` to scrub field paths before encoding:

```erlang
{nova_jsonlogger, #{
  schema => ecs,
  redact => [
    [req, headers, authorization],
    [user, password],
    [stripe, api_key]
  ]
}}
```

Each path is a list of keys walked through the report. The matched value is replaced with `<<"[REDACTED]">>`. Missing paths are a no-op.

## Term-size guards

Two caps prevent a single oversized term from ballooning a log line:

| Key                  | Default | Behaviour                                                                          |
|----------------------|---------|-------------------------------------------------------------------------------------|
| `max_term_size`      | 8192    | Caps the output of `~0p`-formatted complex terms (lists, tuples, nested terms)      |
| `max_string_length`  | 8192    | Caps binaries, pid/port/function representations, and string lists                  |

When a value exceeds its cap, it is truncated and a `"...[truncated]"` marker is appended.

## Hooks for customisation

Three config keys let you override anything the schema produced:

- `meta_with` / `meta_without` - allowlist or denylist meta keys before rendering
- `key_mapping` - rename keys after schema rendering, e.g. `#{level => severity}`
- `format_funs` - transform values after schema and key mapping, e.g. `#{message => fun string:uppercase/1}`

These run in the order `schema -> key_mapping -> format_funs`, so a `format_funs` entry targets the post-schema key name.

## Choosing a schema

- **`ecs`** if you ship to Elastic, OpenSearch, or anything that speaks the Elastic Common Schema.
- **`otel`** for OpenTelemetry-native ingestors and any backend that joins logs to traces via OTel semantic conventions.
- **`gcp`** for Google Cloud Logging.
- **`datadog`** for Datadog Logs.
- **`nova`** (default) for development, file logs, or anywhere the keys are read by humans.

You can switch at runtime by reconfiguring the handler - no application code needs to change.
