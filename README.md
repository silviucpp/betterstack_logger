# betterstack_logger

[![Build Status](https://app.travis-ci.com/silviucpp/betterstack_logger.svg?branch=main)](https://travis-ci.com/github/silviucpp/betterstack_logger)
[![GitHub](https://img.shields.io/github/license/silviucpp/betterstack_logger)](https://github.com/silviucpp/betterstack_logger/blob/main/LICENSE)
[![Hex.pm](https://img.shields.io/hexpm/v/betterstack_logger)](https://hex.pm/packages/betterstack_logger)

OTP [logger][1] backend that sends log events to [BetterStack Logs][2].

## Quick start

Add to your `rebar.config`

```erlang
{deps, [betterstack_logger]}.
```

add the logger handler settings to your `sys.config`

```erlang
{betterstack_logger, [
    {logger,
        [
            {handler, betterstack_logs, betterstack_logger, #{
                level => info,
                config => #{
                    http_pool_options => [
                        {timeout, 15000},
                        {max_connections, 10}
                    ],

                    betterstack_source_token => <<"SOURCE_TOKEN_HERE">>,
                    extra_fields => [
                      {<<"_env">>, <<"prod">>}
                    ],
                    upload_batch_max_size => 50,
                    upload_batch_inteval_ms => 5000,
                    upload_failed_retry_count => 3,
                    upload_failed_retry_delay_ms => 1000
                },
                formatter => {
                    logger_formatter, #{
                        single_line => true,
                        template => [pid, " ", mfa,":",line, " => ", msg],
                        time_offset => "Z"
                    }
                },
                filters => [
                    %{remote_group_leader, {fun logger_filters:remote_gl/2, stop}},
                    %{progress, {fun logger_filters:progress/2, stop}},
                    %{sasl, {fun logger_filters:domain/2, {stop, sub, [otp, sasl]}}}
                ]
            }}
        ]}
]}
```

Or you can add it via logger API from your code:

```erlang
logger:add_handler(my_handler, betterstack_logs, #{...}).
```

**Note:** It's not currently possible to set `betterstack_logger` as `default` handler via `sys.config` (or add the handler there directly under the `kernel` app),
because `sys.config` is applied at `kernel` application start time and `betterstack_logger` application depends on `kernel` application (cyclic dependency). 

## Config properties

| Property               | Mandatory | Description                                                                                                                                                                                          |
| --------------------- |:---------:|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| http_pool_options      |           | [hackney][3] pool config.                                                                                                                                                                            |
|betterstack_source_token|     Y     | BetterStack Source Token used for authentication.                                                                                                                                                    |
|extra_fields|           | Extra fields to be added to the message event.|                                                                                                                                                       |
| upload_batch_max_size |           | *Default: 50*. The events are sent in batches. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed.                                |
| upload_batch_inteval_ms|           | *Default: 5000*. Number of milliseconds we can wait for events to accumulate. A batch is sent when is reaching the `upload_batch_max_size` size or a number of `upload_batch_inteval_ms` ms elapsed. |
| upload_failed_retry_count |           | *Default: 3*. In case a batch sending operation failed, how many times we retry to resubmit.                                                                                                         |
| upload_failed_retry_delay_ms|           | *Default: 1000*. The delay between resubmitting failed batches.                                                                                                                                      |

- Beside this custom settings, all other standard `logger:handler_config()` properties are accepted (`level`, `filters`, `formatter`).
- Multiple instances of `betterstack_logger` handler can be started. 

[1]:https://www.erlang.org/doc/apps/kernel/logger_chapter.html
[2]:https://logs.betterstack.com
[3]:https://github.com/benoitc/hackney
