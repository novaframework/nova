# Graceful shutdown

Nova includes built-in graceful shutdown support for safe deployments. When the BEAM VM receives a `SIGTERM` signal (or the application is stopped), Nova will:

1. Wait an optional delay for load balancers to stop routing traffic
2. Suspend the Cowboy listener (stop accepting new connections)
3. Wait for in-flight requests to complete
4. Stop the listener

This ensures that active requests are served before the node exits.

## Configuration

The following parameters can be set under the `nova` key in your `sys.config`:

| Key | Description | Default |
|-----|-------------|---------|
| `shutdown_delay` | Milliseconds to wait before suspending the listener. Gives load balancers time to remove the node from their routing pool. | `0` |
| `shutdown_drain_timeout` | Maximum milliseconds to wait for active connections to finish after the listener is suspended. | `15000` |

Example configuration:

```erlang
{nova, [
    {bootstrap_application, my_app},
    {shutdown_delay, 5000},
    {shutdown_drain_timeout, 15000}
]}
```

## Kubernetes deployments

When deploying Nova to Kubernetes, the shutdown sequence interacts with the pod lifecycle:

1. A new pod starts and passes its readiness probe
2. Kubernetes sends `SIGTERM` to the old pod
3. **In parallel**: the pod is removed from Service endpoints and the BEAM receives the signal

There is a propagation delay (typically 1-5 seconds) between Kubernetes sending `SIGTERM` and all load balancers/proxies updating their routing tables. During this window, new requests can still arrive at the shutting-down pod.

### Recommended settings

Set `shutdown_delay` to cover the propagation window:

```erlang
{nova, [
    {bootstrap_application, my_app},
    {shutdown_delay, 5000},
    {shutdown_drain_timeout, 15000}
]}
```

Set `terminationGracePeriodSeconds` in your pod spec higher than the sum of `shutdown_delay` and `shutdown_drain_timeout` to avoid a `SIGKILL` before drain completes:

```yaml
spec:
  terminationGracePeriodSeconds: 30
  containers:
    - name: my_app
      # ...
```

> #### Important
>
> Without `shutdown_delay`, you may see occasional 502 errors during rolling deployments because requests reach a pod that has already stopped accepting connections.

### Health probes

If you use readiness probes, your health endpoint should reflect the application state. During shutdown, the endpoint should return an error status so Kubernetes stops routing traffic sooner. The [nova_resilience](https://github.com/novaframework/nova_resilience) library provides a health gate that automatically returns 503 during shutdown.

## How it works

Nova implements graceful shutdown in `nova_app:prep_stop/1`, which is called by OTP before the supervision tree is terminated. The sequence is:

1. **Delay** — Sleep for `shutdown_delay` milliseconds. During this time, the listener is still active and serving requests normally. This covers the load balancer propagation window.
2. **Suspend** — Call `ranch:suspend_listener(nova_listener)` to stop accepting new TCP connections. Existing connections continue to be served.
3. **Drain** — Poll `ranch:info/1` every 500ms until active connections reach zero or `shutdown_drain_timeout` is exceeded.
4. **Stop** — Call `cowboy:stop_listener(nova_listener)` to fully shut down the listener.

After `prep_stop` returns, OTP proceeds with the normal supervision tree shutdown.
