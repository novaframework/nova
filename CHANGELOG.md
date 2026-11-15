# Changelog

All notable changes to this project will be documented in this file.

## [unreleased]

### Bug Fixes

- *(deps)* Bump cowboy 2.20, cowlib 2.21, ranch 2.3
- *(deps)* Require cowboy ~> 2.20 and OTP 27
- Treat a malformed Cookie header as no session cookie
- Keep the client's session cookie when the Cookie header does not parse
- *(deps)* Pin cowboy 2.20.0 exactly
- *(deps)* Bump cowboy 2.21.0, cowlib 2.22.0, ranch 2.4.0
- *(nova_routing_trie)* Distinguish normalized output routes from input routes
- Keep request data out of crash logs and error pages
- *(session)* Safe session cookie, crypto ids, no fixation, renew/1
- *(session)* Read use_sessions with the bootstrap app first
- *(session)* Check presented ids in the request process, notify only from it
- *(session)* Sweep when session_max_age is set later, convert old rows
- Redact secrets by key in crash logs and error pages
- Log an unformattable stacktrace frame as a redacted report
- *(deps)* Bump cowboy to 2.20.0, cowlib to 2.21.0 and ranch to 2.3.0
- *(session)* Refuse an invalid session cookie name at boot, never loop on it
- Refuse an empty or non-binary session cookie name at boot
- *(deps)* Keep cowboy 2.18, cowlib 2.19 and ranch 2.2
- *(session)* Check the cookie name and attributes without relying on cowlib
- *(session)* Valid_cookie/2 returns false for a name that is not a binary
- *(session)* Never set or expire the cookie when the Cookie header does not parse
- *(redact)* Match sensitive keys by fragment, not by exact name
- Accept OTP json as json_lib and render the default 404/500 bodies
- Handle every documented plugin return and call stop/1
- Handler crashes on redirects, json order, terminate and odd returns
- Multipart max_parts, static mimetypes and per-route plugin_strategy
- *(cors)* Echo allowed origins and answer preflights properly
- *(plugins)* Treat break as a deprecated stop and send every reply
- Escape crash details and show them only in dev
- *(router)* Let the nova_apps plugin_strategy win over the route entry
- *(cors)* Answer only real preflights and append to vary
- Parse Accept in linear time and show crash details only in explicit dev
- Match request body content types case-insensitively
- Redact plugin results in logs and always give the 500 page crash_info
- Send static files with nosniff and keep document types octet-stream
- *(static)* Refuse dot segments in wildcard pathinfo
- Redact controller crashes and format map stacktrace frames
- Read the json tuple order from the fourth element and warn once
- Cap the request body nova_request_plugin reads
- Share one redirect location helper and handle invalid chardata
- Refuse redirect targets with control bytes or invalid UTF-8
- Give body reads a deadline and lower the default body cap to 1 MB
- Check controller redirect targets and keep userinfo out of logs
- Close the connection after a body 408 or 413
- *(router)* Warn once when a route entry sets plugin_strategy
- *(plugins)* Warn once per module when a plugin returns {stop, Req}
- *(redact)* Match sensitive keys by fragment, not by exact name
- Render 500 when a Cowboy handler's init crashes
- Strip call arguments from telemetry exception stacktraces
- Replace req in controller telemetry metadata with request fields
- Report {Status, Reason} of any class as a controller stop
- Emit only the tag of the websocket close reason
- Report invalid as the plugin result for unsupported returns
- *(telemetry)* Export only the tag of an exception reason
- *(telemetry)* Keep the raw path and the peer out of controller metadata by default
- *(telemetry)* Report error and a two-element stop as plugin results
- *(router)* Keep the security callback a mounted application declares
- *(static)* Keep files served from a directory route inside it
- *(router)* Compile a new routing table when a listener starts
- *(router)* Serve catch-all routes for every kind of handler
- Deny the request when security is misconfigured or a plugin crashes
- *(router)* Refuse a route method that is not an HTTP method
- *(static)* Answer every Range header without crashing
- *(router)* Let the mounting application win a route clash
- *(csrf)* Match excluded paths on the path that is routed
- Fail the boot when the listener cannot be started
- *(plugins)* Initialise a plugin that is first used at runtime
- *(router)* Render status pages from the listener that got the request
- *(websocket)* Run the websocket plugins a route declares
- *(logger)* Keep the JSON formatter working on values that are not UTF-8
- *(correlation)* Only take a correlation id made of visible ASCII
- *(watcher)* Start a watcher without moving the node's working directory
- *(router)* Name the static path that does not exist
- Supervise the pubsub scope
- Lowercase origins without cowboy_bstr in the websocket origin plugin
- Delete spooled uploads with the request and bound the spool
- Answer a refused websocket upgrade with the 500 status page
- Run post_ws_connection plugins without a terminate callback
- Pass extra_state and the route template to websocket routes
- Leave the request alone when a controller returns a bare status
- Do not render a status page again after it failed
- Let a CORS preflight reach the plugins of a method-restricted route
- Answer unparsable forms and query strings with a 4xx in the request plugin
- Treat a Cowboy request error as the client's fault
- Call a plugin given as a module with the arity plugins have
- Name the use_sessions option in the log line about disabled sessions
- Bound the ETS session table and read it without the server
- Keep registered handlers when nova_handlers is restarted
- Forget the plugins of an earlier run when nova starts
- List the applications of every listener in nova:get_apps/0
- Send a 204 or a 304 without the body a controller gave it
- Drop the reply of a post_request plugin instead of crashing on it
- Render the 500 page when the security callback crashes
- Spool knows a file by one name and refuses a request it cannot read
- Start again when the listener of an earlier nova_sup is still running
- *(router)* Accept any HTTP method written as a binary or a string
- *(websocket)* Check the origin of a websocket opened over HTTP/2
- *(router)* Stop the boot on a security value that is not a callback
- *(router)* Log a static path that does not exist as an error
- *(static)* Send the whole file for a Range that is not valid or has an If-Range
- *(static)* Read the numbers of a Range header in linear time
- *(static)* Serve a file from the root it was checked against
- *(logger)* Bound the size of a value printed as an Erlang term
- *(router)* Keep override_secure in force for the applications a mounted application mounts
- *(router)* Run the secure of a mount and the security of a route group, and need both to allow
- Run the plugins listed after security once, when every callback has allowed
- Serve a single static file from where it was compiled, and a lost directory with 404
- Show the data of the mount's secure to the group's callback on a websocket route
- Check the security values an override_secure replaces
- Look an application up by name in the url tag without making an atom
- Name a view or a router module without making an atom for every name
- Format the stack frame of a fun, and return ok from use_stacktrace(false)
- Do not answer a request again once its response has begun
- Mount the application of a listener with the entry it wrote for itself
- Warn about a security key written where the router does not read it
- Answer a 500 without an accept header with JSON, as a 404 is
- Name the real stacktrace setting in the warning of the error page
- Describe the options of nova_request_plugin as they work
- Let the specs of nova_session say that a value is any term
- Give a route the extra_state of its route group when it sets none
- Keep the content type of a view when its headers option names none
- Ask the stream handler whether a response began before an error page
- Never answer a request a second time on the normal path
- Answer 404 for a static file that is gone
- Refuse watcher arguments that are not strings
- Give every 500 of Nova's own page the body the client accepts
- Say how to turn stacktraces on when a crash page has none
- Negotiate the body of the default 404 and 500 pages
- Set the session cookie on a response with a binary status
- Find the router of an Elixir application named as OTP names it
- Let a route take an empty extra_state from its route group
- Log a static root that is gone when a file under it is asked for
- Log the correlation id under an atom key
- Drop a second answer in the stream handler instead of asking before each
- Give render_error_pages one meaning, read in one place
- *(router)* Refuse a security callback under a key that is not read there
- *(router)* Keep secure and group security in force for the applications below
- Refuse an application two nova_apps entries mount with different security
- *(session)* Let requests without a cookie neither fill the store nor empty it
- *(multipart)* Answer an upload that stalls or trickles with a 408
- *(multipart)* Count spooled files across the node only when asked to
- *(jsonlogger)* Redact the arguments of a format string before rendering them
- *(ws)* Stop the connection when the handler cannot take a controller's return
- Take {redirect_local, Path} from a security callback and a plugin
- *(static)* Send nosniff with the whole file an invalid range gets
- Send a controller return for another protocol to the fallback
- Answer a view without a template with a 500
- *(router)* Hand the data of a status page to its controller
- *(spec)* Type a handler's return as nova_handler takes it
- *(spec)* Register_handler/2 returns ok
- *(spec)* Delete/1 and delete/2 of nova_session return {ok, Req}
- *(session)* Ask the backend about a presented id on every write
- *(session)* Convert rows with an expiry on a hot upgrade
- *(router)* One callback written as a fun and as {Mod, Fun} is the same security
- *(websocket)* Missing handler message on one line
- *(stream)* One warning a second for responses that are dropped
- *(error pages)* No content-type on the empty answer to a client that accepts neither form
- *(multipart)* No spooled file is left behind by a request that ends inside the rename
- *(session)* A max_sessions that holds no session stops the boot
- *(status pages)* The page of a failed controller gets the status in its request
- *(status pages)* A failing page the router asked for runs once
- *(router)* The request of a status page is typed as the router hands it over
- *(once)* The cell of per_interval/2 is taken from the store as it is
- *(csrf)* A token that decodes short is refused, not a crash
- *(csrf)* No shape of csrf_secret puts a secret in the boot report
- *(origin)* Same_host takes a Host header that names the default port
- *(router)* Plugins_before_security that names no plugin stops the boot
- *(router)* Allowed_origins of a plugin is checked with the routes
- *(websocket)* The Origin of an upgrade is checked before the route runs
- *(once)* First/1 and per_interval/2 keep apart what they store
- *(router)* A preflight to a route without OPTIONS runs pre_security/4 alone
- *(router)* A path that does not start with a slash is refused
- *(types)* The changes to the settings and origins pass eqwalizer
- *(csrf)* Any origin of the nova environment is for websockets alone
- *(csrf)* The plugin on a node without sessions stops the boot
- *(session)* The id a visitor was handed is kept by the first write
- *(security)* Auth_data is what the security callback returned
- *(router)* The notice about preflights names a plugin wherever it is listed
- *(origin)* Same_host of a request over TLS needs an https origin
- *(request)* A body that is not JSON is logged without the body
- *(json)* An object that has a key twice does not decode
- *(session)* A signed id is the characters Nova handed out
- *(session)* A session that was deleted is not brought back
- *(websocket)* An upgrade the browser says is from elsewhere is refused
- *(router)* What is said about a body behind the callback has no blind spots
- *(settings)* A secret that plainly is not random stops the boot

### Documentation

- *(session)* Fixation defence, multi-node behaviour and config rule
- Note that nova:format_stacktrace/1 always returns an integer arity
- *(session)* Spell out the Secure default and sign-outs with in-memory sessions
- *(session)* Say an unset environment gets a Secure cookie
- Plugin returns, status-code routes, pg pubsub and nova_websocket
- Deprecations, CORS preflights, plugin state and websocket plugins
- Behaviour notes for plugin replies, error details and body types
- Note that nova:format_stacktrace/1 always returns an integer arity
- List the behaviour changes of this branch in deprecations.md
- *(routing)* Show plugin_strategy in nova_apps and when the route entry form works
- Mark {stop, Req} from a plugin as deprecated
- Deprecation notes for route-entry plugin_strategy, {stop, Req} and +json bodies
- *(configuration)* An unset environment shows no crash details
- *(websocket)* Describe what a refused upgrade and websocket plugins really do
- *(plugins)* Use the supported json tuple order in the multipart example
- *(cors)* A preflight only reaches the plugin on a route that accepts OPTIONS
- *(handlers)* Keep {Module, Function} handler registration deprecated
- Correct the telemetry guide and tidy the telemetry tests
- *(multi-app)* Document how mount and route group prefixes combine
- *(handlers)* Give a long-lived response its own idle timeout
- *(routing)* Keep the websocket notes with the route format
- Say when the post_ws_connection plugins run
- Correct the comment on who owns the listener table
- Link the websocket init callback so ex_doc resolves it
- *(websocket)* Say what same_host compares on
- *(deprecations)* List the behaviour changes since the last release
- *(pubsub)* Say that a restarted scope has no subscribers
- *(plugins)* Describe the order a request passes plugins and security in
- Add security guide and security policy
- Add websockets, testing, error handling, logging and request lifecycle guides
- Version the deprecations and add an upgrading guide
- Fix README route example, install snippet and dead links
- Rewrite quick start and list every rebar3_nova command
- Correct controllers, handlers and views guides
- Correct routing and plugins guides
- Correct configuration, sessions, pubsub and watchers guides
- Update release guide, reading list and guide index
- Keep Cowboy handlers in the list of nova_route readers
- Describe what happens to a response that has begun
- Say how the default error pages follow the accept header
- Describe the nova_apps entry an application writes for itself
- Say that a security key that is not read is logged
- Say that a route takes the extra_state of its route group
- Update the headers option of a view and the url tag
- Say that a session value is any term
- Say that auth_data is in the upgrade request of a websocket
- Say when a lost static directory is logged
- Say what a missing router module does to the start
- List the metadata options of nova_jsonlogger
- Add the merged fixes to the upgrade and deprecation notes
- Rewrap the plugin_strategy paragraph
- Show a controller that exports its action in the view example
- Say that a request a controller answered itself is answered once
- Describe how the default pages negotiate and what render_error_pages turns off
- List the log messages of own replies, lost static files and the stacktrace hint
- Update lost static paths, the Elixir router name and refused watchers
- Bring the upgrade and deprecation notes in line with the merged fixes
- Document the router, routing table and controller behaviour
- Document the session callbacks and the ETS backend
- Document the plugin behaviour and the bundled plugins
- Document the multipart handler behaviour and its handlers
- Document the websocket behaviour and the handlers
- Document the remaining types and functions of the public modules
- Hide the modules applications do not call
- *(session)* Say in the callback that set_value/3 gets any term
- Bring in-source docs in line with the merged fixes
- Describe error pages, dropped second answers and missing templates
- Say that a binary status body is sent without a content type
- Describe provisional sessions and a full session store
- Describe security across mounted applications and misplaced keys
- Say what add_routes/2 throws for a misplaced security callback
- Describe chunk_timeout, the spool defaults and redirect_local in plugins
- Say what a flood can still do to sessions and uploads
- Update log messages, redaction and configuration defaults
- Add the merged fixes to the upgrade notes
- Name private vulnerability reporting as the channel and the supported line
- Add the changelog generated from the release tags
- Add a contributing guide
- Link the contributing guide from the README
- *(csrf)* Hyphens in the option list
- *(session)* What client_id/1 gives a request whose Cookie header does not parse
- *(origin)* A list of allowed origins has to hold the origin of the site
- *(session)* The session cookie is also handed out by client_id/1
- *(configuration)* The settings checked at boot, allowed origins and prod as the unset environment
- *(plugins)* Pre_security/4, the order around the security callback and the two checks of the CSRF plugin
- *(routing)* Paths that are not canonical are refused, and the security callback runs before the body is read
- *(sessions)* Client_id/1, the values of max_sessions and when session_exists/1 is asked
- *(websockets)* The origin check runs on every websocket route
- *(security)* Defaults at a glance, the checks behind a TLS proxy and what is redacted
- *(errors)* What the request of a status page holds, and the 400 for a path that is not canonical
- *(telemetry)* The pre_security callback label and status_page for pages the router asks for
- *(logging)* The messages of the settings, the plugin order and the unset environment
- *(lifecycle)* The steps of a request with pre_security/4 ahead of the security callback
- *(upgrading)* One table of the defaults that changed, who they affect and how to go back
- *(deprecations)* The defaults that changed in the next release
- *(releases)* Csrf_secret and allowed_origins in the configuration of a release
- *(environment)* Development needs environment set to dev
- *(views)* Every response gets another csrf_token
- *(testing)* What to cover for the origin checks and the plugin order
- *(changelog)* The changes since 0.18.3
- *(configuration)* An unset environment is prod
- *(deprecations)* The note on JSON keys names no internal module
- *(guides)* Sign-in, exact ids, ended sessions and the new notices are in the guides
- *(configuration)* Secret_key_base, trusted_origins, csrf_token and the settings that are refused
- *(plugins)* Security_reads_body, the order notices, the secret of the CSRF tokens and what auth_data holds
- *(routing)* Security_reads_body and trusted_origins in a route group
- *(sessions)* Sign_in/3, signed ids and a session that has ended
- *(websockets)* The headers the origin check reads, and a proxy that terminates TLS
- *(security)* Signing in, the secret of the node and the origin checks behind a proxy
- *(logging)* The messages of the renamed settings, the order notices and a refused request
- *(lifecycle)* What auth_data holds after the security callback
- *(multi-app)* The secure of an entry runs before the body is read
- *(upgrading)* The rows for sign-in, signed ids, the secret and the origin checks
- *(deprecations)* Signing in and out, the renamed settings and the origin checks
- *(releases)* Secret_key_base and trusted_origins in the configuration of a release
- *(testing)* What to cover for the sign-in and security_reads_body
- *(sessions)* The session cookie is named __Host-session_id outside dev

### Features

- Warn at boot when the session manager lacks session_exists/1
- *(session)* Safer cookie defaults and a configurable cookie name
- Plugin reply headers as a list, ok takes only reply, break warns once
- Decode application/*+json bodies in nova_request_plugin
- Expose the matched route template as nova_route in Req
- Emit telemetry events for controllers, plugins, websockets and pubsub
- Mark error-page controller spans with status_page
- *(websocket)* Connection options from init/1 and an opt-in origin check
- Let listed plugins run after the security callback
- Add redirect_local for redirect targets taken from the request
- Let nova_jsonlogger redact what it formats
- Warn at boot when the nova environment is not set
- *(router)* Name the application whose route lost a clash
- *(websocket)* Same_origin for the origin check, which compares the scheme too
- *(security)* Let a route carry more than one security callback
- Say at boot that an application is compiled with its own nova_apps entry
- An unset environment is prod
- *(plugins)* The security callback runs before pre_request plugins
- *(router)* A request path that is not canonical is answered 400
- *(websocket)* The Origin of an upgrade is checked on every websocket route
- *(json)* Encode and decode with OTP json by default and drop thoas
- *(session)* Client_id/1 gives the session id a client holds without a session being stored
- *(csrf)* Signed tokens that store nothing when csrf_secret is set
- *(csrf)* The token of a session is masked anew in every response
- *(csrf)* A request that changes state is refused when the browser says another site sent it
- *(router)* A route group is warned about what its plugin order leaves out
- *(settings)* One secret for the node, a key for every purpose
- *(router)* A route group says whether its security callback reads the body
- *(session)* Sign_in/3 changes the id and stores who signed in
- *(session)* The session cookie is named __Host-session_id outside dev

### Miscellaneous

- Merge fix/bump-cowboy-2.20 (cowboy 2.21.0)
- Merge pr/424 (routing trie route_out type)
- Merge fix/session-security (session hardening)
- Spec the bounded body read and keep the location test well typed
- Type the body read deadline for eqwalizer
- Merge fix/book-findings (book findings)
- Merge feat/route-template-telemetry (route template telemetry)
- Merge fix/router-security-and-static-paths (router security and static paths)
- Merge fix/router-security-and-static-paths (review fixes)
- Group guides and modules in ex_doc and list the new guides
- Tell ELP which rebar profile to read
- Regroup the modules of the API reference
- Take Cowboy 2.21 patch releases without a Nova release
- Deprecate nova_basic_handler:handle_websocket/3
- Add a git-cliff configuration for the changelog

### Performance

- *(static)* Resolve the root of a static directory when the route is compiled
- *(session)* Ask the backend about a presented session id once per request
- *(multipart)* Keep the spooled files of a request with their owner

### Refactor

- Build every crash_info through nova_redact:crash_info/3
- *(session)* Merge cookie options with guarded clauses
- *(session)* Tell the stream handler through cowboy_req:cast/2
- Read json_lib through a shared nova_config lookup
- *(error)* Read the accept header once and trim to an empty binary
- Drop redundant guards from telemetry result and reason tags
- Name the listener child after what the supervisor starts
- Give the type checker what it needs on the changed lines
- Narrow the terms the type checker could not follow
- *(router)* Keep the dispatch key on the request
- Keep the exports Nova makes for itself out of the documentation
- *(websocket)* Name the connection options with a Nova type
- Export the functions the tests call by name instead of export_all
- Drop includes nothing uses and an old-style catch in a test
- Match on maps and build them with comprehensions where the linter asks
- Render Nova's error page through the one call into a compiled template
- Give nova_jsonlogger the types its code already takes
- Make the specs of the handlers cover what they take and return
- Type what nova_plugin_manager stores as the plugin gave it
- Type what the router reads from the env and gives back for a status page
- Type the state nova_stream_h hands to the next stream handler
- Narrow what nova_watcher reads from the env and passes to a port
- Say which static payloads static_state/2 takes
- Drop the clause of preferred_type/1 no caller reaches
- Call the request key of the stream handler nova_stream
- Keep what is logged once in one helper
- Offer dispatch_keys/0 from nova_router
- *(router)* Put the message of a log report under msg
- *(handler)* Answered/1 is no longer exported
- *(logger)* Type the arguments of a format string as the list they are
- *(websocket)* The allowed origins are one setting in a module of their own
- *(settings)* One module checks the node settings when Nova starts
- *(types)* Specs for the private functions of the router and the CSRF plugin
- *(router)* Plugins_after_security is gone
- *(origin)* Allowed_origins is trusted_origins
- *(types)* What the secret, a JSON error and a route group option are is told to the type checker

### Testing

- Type the cookie test helpers for eqwalizer
- Cover the cowboy and cowlib advisories against a real listener
- Read the dev error page through a typed helper
- Read set-cookie headers through a helper for eqwalizer
- *(session)* An invalid cookie setting raises without nova_stream_h on any cowlib
- A malformed Cookie header does not crash nova_stream_h or log the request
- Type the cookie header test helpers for eqwalizer
- Type the cookie test helpers for eqwalizer
- *(session)* Drop the stale cookie mocks from nova_session_test
- Narrow the decoded error page JSON for eqwalizer
- Type the full app suite for eqwalizer and loosen its timing bound
- Build printable redirect targets through a typed helper
- Application/x+json+xml is not decoded as JSON
- Pass setup and cleanup to the new warning tests as fun references
- Keep the first websocket frame out of the handshake read
- Cover route template and telemetry paths in the mutated eunit modules
- Cover a failing plugin on protected websocket and Cowboy handler routes
- *(router)* Keep the static path case with the other add_routes cases
- Render views and run CSRF protection over a real listener
- Run the CORS plugin behind the router over a real listener
- Keep the controllers of the real-listener suites in their own modules
- Export the callbacks the router security tests refer to
- Send an Origin in the CORS suite and expect 204 for a preflight
- Pin that a plugin chain runs once when a plugin fails
- Render Nova's 500 page without a crash map
- Make the mocked cowboy_req:reply refuse what Cowboy refuses
- Move the observer helper of the listener suites to its own module
- A plugin returning break ends the chain and its reply goes out
- Build the binary spool path without an untyped conversion
- A refusing plugin ends the request and leaks no credentials
- Give the type checker and the linter what they need in the added cases
- *(logger)* Read the bounded output with the json module of OTP
- Expect a method given as a string or binary to keep its case in routes/1
- A security callback redirects to a binary location over a listener
- Give eunit fixtures fun references instead of wrapper funs
- Give the tests the types the calls they make ask for
- Match the warning of a disabled stacktrace as a binary
- Remove the static path fixture when the suite ends
- Hand the streamed request back instead of exiting
- Read the logger metadata with a map generator
- Make the directory of a watcher test even when a stopped run left one
- Give eqwalizer two tests it can type
- Give the stalled upload suite a directory of its own
- *(websocket)* Check the one redacted error of a return the handler cannot take
- *(multipart)* Stalled upload over HTTP/2
- *(correlation)* Erase the cached key the test leaves behind
- *(websocket)* Cover the request a controller hands back with the upgrade
- Name the session server and the spool in a way eqwalizer follows
- *(csrf)* Compare what is under the mask in the session cap suite
- *(router)* Put back three tests of the plugin order
- *(once)* Per_interval_test does not depend on 40 ms of scheduling
- One way to pick the port of a test listener
- *(multipart)* The directory of a test is its own and always removed
- *(session)* A hot upgrade keeps every session of the older release
- *(settings)* The boot case about the secret is in the suite

## [0.18.3] - 2026-11-08

### Documentation

- Sharpen the Hex description (#420)

## [0.18.2] - 2026-11-06

### Documentation

- Add context7.json to claim the Context7 library and scope indexing to guides (#421)

## [0.18.0] - 2026-09-11

### Features

- *(multipart)* Add nova_multipart_plugin for multipart/form-data uploads (#412)

## [0.17.0] - 2026-09-08

### Features

- Replace routing_tree with an in-tree routing trie and support multiple listeners (#405)

## [0.16.0] - 2026-08-08

### Features

- *(pubsub)* Allow binary channels, not just atoms (#390)

## [0.15.3] - 2026-08-04

### Bug Fixes

- Splice a list-valued websocket reply into the command list (#400)

## [0.15.2] - 2026-07-30

### Dependencies

- Bump cowboy 2.15.0 -> 2.18.0 (CVE-2026-43966) (#398)

## [0.15.1] - 2026-07-02

### Bug Fixes

- Use SPDX 'Apache-2.0' license identifier for Hex (#392)

## [0.15.0] - 2026-05-26

### Features

- Support nested nova_apps (#380)

## [0.14.3] - 2026-05-16

### Miscellaneous

- Upgrade cowboy 2.15.0 and refresh deps (#384)

## [0.14.1] - 2026-03-25

### Documentation

- Add graceful shutdown guide (#379)

## [0.14.0] - 2026-03-19

### Features

- Add graceful shutdown for k8s deployments (#365)

## [0.13.16] - 2026-03-19

### Refactor

- Use binary format strings in logger calls (#377)

## [0.13.15] - 2026-03-19

### Bug Fixes

- Guard port_close in nova_watcher terminate (#376)

## [0.13.14] - 2026-03-18

### Bug Fixes

- *(test)* Update stale add_routes test to match PR #373 fix (#375)

## [0.13.12] - 2026-03-18

### Testing

- Improve test coverage via mutation testing (#374)

## [0.13.11] - 2026-03-18

### Bug Fixes

- Resolve crashes in add_routes/2 and get_view_name/1 (#373)

## [0.13.6] - 2026-02-13

### Bug Fixes

- Pass fork repo URL to nova_request_app CI (#363)

## [0.13.5] - 2026-02-11

### Bug Fixes

- Remove @doc tags from callback declarations (#355)

## [0.13.3] - 2026-02-11

### Testing

- Add EUnit and PropEr tests for core modules (#357)

## [0.13.2] - 2026-02-11

### Miscellaneous

- Update renovate config with hex datasource and ~> support
- Remove duplicate renovate config from .github/
- Add auto-release GitHub Action (#356)

## [0.13.1] - 2026-02-09

### Bug Fixes

- Handle old plugin info (#341)
- Delete labeler (#353)

### Miscellaneous

- Update GitHub Actions to latest versions and add Renovate (#352)

## [0.13.0] - 2025-09-18

### Bug Fixes

- Added the third print so we have correct arguments (#334)
- Updated dependencies (#338)

## [0.11.0] - 2025-03-21

### Bug Fixes

- Solve that we end up in a loop in error controller (#318)
- This will solve an issue whit cowboy and rebar (#317)
- Correct lock without pmod (#320)

### Miscellaneous

- Added jsonlogger module (#316)

## [0.9.17] - 2024-01-31

### Features

- Dynamically enabled stacktraces (#256)
- Improve documentation (#257)

## [0.9.16] - 2024-01-30

### Bug Fixes

- Fix a case where the controller crashed when stacktrace were missing (#252)

## [0.9.13] - 2023-12-27

### Features

- Configuration for correlation plugin (#245)

## [0.9.12] - 2023-10-28

### Bug Fixes

- Restructured the plugins (#224)

## [0.9.11] - 2023-10-24

### Bug Fixes

- Removed hex publish (#239)

## [0.9.10] - 2023-10-24

### Bug Fixes

- Bumper rebar3 in github actions (#238)

## [0.9.9] - 2023-10-24

### Bug Fixes

- Bump checkout version (#237)

## [0.9.8] - 2023-10-24

### Bug Fixes

- Changed from semver to git (#236)

## [0.9.7] - 2023-10-24

### Bug Fixes

- Now take semver version (#235)

## [0.9.6] - 2023-10-24

### Bug Fixes

- Added sPR title (#225)
- Added needs to job (#229)
- Changed github token (#230)
- Changed steps and workflow (#232)
- Added commit and tag (#234)

## [0.8.5] - 2021-11-25

### Bug Fixes

- Found yer problem
- Include nova.hrl, parse_options.
- Incorrect filename, undid include_lib changes
- Flush options list.
- Separation of concerns, better naming.
- Removed garbage.
- Removed superfluous string conversion.
- Spelling error, dialyzer.

### Features

- Cors plugin, utilities, separate folder
- Ignore crashdump.

