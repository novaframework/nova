# Security policy

## Supported versions

Only the latest minor release of Nova on
[Hex](https://hex.pm/packages/nova) gets security fixes. A fix lands in a
new release of that line, not in older ones. Upgrade to the latest release
before reporting.

## Reporting a vulnerability

Do not report a vulnerability in a public issue, pull request or discussion.

Report it through GitHub's private vulnerability reporting, which is enabled
for this repository:

1. Go to the [Security tab](https://github.com/novaframework/nova/security) of the repository.
2. Choose **Report a vulnerability**.
3. Describe the problem, the versions it affects and how to reproduce it.

This is the only channel for reports. There is no address to mail them to.

The report is only visible to the maintainers and to you. GitHub describes
the process in
[Privately reporting a security vulnerability](https://docs.github.com/en/code-security/security-advisories/guidance-on-reporting-and-writing-information-about-vulnerabilities/privately-reporting-a-security-vulnerability).

A useful report has:

* the Nova version and the Erlang/OTP version,
* the configuration that is needed, such as the plugins and the route options involved,
* a request or a small application that shows the problem,
* what an attacker gains from it.

## What happens next

The maintainers look at the report, work on a fix in a private advisory and
publish the advisory together with the release that has the fix.

## Scope

This policy covers the `nova` package. Report problems in other packages of
the Nova ecosystem in their own repositories. Problems in Cowboy, ErlyDTL or
Erlang/OTP are reported to those projects.

The [security guide](guides/security.md) describes how to
configure a Nova application securely.
