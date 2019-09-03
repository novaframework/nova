# 01 - Introduction

## Content

* 01 - Introduction
  * Erlang
  * What is nova?
  * [Guides](./guides)
* [02 - Quick start](02_quick-start.md)
  * Use rebar3 and nova_rebar3
  * nova_admin
* [03 - Routing](03_routing.md)
  * How to create routes
  * Using prefix
  * Secure routing
* [04 - Controllers](04_controllers.md)
  * HTML
  * REST
  * Websockets
* [05 - Views](05_views.md)
* [06 - Building releases](06_building-releases.md)
* [07 - Books and links](07_books-and-links.md)
* [XX - Future work and todos](xx_future-work-and-todos.md)


### Erlang

Nova needs Erlang 21 or later to run. Mainly it is because we use logger that is new in erlang 21.

We will add how you will install Erlang on different machines.

### What is nova?

Nova is a small VC (View/Controller) framework that builds upon [Cowboy](https://github.com/ninenines/cowboy). It's inspired by [Chicago Boss](https://github.com/ChicagoBoss/ChicagoBoss) and also [Phoenix](https://github.com/phoenixframework/phoenix) but tries
to utilize the OTP way a bit more.
