# KV over HTTP

A scalable, fault tolerant riak core application is only useful if things can
talk to it. Because many clients speak HTTP, it's worth looking at how to wire
up a Riak Core app to a HTTP server or web application.

## Exercise

(If you're building up YakDB, try not to get stuck in the rabbit hole of tacking
HTTP into your app: borrow liberally from the reference implementation)

* Add a route to handle "fetch".

See git tag http-kv-complete if you'd like a reference.
