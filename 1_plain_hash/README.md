# Simple Hash

This is the simplest possible Riak Core application. It provides a single API,
`simple_hash_service:ping/0`, which will ping a semi-randomly chosen vnode (based on the hash of the current timestamp) in the hash ring.

See the language specific READMEs for how to run.

## Coding exercises in this workshop

There are two ways you can tackle coding in this workshop

1. Start each exercise fresh, using the reference implementation provided in
   the exercise directory for your language.
2. Build up the base project, YakDB, using the reference implementations as
   a fallback or guide.

If typing out code helps you learn, or you want to have a deeper understanding
on how things fit together, I suggest going with #2. If you learn better by
tinkering with existing code, go with approach #1.

## Exercise

Build a second service command, `other_ping`, and have it hash something other
than the time.

* Add a new exported function to the Service
* Add the backing implementation in the Vnode
