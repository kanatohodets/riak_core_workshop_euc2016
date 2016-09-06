# Coordinated reads/writes

So far we have a fairly terrible service: a single node failure will result in
permanently lost data. In order to avoid that, we need to write our data to
more places. In terms of Riak Core, "multiple places" means "multiple vnodes".

# Does that help protect from a hardware failure?
"basically yes" -- Riak Core takes care to put sequential vnodes on different
physical nodes when possible, so writing a piece of data to 2+ vnodes results
in fault tolerance (at the expense of cluster capacity).


