CacheSim
========


This is a simple implementation of a cache simulator. Basically, the simulator
processes an address stream and passes it to the chosen cache implementation, 
so students can see what the hit/missrates are for different cache organisations
and configurations.

The simulator provides three implementation at this point, all of which are defined
in terms of the n-way set associative cache.

- DirectMappedCache
- FullyAssociativeCache
- NWaySetAssociativeCache


