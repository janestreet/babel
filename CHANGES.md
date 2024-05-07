## Release v0.17.0

* Caller: Various list-returning and list-accepting functions now guarantee they have at least one element via [Nonempty_list]
* Caller: Added additional [Or_error.t]s to correspond to connection-level issues
* Caller: Removed [metadata] parameters as they are now computed as part of the [Connection]
* Direct Stream Writer: Added [store_last_value_and_send_on_add] functionality to [Group]
* Pipe_extended: Added, allows for batched [map]ping of values.
