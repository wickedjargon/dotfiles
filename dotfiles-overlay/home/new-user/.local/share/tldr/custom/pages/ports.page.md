# ports

> Show and free listening network ports. Wraps ss with a readable interface.
> Addresses shown in yellow (0.0.0.0) are reachable from other machines.

- Show everything that is listening:

`ports`

- Show what is on a specific port:

`ports {{3000}}`

- SIGTERM whatever is listening on a port:

`ports kill {{3000}}`

- Machine-parseable list (port, proto, addr, process):

`ports list`

- See owners of root-owned sockets:

`sudo ports`
