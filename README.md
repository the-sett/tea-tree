# Tea Trees

The 'tea-tree' data structure is a rose-tree implementation with some additional features that help when working with the Elm update cycle.

In TEA update messages should contain only enough information to perform a 'delta' on the Model to derive a new Model. The model itself, or larger chunks of state from it, should not be captured as they may get stale. Updates are asynchronous, so it is possible to have >1 Cmd in flight at the same time, which are derived from the same version of the Model.

Every node in the tree is assigned a unique id.

* This means that when re-walking a tree to the same position, this can be verified.
* The nodes in the tree can potentially be updated faster, by using Array.map.

A position in the tree can have a Path extracted, which can be used to return to the same point in the tree later. This can be passed with messages attached to Elm Cmds, to associate only a position within a tree, not the data at that position.

* This avoid the stale update problem.
