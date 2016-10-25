### The following are not currently supported, with no plan to support them for now:
* Face down attack position monsters
* A card being in a hand other than that of its owner.

### The following are other invariants that should be maintained, and so anything that doesn't fit is a bug:
* Cards in the grave shouldn't ever be face down.
* A card should never be in the grave or banished zone of a non-owner.

### Big short to mid-term TODOs:
* Game loop should be mutated by commands issued by game mechanics.
* Support for a trap like Trap Hole. Event system needs some more sophistication on this front.
* Documentation. State machines need visual documentation.
* Commands should have an issuer
* ALL state mutation should be done by commands, and game mechanics should also issue commands.

### Long-term TODOs:
* Undo to commands.
* Design - players should emit commands.
* Gamestate checksum to detect non-progression; should not be dependent on things like deck order.

### Use
* Main class should be independently runnable.
* Can use "sbt publishLocal" to publish for use by a separate project.
