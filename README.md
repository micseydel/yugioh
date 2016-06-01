### The following are not currently supported, with no plan to support them for now:
* Face down attack position monsters
* A card being in a hand other than that of its owner.

### The following are other invariants that should be maintained, and so anything that doesn't fit is a bug:
* Cards in the grave shouldn't ever be face down.
* A card should never be in the grave or banished zone of a non-owner.

### Big short to mid-term TODOs:
* Battle.
* Support for a trap like Trap Hole. Event system needs some more sophistication on this front.
* Tribute summons need to be enforced properly; currently, tribute summons require one or more tributes, without further specificity.
* Documentation

