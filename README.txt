The following are not currently supported, with no plan to support them for now:
  Face down attack position monsters
  A card being in a hand other than that of its owner.

The following are other invariants that should be maintained, and so anything that doesn't fit is a bug:
  Cards in the grave shouldn't ever be face down.
  A card should never be in the grave or banished zone of a non-owner.

Big TODOs:
  System for triggering events. Trap Hole will probably be the first card to leverage that.

