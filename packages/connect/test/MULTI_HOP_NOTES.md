Multi-hop aliasing test notes

Current join tests use the real shared schema (orders -> products -> reviews) to
validate shared-hop dedupe behavior and the original production failure.

Why multi-hop aliasing still matters
- The bug only appears when the same table is joined twice via different keys,
  and a later hop uses one of those aliases as an intermediate hop.
- Example shape: base.primary_mid_id -> mid, base.secondary_mid_id -> mid,
  then mid -> end. The second hop must bind to the correct mid alias.

How to add a multi-hop aliasing test
- Create a small fixture schema with two foreign keys to the same table and a
  follow-on hop (base -> mid [primary/secondary] -> end).
- Put it in a dedicated join test file and compare results to a canonical query
  that uses explicit SQL aliases (no regex join counting).
