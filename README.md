## Sadly nowhere near ready yet

The idea is this'll be a Scala GraphQL client that uses code generation (remember that?!) to
produce strongly typed GraphQL calls. I can't really see how else you'd do it given
that GraphQL lets you select as much or as little from a type as you want.

Currently there are parsers for queries and (most) of the parts of a schema,
the idea is then to write code to stitch the resulting data together to produce scala files
containing the unique types used by each GraphQL query.


