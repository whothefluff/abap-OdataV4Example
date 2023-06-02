# OData v4 template

This README file outlines how the code defines models and CRUD based on CDS views.

## Defines model based on CDS views

- Creates entity types with cds name + 'Type'
  - Internal name is cds name + '_TY' so the cds name has to be limited to 27 characters
  - Defines properties from the cds using the name as is (respecting case)
  - Defines edm types, conversions, and tags of properties based on a standard mappings of the ABAP Dictionary
  - Defines properties as key from the cds keys
  - Defines properties as nullable if they are not keys and do not depend on a foreign key either (based on annotation @ObjectModel.foreignKey)
  - Defines properties as etag from cds annotation @Semantics.systemDate.lastChangedAt (only one eTag for entity type is allowed)
- Creates navigations based on exposed cds associations with alias name (respecting case)
  - Defines cardinality from the association cardinality
  - Defines referential constraints from the association on condition (for entities that are not part of a composition and that don't have cardinality to many)
  - Defines contained entities for child entities based on annotation @ObjectModel.association.type and sets cascade delete on deletion of parent
- Creates entities with cds name as is (respecting case)
  - Defines entities only if they have no parent (if they have parents they will be defined as contained entities)
- Creates navigation bindings based on exposed associations
  - Defines navigation bindings for entities that are not part of a composition

## Defines CRUD based on CDS views

- Creates GET of single entities
  - Defines query options $select, $expand
  - Allows retrieval of single property (without $value only)
  - Allows conditional retrieval of property with header If-None-Match and the entity eTag field (the cds field annotated with @Semantics.systemDate.lastChangedAt)
- Creates GET of collections
  - Defines query options $select, $expand, $skip, $top (virtually no limit), $skiptoken (default pagination 1000 entries), $count, $orderby, $deltatoken, $search (reads annotation @ObjectModel.semanticKey and creates three LIKE SQL statements one case-sensitive, one lowercase and one uppercase), $expand
  - *Warning:* combinations of skiptoken + either skip or top are not allowed
  - *Warning:* pagination in version 7.50 of ABAP is not recommended so $top=2147483647 (4 byte int) can and should be used liberally (explicit $top supersedes implicit server-side pagination with $skiptoken)
- Creates CREATE operations
  - ...
- Creates DELETE operations
  - ...
- Creates UPDATE operations
  - ...
- Creates PATCH operations
  - ...
