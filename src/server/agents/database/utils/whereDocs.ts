export const whereConditionDocs = `# Filters

## Base Filters
You have access to the following base filters:  
\`equals\`, \`not\`, \`in\`, \`notIn\`, \`lt\`, \`lte\`, \`gt\`, \`gte\`, \`contains\`, \`contains_insensitive\`.  
These filters can be applied to any column in the table.


# Related Record Filters
## Filter on "-to-many" Relations
You can scope a parent record by the properties of its **many-side** children with the options **\`some\`**, **\`every\`**, and **\`none\`**.

> **Decision rule**  
> • If the wording implies *at least one*, use **\`some\`**.  
> • If the wording implies *exclusivity* (“only”, “all”, “every”), use **\`every\`**.  
> • If the wording is “no / none”, use **\`none\`**.


### Contrast example - spotting the exclusivity cue

**A. “some” (at-least-one semantics)**  
“List users who have at least one order over \$100.”
generateRelationToManyCondition({
  "relation": "orders",
  "filterOption": "some",
  "column": "total_order_value",
  "operator": "gt",
  "value": 100
})


B. “every” (exclusive semantics)
“List users who's every order is after 2024”
generateRelationToManyCondition{
  "relation": "orders",
  "filterOption": "every",
  "column": "order_date",
  "operator": "gt",
  "value": "2024-01-01T00:00:00.000Z"
})

C. Combined example
“List users with no posts over 100 views and where all posts have ≤ 50 likes”

generateRelationToManyCondition({ 
  "relation": "posts",
  "filterOption": "every",
  "column": "likes",
  "operator": "lte",
  "value": 50,
})

generateRelationToManyCondition({
  "relation": "posts",
  "filterOption": "none",
  "column": "views",
  "operator": "gt",
  "value": 100
})


## Filter on "-to-one" Relations
You can scope a parent record by the properties of its **one-side** children with the options **\`is\`** and **\`isNot\`**.
Use \`is\` and \`isNot\` to target a single related record.

Example - posts whose author is not Bob and is older than 40:

generateRelationToOneCondition({
  "relation": "author",
  "filterOption": "isNot",
  "column": "name",
  "operator": "contains_insensitive",
  "value": "Bob"
})

generateRelationToOneCondition({
  "relation": "author",
  "filterOption": "is",
  "column": "age",
  "operator": "gt",
  "value": 40
})

## Filter on Absence of "-to-many" Records
Example query to return all users that have zero posts:

generateRelationToManyCondition({
  "relation": "posts",
  "filterOption": "none",
  "value": {}
})

## Filter on Absence of "-to-one" Relations
Example query to return all posts that don't have an author relation:

generateRelationToOneAbsentCondition({
  "relation": "author",
  "filterOption": "is",
  "value": {}
})

`;

// Filter tool does not exist
// ## Filter on Presence of Related Records

// Example query to return all users with at least one post:

// {
//   "relation": "posts",
//   "filterOption": "some",
//   "value": {}
// }
