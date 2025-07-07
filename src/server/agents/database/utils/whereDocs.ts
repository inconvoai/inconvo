export const whereConditionDocs = `# Filters

## Base Filters
You have access to the following base filters:  
\`equals\`, \`not\`, \`in\`, \`notIn\`, \`lt\`, \`lte\`, \`gt\`, \`gte\`.  
These filters apply to columns on the record itself.

## Filter on "-to-many" Relations

You can scope a parent record by the properties of its **many-side** children with the options **\`some\`**, **\`every\`**, and **\`none\`**.

| Natural-language requirement | Prisma option to use |
|--------------------------------------------------------------------|----------------------------------------------|
| “…has **at least one** related record that matches X” | \`some\` |
| “…has **no** related records that match X” | \`none\` |
| “…has **only / all / every** related records that match X (no exceptions)” | \`every\` |


> **Decision rule**  
> • If the wording implies *at least one*, use **\`some\`**.  
> • If the wording implies *exclusivity* (“only”, “all”, “every”), use **\`every\`**.  
> • If the wording is “no / none”, use **\`none\`**.


### Contrast example - spotting the exclusivity cue

**A. “some” (at-least-one semantics)**  
“List users who have at least one order over \$100.”

{
  "relation": "orders",
  "filterOption": "some",
  "column": "total_order_value",
  "operator": "gt",
  "value": 100
}


B. “every” (exclusive semantics)
“List users who's every order is after 2024”
{
  "relation": "orders",
  "filterOption": "every",
  "column": "order_date",
  "operator": "gt",
  "value": "2024-01-01"
}

C. Combined example
Return users with no posts over 100 views and where all posts have ≤ 50 likes:

{ 
  "relation": "posts",
  "filterOption": "every",
  "column": "likes",
  "operator": "lte",
  "value": 50,
}
  
{
  "relation": "posts",
  "filterOption": "none",
  "column": "views",
  "operator": "gt",
  "value": 100
}


## Filter on "-to-one" Relations

Use \`is\` and \`isNot\` to target a single related record.

Example - posts whose author is not Bob and is older than 40:

{
  "relation": "author",
  "filterOption": "isNot",
  "column": "name",
  "operator": "contains_insensitive",
  "value": "Bob"
    
  },
{
  "relation": "author",

  "filterOption": "is",
  "column": "age",
  "operator": "gt",
  "value": 40
} 
## Filter on Absence of "-to-many" Records

Example query to return all users that have zero posts:

{
  "relation": "posts",
  "filterOption": "none",
  "value": {}
  }

## Filter on Absence of "-to-one" Relations

Example query to return all posts that don't have an author relation:

{
  "relation": "author",
  "filterOption": "is",
  "value": {}
}

`;

// Filter tool does not exist
// ## Filter on Presence of Related Records

// Example query to return all users with at least one post:

// {
//   "relation": "posts",
//   "filterOption": "some",
//   "value": {}
// }
