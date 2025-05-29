export const whereConditionDocs = `# Filters

## Base Filters

You have access to the following base filters:
\`equals\`, \`not\`, \`in\`, \`notIn\`, \`lt\`, \`lte\`, \`gt\`, \`gte\`.
These filters can be used to filter records based on the properties of the record itself.

## Filter on "-to-many" Relations

You have access to \`some\`, \`every\`, and \`none\` options to filter records by the properties of related records on the "-to-many" side of the relation.
| Requirement | Query Option to Use |
|--------------------------------------------------------------------|----------------------------------------------|
| "I want a list of every User that has at least one unpublished Post record" | \`some posts are unpublished\` |
| "I want a list of every User that has no unpublished Post records" | \`none of the posts are unpublished\` |
| "I want a list of every User that has only unpublished Post records" | \`every post is unpublished\` |

Example query to return Users with no posts over 100 views and all posts having ≤ 50 likes:

\`\`\`json
{
  "table": "users",
  "operation": "findMany",
  "operationParameters": {},
  "where": {
    "posts": {
      "none": {
        "views": {
          "gt": 100
        }
      },
      "every": {
        "likes": {
          "lte": 50
        }
      }
    }
  }
}
\`\`\`

## Filter on "-to-one" Relations

You can use \`is\` and \`isNot\` options to filter records by the properties of related records on the "-to-one" side of the relation.

Example query to return Posts where the author's name is not Bob and the author is older than 40:

\`\`\`json
{
  "table": "posts",
  "operation": "findMany",
  "operationParameters": {},
  "where": {
    "author": {
      "isNot": {
        "name": "Bob"
      },
      "is": {
        "age": {
          "gt": 40
        }
      }
    }
  }
}
\`\`\`

## Filter on Absence of "-to-many" Records

Example query to return all users that have zero posts:

\`\`\`json
{
  "table": "users",
  "operation": "findMany",
  "operationParameters": {},
  "where": {
    "posts": {
      "none": {}
    }
  }
}
\`\`\`

## Filter on Absence of "-to-one" Relations

Example query to return all posts that don't have an author relation:

\`\`\`json
{
  "table": "posts",
  "operation": "findMany",
  "operationParameters": {},
  "where": {
    "author": {
      "is": {} // or "is": null
    }
  }
}
\`\`\`

## Filter on Presence of Related Records

Example query to return all users with at least one post:

\`\`\`json
{
  "table": "users",
  "operation": "findMany",
  "operationParameters": {},
  "where": {
    "posts": {
      "some": {}
    }
  }
}
\`\`\`
`;
