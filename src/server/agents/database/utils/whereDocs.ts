export const whereConditionDocsSummary = `
Supported WHERE conditions (capability-only cheat sheet)

WHAT YOU CAN FILTER BY
• Numbers: equals, not equals, lt, lte, gt, gte, in [list]
• Strings: equals, not equals, in [list], contains (case-sensitive), contains_insensitive (case-insensitive)
• Booleans: equals, not
• DateTime: equals, not, lt (before), lte, gt (after), gte
• Null checks: is null / is not null (via equals null / not null)
• List membership: in with non-empty arrays (strings for string columns, numbers for numeric)

TEMPORAL PHRASE HANDLING (NL → date range translation)
• “last N days|weeks|months”: lower bound at 00:00:00.000Z exactly N units before NOW (gte only)
• “last week/month/year”: previous full ISO week (Mon-Sun UTC), calendar month, or calendar year; inclusive bounds (>= start and <= end at 23:59:59.999Z)
• “on YYYY-MM-DD”: full UTC day (>= start-of-day and <= end-of-day)
• “between A and B” (inclusive): missing time defaults to 00:00:00.000Z for A and 23:59:59.999Z for B
• Explicit times are used as given (assumed UTC unless specified)

RELATION-BASED FILTERS
To-one relations:
• Match a related record by one of its columns (is …)
• Exclude a related record (isNot …)
• No related record present (is: {})

To-many relations:
• some: at least one related row matches the condition
• every: all related rows match (vacuous truth: if none exist, still true)
• none: no related row matches the condition
• No related rows at all: none: {}

Patterns:
• “Has any children”: some with any valid child-column predicate
• “No children at all”: none: {}
• “All children satisfy X” and must exist: combine some (presence) + every (universality)

COMBINATION NOTES
• Combine conditions with AND / OR (no logical NOT group; use scalar not operator instead)
• Each relation condition targets exactly one scalar column on the related table

NOT SUPPORTED 
• Regex or arbitrary functions
• Free-text fuzzy search beyond contains / contains_insensitive
• Custom time zones for date phrases (UTC/Z assumed unless specified)
`;
