/**
 * Python scripts used in the sandbox.
 * Extracted here to keep index.ts cleaner.
 */

export const ANALYZE_JSON_SCRIPT = `import pandas as pd
import json
import sys

def analyze_json_file(file_path):
    """
    Analyze a JSON file and display its structure using pandas.
    Returns the first 5 rows of the normalized DataFrame.
    """
    with open(file_path, 'r') as f:
        data = json.load(f)

    # Normalize JSON data - handle different structures
    if 'data' in data:
        df = pd.json_normalize(data['data'])
    else:
        df = pd.json_normalize(data if isinstance(data, list) else [data])

    # Print first 5 rows
    print(df.head(5).to_string())

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python analyze_json.py <file_path>", file=sys.stderr)
        sys.exit(1)

    file_path = sys.argv[1]
    analyze_json_file(file_path)
`;

/**
 * Inconvo response helper module - written to sandbox before code execution.
 * Provides convenient functions for generating JSON responses.
 */
export const INCONVO_HELPER_MODULE = `"""Inconvo response helpers for sandbox code execution."""
import json

def text(message: str) -> None:
    """Output a text response.

    Args:
        message: The message to display to the user
    """
    print(json.dumps({"type": "text", "message": message}))

def table(df, message: str, columns: list = None) -> None:
    """Output a table response from a DataFrame.

    Args:
        df: pandas DataFrame containing the data
        message: Explanation for the user
        columns: Optional list of columns to include (default: all)
    """
    if columns:
        df = df[columns]

    # Convert all values to strings for consistent JSON output
    body = []
    for row in df.values.tolist():
        body.append([str(v) if v is not None else "" for v in row])

    print(json.dumps({
        "type": "table",
        "message": message,
        "table": {
            "head": [str(c) for c in df.columns.tolist()],
            "body": body
        }
    }))

def chart(alt_chart, message: str) -> None:
    """Output a chart response. MUST pass an Altair Chart object, NOT a dict.

    Args:
        alt_chart: Altair Chart object created with alt.Chart(df).mark_*().encode(...)
        message: Explanation highlighting key insights

    Example:
        import altair as alt
        c = alt.Chart(df).mark_bar().encode(x='category:N', y='value:Q')
        chart(c, "Sales by category")
    """
    import altair as alt
    alt.data_transformers.enable('default', max_rows=None)
    spec = alt_chart.to_dict()

    # Remove $schema if present
    spec.pop('$schema', None)

    # Convert named datasets to inline data.values
    if 'datasets' in spec and 'data' in spec:
        data_name = spec['data'].get('name')
        if data_name and data_name in spec['datasets']:
            spec['data'] = {'values': spec['datasets'][data_name]}
            del spec['datasets']

    print(json.dumps({
        "type": "chart",
        "message": message,
        "spec": spec
    }))
`;
