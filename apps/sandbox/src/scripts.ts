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

// Note: inconvo.py helper module is now in scripts/inconvo.py and baked into the container via Dockerfile
