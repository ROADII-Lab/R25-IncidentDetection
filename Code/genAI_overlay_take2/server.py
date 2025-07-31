#from mcp.server.fastmcp import FastMCP
import pandas as pd
import os
from fastmcp import FastMCP
import json
import traceback

####

# Base directory where data live
#DATA_DIR = Path(__file__).resolve().parent.parent / "data"
DATA_DIR = os.getcwd()
file_path = os.path.join(DATA_DIR, 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv')
df = pd.read_csv(file_path)

####

# Create an MCP server
mcp = FastMCP("csv_server")

@mcp.tool()
def execute_python(code: str) -> str:
    """
    Executes the provided code string in a sandboxed dict namespace
    where `df` is preloaded DataFrame. Expects user code to
    assign the final result to a name called `output`.
    Returns json.dumps(output) or an error message.
    Requires that the code assign the final answer to a variable named output.
    Catch exceptions and return them to surface errors back to the model/user.
    """
    # Prepare a sandboxed globals/locals
    sandbox_globals = {"pd": pd}
    sandbox_locals = {"df": df, "output": None}

    try:
        # Execute the user's code
        exec(code, sandbox_globals, sandbox_locals)

        # Take the 'output' variable and turn into JSON
        result = sandbox_locals.get("output", None)
        return json.dumps(result, default=str)

    except Exception as e:
        tb = traceback.format_exc()
        return json.dumps({
            "error": str(e),
            "traceback": tb
        })

@mcp.tool()
def read_csv_summary(filename: str) -> str:
    """
    Read a CSV file and return a simple summary.
    Args:
        filename: Name of the CSV file (e.g. 'sample.csv')
    Returns:
        A string describing the file's contents.
    """
    file_path = os.path.join(DATA_DIR, filename)
    df = pd.read_csv(file_path)
    return f"CSV file '{filename}' has {len(df)} rows and {len(df.columns)} columns."

# Add an addition tool
@mcp.tool()
def add(a: int, b: int) -> int:
    """Add two numbers"""
    return a + b

# Add a dynamic greeting resource
@mcp.resource("greeting://{name}")
def get_greeting(name: str) -> str:
    """Get a personalized greeting"""
    return f"Hello, {name}!"

'''# server.py
from fastmcp import FastMCP

mcp = FastMCP("Demo 🚀")

@mcp.tool
def add(a: int, b: int) -> int:
    """Add two numbers"""
    return a + b'''

if __name__ == "__main__":
    mcp.run()