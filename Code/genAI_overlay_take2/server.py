#from mcp.server.fastmcp import FastMCP
import pandas as pd
import os
from fastmcp import FastMCP

####

# Base directory where data live
#DATA_DIR = Path(__file__).resolve().parent.parent / "data"
DATA_DIR = os.getcwd()

####

# Create an MCP server
mcp = FastMCP("csv_server")

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