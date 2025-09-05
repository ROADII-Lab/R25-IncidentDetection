#from mcp.server.fastmcp import FastMCP
import pandas as pd
import os
import sys
from fastmcp import FastMCP
import json
import traceback
from typing import List, Dict, Optional, Union, Any
from datetime import datetime
import glob

####

# Base directory where data live
#DATA_DIR = Path(__file__).resolve().parent.parent / "data"
DATA_DIR = os.getcwd()
file_path = os.path.join(DATA_DIR, 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv')
df = pd.read_csv(file_path)

####

# Create an MCP server
mcp = FastMCP("file_analysis_server")

# File reading tools
@mcp.tool()
def read_file(path: str) -> str:
    """
    Read and return the contents of a text file.
    
    Args:
        path: Path to the file to read
        
    Returns:
        The file contents as text
    """
    print(f"Attempting to read file: {path}", file=sys.stderr)
    file_path = path
    
    if not os.path.exists(file_path):
        return f"Error: File '{path}' not found."
    
    try:
        with open(file_path, 'r') as f:
            return f.read()
    except Exception as e:
        return f"Error reading file: {str(e)}"

@mcp.tool()
def write_file(path: str, content: str) -> str:
    """
    Write content to a text file.
    
    Args:
        path: Path where the file should be written
        content: Text content to write to the file
        
    Returns:
        Confirmation message
    """
    print(f"Attempting to write file: {path}", file=sys.stderr)
    try:
        file_path = path
        
        # Ensure directory exists
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        
        with open(file_path, 'w') as f:
            f.write(content)
            
        return f"Successfully wrote {len(content)} characters to {path}"
    except Exception as e:
        return f"Error writing file: {str(e)}"

@mcp.tool()
def list_files(directory: str, pattern: str = "*") -> str:
    """
    List files in a directory matching a pattern.
    
    Args:
        directory: Directory to list files from
        pattern: Glob pattern to match files (default: "*")
        
    Returns:
        List of matching files
    """
    print(f"Listing files in: {directory} with pattern: {pattern}", file=sys.stderr)
    try:
        dir_path = directory
        
        if not os.path.isdir(dir_path):
            return f"Error: '{directory}' is not a directory."
        
        matched_files = glob.glob(os.path.join(dir_path, pattern))
        
        if not matched_files:
            return f"No files matching '{pattern}' found in {directory}"
        
        file_list = "\n".join([os.path.basename(f) for f in matched_files])
        return f"Files in {directory} matching '{pattern}':\n{file_list}"
    except Exception as e:
        return f"Error listing files: {str(e)}"

@mcp.tool()
def file_info(path: str) -> str:
    """
    Get information about a file.
    
    Args:
        path: Path to the file
        
    Returns:
        File information (size, creation time, modification time)
    """
    print(f"Getting file info for: {path}", file=sys.stderr)
    try:
        file_path = path
        
        if not os.path.exists(file_path):
            return f"Error: File '{path}' not found."
        
        stat_info = os.stat(file_path)
        
        created = datetime.fromtimestamp(stat_info.st_ctime).strftime('%Y-%m-%d %H:%M:%S')
        modified = datetime.fromtimestamp(stat_info.st_mtime).strftime('%Y-%m-%d %H:%M:%S')
        size_bytes = stat_info.st_size
        
        if size_bytes < 1024:
            size_str = f"{size_bytes} bytes"
        elif size_bytes < 1024 * 1024:
            size_str = f"{size_bytes/1024:.2f} KB"
        else:
            size_str = f"{size_bytes/(1024*1024):.2f} MB"
        
        return f"File: {path}\nSize: {size_str}\nCreated: {created}\nModified: {modified}"
    except Exception as e:
        return f"Error getting file info: {str(e)}"

@mcp.resource("dir://{directory}")
def directory_resource(directory: str) -> str:
    """
    List directory contents as a resource.
    
    Args:
        directory: Directory path
        
    Returns:
        Directory listing
    """
    print(f"Accessing directory resource: {directory}", file=sys.stderr)
    try:
        dir_path = directory
        
        if not os.path.isdir(dir_path):
            return f"Error: '{directory}' is not a directory."
        
        files = os.listdir(dir_path)
        
        result = [f"Directory: {directory}"]
        for file in files:
            full_path = os.path.join(dir_path, file)
            if os.path.isdir(full_path):
                result.append(f"📁 {file}/")
            else:
                size = os.path.getsize(full_path)
                if size < 1024:
                    size_str = f"{size} bytes"
                elif size < 1024 * 1024:
                    size_str = f"{size/1024:.1f} KB"
                else:
                    size_str = f"{size/(1024*1024):.1f} MB"
                result.append(f"📄 {file} ({size_str})")
        
        return "\n".join(result)
    except Exception as e:
        return f"Error accessing directory resource: {str(e)}"

@mcp.tool()
def query_csv(file_name: str, code: str) -> str:
    """
    Executes the provided code string in a sandboxed dict namespace. Expects user code to
    assign the final result to a name called `output`.
    Returns json.dumps(output) or an error message.
    Requires that the code assign the final answer to a variable named output.
    Catch exceptions and return them to surface errors back to the model/user.
    """
    file_path = os.path.join(DATA_DIR, file_name)
    data = pd.read_csv(file_path)

    # Prepare a sandboxed globals/locals
    sandbox_globals = {"pd": pd}
    sandbox_locals = {"data": data, "output": None}

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

# Prompt for .csv file analysis
@mcp.prompt()
def analyze_csv(filename: str) -> str:
    """
    Create a prompt for analyzing a .csv file.
    
    Args:
        filename: filename of the .csv to analyze
    """
    return f"""Please help me derive insights from "{filename}". For this task:

1. Use the query_csv tool to extract information from the .csv file
2. Analyze the 'Probability_of_Crash' column in relation to other columns, namely: 'highway', 'hour', 'maxspeed', 'weekday', 'precipitation', and 'SNOW'.
3. Identify which column is the strongest predictor of 'Probability_of_Crash'
4. For the strongest predictor, how does 'Probability_of_Crash' vary by the values in that column?

You can use the list_files tool to see what's in the directory if the filename is unclear or unspecified, and write_file to create any new files needed."""

'''@mcp.tool()
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
    return f"CSV file '{filename}' has {len(df)} rows and {len(df.columns)} columns."'''

if __name__ == "__main__":
    mcp.run()