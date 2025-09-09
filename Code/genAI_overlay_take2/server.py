#from mcp.server.fastmcp import FastMCP
import pandas as pd
import os
import sys
from fastmcp import FastMCP
from fastmcp.client.transports import StreamableHttpTransport
import json
import traceback
from typing import List, Dict, Optional, Union, Any
from datetime import datetime
import glob

import threading
from io import StringIO
import plotly.express as px
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error

from mcp.server.models import InitializationOptions
from mcp.types import (
    TextContent,
    Tool,
    Resource,
    INTERNAL_ERROR,
    Prompt,
    PromptArgument,
    EmbeddedResource,
    GetPromptResult,
    PromptMessage,
)
from mcp.shared.exceptions import McpError
from typing import List
import mcp.server.stdio
####

# Base directory where data live
#DATA_DIR = Path(__file__).resolve().parent.parent / "data"
DATA_DIR = os.getcwd()
file_path = os.path.join(DATA_DIR, 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv')
df = pd.read_csv(file_path)

####

# === ScriptRunner Class to manage dataframes and safe executions ===

class ScriptRunner:
    def __init__(self):
        self.data = {}       # dict: name -> DataFrame
        self.df_count = 0
        self.notes = []

    def load_csv(self, csv_path: str, df_name: str = None):
        self.df_count += 1
        if not df_name:
            df_name = f"df_{self.df_count}"
        try:
            df = pd.read_csv(csv_path)
        except Exception as e:
            return {"status": "error", "message": f"Failed to load {csv_path}: {str(e)}"}
        self.data[df_name] = df
        self.notes.append(f"Loaded '{csv_path}' as '{df_name}' with {len(df)} rows, {len(df.columns)} columns")
        return {"status": "ok", "message": f"Loaded '{csv_path}' as '{df_name}'", "df_name": df_name,
                "rows": len(df), "cols": len(df.columns)}

    def safe_exec(self, script: str, allowed_globals=None, timeout_seconds: int = 10, save_to_memory=None):
        if allowed_globals is None:
            allowed_globals = {
                "pd": pd,
                "np": np,
                "RandomForestRegressor": RandomForestRegressor,
                "LinearRegression": LinearRegression,
                "train_test_split": train_test_split,
                "r2_score": r2_score,
                "mean_squared_error": mean_squared_error,
            }
        local_vars = dict(self.data)  # copy current dataframes
        stdout_capture = StringIO()
        old_stdout = sys.stdout
        try:
            sys.stdout = stdout_capture
            # Run code in thread with timeout
            exec_thread = threading.Thread(target=exec, args=(script, allowed_globals, local_vars))
            exec_thread.start()
            exec_thread.join(timeout_seconds)
            if exec_thread.is_alive():
                raise TimeoutError(f"Script execution timed out after {timeout_seconds} seconds")
        except Exception as e:
            return {"status": "error", "message": f"Script error: {str(e)}"}
        finally:
            sys.stdout = old_stdout
        out_text = stdout_capture.getvalue()
        # Save specified dataframes back to memory
        if save_to_memory:
            for dfname in save_to_memory:
                if dfname in local_vars and isinstance(local_vars[dfname], pd.DataFrame):
                    self.data[dfname] = local_vars[dfname]
                    self.notes.append(f"Saved DataFrame '{dfname}' to memory")
        return {"status": "ok", "output": out_text.strip() or "No stdout output"}

# Global instance for this server
script_runner = ScriptRunner()

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

@mcp.tool()
def load_csv(csv_path: str, df_name: str = None) -> str:
    """Load CSV from path into memory under DataFrame name"""
    res = script_runner.load_csv(csv_path, df_name)
    return json.dumps(res)

@mcp.tool()
def run_script(script: str, save_to_memory: list[str] = None) -> str:
    """Run Python script with limited globals, optionally save new DataFrames."""
    # Defensive: sanitize save_to_memory
    save_list = save_to_memory if save_to_memory else []
    res = script_runner.safe_exec(script, save_to_memory=save_list)
    return json.dumps(res)

@mcp.tool()
def quick_stats(df_name: str = "df_1", numeric_cols: list[str] = None, top_n_cats: int = 10) -> str:
    if df_name not in script_runner.data:
        return json.dumps({"status":"error", "message": f"DataFrame '{df_name}' not loaded"})
    df = script_runner.data[df_name]
    result = {"rows": len(df), "cols": len(df.columns)}
    try:
        dtypes = df.dtypes.apply(str).to_dict()
        missing = df.isnull().sum().to_dict()
        result["dtypes"] = dtypes
        result["missing_values_count"] = missing
        numeric = df.select_dtypes(include=["number"])
        if numeric_cols:
            numeric = numeric[numeric_cols]
        result["numeric_summary"] = numeric.describe().to_dict()
        cats = {c: list(df[c].value_counts().head(top_n_cats).items()) for c in df.select_dtypes(include=["object","category"]).columns}
        result["top_categories"] = cats
    except Exception as e:
        return json.dumps({"status":"error", "message": f"Error computing quick stats: {str(e)}"})
    return json.dumps({"status":"ok", "summary": result})

@mcp.tool()
def correlation_summary(df_name: str = "df_1", target: str = "Probability_of_Crash", top_n: int = 10) -> str:
    if df_name not in script_runner.data:
        return json.dumps({"status":"error", "message": f"DataFrame '{df_name}' not loaded"})
    df = script_runner.data[df_name]
    if target not in df.columns:
        return json.dumps({"status":"error", "message": f"Target column '{target}' not in dataframe"})
    try:
        out = {}
        numeric_df = df.select_dtypes(include=["number"])
        if target in numeric_df.columns:
            corrs = numeric_df.corr()[target].drop(target).abs().sort_values(ascending=False).head(top_n)
            out["numeric_correlations_abs"] = corrs.to_dict()
        cat_effects = {}
        for col in df.select_dtypes(include=["object","category"]).columns:
            means = df.groupby(col)[target].mean()
            effect = float(means.max() - means.min()) if not means.empty else 0
            cat_effects[col] = {"min_mean": float(means.min() if not means.empty else 0),
                                "max_mean": float(means.max() if not means.empty else 0),
                                "range": effect}
        out["categorical_effects"] = cat_effects
    except Exception as e:
        return json.dumps({"status":"error", "message": f"Error computing correlations: {str(e)}"})
    return json.dumps({"status":"ok", "correlation_summary": out})

@mcp.tool()
def feature_importance_simple(df_name: str = "df_1", target: str = "Probability_of_Crash", features: list[str] = None, model: str = "rf", max_rows: int = 20000) -> str:
    if df_name not in script_runner.data:
        return json.dumps({"status":"error", "message": f"DataFrame '{df_name}' not loaded"})
    df = script_runner.data[df_name].copy()
    if target not in df.columns:
        return json.dumps({"status":"error", "message": f"Target column '{target}' not found"})

    try:
        # Sample large datasets to keep performance safe
        if len(df) > max_rows:
            df = df.sample(max_rows, random_state=0)

        if features is None:
            candidates = [c for c in df.columns if c != target]
            numeric = df.select_dtypes(include=["number"]).columns.tolist()
            cats = [c for c in candidates if c not in numeric and df[c].nunique() < 100]
            features = numeric + cats

        X = df[features].copy()
        y = df[target]

        # Encode categoricals as int codes, fill missing
        for col in X.select_dtypes(exclude=["number"]).columns:
            X[col] = X[col].astype("category").cat.codes.replace(-1, np.nan)
        X = X.fillna(X.median(numeric_only=True))

        # Train/test split 
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.25, random_state=0)

        if model == "rf":
            estimator = RandomForestRegressor(n_estimators=100, random_state=0, n_jobs=1)
            estimator.fit(X_train, y_train)
            importances = estimator.feature_importances_
            preds = estimator.predict(X_test)
        else:
            # default linear regression
            estimator = LinearRegression()
            estimator.fit(X_train, y_train)
            importances = np.abs(estimator.coef_)
            preds = estimator.predict(X_test)

        imp_dict = dict(zip(X.columns, importances))
        metrics = {"r2": float(r2_score(y_test, preds)), "rmse": float(np.sqrt(mean_squared_error(y_test, preds)))}
        sorted_imp = {k: v for k, v in sorted(imp_dict.items(), key=lambda item: item[1], reverse=True)}

        return json.dumps({"status":"ok", "model": model, "metrics": metrics, "importances": sorted_imp})

    except Exception as e:
        return json.dumps({"status":"error", "message": f"Feature importance error: {str(e)}"})

@mcp.tool()
def plot_groupby_save(df_name: str, group_by_col: str, target: str = "Probability_of_Crash", out_path: str = "plot_groupby.html", sample_n: int = 5000) -> str:
    if df_name not in script_runner.data:
        return json.dumps({"status":"error", "message": "DataFrame not loaded"})
    df = script_runner.data[df_name]
    if group_by_col not in df.columns:
        return json.dumps({"status":"error", "message": f"Column '{group_by_col}' not in dataframe"})
    if target not in df.columns:
        return json.dumps({"status":"error", "message": f"Target '{target}' not in dataframe"})
    try:
        if len(df) > sample_n:
            plot_df = df.sample(sample_n, random_state=0)
        else:
            plot_df = df
        summary = plot_df.groupby(group_by_col)[target].agg(["mean", "count"]).reset_index()
        fig = px.bar(summary, x=group_by_col, y="mean", labels={"mean": f"Mean {target}"})
        fig.update_layout(title=f"{target} by {group_by_col}", xaxis_tickangle=-45)

        # Ensure directory exists
        out_dir = os.path.dirname(out_path) or "."
        os.makedirs(out_dir, exist_ok=True)

        fig.write_html(out_path, include_plotlyjs="cdn")
        return json.dumps({"status":"ok", "filepath": out_path, "rows_used": len(plot_df)})
    except Exception as e:
        return json.dumps({"status":"error", "message": f"Plotting error: {str(e)}"})

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

# --- TOOL, PROMPT, RESOURCE HANDLERS for MCP Server ---

######################
# Tools Handling
######################

@mcp.server.list_tools()
async def list_tools() -> List[Tool]:
    # Return all tools your server supports
    return [
        Tool(name="read_file", description="Read and return the contents of a text file"),
        Tool(name="write_file", description="Write content to a file"),
        Tool(name="list_files", description="List files in a directory"),
        Tool(name="file_info", description="Get information about a file"),
        Tool(name="query_csv", description="Run user code on CSV data and return JSON"),
        # New analysis tools
        Tool(name="load_csv", description="Load CSV file into internal memory"),
        Tool(name="run_script", description="Execute Python analysis script on loaded data"),
        Tool(name="quick_stats", description="Quick statistics summary of a DataFrame"),
        Tool(name="correlation_summary", description="Correlation and categorical effect analysis"),
        Tool(name="feature_importance_simple", description="Simple feature importance via ML models"),
        Tool(name="plot_groupby_save", description="Plot grouped means of target, save to HTML file"),
    ]

@mcp.server.call_tool()
async def call_tool(name: str, arguments: dict | None) -> List[TextContent | EmbeddedResource]:
    # Map tool name to your function, call it and wrap result as TextContent for MCP response
    try:
        arguments = arguments or {}
        if name == "read_file":
            result = read_file(**arguments)
        elif name == "write_file":
            result = write_file(**arguments)
        elif name == "list_files":
            result = list_files(**arguments)
        elif name == "file_info":
            result = file_info(**arguments)
        elif name == "query_csv":
            result = query_csv(**arguments)
        elif name == "load_csv":
            result = load_csv(**arguments)
        elif name == "run_script":
            result = run_script(**arguments)
        elif name == "quick_stats":
            result = quick_stats(**arguments)
        elif name == "correlation_summary":
            result = correlation_summary(**arguments)
        elif name == "feature_importance_simple":
            result = feature_importance_simple(**arguments)
        elif name == "plot_groupby_save":
            result = plot_groupby_save(**arguments)
        else:
            raise McpError(INTERNAL_ERROR, f"Unknown tool: {name}")
        # Return text results wrapped properly
        return [TextContent(type="text", text=result)]
    except Exception as e:
        raise McpError(INTERNAL_ERROR, f"Exception while running tool '{name}': {str(e)}")

######################
# Prompt Handling
######################

@mcp.server.list_prompts()
async def list_prompts() -> List[Prompt]:
    # Return available prompts
    return [
        Prompt(
            name="analyze_csv",
            description="Prompt to analyze a CSV file with analytical tools",
            arguments=[
                PromptArgument(
                    name="filename",
                    description="CSV filename to analyze",
                    required=True,
                )
            ],
        )
    ]

@mcp.server.get_prompt()
async def get_prompt(name: str, arguments: dict | None) -> GetPromptResult:
    # Provide prompt content to model on request
    if name != "analyze_csv":
        raise ValueError(f"Unknown prompt: {name}")
    if not arguments or "filename" not in arguments:
        raise ValueError("Missing required argument: filename")
    filename = arguments["filename"]
    prompt_text = f"""
Please help me derive insights from the CSV file '{filename}'. Use the following analytical tools available on the server: load_csv, quick_stats, correlation_summary, feature_importance_simple, and plot_groupby_save.

Start by loading the CSV file. Then provide:
1. An overview summary of data statistics.
2. Correlation and categorical analyses with respect to 'Probability_of_Crash'.
3. Feature importance ranking showing the strongest predictors.
4. A grouped mean bar chart for the strongest predictor variable.

Limit outputs to concise JSON results, and save charts as HTML files. Respond step-by-step using the tools. 
"""
    return GetPromptResult(
        description=f"Analyze CSV file: {filename}",
        messages=[
            PromptMessage(role="user", content=TextContent(type="text", text=prompt_text.strip()))
        ],
    )

######################
# Resource Handling ##
######################

@mcp.server.list_resources()
async def list_resources() -> List[Resource]:
    # Adjust or extend resources here as needed
    return [
        Resource(
            uri="dir://./",
            name="Current Working Directory",
            description="Lists files in the current working directory",
            mimeType="text/plain",
        ),
        Resource(
            uri="data-exploration://notes",
            name="Data Exploration Notes",
            description="Notes generated by the data exploration scripts",
            mimeType="text/plain",
        ),
    ]

@mcp.server.read_resource()
async def read_resource(uri: str) -> str:
    # Simple dispatcher for your resources
    if uri.startswith("dir://"):
        directory = uri[len("dir://"):]
        return directory_resource(directory)
    elif uri == "data-exploration://notes":
        return "\n".join(script_runner.notes)
    else:
        raise ValueError(f"Unknown resource URI: {uri}")


if __name__ == "__main__":
    mcp.run()
    ## Start an HTTP server on port 8000
    # mcp.run(transport="streamable-http", host="127.0.0.1", port=8000)