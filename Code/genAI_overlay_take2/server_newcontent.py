import threading
from io import StringIO
import sys
import base64
import plotly.express as px
import json
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score, mean_squared_error
import os

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

# === MCP Tool Wrappers to register ===

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

# === End of new tools ===

# === Optional: update your analyze_csv prompt or other server hooks to guide usage ===
# Eg: use load_csv -> quick_stats -> correlation_summary -> feature_importance_simple -> plot_groupby_save sequence

# Make sure to add these new tools to your MCP server registration if you override handle_list_tools or similar

# Example snippet in main or your MCP server registration:

# @server.list_tools()
# async def handle_list_tools() -> list[Tool]:
#     # Return your existing tools plus these new ones with descriptions and schemas

# @server.call_tool()
# async def handle_call_tool(name: str, arguments: dict | None) -> ...:
#     # Dispatch calls to the above functions by tool name
