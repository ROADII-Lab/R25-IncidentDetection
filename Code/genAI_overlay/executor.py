import pandas as pd
import json
import traceback

# Load CSV once at import time
df = pd.read_csv("data.csv")

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