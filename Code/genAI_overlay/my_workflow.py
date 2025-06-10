from dify_sdk.workflow import Workflow, ModelContextProtocol
from dify_sdk.schemas import FunctionSpec
from executor import execute_python

python_exec_spec = FunctionSpec(
    name="execute_python",
    description=(
        "Executes a snippet of Python code against a preloaded DataFrame `df`. "
        "The code must set its result into a variable named `output`. "
        "Returns the JSON-serialized `output` or an error message."
    ),
    parameters={
        "type": "object",
        "properties": {
            "code": {
                "type": "string",
                "description": (
                    "A Python code snippet. You can reference `df` as the CSV data. "
                    "Assign your final result to `output`, e.g. `output = df[df.x > 10].to_dict(orient='records')`."
                )
            }
        },
        "required": ["code"]
    }
)

class CsvCodeWorkflow(Workflow):
    def __init__(self):
        super().__init__()
        self.register_function(python_exec_spec, execute_python)

    def run(self, mcp: ModelContextProtocol, user_input: str) -> str:
        # 1) Kick off with user question
        mcp.send_message("user", user_input)

        # 2) Get LLM response or function call
        msg = mcp.next()
        if msg.is_function_call and msg.function_name == "execute_python":
            # 3) Extract the code the model generated
            code_snippet = msg.arguments["code"]

            # 4) Execute it
            exec_result = mcp.call_function("execute_python", {"code": code_snippet})

            # 5) Give LLM the result and ask for final answer
            mcp.send_message("function", exec_result, name="execute_python")
            final = mcp.next()
            return final.content

        # If the model answered directly without using the tool
        return msg.content

'''# Example of testing locally
if __name__ == "__main__":
    wf = CsvCodeWorkflow()
    question = "Show me the three rows where value > 50, sorted by name."
    print(wf.run(user_input=question))'''