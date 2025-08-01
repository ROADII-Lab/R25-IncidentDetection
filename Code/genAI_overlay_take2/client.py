from mcp_use import MCPClient, MCPAgent
import asyncio
from dotenv import load_dotenv
import os
#import openai
#from langchain_openai import ChatOpenAI
from langchain_openai import AzureChatOpenAI

async def main():
    # Load environment variables from .env file
    load_dotenv() 

    # Load api key from environment
    AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
    # Assign the endpoint environmental variable
    os.environ["AZURE_OPENAI_ENDPOINT"] = "http://10.75.42.137:4000"

    # Configuration dictionary for custom MCP servers
    config = {
    "mcpServers": {
        "info-server": {
            "command": "/Users/Andrew.Breck/.local/bin/uv",  # or 'uvicorn' if installed globally
            "args": [
                "--directory",
                "/Users/Andrew.Breck/Documents/GitHub/R25-IncidentDetection/Code/genAI_overlay_take2",
                "run",
                "server.py"
            ],
            "env": {
                "DISPLAY": ":1"
            }
        }
        }
    }

    '''config = {
    "mcpServers": {
        "csv_server": {
        "command": "/Users/Andrew.Breck/Anaconda3/condabin/conda",
        "args": [
            "run",
            "-n",
            "track3",
            "python",
            os.path.join(os.getcwd(), "server.py")
        ]
        }
    }
    }'''
    
    '''config = {
    "mcpServers": {
        "csv_server": {
        "command": "/Users/Andrew.Breck/Anaconda3/condabin/conda",
        "args": [
            "run",
            "-n",
            "track3",
            "--no-capture-output",
            "mcp",
            "run",
            os.path.join(os.getcwd(), "server.py")
        ],
        "version": "0.1.0"
        }
    }
    }'''

    # Create MCPClient from configuration dictionary
    client = MCPClient.from_dict(config)

    # Create LLM
    llm = AzureChatOpenAI(
    azure_deployment="GPT-4.1-nano",  # or your deployment
    api_version="2025-04-01-preview",  # or your api version
    api_key = AZURE_OPENAI_API_KEY,
    azure_endpoint = "http://10.75.42.137:4000/",
    temperature=0,
    max_tokens=None,
    timeout=None,
    max_retries=2,
    streaming = True
    # other params...
    )

    # Create agent with the client and the llm defined above
    agent = MCPAgent(llm=llm, client=client, max_steps=30)

    # Run the query
    result = await agent.run(
        "what is the average value for the Prob_Crash column in the file called 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv'?")
    print(f"\nResult: {result}")

if __name__ == "__main__":
    asyncio.run(main())