from mcp_use import MCPClient, MCPAgent
import asyncio
from dotenv import load_dotenv
import os
import openai

async def main():
    # Load environment variables from .env file
    load_dotenv() 

    # Load api key from environment
    OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")

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
    '''llm = ChatGroq(
        model_name="Llama3-8b-8192",
        streaming=True
    )'''

    llm = openai.OpenAI(
        api_key=OPENAI_API_KEY,
        base_url="http://10.75.42.137:4000/" 
    )

    # Create agent with the client
    agent = MCPAgent(llm=llm, client=client, max_steps=30)

    # Run the query
    result = await agent.run(
        "add 3 and 5",
    )
    print(f"\nResult: {result}")

if __name__ == "__main__":
    asyncio.run(main())