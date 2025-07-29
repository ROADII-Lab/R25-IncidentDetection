from mcp_use import MCPClient, MCPAgent
import asyncio
from dotenv import load_dotenv
import os

async def main():
    # Load environment variables from .env file
    load_dotenv() 

    # Load api key from environment
    OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")

    # Configuration dictionary for custom MCP servers
    '''config = {
    "mcpServers": {
        "info-server": {
            "command": "/Users/yuvrajfirodiya/.local/bin/uv",  # or 'uvicorn' if installed globally
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
    }'''

    config = {
    "mcpServers": {
        "csv_server": {
        "command": "C:/Users/Andrew.Breck/Anaconda3/condabin/conda.bat",
        "args": [
            "run",
            "-n",
            "track3_v2",
            "python",
            os.path.join(os.getcwd(), "server.py")
        ]
        }
    }
    }
    
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
        "tell me about the file with the filename of 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv' ",
    )
    print(f"\nResult: {result}")

if __name__ == "__main__":
    asyncio.run(main())