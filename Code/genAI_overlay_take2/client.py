from mcp_use import MCPClient, MCPAgent
import asyncio
from dotenv import load_dotenv
import os
#import openai
#from langchain_openai import ChatOpenAI
from langchain_openai import AzureChatOpenAI

#
import streamlit as st 
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.chains import RetrievalQA
from langchain_community.vectorstores import Chroma
from langchain_community.document_loaders import PyPDFLoader
from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings
#

def main():
    # Load environment variables from .env file
    load_dotenv() 

    # Load api key from environment
    AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
    # Assign the endpoint environmental variable
    os.environ["AZURE_OPENAI_ENDPOINT"] = "http://10.75.42.137:4000"

    '''# Configuration dictionary for custom MCP servers
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
    }'''
    
    '''# Configuration dictionary for custom MCP servers
    config = {
    "mcpServers": {
        "info-server": {
            "command": "/Users/Andrew.Breck/.local/bin/uv",  # or 'uvicorn' if installed globally
            "args": [
                "-y",
                "mcp-remote"
                "http://127.0.0.1:8000/mcp/"
            ],
            "env": {
                "MCP_TRANSPORT_STRATEGY": "http-only"
            }
        }
        }
    }'''

    config = {
        "mcpServers": {
            "http": {
                "url": "http://127.0.0.1:8000/mcp/"
            }
        }
    }

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
    agent = MCPAgent(llm=llm, client=client, max_steps=30, memory_enabled=True)


    # -------------------
    # Async wrapper for Streamlit
    # -------------------
    async def ask_question_async(question: str):
        messages = [
            {"role": "system", "content": "You are a helpful assistant that can answer questions"},
            {"role": "user", "content": question}
        ]
        result = await agent.run(question)
        return result
    
    '''async def stream_agent_output(agent, query):
        """Streams the output of an MCP agent and prints intermediate steps."""
        print(f"Executing query: '{query}'")
        async for chunk in agent.astream(query):
            if "messages" in chunk:
                for message in chunk["messages"]:
                    print(f"Message from agent: {message}")
            if "actions" in chunk:
                for action in chunk["actions"]:
                    print(f"Tool action: {action}")
            if "steps" in chunk:
                for step in chunk["steps"]:
                    print(f"Step: {step}")
            if "output" in chunk:
                print(f"Final output: {chunk['output']}")'''    

    def ask_question(question: str):
        # Use asyncio.run to safely run async function
        return asyncio.run(ask_question_async(question))
    
    '''def ask_question(question: str):
        # Use asyncio.run to safely run async function
        return asyncio.run(stream_agent_output(agent, question))'''    

    # -------------------

    #
    # Streamlit UI Title
    st.set_page_config(page_title="Chatbot Interpretation of Model Results") 
    st.title("Chatbot Interpretation of Model Results")

    prompt = st.text_area("Ask a question") 
    if st.button("Submit") and prompt: 
        result = ask_question(prompt)#['result']
        st.markdown("### Response") 
        st.write(result)

if __name__ == "__main__":
    main()