from openai import OpenAI
import streamlit as st
from dotenv import load_dotenv
import os
from langchain_openai import AzureChatOpenAI

from mcp_use import MCPClient, MCPAgent
import asyncio

from langchain.chains import create_history_aware_retriever
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder


# Load environment variables from .env file
load_dotenv() 

# Load api key from environment
AZURE_OPENAI_API_KEY = os.getenv("AZURE_OPENAI_API_KEY")
# Assign the endpoint environmental variable
os.environ["AZURE_OPENAI_ENDPOINT"] = "http://10.75.42.137:4000"

if "openai_model" not in st.session_state:
    st.session_state["openai_model"] = "GPT-4.1-nano"


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

# Create MCPClient from configuration dictionary
client = MCPClient.from_dict(config)

llm = AzureChatOpenAI(
azure_deployment = st.session_state["openai_model"], 
api_version="2025-04-01-preview",  # or your api version
api_key = AZURE_OPENAI_API_KEY,
azure_endpoint = "http://10.75.42.137:4000/",
temperature=0,
max_tokens=1024,
timeout=None,
max_retries=2,
streaming = True
# other params...
)

# Create agent with the client and the llm defined above
agent = MCPAgent(llm=llm, client=client, max_steps=30)

# -------------------
# Async wrapper for Streamlit
# -------------------
async def ask_question_async(input: str):
    result = await agent.run(input)
    return result

def ask_question(input: str):
    # Use asyncio.run to safely run async function
    return asyncio.run(ask_question_async(input))

# -------------------

#
# Streamlit UI Title
st.set_page_config(page_title="Chatbot Interpretation of Model Results") 
st.title("Chatbot Interpretation of Model Results")

if "messages" not in st.session_state:
    st.session_state.messages = []

for message in st.session_state.messages:
    with st.chat_message(message["role"]):
        st.markdown(message["content"])

if prompt := st.chat_input("Ask a question"):
    st.session_state.messages.append({"role": "user", "content": prompt})
    with st.chat_message("user"):
        st.markdown(prompt)

    # Flatten list of messages into a single string
    messages_str = "\n".join(m["content"] for m in st.session_state.messages)

    with st.chat_message("assistant"):
        result = ask_question(input=messages_str)

        st.markdown(result)

    st.session_state.messages.append({"role": "assistant", "content": result})   