import streamlit as st 
import os 
import tempfile 
from litellm import completion, embedding
from dotenv import load_dotenv, find_dotenv
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain.chains import RetrievalQA
from langchain_community.vectorstores import Chroma
from langchain_community.document_loaders import PyPDFLoader
from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings

########################################### Instructions #############################
# Put your key in the .env file
# >conda activate track3
# Change the directory to this code's directory
# >streamlit run app.py 
###################################################################################### 

# Load the .env file
load_dotenv(find_dotenv())
# Access the Azure API key from the environment variable
OPENAI_API_KEY  = os.getenv("AZURE_API_KEY")
# Set the Azure endpoint
os.environ["AZURE_API_BASE"] = "http://10.75.42.137:4000" 
# Set the OpenAI model's release date
os.environ["AZURE_API_VERSION"] = "2025-04-01-preview"

# Load PDF and split into chunks, generare embeddings, and stuff into the Chroma DB
def load_and_index_pdf(pdf_path: str, api_key: str, persist_directory: str = "chroma_db"):
    # Load PDF and split into text chunks
    loader = PyPDFLoader(pdf_path)
    documents = loader.load()

    splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
    chunks = splitter.split_documents(documents)

    embeddings = AzureOpenAIEmbeddings(
        azure_deployment = "azure/text-embedding-3-large",
        openai_api_key = api_key,
        azure_endpoint = os.environ["AZURE_API_BASE"],
        openai_api_version = os.environ["AZURE_API_VERSION"]
    )

    vectordb = Chroma.from_documents(chunks, embedding=embeddings, persist_directory=persist_directory)
    return vectordb

# Streamlit UI Title
st.set_page_config(page_title="ChatGPT-style RAG App") 
st.title("ChatGPT-style RAG App")

# Drag and drop
uploaded_files = st.file_uploader("Drag and drop PDF files here", type="pdf", accept_multiple_files=True) 
if uploaded_files: 
    for uploaded_file in uploaded_files:    
        with tempfile.NamedTemporaryFile(delete=False, suffix='.pdf') as tmp_file: 
            tmp_file.write(uploaded_file.read())
            tmp_path = tmp_file.name 
            loader = PyPDFLoader(tmp_path)
            documents = loader.load()

            splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
            chunks = splitter.split_documents(documents)

            embeddings = AzureOpenAIEmbeddings(
                azure_deployment = "azure/text-embedding-3-large",
                openai_api_key = OPENAI_API_KEY,
                azure_endpoint = os.environ["AZURE_API_BASE"],
                openai_api_version = os.environ["AZURE_API_VERSION"]
            )

            vectordb = Chroma.from_documents(chunks, embedding=embeddings)
    
            st.success(f"Loaded from the PDF.")

# Retrieve the vector database and submit a question.
def ask_question(question: str, api_key: str) -> str:
    # Retrieve the vector DB - return the top 4 documents
    retriever = vectordb.as_retriever(search_kwargs={"k": 4})
        
    # Use GPT-4o via LangChain
    llm = AzureChatOpenAI(
        azure_deployment = "GPT-4.1",
        api_key = api_key,
        azure_endpoint = os.environ["AZURE_API_BASE"],
        api_version = os.environ["AZURE_API_VERSION"]
    )
    
    # Setup Retrieval-QA Chain
    qa_chain = RetrievalQA.from_chain_type(llm=llm, retriever=retriever, return_source_documents=False)
    
    # Ask the LLM directly (i.e., no embeddings)
    if False: 
        messages=[
                {"role": "system", "content": "You are a helpful assistant that can can answer questions"},
                {
                    "role": "user",
                    "content": {question}
                }
            ]
        response = llm.invoke(messages)

    result = qa_chain.invoke({"query": question})
    return result

prompt = st.text_area("Ask a question") 
if st.button("Submit") and prompt: 
    result = ask_question(question=prompt, api_key=OPENAI_API_KEY)['result']
    st.markdown("### Response") 
    st.write(result)