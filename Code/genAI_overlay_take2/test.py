'''from litellm import completion
import os
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# set ENV variables
AZURE_OPENAI_API_KEY = os.getenv("AZURE_API_KEY")

os.environ["AZURE_OPENAI_API_KEY"] = "sk-fFbciGZkSxj_S3w4QL6fXA"
os.environ["AZURE_API_BASE"] = "http://10.75.42.137:4000" 
os.environ["AZURE_API_VERSION"] = "2025-04-01-preview"

# name the model to use
model = 'azure/GPT-4.1' # 1,047,576 tokens

# azure call
response = completion(
  "azure/<your_deployment_name>",
  messages = [{ "content": "Hello, how are you?","role": "user"}]
)

response = litellm.completion(
    model = model,  # registered name under Azure deployment
    messages=[
        {"role": "system", "content": "You are a helpful assistant that can answer questions."},
        {
            "role": "user",
            "content": "Write a short poem."
        }
    ]
)

print(response)'''

import openai
from dotenv import load_dotenv
import os

load_dotenv()

OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")

client = openai.OpenAI(
    api_key=OPENAI_API_KEY,
    # api_key="sk-fFbciGZkSxj_S3w4QL6fXA",
    base_url="http://10.75.42.137:4000/" 
)

response = client.chat.completions.create(
    model="GPT-4.1-nano", # model to send to the proxy
    messages = [
        {
            "role": "user",
            "content": "Write a short poem."
        }
    ]
)

print(response.choices[0].message.content)
