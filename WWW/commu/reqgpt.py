import os
import openai
import sys
import json

# 读取 R 传入的 JSON 格式的输入
input_data = sys.stdin.read()
params = json.loads(input_data)

# 配置 API Key
openai.api_key = params.get("api_key")  # 允许从传入参数获取 API Key
openai.base_url = "https://free.v36.cm/v1/"  # API 代理地址

# 发送 OpenAI 请求
completion = openai.chat.completions.create(
    model=params.get("model", "gpt-3.5-turbo"),
    messages=params.get("messages", [{"role": "user", "content": "Hello world!"}]),
    temperature=params.get("temperature", 0.7)
)

# 获取 API 响应
response_text = completion.choices[0].message.content

# 返回 JSON 格式结果
print(json.dumps({"response": response_text}))