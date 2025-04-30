library(jsonlite)
# library(reticulate)
# use_python("/root/miniconda3/bin/python3", required = TRUE)

reqgpt<-function(message){
  # 设置 API Key（替换为你的 API Key）
  api_key <-readLines("WWW/commu/API_KEY.txt")

  # 定义请求参数
  request_body <- list(
    api_key = api_key,
    model = "gpt-3.5-turbo",
    messages = list(
      list(role = "user", content = message)
    ),
    temperature = 0.2
  )
  
  # 将 R 列表转换为 JSON 格式
  json_input <- toJSON(request_body, auto_unbox = TRUE)
  
  # 调用 Python 文件并传递 JSON 输入
  output <- system2("python", args = "WWW/commu/reqgpt.py", input = json_input, stdout = TRUE)
  
  # 解析 Python 返回的 JSON 响应
  response_data <- fromJSON(output)
  
  # 打印 OpenAI 生成的回答
  response_data$response
}


