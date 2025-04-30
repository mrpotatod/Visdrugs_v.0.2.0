# 安装 & 加载 reticulate
# install.packages("reticulate")
library(reticulate)

# 设置 Python 解释器（可选）
# use_python("C:/path/to/python.exe", required=TRUE)  
# use_virtualenv("myenv")  # 如果你在虚拟环境里运行 Python
# use_condaenv("myenv")  # 如果你使用 conda 环境

reqncbi<-function(keywords_list=keywords_list){
# 加载 Python 文件 reqncbi.py

reqncbi <- import_from_path("reqncbi", path="WWW/commu/")
# 调用 Python 代码查询 PubMed
pubmed_counts <- reqncbi$get_pubmed_count(keywords_list)

# 打印 Python 返回的结果
return(pubmed_counts)
}
