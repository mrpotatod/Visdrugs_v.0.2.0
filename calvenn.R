
list_to_binary_matrix<-function(mylist=NULL){
# 1. 拍平成两列（集合名 + 元素）
df <- enframe(mylist, name = "Set", value = "Element") %>%
  unnest(Element)

# 2. 转为二进制存在-与否矩阵
binary_matrix <- df %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = Set, values_from = value, values_fill = 0)
return(binary_matrix)
}
# 
# minpub<-0
# maxadr=20
# input_est=0
# input_low=0
# # 存入列表
# i=1
process_drug_list<-function(Drug_list=Drug_list,minpub="",maxadr="",input_est=NULL,input_low=NULL,USRID=USRID,seevenn=TRUE){
  if(seevenn==FALSE){
    c<-vector()
    for (i in 1:length(Drug_list)){
      if(minpub==""){minpub<-3}
      c<-c(c,Drug_list[[i]]$groupname[1])
      colnames(Drug_list[[i]])[c(1,8)]<-c("Reaction",Drug_list[[i]]$groupname[1])
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]]$Frequency>=as.numeric(minpub),]
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]][8]>input_est,]
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]]$low>input_low,]
      
      Drug_list[[i]]<-Drug_list[[i]][,c(1,3,8)]
      Drug_list[[i]]<-Drug_list[[i]][order(Drug_list[[i]][,2],decreasing = TRUE),]
    }
    
    binary_matrix<-list_to_binary_matrix(lapply(Drug_list, function(x) x$Reaction))
    colnames(binary_matrix)[-1]<-sapply(Drug_list[as.numeric(colnames(binary_matrix)[-1])], function(x) x$groupname[1])
    write.table(binary_matrix,paste0("WWW/temp/",USRID,"_venn_binary_matrix.xls"),sep = "\t")

    if(maxadr==""){maxadr<-20}
    filtered_df_list <- lapply(Drug_list, function(df) {
      head(df,maxadr)
    })
    # 绘制 Venn 图
    venn_data <- lapply(Drug_list, function(x) x$Reaction)
    names(venn_data)<-c
    # 计算韦恩分析的交集信息
    venn_processed <- process_data(ggVennDiagram::Venn(venn_data))
    
    # 提取交集数据
    venn_table <- venn_processed$regionData
    venn_table$item<-sapply(venn_table$item,function(x) paste(x,collapse = ", "))
    # 打印表格
    write.table(venn_table,paste0("WWW/temp/",USRID,"_venn.xls"),sep = "\t")
    filtered_df_list <- Filter(function(x) nrow(x) > 0, filtered_df_list)
    return(filtered_df_list) 
  }else{
    c<-vector()
    for (i in 1:length(Drug_list)){
      if(minpub==""){minpub<-3}
      c<-c(c,Drug_list[[i]]$groupname[1])
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]]$Frequency>=as.numeric(minpub),]
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]][8]>input_est,]
      Drug_list[[i]]<-Drug_list[[i]][Drug_list[[i]]$low>input_low,]
      colnames(Drug_list[[i]])[c(1,8)]<-c("Reaction",Drug_list[[i]]$groupname[1])
      Drug_list[[i]]<-Drug_list[[i]][,c(1,3,8)]
      Drug_list[[i]]<-Drug_list[[i]][order(Drug_list[[i]][,2],decreasing = TRUE),]
    }

    binary_matrix<-list_to_binary_matrix(lapply(Drug_list, function(x) x$Reaction))
    colnames(binary_matrix)[-1]<-sapply(Drug_list[as.numeric(colnames(binary_matrix)[-1])], function(x) x$groupname[1])
    write.table(binary_matrix,paste0("WWW/temp/",USRID,"_venn_binary_matrix.xls"),sep = "\t")
    
    reaction_sets <- lapply(Drug_list, function(df) df$Reaction)
    reaction_sets<-unlist(reaction_sets)%>%table()%>%data.frame()
    reaction_sets<-reaction_sets$.[reaction_sets$Freq>=2]%>%as.character()
    # 找出至少出现 2 次的 Reaction（可改为3次以筛选更严格）

    
    # 过滤原始 Drug_list，仅保留重复 Reaction 行
    if(maxadr==""){maxadr<-20}
    filtered_df_list <- lapply(Drug_list, function(df) {
      df2<-df[df$Reaction %in% reaction_sets, ]
      head(df2,maxadr)
    })
    
    
     # 绘制 Venn 图
    venn_data <- lapply(Drug_list, function(x) x$Reaction)
    names(venn_data)<-c
    # 计算韦恩分析的交集信息
    venn_processed <- process_data(ggVennDiagram::Venn(venn_data))

    # 提取交集数据
    venn_table <- venn_processed$regionData
    venn_table$item<-sapply(venn_table$item,function(x) paste(x,collapse = ", "))
    # 打印表格
    write.table(venn_table,paste0("WWW/temp/",USRID,"_venn.xls"),sep = "\t")
    filtered_df_list <- Filter(function(x) nrow(x) > 0, filtered_df_list)
    return(filtered_df_list)
}



# 1️⃣ 获取每个 df 中的 Reaction 列，并统计重复出现的情况

}
