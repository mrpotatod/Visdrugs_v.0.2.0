try(Drug1<-fdapieplot_hdv(drugs1=toupper(c("Ruxolitinib","Ruxolitinib phosphate")),druggroupname1="Ruxolitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug2<-fdapieplot_hdv(drugs1=toupper(c("Tofacitinib","Tofacitinib citrate")),druggroupname1="Tofacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug3<-fdapieplot_hdv(drugs1=toupper(c("Baricitinib")),druggroupname1="Baricitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug4<-fdapieplot_hdv(drugs1=toupper(c("Upadacitinib","Upadacitinib hemihydrate")),druggroupname1="Upadacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug5<-fdapieplot_hdv(drugs1=toupper(c("Fedratinib","Fedratinib hydrochloride")),druggroupname1="Fedratinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug6<-fdapieplot_hdv(drugs1=toupper(c("Abrocitinib")),druggroupname1="Abrocitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())

try(Drug1<-fdapieplot_hdv(drugs1=toupper(c("Ruxolitinib","Ruxolitinib phosphate")),druggroupname1="Ruxolitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
try(Drug2<-fdapieplot_hdv(drugs1=toupper(c("Tofacitinib","Tofacitinib citrate")),druggroupname1="Tofacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
try(Drug3<-fdapieplot_hdv(drugs1=toupper(c("Baricitinib")),druggroupname1="Baricitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
try(Drug4<-fdapieplot_hdv(drugs1=toupper(c("Upadacitinib","Upadacitinib hemihydrate")),druggroupname1="Upadacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
try(Drug5<-fdapieplot_hdv(drugs1=toupper(c("Fedratinib","Fedratinib hydrochloride")),druggroupname1="Fedratinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
try(Drug6<-fdapieplot_hdv(drugs1=toupper(c("Abrocitinib")),druggroupname1="Abrocitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())

try(Drug1<-fdapieplot_hdv(drugs1=toupper(c("CEFTRIAXONE")),druggroupname1="CEFTRIAXONE",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())
try(Drug2<-fdapieplot_hdv(drugs1=toupper(c("INFLIXIMAB")),druggroupname1="INFLIXIMAB",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=list(R1=c("Death","abortion"),R2=c("Immunodeficiency",	"Immunosuppression")),USRID=NULL)%>%as.data.frame())

try(Drug1<-fdapieplot_hdv(drugs1=toupper(c("CEFTRIAXONE")),druggroupname1="CEFTRIAXONE",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac="Death",USRID=NULL)%>%as.data.frame())
try(Drug2<-fdapieplot_hdv(drugs1=toupper(c("INFLIXIMAB")),druggroupname1="INFLIXIMAB",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac="Death",USRID=NULL)%>%as.data.frame())


Drug_list<-list(Drug1,Drug2)
Drug_list<-list(Drug1,Drug2,Drug3,Drug4,Drug5,Drug6,NULL,NULL)
Drug_list<-Filter(Negate(is.null), Drug_list)
Drug_list2=Drug_list
names(Drug_list2)<-sapply(Drug_list2, function(x) x$groupname[1])
print(Drug_list2)

names(Drug_list2)<-sapply(Drug_list2, function(x) x$groupname[1])
write_xlsx(Drug_list2,paste("WWW/temp/",123123,"_forest3.xlsx",sep=""))

Drug_list<-process_drug_list(Drug_list=Drug_list,minpub=1,maxadr=20,input_est=1,input_low=1,USRID=123123,seevenn=T)
visdrugs_upset(USRID=123123,ndrug=length(Drug_list))
if(nrow(Drug_list[[1]])==0){stop(
  showModal(modalDialog(
    h4("Some drug have too less reaction, please remove the drug or loose filtering settings"),
    easyClose = TRUE,
    footer = modalButton("Close"),
    size = "l"  # 放大对话框
  )))
}


Drug_all<-Drug_list[[1]][,c(1,3)]
Drug_name_all<-Drug_list[[1]][1,2]
if(length(Drug_list)>1){
  for (i in 2:length(Drug_list)){
    Drug_all<-merge(Drug_all,Drug_list[[i]][,c(1,3)],by.x="Reaction",by.y="Reaction",incomparables=0,all = TRUE)
    Drug_name_all<-c(Drug_name_all,Drug_list[[i]][1,2])
  }}
Drug_all[is.na(Drug_all)]<-0
Drug_names<-Drug_name_all

Drug_names<-Drug_names[Drug_names!=""]
print(Drug_names)
colnames(Drug_all)[2:ncol(Drug_all)]<-Drug_names
print("rename Drug_all OK")
#得到两个用于制作网络图的数据drug_data，adr_data
drug_data <- data.frame(
  drug_id = 1:length(Drug_list),
  drug_name = Drug_names,
  drug_color = c("#4E79A7", "#F28E2B", "#59A14F", "#FF9DA7", "#76B7B2", "#E15759", "#9ECAE1", "#B07AA1")[1:length(Drug_list)]  # 药物节点的颜色
)
print("drug_data OK")
# 计算不良反应的大小和颜色
if(ncol(Drug_all)==2){
  adr_data <- Drug_all %>%  mutate(
    ADR_size =log10 (Drug_all[,2]),  # 不良反应的大小由ROR的对数和来计算
    ADR_color = "#4E79A7"
    # 不良反应的颜色由最大ROR的药物决定
  )
}else{
  adr_data <- Drug_all %>%
    mutate(
      ADR_size =apply(Drug_all[, 2:ncol(Drug_all)],1,function(x) log10 (sum(x))),  # 不良反应的大小由ROR的对数和来计算
      ADR_color = apply(Drug_all[, 2:ncol(Drug_all)], 1, function(x) {
        ifelse(max(x) == x[1], "#4E79A7", ifelse(max(x) == x[2], "#F28E2B", ifelse(max(x) == x[3], "#59A14F", ifelse(max(x) == x[4], "#FF9DA7", ifelse(max(x) == x[5], "#76B7B2", ifelse(max(x) == x[6], "#E15759", ifelse(max(x) == x[7], "#9ECAE1", "#B07AA1")))))))
      })  # 不良反应的颜色由最大ROR的药物决定
    )  
} 
print("adr_data OK")
print(adr_data)



  # 构建节点数据
  nodes <- data.frame(
    id = c(drug_data$drug_name, adr_data$Reaction),
    label = c(drug_data$drug_name, adr_data$Reaction),
    size = c(rep(20, length(drug_data$drug_name)), adr_data$ADR_size*2.5),  # 药物节点大小固定，ADR节点大小依据ADR_size
    color = c(drug_data$drug_color, adr_data$ADR_color)  # 药物节点颜色为 lightblue，ADR节点颜色根据ADR_color
  )
  print(nodes)
  # 构建边数据
  edges <- data.frame(
    from = rep(drug_data$drug_name, each = nrow(adr_data)),
    to = rep(adr_data$Reaction, times = nrow(drug_data)),
    value = lapply(adr_data[,2:(ncol(adr_data)-2)],as.numeric)%>%unlist()
  )
  edges$color<-drug_data$drug_color[match(edges$from,drug_data$drug_name)]
  print(edges$value)
  
  
  nodes <- data.frame(id = 1:10, 
                      label = paste("Node", 1:10),                                 # labels
                      group = c("GrA", "GrB"),                                     # groups 
                      value = 1:10,                                                # size 
                      shape = c("square", "triangle", "box", "circle", "dot", "star",
                                "ellipse", "database", "text", "diamond"),         # shape
                      title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip
                      color = c("darkred", "grey", "orange", "darkblue", "purple"),# color
                      shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))                  # shadow
  
  edges <- data.frame(from = sample(1:10,8), to = sample(1:10, 8),
                      label = paste("Edge", 1:8),                                 # labels
                      length = c(100,500),                                        # length
                      arrows = c("to", "from", "middle", "middle;to"),            # arrows
                      dashes = c(TRUE, FALSE),                                    # dashes
                      title = paste("Edge", 1:8),                                 # tooltip
                      smooth = c(FALSE, TRUE),                                    # smooth
                      shadow = c(FALSE, TRUE, FALSE, TRUE))                       # shadow
  
  visNetwork(nodes, edges) 
  
  
  
  # 加载所需包
  library(writexl)
  
  # 假设有 3 个药物数据框
  Drug1 <- data.frame(ID = 1:3, Name = c("A", "B", "C"))
  Drug2 <- data.frame(ID = 4:6, Name = c("D", "E", "F"))
  Drug3 <- NULL  # 假设这个为 NULL，应该被跳过
  
  # 创建一个命名的 list
  drug_list <- list(
    Aspirin = Drug1,
    Tylenol = Drug2,
    EmptyDrug = Drug3
  )
  
  # 过滤掉 NULL 元素
  drug_list_filtered <- drug_list[!sapply(drug_list, is.null)]
  
  # 写入 Excel 文件
  write_xlsx(drug_list_filtered, path = "example_drugs.xlsx")
  
  # 提示
  cat("Excel 文件已保存为 example_drugs.xlsx\n")