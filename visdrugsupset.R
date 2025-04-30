# install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)

# CUSRID=123123 
# ndrug=2
visdrugs_upset<-function(USRID=NULL,ndrug=NULL){
ADR_matrix<-read.table(paste0("WWW/temp/",USRID,"_venn_binary_matrix.xls"),sep = "\t")
# 示例数据（电影类型）

dt <- ADR_matrix[,-1]

# 绘图
upsetplot <- ComplexUpset::upset(
  dt,
  intersect = colnames(dt),
  name = "Overlapped reactions",
  base_annotations = list(
    'Intersection size' = intersection_size(
      counts = FALSE
    )
  )
)

ggsave(filename=paste("WWW/temp/",USRID,"_upset.png",sep=""), plot = upsetplot, width = 3500, height = 1500, units = "px", dpi = -17.14 * ndrug + 480,limitsize = FALSE)
resize_image(input_path=paste("WWW/temp/",USRID,"_upset.png",sep=""),output_path=paste("WWW/temp/",USRID,"_upset2.png",sep=""), fixed_width =1300)

}