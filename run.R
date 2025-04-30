#################################################################
#                          功能汇总代码                         #
#################################################################
library(stringr)
library(dplyr)
#-----------------------------------

# fdapieplot(drugs1="ETANERCEPT",druggroupname1="ETANERCEPT",year=2014:2024,targetreac=c("Death","abortion"),topn=15,minpub=3,odbyror=FALSE,removeindi=FALSE,USRID=NULL)

#show most potential reactions-----------------
fdapieplot<-function(drugs1=NULL,druggroupname1=NULL,year=NULL,targetreac=NULL,topn=15,minpub=3,odbyror=FALSE,removeindi=FALSE,USRID=NULL){
  if(druggroupname1==""){druggroupname1=drugs1[1]%>%str_to_sentence}
  USRID_mpt<-paste(USRID,"_mpt",sep = "")
  drugreaction_1drug<-extractcases(d1=drugs1,d2="OTHER DRUGS",year=year)
  groupres<-groupbynon(drugreaction=drugreaction_1drug)
  drug1pie<-tdte2pie(tdte=drugreaction_1drug$d1alle[,c(1,3)]%>%unique(),druggroupname=druggroupname1,USRID=USRID)
  drug1pie$df$same_as_indi<-"No"
  drug1pie$df$same_as_indi[as.character(tolower(drug1pie$df$Reaction))%in%(drugreaction_1drug$tdallindi$indi_pt%>%table()%>%names())]<-"Yes"

  if(is.null(targetreac)){
    #剔除与indi相同的reaction
    drug1pie_df_Reaction<-drug1pie$df$Reaction
    oridata_fbyreac=drugreaction_1drug$d1alle
    if(removeindi==TRUE){
    drug1pie_df_Reaction<-drug1pie$df$Reaction[drug1pie$df$same_as_indi!="Yes"]
    drug1pie$df2<-drug1pie$df2[drug1pie$df2$Reaction%in%drug1pie_df_Reaction,]
    oridata_fbyreac=drugreaction_1drug$d1alle[drugreaction_1drug$d1alle$pt%in%drug1pie_df_Reaction,]
    }
    
    #在有reaction的情况下，为点击地区饼图提供筛选后的数据
    
    tr=as.character(drug1pie_df_Reaction)
  }else{
    tr=targetreac%>%str_to_sentence()
    
    #剔除与indi相同的reaction
    tgdrug1pie<-drug1pie$df[drug1pie$df$Reaction%in%targetreac,]
    if(nrow(tgdrug1pie)==0){return(NULL)}
    tgdrug1pie$Reaction <- factor(tgdrug1pie$Reaction, levels = tgdrug1pie$Reaction)
    tgdrug1pie$Percentage <- tgdrug1pie$Frequency / sum(tgdrug1pie$Frequency) * 100
    tgdrug1pie$Label <- paste0(round(tgdrug1pie$Percentage, 1), "%")
    tgdrug1pie$Frequency<-as.character(tgdrug1pie$Frequency)
    drug1pie$df2<-tgdrug1pie
    oridata_fbyreac=drugreaction_1drug$d1alle[drugreaction_1drug$d1alle$pt%in%tr,]

  }
  
  drug1pie$df$groupname<-druggroupname1
  df<-drug1pie$df
  df$tdtr<-df$Frequency%>%as.numeric()
  df$tdntr<-nrow(drugreaction_1drug$d1alle)-df$tdtr
  df$ntdtr<-reacount$Count[match(df$Reaction,reacount$Reaction)]%>%as.numeric()-df$tdtr
  df$ntdntr<-nrow(drugreaction_1drug$d2alle)-df$ntdtr
  
  df$Reaction<-as.character(df$Reaction)
  #提供排序选项

  #筛选报道数大于某个值的reaction
  df<-df[df$Frequency>=minpub,]
  #计算ROR
  df2 <- df %>%
    rowwise() %>%
    mutate(
      est = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$est,
      low = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$low,
      hi = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$hi,
      se = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$se
    ) %>%
    ungroup()
  df2$Frequency<-paste(df2$tdtr,"/",df2$tdntr)
  df2$same_as_indi<-paste(df2$ntdtr,"/",df2$ntdntr)
  df2$groupname<-df2$low
  df2$tdtr<-df2$hi
  df2$tdntr<-df2$est
  df2$ntdtr<-df2$se
  df2$ntdntr<-paste(df2$est," (",df2$low," to ",df2$hi,")",sep = "")
  df2$est<-""
  df2<-df2[,c(1:7,9,8)]
  colnames(df2)<-c("Reaction","Drug_of_interest","Other_drugs","low","hi","est","se","                                       ","ROR (95% CI)")
  most_potential_reactions<-df2[df2$Reaction%in%tr,]
  if(odbyror){dfall<-most_potential_reactions[order(most_potential_reactions$est,decreasing = TRUE),]}else{dfall<-most_potential_reactions}
  if(is.null(topn)){dfall<-head(dfall,15)}else{dfall<-head(dfall,topn)}
  dfall<-dfall[order(dfall$est,decreasing = TRUE),]
  cgres2plot(dfall=dfall,druggroupname1=druggroupname1,druggroupname2="Other drugs",USRID=USRID_mpt)
  write.table(most_potential_reactions,paste("WWW/temp/",USRID_mpt,"_forest.xls",sep=""),col.names = TRUE,row.names = FALSE,sep="\t")
  write.table(drugreaction_1drug$d1alle,paste("WWW/temp/",USRID,"_pie.xls",sep=""),col.names = TRUE,row.names = FALSE,sep="\t")
  return(list(piedata=drug1pie$df2, oridata=drugreaction_1drug$d1alle, oridata_fbyreac=oridata_fbyreac,forestdata=dfall))
  }



# drugs1="LAMIVUDINE"
# drugs2="OTHER DRUGS"
# druggroupname1="test"
# # x<-drugreaction_1drug$d1alle
# USRID="ASDASD"
# targetreac="Death"

# draw forest plot for drug vs drug reactions-----------------
ffplot_dvd<-function(drugs1=NULL,drugs2=NULL,year=NULL,USRID=NULL,druggroupname1=NULL,druggroupname2=NULL,targetreac=NULL,subgroup=NULL,age_unit=NULL,age=NULL){
  targetreac=targetreac%>%str_to_sentence()
  USRID_dvd<-paste(USRID,"_dvd",sep = "")
  drugreaction_1drug<-extractcases(d1=drugs1,d2=drugs2,year=year)
  if (subgroup=="age"){
  groupres<-groupbyage(drugreaction=drugreaction_1drug,age_unit=age_unit,age=age)
  dfall<-combagegroupres(groupres=groupres,targetreac=targetreac,age=age)
  }
  if (subgroup=="gender"){
  groupres<-groupbygender(drugreaction=drugreaction_1drug)
  dfall<-combgendergroupres(groupres=groupres,targetreac=targetreac)
  }
  if (subgroup=="no"){
  groupres<-groupbynon(drugreaction=drugreaction_1drug)
  dfall<-combnongroupres(groupres=groupres,targetreac=targetreac)
  }
  if(druggroupname1==""){druggroupname1=drugs1[1]%>%str_to_sentence}
  if(druggroupname2==""){druggroupname2=drugs2[1]%>%str_to_sentence}
  cgres2plot(dfall=dfall,druggroupname1=druggroupname1,druggroupname2=druggroupname2,USRID=USRID_dvd)
  write.table(dfall,paste("WWW/temp/",USRID_dvd,"_forest.xls",sep=""),col.names = TRUE,row.names = FALSE,sep="\t")
  write.table(drugreaction_1drug$d1alle,paste("WWW/temp/",USRID_dvd,"_allreac.xls",sep=""),col.names = TRUE,row.names = FALSE,sep="\t")
  drugreaction_1drug$d1alle
}

#Multiple compare-----------------
fdapieplot_hdv<-function(drugs1=NULL,druggroupname1=NULL,reacount=reacount,reactotal=reactotal,removeindi=FALSE,year=NULL,odbyror=FALSE,targetreac=NULL,USRID=NULL){
  USRID_mpt<-paste(USRID,"_mpt",sep = "")
  drugreaction_1drug<-extractcases(d1=drugs1,d2="OTHER DRUGS",year=year)
  drug1pie<-tdte2pie(tdte=drugreaction_1drug$d1alle[,c(1,3)]%>%unique(),druggroupname=druggroupname1,USRID=USRID)
  if(removeindi==TRUE){
    drug1pie$df<-drug1pie$df[!as.character(tolower(drug1pie$df$Reaction))%in%(drugreaction_1drug$tdallindi$indi_pt%>%table()%>%names()),]
    print("filtered")
    drug1pie$df$Reaction[as.character(tolower(drug1pie$df$Reaction))%in%(drugreaction_1drug$tdallindi$indi_pt%>%table()%>%names())]%>%as.character(drug1pie$df$Reaction)
  }
  if(!is.null(targetreac)){
    targetreac=targetreac%>%str_to_sentence()
    drug1pie$df<-drug1pie$df[drug1pie$df$Reaction%in%targetreac,]
  }
  if(nrow(drug1pie$df)==0){
    stop("Please select reaction available for selected drugs")
    targetreac<-drug1pie$df$Reaction
  }
  drug1pie$df$groupname<-druggroupname1
  df<-drug1pie$df
  df$tdtr<-df$Frequency%>%as.numeric()
  df$tdntr<-nrow(drugreaction_1drug$d1alle)-df$tdtr
  df$ntdtr<-reacount$Count[match(df$Reaction,reacount$Reaction)]%>%as.numeric()-df$tdtr
  df$ntdntr<-nrow(drugreaction_1drug$d2alle)-df$ntdtr
  
  df$Reaction<-as.character(df$Reaction)
  
  df2 <- df %>%
    rowwise() %>%
    mutate(
      est = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$est,
      low = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$low,
      hi = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$hi,
      se = calculate_ROR_with_CI(a=tdtr, b=tdntr, c=ntdtr, d=ntdntr)$se
    ) %>%
    ungroup()
  if(odbyror){df2<-df2[order(df2$est,decreasing = TRUE),]}
  
  return(df2)
}
