library(shiny)
library(tidyverse)
library(bslib)
library(stringr)
library(stringi)
library(shinycssloaders)
library(shinyjs)
library(plotly)
library(echarts4r)
library(DT)
library(visNetwork)
library(future)
library(promises)
library(shinyalert)

# install.packages("UpSetR")
# library(random)
source("makefigures.R")
source("run.R")
source("reqfreegpt.R")
source("reqds.R")
source("calvenn.R")
source("reqncbi.R")
source("visdrugsupset.R")

# "Eczema herpeticum"
# "Upadacitinib" "Abrocitinib"
# library(dplyr)
# ids<-indexdata_prof$primaryid[indexdata_prof$prod_ai==toupper("Upadacitinib")]
# idss<-allindi_prof$indi_pt[allindi_prof$primaryid%in%ids]%>%table()%>%as.data.frame()

# install.packages("shinycssloaders",dependencies = T)
# install.packages("shinyalert")
# Define UI ----
my_theme <- bs_theme(bootswatch="cerulean")
# 定义 UI
ui <- fluidPage(
  #loading---------------------------
  useShinyjs(),  # 启用 shinyjs
  
  # 自定义CSS样式
  tags$head(
    tags$style(HTML("
                    body {
        min-width: 1500px;
      }
      .container-fluid {
        max-width: 1500px;
        margin: 0 auto;
      }
      /* 全屏遮罩层样式 */
      #loading-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(255, 255, 255, 0.9);
        z-index: 10000;
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
      }
        /* 提示文本样式 */
        .loading-text {
         font-size: 18px;
          color: #333;
          margin-bottom: 20px;
          text-align: center;
          font-weight: bold;
       }
      /* 进度条容器样式 */
      .progress-container {
        width: 50%;
        height: 30px;
        background-color: #f3f3f3;
        border: 2px solid #ddd;
        border-radius: 5px;
        overflow: hidden;
      }
      /* 进度条样式 */
      .progress-bar {
        height: 100%;
        background-color: #4caf50;
        width: 0;
        text-align: center;
        line-height: 30px;
        color: white;
        font-weight: bold;
      }
      /* 完成文本样式 */
      .complete-text {
        display: none;
        margin-top: 20px;
        font-size: 20px;
        font-weight: bold;
        color: #4caf50;
      }
    "))
  ),
  
  # 加载遮罩层
  div(id = "loading-overlay",
      div(class = "loading-text",
          span("Due to the large dataset, loading may take some time. Please be patient, Thanks!"),
          br(),             
          span("数据集较大，载入需要些时间，请耐心等待，感谢您的使用！")
      ),
      div(class = "progress-container",
          div(id = "progress-bar", class = "progress-bar", "0%")  # 进度条
      ),
      div(id = "complete-text", class = "complete-text", "Loading Complete!")  # 完成文本
  ),
  
  
  #main progranm---------------------------------------
  theme = my_theme,
  
  # 标题栏
  navbarPage(
    #设置分栏格式

    title = div(
      img(src = paste0("LOGO/SCT LOGO2.png?", Sys.time()), height = "80px", style = "vertical-align: middle;"),
      "FAERS Analysis", style = "display: inline-block; vertical-align: middle;"
    ),
    #1st tab----------------------------------
    tabPanel("Most Potential ADEs of Target Drug",
             fluidRow(
               column(12,
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt1_1",
                            label = "Choose Drugs to Show Their Most Potential ADEs:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "1200px"
                          ),
                          tags$span(style = "margin-right: 15px;"),
                          tags$a(href = "INFO/INFO_EN.htm",
                                 "Click here for user guide",
                                 target = "_blank",style="margin-top: 10px;")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "targetreac1_1",
                            label = "ADEs to focus on (blank for all)",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 50,
                              create = T,  # 禁止用户输入自定义选项
                              delimiter = ","  # 允许逗号分隔多个输入
                            ),
                            width = "1200px"
                          ),
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
 
                          textInput("druggroupname1_1", "Give Chosen drugs a name:",width = "1200px",
                            placeholder = 'Name typed in would be displayed in plot and be used for PubMed and AI analysis. Leave it empty the first drug selected would be used.',
                            ),
                          tags$span(style = "margin-right: 20px;"),
                          tags$a(href = "INFO/INFO_CN.htm",
                                 "中文版使用说明请点这里",
                                 target = "_blank",style="margin-top: 10px;")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          numericInput(inputId = "minpub1",label="Reports number for a ADE should exceed? (default≥1)",1,min = 1,max = 999999,width = "590px"),
                          tags$span(style ="color: white; margin-right: 20px;", "*"),
                          numericInput(inputId = "topn1",label="How many ADEs in forest plot? (1-50)",15,min = 1,max = 50,width = "590px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "odbyror1",
                            label = "How to filter the top data in forest plot?",
                            choices = list("By Report Number" = F, "By ROR" = T),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "removeindi1",
                            label = "Whether to remove ADEs same as indications?",
                            choices = list("Yes" = T, "No" = F),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "reqncbi1",
                            label = "Whether to count related publications in Pubmed database?",
                            choices = list("Yes" = T, "No" = F),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      ),

                      tags$span(style = "margin-right: 10px;"),
                      actionButton("submit1", "Submit"),
                      # downloadButton("downloadImage_pie","Download Pie Plot"),
                      downloadButton("downloaddata_pie","Download Pie Data"),
                      downloadButton("downloadImage_forest","Download Forest Plot"),
                      downloadButton("downloaddata_forest","Download Forest Data"),
                      div(br()),
               ),

               sidebarLayout(
                 sidebarPanel(
                   # 用于动态显示选中的年份
                   textOutput("selected_years_text"),
                   actionButton("select_all", "select_all",style="margin-bottom:10px;margin-top:10px;"),
                   actionButton("clear_all", "clear_all",style="margin-bottom:10px;margin-top:10px;"),
                   # 表格布局年份选择（放在 HTML 中）
                   tags$div(
                     id = "year_grid",
                     style = "display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px;",
                     lapply(2014:2024, function(year) {
                       actionButton(inputId = paste0("year_", year),
                                    label = year,
                                    style = "padding: 10px; text-align: center;")
                     })
                   )
                 ),
                 mainPanel(
                   # 使用一个 div 容器包裹 plotlyOutput，并设置局部居中
                   div(
                     style = "display: flex; justify-content: center; align-items: center;",
                     withSpinner(plotlyOutput("bar_plot1", height = 240, width = 480)),
                     withSpinner(plotlyOutput("bar_plot2", height = 240, width = 480))
                   )
                 )
               )
             ),
             tags$hr(),
             fluidRow(
               column(12,align = "center",
                      # textOutput("select_drugs"),
                      fluidRow(
                        column(6, withSpinner(echarts4rOutput("sh_pie"))),
                        column(6, withSpinner(echarts4rOutput("region_plot_nf")))
                      ),
                      br(),
                      uiOutput("dynamic_plots"),
                      withSpinner(uiOutput("drug_mpt_forest_plot")),
                      tags$script(HTML("
                       $(document).on('click', '.drug-link', function() {
                       var reaction = $(this).data('reaction');
                       Shiny.setInputValue('selected_reaction', reaction, {priority: 'event'});
                        });
                        ")),
                      
                      withSpinner(DTOutput("forest_table")),
                      br(),
                      br()
               )
             )
    ),
    #2nd tab----------------------------------
    tabPanel("Two Drugs Comparison",
             fluidRow(
               column(12,
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_dvd1",
                            label = "Target Drugs:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search drugs...',
                              maxOptions = 10,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "1200px"
                          ),
                          tags$span(style = "margin-right: 15px;"),
                          tags$a(href = "INFO/INFO_EN.htm",
                                 "Click here for user guide",
                                 target = "_blank",style="margin-top: 10px;")
                          ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_dvd2",
                            label = "Control Drugs:",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search drugs...',
                              maxOptions = 10,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "1200px"
                          ),
                          tags$span(style = "margin-right: 20px;"),
                          tags$a(href = "INFO/INFO_CN.htm",
                                 "中文版使用说明请点这里",
                                 target = "_blank",style="margin-top: 10px;")
                          ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "targetreac2_1",
                            label = "ADEs to focus on (blank for all)",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 50,
                              create = T,  # 禁止用户输入自定义选项
                              delimiter = ","  # 允许逗号分隔多个输入
                            ),
                            width = "1200px"
                          )
                          ),
                      radioButtons(
                        inputId = "subgroup",
                        label = "Whether use subgroup?",
                        choices = list("By Age" = "age", "By Gender" = "gender", "No" = "no"),
                        selected = "no"
                      ),
                      conditionalPanel(
                        condition = "input.subgroup == 'age'",
                        radioButtons(
                          inputId = "age_unit",
                          label = "Select age unit:",
                          choices = list("Year" = "year", "Day" = "day", "Hour" = "hour"),
                          selected = NULL
                        )
                      ),
                      conditionalPanel(
                        condition = "input.subgroup == 'age' && input.age_unit != null",
                        div(style = "display: flex; align-items: center;",#add red *
                            tags$span(style ="color: red; margin-right: 5px;", "*"),
                            numericInput(
                              inputId = "age_value",
                              label = "Enter age value:",
                              value = 0,
                              min = 0
                            ))
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput("druggroupname_dvd_1", "Give Target Group a Name:",width = "1200px",
                                    placeholder = 'Name typed in would be displayed in plot. Leave it empty the first target and control drugs selected would be used.')),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput("druggroupname_dvd_2", "Give Control Group a Name:",width = "1200px",
                                    placeholder = 'Name typed in would be displayed in plot. Leave it empty the first target and control drugs selected would be used.')),
                      tags$span(style = "margin-right: 10px;"),
                      actionButton("submit2", "Submit"),
                      downloadButton("DL_dvd_forest_plot","Download Forest Plot"),
                      downloadButton("DL_dvd_forest_data","Download Forest Data"),
                      # downloadButton("DL_dvd_areac_data","Download Target Drugs' All Reaction Data"),
                      div(br())
               ),
               sidebarLayout(
                 sidebarPanel(
                   # 用于动态显示选中的年份
                   textOutput("selected_years_text2"),
                   actionButton("select_all2", "select_all",style="margin-bottom:10px;margin-top:10px;"),
                   actionButton("clear_all2", "clear_all",style="margin-bottom:10px;margin-top:10px;"),
                   # 表格布局年份选择（放在 HTML 中）
                   tags$div(
                     id = "year_grid2",
                     style = "display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px;",
                     lapply(2014:2024, function(year2) {
                       actionButton(inputId = paste0("year2_", year2),
                                    label = year2,
                                    style = "padding: 10px; text-align: center;")
                     })
                   )
                 ),
                 mainPanel(
               ))
             ),
             tags$hr(),
             fluidRow(
               column(12,align = "center",
                      # textOutput("select_drugs2"),
                      withSpinner(uiOutput("drug_dvd_forest_plot")),
                      br(),
                      br()
               )
             )
    ),
    #3rd tab----------------------------------
    tabPanel("Multiple Drugs Comparison",
             fluidRow(
               column(12,
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt1",
                            label = "Choose Drugs for group1:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right:95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm1",label="Name of group1:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: red; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt2",
                            label = "Choose Drugs for group2:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm2",label="Name of group2:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt3",
                            label = "Choose Drugs for group3:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm3",label="Name of group3:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt4",
                            label = "Choose Drugs for group4:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm4",label="Name of group4:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt5",
                            label = "Choose Drugs for group5:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm5",label="Name of group5:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt6",
                            label = "Choose Drugs for group6:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm6",label="Name of group6:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt7",
                            label = "Choose Drugs for group7:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm7",label="Name of group7:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "Drugs_mpt8",
                            label = "Choose Drugs for group8:",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 15,
                              create = FALSE  # 禁止用户输入自定义选项
                            ),
                            width = "800px"
                          ),
                          tags$span(style = "margin-right: 95px;"),
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          textInput(inputId = "Drugs_nm8",label="Name of group8:",width = "300px")
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          selectizeInput(
                            inputId = "targetreac3_1",
                            label = "ADEs to focus on (blank for all)",
                            choices = NULL,
                            selected = NULL,
                            multiple = TRUE,
                            options = list(
                              placeholder = 'Start typing to search...',
                              maxOptions = 50,
                              create =T,  # 禁止用户输入自定义选项
                              delimiter = ","  # 允许逗号分隔多个输入
                            ),
                            width = "1205px"
                          )),
                          div(style = "display: flex; align-items: center;",#add red *
                              tags$span(style ="color: white; margin-right: 5px;", "*"),
                              numericInput(inputId = "minpub3",label="Reports for one ADR (Default≥1)",1,min = 0,max = 99999999,width = "290px"),
                              tags$span(style = "margin-right: 20px;"),
                              numericInput(inputId = "est",label="ROR should exceed (Default>1)",1,min=0,max=99999999,width = "290px"),
                              tags$span(style = "margin-right: 20px;"),
                              numericInput(inputId = "low",label=" 95% CI lower limit (Default>1)",1,min=0,max=99999999,width = "290px"),
                              tags$span(style = "margin-right: 20px;"),
                              numericInput(inputId = "maxadr3",label="Top ADRs to show (Default≤20)",20,min=1,max=50,width = "275px")
                              ),

                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "odbyror3",
                            label = "How to filter the top data in network?",
                            choices = list("By Report Number" = F, "By ROR" = T),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "removeindi3",
                            label = "Whether to remove ADEs same as indications?",
                            choices = list("Yes" = T, "No" = F),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      ),
                      div(style = "display: flex; align-items: center;",#add red *
                          tags$span(style ="color: white; margin-right: 5px;", "*"),
                          radioButtons(
                            inputId = "seevenn",
                            label = "Whether only show overlapped results",
                            choices = list("Yes" = T, "No" = F),
                            inline = T,
                            selected = F,
                            width = "1200px",
                          )
                      )
),
               div(tags$span(style = "margin-right: 10px;"), 
                   div(tags$span(style = "margin-right: 5px;"), 
                       actionButton("submit3", "Submit"),
                       tags$span(style = "margin-right: 5px;"),
                       downloadButton("downloadvenn_data",label = "Download Data"),
                       tags$span(style = "margin-right: 5px;"),
                       downloadButton("downloadupset_plot",label = "Download Upset Plot")),
      
                   div(br()),
                   div(tags$span(style = "margin-right: 15px;"), 
                       tags$a(href = "INFO/INFO_EN.htm", 
                              "Click here for user guide", 
                              target = "_blank",style="margin-top: 10px;"),
                       tags$span(style = "margin-right: 15px;"), 
                       tags$a(href = "INFO/INFO_CN.htm", 
                              "中文版使用说明请点这里", 
                              target = "_blank",style="margin-top: 10px;")
                   ),
                   div(br())
               ),
               
               sidebarLayout(
                 sidebarPanel(
                   # 用于动态显示选中的年份
                   textOutput("selected_years_text3"),
                   actionButton("select_all3", "select_all",style="margin-bottom:10px;margin-top:10px;"),
                   actionButton("clear_all3", "clear_all",style="margin-bottom:10px;margin-top:10px;"),
                   # 表格布局年份选择（放在 HTML 中）
                   tags$div(
                     id = "year_grid3",
                     style = "display: grid; grid-template-columns: repeat(6, 1fr); gap: 10px;",
                     lapply(2014:2024, function(year3) {
                       actionButton(inputId = paste0("year3_", year3), 
                                    label = year3, 
                                    style = "padding: 10px; text-align: center;")
                     })
                   )
                 ),
                 mainPanel(
                   # 使用一个 div 容器包裹 plotlyOutput，并设置局部居中
                   div(
                     style = "display: flex; justify-content: center; align-items: center;",
                     withSpinner(plotlyOutput("bar_plot3", height = 240, width = 480))
                   )
                 )
               )
             ),
             tags$hr(),
             fluidRow(
               column(12,align = "center",
                      div(style = "width:1400px; height:100vh;",  # 设置整个页面大小
                          withSpinner(visNetworkOutput("network_plot", height = "100vh", width = "100%"))
                      ),
                      br(),
                      br()
                      
                      # withSpinner(uiOutput("drug_mpt_pie_plot"))
               )
             ),
             visNetworkOutput("network"),
             verbatimTextOutput("selected_node"),
             tags$hr(),
             fluidRow(
               column(12,align = "center",
                      div(style = "width:1200px; height:100vh;",
                      withSpinner(uiOutput("upset_plot", height = "100vh", width = "100%"))
                      ),
                      br(),
                      br()
               )
             )
             
    )
  )
  
)




# Define server logic ----
server <- function(input, output,session) {
  
  
  
  # 数据加载完成后，隐藏加载遮罩层并显示主内容------------------
  # 模拟数据预加载并更新进度条
  for (i in 1:100) {
    Sys.sleep(0.01)  # 模拟数据加载的时间
    # 更新进度条的宽度和百分比
    shinyjs::runjs(sprintf("$('#progress-bar').css('width', '%d%%').text('%d%%');", i, i))
  }
  
  # 显示“加载完成”文本
  shinyjs::runjs("$('#complete-text').show();")
  
  # 短暂延迟后隐藏加载遮罩层并显示主内容
  shinyjs::delay(1000, {
    shinyjs::runjs("$('#loading-overlay').hide();")
  })
  
  
  USRID<-stri_rand_strings(1, 10, pattern = "[A-Za-z0-9]")
  #1st server--------------------------------
  
  # output$select_drugs <- renderText({
  #   paste("Most Potential Reactions for", input$druggroupname1)
  # })
  #按按钮才进行计算
  
  # 初始化选中的年份（反应式值）
  selected_years <- reactiveVal(NULL)
  # 为每个年份的按钮创建事件监听
  observe({
    lapply(2014:2024, function(year) {
      observeEvent(input[[paste0("year_", year)]], {
        current_years <- selected_years()
        if (year %in% current_years) {
          # 如果已经选中，则取消选择
          selected_years(setdiff(current_years, year))
        } else {
          # 否则添加到选中列表
          selected_years(c(current_years, year))
        }
      })
    })
  })
  
  observeEvent(input$select_all, {
    selected_years(2014:2024) # 全选所有年份
  })
  
  observeEvent(input$clear_all, {
    selected_years(NULL) # 清空选择
  })
  
  # 显示选中的年份（供用户参考）
  output$selected_years_text <- renderText({
    year <- selected_years()
    if (is.null(year) || length(year) == 0) {
      "When no year selected, show all results"
    } else {
      paste("Years selected：", paste(sort(year), collapse = ", "))
    }
  })
  
  
  # 点击submit1过滤数据
  filtered_data_all <- eventReactive(input$submit1, {
    years <- selected_years()
    if (is.null(years) || length(years) == 0) {
      years <- 2014:2024 # 默认年份范围
    }
    calrepyear(years) # 使用自定义函数生成数据
  })
  
  filtered_data_tg <- eventReactive(input$submit1, {
    if(is.null(input$Drugs_mpt1_1)){shinyalert("Please check parameters", "Some parameters may be missing, or no ADEs under the current settings", type = "error")
      return(NULL)}
    years <- selected_years()
    if (is.null(years) || length(years) == 0) {
      years <- 2014:2024 # 默认年份范围
    }
    tgs<-finddrugreacs(drugs=input$Drugs_mpt1_1,year=years)
    tgs$tdalle[,c(1,5)]
  })
  
  # 绘制柱状图 目标药物
  output$bar_plot1 <- renderPlotly({
    data <- filtered_data_tg()
    if (is.null(data)) return(NULL)

    # 假设 calrepyear(years) 返回一个表格，其中包含 "year" 和 "count" 列
    summary_data <- as.data.frame(table(data$sysyear)) # 统计每年数量
    colnames(summary_data) <- c("year", "count")
    summary_data$year <- as.numeric(as.character(summary_data$year))

    plot_ly(
      data = summary_data,
      x = ~year,
      y = ~count,
      type = "bar",
      marker = list(color = "skyblue"),
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = paste0("Reports of Target Drugs (total: ",sum(summary_data$count),")"),
        xaxis = list(
          title = "Year",
          tickmode = "array",  # 设置 tickmode 为 array
          tickvals = ~year,    # 在 x 轴显示的值
          ticktext = ~year     # 与 tickvals 相对应的标签
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        height = 240,
        width = 480
      )
  })

  output$bar_plot2 <- renderPlotly({
    data <- filtered_data_all()
    if (is.null(data)) return(NULL)

    # 假设 calrepyear(years) 返回一个表格，其中包含 "year" 和 "count" 列
    summary_data <- as.data.frame(table(data$sysyear)) # 统计每年数量
    colnames(summary_data) <- c("year", "count")
    summary_data$year <- as.numeric(as.character(summary_data$year))

    plot_ly(
      data = summary_data,
      x = ~year,
      y = ~count,
      type = "bar",
      marker = list(color = "skyblue"),
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = paste0("Reports of All Drugs (total: ",sum(summary_data$count),")"),
        xaxis = list(
          title = "Year",
          tickmode = "array",  # 设置 tickmode 为 array
          tickvals = ~year,    # 在 x 轴显示的值
          ticktext = ~year     # 与 tickvals 相对应的标签
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        height = 240,
        width = 480
      )
  })
  
  #正式计算生成饼图森林图
  reactive_sh_pie <- eventReactive(input$submit1, {
    year <- selected_years()
    if (is.null(year) || length(year) == 0) {
      year <-2014:2024 # 未选择年份时，显示所有数据
    }
    res_reactive_sh_pie<-try(
    fdapieplot(drugs1=input$Drugs_mpt1_1,druggroupname1=input$druggroupname1_1,targetreac = input$targetreac1_1,topn=input$topn1,odbyror=input$odbyror1,minpub=input$minpub1,removeindi = input$removeindi1,year=year,USRID=USRID)
    )
    if (inherits(res_reactive_sh_pie, "try-error")) {
      return(NULL)
    } else {
      return(res_reactive_sh_pie)
    }
    })
  
  # reactive_img_path_pie <- eventReactive(input$submit1, {
  #   paste("temp/",USRID,"_pie2.png?", Sys.time(),sep = "")
  # })
  
  reactive_img_path_forest <- eventReactive(input$submit1, {
    paste("temp/",USRID,"_mpt_forest2.png?", Sys.time(),sep = "")
  })
  
  reactive_name <- eventReactive(input$submit1, {
    if(input$druggroupname1_1==""){return(input$Drugs_mpt1_1[1]%>%str_to_sentence)}else{return((input$druggroupname1_1))}
  })
  
  # 饼图点击事件处理
  output$sh_pie <- renderEcharts4r({
    if(is.null(reactive_sh_pie())){
      shinyalert("Pie plot is empty", "Some parameters may be missing, or no ADEs under the current settings", type = "error")
      return(NULL)}
    reactive_sh_pie()[[1]]  %>%
      e_charts(Reaction) %>%
      e_pie(Frequency, radius = "50%") %>%
      e_title(paste("ADEs Distribution of", reactive_name()), left = "center") %>%
      e_tooltip(trigger = "item", 
                formatter = htmlwidgets::JS("
                function(params) {
                  return params.name + ': ' + params.value + ' occurrences';
                }
              ")) %>%
      # e_legend(orient = "vertical", right = "10%") 
      e_legend(show = FALSE)%>%
      e_on(
        list(seriesName = "Frequency"),
        htmlwidgets::JS("
        function(params) {
          Shiny.setInputValue('pieChart_click', params.name, {priority: 'event'});
        }
      ")
      )
    
  })
  
  observe({
    updateTextInput(session, "selected_reaction", value = NULL)  # 或者直接通过其他方式清除该变量
  })  
  
  # 根据选定区域生成年份和区域统计数量图
  output$region_plot_nf <- renderEcharts4r({
    df_nf <- reactive_sh_pie()[[3]]
    df_nf<-df_nf$occr_country%>%table%>%as.data.frame()
    colnames(df_nf)<-c("Region","Count")
    # 按Count降序排列，并取Top 10
    df_nf <- df_nf %>%
      arrange(desc(Count)) %>%
      head(10)
    # 创建按区域统计数量的饼图
    df_nf %>%
      e_charts(Region) %>%
      e_pie(Count, radius = "50%") %>%  # 生成饼图，设置内外半径
      e_title(paste("Report Regions for", reactive_name()), left = "center") %>%
      e_tooltip(trigger = "item", 
                formatter = htmlwidgets::JS("
                function(params) {
                  return params.name + ': ' + params.value + ' occurrences';
                }
              ")) %>%  # 显示详细信息（百分比和数量）
      # e_legend(orient = "vertical", right = "10%", top = "middle")
      e_legend(show = FALSE)%>%  # 显示图例
      e_on(
        list(seriesName = "Count"),
        htmlwidgets::JS("
        function(params) {
          Shiny.setInputValue('pieChart_click2', params.name, {priority: 'event'});
        }
      ")
      )
  })
  
  observe({
    updateTextInput(session, "selected_c", value = NULL)  # 或者直接通过其他方式清除该变量
  })
  
  

  
  # 1. 创建一个reactiveVal存储点击状态
  pieChartClick <- reactiveVal(NULL)
  pieChartClick2 <- reactiveVal(NULL)
  # 2. 在 `observeEvent()` 监听提交按钮点击时重置
  observeEvent(input$submit1, {
    first_reaction <- reactive_sh_pie()[[1]][1,1]
    pieChartClick(first_reaction)
    pieChartClick2(NULL)# 重置点击状态
  })
  
  # 3根据点击的饼图区域名称更新数据
  reactive_filtered_data <- reactive({
    req(pieChartClick()) # 确保点击事件发生时才更新

    # 假设df是您原始的数据框，包含“Reaction”, "Year", "Region" 和 "Count" 列
    selected_reaction <- pieChartClick()
    
    if (is.null(selected_reaction)) {
      rsp<-reactive_sh_pie()[[3]]
      selected_reaction <- rsp[1, "pt"]  # 默认选择数据中的第一个Reaction
    }
    
    # 过滤数据，根据区域名称选择数据
    df_filtered <- reactive_sh_pie()[[3]] %>%
      filter(pt == selected_reaction)
    
    return(df_filtered)
  })
  
  # 根据点击的2号饼图区域显示PT
  reactive_filtered_by_region <- reactive({
    req(pieChartClick2()) # 确保点击事件发生时才更新
    
    # 假设df是您原始的数据框，包含“Reaction”, "Year", "Region" 和 "Count" 列
    selected_c <- pieChartClick2()
    
    if (is.null(selected_c)) {
      rsp<-reactive_sh_pie()[[3]]
      selected_c <- rsp[1, "occr_country"]  # 默认选择数据中的第一个occr_country
    }
    
    # 过滤数据，根据区域名称选择数据
    df_filtered <- reactive_sh_pie()[[3]] %>%
      filter(occr_country == selected_c)
    
    return(df_filtered)
  })
  
  # 4. 在 `echarts4r` 的点击事件监听时更新 `pieChartClick`
  observeEvent(input$pieChart_click, {
    pieChartClick(input$pieChart_click)  # 更新存储的点击值
  })
  
  observeEvent(input$pieChart_click2, {
    pieChartClick2(input$pieChart_click2)  # 更新存储的点击值
  })

  
  # 根据选定ADR生成年份和区域统计数量图
  output$year_plot <- renderEcharts4r({
    df_filtered <- reactive_filtered_data()
    df_filtered<-df_filtered$sysyear%>%table%>%as.data.frame()
    colnames(df_filtered)<-c("Year","Count")
    # 创建按年份和区域统计数量的柱状图
    df_filtered %>%
      e_charts(Year) %>%
      e_bar(Count) %>%
      e_title(paste("Yearly Counts for", pieChartClick(),"of",reactive_name()), left = "center") %>%
      e_x_axis(name = "Year",axisLabel = list(interval = 0, rotate = 90)) %>%
      e_y_axis(name = "Count") %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(show = FALSE)
  })
  
  # 根据选定ADR生成年份和区域统计数量图
  output$region_plot <- renderEcharts4r({
    df_filtered <- reactive_filtered_data()
    df_filtered<-df_filtered$occr_country%>%table%>%as.data.frame()
    colnames(df_filtered)<-c("Region","Count")
    # 按Count降序排列，并取Top 10
    df_filtered <- df_filtered %>%
      arrange(desc(Count)) %>%
      head(10)
    # 创建按区域统计数量的饼图
    df_filtered %>%
      e_charts(Region) %>%
      e_pie(Count, radius = "50%") %>%  # 生成饼图，设置内外半径
      e_title(paste("Report Region Counts for", pieChartClick(),"of",reactive_name()), left = "center") %>%
      e_tooltip(trigger = "item", 
                formatter = htmlwidgets::JS("
                function(params) {
                  return params.name + ': ' + params.value + ' occurrences';
                }
              ")) %>%  # 显示详细信息（百分比和数量）
      # e_legend(orient = "vertical", right = "10%", top = "middle")  # 显示图例
      e_legend(show = FALSE)
  })
  
  # 根据选定区域生成年份和ADR统计数量图
  output$year_plot2 <- renderEcharts4r({
    df_filtered <- reactive_filtered_by_region()
    df_filtered<-df_filtered$sysyear%>%table%>%as.data.frame()
    colnames(df_filtered)<-c("Year","Count")
    # 创建按年份统计数量的柱状图
    df_filtered %>%
      e_charts(Year) %>%
      e_bar(Count) %>%
      e_title(paste("Yearly Counts of Reports from", input$pieChart_click2), left = "center") %>%
      e_x_axis(name = "Year",axisLabel = list(interval = 0, rotate = 90)) %>%
      e_y_axis(name = "Count") %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(show = FALSE)
  })
  
  # 根据选定区域生成年份和ADR统计数量图
  output$adr_plot2 <- renderEcharts4r({
    df_filtered <- reactive_filtered_by_region()
    df_filtered<-df_filtered$pt%>%table%>%as.data.frame()
    colnames(df_filtered)<-c("Reaction","Count")
    # 按Count降序排列，并取Top 10
    df_filtered <- df_filtered %>%
      arrange(desc(Count)) %>%
      head(10)
    # 创建按区域统计数量的饼图
    df_filtered %>%
      e_charts(Reaction) %>%
      e_pie(Count, radius = "50%") %>%  # 生成饼图，设置内外半径
      e_title(paste("Reaction Counts of Reports from", input$pieChart_click2), left = "center") %>%
      e_tooltip(trigger = "item", 
                formatter = htmlwidgets::JS("
                function(params) {
                  return params.name + ': ' + params.value + ' occurrences';
                }
              ")) %>%  # 显示详细信息（百分比和数量）
      # e_legend(orient = "vertical", right = "10%", top = "middle")
      e_legend(show = FALSE)# 显示图例
  })
  
  output$drug_mpt_forest_plot <- renderUI({
    img_path_for <- reactive_img_path_forest()
    tags$img(src = img_path_for)
  })
  indexdata_prof$prod_ai%>%unique()%>%length()
  
  #创建可交互式表格 森林图数据的
  output$forest_table <- renderDT({
    df <- reactive_sh_pie()[[4]]
    keywords_list<-lapply(df$Reaction,function(x) paste0(indexdrug_prof[i]," AND ",x,""))
    if(input$reqncbi1){pubrep<-reqncbi(keywords_list)%>%as.numeric()}else{pubrep<-"Step Skipped"}
    # 使 Reaction 列变为可点击的 HTML 超链接
    df$Reaction <- paste0('<span class="drug-link" data-reaction="', df$Reaction, '" style="cursor:pointer; color:blue; text-decoration:underline;">', df$Reaction, '</span>')
    df$Pubmed_count<-pubrep
    datatable(df, escape = FALSE, selection = "none", options = list(
      dom = "t",
      paging = FALSE
    ))
  }, server = FALSE)

  # 监听点击的 Reaction
  observeEvent(input$selected_reaction, {
    #判断是否使用药物名称
    pubmed_links<-a(input$selected_reaction, href = paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", URLencode(paste(reactive_name(), input$selected_reaction))), target = "_blank")
    
    showModal(modalDialog(
      title = paste("Details for", reactive_name()),
      h4("Pubmed reports about target drug(s) and selected ADE:"),
      div(br()),
      tagList(pubmed_links),
      tags$head(tags$style(HTML("
    #chatres_content_1 {
      max-height: 600px;  
      max-width:1200px;
      overflow-y: auto;   
      word-wrap: break-word;
      white-space: pre-wrap;
    }
  "))),
      div(br()),
      h4("Wait a sec to see AI analysis:"),
      div(
        id = "chatres_content_1",
        style = "max-height: 600px; max-width:1200px; overflow-y: auto; word-wrap: break-word; white-space: pre-wrap;",
        pre(style = "white-space: pre-wrap; word-break: break-word;", withSpinner(textOutput("chatres_text1")))
      ),
      
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        tags$script(HTML("
      $('#shiny-modal').on('hidden.bs.modal', function () {
        $('body').css('overflow', 'auto');
      });
    "))
      ),
      size = "l"
    ))
    
    # 异步获取 ChatGPT 结果
    chatres1("Loading......")
    rname<-reactive_name()
    rsr<-input$selected_reaction
    future({
      reqgpt(message = paste("Please show the relationship between drugs", related_nodes_str, "and ADR", node_name,"only say what was recorded but not deduced.",
                             "Within 200 words using the following format.
                              drug funtion:
                              Is the ADR related to drug indication:(Yes/No)
                              Is the ADR a known side effect:(Yes/No)
                              relation ship between the drug and reaction:
                              "))
    }) %>% 
      then(function(result) {
        chatres1(result)  # 更新 ChatGPT 结果
      }) %>%
      catch(function(e) {
        chatres1("Error: Failed to fetch AI response!")  # 失败时返回错误信息
      })
  })
  
  chatres1 <- reactiveVal("Loading......")
  
  # 渲染 ChatGPT 结果
  output$chatres_text1 <- renderText({
    chatres1()
  })
  
  observeEvent(input$toggle_chatres1, {
    toggle("chatres_content_1")
  })
  
  
  
  #动态渲染点击后的图
  # 1. 创建一个 reactiveVal() 变量来存储当前点击的图表
  selected_pie_chart <- reactiveVal("sh_pie")
  
  # 2. 监听 `sh_pie` 的点击事件，更新 selected_pie_chart
  observeEvent(input$pieChart_click, {
    selected_pie_chart("sh_pie")
  })
  
  # 3. 监听 `region_plot_nf` 的点击事件，更新 selected_pie_chart
  observeEvent(input$pieChart_click2, {
    selected_pie_chart("region_plot_nf")
  })
  
  # 4. 根据 selected_pie_chart 动态渲染 UI
  output$dynamic_plots <- renderUI({
    if (selected_pie_chart() == "sh_pie") {
      fluidRow(
        column(6, withSpinner(echarts4rOutput("year_plot", width = "100%"))),
        column(6, withSpinner(echarts4rOutput("region_plot", width = "100%")))
      )
    } else {
      fluidRow(
        column(6, withSpinner(echarts4rOutput("year_plot2", width = "100%"))),
        column(6, withSpinner(echarts4rOutput("adr_plot2", width = "100%")))
      )
    }
  })
  

  output$downloadImage_forest<-downloadHandler(
    filename = function() {
      paste("High-resolution image of the most potential reaction forest plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_mpt_forest.png",sep=""), file)
    }
  )
  
  output$downloaddata_forest<-downloadHandler(
    filename = function() {
      paste("Data of the most potential reaction forest plot", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_mpt_forest.xls",sep=""), file)
    }
  )
  
  output$downloaddata_pie<-downloadHandler(
    filename = function() {
      paste("Data of the most potential reaction pie plot", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_pie.xls",sep=""), file)
    }
  )
  #2nd server--------------------------------

  # 初始化选中的年份（反应式值）
  selected_years2 <- reactiveVal(NULL)
  # 为每个年份的按钮创建事件监听
  observe({
    lapply(2014:2024, function(year2) {
      observeEvent(input[[paste0("year2_", year2)]], {
        current_years2 <- selected_years2()
        if (year2 %in% current_years2) {
          # 如果已经选中，则取消选择
          selected_years2(setdiff(current_years2, year2))
        } else {
          # 否则添加到选中列表
          selected_years2(c(current_years2, year2))
        }
      })
    })
  })
  
  observeEvent(input$select_all2, {
    selected_years2(2014:2024) # 全选所有年份
  })
  
  observeEvent(input$clear_all2, {
    selected_years2(NULL) # 清空选择
  })
  
  # 显示选中的年份（供用户参考）
  output$selected_years_text2 <- renderText({
    year2 <- selected_years2()
    if (is.null(year2) || length(year2) == 0) {
      "When no year selected, show all results"
    } else {
      paste("Years selected：", paste(sort(year2), collapse = ", "))
    }
  })
  
  
  #按按钮才进行计算
  reactive_img_path_dvd_forest <- eventReactive(input$submit2, {
    tryCatch({
      year2 <- selected_years2()
      if (is.null(year2) || length(year2) == 0) {
        year2 <- 2014:2024 # 未选择年份时，显示所有数据
      }
      
      # 调用ffplot_dvd生成数据
      forest_data <- ffplot_dvd(drugs1 = input$Drugs_dvd1, drugs2 = input$Drugs_dvd2, year = year2, 
                                USRID = USRID, targetreac = input$targetreac2_1, 
                                druggroupname1 = input$druggroupname_dvd_1, 
                                druggroupname2 = input$druggroupname_dvd_2, 
                                subgroup = input$subgroup, age_unit = input$age_unit, age = input$age_value)
      
      # 设置生成图像路径
      forest_path <- paste("temp/", USRID, "_dvd_forest2.png?", Sys.time(), sep = "")
      
      # 返回数据
      return(list(forest_data = forest_data, forest_path = forest_path))
    }, error = function(e) {
      # 捕获到错误时弹出对话框
      shinyalert("Target drug or reaction not filled", "Some parameters may be missing, or no common adverse reactions between the two drugs were found under the current settings", type = "error")
      # 返回一个空的列表或适当的默认值
      return(NULL)
    })
  })
  
  # 渲染 UI 输出
  
  output$drug_dvd_forest_plot <- renderUI({
    img_path_for <- reactive_img_path_dvd_forest()$forest_path
    tags$img(src = img_path_for)
  })
  
  # output$dataTable2 <- renderDT({
  #   res<-reactive_img_path_dvd_forest()$forest_data
  #   rownames(res)<-1:nrow(res)
  #   res# +1 由于数据索引从0开始
  #   
  # }, options = list(pageLength = 5))
  
  output$DL_dvd_forest_plot<-downloadHandler(
    filename = function() {
      paste("High-resolution image of the two drugs comparison forest plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_dvd_forest.png",sep=""), file)
    }
  )
  
  output$DL_dvd_forest_data<-downloadHandler(
    filename = function() {
      paste("Data of the two drugs comparison forest plot", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_dvd_forest.xls",sep=""), file)
    }
  )
  
  # output$DL_dvd_areac_data<-downloadHandler(
  #   filename = function() {
  #     paste("Forest DATA for ",input$druggroupname_dvd_1," ", Sys.Date(), ".xls", sep = "")
  #   },
  #   content = function(file) {
  #     file.copy(paste("WWW/temp/",USRID,"_dvd_allreac.xls",sep=""), file)
  #   }
  # )
  #允许自动重连
  session$allowReconnect(TRUE)
  # 当会话结束时删除图片文件
  session$onSessionEnded(function() {
    image_path<-c(
                  paste("WWW/temp/",USRID,"_pie2.png",sep=""),
                  paste("WWW/temp/",USRID,"_pie.xls",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest.png",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest2.png",sep=""),
                  paste("WWW/temp/",USRID,"_mpt_forest.xls",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest.png",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest2.png",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_forest.xls",sep=""),
                  paste("WWW/temp/",USRID,"_dvd_allreac.xls",sep=""),
                  paste0("WWW/temp/",USRID,"_venn.xls"),
                  paste0("WWW/temp/",USRID,"_upset.png"),
                  paste0("WWW/temp/",USRID,"_upset2.png"),
                  paste0("WWW/temp/",USRID,"_venn_binary_matrix.xls")
                  )
    lapply(image_path, function(x){
      if (file.exists(x)) {
        file.remove(x)
        message("Deleted file: ", x)
      }
    })
  })
 
  #3rd server--------------------------------
  
  output$downloadvenn_data<-downloadHandler(
    filename = function() {
      paste("Overlapped reactions", Sys.Date(), ".xls", sep = "")
    },
    content = function(file) {
      file.copy(paste("WWW/temp/",USRID,"_venn.xls",sep=""), file)
    }
  )
  
  output$downloadupset_plot<-downloadHandler(
    filename = function() {
      paste("Upset_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      file.copy(paste0("WWW/temp/",USRID,"_upset.png"), file)
    }
  )
  

  
  # 初始化选中的年份（反应式值）
  selected_years3 <- reactiveVal(NULL)
  # 为每个年份的按钮创建事件监听
  observe({
    lapply(2014:2024, function(year3) {
      observeEvent(input[[paste0("year3_", year3)]], {
        current_years3 <- selected_years3()
        if (year3 %in% current_years3) {
          # 如果已经选中，则取消选择
          selected_years3(setdiff(current_years3, year3))
        } else {
          # 否则添加到选中列表
          selected_years3(c(current_years3, year3))
        }
      })
    })
  })
  
  observeEvent(input$select_all3, {
    selected_years3(2014:2024) # 全选所有年份
  })
  
  observeEvent(input$clear_all3, {
    selected_years3(NULL) # 清空选择
  })
  
  # 显示选中的年份（供用户参考）
  output$selected_years_text3 <- renderText({
    year3 <- selected_years3()
    if (is.null(year3) || length(year3) == 0) {
      "When no year selected, show all results"
    } else {
      paste("Years selected：", paste(sort(year3), collapse = ", "))
    }
  })
  
  
  # 点击submit3过滤数据
  filtered_data_all3 <- eventReactive(input$submit3, {
    years3 <- selected_years3()
    if (is.null(years3) || length(years3) == 0) {
      years3 <- 2014:2024 # 默认年份范围
    }
    calrepyear(years3) # 使用自定义函数生成数据
  })
  
  # filtered_data_tg3 <- eventReactive(input$submit3, {
  #   years3 <- selected_years3()
  #   if (is.null(years3) || length(years3) == 0) {
  #     years3 <- 2014:2024 # 默认年份范围
  #   }
  #   tgs<-finddrugreacs(drugs=input$Drugs_mpt1,year=years3)
  #   tgs$tdalle[,c(1,5)]
  # })
  
  
  
  output$bar_plot3 <- renderPlotly({
    data <- filtered_data_all3()
    if (is.null(data)) return(NULL)
    
    # 假设 calrepyear(years) 返回一个表格，其中包含 "year" 和 "count" 列
    summary_data <- as.data.frame(table(data$sysyear)) # 统计每年数量
    colnames(summary_data) <- c("year", "count")
    summary_data$year <- as.numeric(as.character(summary_data$year))
    
    plot_ly(
      data = summary_data,
      x = ~year,
      y = ~count,
      type = "bar",
      marker = list(color = "skyblue"),
      hoverinfo = "x+y"
    ) %>%
      layout(
        title = paste0("Reports of All Drugs (total: ",sum(summary_data$count),")"),
        xaxis = list(
          title = "Year",
          tickmode = "array",  # 设置 tickmode 为 array
          tickvals = ~year,    # 在 x 轴显示的值
          ticktext = ~year     # 与 tickvals 相对应的标签
        ),
        yaxis = list(title = "Count"),
        barmode = "stack",
        height = 240,
        width = 480
      )
  })
  
  #计算网络图数据
  reactive_sh_nw <- eventReactive(input$submit3, {
    year <- selected_years3()
    if (is.null(year) || length(year) == 0) {
      year <-2014:2024 # 未选择年份时，显示所有数据
    }
    #提取ROR
    print("year OK")
    Drug1=NULL
    Drug2=NULL
    Drug3=NULL
    Drug4=NULL
    Drug5=NULL
    Drug6=NULL
    Drug7=NULL
    Drug8=NULL
    try(if(!is.null(input$Drugs_mpt1)){Drug1<-fdapieplot_hdv(drugs1=input$Drugs_mpt1,druggroupname1=ifelse(input$Drugs_nm1=="",input$Drugs_mpt1[1]%>%str_to_sentence(),input$Drugs_nm1),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt2)){Drug2<-fdapieplot_hdv(drugs1=input$Drugs_mpt2,druggroupname1=ifelse(input$Drugs_nm2=="",input$Drugs_mpt2[1]%>%str_to_sentence(),input$Drugs_nm2),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt3)){Drug3<-fdapieplot_hdv(drugs1=input$Drugs_mpt3,druggroupname1=ifelse(input$Drugs_nm3=="",input$Drugs_mpt3[1]%>%str_to_sentence(),input$Drugs_nm3),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt4)){Drug4<-fdapieplot_hdv(drugs1=input$Drugs_mpt4,druggroupname1=ifelse(input$Drugs_nm4=="",input$Drugs_mpt4[1]%>%str_to_sentence(),input$Drugs_nm4),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt5)){Drug5<-fdapieplot_hdv(drugs1=input$Drugs_mpt5,druggroupname1=ifelse(input$Drugs_nm5=="",input$Drugs_mpt5[1]%>%str_to_sentence(),input$Drugs_nm5),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt6)){Drug6<-fdapieplot_hdv(drugs1=input$Drugs_mpt6,druggroupname1=ifelse(input$Drugs_nm6=="",input$Drugs_mpt6[1]%>%str_to_sentence(),input$Drugs_nm6),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt7)){Drug7<-fdapieplot_hdv(drugs1=input$Drugs_mpt7,druggroupname1=ifelse(input$Drugs_nm7=="",input$Drugs_mpt7[1]%>%str_to_sentence(),input$Drugs_nm7),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    try(if(!is.null(input$Drugs_mpt8)){Drug8<-fdapieplot_hdv(drugs1=input$Drugs_mpt8,druggroupname1=ifelse(input$Drugs_nm8=="",input$Drugs_mpt8[1]%>%str_to_sentence(),input$Drugs_nm8),reacount=reacount,reactotal=reactotal,removeindi = input$removeindi3,odbyror=input$odbyror3,year=year,targetreac=input$targetreac3_1,USRID=USRID)%>%as.data.frame()})
    # #整理ROR
    # USRID<-NULL
    # year<-2020:2024
    
    # try(Drug1<-fdapieplot_hdv(drugs1=toupper(c("Ruxolitinib","Ruxolitinib phosphate")),druggroupname1="Ruxolitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
    # try(Drug2<-fdapieplot_hdv(drugs1=toupper(c("Tofacitinib","Tofacitinib citrate")),druggroupname1="Tofacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
    # try(Drug3<-fdapieplot_hdv(drugs1=toupper(c("Baricitinib")),druggroupname1="Baricitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
    # try(Drug4<-fdapieplot_hdv(drugs1=toupper(c("Upadacitinib","Upadacitinib hemihydrate")),druggroupname1="Upadacitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
    # try(Drug5<-fdapieplot_hdv(drugs1=toupper(c("Fedratinib","Fedratinib hydrochloride")),druggroupname1="Fedratinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())
    # try(Drug6<-fdapieplot_hdv(drugs1=toupper(c("Abrocitinib")),druggroupname1="Abrocitinib",reacount=reacount,reactotal=reactotal,year=2014:2024,targetreac=c("Immunodeficiency",	"Immunosuppression"),USRID=NULL)%>%as.data.frame())

    # # # # # # #
    print("input OK")
    # Drug_list<-list(Drug1,Drug2,Drug3,Drug4,Drug5,Drug6,Drug7,NULL)
    Drug_list<-list(Drug1,Drug2,Drug3,Drug4,Drug5,Drug6,Drug7,Drug8)
    Drug_list<-Filter(Negate(is.null), Drug_list)
    
    # Drug_list<-process_drug_list(Drug_list=Drug_list,minpub=3,maxadr=20,USRID=123123,seevenn=T)
    if(length(Drug_list)<2){shinyalert("Please check parameters", "Some parameters may be missing, or no common adverse reactions were found under the current settings", type = "error")
      return(NULL)}
    #处理druglist 同时进行VENN分析
    Drug_list<-process_drug_list(Drug_list=Drug_list,minpub=input$minpub3,maxadr=input$maxadr3,input_est=input$est,input_low=input$low,USRID=USRID,seevenn=input$seevenn)
    visdrugs_upset(USRID=USRID,ndrug=length(Drug_list))
        # i=1
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
    
    # Drug_names<-c("ADE","LAM")
    # Drug_names<-c(input$Drugs_nm1,input$Drugs_nm2,input$Drugs_nm3,input$Drugs_nm4,input$Drugs_nm5,input$Drugs_nm6,input$Drugs_nm7,input$Drugs_nm8)
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
    return(list(drug_data=drug_data,adr_data=adr_data))
  })
  
  
  output$network_plot <- renderVisNetwork({
    # 构建节点数据
    nodes <- data.frame(
      id = c(reactive_sh_nw()[[1]]$drug_name, reactive_sh_nw()[[2]]$Reaction),
      label = c(reactive_sh_nw()[[1]]$drug_name, reactive_sh_nw()[[2]]$Reaction),
      size = c(rep(20, length(reactive_sh_nw()[[1]]$drug_name)), reactive_sh_nw()[[2]]$ADR_size*2.5),  # 药物节点大小固定，ADR节点大小依据ADR_size
      color = c(reactive_sh_nw()[[1]]$drug_color, reactive_sh_nw()[[2]]$ADR_color)  # 药物节点颜色为 lightblue，ADR节点颜色根据ADR_color
    )
    print(nodes)
    # 构建边数据
    edges <- data.frame(
      from = rep(reactive_sh_nw()[[1]]$drug_name, each = nrow(reactive_sh_nw()[[2]])),
      to = rep(reactive_sh_nw()[[2]]$Reaction, times = nrow(reactive_sh_nw()[[1]])),
      value = lapply(reactive_sh_nw()[[2]][,2:(ncol(reactive_sh_nw()[[2]])-2)],as.numeric)%>%unlist()
    )
    edges$color<-reactive_sh_nw()[[1]]$drug_color[match(edges$from,reactive_sh_nw()[[1]]$drug_name)]
    print(edges$value)
    
    # 过滤掉值为0的边
    edges <- edges %>% filter(value > 0)
    # 归一化边的宽度，使最大边宽度适中
    edges$value<-log2(edges$value)
    max_value <- max(edges$value)
    min_value <- min(edges$value)
    edges$normalized_value <- (edges$value - min_value) / (max_value - min_value) * 10 + 1  # 缩放至1到10之间
    # 创建visNetwork图形
    visNetwork(nodes, edges) %>%
      visNodes(size = "size", color = "color",opacity=1,borderWidth=1.5,font = list(size = 16,strokeWidth=1, color = "black",bold=T , face = "arial")) %>%  # 设置节点的大小和颜色
      visEdges(arrows = 'to',width = "normalized_value", color = list(color ="color", highlight = "#FF6347")) %>%  # 设置边的宽度和颜色
      visOptions(highlightNearest = list(enabled = TRUE, degree = list(from = 1, to = 1)), 
                 nodesIdSelection = FALSE,
                 ) %>%  # 开启节点高亮和节点选择功能
      visLayout(randomSeed = 123)%>%
      visInteraction(navigationButtons = TRUE) %>%# 设置随机种子，保证每次布局一致
      visPhysics(
        stabilization = TRUE,      # 启用稳定化
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(gravitationalConstant = -15)# 使用不同的力导向布局
      )%>%
      visEvents(click = "function(properties) {
        var clickedNodeId = properties.nodes[0];
        Shiny.setInputValue('clicked_node', clickedNodeId, {priority: 'event'});
      }")
  })

  output$upset_plot <- renderUI({
    req(input$submit3)  # 只有点击 submit 后才执行以下代码
    img_path_upset <- paste0("temp/", USRID, "_upset2.png?",Sys.time())
    tags$img(src = img_path_upset)
  })
  
  # 监听点击事件并获取相关 edges
  observeEvent(input$clicked_node, {
    req(input$clicked_node)
    #下面这个edges表格是否可以替换  ############################################
    edges <- data.frame(
      from = rep(reactive_sh_nw()[[1]]$drug_name, each = nrow(reactive_sh_nw()[[2]])),
      to = rep(reactive_sh_nw()[[2]]$Reaction, times = nrow(reactive_sh_nw()[[1]])),
      value = lapply(reactive_sh_nw()[[2]][,2:(ncol(reactive_sh_nw()[[2]])-2)],as.numeric)%>%unlist()
    )
    edges <- edges %>% filter(value > 0)
    # 获取被点击的 node 名称
    node_name <- input$clicked_node
    print(node_name)
    # 获取与 node 相关联的 edges
    related_edges <- edges[edges$from == input$clicked_node | edges$to == input$clicked_node, ]
    
    # 获取所有关联的 node 作为关键词
    related_nodes <- unique(c(related_edges$from, related_edges$to))
    related_nodes<-related_nodes[related_nodes!=node_name]
    
    # 生成 PubMed 搜索链接
    pubmed_links <- lapply(related_nodes, function(edge_name) {
      url <- paste0("https://pubmed.ncbi.nlm.nih.gov/?term=", URLencode(paste(node_name, edge_name)))
      a(edge_name, href = url, target = "_blank")
    })
    
    related_nodes_str<-paste(related_nodes,collapse = ", ")
    
    showModal(modalDialog(
      title = paste("Details for", node_name),
      h4("Pubmed reports about target drug(s) and selected ADE:"),
      div(br()),
      do.call(tagList, pubmed_links),
      tags$head(tags$style(HTML("
        #chatres_content_3 {
          max-height: 600px;  /* 限制高度，超出可滚动 */
          max-width:1200px;
          overflow-y: auto;   /* 开启滚动 */
          word-wrap: break-word;  /* 自动换行 */
          white-space: pre-wrap;  /* 让文本换行 */
        }
      "))),
      div(br()),
      # 让 ChatGPT 结果始终可见
      h4("Wait a sec to see AI analysis："),
      div(
        id = "chatres_content_3",
        style = "max-height: 600px;max-width:1200px;max overflow-y: auto; word-wrap: break-word; white-space: pre-wrap;",
        pre(style = "white-space: pre-wrap; word-break: break-word;",withSpinner(textOutput("chatres_text3")))
      ),
      
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"  # 放大对话框
    ))
    
    # 异步获取 ChatGPT 结果
    chatres3("Loading......")
    
    future({
      reqgpt(message = paste("Please show the relationship between drugs", related_nodes_str, "and ADR", node_name,
                              "within 200 words using the following format.
                              drug funtion:
                              Is the ADR related to drug indication:(Yes/No)
                              Is the ADR a known side effect:(Yes/No)
                              relation ship between the drug and reaction:
                              "))
    }) %>% 
      then(function(result) {
        chatres3(result)  # 更新 ChatGPT 结果
      }) %>%
      catch(function(e) {
        chatres3("Error: Failed to fetch AI response!")  # 失败时返回错误信息
      })
  })
  
  chatres3 <- reactiveVal("Loading......")
  
  # 渲染 ChatGPT 结果
  output$chatres_text3 <- renderText({
    chatres3()
  })
  
  observeEvent(input$toggle_chatres3, {
    toggle("chatres_content_3")
  })
  
  
  #服务器端更新选项-----------
  updateSelectizeInput(session, "targetreac1_1", 
                       choices = indexpt_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "targetreac2_1", 
                       choices = indexpt_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "targetreac3_1", 
                       choices = indexpt_prof,  # 加载所有药物选项
                       server = TRUE)
  
  updateSelectizeInput(session, "Drugs_mpt1_1", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  
  observe({
    req(input$targetreac1_1)
    
    # 拆分逗号分隔的输入并去除首尾空格
    selected_reactions <- unique(trimws(unlist(strsplit(input$targetreac1_1, ","))))
    
    # 仅当 `selected_reactions` 与当前选中的不同时才更新，避免循环触发
    isolate({
      if (!setequal(selected_reactions, input$targetreac1_1)) {
        updateSelectizeInput(session, "targetreac1_1", 
                             selected = selected_reactions, 
                             server = TRUE)
      }
    })
  })

  updateSelectizeInput(session, "Drugs_dvd1", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_dvd2", 
                       choices = c(indexdrug_prof,"OTHER DRUGS"),
                       selected = "OTHER DRUGS",# 加载所有药物选项
                       server = TRUE)
  
  observe({
    req(input$targetreac2_1)
    
    # 拆分逗号分隔的输入并去除首尾空格
    selected_reactions <- unique(trimws(unlist(strsplit(input$targetreac2_1, ","))))
    
    # 仅当 `selected_reactions` 与当前选中的不同时才更新，避免循环触发
    isolate({
      if (!setequal(selected_reactions, input$targetreac2_1)) {
        updateSelectizeInput(session, "targetreac2_1", 
                             selected = selected_reactions, 
                             server = TRUE)
      }
    })
  })
  
  updateSelectizeInput(session, "targetreac2_1", 
                       choices = indexpt_prof,# 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt1", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt2", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt3", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt4", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt5", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt6", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt7", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  updateSelectizeInput(session, "Drugs_mpt8", 
                       choices = indexdrug_prof,  # 加载所有药物选项
                       server = TRUE)
  observe({
    req(input$targetreac3_1)
    
    # 拆分逗号分隔的输入并去除首尾空格
    selected_reactions <- unique(trimws(unlist(strsplit(input$targetreac3_1, ","))))
    
    # 仅当 `selected_reactions` 与当前选中的不同时才更新，避免循环触发
    isolate({
      if (!setequal(selected_reactions, input$targetreac3_1)) {
        updateSelectizeInput(session, "targetreac3_1", 
                             selected = selected_reactions, 
                             server = TRUE)
      }
    })
  })

}
# Run the app ----
shinyApp(ui = ui, server = server)