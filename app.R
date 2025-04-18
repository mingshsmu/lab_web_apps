# .libPaths("/home/shiny/miniconda3/envs/shiny/lib/R/library/")
library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)
library(stringr)
library(DT)
library(drc)
library(patchwork)
library(RColorBrewer)
library(ComplexHeatmap)
library(circlize)
library(ggplotify)
library(dplyr)
library(ggsci)
library(reshape2)

options(shiny.maxRequestSize = 60*1024^2)


# Define UI for application that draws a histogram
ui <- navbarPage(title = "Lab web apps",
                 tabPanel(title = "Introduction", fluidPage(theme = shinytheme("flatly")),
                          icon = icon("wand-magic-sparkles"),
                    p("Welcome to ",a("minglab.tech",href="http://minglab.tech/",style="color: #386cb0; font-weight: bold; text-decoration: underline;"),"! \nPlease enjoy the Laboratory Web Applications!",style = "font-size:30px; color:#18bc9c"),
                    # p("Welcome to use the Laboratory Web Applications!",style = "font-size:30px; color:#18bc9c"),
                    column(12,
                      fluidRow(
                        column(6,
                               p(strong("qPCR Module"),style="font-size:25px;color:#000080"),
                               img(src = "intro_qpcr_2.png",width=3000/4,height=1500/4),
                               img(src = "intro_equation_qpcr.png",width=3915/5,height=749/5),
                               
                               ),
                        
                        column(6,
                               p(strong("CCK8 Module"),style="font-size:25px;color:#b22222"),
                               img(src = "intro_cck8.png",width=2100/4,height=1500/4),
                               img(src = "intro_equation_cck8.png",width=3914/5,height=277/5),
                               
                               )
                      ),
                      fluidRow(
                        column(6,
                               p(strong("CTG Module"),style="font-size:25px;color:#000080"),
                               img(src = "intro_ctg.png",width=3900/5,height=2400/5),
                               img(src = "intro_equation_ctg.png",width=3921/5,height=1079/5),
                               
                        ),
                        
                        column(6,
                               p(strong("WB Module"),style="font-size:25px;color:#b22222"),
                               img(src = "intro_wb.png",width=4500/5,height=1800/5),
                               img(src = "intro_equation_wb.png",width=3895/6,height=1479/6),
                        )
                      ),
                      fluidRow(
                        column(6,
                               p(strong("Base Process Module"),style="font-size:25px;color:#000080"),
                               img(src = "intro_base_process.png",width=1571/2,height=178/2),
                        ),
                      ),
                      fluidRow(p("Minglab小明实验室 @ 2024 | ",a("沪ICP备2024063283号-1", target="_blank",href="https://beian.miit.gov.cn/"),style = "font-size:25px; text-align:center;"))
                      )),
                 
              # qPCR ===========================================================   
                 tabPanel(title = "qPCR",icon = icon("sun"),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel('qPCR analysis application'),
                            sidebarPanel(
                              width = 4,
                              fluidRow(
                                column(12,
                                       fileInput(inputId = "file_qpcr",label = "Upload your file",
                                                 multiple = FALSE, buttonLabel = "Browse...",
                                                 placeholder = "No file selected"))
                              ),
                             uiOutput(outputId = "side_qpcr"),
                             fluidRow(column(6,
                                             actionButton(inputId = "calculate_qpcr",label = "Submit & Calculate",icon = icon("calculator"),
                                                          class = "btn-info"))
                             )
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel(title = "RawData", dataTableOutput("rawdata_qpcr",width = "80%"),icon = icon("database")),
                                tabPanel(title = "SelectData", dataTableOutput("selectdata_qpcr"), icon = icon("table")),
                                tabPanel(title = "QualityControl",icon = icon("calendar"),
                                         uiOutput(outputId = "ui_qc_qpcr")),
                                tabPanel(title = "Result", icon = icon("square-poll-vertical"),
                                         uiOutput(outputId = "ui_res_qpcr")),
                                tabPanel(title = "Plot",icon = icon("image"),
                                         uiOutput(outputId = "ui_plot_qpcr"))
                              ),
                            )
                          )
                 ),
                 
              # CCK8 ===========================================================   
                 tabPanel(title = "CCK8", icon = icon("water"),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel("CCK8 analysis application"),
                            sidebarPanel(
                              width = 4,
                              fluidRow(
                                column(8,
                                       fileInput(inputId = "file_cck8",label = "Upload your file",
                                                 multiple = FALSE, buttonLabel = "Browse...",
                                                 placeholder = "No file selected")),
                                column(4,
                                       selectInput(inputId = "n_sheet_cck8",label = "Number of sheets:",
                                                    choices = 1:10,selected = 1,multiple = FALSE))),
                              uiOutput(outputId = "side_cck8"),
                              fluidRow(column(6,
                                              actionButton(inputId = "calculate_cck8",label = "Submit & Calculate",
                                                           icon = icon("calculator"),class = "btn-info")))
                            ),
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel(title = "RawData",dataTableOutput(outputId = "rawdata_cck8",width = "80%"),icon=icon("database")),
                                tabPanel(title = "SelectData", uiOutput(outputId = "ui_select_cck8"), icon = icon("table")),
                                tabPanel(title = "Result",icon = icon("square-poll-vertical"),
                                         uiOutput(outputId = "ui_result_cck8")),
                                tabPanel(title = "Summary",icon = icon("list"),
                                         uiOutput(outputId = "ui_summary_cck8")),
                                tabPanel(title = "Plot",icon = icon("image"),
                                         uiOutput(outputId = "ui_plot_cck8"))
                              )
                            )
                          )
                          
                 ),
                 
              # CTG ============================================================   
                 tabPanel(title = "CTG", icon = icon("react"),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel("CTG analysis application"),
                            sidebarPanel(
                              width = 4,
                              fluidRow(
                                column(8,
                                       fileInput(inputId = "file_ctg",label = "Upload your file",
                                                 multiple = FALSE, buttonLabel = "Browse...",
                                                 placeholder = "No file selected")),
                                column(4,
                                       selectInput(inputId = "n_sheet_ctg",label = "Number of sheets:",
                                                   choices = 1:16,selected = 1,multiple = FALSE))),
                              uiOutput(outputId = "side_ctg"),
                              fluidRow(column(6,
                                              actionButton(inputId = "calculate_ctg",label = "Submit & Calculate",
                                                           icon = icon("calculator"),class = "btn-info")))
                            ),
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel(title = "RawData",dataTableOutput(outputId = "rawdata_ctg",width = "80%"),icon=icon("database")),
                                tabPanel(title = "SelectData", uiOutput(outputId = "ui_select_ctg"), icon = icon("table")),
                                tabPanel(title = "Result",icon = icon("square-poll-vertical"),
                                         uiOutput(outputId = "ui_result_ctg")),
                                tabPanel(title = "Summary",icon = icon("list"),
                                         uiOutput(outputId = "ui_summary_ctg")),
                                tabPanel(title = "Heatmap",icon = icon("layer-group"),
                                         uiOutput(outputId = "ui_ctg_heatmap_control"),
                                         br(),
                                         uiOutput(outputId = "ui_ctg_heatmap_plot")),
                                tabPanel(title = "Plot",icon = icon("image"),
                                         uiOutput(outputId = "ui_plot_ctg"))
                              )
                            )
                          )
                          
                 ),
                 
              # WB =============================================================   
                 tabPanel(title = "WB",icon = icon("scissors"),
                          tags$head(
                            tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                          pageWithSidebar(
                            headerPanel("WB analysis application"),
                            sidebarPanel(
                              width = 4,
                              fluidRow(
                                column(8,
                                       fileInput(inputId = "file_wb",label = "Upload your file",
                                                 multiple = FALSE, buttonLabel = "Browse...",
                                                 placeholder = "No file selected")),
                                column(4,
                                       selectInput(inputId = "n_sheet_wb",label = "Number of sheets:",
                                                   choices = 1:10,selected = 1,multiple = FALSE))),
                              uiOutput(outputId = "side_wb"),
                              fluidRow(column(6,
                                              actionButton(inputId = "calculate_wb",label = "Submit & Calculate",
                                                           icon = icon("calculator"),class = "btn-info")))
                            ),
                            mainPanel(
                              tabsetPanel(
                                type = "tabs",
                                tabPanel(title = "RawData",
                                         dataTableOutput(outputId = "rawdata_wb",width = "80%"),icon=icon("database")),
                                tabPanel(title = "SelectData", icon = icon("table"), uiOutput(outputId = "ui_select_wb")),
                                tabPanel(title = "Result",icon = icon("square-poll-vertical"),
                                         uiOutput(outputId = "ui_result_wb")),
                                tabPanel(title = "Summary",icon = icon("list"),
                                         uiOutput(outputId = "ui_summary_wb")),
                                tabPanel(title = "Plot",icon = icon("image"),
                                         uiOutput(outputId = "ui_plot_wb"))
                              )
                            )
                          )
                 ),
              # FAM-BHQ ========================================================   
              tabPanel(title = "FAM-BHQ", icon = icon("ghost"),
                       tags$head(
                         tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                       pageWithSidebar(
                         headerPanel("FAM-BHQ analysis application"),
                         sidebarPanel(
                           width = 4,
                           fluidRow(
                             column(6,
                                    fileInput(inputId = "file_fam",label = "Upload your file",
                                              multiple = FALSE, buttonLabel = "Browse...",
                                              placeholder = "No file selected")),
                             column(3,
                                    selectInput(inputId = "n_sheet_fam",label = "Number of sheets:",
                                                choices = 1:16,selected = 1,multiple = FALSE)),
                             column(3,
                                    selectInput(inputId = "n_rep_fam",label = "Number of reps:",
                                                choices = 1:10,selected = 1,multiple = FALSE)),),
                           uiOutput(outputId = "side_fam"),
                           fluidRow(column(6,
                                           actionButton(inputId = "calculate_fam",label = "Submit & Calculate",
                                                        icon = icon("calculator"),class = "btn-info")))
                         ),
                         mainPanel(
                           tabsetPanel(
                             type = "tabs",
                             tabPanel(title = "RawData",dataTableOutput(outputId = "rawdata_fam",width = "80%"),icon=icon("database")),
                             tabPanel(title = "Summary",icon = icon("list"),
                                      uiOutput(outputId = "ui_summary_fam")),
                             tabPanel(title = "Heatmap",icon = icon("layer-group"),
                                      uiOutput(outputId = "ui_fam_heatmap_control"),
                                      br(),
                                      uiOutput(outputId = "ui_fam_heatmap_plot")),
                             tabPanel(title = "Plot",icon = icon("image"),
                                      uiOutput(outputId = "ui_plot_fam")),
                             tabPanel(title = "Plot2",icon = icon("images"),
                                      uiOutput(outputId = "ui_fam_plot2_control"),
                                      br(),
                                      uiOutput(outputId = "ui_plot2_fam")),
                           )
                         )
                       )
                       
              ),
              
              # Base Process ===========================================================   
              tabPanel(title = "Base Process",icon = icon("figma"),
                       tags$head(
                         tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                       pageWithSidebar(
                         "",
                         sidebarPanel(
                           width = 12,
                           h2("Base Process"),
                           uiOutput(outputId = "base_process_side"),
                           fluidRow(column(6,
                                           actionButton(inputId = "calculate_base",label = "Submit & Calculate",
                                                        icon = icon("calculator"),class = "btn-info"))),
                           br(),
                           h2("Mismatchs/Mutations Finder"),
                           uiOutput(outputId = "mismatch_side"),
                           fluidRow(column(6,
                                           actionButton(inputId = "calculate_base_mismatch",label = "Submit & Find",
                                                        icon = icon("calculator"),class = "btn-success")))
                           ),
                         mainPanel(
                           width = 0,
                           
                           )
                       )
                       
              ),
                 
              # Molecular Weight ===========================================================   
              tabPanel(title = "Molecular Weight",icon = icon("slack"),
                       tags$head(
                         tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                       pageWithSidebar("","",
                         sidebarPanel(
                           h2("Molecular Weight",tags$sup("1")),
                           width = 12,
                           uiOutput(outputId = "mw_main"),
                           fluidRow(column(6,
                                           actionButton(inputId = "calculate_mw",label = "Submit & Calculate",
                                                        icon = icon("calculator"),class = "btn-info"))),
                           br(),
                           h2("Concentration Unit Conversions",tags$sup("2, 3")),
                           uiOutput(outputId = "mw_convert_main"),
                           fluidRow(column(6,
                                           actionButton(inputId = "calculate_mw_convert",label = "Convert",
                                                        icon = icon("calculator"),class = "btn-success"))),
                           br(),br(),
                           fluidRow(
                             p("Reference:",style = "font-size:30px"),
                             p("1. ",a("https://www.thermofisher.cn/cn/zh/home/references/ambion-tech-support/rna-tools-and-calculators/dna-and-rna-molecular-weights-and-conversions.html",
                                       href = "https://www.thermofisher.cn/cn/zh/home/references/ambion-tech-support/rna-tools-and-calculators/dna-and-rna-molecular-weights-and-conversions.html"),
                               style = "font-size:20px"),
                             p("2. ",a("https://www.sigmaaldrich.com/HK/zh/technical-documents/technical-article/protein-biology/protein-structural-analysis/amino-acid-reference-chart",
                                       href = "https://www.sigmaaldrich.com/HK/zh/technical-documents/technical-article/protein-biology/protein-structural-analysis/amino-acid-reference-chart"),
                               style = "font-size:20px"),
                             p("3. ",a("https://www.bachem.com/knowledge-center/posters/amino-acid-chart-and-its-20-proteinogenic-amino-acids/",
                                       href = "https://www.bachem.com/knowledge-center/posters/amino-acid-chart-and-its-20-proteinogenic-amino-acids/"),
                               style = "font-size:20px")
                           )
                           
                         )
                       )
                       
              ),
              # Tutorial =======================================================
                 tabPanel(title = "Tutorial",icon = icon("book-skull"),
                          navlistPanel(widths = c(2,10),
                            tabPanel("Demo data & Software",
                              column(12,
                                p("1. Design and Analysis",style = "font-size:25px"),
                                p(a('https://pan.baidu.com/s/1JD5m1xHGZlA-2p52B6GgfQ?pwd=5lex',
                                    href = 'https://pan.baidu.com/s/1JD5m1xHGZlA-2p52B6GgfQ?pwd=5lex'),
                                  style="font-size:20px"),
                                p("2. qPCR demo data",style = "font-size:25px"),
                                downloadButton(outputId = "tu_down_qPCR_demo_eds",label = "qPCR_demo.eds"),
                                downloadButton(outputId = "tu_down_qPCR_demo_xlsx",label = "qPCR_demo.xlsx"),
                                p("3. CCK8 demo data",style = "font-size:25px"),
                                downloadButton(outputId = "tu_down_CCK8_demo_xlsx",label = "CCK8_demo.xlsx"),
                                p("4. CTG demo data",style = "font-size:25px"),
                                downloadButton(outputId = "tu_down_CTG_demo_xlsx",label = "CTG_demo.xlsx"),
                                p("5. WB demo data",style = "font-size:25px"),
                                downloadButton(outputId = "tu_down_WB_demo_xlsx",label = "WB_demo.xlsx")
                                     )),
                            tabPanel("qPCR",
                              h2(a("qPCR 分析模块使用教程")),
                              p("1. 原始数据预处理：",style="font-size:25px"),
                              p("使用“",strong("Design and Analysis"),"”软件打开qPCR的下机数据文件，例如：“qPCR_demo.eds”，",style="font-size:20px"),
                              img(src = "tu_qpcr_1.png",width=358/2,height=730/2),
                              img(src = "tu_qpcr_2.png",width=743/2,height=365/2),
                              p("依次点击“Analyze”-> “Actions: Export...”，在Export Plate中勾选“Combined all reports in one file”，
                                最后点击“Export”，将导出的文件直接上传至qPCR网页分析模块中，开始分析。",style="font-size:20px"),
                              p("2.上传文件并分析:",style="font-size:25px"),
                              img(src = "tu_qpcr_3.png",width=801/2,height=674/2),
                              p("上传预处理后得到的表格文件，设置好各种参数后，点击“Submit & Calculate”按钮即可开始分析和绘图。",style="font-size:20px"),
                              p("3.查看和下载结果：",style="font-size:25px"),
                              img(src = "tu_qpcr_4.png",width=2120/2,height=840/2),
                              p("“RawData”中可以查看原始数据;",style="font-size:20px"),
                              p("“SelectData”中可以查看所选择的要分析的数据;",style="font-size:20px"),
                              p("“QualityControl”中可以查看和下载amplification plot和Melt Curve plot;",style="font-size:20px"),
                              p("“Result”中可以下载结果表格，后续可以在GraphPad中进行绘图和统计分析;",style="font-size:20px"),
                              p("“Plot”中可以查看和下载本次qPCR的结果图（两种风格）。",style="font-size:20px"),
                              p("“点击图片或表格上方的下载按钮可直接下载该图片或表格。",style="font-size:20px"),
                              p("下载图片的宽度和高度可以在左侧工具栏中设置，",strong("设置好不用每次都点击“Submit & Calculate”"),"。
                                其中PDF为矢量图，可以在adobe illustrator中对文字、颜色等进行编辑；PNG为位图，可以放在PPT中进行汇报。
                                下载的表格为csv文件（逗号分隔符表格），可以按照Excel的方式打开。",style="font-size:20px"),
                              
                              
                                     ),
                            tabPanel("CCK8",
                              h2(a("CCK8 分析模块使用教程")),
                              p("1. 原始数据预处理：",style="font-size:25px"),
                              p("需要将下机数据手动处理成如下图的格式：",style="font-size:20px"),
                              img(src = "tu_cck8_1.png",width=619/2,height=245/2),
                              p("其中，",strong("第一列"),"为样本名，其余均为该样本所对应的OD值，例如上图中做了5个重复。",style="font-size:20px"),
                              p("同一次测的数据放在一个子表中，子表的名称不重要，但排列顺序决定了网页分析是读入的顺序。例如上表中的Sheet1, Sheet2, Sheet3, Sheet4, Sheet5, Sheet6在分析时会被重命名为D0, D1, D2, D3, D4, D5。
                                blank在哪个子表中无所谓，但需至少在一个子表含有。",style="font-size:20px"),
                              p("2.上传文件并分析：",style="font-size:25px"),
                              img(src = "tu_cck8_2.png",width=807/2,height=647/2),
                              p("上传预处理后得到的表格文件，设置好各种参数后，点击“Submit & Calculate”按钮即可开始分析和绘图。",style="font-size:20px"),
                              p("3.查看和下载结果：",style="font-size:25px"),
                              img(src = "tu_cck8_3.png",width=1655/2,height=863/2),
                              p("“RawData”中可以查看原始数据;",style="font-size:20px"),
                              p("“SelectData”中可以查看所选择的要分析的数据;",style="font-size:20px"),
                              p("“Result”中可以查看过滤后的，减去blank均值的结果，下载后可以在GraphPad中进行绘图和统计学分析;",style="font-size:20px"),
                              p("“Summary”中为各样本每天所测数据的均值和标准差;",style="font-size:20px"),
                              p("“Plot”中可以查看和下载本次qPCR的结果图（两种风格）。",style="font-size:20px"),
                              p("“点击图片或表格上方的下载按钮可直接下载该图片或表格。",style="font-size:20px"),
                              p("下载图片的宽度和高度可以在左侧工具栏中设置，",strong("设置好不用每次都点击“Submit & Calculate”"),"。
                                其中PDF为矢量图，可以在adobe illustrator中对文字、颜色等进行编辑；PNG为位图，可以放在PPT中进行汇报。
                                下载的表格为csv文件（逗号分隔符表格），可以按照Excel的方式打开。",style="font-size:20px"),
                              
                              
                                     ),
                            tabPanel("CTG",
                              h2(a("CTG 分析模块使用教程")),
                              p("1. 原始数据预处理：",style="font-size:25px"),
                              p("将CTG的下机数据按照下图中的格式处理，",style="font-size:20px"),
                              img(src = "tu_ctg_1.png",width=655/2,height=364/2),
                              p(strong("第一行第二列"),"（B1）为样本名，",strong("第一列"),"为一系列药物浓度和对照组（这里是DMSO），
                                每组可以有多个重复（上图中为3个重复），每个样本占一个子表。若有多个样本，可以新建多个子表。",style="font-size:20px"),
                              p("2. 上传数据并分析：",style="font-size:25px"),
                              img(src = "tu_ctg_2.png",width=819/2,height=745/2),
                              p("上传数据并设置好参数后点击“Submit & Calculate”按钮，进行分析和绘图。",style="font-size:20px"),
                              p("3.结果查看和下载：",style="font-size:25px"),
                              img(src = "tu_ctg_3.png",width=2555/2,height=775/2),
                              p("“RawData”中为上传的原始数据；",style="font-size:20px"),
                              p("“SelectData”中为所选择的要分析的数据；",style="font-size:20px"),
                              p("“Result”中可以查看过滤后的，除以blank均值后得到的细胞活率，下载后可以在GraphPad中进行绘图和统计学分析；",style="font-size:20px"),
                              p("“Summary”中为各样本的IC50和标准差；",style="font-size:20px"),
                              p("“Heatmap”中为细胞活率的热图。热图的颜色和图例的颜色可以在上方的栏目中进行选择。
                                下载热图的宽度和高度可以在左侧工具栏中设置，",
                                strong("设置好不用每次都点击“Submit & Calculate”，直接点击图片上方Download即可"),"。",style="font-size:20px"),
                              p("“Plot”中数据的实际值绘图和拟合图。",style="font-size:20px"),
                              p("“点击图片或表格上方的下载按钮可直接下载该图片或表格。",style="font-size:20px"),
                              p("下载图片的宽度和高度可以在左侧工具栏中设置，",strong("设置好不用每次都点击“Submit & Calculate”"),"。
                                其中PDF为矢量图，可以在adobe illustrator中对文字、颜色等进行编辑；PNG为位图，可以放在PPT中进行汇报。
                                下载的表格为csv文件（逗号分隔符表格），可以按照Excel的方式打开。",style="font-size:20px"),
                              
                              
                                     ),
                            tabPanel("WB",
                              h2(a("WB 分析模块使用教程")),
                              p("1. 原始数据预处理：",style="font-size:25px"),
                              img(src = "tu_wb_1.png",width=720/2,height=363/2),
                              p("将WB膜的曝光结果使用ImageJ 等软件计算条带灰度值，然后整理为上图所示的表格格式。",style="font-size:20px"),
                              p(strong("第一列"),"为样本名，",strong("第一行"),"为所检测的蛋白名，数值为ImageJ软件得到的每个条带的灰度值。",strong("每个子表代表一次实验"),"。
                                上图中有3个子表，代表3次重复实验。（该demo数据为随机生成的演示数据，切勿根据该演示数据设计自己的实验！！！）",style="font-size:20px"),
                              p("2. 上传数据并分析：",style="font-size:25px"),
                              img(src = "tu_wb_2.png",width=818/2,height=646/2),
                              p("上传数据并设置好参数后点击“Submit & Calculate”按钮，进行分析和绘图。",style="font-size:20px"),
                              p("3. 结果查看和下载：",style="font-size:25px"),
                              img(src = "tu_wb_3.png",width=2257/2,height=788/2),
                              p("“RawData”中为上传的原始数据；",style="font-size:20px"),
                              p("“SelectData”中为所选择的要分析的数据；",style="font-size:20px"),
                              p("“Result”中可以查看过滤后的相对蛋白表达量（相对于内参和对照组），下载后可以在GraphPad中进行绘图和统计学分析；",style="font-size:20px"),
                              p("“Summary”中为各样本的均值和标准差；",style="font-size:20px"),
                              p("“Plot”中蛋白相对表达数据的绘图。",style="font-size:20px"),
                              p("“点击图片或表格上方的下载按钮可直接下载该图片或表格。",style="font-size:20px"),
                              p("下载图片的宽度和高度可以在左侧工具栏中设置，",strong("设置好不用每次都点击“Submit & Calculate”"),"。
                                其中PDF为矢量图，可以在adobe illustrator中对文字、颜色等进行编辑；PNG为位图，可以放在PPT中进行汇报。
                                下载的表格为csv文件（逗号分隔符表格），可以按照Excel的方式打开。",style="font-size:20px"),
                              
                              
                              
                                     ),
                            tabPanel("Base Process",
                              h2(a("Base Process 模块使用教程")),
                              img(src = "tu_base_process_1.png",width=2516/2,height=837/2),
                              p("在相应文本框中输入碱基序列后点击“Submit & Calculate”按钮进行相应功能的实现。",style="font-size:20px"),
                              p("注意：DNA序列中仅允许包括“ATCGNatcgn”，RNA序列中仅允许包括“AUCGNaucgn”，输入以外的字母则会在结果中显示“Input error!”",style="font-size:20px"),
                              p("结果示例：",style="font-size:25px;color:#ff7f00"),
                              img(src = "tu_base_process_2.png",width=2494/2,height=1041/2),
                              p("其中上方红色框中为输入序列，下方橙色框中为输出的结果。",style="font-size:20px"),
                                     
                                     )
                          )
                 ),
                 
                 tabPanel(title = "Developers",icon = icon("user-astronaut"),
                 fluidRow(column(1,img(src = "logo.jpg",width=150)),
                          column(2,p(a("Ming小明", target="_blank"),style = "font-size:25px"),
                          ))
                          
                          
                             
                 ),
                 tabPanel(title = "Support",icon = icon("battery-half"),
                          
                          p("感谢各位院士杰青们的 ",a("打赏") , "(✪ω✪)",style = "font-size:25px"),
                         
                          br(),
                          img(src="wxzzm.jpg",width=300)
                             
                 ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # qPCR +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  infile_qpcr <- reactive({
    input$file_qpcr$datapath
  })
  
  output$side_qpcr <- renderUI({
    sample_choice <- NULL
    target_choice <- NULL
    outlier_qpcr <- NULL
    palette_choice <- NULL
    plot_width <- NULL
    plot_height <- NULL
    title_qpcr <- NULL
    if(!is.null(infile_qpcr())){
      ct_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Results", skip = 24)
      sample_choice <- ct_data %>% pull("Sample") %>% unique()
      target_choice <- ct_data %>% pull("Target") %>% unique()
      outlier_qpcr <- 1.5
      plot_width <- 10
      plot_height <- 8
      palette_choice <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                          "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                          "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                          "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                          "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                          "Paired (12)","Dark2 (8)","Accent (8)")
    }
    column(12, 
           fluidRow(
      column(6,
             selectInput(inputId = "sample_qpcr",label = "Select Samples: (Multi)",
                         choices = sample_choice,multiple = TRUE)),
      column(6,
             selectInput(inputId = "target_qpcr",label = "Select Targets: (Multi)",
                         choices = target_choice,multiple = TRUE))
    ),
    fluidRow(
      column(4,
             selectInput(inputId = "control_qpcr",label = "Select Control:",
                         choices = sample_choice,multiple = FALSE)),
      column(4,
             selectInput(inputId = "ref_qpcr",label = "Select Reference:",
                         choices = target_choice,multiple = FALSE)),
      column(4,
             selectInput(inputId = "palette_qpcr",label = "Select Palette:",
                         choices = palette_choice,
                         multiple = FALSE)
             )),
    fluidRow(
      column(4,
             numericInput(inputId = "outlier_qpcr",label = "Outlier Threshold",
                          value = outlier_qpcr, min = 1, step = 0.5)),
      column(8,
             textInput(inputId = "title_qpcr",label = "Plot Title:",value = title_qpcr))
      ),
    fluidRow(
      column(4,
             numericInput(inputId = "plot_width_qpcr",label = "Image Width (Download)",
                          value = plot_width, min = 1, step = 1)),
      column(4,
             numericInput(inputId = "plot_height_qpcr",label = "Image Height (Download)",
                          value = plot_height, min = 1, step = 1))
    ))
    
  })
  
  output$rawdata_qpcr <- renderDataTable({
    ct_data <- NULL
    if(!is.null(infile_qpcr())){
      ct_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Results", skip = 24) %>% 
        dplyr::select(c(2,4,5,7,10,13,18,22,23)) %>% 
        DT::datatable(options = list(pageLength = 25))
    }
    
  })
  # parameters: eventReactive
  sample_qpcr <- eventReactive(input$calculate_qpcr,{input$sample_qpcr})
  target_qpcr <- eventReactive(input$calculate_qpcr,{input$target_qpcr})
  control_qpcr <- eventReactive(input$calculate_qpcr,{input$control_qpcr})
  ref_qpcr <- eventReactive(input$calculate_qpcr,{input$ref_qpcr})
  outlier_qpcr <- eventReactive(input$calculate_qpcr,{input$outlier_qpcr})
  plot_width_qpcr <- eventReactive(input$calculate_qpcr,{input$plot_width_qpcr})
  plot_height_qpcr <- eventReactive(input$calculate_qpcr,{input$plot_height_qpcr})
  title_qpcr <- eventReactive(input$calculate_qpcr,{input$title_qpcr})
  pal_qpcr <- eventReactive(input$calculate_qpcr,{
    switch (input$palette_qpcr,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  
  # Select Data --------------------------------
  output$selectdata_qpcr <- renderDataTable({
    ct_data <- NULL
    if(!is.null(infile_qpcr())){
      ct_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Results", skip = 24) %>% 
        filter(Sample %in% sample_qpcr() & Target %in% target_qpcr()) %>% 
        dplyr::select(c(2,4,5,7,10,13,18,22,23)) %>% 
        DT::datatable(options = list(pageLength = 25))
    }
  })
  # QC -----------------------------------------
  output$ui_qc_qpcr <- renderUI({
    width1 <- length(target_qpcr())*250
    height1 <- length(sample_qpcr())*210
    fluidRow(
      h3("Amplification Plot:"), 
      downloadButton(outputId = "down_amp_png",label = "Download PNG"), 
      downloadButton(outputId = "down_amp_pdf",label = "Download PDF"),
      br(),
      plotOutput(outputId = "amp_qpcr",width = width1,height = height1),
      br(),br(),
      h3("Melt Curve Plot:"),
      downloadButton(outputId = "down_melt_png",label = "Download PNG"), 
      downloadButton(outputId = "down_melt_pdf",label = "Download PDF"),
      br(),
      plotOutput(outputId = "melt_qpcr",width = width1,height = height1)
    )
  })
  
  amp_qpcr <- reactive({
    if(!is.null(infile_qpcr())){
      amp_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Amplification Data", skip = 24) %>%
        filter(Sample %in% sample_qpcr() & Target %in% target_qpcr())
      amp_plot <- function(data,palette,title){
        sample_level <- unique(data$Sample)
        target_level <- unique(data$Target)
        data$Sample <- factor(data$Sample, levels = c(control_qpcr(),sample_level[!sample_level %in% control_qpcr()]))
        data$Target <- factor(data$Target, levels = c(ref_qpcr(),target_level[!target_level %in% ref_qpcr()]))
        data$dRn <- as.numeric(data$dRn)
        p <- ggplot(data = data, aes(x=`Cycle Number`,y=dRn,color=Target,group=Well))+
          geom_line()+
          facet_grid(Sample~Target)+
          theme_bw()+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                strip.background = element_rect(fill = "#eaeae0"),
                strip.text = element_text(size=12),
                axis.title = element_text(size=15,face = "bold"),
                axis.text = element_text(size=12),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_color_manual(values = palette)+
          ggtitle(title)
        return(p)
      }
      amp_plot(data = amp_data,palette = pal_qpcr(),title=title_qpcr())
    }
  })
  output$amp_qpcr <- renderPlot({
    amp_qpcr()
  })
  
  melt_qpcr <- reactive({
    if(!is.null(infile_qpcr())){
      melt_plot <- function(data,palette,title){
        sample_level <- unique(data$Sample)
        target_level <- unique(data$Target)
        data$Sample <- factor(data$Sample, levels = c(control_qpcr(),sample_level[!sample_level %in% control_qpcr()]))
        data$Target <- factor(data$Target, levels = c(ref_qpcr(),target_level[!target_level %in% ref_qpcr()]))
        data$Derivative <- as.numeric(data$Derivative)
        p <- ggplot(data = data, aes(x=Temperature,y=Derivative,color=Target,group=`Well Position`))+
          geom_line()+
          facet_grid(Sample~Target)+
          theme_bw()+
          theme(legend.position = "none",
                panel.grid = element_blank(),
                strip.background = element_rect(fill = "#eaeae0"),
                strip.text = element_text(size=12),
                axis.title = element_text(size=15,face = "bold"),
                axis.text = element_text(size=12),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_color_manual(values = palette)+
          ggtitle(title)
        return(p)
      }
      melt_data <-  readxl::read_excel(path = infile_qpcr(),sheet = "Melt Curve Raw", skip = 24) %>% 
        filter(Target %in% target_qpcr())
      ct_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Results", skip = 24) %>% 
        filter(Sample %in% sample_qpcr() & Target %in% target_qpcr()) %>% 
        dplyr::select(c("Well Position","Sample"))
      melt_data <- inner_join(x = melt_data,y = ct_data,by = "Well Position")
      melt_plot(data = melt_data,palette = pal_qpcr(),title=title_qpcr())
    }
  })
  output$melt_qpcr <- renderPlot({
    melt_qpcr()
  })
  
  # result ------------------------------------------
  res_qpcr <- reactive({
    if(!is.null(infile_qpcr())){
      ct_data <- readxl::read_excel(path = infile_qpcr(),sheet = "Results", skip = 24) %>%
        filter(Sample %in% sample_qpcr() & Target %in% target_qpcr()) %>% 
        dplyr::select(Sample, Target, Cq)
      colnames(ct_data) <- c("samples","gene","ct")
      
      qpcr_analysis <- function(data,ref,con,fold){
        data$ct <- ifelse(data$ct!="Undetermined",data$ct,40)
        sample_level <- unique(data$samples)
        target_level <- unique(data$gene)
        data$ct <- as.numeric(data$ct)
        data$samples <- factor(data$samples, levels = c(control_qpcr(),sample_level[!sample_level %in% control_qpcr()]))
        data$gene <- factor(data$gene, levels = c(ref_qpcr(),target_level[!target_level %in% ref_qpcr()]))
        data <- data %>% group_by(gene,samples) %>% 
          mutate(ct_mean = mean(ct)) %>% 
          mutate(ct_sd = sd(ct)) %>% 
          mutate(ct_iqr = IQR(ct)) %>% 
          mutate(keep = ifelse(abs(ct-ct_mean) > fold*ct_sd, FALSE,TRUE)) %>% 
          filter(keep==TRUE)
        
        ref_mean <- data %>% filter(gene==ref) %>% 
          group_by(samples) %>%
          summarise(ref_mean=mean(ct))
        ref_mean <- setNames(ref_mean$ref_mean, ref_mean$samples)
        
        res <- data.frame()
        for (i in names(ref_mean)) {
          res_tmp <- data %>% filter(samples==i) %>% 
            mutate(dct = ct-ref_mean[i])
          res <- rbind(res,res_tmp)
        }
        
        con_dmean <- res %>% filter(samples==con) %>% 
          group_by(gene) %>% 
          summarise(con_dmean = mean(dct))
        con_dmean <- setNames(con_dmean$con_dmean, con_dmean$gene)
        
        result <- data.frame()
        for (j in names(con_dmean)) {
          res_tmp <- res %>% filter(gene==j) %>% 
            mutate(ddct = dct - con_dmean[j])
          result <- rbind(result, res_tmp)
        }
        
        # final
        result$exp <- 2^(-result$ddct)
        result <- result %>% group_by(gene,samples) %>% 
          mutate(exp_mean = mean(exp)) %>% 
          mutate(exp_sd = sd(exp))
        return(result)
      }
      qpcr_analysis(data = ct_data,ref = ref_qpcr(),
                    con = control_qpcr(),fold = outlier_qpcr())
    }
  })
    
  output$result_qpcr <- renderDataTable({
    res_qpcr() %>% dplyr::select(1,2,3,10,11,12) %>% 
      setNames(c("Sample","Target","Cq","Expression","Exp_mean","Exp_sd")) %>% 
      DT::datatable(options = list(pageLength = 25))
  })
  # hide the button
  down_res_label <- eventReactive(eventExpr = input$calculate_qpcr,
                                  {"Download Table"})
  output$ui_res_qpcr <- renderUI({ 
    if(!is.null(infile_qpcr())){
      fluidRow(downloadButton(outputId = "down_res_table",label = down_res_label()),
               dataTableOutput("result_qpcr",width = "80%"))
    }
  })
  
  # plot -----------------------------------------
  plot_qpcr <- reactive({
    if(!is.null(infile_qpcr())){
      qpcr_plot <- function(result, summ, palette,title){
        p1 <- ggplot()+
          geom_bar(data=summ,mapping=aes(x=samples,y=exp_mean,fill=samples,color=samples),alpha=1,stat = "identity")+
          geom_errorbar(data=summ,mapping=aes(x=samples,ymin=exp_mean-exp_sd,ymax=exp_mean+exp_sd,
                                              color=samples),width=0.3)+
          geom_jitter(data=result,mapping = aes(x=samples,y=exp),width = 0.3)+
          facet_wrap(.~gene,scales = "free_y",nrow = 1)+
          theme_bw()+
          theme(panel.grid = element_blank(),
                legend.title = element_blank(),
                legend.text = element_text(size = 11,face = "bold"),
                axis.text.x = element_text(angle = 45,hjust = 1),
                strip.background = element_rect(fill = "#eaeae0"),
                strip.text = element_text(size=14),
                axis.title = element_text(size=15,face = "bold"),
                axis.text = element_text(size=14),
                axis.ticks = element_line(linewidth = 1),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          labs(x="",y="Relative mRNA expression level")+
          scale_fill_manual(values = palette)+
          scale_color_manual(values = palette)+
          ggtitle(title)
        return(p1)
      }
      
      qpcr_summary <- function(result){
        summ <- result %>% distinct(samples, gene,.keep_all = T)
        return(summ)
      }
      summ <- qpcr_summary(result = res_qpcr())
      summ <- summ[,c(1,2,11,12)]
      qpcr_plot(result = res_qpcr(),summ = summ,palette = pal_qpcr(),title = title_qpcr())
    }
  })
  
  plot_qpcr2 <- reactive({
    if(!is.null(infile_qpcr())){
      qpcr_plot2 <- function(result, palette,title){
        p1 <- ggplot(result)+
          stat_summary(aes(x=gene,y=exp,fill=samples),geom="bar",color="black",
                       position=position_dodge(0.75),width=0.6,fun.data = "mean_se")+
          stat_summary(aes(x=gene,y=exp,group=samples),geom="errorbar",
                       position=position_dodge(0.75),width=0.2,fun.data = "mean_se")+
          geom_point(aes(x=gene,y=exp,group=samples),position = position_dodge(0.75))+
          scale_y_continuous(expand = c(0,0),limits = c(0, max(result$exp)*1.1))+
          theme_classic()+
          labs(x=NULL,y="Relative mRNA expression level",fill="Sample")+
          theme(axis.title = element_text(size = 15,face = "bold"),
                axis.text = element_text(size = 14,face = "bold",vjust = 0.5),
                axis.line = element_line(linewidth = 1),
                axis.ticks = element_line(linewidth = 1),
                legend.text = element_text(size = 11,face = "bold"),
                legend.title = element_text(size = 14,face = "bold"),
                legend.position = "right",
                legend.background = element_rect(linewidth = 1),
                legend.box.background = element_rect(linewidth = 1),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_fill_manual(values = palette)+
          ggtitle(title)
        return(p1)
      }
      
      qpcr_plot2(result = res_qpcr(),palette = pal_qpcr(),title=title_qpcr())
    }
  })
  
  output$plot_qpcr <- renderPlot({
    plot_qpcr()
  })
  output$plot_qpcr2 <- renderPlot({
    plot_qpcr2()
  })
  
  output$ui_plot_qpcr <- renderUI({
    width1 <- length(target_qpcr())*250
    height1 <- 2.5*210
    column(12,
           h3("Plot (Style 1):"),
           downloadButton(outputId = "down_plot_png",label = "Download PNG"), 
           downloadButton(outputId = "down_plot_pdf",label = "Download PDF"),
           br(),
           plotOutput("plot_qpcr",width = width1,height = height1),
           br(),
           h3("Plot (Style 2):"),
           downloadButton(outputId = "down_plot_png2",label = "Download PNG"), 
           downloadButton(outputId = "down_plot_pdf2",label = "Download PDF"),
           br(),
           plotOutput("plot_qpcr2",width = width1,height = height1),
           )
  })
  
  # Download ---------------------------------------
  output$down_amp_png <- downloadHandler(
    filename = paste0("qPCR_QC_AmpPlot_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             dpi = 300, plot = amp_qpcr())
    }
  )
  
  output$down_amp_pdf <- downloadHandler(
    filename = paste0("qPCR_QC_AmpPlot_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             plot = amp_qpcr())
    }
  )
  
  output$down_melt_png <- downloadHandler(
    filename = paste0("qPCR_QC_MeltPlot_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             dpi = 300, plot = melt_qpcr())
    }
  )
  
  output$down_melt_pdf <- downloadHandler(
    filename = paste0("qPCR_QC_MeltPlot_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
            plot = melt_qpcr())
    }
  )
  
  output$down_plot_png <- downloadHandler(
    filename = paste0("qPCR_Plot_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             dpi = 300, plot = plot_qpcr())
    }
  )
  
  output$down_plot_pdf <- downloadHandler(
    filename = paste0("qPCR_Plot_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             plot = plot_qpcr())
    }
  )
  
  output$down_plot_png2 <- downloadHandler(
    filename = paste0("qPCR_Plot2_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             dpi = 300, plot = plot_qpcr2())
    }
  )
  
  output$down_plot_pdf2 <- downloadHandler(
    filename = paste0("qPCR_Plot2_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_qpcr, height = input$plot_height_qpcr,
             plot = plot_qpcr2())
    }
  )
  
  output$down_res_table <- downloadHandler(
    filename = paste0("qPCR_Result_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(res_qpcr() %>% dplyr::select(1,2,3,10,11,12) %>% 
                  setNames(c("Sample","Target","Cq","Expression","Exp_mean","Exp_sd")),
                file)
    }
  )
  
  # CCK8 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read_data
  infile_cck8 <- reactive({
    input$file_cck8$datapath
  })
  rawdata_cck8 <- reactive({
    CCK8_read <- function(path,n_sheet){
      read_CCK8 <- function(path,i){
        dat <- readxl::read_excel(path = path,sheet = i,col_names = FALSE)
        colnames(dat) <- c("Sample",paste0("Rep",1:(ncol(dat)-1)))
        dat$Day <- paste0("D",i-1)
        return(dat)
      }
      dat <- lapply(1:n_sheet, function(i){
        dat_tmp <- read_CCK8(path = path,i = i)
        return(dat_tmp)
      })
      dat <- do.call(rbind,dat)
      return(dat)
    }
    # dat 
    dat <- CCK8_read(path = infile_cck8(),n_sheet = input$n_sheet_cck8)
    return(dat)
  })
  output$rawdata_cck8 <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_cck8())){
      dat <- rawdata_cck8()
      dat <- dat[,c(ncol(dat),1:(ncol(dat)-1))]
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  output$side_cck8 <- renderUI({
    group_choice_cck8 <- NULL
    day_choice_cck8 <- NULL
    palette_cck8 <- NULL
    outlier_cck8 <- NULL
    plot_width_cck8 <- NULL
    plot_height_cck8 <- NULL
    if(!is.null(infile_cck8())){
      group_choice_cck8 <- rawdata_cck8() %>% pull("Sample") %>% unique()
      day_choice_cck8 <- rawdata_cck8() %>% pull("Day") %>% unique()
      outlier_cck8 <- 1.5
      plot_width_cck8 <- 10
      plot_height_cck8 <- 8
      palette_cck8 <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                          "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                          "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                          "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                          "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                          "Paired (12)","Dark2 (8)","Accent (8)")
    }
    column(12, 
           fluidRow(
             column(6,
                    selectInput(inputId = "sample_cck8",label = "Select Samples: (Multi)",
                                choices = group_choice_cck8, multiple = TRUE)),
             column(6,
                    selectInput(inputId = "day_cck8",label = "Select Days: (Multi)",
                                choices = day_choice_cck8, multiple = TRUE))
           ),
           fluidRow(
             column(4,
                    selectInput(inputId = "blank_cck8",label = "Select Blank:",
                                choices = group_choice_cck8, multiple = FALSE)),
             column(4,
                    selectInput(inputId = "palette_cck8",label = "Select Palette:",
                                choices = palette_cck8,
                                multiple = FALSE)),
             column(4,
                    numericInput(inputId = "outlier_cck8",label = "Outlier Threshold:",
                                 value = outlier_cck8, min = 1, step = 0.5))
             ),
             fluidRow(
               column(4,
                      textInput(inputId = "title_cck8",label = "Plot Title:",
                                value = NULL)),
               column(4,
                      numericInput(inputId = "plot_width_cck8",label = "Image Width (Download)",
                                   value = plot_width_cck8, min = 1, step = 1)),
               column(4,
                      numericInput(inputId = "plot_height_cck8",label = "Image Height (Download)",
                                   value = plot_height_cck8, min = 1, step = 1))
             )
           )
  })
  # parameters
  sample_cck8 <- eventReactive(input$calculate_cck8,{input$sample_cck8})
  day_cck8 <- eventReactive(input$calculate_cck8,{input$day_cck8})
  blank_cck8 <- eventReactive(input$calculate_cck8,{input$blank_cck8})
  outlier_cck8 <- eventReactive(input$calculate_cck8,{input$outlier_cck8})
  title_cck8 <- eventReactive(input$calculate_cck8,{input$title_cck8})
  palette_cck8 <- eventReactive(input$calculate_cck8,{
    switch (input$palette_cck8,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  # select data
  select_cck8 <- eventReactive(input$calculate_cck8,{
    rawdata_cck8() %>% dplyr::filter(Sample %in% sample_cck8()) %>% 
      dplyr::filter(Day %in% day_cck8())
  })
  output$select_cck8 <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_cck8())){
      dat <- select_cck8()
      dat <- dat[,c(ncol(dat),1:(ncol(dat)-1))]
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  output$ui_select_cck8 <- renderUI({
    dataTableOutput(outputId = "select_cck8",width = "80%")
  })
  # analysis
  cck8_analysis_dat <- eventReactive(input$calculate_cck8,{
    cck8_analysis_fun <- function(dat,blank_name,fold){
      dat <- reshape2::melt(dat,id = c("Sample","Day"))
      blank_val <- dat %>% filter(Sample==blank_name) %>% pull(value) %>% 
        mean(na.rm = T) # NA value should be removed
      dat$value <- dat$value - blank_val
      dat <- dat %>% filter(Sample!=blank_name)
      dat$Sample <- factor(dat$Sample,levels = unique(dat$Sample))
      dat <- dat %>% group_by(Day,Sample) %>% 
        mutate(mean=mean(value,na.rm = T),sd=sd(value,na.rm = T)) %>% 
        mutate(keep=ifelse(abs(value-mean)>fold*sd,FALSE,TRUE)) %>% 
        filter(keep==TRUE) %>% 
        dplyr::select(1:4)
      return(dat)
    }
    cck8_analysis_fun(dat = select_cck8(),blank_name = blank_cck8(),fold = outlier_cck8())
  })
  
  # output result
  result_cck8 <- reactive({
    reshape2::dcast(cck8_analysis_dat(),formula = Day+Sample~variable)
    
  })
  output$result_cck8 <- renderDataTable({
    DT::datatable(data = result_cck8(),
                  options = list(pageLength=25))
  })
  
  down_res_label_cck8 <- eventReactive(eventExpr = input$calculate_cck8,
                                       {"Download Table"})
  output$ui_result_cck8 <- renderUI({
    if(!is.null(infile_cck8())){
      column(12,
             fluidRow(downloadButton(outputId = "down_res_table_cck8",label = down_res_label_cck8())),
             br(),
             dataTableOutput("result_cck8",width = "80%"))
    }
  })
  
  # output summary
  summary_cck8 <- reactive({
    summary_cck8_fun <- function(dat){
      summ <- dat %>% group_by(Day,Sample) %>% 
        summarise(mean = mean(value),sd=sd(value)) %>% 
        mutate(lower=mean-sd,upper=mean+sd)
      summ$Day <- gsub("D","",summ$Day) %>% as.numeric()
      return(summ)
    }
    summary_cck8_fun(dat = cck8_analysis_dat()) %>% arrange(desc(Day))
  })
  output$summary_cck8 <- renderDataTable({
    summary_cck8()  %>% 
      DT::datatable(options = list(pageLength=25))
  })
  
  down_summ_label_cck8 <- eventReactive(eventExpr = input$calculate_cck8,
                                       {"Download Summary"})
  output$ui_summary_cck8 <- renderUI({
    if(!is.null(infile_cck8())){
      column(12,
             fluidRow(downloadButton(outputId = "down_summ_table_cck8",label = down_summ_label_cck8())),
             br(),
             dataTableOutput("summary_cck8",width = "80%"))
      
    }
  })
  # plot
  cck8_plot_line <- reactive({
    if(!is.null(infile_cck8())){
      CCK8_plot_line_fun <- function(summ,palette,title){
        p1 <- ggplot()+
          geom_point(data = summ,aes(x=Day,y=mean,color=Sample),size=4)+
          geom_line(data = summ,aes(x=Day,y=mean,color=Sample),lwd=1)+
          geom_errorbar(data = summ,aes(x=Day,ymin=lower,ymax=upper,color=Sample),
                        width=0.1,lwd=1)+
          theme_classic()+
          labs(y="OD value (450 nm)")+
          theme(axis.title = element_text(size = 15,face = "bold"),
                axis.text = element_text(size = 14),
                legend.position = "right",
                legend.background = element_rect(linewidth = 1),
                legend.box.background = element_rect(linewidth = 1),
                legend.text = element_text(size = 11,face = "bold"),
                legend.title = element_text(size = 14,face = "bold"),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_color_manual(values = palette)+
          ggtitle(title)
        return(p1)
      }
      CCK8_plot_line_fun(summ = summary_cck8(),palette = palette_cck8(),title=title_cck8())
    }
  })
  
  cck8_plot_box <- reactive({
    if(!is.null(infile_cck8())){
      CCK8_plot_box_fun <- function(dat,palette,title){
        p1 <- ggplot(dat,aes(x=Sample,y=value,fill=Sample))+
          geom_boxplot(outlier.color = NA)+
          geom_jitter(width = 0.2)+
          labs(x="",y="OD value (450 nm)")+
          facet_wrap(~Day,nrow = ceiling(length(unique(dat$Day))/3),ncol = 3)+
          theme_bw()+
          theme(panel.grid = element_blank(),
                axis.title = element_text(size = 15,face = "bold"),
                axis.text.x = element_text(size = 14,face = "bold",angle = 45,hjust = 1),
                axis.text.y = element_text(size = 14,face = "bold"),
                axis.ticks = element_line(linewidth = 1),
                legend.text = element_text(size = 11,face = "bold"),
                legend.title = element_text(size = 14,face = "bold"),
                legend.position = "right",
                legend.background = element_rect(linewidth = 1),
                legend.box.background = element_rect(linewidth = 1),
                strip.background = element_rect(fill = "#eaeae0"),
                strip.text = element_text(size=12),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_fill_manual(values = palette)+
          ggtitle(title)
        return(p1)
      }
      CCK8_plot_box_fun(dat = cck8_analysis_dat(),palette = palette_cck8(),title=title_cck8())
    }
  })
  output$cck8_plot_line <- renderPlot({
    cck8_plot_line()
  })
  output$cck8_plot_box <- renderPlot({
    cck8_plot_box()
  })
  
  output$ui_plot_cck8 <- renderUI({
    if(!is.null(infile_cck8())){
      n_sample <- length(select_cck8() %>% pull("Sample") %>% unique())
      width1 <- n_sample*100
      height1 <- n_sample*80
      width2 <- 3*250
      height2 <- eventReactive(input$calculate_cck8,{ceiling(as.numeric(isolate(input$n_sheet_cck8)) / 3)*250})()
      
      eventReactive(input$calculate_cck8,
                    {column(12,
                            h3("Plot Line:"),
                            downloadButton(outputId = "cck8_down_plot_line_png",label = "Download PNG"), 
                            downloadButton(outputId = "cck8_down_plot_line_pdf",label = "Download PDF"),
                            br(),
                            plotOutput("cck8_plot_line",width = width1,height = height1),
                            br(),
                            h3("Plot Box:"),
                            downloadButton(outputId = "cck8_down_plot_box_png",label = "Download PNG"), 
                            downloadButton(outputId = "cck8_down_plot_box_pdf",label = "Download PDF"),
                            br(),
                            plotOutput("cck8_plot_box",width = width2,height = height2)
                    )})()
    }
    
  })
  
  # download
  output$down_res_table_cck8 <- downloadHandler(
    filename = paste0("CCK8_Result_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(result_cck8(),file)
    }
  )
  output$down_summ_table_cck8 <- downloadHandler(
    filename = paste0("CCK8_Summary_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(summary_cck8(),file)
    }
  )
  output$cck8_down_plot_line_png <- downloadHandler(
    filename = paste0("CCK8_Plot_line_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_cck8, height = input$plot_height_cck8,
             dpi = 300, plot = cck8_plot_line())
    }
  )
  output$cck8_down_plot_line_pdf <- downloadHandler(
    filename = paste0("CCK8_Plot_line_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_cck8, height = input$plot_height_cck8,
             plot = cck8_plot_line())
    }
  )
  output$cck8_down_plot_box_png <- downloadHandler(
    filename = paste0("CCK8_Plot_box_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_cck8, height = input$plot_height_cck8,
             dpi = 300, plot = cck8_plot_box())
    }
  )
  output$cck8_down_plot_box_pdf <- downloadHandler(
    filename = paste0("CCK8_Plot_box_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_cck8, height = input$plot_height_cck8,
             plot = cck8_plot_box())
    }
  )
  # CTG ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read_data
  infile_ctg <- reactive({
    input$file_ctg$datapath
  })
  rawdata_ctg <- reactive({
    CTG_read <- function(path,n_sheet){
      read_CTG <- function(path,i){
        dat <- readxl::read_excel(path = path,sheet = i,col_names = TRUE)
        group <- grep("[.][.][.]",x = colnames(dat),value = T,invert = T)
        colnames(dat) <- c("Dose",paste0("Rep",1:(ncol(dat)-1)))
        dat$Group <- group
        return(dat)
      }
      
      dat <- lapply(1:n_sheet, function(i){
        dat_tmp <- read_CTG(path = path,i = i)
        return(dat_tmp)
      })
      
      dat <- do.call(rbind,dat)
      return(dat)
    }
    CTG_read(path = infile_ctg(),n_sheet = input$n_sheet_ctg)
  })
  
  output$rawdata_ctg <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_ctg())){
      dat <- rawdata_ctg()
      dat <- dat[,c(ncol(dat),1:(ncol(dat)-1))]
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  
  output$side_ctg <- renderUI({
    sample_choice_ctg <- NULL
    dose_choice_ctg <- NULL
    palette_ctg <- NULL
    outlier_ctg <- NULL
    plot_width_ctg <- NULL
    plot_height_ctg <- NULL
    already_log_ctg <- NULL
    log_base_ctg <- NULL
    if(!is.null(infile_ctg())){
      sample_choice_ctg <- rawdata_ctg() %>% pull("Group") %>% unique()
      dose_choice_ctg <- rawdata_ctg() %>% pull("Dose") %>% unique()
      outlier_ctg <- 1.5
      plot_width_ctg <- 10
      plot_height_ctg <- 8
      already_log_ctg <- c(FALSE,TRUE)
      log_base_ctg <- 10
      palette_ctg <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                        "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                        "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                        "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                        "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                        "Paired (12)","Dark2 (8)","Accent (8)")
    }
    
    column(12, 
           fluidRow(
             column(6,
                    selectInput(inputId = "sample_ctg",label = "Select Samples: (Multi)",
                                choices = sample_choice_ctg, multiple = TRUE)),
             column(6,
                    selectInput(inputId = "dose_ctg",label = "Select Dose: (Multi)",
                                choices = dose_choice_ctg, multiple = TRUE))
           ),
           fluidRow(
             column(6,
                    selectInput(inputId = "blank_ctg",label = "Select Blank:",
                                choices = dose_choice_ctg, multiple = FALSE)),
             column(6,
                    selectInput(inputId = "palette_ctg",label = "Select Palette:",
                                choices = palette_ctg, multiple = FALSE))
           ),
           fluidRow(
             column(4,
                    selectInput(inputId = "already_log_ctg",label = "Already log(Dose)?",
                                choices = already_log_ctg, multiple = FALSE)),
             column(4,
                    numericInput(inputId = "log_base_ctg",label = "Used log base:",
                                 value = log_base_ctg,min = 0.0001,step = 1)),
             column(4,
                    numericInput(inputId = "outlier_ctg",label = "Outlier Threshold:",
                                 value = outlier_ctg, min = 1, step = 0.5)),
             ),
           fluidRow(
             column(4,
                    textInput(inputId = "title_ctg",label = "Plot Title:",
                              value = NULL)),
             column(4,
                    numericInput(inputId = "plot_width_ctg",label = "Image Width (Download)",
                                 value = plot_width_ctg, min = 1, step = 1)),
             column(4,
                    numericInput(inputId = "plot_height_ctg",label = "Image Height (Download)",
                                 value = plot_height_ctg, min = 1, step = 1))
           )
    )
  })
  
  # parameters
  sample_ctg <- eventReactive(input$calculate_ctg,{input$sample_ctg})
  dose_ctg <- eventReactive(input$calculate_ctg,{input$dose_ctg})
  blank_ctg <- eventReactive(input$calculate_ctg,{input$blank_ctg})
  outlier_ctg <- eventReactive(input$calculate_ctg,{input$outlier_ctg})
  title_ctg <- eventReactive(input$calculate_ctg,{input$title_ctg})
  already_log_ctg <- eventReactive(input$calculate_ctg,{input$already_log_ctg})
  log_base_ctg <- eventReactive(input$calculate_ctg,{as.numeric(input$log_base_ctg)})
  palette_ctg <- eventReactive(input$calculate_ctg,{
    switch (input$palette_ctg,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  # select data
  select_ctg <- eventReactive(input$calculate_ctg,{
    rawdata_ctg() %>% dplyr::filter(Group %in% sample_ctg()) %>% 
      dplyr::filter(Dose %in% dose_ctg())
  })
  output$select_ctg <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_ctg())){
      dat <- select_ctg()
      dat <- dat[,c(ncol(dat),1:(ncol(dat)-1))]
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  output$ui_select_ctg <- renderUI({
    dataTableOutput(outputId = "select_ctg",width = "80%")
  })
  
  
  # analysis_1
  ctg_analysis_dat1 <- eventReactive(input$calculate_ctg,{
    # print("ctg_analysis1_start...")
    ctg_analysis_fun1 <- function(dat_wide,blank,already_log,log_base){
      # choose blank
      blank_mean <- dat_wide %>% dplyr::filter(Dose==blank)
      blank_mean <- blank_mean %>% 
        mutate(mean = rowMeans(blank_mean[,grep("^Rep",x = colnames(blank_mean))]))
      blank_mean <- setNames(blank_mean$mean,blank_mean$Group)
      
      dat <- dat_wide %>% filter(Dose != blank) %>% reshape2::melt(id=c("Dose","Group"))
      dat$Group <- factor(dat$Group,levels = unique(dat$Group))  
      # / blank
      res <- data.frame()
      for (i in names(blank_mean)) {
        res_tmp <- dat %>% filter(Group==i) %>% 
          mutate(dval = value/blank_mean[i] * 100)
        res <- rbind(res, res_tmp)
      }
      
      # check if already log
      res$Dose <- as.numeric(res$Dose)
      if(already_log){
        # data for calculation
        res$Dose <- log_base ^ res$Dose
      }
      return(res)
    }
    ctg_analysis_fun1(dat_wide = select_ctg(),blank = blank_ctg(),
                      already_log = already_log_ctg(),log_base = log_base_ctg())
  })
  ## for heatmap plot
  res_wide_heat <- reactive({
    # print("res_heat_done...")
    reshape2::dcast(data = ctg_analysis_dat1() %>% dplyr::select(-value),
                    formula = Group+Dose~variable,
                    value.var = "dval")
  })

  # analysis_2
  res_ctg <- eventReactive(input$calculate_ctg,{
    # print("ctg_analysis_2_start...")
    ctg_analysis_fun2 <- function(res,fold){
      # QC
      res <- res %>% group_by(Dose,Group) %>% 
        mutate(mean=mean(dval),sd=sd(dval)) %>% 
        mutate(keep=ifelse(abs(dval-mean)>fold*sd,FALSE,TRUE)) %>% 
        filter(keep==TRUE) %>% 
        dplyr::select(1:3,5)
      return(res)
    }
    ctg_analysis_fun2(res = ctg_analysis_dat1(),fold = outlier_ctg())
  })
  
  ### result_ctg for display
  result_ctg <- reactive({
    reshape2::dcast(res_ctg(),formula = Group+Dose~variable,value.var = "dval")
  })
  output$result_ctg <- renderDataTable({
    DT::datatable(data = result_ctg(),options = list(pageLength=25))
  })
  down_res_label_ctg <- eventReactive(eventExpr = input$calculate_ctg,
                                       {"Download Table"})
  output$ui_result_ctg <- renderUI({
    if(!is.null(infile_ctg())){
      column(12,
             fluidRow(downloadButton(outputId = "down_res_table_ctg",label = down_res_label_ctg())),
             br(),
             dataTableOutput("result_ctg",width = "80%"))
    }
  })

  # output summary
  summary_ctg <- reactive({
    summary_ctg_fun <- function(res){
      drm_res <- drc::drm(dval ~ Dose,Group,data = res,
                          fct = LL.5(fixed = c(NA,0,100,NA,1),names = c('b', 'c', 'd', 'EC50', 'f')))
      
      ## result_IC50
      res_ic50 <- as.data.frame(summary(drm_res)$coefficients)[-c(1:length(select_ctg() %>% pull("Group") %>% unique())),]
      rownames(res_ic50) <- gsub("EC50:","IC50: ",rownames(res_ic50))
      res_ic50$Group <- gsub("IC50: ","",rownames(res_ic50))
      if(nrow(res_ic50)==1){
        res_ic50$Group <- res$Group[1]
      }
      return(res_ic50)
    }
    summary_ctg_fun(res = res_ctg())
  })
  
  output$summary_ctg <- renderDataTable({
    res_ic50 <- summary_ctg()
    summ_ctg <- res_ic50[,c(ncol(res_ic50),1:(ncol(res_ic50)-1))]
    colnames(summ_ctg)[2] <- "IC50_estimate"
    summ_ctg  %>%
      DT::datatable(options = list(pageLength=25))
  })

  down_summ_label_ctg <- eventReactive(eventExpr = input$calculate_ctg,
                                        {"Download Summary"})
  output$ui_summary_ctg <- renderUI({
    if(!is.null(infile_ctg())){
      column(12,
             fluidRow(downloadButton(outputId = "down_summ_table_ctg",label = down_summ_label_ctg())),
             br(),
             dataTableOutput("summary_ctg",width = "80%"))

    }
  })
  
  # ctg_heatmap
  output$ui_ctg_heatmap_control <- renderUI({
    palette_group <- NULL
    palette_dose <- NULL
    palette_rep <- NULL
    palette_cv <- NULL
    palette_in_discrete <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                    "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                    "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                    "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                    "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                    "Paired (12)","Dark2 (8)","Accent (8)")
    
    palette_in_continuous <- rownames(RColorBrewer::brewer.pal.info %>% filter(category %in% c("div","seq")))
    
    if(!is.null(infile_ctg())){
      palette_group <- palette_in_discrete
      palette_dose <- palette_in_continuous
      palette_rep <- palette_in_discrete
      palette_cv <- palette_in_continuous
      eventReactive(input$calculate_ctg,{
        column(12,
               fluidRow(
                 column(6,selectInput(inputId = "palette_group_ctg",label = "Palette for Group:",
                                      choices = palette_group, selected = sample(palette_group,1),multiple = FALSE)),
                 column(6,selectInput(inputId = "palette_dose_ctg",label = "Palette for Dose:",
                                      choices = palette_dose, selected = sample(palette_dose,1),multiple = FALSE))),
               fluidRow(
                 column(6,selectInput(inputId = "palette_rep_ctg",label = "Palette for Rep:",
                                      choices = palette_rep, selected = sample(palette_rep,1),multiple = FALSE)),
                 column(6,selectInput(inputId = "palette_cv_ctg",label = "Palette for Cell viability:",
                                      choices = palette_cv, selected = sample(palette_cv,1),multiple = FALSE))
               ))
      })()
    }
    
  })
  
  # heatmap parameters:
  ctg_heatmap_params <- reactive({
    palette_trans_fun <- function(palette){
      switch (palette,
              "Lancet (9)" = ggsci::pal_lancet()(9),
              "NPG (10)" = ggsci::pal_npg()(10),
              "NEJM (8)" = ggsci::pal_nejm()(8),
              "JCO (10)" = ggsci::pal_jco()(10),
              "JAMA (7)" = ggsci::pal_jama()(7),
              "AAAS (10)" = ggsci::pal_aaas()(10),
              "D3 (10)" = ggsci::pal_d3()(10),
              "Futurama (12)" = ggsci::pal_futurama()(12),
              "GSEA (12)" = ggsci::pal_gsea()(12),
              "IGV (51)" = ggsci::pal_igv()(51),
              "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
              "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
              "Simpsons (16)" = ggsci::pal_simpsons()(16),
              "Star Trek (7)" = ggsci::pal_startrek()(7),
              "Tron Legacy (7)" = ggsci::pal_tron()(7),
              "Chicago (9)" = ggsci::pal_uchicago()(9),
              "UCSC (26)" = ggsci::pal_ucscgb()(26),
              "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
              "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
              "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
              "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
              "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
              "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
              "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
              "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent"),
              "BrBG" = RColorBrewer::brewer.pal(11,"BrBG")[c(1,6,11)] %>% rev(),
              "PiYG" = RColorBrewer::brewer.pal(11,"PiYG")[c(1,6,11)] %>% rev(),
              "PRGn" = RColorBrewer::brewer.pal(11,"PRGn")[c(1,6,11)] %>% rev(),
              "PuOr" = RColorBrewer::brewer.pal(11,"PuOr")[c(1,6,11)] %>% rev(),
              "RdBu" = RColorBrewer::brewer.pal(11,"RdBu")[c(1,6,11)] %>% rev(),
              "RdGy" = RColorBrewer::brewer.pal(11,"RdGy")[c(1,6,11)] %>% rev(),
              "RdYlBu" = RColorBrewer::brewer.pal(11,"RdYlBu")[c(1,6,11)] %>% rev(),
              "RdYlGn" = RColorBrewer::brewer.pal(11,"RdYlGn")[c(1,6,11)] %>% rev(),
              "Spectral" = RColorBrewer::brewer.pal(11,"Spectral")[c(1,6,11)] %>% rev(),
              "Blues" = RColorBrewer::brewer.pal(9,"Blues")[c(1,5,9)],
              "BuGn" = RColorBrewer::brewer.pal(9,"BuGn")[c(1,5,9)],
              "BuPu" = RColorBrewer::brewer.pal(9,"BuPu")[c(1,5,9)],
              "GnBu" = RColorBrewer::brewer.pal(9,"GnBu")[c(1,5,9)],
              "Greens" = RColorBrewer::brewer.pal(9,"Greens")[c(1,5,9)],
              "Greys" = RColorBrewer::brewer.pal(9,"Greys")[c(1,5,9)],
              "Oranges" = RColorBrewer::brewer.pal(9,"Oranges")[c(1,5,9)],
              "OrRd" = RColorBrewer::brewer.pal(9,"OrRd")[c(1,5,9)],
              "PuBu" = RColorBrewer::brewer.pal(9,"PuBu")[c(1,5,9)],
              "PuBuGn" = RColorBrewer::brewer.pal(9,"PuBuGn")[c(1,5,9)],
              "PuRd" = RColorBrewer::brewer.pal(9,"PuRd")[c(1,5,9)],
              "Purples" = RColorBrewer::brewer.pal(9,"Purples")[c(1,5,9)],
              "RdPu" = RColorBrewer::brewer.pal(9,"RdPu")[c(1,5,9)],
              "Reds" = RColorBrewer::brewer.pal(9,"Reds")[c(1,5,9)],
              "YlGn" = RColorBrewer::brewer.pal(9,"YlGn")[c(1,5,9)],
              "YlGnBu" = RColorBrewer::brewer.pal(9,"YlGnBu")[c(1,5,9)],
              "YlOrBr" = RColorBrewer::brewer.pal(9,"YlOrBr")[c(1,5,9)],
              "YlOrRd" = RColorBrewer::brewer.pal(9,"YlOrRd")[c(1,5,9)]
      )
    }
    palette_group <- palette_trans_fun(input$palette_group_ctg)
    palette_dose <- palette_trans_fun(input$palette_dose_ctg)
    palette_rep <- palette_trans_fun(input$palette_rep_ctg)
    palette_cv <- palette_trans_fun(input$palette_cv_ctg)
    return(list(palette_group=palette_group, palette_dose=palette_dose,
                palette_rep=palette_rep, palette_cv=palette_cv))
    
  })
  
  ctg_heatmap_plot <- reactive({
    CTG_heatmap <- function(res_wide_heat,title,palette_group,
                            palette_dose, palette_rep,palette_cv){
      res_wide_heat$logDose <- log10(res_wide_heat$Dose)
      top_anno <- HeatmapAnnotation(Group=res_wide_heat$Group,
                                    logDose=res_wide_heat$logDose,
                                    col = list(Group=setNames(palette_group[1:length(unique(res_wide_heat$Group))],
                                                              unique(res_wide_heat$Group)),
                                               logDose=colorRamp2(unique(res_wide_heat$logDose),
                                                                  colorRampPalette(palette_dose)(length(unique(res_wide_heat$logDose))))
                                    ))
      reps <- grep("^Rep",colnames(res_wide_heat),value = T)
      left_anno <- HeatmapAnnotation(Rep=reps,which = "row",
                                     col = list(Rep=setNames(palette_rep[1:length(reps)],reps)))
      
      p1 <- Heatmap(res_wide_heat[,reps] %>% t(),
                    cluster_rows = F,cluster_columns = F,
                    column_labels = rep("",nrow(res_wide_heat)),
                    top_annotation = top_anno,
                    name = "Cell\nviability",
                    left_annotation = left_anno,
                    column_split = res_wide_heat$Group,
                    border = T,
                    border_gp = gpar(col = 'black', lwd = 2),
                    rect_gp = gpar(col = 'white', lwd = 0),
                    na_col = "black",
                    col = circlize::colorRamp2(c(0,50,100),palette_cv),
                    heatmap_legend_param = list(col_fun=colorRamp2(c(0,50,100),palette_cv),
                                                at=c(0,50,100)),
                    column_title = title)
      return(p1)
    }
    return(CTG_heatmap(res_wide_heat = res_wide_heat(),title = title_ctg(),
                       palette_group = ctg_heatmap_params()$palette_group,palette_dose = ctg_heatmap_params()$palette_dose,
                       palette_rep = ctg_heatmap_params()$palette_rep,palette_cv = ctg_heatmap_params()$palette_cv))
  })
  
  output$heatmap_ctg <- renderPlot({
    ctg_heatmap_plot()
  })
  
  output$ui_ctg_heatmap_plot <- renderUI({
    if(!is.null(infile_ctg())){
      dat <- select_ctg()
      n_sample <- length(unique(dat$Group))
      width1 <- n_sample*length(unique(dat$Dose))*20
      height1 <- length(grep("^Rep",colnames(dat)))*60
      if(n_sample < 2){
        width1 <- 1.5*length(unique(dat$Dose))*20
      }
      eventReactive(input$calculate_ctg,{
        column(12,
               h3("Heatmap:"),
               downloadButton(outputId = "ctg_down_heat_png",label = "Download PNG"), 
               downloadButton(outputId = "ctg_down_heat_pdf",label = "Download PDF"),
               br(),
               plotOutput("heatmap_ctg",width = width1,height = height1)
        )
      })()
    }
  })
  
  # ctg_plot
  ctg_fit_plot <- reactive({
    res <- res_ctg()
    res_ic50 <- summary_ctg()
    
    res<- left_join(res,res_ic50[,c("Estimate","Group")],by="Group")
    res$label <- paste0(res$Group,": IC50 = ",signif(res$Estimate,4))
    res$label <- factor(res$label,levels = unique(res$label))
    
    plot_data_gen <- function(res){
      new_x <- data.frame(Dose=seq(min(res$Dose),max(res$Dose),length.out=100))
      dat_generate <- function(res,i){
        res_1 <- res %>% dplyr::filter(Group==i)
        drm_1 <- drc::drm(dval ~ Dose,Group,data = res_1,
                          fct = LL.5())
        dat_1 <- data.frame(Dose=new_x$Dose,Response=predict(drm_1,newdata = new_x))
        dat_1$Group <- i
        return(dat_1)
      }
      plot_data <- do.call(rbind,
                           lapply(unique(res$Group), function(i){
                             return(dat_generate(res = res,i=i))
                           }))
      plot_data <- left_join(plot_data,res[,c("Group","label")],by="Group",multiple = "all")
      return(plot_data)
    }
    
    plot_data <- plot_data_gen(res = res)
    ### plot: fitting
    CTG_plot1 <- function(res,plot_data,palette,title){
      p1 <- ggplot()+
        geom_line(aes(x=Dose,y=Response,color=label),data = plot_data,lwd=2)+
        geom_point(aes(x=Dose,y=dval,color=label),data = res)+
        geom_hline(yintercept = 50,lty=2)+
        scale_y_continuous(breaks = c(0,20,40,60,80,100,120),
                           limits = c(0,ifelse(max(res$dval)*1.1>120,max(res$dval)*1.1,120)),
                           expand = c(0,0))+
        theme_classic()+
        theme(axis.title = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 14,face = "bold",angle = 45,hjust = 1),
              axis.text.y = element_text(size = 14,face = "bold"),
              axis.ticks = element_line(linewidth = 1),
              legend.text = element_text(size = 11,face = "bold"),
              legend.title = element_blank(),
              legend.position = "right",
              plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
        labs(x="Dose",y="Cell Viablity (%)")+
        scale_color_manual(values = palette)+
        ggtitle(title)
      
      p2 <- ggplot()+
        geom_line(aes(x=Dose,y=Response,color=label),data = plot_data,lwd=2)+
        geom_point(aes(x=Dose,y=dval,color=label),data = res)+
        geom_hline(yintercept = 50,lty=2)+
        scale_y_continuous(breaks = c(0,20,40,60,80,100,120),
                           limits = c(0,ifelse(max(res$dval)*1.1>120,max(res$dval)*1.1,120)),
                           expand = c(0,0))+
        theme_classic()+
        theme(axis.title = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 14,face = "bold",angle = 45,hjust = 1),
              axis.text.y = element_text(size = 14,face = "bold"),
              axis.ticks = element_line(linewidth = 1),
              legend.text = element_text(size = 11,face = "bold"),
              legend.title = element_blank(),
              legend.position = "right",
              plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
        labs(x="Dose",y="Cell Viablity (%)")+
        scale_color_manual(values = palette)+
        ggtitle(title)+
        scale_x_log10()
      return(p1+p2+plot_layout(guides = "collect"))
    }
    CTG_plot1(res = res,plot_data = plot_data,palette = palette_ctg(),title = title_ctg())
  })
  output$ctg_fit_plot <- renderPlot({
    ctg_fit_plot()
  })
  
  ctg_box_plot <- reactive({
    res <- res_ctg()
    res_ic50 <- summary_ctg()
    
    res<- left_join(res,res_ic50[,c("Estimate","Group")],by="Group")
    res$label <- paste0(res$Group,": IC50 = ",signif(res$Estimate,4))
    res$label <- factor(res$label,levels = unique(res$label))
    res$Group <- factor(res$Group, levels = unique(res$Group))
    ### plot:rawdata
    CTG_plot2 <- function(res,palette,title){
      p1 <- ggplot(res,aes(x=Dose,y=dval,color=label))+
        stat_summary(geom = "errorbar",width=0.2,fun.data = "mean_se",lwd=1)+
        stat_summary(geom = "point",size=3,fun.data = "mean_se")+
        geom_hline(yintercept = 50,lty=3)+
        facet_wrap(~Group,nrow = ceiling(length(unique(res$Group))/3),ncol = 3)+
        theme_bw()+
        scale_x_log10()+
        scale_y_continuous(breaks = c(0,25,50,75,100,120),
                           limits = c(0,ifelse(max(res$dval)*1.1>120,max(res$dval)*1.1,120)),
                           expand = c(0,0))+
        theme(axis.title = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 14,face = "bold",angle = 45,hjust = 1),
              axis.text.y = element_text(size = 14,face = "bold"),
              axis.ticks = element_line(linewidth = 1),
              legend.text = element_text(size = 11,face = "bold"),
              legend.title = element_blank(),
              legend.position = "right",
              legend.background = element_rect(linewidth = 1),
              legend.box.background = element_rect(linewidth = 1),
              strip.background = element_rect(fill = "#eaeae0"),
              strip.text = element_text(size=12),
              plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
        labs(x="Dose",y="Cell Viablity (%)")+
        scale_color_manual(values = palette)+
        ggtitle(title)
      return(p1)
    }
    CTG_plot2(res = res,palette = palette_ctg(),title = title_ctg())
  })
  output$ctg_box_plot <- renderPlot({
    ctg_box_plot()
  })
  ### plot:rawdata box raw
  ctg_box_plot_raw <- reactive({
    res <- res_ctg()
    res$Group <- factor(res$Group, levels = unique(res$Group))
    CTG_plot4 <- function(res,palette,title){
      p1 <- ggplot(res,aes(x=Dose,y=dval,color=Group))+
        stat_summary(geom = "errorbar",width=0.2,fun.data = "mean_se",lwd=1)+
        stat_summary(geom = "point",size=3,fun.data = "mean_se")+
        geom_hline(yintercept = 50,lty=3)+
        facet_wrap(~Group,nrow = ceiling(length(unique(res$Group))/3),ncol = 3)+
        theme_bw()+
        scale_x_log10()+
        scale_y_continuous(breaks = c(0,25,50,75,100,120),
                           limits = c(0,ifelse(max(res$dval)*1.1>120,max(res$dval)*1.1,120)),
                           expand = c(0,0))+
        theme(axis.title = element_text(size = 15,face = "bold"),
              axis.text.x = element_text(size = 14,face = "bold",angle = 45,hjust = 1),
              axis.text.y = element_text(size = 14,face = "bold"),
              axis.ticks = element_line(linewidth = 1),
              legend.text = element_text(size = 11,face = "bold"),
              legend.title = element_blank(),
              legend.position = "right",
              legend.background = element_rect(linewidth = 1),
              legend.box.background = element_rect(linewidth = 1),
              strip.background = element_rect(fill = "#eaeae0"),
              strip.text = element_text(size=12),
              plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
        labs(x="Dose",y="Cell Viablity (%)")+
        scale_color_manual(values = palette)+
        ggtitle(title)
      return(p1)
    }
    CTG_plot4(res = res,palette = palette_ctg(),title = title_ctg())
  })
  
  output$ctg_box_plot_raw <- renderPlot({
    ctg_box_plot_raw()
  })
  output$ui_plot_ctg <- renderUI({
    if(!is.null(infile_ctg())){
      res <- res_ctg()
      n_group <- length(unique(res$Group))
      width1 <- 3 * 300
      height1 <- ceiling(n_group/3) * 220
      if(n_group <= 3){
        width1 <- n_group * 350
        height1 <- 300
      }
      eventReactive(input$calculate_ctg,{
        column(12,
               h3("Box Plot:"),
               br(),
               downloadButton(outputId = "ctg_down_plot_box_png",label = "Download PNG"),
               downloadButton(outputId = "ctg_down_plot_box_pdf",label = "Download PDF"),
               br(),
               plotOutput(outputId = "ctg_box_plot",width = width1,height = height1),
               h3("Fitted Curves:"),
               br(),
               downloadButton(outputId = "ctg_down_plot_fit_png",label = "Download PNG"),
               downloadButton(outputId = "ctg_down_plot_fit_pdf",label = "Download PDF"),
               plotOutput(outputId = "ctg_fit_plot",width = 1000,height = 400),
               h3("Box Plot Raw:"),
               br(),
               downloadButton(outputId = "ctg_down_plot_box_raw_png",label = "Download PNG"),
               downloadButton(outputId = "ctg_down_plot_box_raw_pdf",label = "Download PDF"),
               br(),
               plotOutput(outputId = "ctg_box_plot_raw",width = width1,height = height1),
        )
      })()
    }
  })
  
  # download ctg
  # download
  output$down_res_table_ctg <- downloadHandler(
    filename = paste0("CTG_Result_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(result_ctg(),file)
    }
  )
  output$down_summ_table_ctg <- downloadHandler(
    filename = paste0("CTG_Summary_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(summary_ctg(),file)
    }
  )
  output$ctg_down_heat_png <- downloadHandler(
    filename = paste0("CTG_Heatmap_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             dpi = 300, plot = ggplotify::as.ggplot(ctg_heatmap_plot()))
    }
  )
  output$ctg_down_heat_pdf <- downloadHandler(
    filename = paste0("CTG_Heatmap_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
            plot = ggplotify::as.ggplot(ctg_heatmap_plot()))
    }
  )
  output$ctg_down_plot_box_png <- downloadHandler(
    filename = paste0("CTG_Plot_Box_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             dpi = 300, plot = ctg_box_plot())
    }
  )
  output$ctg_down_plot_box_pdf <- downloadHandler(
    filename = paste0("CTG_Plot_Box_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             plot = ctg_box_plot())
    }
  )
  output$ctg_down_plot_fit_png <- downloadHandler(
    filename = paste0("CTG_Plot_fit_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             dpi = 300, plot = ctg_fit_plot())
    }
  )
  output$ctg_down_plot_fit_pdf <- downloadHandler(
    filename = paste0("CTG_Plot_fit_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             plot = ctg_fit_plot())
    }
  )
  output$ctg_down_plot_box_raw_png <- downloadHandler(
    filename = paste0("CTG_Plot_Box_Raw_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             dpi = 300, plot = ctg_box_plot_raw())
    }
  )
  output$ctg_down_plot_box_raw_pdf <- downloadHandler(
    filename = paste0("CTG_Plot_Box_Raw_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_ctg, height = input$plot_height_ctg,
             plot = ctg_box_plot_raw())
    }
  )
  
  # WB +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read_data
  infile_wb <- reactive({
    input$file_wb$datapath
  })
  rawdata_wb <- reactive({
    WB_read <- function(path,n_sheet){
      read_WB <- function(path, i){
        dat <- readxl::read_excel(path = path,sheet = i,trim_ws = TRUE) %>% 
          as.data.frame()
        colnames(dat)[1] <- "sample"
        dat$rep <- paste0("Rep",i)
        dat <- reshape2::melt(dat,id = c("sample","rep"))
        dat$id <- paste(dat$rep,dat$sample,sep = "_")
        dat$id2 <- paste(dat$rep,dat$variable,sep = "_")
        dat$sample <- factor(dat$sample,levels = unique(dat$sample))
        return(dat)
      }
      
      dat <- lapply(1:n_sheet, function(i){
        return(read_WB(path = path,i = i))
      })
      
      dat <- do.call(rbind,dat)
      return(dat)
    }
    dat <- WB_read(path = infile_wb(),n_sheet = input$n_sheet_wb)
    return(dat)
  })
  
  
  output$rawdata_wb <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_wb())){
      dat <- rawdata_wb() %>% dplyr::select(1:4)
      colnames(dat) <- c("Sample","Replication","Target","Value")
      dat <- dat %>% reshape2::dcast(formula = Replication+Sample~Target,value.var = "Value")
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  
  
  output$side_wb <- renderUI({
    sample_choice_wb <- NULL
    target_choice_wb <- NULL
    palette_wb <- NULL
    plot_width_wb <- NULL
    plot_height_wb <- NULL
    if(!is.null(infile_wb())){
      sample_choice_wb <- rawdata_wb() %>% pull("sample") %>% unique()
      target_choice_wb <- rawdata_wb() %>% pull("variable") %>% unique()
      plot_width_wb <- 10
      plot_height_wb <- 8
      palette_wb <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                        "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                        "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                        "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                        "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                        "Paired (12)","Dark2 (8)","Accent (8)")
    }
    column(12, 
           fluidRow(
             column(6,
                    selectInput(inputId = "sample_wb",label = "Select Samples: (Multi)",
                                choices = sample_choice_wb, multiple = TRUE)),
             column(6,
                    selectInput(inputId = "target_wb",label = "Select Targets: (Multi)",
                                choices = target_choice_wb,
                                multiple = TRUE))
           ),
           fluidRow(
             column(4,
                    selectInput(inputId = "control_wb",label = "Select Control:",
                                choices = sample_choice_wb, multiple = FALSE)),
             column(4,
                    selectInput(inputId = "reference_wb",label = "Select Reference:",
                                choices = target_choice_wb,
                                multiple = FALSE)),
             column(4,
                    selectInput(inputId = "palette_wb",label = "Select Palette:",
                                choices = palette_wb,
                                multiple = FALSE))
           ),
           fluidRow(
             column(4,
                    textInput(inputId = "title_wb",label = "Plot Title:",
                              value = NULL)),
             column(4,
                    numericInput(inputId = "plot_width_wb",label = "Image Width (Download)",
                                 value = plot_width_wb, min = 1, step = 1)),
             column(4,
                    numericInput(inputId = "plot_height_wb",label = "Image Height (Download)",
                                 value = plot_height_wb, min = 1, step = 1)))
           
    )
    
  })
  # parameters
  sample_wb <- eventReactive(input$calculate_wb,{input$sample_wb})
  target_wb <- eventReactive(input$calculate_wb,{input$target_wb})
  control_wb <- eventReactive(input$calculate_wb,{input$control_wb})
  reference_wb <- eventReactive(input$calculate_wb,{input$reference_wb})
  title_wb <- eventReactive(input$calculate_wb,{input$title_wb})
  palette_wb <- eventReactive(input$calculate_wb,{
    switch (input$palette_wb,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  # select data
  select_wb <- eventReactive(input$calculate_wb,{
    rawdata_wb() %>% dplyr::filter(sample %in% sample_wb()) %>% 
      dplyr::filter(variable %in% target_wb())
  })
  output$select_wb <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_wb())){
      dat <- select_wb() %>% dplyr::select(1:4)
      colnames(dat) <- c("Sample","Replication","Target","Value")
      dat <- dat %>% reshape2::dcast(formula = Replication+Sample~Target,value.var = "Value")
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  output$ui_select_wb <- renderUI({
    dataTableOutput(outputId = "select_wb",width = "80%")
  })
  # analysis
  res_wb <- eventReactive(input$calculate_wb,{
    WB_analysis <- function(dat,ref,con){
      sample_level <- unique(dat$sample)
      target_level <- unique(dat$variable)
      # dat$sample <- factor(dat$sample, levels = c(control_wb(),sample_level[!sample_level %in% control_wb()]))
      # dat$variable <- factor(dat$variable, levels = c(reference_wb(),target_level[!target_level %in% reference_wb()]))
      ref_val <- dat %>% filter(variable == ref)
      ref_val <- setNames(ref_val$value,ref_val$id)
      
      res <- data.frame()
      for (i in names(ref_val)) {
        res_tmp <- dat %>% filter(id == i) %>% 
          mutate(dval = value/ref_val[i])
        res <- rbind(res, res_tmp)
      }
      
      con_dval <- res %>% filter(sample == con)
      con_dval <- setNames(con_dval$dval, con_dval$id2)
      
      res2 <- data.frame()
      for (j in names(con_dval)) {
        res_tmp <- res %>% filter(id2 == j) %>% 
          mutate(exp = dval/con_dval[j])
        res2 <- rbind(res2, res_tmp)
      }
      return(res2)
    }
    WB_analysis(dat = select_wb(),ref = reference_wb(),con = control_wb()) %>% 
      filter(variable != reference_wb())
  })

  # output result
  result_wb <- reactive({
    dat <- res_wb() %>% dplyr::select(1,2,3,8)
    colnames(dat) <- c("Sample","Replication","Target","Expression")
    dat %>% reshape2::dcast(formula = Replication+Sample~Target,value.var = "Expression")
  })
  output$result_wb <- renderDataTable({
    DT::datatable(data = result_wb(),
                  options = list(pageLength=25))
  })

  down_res_label_wb <- eventReactive(eventExpr = input$calculate_wb,
                                       {"Download Table"})
  output$ui_result_wb <- renderUI({
    if(!is.null(infile_wb())){
      column(12,
             fluidRow(downloadButton(outputId = "down_res_table_wb",label = down_res_label_wb())),
             br(),
             dataTableOutput("result_wb",width = "80%"))
    }
  })
  
  # # output summary
  summary_wb <- reactive({
    res_wb() %>% dplyr::select(1,3,8) %>% group_by(sample,variable) %>% 
      summarise(mean=mean(exp),sd=sd(exp)) %>% 
      mutate(upper=mean+sd,lower=mean-sd)
  })
  output$summary_wb <- renderDataTable({
    summary_wb()  %>%
      DT::datatable(options = list(pageLength=25))
  })

  down_summ_label_wb <- eventReactive(eventExpr = input$calculate_wb,
                                        {"Download Summary"})
  output$ui_summary_wb <- renderUI({
    if(!is.null(infile_wb())){
      column(12,
             fluidRow(downloadButton(outputId = "down_summ_table_wb",label = down_summ_label_wb())),
             br(),
             dataTableOutput("summary_wb",width = "80%"))
    }
  })
  # # plot
  wb_plot1 <- reactive({
    if(!is.null(infile_wb())){
      WB_plot1 <- function(res,palette,title){
        p1 <- ggplot(res)+
          stat_summary(aes(x=variable,y=exp,fill=sample),geom = "bar",color="black",
                       position = position_dodge(0.75),width=0.6,fun.data = "mean_se")+
          stat_summary(aes(x=variable,y=exp,group=sample),geom = "errorbar",
                       position = position_dodge(0.75),width=0.2,fun.data = "mean_se")+
          geom_point(aes(x=variable,y=exp,group=sample),position = position_dodge(0.75))+
          scale_y_continuous(expand = c(0,0), limits = c(0,max(res$exp)*1.1))+
          theme_classic()+
          labs(x="",y="Relative protein level",fill="Sample")+
          theme(axis.title = element_text(size = 15,face = "bold"),
                axis.text = element_text(size = 14),
                axis.line = element_line(linewidth = 1),
                axis.ticks = element_line(linewidth = 1),
                legend.position = "right",
                legend.background = element_rect(linewidth = 1),
                legend.text = element_text(size = 11,face = "bold"),
                legend.title = element_text(size = 14,face = "bold"),
                legend.box.background = element_rect(linewidth = 1),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_fill_manual(values = palette)+
          ggtitle(title)
        return(p1)
      }
      WB_plot1(res = res_wb(),palette = palette_wb(),title=title_wb())
    }
  })

  wb_plot2 <- reactive({
    if(!is.null(infile_wb())){
      WB_plot2 <- function(res,palette,title){
        ggplot(res)+
          geom_bar(aes(x=sample,y=exp,fill=sample),stat="identity")+
          facet_grid(rep~variable)+
          labs(x="",y="Relative protein level",fill="Sample")+
          scale_y_continuous(expand = c(0,0))+
          theme_bw()+
          theme(axis.title = element_text(size = 15,face = "bold"),
                axis.text = element_text(size = 14),
                axis.ticks.x = element_blank(),
                legend.position = "right",
                legend.background = element_rect(linewidth = 1),
                legend.box.background = element_rect(linewidth = 1),
                legend.text = element_text(size = 11,face = "bold"),
                legend.title = element_text(size = 14,face = "bold"),
                axis.text.x = element_blank(),
                panel.grid = element_blank(),
                strip.background = element_rect(fill = "#eaeae0"),
                strip.text = element_text(size = 12,face = "bold"),
                plot.title = element_text(size=20,hjust = 0.5,face="bold"))+
          scale_fill_manual(values = palette)+
          ggtitle(title)
      }
      WB_plot2(res = res_wb(),palette = palette_wb(),title=title_wb())
    }
  })
  output$wb_plot1 <- renderPlot({
    wb_plot1()
  })
  output$wb_plot2 <- renderPlot({
    wb_plot2()
  })

  output$ui_plot_wb <- renderUI({
    if(!is.null(infile_wb())){
      n_sample <- length(select_wb() %>% pull("sample") %>% unique())
      n_target <- length(select_wb() %>% pull("variable") %>% unique())
      n_rep <- eventReactive(input$calculate_wb,{length(select_wb() %>% pull("rep") %>% unique())})()
      width1 <- n_target * 180
      height1 <- 400
      width2 <- n_target * 180
      height2 <- n_rep * 180
      if(n_rep < 2){
        height2 <- 220
      }

      eventReactive(input$calculate_wb,
                    {column(12,
                            h3("Plot (Style1):"),
                            downloadButton(outputId = "wb_down_plot1_png",label = "Download PNG"),
                            downloadButton(outputId = "wb_down_plot1_pdf",label = "Download PDF"),
                            br(),
                            plotOutput("wb_plot1",width = width1,height = height1),
                            br(),
                            h3("Plot (Style2):"),
                            downloadButton(outputId = "wb_down_plot2_png",label = "Download PNG"),
                            downloadButton(outputId = "wb_down_plot2_pdf",label = "Download PDF"),
                            br(),
                            plotOutput("wb_plot2",width = width2,height = height2)
                    )})()
    }

  })

  # # download
  output$down_res_table_wb <- downloadHandler(
    filename = paste0("WB_Result_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(result_wb(),file)
    }
  )
  output$down_summ_table_wb <- downloadHandler(
    filename = paste0("WB_Summary_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(summary_wb(),file)
    }
  )
  output$wb_down_plot1_png <- downloadHandler(
    filename = paste0("WB_Plot1_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_wb, height = input$plot_height_wb,
             dpi = 300, plot = wb_plot1())
    }
  )
  output$wb_down_plot1_pdf <- downloadHandler(
    filename = paste0("WB_Plot1_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_wb, height = input$plot_height_wb,
             plot = wb_plot1())
    }
  )
  output$wb_down_plot2_png <- downloadHandler(
    filename = paste0("WB_Plot2_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_wb, height = input$plot_height_wb,
             dpi = 300, plot = wb_plot2())
    }
  )
  output$wb_down_plot2_pdf <- downloadHandler(
    filename = paste0("WB_Plot2_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_wb, height = input$plot_height_wb,
             plot = wb_plot2())
    }
  )
  # FAM-BHQ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # read_data
  infile_fam <- reactive({
    input$file_fam$datapath
  })
  dat_fam <- reactive({
    if(!is.null(infile_fam())){
      FAM_read <- function(path1,n_sheet,n_rep){
        read_FAM <- function(path1,sheet,n_rep){
          dat <- read_excel(path = path1,sheet = sheet)
          col_names <- grep("^[.][.][.]",x = colnames(dat),value = T,invert = T)[-1]
          colnames(dat)[2:ncol(dat)] <- c(paste0(rep(col_names,each=n_rep),"_Rep",rep(1:n_rep,length(col_names))))
          dat$group <- colnames(dat)[1]
          colnames(dat)[1] <- "sample"
          dat_m <- reshape2::melt(data = dat,id.vars = c("sample","group"))
          return(dat_m)
        }
        
        dat <- lapply(1:n_sheet,function(i){
          read_FAM(path1 = path1,sheet = i,n_rep = n_rep)
        }) %>% do.call(rbind,.)
        dat$sample <- factor(dat$sample,levels = unique(dat$sample))
        dat$group <- factor(dat$group,levels = unique(dat$group))
        return(dat)
      }
      
      FAM_read(path1 = infile_fam(),n_sheet = input$n_sheet_fam,n_rep = input$n_rep_fam)
    }
    
  })
  
  # rawdata
  rawdata_fam <- reactive({
    rawdata <- dat_fam() %>% reshape2::dcast(formula = group+sample~variable)
    
  })
  output$rawdata_fam <- renderDataTable({
    dat <- data.frame()
    if(!is.null(infile_fam())){
      dat <- rawdata_fam()
    }
    DT::datatable(data = dat,
                  options = list(pageLength = 25))
  })
  
  output$side_fam <- renderUI({
    unit_time_choice_fam <- NULL
    palette_fam <- NULL
    plot_width_fam <- NULL
    plot_height_fam <- NULL
    if(!is.null(infile_fam())){
      unit_time_choice_fam <- c("min","h","sec")
      plot_width_fam <- 10
      plot_height_fam <- 8
      palette_fam <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                       "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                       "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                       "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                       "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                       "Paired (12)","Dark2 (8)","Accent (8)")
    }
    
    column(12, 
           fluidRow(
             column(6,
                    selectInput(inputId = "unit_time_fam",label = "Select Time unit:",
                                choices = unit_time_choice_fam, multiple = FALSE)),
             column(6,
                    selectInput(inputId = "palette_fam",label = "Select Palette:",
                                choices = palette_fam, multiple = FALSE))
           ),
           fluidRow(
             column(4,
                    textInput(inputId = "title_fam",label = "Plot Title:",
                              value = NULL)),
             column(4,
                    numericInput(inputId = "plot_width_fam",label = "Image Width (Download)",
                                 value = plot_width_fam, min = 1, step = 1)),
             column(4,
                    numericInput(inputId = "plot_height_fam",label = "Image Height (Download)",
                                 value = plot_height_fam, min = 1, step = 1))
           )
    )
  })
  
  # parameters
  title_fam <- eventReactive(input$calculate_fam,{input$title_fam})
  unit_time_fam <- eventReactive(input$calculate_fam,{input$unit_time_fam})
  palette_fam <- eventReactive(input$calculate_fam,{
    switch (input$palette_fam,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  
  # preprocess
  res_fam <- reactive({
    res_fam_fun <- function(dat){
      dat <- na.omit(dat)
      dat$id <- paste(dat$sample,dat$group,dat$variable,sep = "_")
      dat$time <- as.data.frame(strsplit(as.character(dat$variable),split = "_",fixed = T))[1,] %>%
        as.numeric()
      dat$well <- as.data.frame(strsplit(as.character(dat$variable),split = "_",fixed = T))[2,] %>% 
        as.character() %>% paste(dat$sample,dat$group,.,sep = "_")
      dat$sample_group <- paste(dat$sample,dat$group,sep = "_")
      dat$value <- as.numeric(dat$value)
      dat$sample <- factor(dat$sample,levels = unique(dat$sample))
      dat$group <- factor(dat$group,levels = unique(dat$group))
      dat$well <- factor(dat$well,levels = unique(dat$well))
      dat$sample_group <- factor(dat$sample_group,levels = unique(dat$sample_group))
      dat$sample_group_time <- paste(dat$sample_group,dat$time,sep = "_")
      dat$sample_group_time <- factor(dat$sample_group_time,levels = unique(dat$sample_group_time))
      dat$time_unit <- paste(dat$time,unit_time_fam(),sep = " ")
      dat$time_unit <- factor(dat$time_unit,levels = unique(dat$time_unit))
      return(dat)
    }
    res_fam_fun(dat = dat_fam())
  })
  
  # summ_fam
  summ_fam <- reactive({
    summ_fam_fun <- function(dat){
      summ <- dat %>% group_by(sample,group,time) %>% 
        summarise(mean = mean(value),sd=sd(value)) %>% 
        mutate(upper = mean+sd, lower = mean-sd)
      return(summ)
    }
    summ_fam_fun(dat = res_fam())
  })
  output$summ_fam <- renderDataTable({
    summ_fam() %>%
      DT::datatable(options = list(pageLength=25))
  })
  
  down_summ_label_fam <- eventReactive(eventExpr = input$calculate_fam,
                                       {"Download Summary"})
  output$ui_summary_fam <- renderUI({
    if(!is.null(infile_fam())){
      column(12,
             fluidRow(downloadButton(outputId = "down_summ_table_fam",label = down_summ_label_fam())),
             br(),
             dataTableOutput("summ_fam",width = "80%"))
      
    }
  })
 
  
  # fam_heatmap
  output$ui_fam_heatmap_control <- renderUI({
    palette_group <- NULL
    palette_dose <- NULL
    palette_rep <- NULL
    palette_cv <- NULL
    palette_in_discrete <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                             "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                             "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                             "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                             "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                             "Paired (12)","Dark2 (8)","Accent (8)")
    
    palette_in_continuous <- rownames(RColorBrewer::brewer.pal.info %>% filter(category %in% c("div","seq")))
    
    if(!is.null(infile_fam())){
      palette_target <- palette_in_discrete
      palette_crRNA <- palette_in_discrete
      palette_rep <- palette_in_discrete
      palette_time <- palette_in_discrete
      palette_cv <- palette_in_continuous
      eventReactive(input$calculate_fam,{
        column(12,
               fluidRow(
                 column(6,selectInput(inputId = "palette_target_fam",label = "Palette for Target:",
                                      choices = palette_target, selected = sample(palette_target,1),multiple = FALSE)),
                 column(6,selectInput(inputId = "palette_crRNA_fam",label = "Palette for crRNA:",
                                      choices = palette_crRNA, selected = sample(palette_crRNA,1),multiple = FALSE))),
               fluidRow(
                 column(6,selectInput(inputId = "palette_rep_fam",label = "Palette for Rep:",
                                      choices = palette_rep, selected = sample(palette_rep,1),multiple = FALSE)),
                 column(6,selectInput(inputId = "palette_time_fam",label = "Palette for Time:",
                                      choices = palette_time, selected = sample(palette_time,1),multiple = FALSE)),),
               fluidRow(
                 column(6,selectInput(inputId = "palette_cv_fam",label = "Palette for OD value:",
                                      choices = palette_cv,selected = sample(palette_cv,1),multiple = FALSE))
               ))
      })()
    }
    
  })
  
  # heatmap parameters:
  fam_heatmap_params <- reactive({
    palette_trans_fun <- function(palette){
      switch (palette,
              "Lancet (9)" = ggsci::pal_lancet()(9),
              "NPG (10)" = ggsci::pal_npg()(10),
              "NEJM (8)" = ggsci::pal_nejm()(8),
              "JCO (10)" = ggsci::pal_jco()(10),
              "JAMA (7)" = ggsci::pal_jama()(7),
              "AAAS (10)" = ggsci::pal_aaas()(10),
              "D3 (10)" = ggsci::pal_d3()(10),
              "Futurama (12)" = ggsci::pal_futurama()(12),
              "GSEA (12)" = ggsci::pal_gsea()(12),
              "IGV (51)" = ggsci::pal_igv()(51),
              "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
              "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
              "Simpsons (16)" = ggsci::pal_simpsons()(16),
              "Star Trek (7)" = ggsci::pal_startrek()(7),
              "Tron Legacy (7)" = ggsci::pal_tron()(7),
              "Chicago (9)" = ggsci::pal_uchicago()(9),
              "UCSC (26)" = ggsci::pal_ucscgb()(26),
              "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
              "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
              "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
              "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
              "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
              "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
              "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
              "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent"),
              "BrBG" = RColorBrewer::brewer.pal(11,"BrBG")[c(1,6,11)] %>% rev(),
              "PiYG" = RColorBrewer::brewer.pal(11,"PiYG")[c(1,6,11)] %>% rev(),
              "PRGn" = RColorBrewer::brewer.pal(11,"PRGn")[c(1,6,11)] %>% rev(),
              "PuOr" = RColorBrewer::brewer.pal(11,"PuOr")[c(1,6,11)] %>% rev(),
              "RdBu" = RColorBrewer::brewer.pal(11,"RdBu")[c(1,6,11)] %>% rev(),
              "RdGy" = RColorBrewer::brewer.pal(11,"RdGy")[c(1,6,11)] %>% rev(),
              "RdYlBu" = RColorBrewer::brewer.pal(11,"RdYlBu")[c(1,6,11)] %>% rev(),
              "RdYlGn" = RColorBrewer::brewer.pal(11,"RdYlGn")[c(1,6,11)] %>% rev(),
              "Spectral" = RColorBrewer::brewer.pal(11,"Spectral")[c(1,6,11)] %>% rev(),
              "Blues" = RColorBrewer::brewer.pal(9,"Blues")[c(1,5,9)],
              "BuGn" = RColorBrewer::brewer.pal(9,"BuGn")[c(1,5,9)],
              "BuPu" = RColorBrewer::brewer.pal(9,"BuPu")[c(1,5,9)],
              "GnBu" = RColorBrewer::brewer.pal(9,"GnBu")[c(1,5,9)],
              "Greens" = RColorBrewer::brewer.pal(9,"Greens")[c(1,5,9)],
              "Greys" = RColorBrewer::brewer.pal(9,"Greys")[c(1,5,9)],
              "Oranges" = RColorBrewer::brewer.pal(9,"Oranges")[c(1,5,9)],
              "OrRd" = RColorBrewer::brewer.pal(9,"OrRd")[c(1,5,9)],
              "PuBu" = RColorBrewer::brewer.pal(9,"PuBu")[c(1,5,9)],
              "PuBuGn" = RColorBrewer::brewer.pal(9,"PuBuGn")[c(1,5,9)],
              "PuRd" = RColorBrewer::brewer.pal(9,"PuRd")[c(1,5,9)],
              "Purples" = RColorBrewer::brewer.pal(9,"Purples")[c(1,5,9)],
              "RdPu" = RColorBrewer::brewer.pal(9,"RdPu")[c(1,5,9)],
              "Reds" = RColorBrewer::brewer.pal(9,"Reds")[c(1,5,9)],
              "YlGn" = RColorBrewer::brewer.pal(9,"YlGn")[c(1,5,9)],
              "YlGnBu" = RColorBrewer::brewer.pal(9,"YlGnBu")[c(1,5,9)],
              "YlOrBr" = RColorBrewer::brewer.pal(9,"YlOrBr")[c(1,5,9)],
              "YlOrRd" = RColorBrewer::brewer.pal(9,"YlOrRd")[c(1,5,9)]
      )
    }
    palette_target <- palette_trans_fun(input$palette_target_fam)
    palette_crRNA <- palette_trans_fun(input$palette_crRNA_fam)
    palette_rep <- palette_trans_fun(input$palette_rep_fam)
    palette_time <- palette_trans_fun(input$palette_time_fam)
    palette_cv <- palette_trans_fun(input$palette_cv_fam)
    return(list(palette_target=palette_target, palette_crRNA=palette_crRNA,
                palette_rep=palette_rep, palette_time = palette_time,
                palette_cv=palette_cv))
    
  })
  
  fam_heatmap_plot <- reactive({
    fam_heatmap <- function(rawdata,title,palette_target,
                            palette_crRNA, palette_rep,palette_time,palette_cv){
      heat_dat <- rawdata[,3:ncol(rawdata)]
      left_anno_df <- rawdata[,1:2]
      left_anno <- HeatmapAnnotation(Target=left_anno_df$group,crRNA = left_anno_df$sample,
                                     which = "row",
                                     col = list(Target= setNames(palette_target[1:length(unique(left_anno_df$group))],unique(left_anno_df$group)),
                                                crRNA = setNames(palette_crRNA[1:length(unique(left_anno_df$sample))],unique(left_anno_df$sample))))
      top_anno_df <- as.data.frame(strsplit(x = colnames(heat_dat),split = "_"))
      top_anno_df[1,] <- paste(top_anno_df[1,],unit_time_fam(),sep = " ")
      top_anno <- HeatmapAnnotation(Time = as.character(top_anno_df[1,]),
                                    Rep = as.character(top_anno_df[2,]),
                                    col = list(Time = setNames(palette_time[1:length(unique(as.character(top_anno_df[1,])))],unique(as.character(top_anno_df[1,]))),
                                               Rep = setNames(palette_rep[1:length(unique(as.character(top_anno_df[2,])))],unique(as.character(top_anno_df[2,])))))
      
      
      p1 <- Heatmap(as.matrix(heat_dat),
              cluster_rows = F,cluster_columns = F,
              na_col = "black",
              column_labels = rep("",ncol(heat_dat)),
              top_annotation = top_anno,
              name = "OD Value",
              left_annotation = left_anno,
              column_split = top_anno_df[1,] %>% as.character(),
              row_split = left_anno_df$sample,
              border = T,
              border_gp = gpar(col = 'black', lwd = 2),
              rect_gp = gpar(col = 'white', lwd = 0),
              col = colorRamp2(breaks = quantile(heat_dat,na.rm = T)[c(2,3,4)],colors = palette_cv),
              # heatmap_legend_param = list(col_fun=colorRamp(palette_cv)),
              column_title = title)
      return(p1)
    }
    return(fam_heatmap(rawdata = rawdata_fam(),title = title_fam(),
                       palette_target = fam_heatmap_params()$palette_target,
                       palette_crRNA = fam_heatmap_params()$palette_crRNA,
                       palette_rep = fam_heatmap_params()$palette_rep,
                       palette_time = fam_heatmap_params()$palette_time,
                       palette_cv = fam_heatmap_params()$palette_cv))
  })
  
  output$heatmap_fam <- renderPlot({
    fam_heatmap_plot()
  })
  
  output$ui_fam_heatmap_plot <- renderUI({
    if(!is.null(infile_fam())){
      rawdata <- rawdata_fam()
      width1 <- ncol(rawdata)*60
      height1 <- nrow(rawdata)*40
      eventReactive(input$calculate_fam,{
        column(12,
               h3("Heatmap:"),
               downloadButton(outputId = "fam_down_heat_png",label = "Download PNG"), 
               downloadButton(outputId = "fam_down_heat_pdf",label = "Download PDF"),
               br(),
               plotOutput("heatmap_fam",width = width1,height = height1)
        )
      })()
    }
  })
  
  # fam_plot
  fam_plot_line <- reactive({
    ### plot: line
    fam_plot_line_fun <- function(dat,unit_time,title1,palette){
      ggplot(dat,aes(x=time,y = value,color=group,group=well))+
        geom_point()+
        geom_line()+
        facet_grid(group~sample)+
        theme_bw()+
        theme(legend.position = "right",
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x=paste0("Time (",unit_time,")"),y="OD value (520nm)",title = title1,
             color="Target")+
        scale_color_manual(values = palette)
    }
    fam_plot_line_fun(dat = res_fam(),unit_time = unit_time_fam(),
                      palette = palette_fam(),title1 = title_fam())
  })
  output$fam_plot_line <- renderPlot({
    fam_plot_line()
  })
  
  fam_plot_errorbar <- reactive({
    ### plot:rawdata
    fam_plot_errorbar <- function(summ,unit_time,title1,palette){
      ggplot(summ,aes(x=time,y = mean,color=group))+
        geom_line()+
        geom_errorbar(aes(ymax=upper,ymin=lower),width=5)+
        geom_point(size=3)+
        facet_grid(group~sample)+
        theme_bw()+
        theme(legend.position = "right",
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x=paste0("Time (",unit_time,")"),y="OD value (520nm)",title = title1,
             color="Target")+
        scale_color_manual(values = palette)
    }
    fam_plot_errorbar(summ=summ_fam(),unit_time = unit_time_fam(),
                      title1 = title_fam(), palette = palette_fam())
  })
  output$fam_plot_errorbar <- renderPlot({
    fam_plot_errorbar()
  })
  
  fam_plot_bar <- reactive({
    ### plot:rawdata
    fam_plot_bar <- function(dat,title1,palette){
      ggplot(dat,aes(x=sample,y=value,fill=group,group=group))+
        stat_summary(geom = "bar",color="black",
                     position = position_dodge(0.75),width=0.6,fun.data = "mean_se")+
        stat_summary(geom = "errorbar",
                     position = position_dodge(0.75),width=0.2,fun.data = "mean_se")+
        geom_point(position=position_dodge(0.75))+
        facet_wrap(~time_unit)+
        theme_bw()+
        theme(panel.grid = element_blank(),
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x="crRNA",y="OD value (520nm)",title = title1,fill="Target")+
        scale_fill_manual(values = palette)
    }
    fam_plot_bar(dat=res_fam(),title1 = title_fam(), 
                 palette = palette_fam())
  })
  output$fam_plot_bar <- renderPlot({
    fam_plot_bar()
  })
  
  output$ui_plot_fam <- renderUI({
    if(!is.null(infile_fam())){
      dat <- res_fam()
      n_target <- length(unique(dat$group))
      n_crRNA <- length(unique(dat$sample))
      width1 <- n_crRNA *100
      width2 <- n_crRNA *100
      height1 <- n_target * 150
      height2 <- 500
      eventReactive(input$calculate_fam,{
        column(12,
               h3("Line Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot_line_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot_line_pdf",label = "Download PDF"),
               br(),
               plotOutput(outputId = "fam_plot_line",width = width1,height = height1),
               h3("Errorbar Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot_errorbar_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot_errorbar_pdf",label = "Download PDF"),
               plotOutput(outputId = "fam_plot_errorbar",width = width1,height = height1),
               h3("Bar Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot_bar_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot_bar_pdf",label = "Download PDF"),
               plotOutput(outputId = "fam_plot_bar",width = width2,height = height2),
        )
      })()
    }
  })
  # plot2 ---
  output$ui_fam_plot2_control <- renderUI({
    palette_fam_plot2 <- NULL
    palette_in_discrete <- c("Lancet (9)","NPG (10)", "NEJM (8)","JCO (10)","JAMA (7)",
                             "AAAS (10)","D3 (10)","Futurama (12)","GSEA (12)","IGV (51)",
                             "LocusZoom (7)","Rick and Morty (12)","Simpsons (16)",
                             "Star Trek (7)","Tron Legacy (7)","Chicago (9)","UCSC (26)",
                             "Set1 (9)","Set2 (8)","Set3 (12)","Pastel1 (9)","Pastel2 (8)",
                             "Paired (12)","Dark2 (8)","Accent (8)")
    
    # palette_in_continuous <- rownames(RColorBrewer::brewer.pal.info %>% filter(category %in% c("div","seq")))
    
    if(!is.null(infile_fam())){
      palette_fam_plot2 <- palette_in_discrete
      eventReactive(input$calculate_fam,{
        column(12,
               fluidRow(
                 column(4,selectInput(inputId = "palette_fam_plot2",label = "Palette for Plot2:",
                                      choices = palette_fam_plot2, multiple = FALSE)),
                 column(4,actionButton(inputId = "fam_plot2_action",label = "Calculate Activity (will take a while...)",
                                       width = "80%",icon = icon("dog"),class = "btn-success"))))
      })()
    }
    
  })
  
  palette_fam_plot2 <- reactive({
    switch (input$palette_fam_plot2,
            "Lancet (9)" = ggsci::pal_lancet()(9),
            "NPG (10)" = ggsci::pal_npg()(10),
            "NEJM (8)" = ggsci::pal_nejm()(8),
            "JCO (10)" = ggsci::pal_jco()(10),
            "JAMA (7)" = ggsci::pal_jama()(7),
            "AAAS (10)" = ggsci::pal_aaas()(10),
            "D3 (10)" = ggsci::pal_d3()(10),
            "Futurama (12)" = ggsci::pal_futurama()(12),
            "GSEA (12)" = ggsci::pal_gsea()(12),
            "IGV (51)" = ggsci::pal_igv()(51),
            "LocusZoom (7)" = ggsci::pal_locuszoom()(7),
            "Rick and Morty (12)" = ggsci::pal_rickandmorty()(12),
            "Simpsons (16)" = ggsci::pal_simpsons()(16),
            "Star Trek (7)" = ggsci::pal_startrek()(7),
            "Tron Legacy (7)" = ggsci::pal_tron()(7),
            "Chicago (9)" = ggsci::pal_uchicago()(9),
            "UCSC (26)" = ggsci::pal_ucscgb()(26),
            "Set1 (9)" = RColorBrewer::brewer.pal(9,"Set1"),
            "Set2 (8)" = RColorBrewer::brewer.pal(8,"Set2"),
            "Set3 (12)" = RColorBrewer::brewer.pal(12,"Set3"),
            "Pastel1 (9)" = RColorBrewer::brewer.pal(9,"Pastel1"),
            "Pastel2 (8)" = RColorBrewer::brewer.pal(8,"Pastel2"),
            "Paired (12)" = RColorBrewer::brewer.pal(12,"Paired"),
            "Dark2 (8)" = RColorBrewer::brewer.pal(8,"Dark2"),
            "Accent (8)" = RColorBrewer::brewer.pal(8,"Accent")
    )
  })
  
  fam_slope_dat <- eventReactive(input$fam_plot2_action,{
    fam_slope_analysis_fun <- function(dat){
      dat <- dat %>% group_by(well) %>% 
        mutate(time_min = min(time),time_max = max(time))
      base_level_df <- dat %>% dplyr::filter(time == time_min)
      base_level <- base_level_df$value
      names(base_level) <- base_level_df$well
      
      dat <- lapply(1:length(base_level), function(i){
        dat %>% dplyr::filter(well == names(base_level)[i]) %>%
          mutate(foldchange = value / base_level[i])
      }) %>% do.call(rbind,.)
      
      # 相对于开始的slope
      dat$relative_fold <- dat$foldchange - 1
      
      # 逐个 slope
      slope_calculate_fun <- function(dat){
        time_level <- unique(dat$time) %>% sort()
        res <- data.frame()
        for (i in 1:length(time_level)) {
          if(i == 1){
            res_tmp <- dat %>% dplyr::filter(time == time_level[i]) %>% 
              mutate(slope_fold = 0)
          }else{
            last_fold_df <- dat %>% 
              dplyr::filter(time == time_level[i-1])
            last_fold <- last_fold_df$foldchange
            names(last_fold) <- last_fold_df$well
            res_tmp <- lapply(1:length(last_fold), function(j){
              tmp <- dat %>% dplyr::filter(time == time_level[i] & well == names(last_fold)[j]) %>% 
                mutate(slope_fold = foldchange - last_fold[j])
            }) %>% do.call(rbind,.)
          }
          res <- rbind(res,res_tmp)
        }
        return(res)
      }
      
      dat <- slope_calculate_fun(dat)
      
      max_value <- max(dat$value)
      activity_flag_fun <- function(dat){
        tmp <- dat %>%  
          dplyr::filter(time > min(time)) %>% 
          group_by(well) %>% 
          mutate(slope_well = slope_fold > 0) %>% 
          mutate(slope_flag = all(slope_well)) %>%
          mutate(abs_activity = max(value) > (max_value/2)) %>% 
          mutate(activity = ifelse(all(abs_activity,slope_flag),"Highly active","Unstable"))
          
        tmp <- tmp[!duplicated(tmp$well),]
        dat$activity <- tmp[match(dat$well,tmp$well),"activity",drop=T]
        return(dat)
      }
      
      dat <- activity_flag_fun(dat)
      return(dat)
    }
    fam_slope_analysis_fun(dat = res_fam())
  })
  
  fam_plot2_foldchange <- reactive({
    ### plot2: foldchange
    fam_plot2_foldchange_fun <- function(dat, unit_time,title1, palette){
      ggplot(dat,aes(x=time,y=foldchange,color=group,group=well))+
        geom_point()+
        geom_line()+
        geom_hline(yintercept = 1,lty=2)+
        facet_grid(group~sample)+
        theme_bw()+
        theme(legend.position = "right",
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x=paste0("Time (",unit_time,")"),y="Fold change",title = title1,
             color="Group")+
        scale_color_manual(values = palette)
    }
    fam_plot2_foldchange_fun(dat=fam_slope_dat(),unit_time = unit_time_fam(),
                             palette = palette_fam_plot2(),title1 = title_fam())
  })
  output$fam_plot2_foldchange <- renderPlot({
    fam_plot2_foldchange()
  })
  
  fam_plot2_activity <- reactive({
    ### plot2: activity
    fam_plot2_activity_fun <- function(dat, unit_time,title1, palette){
      ggplot(dat,aes(x=time,y=foldchange,color=activity,group=well))+
        geom_point()+
        geom_line()+
        geom_hline(yintercept = 1,lty=2)+
        facet_grid(group~sample)+
        theme_bw()+
        theme(legend.position = "right",
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x=paste0("Time (",unit_time,")"),y="Fold change",title = title1,
             color="Activity")+
        scale_color_manual(values = palette)
    }
    fam_plot2_activity_fun(dat=fam_slope_dat(),unit_time = unit_time_fam(),
                             palette = palette_fam_plot2(),title1 = title_fam())
  })
  output$fam_plot2_activity <- renderPlot({
    fam_plot2_activity()
  })
  
  fam_plot2_slope <- reactive({
    ### plot2: slope
    fam_plot2_slope_fun <- function(dat, unit_time,title1, palette){
      ggplot(dat,aes(x=time,y=slope_fold,color=activity,group=well))+
        geom_point()+
        geom_line()+
        geom_hline(yintercept = 0,lty=2)+
        facet_grid(group~sample)+
        theme_bw()+
        theme(legend.position = "right",
              strip.background = element_rect(fill = "#eaeae0"),
              plot.title = element_text(hjust = 0.5,face="bold",size=16))+
        labs(x=paste0("Time (",unit_time,")"),y="Slope of fold change",title = title1,
             color="Activity")+
        scale_color_manual(values = palette)
    }
    fam_plot2_slope_fun(dat=fam_slope_dat(),unit_time = unit_time_fam(),
                           palette = palette_fam_plot2(),title1 = title_fam())
  })
  output$fam_plot2_slope <- renderPlot({
    fam_plot2_slope()
  })
  
  output$ui_plot2_fam <- renderUI({
    if(!is.null(infile_fam())){
      dat <- fam_slope_dat()
      n_target <- length(unique(dat$group))
      n_crRNA <- length(unique(dat$sample))
      width1 <- n_crRNA *100
      width2 <- n_crRNA *100
      height1 <- n_target * 150
      height2 <- 500
      eventReactive(input$fam_plot2_action,{
        column(12,
               h3("Fold Change Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot2_foldchange_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot2_foldchange_pdf",label = "Download PDF"),
               br(),
               plotOutput(outputId = "fam_plot2_foldchange",width = width1,height = height1),
               h3("Activity Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot2_activity_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot2_activity_pdf",label = "Download PDF"),
               plotOutput(outputId = "fam_plot2_activity",width = width1,height = height1),
               h3("Slope Plot:"),
               br(),
               downloadButton(outputId = "fam_down_plot2_slope_png",label = "Download PNG"),
               downloadButton(outputId = "fam_down_plot2_slope_pdf",label = "Download PDF"),
               plotOutput(outputId = "fam_plot2_slope",width = width1,height = height1),
        )
      })()
    }
  })
  # download fam
  # download
  output$down_summ_table_fam <- downloadHandler(
    filename = paste0("FAM-BHQ_Summary_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(summ_fam(),file)
    }
  )
  output$fam_down_heat_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Heatmap_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = ggplotify::as.ggplot(fam_heatmap_plot()))
    }
  )
  output$fam_down_heat_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Heatmap_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = ggplotify::as.ggplot(fam_heatmap_plot()))
    }
  )
  output$fam_down_plot_line_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_line_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot_line())
    }
  )
  output$fam_down_plot_line_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_line_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot_line())
    }
  )
  output$fam_down_plot_errorbar_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_errorbar_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot_errorbar())
    }
  )
  output$fam_down_plot_errorbar_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_errorbar_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot_errorbar())
    }
  )  
  output$fam_down_plot_bar_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_bar_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot_bar())
    }
  )
  output$fam_down_plot_bar_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot_bar_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot_bar())
    }
  )
  # fam_plot2_download_foldchange
  output$fam_down_plot2_foldchange_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_foldchange_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot2_foldchange())
    }
  )
  output$fam_down_plot2_foldchange_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_foldchange_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot2_foldchange())
    }
  )  
  # fam_plot2_download_activity
  output$fam_down_plot2_activity_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_activity_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot2_activity())
    }
  )
  output$fam_down_plot2_activity_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_activity_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot2_activity())
    }
  )  
  # fam_plot2_download_slope
  output$fam_down_plot2_slope_png <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_slope_",Sys.Date(),".png"),
    content = function(file){
      ggsave(filename = file, device = "png",
             width = input$plot_width_fam, height = input$plot_height_fam,
             dpi = 300, plot = fam_plot2_slope())
    }
  )
  output$fam_down_plot2_slope_pdf <- downloadHandler(
    filename = paste0("FAM-BHQ_Plot2_slope_",Sys.Date(),".pdf"),
    content = function(file){
      ggsave(filename = file, device = "pdf",
             width = input$plot_width_fam, height = input$plot_height_fam,
             plot = fam_plot2_slope())
    }
  )  
  # Base Process +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$base_process_side <- renderUI({
    column(12,
           fluidRow(
             column(4,
                    h3("Sequence to UPPER"),
                    textAreaInput(inputId = "seq2upper_base",label = "Input nucleotide sequence:"),
                    uiOutput(outputId = "out_seq2upper_base")),
             column(4,
                    h3("Sequence to lower"),
                    textAreaInput(inputId = "seq2lower_base",label = "Input nucleotide sequence:"),
                    uiOutput(outputId = "out_seq2lower_base")),
             column(4,
                    h3("Sequence Reverse"),
                    textAreaInput(inputId = "seq_rev_base",label = "Input nucleotide sequence:"),
                    uiOutput(outputId = "out_seq_rev_base")),
           ),
           fluidRow(
             column(6,
                    h3("DNA -> RNA"),
                    textAreaInput(inputId = "D2R_base",label = "Input DNA sequence:"),
                    uiOutput(outputId = "out_D2R_base")),
             column(6,
                    h3("RNA -> DNA"),
                    textAreaInput(inputId = "R2D_base",label = "Input RNA sequence:"),
                    uiOutput(outputId = "out_R2D_base")),
           ),
           fluidRow(
             column(6,
                    h3("DNA Complementary Chain"),
                    textAreaInput(inputId = "complement_DNA_base",label = "Input nucleotide sequence:"),
                    uiOutput(outputId = "out_complement_DNA_base")),
             column(6,
                    h3("RNA Complementary Chain"),
                    textAreaInput(inputId = "complement_RNA_base",label = "Input nucleotide sequence:"),
                    uiOutput(outputId = "out_complement_RNA_base"))
           ))
  })
  
  seq2upper_base <- eventReactive(input$calculate_base,{input$seq2upper_base})
  seq2lower_base <- eventReactive(input$calculate_base,{input$seq2lower_base})
  seq_rev_base <- eventReactive(input$calculate_base,{input$seq_rev_base})
  D2R_base <- eventReactive(input$calculate_base,{input$D2R_base})
  R2D_base <- eventReactive(input$calculate_base,{input$R2D_base})
  complement_DNA_base <- eventReactive(input$calculate_base,{input$complement_DNA_base})
  complement_RNA_base <- eventReactive(input$calculate_base,{input$complement_RNA_base})
  
  # seq to upper
  out_seq2upper_base <- reactive({
    seq2upper <- function(sequence){
      ref <- setNames(c("A","T","U","G","C","N","A","T","U","G","C","N"),
                      c("a","t","u","g","c","n","A","T","U","G","C","N"))
      sep_sequence <- unlist(strsplit(sequence,""))
      upper_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(upper_sequence)){
        upper_sequence <- "Input error!"
      }
      return(upper_sequence)
    }
    seq2upper(sequence = seq2upper_base())
  })
  output$out_seq2upper_base <- renderUI({
    if(seq2upper_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_seq2upper_base())
    }
  })
  
  # seq to lower
  out_seq2lower_base <- reactive({
    seq2lower <- function(sequence){
      ref <- setNames(c("a","t","u","g","c","n","a","t","u","g","c","n"),
                      c("A","T","U","G","C","N","a","t","u","g","c","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      lower_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(lower_sequence)){
        lower_sequence <- "Input error!"
      }
      return(lower_sequence)
    }
    seq2lower(sequence = seq2lower_base())
  })
  output$out_seq2lower_base <- renderUI({
    if(seq2lower_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_seq2lower_base())
    }
  })
  
  # seq reverse
  out_seq_rev_base <- reactive({
    rev_seq <- function(sequence){
      ref <- setNames(c("A","T","G","C","U","a","t","g","c","u","N","n"),
                      c("A","T","G","C","U","a","t","g","c","u","N","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      rev_sequence <- ref[sep_sequence] %>% rev() %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(rev_sequence)){
        rev_sequence <- "Input error!"
      }
      return(rev_sequence)
    }
    rev_seq(sequence = seq_rev_base())
  })
  output$out_seq_rev_base <- renderUI({
    if(seq_rev_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_seq_rev_base())
    }
  })
  
  # T2U: DNA -> RNA
  out_D2R_base <- reactive({
    T2U <- function(sequence){
      # stringr::str_replace_all(string = sequence,pattern = "[T]",replacement = "U") %>% 
      #   stringr::str_replace_all(pattern = "[t]",replacement = "u")
      ref <- setNames(c("A","U","G","C","a","g","u","c","N","n"),
                      c("A","T","G","C","a","g","t","c","N","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      T2U_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(T2U_sequence)){
        T2U_sequence <- "Input error!"
      }
      return(T2U_sequence)
    }
    T2U(sequence = D2R_base())
    
  })
  output$out_D2R_base <- renderUI({
    if(D2R_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_D2R_base())
    }
  })
  
  # U2T: RNA -> DNA
  out_R2D_base <- reactive({
    U2T <- function(sequence){
      # stringr::str_replace_all(string = sequence,pattern = "[U]",replacement = "T") %>% 
      #   stringr::str_replace_all(pattern = "[u]",replacement = "t")
      ref <- setNames(c("A","T","G","C","a","g","t","c","N","n"),
                      c("A","U","G","C","a","g","u","c","N","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      U2T_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(U2T_sequence)){
        U2T_sequence <- "Input error!"
      }
      return(U2T_sequence)
    }
    U2T(sequence = R2D_base())
  })
  output$out_R2D_base <- renderUI({
    if(R2D_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_R2D_base())
    }
  })
  
  # Complementary DNA
  out_complement_DNA_base <- reactive({
    complement_DNA <- function(sequence){
      ref <- setNames(c("A","T","G","C","a","g","t","c","N","n"),
                      c("T","A","C","G","t","c","a","g","N","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      com_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(com_sequence)){
        com_sequence <- "Input error!"
      }
      return(com_sequence)
    }
    complement_DNA(sequence = complement_DNA_base())
  })
  output$out_complement_DNA_base <- renderUI({
    if(complement_DNA_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_complement_DNA_base())
    }
  })
  
  # Complementary RNA
  out_complement_RNA_base <- reactive({
    complement_RNA <- function(sequence){
      ref <- setNames(c("A","U","G","C","a","g","u","c","N","n"),
                      c("U","A","C","G","u","c","a","g","N","n"))
      sep_sequence <- unlist(strsplit(sequence,""))
      com_sequence <- ref[sep_sequence] %>% 
        paste0(collapse = "")
      if(str_length(sequence) != str_length(com_sequence)){
        com_sequence <- "Input error!"
      }
      return(com_sequence)
    }
    complement_RNA(sequence = complement_RNA_base())
  })
  output$out_complement_RNA_base <- renderUI({
    if(complement_RNA_base() != ""){
      textInput(inputId = "out_complement_RNA_base_i",label = NULL,
                value = out_complement_RNA_base())
      # html_content <- paste0("<style> #demo {color:#e31a1c;background-color:#f2f2f2}</style>
      #                      <textarea id='demo' name='textarea' cols='78' rows='3' readonly>",
      #                        out_complement_RNA_base(),
      #                        "</textarea>")
      # HTML(html_content)
    }
    
  })
  # mismatch finder
  output$mismatch_side <- renderUI({
    column(12,
           column(6,
                  fluidRow(p("Input DNA/RNA/Amino acid sequences:",style="font-size:20px")),
                  fluidRow(textAreaInput(inputId = "base_mismatch_seq1",label = "Seq1:")),
                  fluidRow(textAreaInput(inputId = "base_mismatch_seq2",label = "Seq2:"))),
           column(6,
                  uiOutput(outputId = "base_mismatch_res")
                  ))
  })
  
  base_mismatch_seq1 <- eventReactive(input$calculate_base_mismatch,{input$base_mismatch_seq1})
  base_mismatch_seq2 <- eventReactive(input$calculate_base_mismatch,{input$base_mismatch_seq2})
  base_mismatch_res <- eventReactive(input$calculate_base_mismatch,{
    # mismatch finder
    mismatch_finder <- function(seq1,seq2){
      ref <- c(LETTERS,letters)
      seq1 <- unlist(strsplit(seq1,""))
      seq2 <- unlist(strsplit(seq2,""))
      if(any(!all(seq1 %in% ref),!all(seq2 %in% ref))){
        return("Input Error! Sequences contain non-letter characters, please check them!")
      }
      if(length(seq1)!=length(seq2)){
        return("Error! The lengths of Seq1 and Seq2 are not equal!")
      }
      mis_id <- which(!seq1 == seq2)
      if(length(mis_id) == 0){
        return("No mismaths were found between Seq1 and Seq2.")
      }
      res <- data.frame(position = mis_id,
                        Seq1 = seq1[mis_id],
                        Seq2 = seq2[mis_id],
                        abbreviation = sapply(mis_id,function(i){
                          return(paste0(seq1[i],i,seq2[i]))
                        }))
      return(res)
    }
    mismatch_finder(seq1 = base_mismatch_seq1(), seq2 = base_mismatch_seq2())
    
  })
  output$base_mismatch_res <- renderUI({
    res <- base_mismatch_res()
    if(is.character(res)){
      output$base_mismatch_res_text <- renderText(base_mismatch_res())
      res_ui <- column(6,
                       br(),br(),br(),
                       textOutput("base_mismatch_res_text"),
                       tags$head(tags$style("#base_mismatch_res_text{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                       )))
    }
    if(is.data.frame(res)){
      output$base_mismatch_res_table <- renderDataTable(base_mismatch_res(),
                                                        options = list(pageLength=5))
      res_ui <- column(6,
                       downloadButton(outputId = "down_base_mismatch_res",label = "Download Result"),
                       dataTableOutput(outputId = "base_mismatch_res_table"))
    }
    return(res_ui)
  })
  
  # Base Process: Download
  output$down_base_mismatch_res <- downloadHandler(
    filename = paste0("Mismatch_Finder_Result_",Sys.Date(),".csv"),
    content = function(file){
      write.csv(base_mismatch_res(),file = file)
    }
  )
  
  
  # Molecular Weight +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$mw_main <- renderUI({
    column(12,
      fluidRow(
        column(4,
               fluidRow(h2("MW of ssRNA:")),
               fluidRow(p("Formula = (An * 329.2) + (Un * 306.2) + (Cn * 305.2) + (Gn * 345.2) + (Nn * 320.5) + 159.0")),
               fluidRow(textAreaInput(inputId = "mw_input_ssRNA",label = "Input RNA sequence:")),
               uiOutput(outputId = "mw_output_ssRNA"),
               
        ),
        column(4,
               fluidRow(h2("MW of ssDNA:")),
               fluidRow(p("Formula = (An * 313.2) + (Tn * 304.2) + (Cn * 289.2) + (Gn * 329.2) + (Nn * 303.7) + 79.0")),
               fluidRow(textAreaInput(inputId = "mw_input_ssDNA",label = "Input DNA sequence:")),
               uiOutput(outputId = "mw_output_ssDNA"),
               
        ),
        column(4,
               fluidRow(h2("MW of dsDNA:")),
               fluidRow(p("Formula = (An * 313.2) + (Tn * 304.2) + (Cn * 289.2) + (Gn * 329.2) + (Nn * 303.7) + 157.9")),
               fluidRow(textAreaInput(inputId = "mw_input_dsDNA",label = "Input DNA sequence:")),
               uiOutput(outputId = "mw_output_dsDNA"),
               
        )),
      fluidRow(
        column(8,
               fluidRow(h2("MW of peptide/protein:")),
               # fluidRow(p("")),
               fluidRow(textAreaInput(inputId = "mw_input_protein",label = "Input AA residue sequence:")),
               uiOutput(outputId = "mw_output_protein"),
               
        )),
      )
  })
  
  
  output$mw_convert_main <- renderUI({
    column(12,
           column(6,
                  fluidRow(h3(strong("Mass/Volume")," to ",strong("Molar mass/Volume"),":")),
                  fluidRow(
                    column(3,numericInput(inputId = "mw_convert_input_mw",label = "Molecular Weight (g/mol):",value = NULL)),
                    column(3,numericInput(inputId = "mw_convert_input_concentration",label = "Concentration:",value = NULL)),
                    column(3,selectInput(inputId = "mw_convert_input_massvolume_unit",label = "Mass/Volume Unit:",
                                         choices = c("ng/uL","mg/mL","g/L"),multiple = FALSE)),
                  ),
                  uiOutput(outputId = "mw_convert_output")),
           column(6,
                  fluidRow(h3(strong("Molar mass/Volume")," to ",strong("Mass/Volume"),":")),
                  fluidRow(
                    column(3,numericInput(inputId = "mw_convert_input_mw2",label = "Molecular Weight (g/mol):",value = NULL)),
                    column(3,numericInput(inputId = "mw_convert_input_concentration2",label = "Concentration:",value = NULL)),
                    column(3,selectInput(inputId = "mw_convert_input_molvolume_unit",label = "Molar mass/Volume Unit:",
                                         choices = c("uM","mM","nM","M"),multiple = FALSE))
                  ),
                  uiOutput(outputId = "mw_convert_output2"))
           
           
           )
  })
  
  # MW calculate
  mw_info <- eventReactive(input$calculate_mw,{
    info <- data.frame(type = c(rep("ssRNA",5), rep("ssDNA",5), rep("dsDNA",5), rep("protein",22)),
                       residue = c(c("A","U","C","G","N"),c("A","T","C","G","N"),c("A","T","C","G","N"),
                                   c("A", "R", "N", "D", "C", 
                                     "E", "Q", "G", "H", "O",
                                     "I", "L", "K", "M", "F",
                                     "P", "U", "S", "T", "W",
                                     "Y", "V")),
                       mw = c(c(329.2, 306.2, 305.2, 345.2, 320.5), 
                              c(313.2, 304.2, 289.2, 329.2, 303.7),
                              c(617.4, 617.4, 618.4, 618.4, 607.4),
                              c(71.08, 156.19, 114.1, 115.09, 103.14,
                                129.11, 128.13, 57.05, 137.14, 113.11,
                                113.16, 113.16, 128.17, 131.20, 147.18,
                                97.12, 111.09, 87.08, 101.10, 186.21,
                                163.17, 99.13)))
    info %>% dplyr::arrange(type, residue)
  })

  mw_input_ssRNA <- eventReactive(input$calculate_mw,{input$mw_input_ssRNA})
  mw_input_ssDNA <- eventReactive(input$calculate_mw,{input$mw_input_ssDNA})
  mw_input_dsDNA <- eventReactive(input$calculate_mw,{input$mw_input_dsDNA})
  mw_input_protein <- eventReactive(input$calculate_mw,{input$mw_input_protein})
  
  mw_calculate_fun <- eventReactive(input$calculate_mw,{
    function(seq1, type1, info){
      mw_info <- info %>% dplyr::filter(type == type1)
      ref <- mw_info$residue
      seq1 <- unlist(strsplit(seq1,"")) %>% toupper()
      if(!all(seq1 %in% ref)){
        res <- "Input error! Please check the sequence."
      }
      
      if(all(seq1 %in% ref)){
        intercept <- setNames(c(159.0, 79.0, 157.9, 18.0),nm = c("ssRNA", "ssDNA", "dsDNA", "protein"))
        unit1 <- setNames(c("nt","nt","bp","AA"),nm = c("ssRNA", "ssDNA", "dsDNA", "protein"))
        residue_freq <- as.data.frame(table(seq1))
        colnames(residue_freq)[1] <- "residue"
        res <- dplyr::inner_join(mw_info, residue_freq,by="residue")
        res$result <- res$mw * res$Freq
        res_raw <- sum(res$result) + intercept[type1]
        res$contain <- paste0(res$residue,": ",res$Freq)
        out_length <- paste0("The length of ",type1,": ",length(seq1)," ",unit1[type1],";\n")
        out_contain <- paste0("Contains: ",paste(res$contain,collapse = ", "),";\n")
        out_mw <- paste0("Molecular Weight: ",res_raw," g/mol; ", round(res_raw*10^(-3),3)," kDa.")
        res <- paste0(out_length, out_contain, out_mw)
      }
      return(res)
    }
  })
  
  
  output$mw_output_ssRNA <- renderUI({
    if(mw_input_ssRNA() != ""){
      output$mw_output_ssRNA_text <-  
        renderText(mw_calculate_fun()(seq1=mw_input_ssRNA(),type1="ssRNA",info=mw_info()))
      res_ui <- fluidRow(
        verbatimTextOutput(outputId = "mw_output_ssRNA_text"),
        tags$head(tags$style("#mw_output_ssRNA_text{color: #f0027f;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
        )))
      return(res_ui)
    }
    
  })
  
  output$mw_output_ssDNA <- renderUI({
    if(mw_input_ssDNA() != ""){
      output$mw_output_ssDNA_text <-  
        renderText(mw_calculate_fun()(seq1=mw_input_ssDNA(),type1="ssDNA",info=mw_info()))
      res_ui <- fluidRow(
        verbatimTextOutput(outputId = "mw_output_ssDNA_text"),
        tags$head(tags$style("#mw_output_ssDNA_text{color: #33a02c;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
        )))
      return(res_ui)
    }
    
  })
  
  output$mw_output_dsDNA <- renderUI({
    if(mw_input_dsDNA() != ""){
      output$mw_output_dsDNA_text <-  
        renderText(mw_calculate_fun()(seq1=mw_input_dsDNA(),type1="dsDNA",info=mw_info()))
      res_ui <- fluidRow(
        verbatimTextOutput(outputId = "mw_output_dsDNA_text"),
        tags$head(tags$style("#mw_output_dsDNA_text{color: #6a3d9a;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
        )))
      return(res_ui)
    }
    
  })
  
  output$mw_output_protein <- renderUI({
    if(mw_input_protein() != ""){
      output$mw_output_protein_text <-  
        renderText(mw_calculate_fun()(seq1=mw_input_protein(),type1="protein",info=mw_info()))
      res_ui <- fluidRow(
        verbatimTextOutput(outputId = "mw_output_protein_text"),
        tags$head(tags$style("#mw_output_protein_text{color: #f46d43;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
        )))
      return(res_ui)
    }
    
  })
  
  # Concentration Unit Conversion
  mw_convert_input_mw <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_mw})
  mw_convert_input_concentration <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_concentration})
  mw_convert_input_massvolume_unit <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_massvolume_unit})
  
  mw_convert_input_mw2 <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_mw2})
  mw_convert_input_concentration2 <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_concentration2})
  mw_convert_input_molvolume_unit <- eventReactive(input$calculate_mw_convert,{input$mw_convert_input_molvolume_unit})
  
  output$mw_convert_output <- renderUI({
    if(!any(is.na(mw_convert_input_mw()), is.na(mw_convert_input_concentration()))){
      convert1_fun <- function(mw,concen,unit1){
        unit_factor <- switch(unit1,
                              "ng/uL" = 10^(-3),
                              "mg/mL" = 1,
                              "g/L" = 1)
        res <- concen * unit_factor / mw # g/L / g/mol = mol/L
        res_text <- paste0("MW: ",mw," g/mol; Concentration: ",concen," ",unit1,";\n",
                           "Molar concentration:", signif(res,6), " M (mol/L);\n",
                           "                    ", signif(res*10^(3),6), " mM (mmol/L);\n",
                           "                    ", signif(res*10^(6),6), " uM (umol/L);\n",
                           "                    ", signif(res*10^(9),6), " nM (nmol/L).")
        return(res_text)
        
      }
      output$mw_convert_output_text <- renderText(convert1_fun(mw = mw_convert_input_mw(),
                                                              concen = mw_convert_input_concentration(),
                                                              unit1 = mw_convert_input_massvolume_unit()))
      
      res_ui <- column(10,
                       fluidRow(
                         verbatimTextOutput(outputId = "mw_convert_output_text"),
                         tags$head(tags$style("#mw_convert_output_text{color: #386cb0;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
                         ))))
      return(res_ui)
     
    }
  })
  
  
  output$mw_convert_output2 <- renderUI({
    if(!any(is.na(mw_convert_input_mw2()), is.na(mw_convert_input_concentration2()))){
      convert2_fun <- function(mw,concen,unit1){
        unit_factor <- switch(unit1,
                              "uM" = 10^(-6),
                              "mM" = 10^(-3),
                              "nM" = 10(-9),
                              "M" = 1)
        res <- concen * mw * unit_factor  # g/mol * mol/L = g/L
        res_text <- paste0("MW: ",mw," g/mol; Concentration: ",concen," ",unit1,";\n",
                           "Molar concentration:", signif(res,6), " g/L;\n",
                           "                    ", signif(res,6), " mg/mL;\n",
                           "                    ", signif(res*10^(3),6), " ng/uL.")
        return(res_text)
      }
      output$mw_convert_output2_text <- renderText(convert2_fun(mw = mw_convert_input_mw2(),
                                                               concen = mw_convert_input_concentration2(),
                                                               unit1 = mw_convert_input_molvolume_unit()))
      res_ui <- column(10,
                       fluidRow(
                         verbatimTextOutput(outputId = "mw_convert_output2_text"),
                         tags$head(tags$style("#mw_convert_output2_text{color: #bf5b17;
                                 font-size: 18px;
                                 font-style: bold;
                                 }"
                         ))))
      return(res_ui)
    }
  })
  
  # Tutorial +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$tu_down_qPCR_demo_eds <- downloadHandler(
    filename = paste0("qPCR_demo.eds"),
    content = function(file){
      file.copy(from = "./demo_data/qPCR_demo.eds", to = file)
    })
  
  output$tu_down_qPCR_demo_xlsx <- downloadHandler(
    filename = paste0("qPCR_demo.xlsx"),
    content = function(file){
      file.copy(from = "./demo_data/qPCR_demo.xlsx", to = file)
    })
  
  output$tu_down_CCK8_demo_xlsx <- downloadHandler(
    filename = paste0("CCK8_demo.xlsx"),
    content = function(file){
      file.copy(from = "./demo_data/CCK8_demo.xlsx", to = file)
    })
  
  output$tu_down_CTG_demo_xlsx <- downloadHandler(
    filename = paste0("CTG_demo.xlsx"),
    content = function(file){
      file.copy(from = "./demo_data/CTG_demo.xlsx", to = file)
    })
  
  output$tu_down_WB_demo_xlsx <- downloadHandler(
    filename = paste0("WB_demo.xlsx"),
    content = function(file){
      file.copy(from = "./demo_data/WB_demo.xlsx", to = file)
    })
  
}
# Run the application 
shinyApp(ui = ui, server = server)


