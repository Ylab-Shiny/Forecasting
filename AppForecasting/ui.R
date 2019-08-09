##############################################################################################################################
#### 建物エネルギーデータ分析ツール ui.R #####################################################################################
##############################################################################################################################

# パッケージ一覧 -----------------------------------------------------------------
library(shiny)
library(shinydashboard)

# Webページ構成要素 --------------------------------------------------------------
# header #
header <- dashboardHeader(title = "Building Electricity Analysys Tool", titleWidth = 500)

# sidebar #
sidebar <- dashboardSidebar(
  # サイドバーメニュー
  sidebarMenu(
    menuItem("Dataset [kWh]", tabName = "table"),
    menuItem("Trend grapg", tabName = "trend", badgeColor = "red"),
    menuItem("clustering", tabName = "clustering")
  ),
  
  # ファイルのアップロードUI
  fileInput("file", "upload the csv file",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # カレンダーの出力
  uiOutput("DateRange"),
  
  # トレンドグラフに描画する項目選択
  uiOutput("selectDeps"),
  
  sliderInput(inputId = "RangeY", label = "Y axis（electricity load[kW]）select the range",
              min = 0, max = 4000, value = c(0, 4000), step = 50),
  
  # クラスタリングの対象とする項目の選択
  uiOutput("target_cluster")
  
)

# body #
body <- dashboardBody(
  
  tabItems(
    tabItem(
      h1("welcome to 『Building energy analysis tool』"),
      tabName = "table",
      DT::dataTableOutput("DataTable")),
    
    tabItem(tabName = "trend",
            
            # トレンドグラフの描画
            plotOutput("trendGragh")
            ),
    
    tabItem(tabName = "clustering",
            
            # アイコン
            infoBoxOutput(width = 3, "Max"),
            infoBoxOutput(width = 3, "Min"),
            infoBoxOutput(width = 3, "Mean"),
            # クラスタセンターの描画
            plotOutput("qqq")
            )
    
  )
)


# 構成要素の組み立て ---------------------------------------------------------------
dashboardPage(header, sidebar, body)