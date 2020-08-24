## app.R ##
library(shinydashboard)
library(rio)
library(tidyverse)
library(reactable)
library(ggthemes)
library(shiny)
library(plotly)
library(lubridate)
library(DT)


################### EMPLOYMENT DATA ###################################

data_list<-data_list <- import_list("I5_LABOUR_1975_2019_Q4_2019.xlsx")
labour<-data_list$Table_I5B
labour<-labour[-c(1:3),]
labour_data<-labour[2:41,]
colnames(labour_data)<-labour[1,]
labour_data_test<-lapply(labour_data, as.numeric)
labour_data_test<-as.data.frame(labour_data_test)
colnames(labour_data_test)<-labour[1,]
labour_data_test[,1]<-labour_data[,1]
labour_data_test[,-1]<-round(labour_data_test[,-1],1)
labour_data<-labour_data_test


labour_data$year<-gsub("1Q","",labour_data$`Period Ended`)
labour_data$year<-gsub("2Q","",labour_data$year)
labour_data$year<-gsub("3Q","",labour_data$year)
labour_data$year<-gsub("4Q","",labour_data$year)
labour_data$year<-as.numeric(labour_data$year)
labour_data$period<-ifelse(str_detect(labour_data$`Period Ended`, "4Q"), "annnual", "quarterly")
labour_data$date <-
    if_else(
        str_detect(labour_data$`Period Ended`, "1Q"),
        paste("3/31/", labour_data$year),
        if_else(
            str_detect(labour_data$`Period Ended`, "2Q"),
            paste("6/30/", labour_data$year),
            if_else(
                str_detect(labour_data$`Period Ended`, "3Q"),
                paste("9/30/", labour_data$year),
                if_else(
                    str_detect(labour_data$`Period Ended`, "4Q"),
                    paste("12/31/", labour_data$year),""
                )
            )
        )
    )


labour_data$date<-as.Date(labour_data$date, "%m/%d/%Y")
fct_reorder(labour_data$`Period Ended`, labour_data$date)

table_data<-labour_data[,-c(19,20,21)]

labour_data_long<-pivot_longer(labour_data, 2:18, "industry", values_to  = "quarterly")

labour_data_long$annual<-if_else(month(labour_data_long$date)==12, labour_data_long$quarterly, 0)

###################### INFLATION DATA #####################

inflation<-read_xlsx("inflation.xlsx")
colnames(inflation)<-c("date", "may_94_ma", "jul_01_ma","jul_01_rw_ma", "jul_01_rw_ptp")
inflation$jul_01_rw_ma<-as.numeric(inflation$jul_01_rw_ma)
inflation$jul_01_rw_ptp<-as.numeric(inflation$jul_01_rw_ptp)
inflation$date<-as_date(inflation$date)
inflation_long<-pivot_longer(inflation, cols = c(2:5), names_to = "type", values_to = "rate")

testvalue<-inflation_long%>%filter(type=="jul_01_rw_ma", date==max(date))%>%select(rate)
testvalue2<-inflation_long%>%filter(type=="jul_01_rw_ptp", date==max(date))%>%select(rate)



########################################################


ui <- dashboardPage(
    dashboardHeader(title = "Demo Dashboard"),
    dashboardSidebar(
        sidebarMenu(id="sidebarid",
                    
                    
                    
            menuItem("LABOUR DATA", tabName = "labour", icon = icon("laptop")
                     ),
            menuItem("INFLATION DATA", tabName = "inflation", icon = icon("chart-line")),
            conditionalPanel(
                'input.sidebarid == "labour"',
            radioButtons(
                inputId = "Period",
                choiceNames = c("Quarterly", "Annual"),
                choiceValues = c("quarterly", "annual"),
                "Data Frequency",
                inline = TRUE,
                selected = "quarterly"),
            
            dateRangeInput(inputId = "date_range", 
                           "Choose Time Period", 
                           start = min(labour_data_long$date), 
                           end = max(labour_data_long$date)),
            
            selectInput(
                "Industry",
                label = "Select Industry:",
                choices = c(
                    "Agriculture, Forestry & Fishing" ,
                    "Manufacturing"  ,
                    "Electricity, Gas, Steam, Water & Air Conditioning Supply",
                    "Construction, Mining & Quarrying" ,
                    "Wholesale & Retail Trade" ,
                    "Accommodation & Food Services"   ,
                    "Transportation & Storage"   ,
                    "Finance & Insurance"  ,
                    "Professional, Scientific & Technical Services"  ,
                    "Public Administration & Defence"   ,
                    "Administrative & Support Service"  ,
                    "Education"      ,
                    "Human Health & Social Work"  ,
                    "Activities of Households as Employers"   ,
                    "Other Groups*"  ,
                    "Other Services"  ,
                    "Total Employed"
                    
                ),
                
                selected = "Total Employed")
            ),
            
            
            conditionalPanel(
                'input.sidebarid == "inflation" && input.tabset3=="INFLATION RATE"',
                radioButtons(
                    inputId = "Type",
                    choiceNames = c("Moving Average", "Point to Point"),
                    choiceValues = c("Moving Average", "Moving Average vs Point to Point"),
                    "Data Type",
                     selected = "Moving Average vs Point to Point"),
                
                dateRangeInput(inputId = "date_range_inf", 
                               "Choose Time Period", 
                               start = "2015-01-01", 
                               end = max(inflation$date))
                
                
            )
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "labour",
                    fluidRow(
                        tabBox(id="tabset1", width=12,
                               tabPanel("Number of Employed(000's)",
                                        fluidRow(plotlyOutput("plot1")),
                                        fluidRow(DTOutput("table1")))
                            )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "inflation",
                    fluidRow(tabBox(id="tabset3", width=12,
                           tabPanel("INFLATION RATE",
                                    fluidRow(
                                    valueBox(paste0(round(testvalue,2),"%"), "12 Month Moving Average", width = 6, color = "orange"),
                                    valueBox(paste0(round(testvalue2,2),"%"), "Point to Point", width=6, color="green")),
                                    fluidRow(plotlyOutput("plot3")),
                                    fluidRow(DTOutput("table2")),),
                                   
                                    
                            tabPanel("RETAIL PRICING INDEX",
                                     h2("BLANK FOR ILLUSTATIVE PURPOSES"))))
            )
        )
    )
)

server <- function(input, output) {
    
    
    
    
    
    output$plot1 <- 
        renderPlotly({
            
            if ("annual" %in% input$Period) {
                plot_data <-
                    labour_data_long %>% 
                    filter(industry == input$Industry) %>% 
                    filter(date %in% seq(input$date_range[1],     
                                         input$date_range[2], 
                                         by = "day")) %>% 
                    filter(annual > 0) %>% 
                    select(date, industry, annual, `Period Ended`)
            }
            else{
                plot_data <-
                    labour_data_long %>% 
                    filter(industry == input$Industry) %>% 
                    filter(date %in% seq(input$date_range[1],     
                                         input$date_range[2], 
                                         by ="day")) %>% 
                    select(date, industry, quarterly, `Period Ended`)
            }
            
            
            
             
               p<-ggplot(plot_data,
                            aes_string(x = "date",
                                       y = input$Period)) +
                geom_col(
                    fill = "steelblue",
                    alpha = .7,
                    aes_string(text = input$Period),
                    group = 1
                )  + 
                theme_fivethirtyeight() +
                labs(title = plot_data$industry) +
                theme(panel.grid.major.x = element_blank(),
                      axis.text.x = element_text(angle = 90),
                      panel.background = element_rect(fill = "transparent"),
                      plot.background = element_rect(fill = "transparent")) + 
                scale_x_date(breaks = plot_data$date, labels = plot_data$`Period Ended`)
               
               ggplotly(p, tooltip = paste0("text")) %>% config(displayModeBar = FALSE)
            
                
        })
    
    
    
    output$plot2 <- 
        renderPlotly({
            
            i<-ggplot(
                inflation %>% 
                    filter(type!="jul_01_rw_ptp" & 
                               date>"1993-12-31" & 
                               type!=""),
                aes(
                    x = date,
                    y = rate,
                    group = type,
                    colour = type
                ))+
              geom_line(size=1)+
                labs(title="% Moving Average")+
                theme_fivethirtyeight()+
                scale_x_date(breaks = inflation$date)+
                theme(panel.background = element_rect(fill = "transparent"),
                      plot.background = element_rect(fill = "transparent"),
                      panel.grid.major.x = element_blank(),
                      legend.position = "right",
                      legend.background = element_rect(fill = "transparent"),
                      axis.text.x = element_text(angle = 90))
                
            ggplotly(i)%>%config(displayModeBar = FALSE)
            
        })
    
    
    
    output$plot3 <- 
        renderPlotly({
            
            
            if ("Moving Average" %in% input$Type) {
                plot_data <-
                    inflation_long %>% 
                    filter(type == "jul_01_ma" | type=="jul_01_rw_ma"| type==
                              "may_94_ma") %>% 
                    filter(date %in% seq(input$date_range_inf[1],     
                                         input$date_range_inf[2], 
                                         by = "day")) %>% 
                    filter(rate > 0) %>% 
                    select(date, type, rate)
            }
            else{
                plot_data <-
                    inflation_long %>% 
                    filter(type == "jul_01_rw_ma"| type==
                               "jul_01_rw_ptp") %>% 
                    filter(date %in% seq(input$date_range_inf[1],     
                                         input$date_range_inf[2], 
                                         by ="day")) %>% drop_na(rate)%>%
                    select(date, type, rate)
            }
            
            j<-ggplot(
                plot_data,
                aes(x=date, y=rate, group=type, colour=type))+
                geom_line()+
                labs(title=input$Type)+
                theme_fivethirtyeight()+
                scale_x_date(breaks = inflation$date)+
                theme(panel.background = element_rect(fill = "transparent"),
                      plot.background = element_rect(fill = "transparent"),
                      panel.grid.major.x = element_blank(),
                      legend.background = element_rect(fill = "transparent"),
                      axis.text.x = element_text(angle = 90))
            
            ggplotly(j)%>%config(displayModeBar = FALSE)
            
        })
    
    
    
    
    
    
    
    
    
    output$table1<-
        renderDT({
            
            datatable(
                table_data, rownames = FALSE,
                extensions = 'Buttons',
                options = list(dom = 'Blftrip',scrollX=TRUE,
                               buttons = c("csv", "excel", "pdf"),
                               lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
                               
                )
            )
        })

    
    output$table2<-
        renderDT({
            
            datatable(
                inflation, rownames = FALSE,
                extensions = 'Buttons',
                options = list(dom = 'Blftrip',scrollX=TRUE,
                               buttons = c("csv", "excel", "pdf"),
                               lengthMenu = list(c(10, 25, 50, -1), c(10, 25, 50, "All"))
                               
                )
            )
        })  
    
    
    
    
    
    }

shinyApp(ui, server)