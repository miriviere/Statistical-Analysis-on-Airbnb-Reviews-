library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyr)
library(rgdal)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(moments)
library(corrplot)

load(file="Airbnb.RData")
nyc_sf <- readOGR("geo_export_6143d480-ee3e-491f-b3da-e1a4446ae67d.shp")
row.names(nyc_sf) <- c("Queens", "Staten Island", "Bronx", "Brooklyn", "Manhattan")

colors<-c("#FF5A5F","#007D8C","#FFAB08","#767676","mediumorchid4") 

#============================================UI============================================

header <- dashboardHeader(title = "Airbnb NYC 2019",
                          dropdownMenu(type="notifications",
                                       notificationItem(text=paste("Total listings:", length(Airbnb_2$ID)), status="success"),
                                       badgeStatus="success")
                          )
sidebar <- dashboardSidebar(
                            sidebarMenu(
                              menuItem(tagList(icon("dollar-sign"), "Overview"),tabName = "overview"),
                              menuItem(tagList(icon("chart-bar",lib = "font-awesome"),"Descriptive stats"),tabName = "Descriptives",
                                       menuSubItem(tagList(icon("chart-pie"),"Univariate stats"),tabName = "Univariates"),
                                       menuSubItem(tagList(icon("chart-line"),"Multivariate stats"),tabName = "Multivariates")),
                              menuItem(tagList(icon("theater-masks"),"Sentiment analysis"),tabName = "sentimental")
                            )
)

body <- dashboardBody(
                      tabItems(
                              tabItem(tabName = "overview",
                                      fluidRow(
                                        column(width = 4,
                                               box(width=NULL,selectInput("Select_Neighbourhood_group","Select a county",
                                                                          choices=unique(Airbnb_2$Boroughs),
                                                                          multiple=TRUE, selected="Manhattan"))
                                               )),
                                      fluidRow(valueBoxOutput("Total_listings",width = 3),
                                               valueBoxOutput("Perc_positives",width = 3),
                                               valueBoxOutput("Perc_negatives",width = 3),
                                               valueBoxOutput("Most_expensive",width = 3)),
                                      fluidRow(
                                        column(width = 6,
                                               box(width = NULL, plotlyOutput("Plot_1"))),
                                        column(width = 6,
                                               box(width = NULL,tags$img(src="wordcloud.jpeg"))))
                                      ),
                          tabItem(tabName = "Univariates",
                                  fluidRow(column(width = 4,
                                                  box(width = NULL,selectInput("Select_variable_univariate","Select a variable to analyze",
                                                                               choices=c("Boroughs","Room_type","Price_USD","Minimum_nights","Number_of_reviews"), 
                                                                               multiple = FALSE, selected = "Price_USD"))),
                                           column(width = 4,
                                                  box(width = NULL,selectInput("Select_breakdown_univariate","Select a variable to breakdown",
                                                                               choices=c("Boroughs","Room_type"),
                                                                               multiple = FALSE,selected="Boroughs"))),
                                           column(width = 4,
                                                  box(width = NULL,selectInput("Select_outliers","Remove outiliers?",
                                                                               choices = c("Yes","No"),multiple = FALSE,selected = "Yes")))
                                           ),
                                  fluidRow(
                                    column(width = 6,
                                           box(width = NULL,plotlyOutput("Plot_2"))),
                                    column(width = 6,
                                           box(width = NULL,plotlyOutput("Plot_3")))
                                  ),
                                  fluidRow(
                                    column(width = 6,
                                           box(width = NULL,plotlyOutput("Plot_4"))),
                                    column(width = 6,
                                           box(width = NULL,dataTableOutput("Table_1")))
                                  )
                                  ),
                          tabItem(tabName = "Multivariates",
                                  fluidRow(
                                    column(width = 4,
                                                  box(width = NULL,selectInput("Select_variable_1_multivariate",
                                                                               "Select the X axis",
                                                                               choices = c("Price_USD","Minimum_nights","Number_of_reviews","Availability_365"),
                                                                               multiple = FALSE,selected = "Price_USD"))),
                                    column(width = 4,box(width = NULL,selectInput("Select_variable_2_multivariate",
                                                                                  "Select the Y axis",
                                                                                  choices = c("Price_USD","Minimum_nights","Number_of_reviews","Availability_365"),
                                                                                  multiple = FALSE,selected = "Number_of_reviews"))),
                                    column(width = 4,box(width = NULL,selectInput("Select_breakdown_multivariate",
                                                                                  "Select the points color",
                                                                                  choices=c("Boroughs","Room_type"),
                                                                                  multiple = FALSE,selected = "Boroughs"))),
                                    fluidRow(
                                      column(width = 6,
                                             box(width = NULL,plotlyOutput("Plot_7"))),
                                      column(width = 6,
                                             box(width = NULL,plotOutput("Plot_8"))))
                                    )
                                   ),
                          tabItem(tabName = "sentimental",
                                  fluidRow(
                                    column(width = 4,
                                           box(width = NULL,selectInput("Select_sentimental_breakdown",
                                                                        "Select a breakdown",
                                                                        choices=c("Boroughs","Room_type"),
                                                                        multiple = FALSE,selected="Boroughs")))),
                                  fluidRow(column(width = 6,
                                                  box(width = NULL,plotlyOutput("Plot_5"))),
                                           column(width = 6,
                                                  box(width = NULL,plotlyOutput("Plot_6")))),
                                  fluidRow(column(width = 6,
                                                  box(width = NULL,plotOutput("Plot_9"))),
                                           column(width = 6,
                                                  box(width = NULL,tags$img(src="Wordcloud_negative.jpeg")))),
                                  fluidRow(column(width = 6,
                                                  box(width = NULL,tags$img(src="Wordcloud_positive.jpeg"))))
                                  )
                      )
)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body,
                    skin="black"
)

#====================================================================App server=====================================================================
server<- shinyServer(function(session, input, output) {

#=================================================For the overview tab=============================
  
  Airbnb_neighboor<-reactive({Airbnb_2 %>% 
                              filter(Boroughs %in% input$Select_Neighbourhood_group)
    
  })
  
  #First value box: Total listings
  output$Total_listings<-renderValueBox({
                                    valueBox(value=prettyNum(length(Airbnb_neighboor()$ID),big.mark = " "),
                                             subtitle = "Total listings", icon = icon("hotel"), 
                                             color="purple")
  })
  
  #Second value box: percentage of positives
  
  Total_positive_reviews<-reactive({
    sum(Airbnb_neighboor()$Positive)
  })
  
  Total_neutral_reviews<-reactive({
    sum(Airbnb_neighboor()$Neutral)
  })
  
  Total_negative_reviews<-reactive({
    sum(Airbnb_neighboor()$Negative)
  })
  
  The_total_reviews<-reactive({
    Total_positive_reviews() + Total_neutral_reviews() + Total_negative_reviews()
  })
  
  output$Perc_positives<-renderValueBox({
    valueBox(value=paste(round(Total_positive_reviews()/The_total_reviews(),2)*100,"%"),
             subtitle = "Percentage of positive reviews", icon = icon("thumbs-up"), 
             color="olive")
  })
  
  #Third value box: percentage of negatives
  output$Perc_negatives<-renderValueBox({
    valueBox(value=paste(round(Total_negative_reviews()/The_total_reviews(),2)*100,"%"),
             subtitle = "Percentage of negative reviews", icon = icon("thumbs-down"), 
             color="red")
  })
  
  #Fourth value box: maximum of price
  output$Most_expensive<-renderValueBox({
    valueBox(value=paste("$",prettyNum(max(Airbnb_neighboor()$Price_USD),big.mark = " ")),
             subtitle = "More expensive listing (per night)", icon = icon("dollar-sign"), 
             color="light-blue")
  })


  
  #NYC map:
  Airbnb_nine_decile<-reactive({
    Airbnb_neighboor()[Airbnb_neighboor()$Positive>=quantile(Airbnb_neighboor()$Positive,0.90),]
  })
  
  
  GG_Plot_1<-reactive({
    ggplot(nyc_sf, aes(long,lat,group=group)) +
      geom_polygon(fill="gray77") +
      geom_point(data=Airbnb_nine_decile(),aes(x=longitude, y=latitude, group = NA, color=Positive)) +
      theme_minimal() +
      theme(legend.position="bottom",
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank()) +
      ggtitle("New York: 9th decile of positive reviews") +
      labs(colour="Positive \nreviews") +
      scale_color_gradient(low="#007D8C",high="#FF5A5F")
  })
  
  output$Plot_1<-renderPlotly({
    ggplotly(GG_Plot_1(),tooltip = c("colour","fill"))
  })
  
  #=====================================For the Univariates tab=================================
  
  Plot_univariate_1_df<-reactive({Airbnb_2 %>% 
      select(ID,input$Select_variable_univariate)
  })
  
  #If statements for the first plot:
  
  #First IF: for create the reactive object if the variable is numeric
  X_1<-reactive({
    is.numeric(Plot_univariate_1_df()[,2])
  })
  
  #In case the variable IS numeric and want to REMOVE outliers:
  Plot_univariate_1_df_2<-reactive({
    if(X_1())
    {
      if(input$Select_outliers=="Yes")
      {
        Plot_univariate_1_df() %>% 
          filter(Plot_univariate_1_df()[,2]<quantile(Plot_univariate_1_df()[,2],0.95))
      } else{
        Plot_univariate_1_df()
      }
    }
    
  })
    
  #In case it is NOT NUMERIC, create this data frame:
  Categorical_univariate_1_df<-reactive({
    if(X_1()==FALSE)
    {
      Plot_univariate_1_df() %>% group_by(Group=Plot_univariate_1_df()[,2]) %>% 
        summarise(Count=n()) %>% mutate(Perc=round(Count/sum(Count),2))
    }
  })
  
  #Create a boxplot ggplot object IF numeric, ELSE a barplot
  GG_Plot_2<-reactive({
    if(X_1())
    {
      ggplot(Plot_univariate_1_df_2(), aes(y=Plot_univariate_1_df_2()[,2])) +
        geom_boxplot(fill="#007D8C",outlier.shape = 21, outlier.fill = "firebrick") +
        ggtitle(paste("Boxplot of",names(Plot_univariate_1_df_2()[2]))) +
        ylab(names(Plot_univariate_1_df_2()[2])) +
        theme_minimal()
    }else{
      ggplot(Categorical_univariate_1_df(), aes(x=NA,y=Perc, fill=Group)) +
        geom_bar(width = 1, stat = "identity") +
        geom_text(aes(label=percent(Perc)),position=position_stack(vjust=0.5), color = "gray20", size = 4.5) +
        ggtitle(paste("Relative frequency of",names(Plot_univariate_1_df()[2]))) +
        ylab("Percentage") +
        scale_fill_manual(values=colors) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              axis.text= element_blank(),
              legend.position="bottom") 
    }
  })
  
  #Output the Plotly with all tips IF NUMERIC, else only the y and the group (fill)
  output$Plot_2<-renderPlotly({
    if(X_1())
    {
      ggplotly(GG_Plot_2())
    }else{
      ggplotly(GG_Plot_2(),tooltip = c("y","fill"))
    }
  })
  
  Plot_univariate_2_df<-reactive({Airbnb_2 %>% 
      select(ID,input$Select_variable_univariate,input$Select_breakdown_univariate)
    
  })
  
  #If statements for the second plot

  #First IF: for create the reactive object if the variable is numeric
  X_2<-reactive({
    is.numeric(Plot_univariate_2_df()[,2])
  })
  
  #In case the variable IS numeric and want to REMOVE outliers:
  Plot_univariate_2_df_2<-reactive({
    if(X_2())
    {
      if(input$Select_outliers=="Yes")
      {
        Plot_univariate_2_df() %>% 
          filter(Plot_univariate_2_df()[,2]<quantile(Plot_univariate_2_df()[,2],0.95))
      } else{
        Plot_univariate_2_df()
      }
    }
    
  })
  
  #In case it is NOT NUMERIC, create this data frame:
  Categorical_univariate_2_df<-reactive({
    if(X_2()==FALSE)
    {
      Plot_univariate_2_df() %>%
        select(names(Plot_univariate_2_df()[2]),names(Plot_univariate_2_df()[3])) %>% 
        group_by_all() %>%
        summarise(Count = n()) %>% 
        mutate(Freq=Count/sum(Count))
    }
  })
  
  #In case it is NOT NUMERIC, create this vector:
  Variable_names<-reactive({
    if(X_2()==FALSE)
    {
      colnames(Categorical_univariate_2_df())
    }
  })
  
  #Create a boxplot ggplot object IF numeric, ELSE a barplot
  GG_Plot_3<-reactive({
    if(X_2())
    {
      ggplot(Plot_univariate_2_df_2(), aes(x=Plot_univariate_2_df_2()[,3], y=Plot_univariate_2_df_2()[,2]))+
        geom_boxplot(aes(color=Plot_univariate_2_df_2()[,3])) +
        ggtitle(paste("Boxplot of",names(Plot_univariate_2_df_2()[2]),"by",names(Plot_univariate_2_df_2()[3])))+
        xlab(names(Plot_univariate_2_df_2()[3])) +
        ylab(names(Plot_univariate_2_df_2()[2])) +
        scale_color_manual(names(Plot_univariate_2_df_2()[3]),values=colors) +
        theme_minimal() 
    }else{
      ggplot(Categorical_univariate_2_df(), 
             aes_string(x=Variable_names()[1],y=Variable_names()[4],fill=Variable_names()[2])) +
        geom_bar(stat="identity") +
        geom_text(aes(label=percent(Freq)),position = position_stack(vjust = 0.5),
                  color="gray20",size=4) +
        scale_fill_manual(values=colors) +
        scale_y_continuous(labels=percent) +
        ggtitle(paste("Relative frequency of",Variable_names()[1],"by",Variable_names()[2]))+
        xlab(Variable_names()[1]) + ylab("Percentage") +
        theme_minimal()
    }
  })
  
  #Output the Plotly with all tips IF variable IS NUMERIC, else only the y and the group (fill)
  output$Plot_3<-renderPlotly({
    if(X_2())
    {
      ggplotly(GG_Plot_3())
    }else{
      ggplotly(GG_Plot_3(),tooltip = c("y","fill"))
    }
  })
  
  Plot_univariate_3_df<-reactive({Airbnb_2 %>% 
      select(ID,input$Select_variable_univariate)
  })
  
  #If statements for the third plot
  
  #First IF: for create the reactive object IF the variable IS NUMERIC
  X_3<-reactive({
    is.numeric(Plot_univariate_3_df()[,2])
  })
  
  #In case the variable IS numeric and want to REMOVE outliers:
  Plot_univariate_3_df_2<-reactive({
    if(X_3())
    {
      if(input$Select_outliers=="Yes")
      {
        Plot_univariate_3_df() %>% 
          filter(Plot_univariate_3_df()[,2]<quantile(Plot_univariate_3_df()[,2],0.95))
      } else{
        Plot_univariate_3_df()
      }
    }
    
  })
  
  #Create an histogram-ggplot object IF numeric, ELSE an empty plot
  GG_Plot_4<-reactive({
    if(X_3())
    {
      ggplot(Plot_univariate_3_df_2(), aes(Plot_univariate_3_df_2()[,2])) +
        geom_histogram(bins = length(hist(Plot_univariate_3_df_2()[,2])$counts),
                       fill="#007D8C") +
        ggtitle(paste("Histogram of",names(Plot_univariate_3_df_2()[2]))) +
        xlab(names(Plot_univariate_3_df_2()[2])) +
        ylab("Count") +
        theme_minimal()
    }else{
      ggplot() +
        theme_minimal()
    }
    
  })
  
  output$Plot_4<-renderPlotly({
    ggplotly(GG_Plot_4())
  })
  
  #Table for indexes
  
  For_summary_df<-reactive({Airbnb_2 %>% 
      select(ID,input$Select_variable_univariate)
  })
  
  #First IF: for create the reactive object IF the variable IS NUMERIC
  X_4<-reactive({
    is.numeric(For_summary_df()[,2])
  })
  
  #In case the variable IS numeric and want to REMOVE outliers:
  For_summary_df_2<-reactive({
    if(X_4())
    {
      if(input$Select_outliers=="Yes")
      {
        For_summary_df() %>% 
          filter(For_summary_df()[,2]<quantile(For_summary_df()[,2],0.95))
      } else{
        For_summary_df()
      }
    }
  })
  
  Indexes<-reactive({
    if(X_4()){
      data.frame(Index=c("Mean","Q1","Median","Q3","Skeweness","Standard deviation"),
                 Value=c(mean(For_summary_df_2()[,2]),quantile(For_summary_df_2()[,2],0.25),
                         median(For_summary_df_2()[,2]), quantile(For_summary_df_2()[,2],0.75),
                         skewness(For_summary_df_2()[,2]), 
                         sd(For_summary_df_2()[,2])))
    }
  })
  
  output$Table_1<-renderDataTable({
    datatable(Indexes())
  })
  
    
  #===================================For the sentimental tab================================== 
    
    Plot_Sentiment_1_df<-reactive({Airbnb_2 %>% 
        select(input$Select_sentimental_breakdown,Negative,Neutral,Positive)
    })
  
  
  Sentimental_1_df<-reactive({Plot_Sentiment_1_df() %>% summarise(Negatives=sum(Negative),
                                                                Neutrals=sum(Neutral),
                                                                Positives=sum(Positive)) %>% 
                    gather(key="Score",value="Total",1:3) %>% 
                    mutate(Perc=Total/sum(Total))
  })
  
  GG_Plot_5<- reactive({
    ggplot(Sentimental_1_df(), aes(x=NA,y=Perc, fill=Score)) + 
      geom_bar(width = 1, stat = "identity") +
      geom_text(aes(label=percent(Perc)),position=position_stack(vjust=0.5), color = "gray20", size = 4.5) +
      ggtitle("Sentimental score (percentages)") +
      scale_fill_manual(values=colors) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text= element_blank(),
            legend.position="bottom")
  })
  
  output$Plot_5<-renderPlotly({
    ggplotly(GG_Plot_5(),tooltip = c("y","fill"))
  })
  
  #Now the sentimental plot with breakdowns
  
  Plot_Sentiment_2_df<-reactive({
    Plot_Sentiment_1_df() %>% 
      rename("The_breakdown"=
               ifelse(names(Plot_Sentiment_1_df())[1]=="Boroughs","Boroughs","Room_type"))
  })
  
  Sentiment_2_df<-reactive({
    Plot_Sentiment_2_df() %>% group_by(The_breakdown) %>% 
      summarise(Negatives=sum(Negative),
                Neutrals=sum(Neutral),
                Positives=sum(Positive)) %>% 
      gather(key="Score",value="Total",2:4) %>% 
      group_by(The_breakdown) %>% 
      mutate(Perc = Total / sum(Total)) 
  })
  
  GG_Plot_6<-reactive({
    ggplot(Sentiment_2_df(), 
           aes(x=The_breakdown,y=Perc,fill=Score)) +
      geom_bar(stat="identity") +
      geom_text(aes(label=percent(Perc)),position = position_stack(vjust = 0.5),
                color="gray20",size=4) +
      scale_fill_manual(values=colors) +
      scale_y_continuous(labels=percent) +
      ggtitle(paste("Sentimental score by",names(Plot_Sentiment_1_df())[1])) +
      xlab(names(Plot_Sentiment_1_df())[1]) + ylab("Percentage") +
      theme_minimal()
  })
  
  output$Plot_6<-renderPlotly({
    ggplotly(GG_Plot_6(),tooltip = c("y","fill"))
  })
  
  #Correlations for sentiment analysis
  
  Multivariate_corr_2<-reactive({Airbnb_2 %>% 
      select(Price_USD,Availability_365,Negative,Positive) %>% 
      filter(Price_USD<=quantile(Airbnb_2$Price_USD,0.95)) %>% rename("Price"="Price_USD",
                                                                      "Availability"="Availability_365",
                                                                      "Negative rev."="Negative",
                                                                      "Positive rev."="Positive")
  })
  
  corr_2<-reactive({
    cor(Multivariate_corr_2())
  })
  
  output$Plot_9<-renderPlot({
    corrplot(corr_2(), method="number",
             col=colorRampPalette(c("#FF5A5F","#FC642D","#00A699"))(100), 
             tl.col="black", 
             tl.srt=45,
             title="Correlation for NY Reviews",
             mar = c(1,1,2,1),
             number.cex = 0.85,
             cl.align.text="l")
  })
  
  
  
  #=====================================For multivariate tab===================================
  
  Plot_multivariate_1_df<-reactive({Airbnb_2 %>% 
      select(input$Select_variable_1_multivariate,input$Select_variable_2_multivariate,
             input$Select_breakdown_multivariate) 
  })
  
  #Removing outliers:
  Plot_multivariate_1_df_2 <-reactive({Plot_multivariate_1_df() %>% 
      filter(Plot_multivariate_1_df()[,1]<quantile(Plot_multivariate_1_df()[,1],0.95) &
               Plot_multivariate_1_df()[,2]<quantile(Plot_multivariate_1_df()[,2],0.95))
  })
  
  GG_Plot_7<-reactive({
    ggplot(Plot_multivariate_1_df_2(),aes(x=Plot_multivariate_1_df_2()[,1],
                                          y=Plot_multivariate_1_df_2()[,2], 
                                          color=Plot_multivariate_1_df_2()[,3])) +
      geom_point() +
      scale_color_manual(values=colors) +
      ggtitle(paste(names(Plot_multivariate_1_df_2())[1], "Vs.",names(Plot_multivariate_1_df_2())[2],
                    "by",names(Plot_multivariate_1_df_2())[3])) +
      xlab(names(Plot_multivariate_1_df_2())[1]) +
      ylab(names(Plot_multivariate_1_df_2())[2])+
      labs(colour=names(Plot_multivariate_1_df_2())[3]) +
      theme_minimal()
  })
  
  output$Plot_7<-renderPlotly({
    ggplotly(GG_Plot_7())
  })
  
  #For the multivariate correlation matrix
  
  Multivariate_corr<-reactive({Airbnb_2 %>% 
      select(Price_USD,Minimum_nights,Number_of_reviews,Availability_365) %>% 
      rename("Price"="Price_USD",
             "Min nights"="Minimum_nights",
             "# Reviews"="Number_of_reviews",
             "Availability"="Availability_365")
  })
  
  corr<-reactive({
    cor(Multivariate_corr())
  })
  
  output$Plot_8<-renderPlot({
    corrplot(corr(), method="number",
             col=colorRampPalette(c("#FF5A5F","#FC642D","#00A699"))(100), 
             tl.col="black", 
             tl.srt=45,
             title="Correlation for NY listing variables",
             mar = c(1,1,2,1),
             number.cex = 0.85,
             cl.align.text="l")
  })
  
  
  
  
  

  
})

shinyApp(ui, server)

