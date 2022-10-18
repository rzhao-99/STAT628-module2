library(shiny)
library(shinyFeedback)
library(ggsci)
library(ggplot2)

data<-data.frame(start=c(2,6,14,18,25),end=c(6,14,18,25,50),group=c("Essential fat","Athletes","Fitness","Average","Obese"))

ui<-fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel(strong("Body Fat Calculator for Adult Men")),
  sidebarLayout(
    sidebarPanel(
      numericInput("age", label = ("Input Your Age"), value = NA,min = 18,max=100),
      helpText("age between 18-100"),
      numericInput("neck", label = ("Input Your Neck Circumference(cm)"), value = NA,min = 30,max=55),
      helpText("neck between 30-55"),
      numericInput("abdomen", label = ("Input Your Abdomen Circumference(cm)"), value = NA,min=60,max=150),
      helpText("abdomen between 60-150"),
      numericInput("forearm", label = ("Input Your Forearm Circumference(cm)"), value = NA,min = 20,max=40),
      helpText("forearm between 20-40"),
      numericInput("wrist", label = ("Input Your Wrist Circumference(cm)"), value = NA,min=10,max=25),
      helpText("wrist between 10-25")
    ),
    mainPanel(
      textOutput("value"),
      plotOutput("arrow")
    )
  ),
  hr(style = "border-color: #cbcbcb;"),
  fluidRow(
    column(9,
           p("App created by Zixuan Zhao, Ran Zhao and Hongyan Xiao in October 2022.",style = "font-size: 85%"),
           p("Have a question? Spot an error? Please send an email to any of them:",style = "font-size: 85%"), 
           p(HTML("&bull;"),"zzhao442@wisc.edu",HTML('&emsp;'),HTML("&bull;"),"rzhao99@wisc.edu",
             HTML('&emsp;'),HTML("&bull;"),"hxiao66@wisc.edu",style = "font-size: 85%"),
           p("Find the code on Github:", 
             tags$a(href = "https://github.com/rzhao-99/STAT628-module2", 
                    tags$i(class = 'fa fa-github', style = 'color:#5000a5'), 
                    target = '_blank'), style = "font-size: 85%"),
    )
  )
)


server <- function(input, output) {
  error<-reactive({
    age_range=(input$age<=100 & input$age>18)#|(input$abdomen>=60 & input$abdomen<=180)|(input$wrist<=30 & input$wrist>=10)
    neck_range=(input$neck<=55 & input$neck>=30)
    abdomen_range=(input$abdomen>=60 & input$abdomen<=150)
    forearm_range=(input$forearm<=40 & input$forearm>=20)
    wrist_range=(input$wrist<=25 & input$wrist>=10)
    shinyFeedback::feedbackWarning(
      "age", 
      !age_range,
      "Please input age between 18~100"
    )  
    req(age_range)
    
    feedbackWarning(
      "neck", 
      !neck_range,
      "Please input neck between 30~55"
    ) 
    req(neck_range)
    
    feedbackWarning(
      "abdomen", 
      !abdomen_range,
      "Please input abdomen between 60~150"
    ) 
    req(abdomen_range)
    
    feedbackWarning(
      "forearm", 
      !forearm_range,
      "Please input forearm between 20~40"
    ) 
    req(forearm_range)
    
    feedbackWarning(
      "wrist", 
      !wrist_range,
      "Please input wrist between 10~25"
    ) 
    req(wrist_range)
    round(-8.02+0.08*input$age-0.39*input$neck+0.73*input$abdomen+0.27*input$forearm-2.04*input$wrist,2)
  })
  output$value <- renderText({
    ifelse((error()>2&error()<50),paste("Your body fat percentage is:",error(),"%"),
           paste("Your body fat percentage is:",error(),"%. This is not correct. You should check if your data are correct"))
  })
  output$arrow<-renderPlot({
    ggplot(data) + 
      geom_rect(aes(xmin=start, xmax=end,ymin=-0.3,ymax=0.3,
                    fill=group),colour="white")+
      ylim(c(-1,1))+
      theme(legend.position="bottom",
            legend.key.size=unit(0.5,'cm'), 
            legend.key.height=unit(0.5,'cm'), 
            legend.key.width=unit(0.5,'cm'), 
            legend.title=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank())+
      scale_fill_manual(breaks=c("Essential fat", "Athletes", "Fitness", "Average","Obese"),values = c("#A3E4D7", "#44c489", "#28a9ae","#4c7788","#34495E"))+
      labs(x=NULL,y=NULL)+
      guides(fill = guide_legend(ncol=5))+
      annotate("text",x = 25, y =0.4, label = "25",
               colour = "black",size = 4)+
      annotate("text",x = 18, y =0.4, label = "18",
               colour = "black",size = 4)+
      annotate("text",x = 14, y =0.4, label = "14",
               colour = "black",size = 4)+
      annotate("text",x = 6, y =0.4, label = "6",
               colour = "black",size = 4)+
      annotate("text",x = 2, y =0.4, label = "2",
               colour = "black",size = 4)+
      annotate("text",x = 48, y =0.4, label = "body fat (%)",
               colour = "black",size = 4)+
      geom_segment(aes(x=error(), y=-1.0, xend=error(), yend=-0.3), arrow = arrow(length=unit(0.3, "cm")),color="black",lineend = "round",linejoin = "round",size=2)
    
  })
}

shinyApp(ui, server,options = list(height = 1080))