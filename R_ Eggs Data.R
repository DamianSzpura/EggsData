library(ggplot2)
library(shiny)
library(reshape2)
library('BCA')
library(wesanderson)
data("Eggs")

first_plot <- ggplot(Eggs, aes(x = Eggs$Week, y = Eggs$Cases, size = Eggs$Cases, fill = Eggs$Cases)) +
              geom_point(shape=21, color="lightskyblue4")+
              geom_text(aes(label=ifelse(Eggs$Cases>120000,as.character(Eggs$Month),'')),hjust=-0.5,vjust=0, size = 5)+
              geom_text(aes(label=ifelse(Eggs$Cases>120000,as.character(Eggs$Week),'')),hjust=-0.5,vjust=1.2, size = 5)+
              geom_smooth(colour = "black", span = 0.3)+
              scale_radius(
                range = c(3,10),
                guide = FALSE
              )+
              scale_fill_gradient(
                name = "Cases",
                low = "lightskyblue4", high = "lightskyblue"
              )+
              labs(x="The observation week (1 to 105)", y="Retail sales of eggs in cases") +
              theme(
                axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                plot.margin = unit(c(2,2,2,1), "cm"))

second_plot <-  ggplot(Eggs, aes(x = Eggs$Month, y = Eggs$Cases, size = Eggs$Cases, fill = Eggs$Cases, shape = Eggs$First.Week)) +
                geom_point( color="lightskyblue4")+
                geom_text(aes(label=ifelse(Eggs$Cases>110000,as.character(Eggs$Easter),'')),hjust=-0.5,vjust=0, size = 5)+
                scale_radius(
                  range = c(3,10),
                  guide = FALSE
                )+
                scale_shape_manual(
                  values = c(21,22),
                  name = "Is it First week?"
                )+
                scale_fill_gradient(
                  name = "Cases",
                  low = "lightskyblue4", high = "lightskyblue"
                )+
                labs(x="Month", y="Retail sales of eggs in cases") +
                theme(
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,1.1,2,1), "cm"))

Eggs.temp <- Eggs[order(-Eggs$Week),]
Eggs.temp[5] <- Eggs.temp$Week
Eggs.long <- melt(Eggs.temp[5:10], id.vars="Cases")


Eggs.long$Cases <- factor(Eggs.long$Cases, levels = rev(unique(Eggs.long$Cases)), ordered=TRUE)

third_plot <-  ggplot(Eggs.long, aes(Cases, value, group=variable, color=variable))+
                geom_line() +
                scale_color_manual(
                  name = "Products",
                  values = wes_palette("FantasticFox1"),
                  aesthetics = c("colour", "fill")
                ) + 
                labs(x="The observation week (1 to 105)", y="Average retail price") +
                theme(
                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = rel(1.1)),
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,1.45,2,1), "cm"))

fourth_plot <-  ggplot(Eggs, aes(x = Eggs$Egg.Pr , y = Eggs$Cases, size = Eggs$Beef.Pr, color = Eggs$Cases)) +
                geom_point()+
                scale_radius(
                  name = "Beef price",
                  range = c(1,10)
                )+
                geom_text(aes(label=ifelse(Eggs$Cases>120000,as.character(Eggs$Month),'')),hjust=-0.5,vjust=0, size = 5)+
                geom_text(aes(label=ifelse(Eggs$Cases>120000,as.character(Eggs$Week),'')),hjust=-0.5,vjust=1.2, size = 5)+
                geom_smooth(method = 'lm', colour = "black", span = 0.3)+
                scale_color_gradient(
                  name = "Cases",
                  low = "lightskyblue4", high = "lightskyblue"
                )+
                labs(x="Average retail egg price in cents per dozen", y="Retail sales of eggs in cases") +
                theme(
                  axis.title.y = element_text(margin = margin(t = 0, r = 14, b = 0, l = 0)),
                  plot.margin = unit(c(2,2,2,1), "cm"))

ui <- bootstrapPage(
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css?family=Allerta+Stencil');
  
        h1, h3 {
          font-family: 'Allerta Stencil', sans-serif;
          font-weight: 500;
          line-height: 1.1;
          color: #000;
          text-align: center;
        }

        p{
          padding: 0rem 8rem;
          text-align: center;
        }

        div {
          position:relative;
        }
      "))
    ),
    
    h1('Diagrams for Eggs'),
    hr(),
    div(
      h3('Retail sales of eggs in cases for every week'),
      plotOutput('oneplot', height = "800px"),
      p('From diagram above we can cleary see that, eggs are sold in much higher ammounts in April, and before it, in March. Its becasue of Easter that happens in April in almost every country in the world, when eggs are neccesary on every table.')
    ),
    hr(),
    div(
      h3('Retail sales of eggs in cases for every month'),
      plotOutput('twoplot', height = "800px"),
      p('This diagram prove my previous statement about Easter. We can see that in April the values are the highest and also the lowest. The value is the highest on easter, and the lowest after easter. We can see it on first diagram. ')
    ),
    hr(),
    div(
      h3('Average retail price for 5 products for every week'),
      plotOutput('threeplot', height = "800px"),
      p('Here I wanted to check if any other products affect eggs prices, but also to look for any really big change in those prices. As we can see, other products does not really affect egg prices in a clear to read way. Only beef prices are changing inversely to eggs prices, which is quite strange. Eggs prices are changing a lot during weeks. We can see many drastic jumps up and down. The two the biggest drops are exactly in Easter, where prices gets really low.')
    ),
    hr(),
    div(
      h3('Relationship between retail sales of eggs in cases and average retail egg price in cents per dozen'),
      plotOutput('fourplot', height = "800px"),
      p('On this diagram we can see that eggs prices are affecting their sales. Black line shows as that there is a little more 
average sales when price is low, then when its bigger by almost 40%. I also added beef prices here to see if it somehow affects egg sales, but we cant really tell if its true.')
    )
    )

server <- function(input, output) {
  output$oneplot <- renderPlot({
    first_plot
  })
  output$twoplot <- renderPlot({
    second_plot
  })
  output$threeplot <- renderPlot({
    third_plot
  })
  output$fourplot <- renderPlot({
    fourth_plot
  })
}

shinyApp(ui = ui, server = server)


