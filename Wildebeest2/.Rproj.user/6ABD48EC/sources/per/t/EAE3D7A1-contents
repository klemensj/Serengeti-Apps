# LIBRARIES

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

# DATA 
# Read in data 
Serengeti <-read.csv("Full_Serengeti_Data.csv")

# Create data set for unique cameras
camData_distinct <- Serengeti %>% distinct(Camera.Site, .keep_all = TRUE)     
camData <- select(camData_distinct, Camera.Site:Camera.Mount)

# Create season variable
Serengeti$Season <- "Inter" 
Serengeti$Season[Serengeti$Month..1.Jan.. %in% c(6,7,8,9,10)]  <- "Dry"
Serengeti$Season[Serengeti$Month..1.Jan.. %in% c(11,12,1,2,3,4) ] <- "Wet"

# Create wet and dry season datasets
dryWilde <- filter(Serengeti, Species == "wildebeest" & Season == "Dry")
wetWilde <- filter(Serengeti, Species == "wildebeest" & Season == "Wet")

# Generate lists for continuous variables


caption = ""   


# UI ###################################
# UI ###################################
# UI ###################################

ui <- fluidPage(
    
    # TITLE
    
    titlePanel("Wildebeest distribution by season"),
    
    # ROW1
    fluidRow(
        
        # SIDEBAR ROW1
        column(4,
        
            p("Circle size represents the relative number of observations made by that camera during each season."),
            p("Orange = Dry season observations", style="color:orange; font-weight: bold"),
            p("Blue = Wet season observations", style="color:blue; font-weight: bold")
            
                      
        ), # end sidebar
        
        # MAIN PANEL ROW1
        column (8,
         
                plotOutput("wildeGraph"),
                p(""),
                p(caption),
                p(""),
                downloadButton(outputId = "downWildeGraph", label = "Download the plot"),
                p("")
        )#end main panel
    )#endrow
)#end ui



# SERVER ###################################
# SERVER ###################################
# SERVER ###################################

server <-
    function(input, output) {
        
        
#Row 1 output
output$wildeGraph <- renderPlot({
            
#make base plot to fix axis length and etc, points for cam locations
plot(camData$Longitude..m., camData$Latitude..m., 
    cex = 0.5, pch = 16,          
    xlab = "Longitude (m)",
    ylab = "Latitude (m)", 
    asp = 1)  # sets aspect ratio for plot

#plot dry season points


    mycol <- rgb(255, 128, 0, max = 255, alpha = 125)

    count <- table(dryWilde$Camera.Site)
    count_df <- as.data.frame(count)  
    Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
    points(Data$Longitude..m.,Data$Latitude..m., 
           cex = (sqrt((Data$Freq/sum(Data$Freq)*500))), pch = 21, bg = mycol)    
    
#plot wet season points    
  
    mycol <- rgb(0, 0, 255, max = 255, alpha = 125)
    
    count <- table(wetWilde$Camera.Site)
    count_df <- as.data.frame(count)  
    Data <- left_join(count_df,camData, by = c("Var1"="Camera.Site"))
    points(Data$Longitude..m.,Data$Latitude..m., 
           cex = (sqrt((Data$Freq/sum(Data$Freq)*500))), pch = 21, bg = mycol, alpha = 125)      
                  
        })

            
# DOWNLOADS
        
# downloadHandler contains 2 arguments as functions, namely filename, content
# code from: https://gist.github.com/aagarw30/6c10d6d92f5d512cae41
        
output$downWildeGraph <- downloadHandler(
    filename =  function() {
                st=format(Sys.time(), "%Y-%m-%d_%H:%M")
                paste(st,"Boxplot", ".pdf", sep="")
            },
# content is a function with argument file. content writes the plot to the device
            content = function(file) {
                
                pdf(file) # open the pdf device

                #PLOT

                dev.off()  # turn the device off
            } 
        ) #end download handler
        
        
        

} # end server -> function()


# Run the application 
shinyApp(ui = ui, server = server)
