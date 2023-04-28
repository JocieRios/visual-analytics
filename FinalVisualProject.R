# Loading Libraries
library(ggplot2)
library(rlang)
library(readxl)
library(dplyr)
library(caret)
library(reshape2)
library(kernlab)
library(Metrics)
library(shiny)
library(MASS)

# Reading in data 
data("BostonHousing")
data <- BostonHousing

# Cleaning data

  #Getting a summary of data to check for NA values
summary(data)

  #Checking if there are any NAs I missed
str(data)

  #Removing Indexed and Ordinal Values 
data <- data[, -c(4, 9)]
str(data)

  #Removing unpertinent values (To personal objective)
data <- data[, -c(4)]
str(data)

#-----------------------------------------------------------------------------------

# Correlation Analysis

## Center titles for all ggplots
theme_update(plot.title=element_text(hjust=0.5), plot.subtitle=element_text(hjust=0.5))


### Heatmap of all variables' correlation
cor <- cor(data)
melt_cor <- melt(cor)


ggplot(melt_cor, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient2(high = "#800000", mid="white", low = "#008080", name = "Pearson Correlation", limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 9)) +
  coord_fixed() + xlab("") + ylab("") + ggtitle("Correlation of Variables \n in Boston Housing Data")

### Correlation between variables and median housing prices

correlation <- cor(data)[,"medv"]
correlation <- sort(correlation, decreasing=TRUE)
correlation

# Removing variables with a pearson correlation below .30
data <- data[, -6]
str(data)

#-----------------------------------------------------------------------------------
# Shiny Web App


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel( h1("Housing Values in Suburbs of Boston", align = "center"), ),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Set Parameters for Scatterplot
      strong('Scatter Plot Parameters'), p(''),
      selectInput('scatterX', 'Independent Variable (X-Axis)', 
                  names(data), selected = 'crim'),
      selectInput('scatterCol', 'Factor (to Color Points By)', 
                  names(data), selected = 'age'),
      
      
      #Brief Variables Description
      strong("Variable Description"),
      tags$ul(
        tags$li('CRIM: per capita crime rate by town'),
        tags$li('ZN: proportion of residential land zoned for lots over 25,000 sq.ft.'),
        tags$li('INDUS: proportion of non-retail business acres per town.'),
        tags$li('RM: average number of rooms per dwelling'),
        tags$li('AGE: proportion of owner-occupied units built prior to 1940'),
        tags$li('TAX: full-value property-tax rate per $10,000'),
        tags$li('PTRATIO: pupil-teacher ratio by town'),
        tags$li('B: 1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town'),
        tags$li('LSTAT: % lower status of the population'),
        tags$li('MEDV: Median value of owner-occupied homes in $1000s')
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      p('This app explores the Boston dataset, which contains information collected by the 
        U.S Census Service concerning housing in the area of Boston Mass. The data was 
        originally published by Harrison, D. and Rubinfeld, 1978.'),
      p('This app allows the exploration of the relationship between varying variables against and median housing value via scatter plots'),
      
      plotOutput('ScatThreeVar')
    )
  )
)

# Define server logic required to create scatter plot
server <- function(input, output) {
  
  #SCATTER PLOT
  
  # Extract the columns of intersect from the Boston dataset and save the data
  scatX <- reactive({
    Boston[, input$scatterX]
  })
  
  scatY <- reactive({
    Boston[, "medv"]
  })
  
  scatCol <- reactive({
    Boston[, input$scatterCol]
  })
  
  # Make the Scatter Plot
  
  output$ScatThreeVar <- renderPlot({
    ggplot(data = Boston, aes(x = scatX(), y = scatY(), color = scatCol())) +
      geom_point() + 
      scale_color_gradient(high="#E0807B", low = "#660000") +
      stat_smooth(method = "lm", color="#800000")+
      xlab(input$scatterX) + ylab("medv") +
      labs(colour = input$scatterCol) +
      ggtitle(paste('Scatter Plot of', input$scatterX, 'vs medv')) +
      theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"), axis.title=element_text(size=15), axis.text=element_text(size=10))
  })
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

#------------------------------------------------------------------------------------
## Conducting Multivariate Regression Analysis

### These variables describing crime rate, proportion of lower status in the population, and
### pupil-teacher ratio seemed particularly interesting- and are being analyzed for their correlation to MEDV


### Investigating the relationship between CRIM and MEDV using linear regression analysis.
model1 <- lm(formula = medv ~ crim, data = data)
summary(model1)
str(model1)

test1 <- data.frame(CRIM = 0.02729)
predict(model1, test1, type = "response")

#Investigating the relationship between CRIM and LSTAT, and MEDV. 
model2 <- lm(formula = MEDV ~ CRIM + LSTAT, data = data)
summary(model2)

test2 <- data.frame(CRIM = 0.02729, LSTAT = 4.03)
predict(model2, test2, type = "response")

#Investigating the relationship between CRIM, LSTAT, PTRATIO, and MEDV.
model3 <- lm(formula = MEDV ~ CRIM + LSTAT + PTRATIO, data = data)
summary(model3)

test3 <- data.frame(CRIM = 0.02729, LSTAT = 4.03, PTRATIO = 17.8)
predict(model3, test3, type = "response")

#-----------------------------------------------------------------------------------
#Plotting Factors by R Squared values

  # Creating a df with r squared values
r_squared <-vector("numeric", length=9) 
for (i in 1:9) {
  colname <- colnames(data)[i]
  x<- data[,colname]
  y<- data$medv
  m <- lm(medv ~ x, data=data)
  s <- summary(m)            # get the summary of the model
  # extract every thing you need from the summary object
  r_squared[i] <- c(r.squared = s$r.squared) 
} 

 # Creating df with factor names and respective r-squared values
colnames(data[1:9])
factor <- (colnames(data[1:9]))

r2_df <- data.frame(factor,r_squared)
r2_df <- r2_df[order(r2_df$r_squared, decreasing=TRUE),]

rownames(r2_df) <- NULL
r2_df


  # Creating barplot with r-squared values
ggplot(data= r2_df, aes(x=reorder(factor, -r_squared), y= r_squared)) +
  geom_col(fill="#008080") + labs(title="R-Squared Values by Factor", subtitle="Linear Regression in Relation to medv") +
  xlab("Factor") +  ylab("R-squared") + ylim(0, .6) +
  theme(
    panel.grid.major = element_blank(),
    legend.position="none"
    
    
  )

