library(shiny)
library(tidyverse)
library(scales)
library(plotly)
load(url("https://github.com/livezz/Life-Expectancy-Data-Report/blob/main/Life%20Data.RData?raw=true"))

df <- life[,c(1,2,3,4,17,18)]

#### Define UI for application that plots features of fake data ----------- ####
ui <- fluidPage(
  
  # Application title
  titlePanel("Life Expectancy by Country"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      
      # Select variable for y-axis
     
      selectInput(inputId = "Cty", 
                  label = "Country:",
                  choices = c("Afghanistan", "Albania", "Algeria", "Angola", 
                              "Antigua and Barbuda", "Argentina", "Armenia", 
                              "Australia", "Austria", "Azerbaijan", "Bahamas",	
                              "Bahrain", "Bangladesh", "Barbados", "Belarus", 
                              "Belgium", "Belize", "Benin", "Bhutan",	
                              "Bolivia (Plurinational State of)", 
                              "Bosnia and Herzegovina", "Botswana", "Brazil", 
                              "Brunei Darussalam", "Bulgaria", "Burkina Faso", 
                              "Burundi", "C??te d'Ivoire", "Cabo Verde",	
                              "Cambodia", "Cameroon", "Canada", 
                              "Central African Republic", "Chad", "Chile",	
                              "China", "Colombia", "Comoros", "Congo", 
                              "Cook Islands", "Costa Rica", "Croatia", "Cuba", 
                              "Cyprus", "Czechia", "Democratic People's Republic of Korea", 
                              "Democratic Republic of the Congo",	"Denmark", 
                              "Djibouti",	"Dominica", "Dominican Republic", "Ecuador", 
                              "Egypt", "El Salvador",	"Equatorial Guinea", "Eritrea", 
                              "Estonia", "Ethiopia", "Fiji", "Finland", "France", 
                              "Gabon", "Gambia", "Georgia", "Germany", "Ghana", 
                              "Greece", "Grenada", "Guatemala", "Guinea",	
                              "Guinea-Bissau", "Guyana", "Haiti", "Honduras",	
                              "Hungary", "Iceland", "India",	"Indonesia", 
                              "Iran (Islamic Republic of)", "Iraq", "Ireland", 
                              "Israel", "Italy", "Jamaica", "Japan", "Jordan", 
                              "Kazakhstan", "Kenya", "Kiribati", "Kuwait", 
                              "Kyrgyzstan", "Lao People's Democratic Republic",
                              "Latvia",	"Lebanon", "Lesotho", "Liberia", "Libya", 
                              "Lithuania", "Luxembourg", "Madagascar", "Malawi", 
                              "Malaysia", "Maldives", "Mali", "Malta", 
                              "Marshall Islands", "Mauritania", "Mauritius", 
                              "Mexico", "Micronesia (Federated States of)", "Monaco", 
                              "Mongolia", "Montenegro", "Morocco",	"Mozambique", 
                              "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", 
                              "New Zealand", "Nicaragua", "Niger", "Nigeria", 
                              "Niue", "Norway", "Oman", "Pakistan", "Palau", 
                              "Panama",	"Papua New Guinea", "Paraguay", "Peru", 
                              "Philippines", "Poland", "Portugal", "Qatar", 
                              "Republic of Korea", "Republic of Moldova", "Romania", 
                              "Russian Federation", "Rwanda",	"Saint Kitts and Nevis", 
                              "Saint Lucia", "Saint Vincent and the Grenadines", 
                              "Samoa", "San Marino", "Sao Tome and Principe",	
                              "Saudi Arabia",	"Senegal", "Serbia", "Seychelles", 
                              "Sierra Leone", "Singapore", "Slovakia", "Slovenia", 
                              "Solomon Islands", "Somalia", "South Africa", 
                              "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", 
                              "Swaziland", "Sweden",	"Switzerland", "Syrian Arab Republic", 
                              "Tajikistan", "Thailand", "The former Yugoslav republic of Macedonia", 
                              "Timor-Leste",	"Togo",	"Tonga", "Trinidad and Tobago", 
                              "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda", 
                              "Ukraine", "United Arab Emirates", 
                              "United Kingdom of Great Britain and Northern Ireland", 
                              "United Republic of Tanzania",	"United States of America", 
                              "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela (Bolivarian Republic of)", 
                              "Viet Nam", "Yemen", "Zambia", "Zimbabwe"), 
                  selected = "Afghanistan"),
      
      # Select Colors
      selectInput(inputId = "color_p", 
                  label = "Choose Point Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      
      selectInput(inputId = "color_l", 
                  label = "Choose Line Color:",
                  choices = c("Red", "Blue", "Black", "Green"), 
                  selected = "Black"),
      
      # Set alpha level
      sliderInput(inputId = "alpha", 
                  label = "Point Transparency:", 
                  min = 0, max = 1, 
                  value = 0.5),
      
      hr(), #Horizontal Line for visual separation
      
      # Set min/max of Cohort Values
      selectInput(inputId = "min", 
                  label = "Choose Cohort Range (Min):", 
                  choices = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
                              2008, 2009, 2010, 2011, 2012, 2013, 2014),
                  selected= 2000),
      
      selectInput(inputId = "max",
                  label = "Choose Cohort Range (Max):", 
                  choices = c(2001, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 
                              2008, 2009, 2010, 2011, 2012, 2013, 2014,2015),
                  selected= 2015),
      
    ),
    
    # Output: Show scatterplot --------------------------------------
    mainPanel(
      tabsetPanel(
        tabPanel("Country Life Expectancy Change", plotlyOutput(outputId = "scatterplot_2")),
        tabPanel("Country Life Expectancy in Age", plotlyOutput(outputId = "scatterplot_3")),
        
        tabPanel("Data",  DT::dataTableOutput(outputId="datasheet"))
      )
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  
  dat2 <- reactive({
    d2 <- df[df$Country %in% input$Cty, ]
    return(d2)
  })
  output$scatterplot_2<-renderPlotly({
    ggplot(data = dat2(), aes(x = dat2()$Year , y = dat2()$Life.expectancy, text=dat2()$Year)) + geom_point(colour=input$color_p, alpha=input$alpha) +
      theme_classic() + xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + ylab("Year of Life Expectancy")+ geom_line(colour=input$color_l) + labs(title="Country Life Expectancy Change by Year")
  })
  
  dat1 <- reactive({
    ds1 <- df[df$Country %in% input$Cty, ]
    return(ds1)
  })
  
  
  output$scatterplot_3 <- renderPlotly({
    ggplot(data = df, aes_string(x = df$Year, y = df$Life.expectancy)) +
      geom_point(colour=input$color_p, alpha=input$alpha) + theme_classic() +
      xlim(as.numeric(input$min), as.numeric(input$max)) +
      xlab("Year") + labs(title=input$plot_title)
  }) 
  
  output$datasheet<-DT::renderDataTable({
    DT::datatable(data=df[,1:6],
                  options=list(pageLength= 20),
                  rownames=FALSE)
  })
  
}

# Create the Shiny app object ---------------------------------------
shinyApp(ui = ui, server = server)

