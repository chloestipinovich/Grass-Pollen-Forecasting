# Final Shiny app
# Libraries
{
  library(shiny)
  library(tidyverse)
  library(reshape2)
  library(plotly)
  library(reactable)
}


# Predictions
# Load predictions from GitHub
predictions = read.csv("https://raw.githubusercontent.com/skycope/grass-pollen/master/Shiny/predictions.csv")  %>%
  mutate(date = as.Date(date)) %>% rename(Date = date)

# format dates and days correctly
predictions$Date = format(predictions$Date, "%d %b")
predictions$day = factor(predictions$day, levels = as.character(predictions$day))

# get the right colours
colours = c('#40b101', '#b1eb36', '#ffe100', '#ffa200', '#ff4a36')


# User Interface -------
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           reactableOutput("table", height = 120),
           div(style = 'padding:20px;')
    )
  ),
  fluidRow(
    column(width = 12,
           plotlyOutput("plot2", height = 400)
    )
  )
)




# Server ---------
risk_df = predictions  %>%
  mutate(lowrisk = Low + Very_Low,
         highrisk = High + Very_High,
         medrisk = Moderate) %>%
  mutate(`Pollen Risk` = case_when(
    lowrisk >= medrisk & lowrisk >= highrisk ~ "Low",
    highrisk >= medrisk & highrisk >= lowrisk ~ 'High',
    medrisk >= highrisk & medrisk >= lowrisk ~ 'Moderate'
  ) ) %>% dplyr::select(-lowrisk, -highrisk, -medrisk, -day) %>% t() %>% data.frame()

names(risk_df) = predictions$day
risk_df = risk_df[-1:-5,]

server <- function(input, output){
  
  output$table <- renderReactable(
    reactable(
      risk_df,
      columns = list(
        Monday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Tuesday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Wednesday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Thursday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Friday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Saturday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        ),
        Sunday = colDef(
          style = function(risk_cat) {
            colour =  case_when(
              risk_cat == 'Low' ~ '#40b101',
              risk_cat == 'Moderate' ~ '#ffa800',
              risk_cat == 'High' ~ '#DC4130'
            )
            list(fontWeight = 600, color = colour)
          }
        )
      )
    ))
  
  output$plot2 = renderPlotly({
    q  = predictions %>% rename(`Very Low` = Very_Low, `Very High` = Very_High) %>% 
      dplyr::select(-Date) %>%
      melt(id.vars = 'day') %>%
      group_by(day, variable) %>%
      rename(Category = variable) %>%
      ggplot(aes(x = day, y = value, fill = Category, group = Category)) +
      geom_area(position = 'stack', alpha = 0.8) +
      scale_fill_manual(values = colours) +
      ylab("Cumulative Probability") +
      xlab("Day") +
      theme_minimal()
    ggplotly(q) 
  })
}

# Run App ----------
shinyApp(ui, server)

