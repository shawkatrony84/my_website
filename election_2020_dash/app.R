#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)

train_df <- read.csv("train_df.csv")

#==================== UI ====================#
ui <- fluidPage(
  
  titlePanel("2020 Voting Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "vote_status",
        label = "Select Voting Status:",
        choices = c("Voted (Yes)" = 1, "Did Not Vote (No)" = 2),
        selected = 1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Age Distribution",
          h3("Age Distribution"),
          p("This plot shows the distribution of respondent ages based on their 2020 voting status. Older respondents tended to vote more frequently."),
          plotOutput("agePlot")
        ),
        tabPanel(
          "Prayer Frequency",
          h3("Prayer Frequency"),
          p("This bar chart shows the percentage of respondents reporting different prayer frequencies. The data is filtered by voting status."),
          plotOutput("prayPlot")
        )
      )
    )
  )
)

#==================== SERVER ====================#
server <- function(input, output) {
  
  #---- AGE PLOT (line + fill) ----
  output$agePlot <- renderPlot({
    
    df_filtered <- train_df %>%
      filter(VOTED2020 == as.numeric(input$vote_status))
    
    age_counts <- df_filtered %>%
      count(AGE) %>%
      arrange(AGE)
    
    ggplot(age_counts, aes(x = AGE, y = n)) +
      geom_line(linewidth = 1, color = "green") +
      geom_area(alpha = 0.3, fill = "#009E73") +
      labs(
        x = "Age",
        y = "Count",
        title = ifelse(input$vote_status == 1,
                       "Age Distribution - Voted in 2020 (Yes)",
                       "Age Distribution - Did Not Vote in 2020 (No)")
      ) +
      theme_minimal(base_size = 13)
  })
  
  #---- PRAYER FREQUENCY PLOT (% bar chart) ----
  output$prayPlot <- renderPlot({
    
    df_filtered <- train_df %>%
      filter(VOTED2020 == as.numeric(input$vote_status))
    
    df_filtered$PRAY <- factor(
      df_filtered$PRAY,
      levels = 0:6,
      labels = c(
        "Several times/day",
        "Once/day",
        "A few times/week",
        "Once/week",
        "A few times/month",
        "Seldom",
        "Never"
      )
    )
    
    pray_pct <- df_filtered %>%
      count(PRAY) %>%
      mutate(pct = n / sum(n) * 100)
    
    ggplot(pray_pct, aes(x = PRAY, y = pct)) +
      geom_col(fill = "#40826D") +
      labs(
        title = ifelse(input$vote_status == 1,
                       "Prayer Frequency among 2020 Voters",
                       "Prayer Frequency among 2020 Non-Voters"),
        x = "Prayer Frequency",
        y = "Percent of respondents",
        caption = "Percentage distribution by 2020 voting status"
      ) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

#==================== RUN APP ====================#
shinyApp(ui = ui, server = server)
