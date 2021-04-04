# Top Chef Fantasy League 2021

library(tidyverse)
#library(googlesheets4)
library(shiny)
library(DT)
library(ggimage)

# get season data with google sheets
# if I can figure out how to keep the auth token
#tc <- read_sheet("https://docs.google.com/spreadsheets/d/15lx_X6BIvkuo1pPWBMRTTdmzWUy5YfIxU8ucAVcd3ow/edit#gid=0") %>%
#  janitor::clean_names()

# get season data from github
tc <- read_csv("https://raw.githubusercontent.com/danmorse314/top-chef-fantasy/main/data/2021_stats.csv",
               col_types = cols()) %>%
  janitor::clean_names() %>%
  mutate(status = ifelse(is.na(status), "", status))

tc[is.na(tc)] <- 0

team_colors <- c(Zach = "#7d826e", Dan = "#d3b140", Chris = "#cdced1")

`%not_in%` <- purrr::negate(`%in%`)

standings <- tc %>%
  mutate(team_url = paste("<img src = ' ",team_url,"' height = '45'></img>")) %>%
  group_by(team_url, team) %>%
  summarize(
    chefs_remaining = 5 - sum(eliminated),
    lck_chefs = sum(status == "LCK"),
    points = sum(weekly_points),
    elim_wins = sum(elim_win),
    elim_top3s = sum(elim_top3),
    qf_wins = sum(qf_win),
    qf_top3s = sum(qf_top3),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(-points)

current_week <- max(tc$week)

elim_chefs <- tc %>%
  filter(eliminated == 1) %>%
  mutate(last_week = week) %>%
  select(cheftestant, last_week)

final_week <- tc %>%
  filter(cheftestant %not_in% elim_chefs$cheftestant) %>%
  group_by(cheftestant) %>%
  summarize(last_week = max(week), .groups = "drop") %>%
  ungroup() %>%
  bind_rows(elim_chefs)

chefs <- tc %>%
  left_join(final_week, by = c("cheftestant")) %>%
  filter(week == last_week) %>%
  select(cheftestant, status, last_week) %>%
  distinct()

chefstats <- tc %>%
  mutate(chef_url = paste("<img src = ' ",chef_url,"' height = '45'></img>")) %>%
  mutate(team_url = paste("<img src = ' ",team_url,"' height = '45'></img>")) %>%
  group_by(chef_url, cheftestant, team_url, team) %>%
  summarize(
    points = sum(weekly_points),
    elim_wins = sum(elim_win),
    elim_top3s = sum(elim_top3),
    qf_wins = sum(qf_win),
    qf_top3s = sum(qf_top3),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(-points) %>%
  left_join(chefs, by = "cheftestant") %>%
  arrange(status) %>%
  select(chef_url, cheftestant, status, everything())

my_colors <- chefs %>%
  mutate(
    color = case_when(
      status == "LCK" ~ "orange",
      status == "ELIMINATED" ~ "red"
    )
  )

team_weekly <- tc %>%
  group_by(team, team_url, week) %>%
  summarize(
    weekly_points = sum(weekly_points),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  group_by(team_url) %>%
  mutate(cumpoints = cumsum(weekly_points)) %>%
  ungroup() %>%
  mutate(
    week = case_when(
      team == "Zach" ~ week - .2,
      team == "Dan" ~ week,
      team == "Chris" ~ week + .2
    )
  )

chefbar <- tc %>%
  group_by(chef_url, cheftestant) %>%
  summarize(
    points = sum(weekly_points),
    .groups = "drop"
  ) %>%
  ungroup() %>%
  arrange(-points) %>%
  left_join(
    distinct(select(tc, cheftestant, team)),
    by = "cheftestant"
  ) %>%
  separate(cheftestant, into = c("first_name","last_name"), sep = " ", remove = FALSE)

# example of plot by week
#tc %>%
#  group_by(cheftestant) %>%
#  mutate(points = cumsum(weekly_points),
#            .groups = "drop") %>%
#  ungroup() %>%
#  ggplot(aes(week, points)) +
#  geom_line(aes(color = cheftestant)) +
#  theme_bw()

ui <- fluidPage(
  titlePanel(
    div(
      column(
        width = 6,
        tags$img(src = "https://www.bravotv.com/sites/bravo/files/styles/scale--1200/public/field_logo/2016/02/top-chef-hero-logo_0.png",
                 height = '200px')
      ),
      column(
        width = 6,
        div(
          br(), br(),
          h3("Slack Fantasy League"),
          h4("with Zach, Dan, & Chris")
        )
      )
    ),
    windowTitle = "Top Chef Fantasy"
  ),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      selected = "main",
      tabPanel(
        "Home", value = "main",
        uiOutput(outputId = "standingshead"),
        DT::dataTableOutput(outputId = "standings"),
        uiOutput(outputId = "teamtitle"),
        DT::dataTableOutput(outputId = "teamtable"),
        uiOutput(outputId = "chefstatshead"),
        DT::dataTableOutput(outputId = "chefstats"),
        br()
      ),
      tabPanel(
        "Charts", value = "charts",
        br(),
        plotOutput(outputId = "teamplot"),
        tags$hr(),
        plotOutput(outputId = "chefplot"),
        br()
      )
    )
  )
)

server <- function(input, output, session){
  
  output$standingshead <- renderUI({
    h2("2021 Standings")
  })
  
  output$standings <- DT::renderDataTable({
    
    DT::datatable(
      standings,
      colnames = c(
        " " = "team_url",
        "Team" = "team",
        "Chefs Remaining" = "chefs_remaining",
        "LCK Chefs" = "lck_chefs",
        "Fantasy Points" = "points",
        "Elimination Wins" = "elim_wins",
        "Elimination Top 3s" = "elim_top3s",
        "Quickfire Wins" = "qf_wins",
        "Quickfire Top 3s" = "qf_top3s"
      ),
      escape = FALSE,
      callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
      selection = "single",
      options = list(
        dom = "t",
        columnDefs = list(
          list(className = "dt-center", targets = seq(1,9,1)),
          list(orderable = FALSE, targets = c(1,2))
        )
      )
    )
    
  })
  
  output$teamtitle <- renderUI({
    
    s <- input$standings_rows_selected
    
    if(length(s)){
      
      team_name <- standings[s, , drop = FALSE] %>%
        pull(team)
      
      team_url <- standings %>%
        filter(team == team_name) %>%
        pull(team_url)
      
      HTML(paste("<div><br><span>",team_url,"<h4>Team",team_name,"</h4></span></div>"))
      
    } else{
      HTML("<em>Select a player to see their full team</em>")
    }
    
  })
  
  output$teamtable <- DT::renderDataTable({
    
    s <- input$standings_rows_selected
    
    if(length(s)){
      
      team_name <- standings[s, , drop = FALSE] %>%
        pull(team)
      
      chefstatsi <- chefstats %>%
        select(-team_url) %>%
        filter(team == team_name) %>%
        select(-team)
      
      DT::datatable(
        chefstatsi,
        colnames = c(
          " " = "chef_url",
          "Chef" = "cheftestant",
          " " = "status",
          "Fantasy Points" = "points",
          "Elimination Wins" = "elim_wins",
          "Elimination Top 3s" = "elim_top3s",
          "Quickfire Wins" = "qf_wins",
          "Quickfire Top 3s" = "qf_top3s",
          "Last Week" = "last_week"
        ),
        escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "none",
        options = list(
          dom = "t",
          columnDefs = list(
            list(className = "dt-center", targets = seq(1,9,1)),
            list(orderable = FALSE, targets = c(1,2,3))
          ),
          pageLength = 15
        )
      ) %>%
        formatStyle(
          "Chef", target = 'row', 
          backgroundColor = styleEqual(chefs$cheftestant,my_colors$color)
        )
      
    }
  })
  
  output$chefstatshead <- renderUI({
    div(
      hr(),
      h2("Cheftestant Season Stats")
    )
  })
  
  output$chefstats <- DT::renderDataTable({
    
    DT::datatable(
      select(chefstats, -team),
      colnames = c(
        " " = "chef_url",
        "Chef" = "cheftestant",
        "Team" = "team_url",
        " " = "status",
        "Fantasy Points" = "points",
        "Elimination Wins" = "elim_wins",
        "Elimination Top 3s" = "elim_top3s",
        "Quickfire Wins" = "qf_wins",
        "Quickfire Top 3s" = "qf_top3s",
        "Last Week" = "last_week"
      ),
      escape = FALSE,
      callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
      selection = "none",
      options = list(
        dom = "t",
        columnDefs = list(
          list(className = "dt-center", targets = seq(1,10,1)),
          list(orderable = FALSE, targets = c(1,2,3))
        ),
        pageLength = 15
      )
    ) %>%
      formatStyle(
        "Chef", target = 'row', 
        backgroundColor = styleEqual(chefs$cheftestant,my_colors$color)
      )
    
  })
  
  output$teamplot <- renderPlot({
    
    team_weekly %>%
      ggplot(aes(week, cumpoints)) +
      geom_line(aes(color = team), show.legend = FALSE, size = 2) +
      scale_color_manual(values = team_colors) +
      geom_image(aes(image = team_url), asp = 1) +
      theme_bw() +
      theme(
        title = element_text(size = 24),
        axis.title = element_text(size = 16),
        panel.grid.minor.x = element_blank()
      ) +
      scale_x_continuous(breaks = seq(0,13,1), limits = c(-.2,13)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = "Week", y = "Fantasy Points",
           title = "Weekly Standings")
    
  })
  
  output$chefplot <- renderPlot({
    
    chefbar %>%
      ggplot(aes(points, reorder(first_name, points))) +
      geom_col(aes(fill = team, color = team)) +
      #ggimage::geom_image(
      #  aes(x = -6, y = reorder(cheftestant, points),
      #      image = chef_url)
      #) +
      scale_color_manual(values = team_colors) +
      scale_fill_manual(values = team_colors) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme_bw() +
      theme(
        title = element_text(size = 24),
        axis.title.x = element_text(size = 16),
        legend.background = element_blank(),
        legend.position = c(.8,1.03),
        legend.direction = "horizontal"
      ) +
      labs(x = "Fantasy Points", y = NULL,
           title = "Cheftestant Points",
           fill = NULL, color = NULL)
    
  })
  
}

shinyApp(ui = ui, server = server)