library(shiny)
library(tidyr)
library(ggplot2)
library(ggdark)
library(dplyr)

# Load helpers and data ---------------------------------------------------------------
load('data/mortality_and_year_of_birth_count_dfs.Rdata')
load('data/first_name_choices.Rdata')
# first_name_choices <- sample(first_name_choices)
# Plot colors
neutral_colors <- c(
    "#ffec8b", "#00e5ee"
)
neutral_colors_sampled <- 
    sample(neutral_colors, 2, replace = FALSE) %>% 
    as.list()
names(neutral_colors_sampled) <- c('m', 'f')

get_estimated_distribution <- function(first_name,
                                       gender = NA_character_,
                                       current_year = 2021,
                                       minimum_age = 0,
                                       maximum_age = 100)
{
    first_name_lower = tolower(first_name)
    maximum_year = current_year - minimum_age
    minimum_year = current_year - maximum_age
    
    if(!is.na(gender)){
        cur_df <- year_of_birth_df %>%
            filter(first_name == first_name_lower,
                   sex == gender,
                   year_of_birth <= maximum_year,
                   year_of_birth >= minimum_year)
        
        year_stats <- mortality_df %>%
            filter(as_of_year == current_year)
        
        cur_df$prob_alive <- 
            approx(x = year_stats$year_of_birth,
                   y = year_stats[[paste0(gender, '_prob_alive')]],
                   xout = cur_df$year_of_birth)$y
        
        cur_df$estimated_count_alive <- cur_df$prob_alive * cur_df$count
        
        cur_df <- cur_df %>%
            select(-sex, -prob_alive) %>%
            mutate(
                estimated_count_alive_total = sum(estimated_count_alive, na.rm = T),
                estimated_prob_alive = estimated_count_alive / estimated_count_alive_total
            ) %>%
            arrange(year_of_birth) %>%
            mutate(estimated_prob_alive_cumulative = cumsum(estimated_prob_alive)) %>%
            mutate(
                abs_25 = abs(estimated_prob_alive_cumulative - 0.25),
                abs_50 = abs(estimated_prob_alive_cumulative - 0.5),
                abs_75 = abs(estimated_prob_alive_cumulative - 0.75)
            )
        
        return(cur_df)
    }
    else {
        cur_df_m <-
            get_estimated_distribution(first_name, 'm', current_year, minimum_age, maximum_age) %>%
            rename_at(
                .vars = vars(
                    count,
                    estimated_count_alive,
                    estimated_count_alive_total,
                    estimated_prob_alive,
                    estimated_prob_alive_cumulative,
                    abs_25,
                    abs_50,
                    abs_75
                ),
                ~ paste0('m_', .)
            )
        cur_df_f <-
            get_estimated_distribution(first_name, 'f', current_year, minimum_age, maximum_age) %>%
            rename_at(
                .vars = vars(
                    count,
                    estimated_count_alive,
                    estimated_count_alive_total,
                    estimated_prob_alive,
                    estimated_prob_alive_cumulative,
                    abs_25,
                    abs_50,
                    abs_75
                ),
                ~ paste0('f_', .)
            )
        cur_df <-
            full_join(cur_df_m, cur_df_f, by = c('first_name', 'year_of_birth')) %>%
            select(first_name, year_of_birth, everything())
        return(cur_df)
    }
    
}

plot_estimated_distribution <- function(first_name, ...){
    df <- get_estimated_distribution(first_name, ...)
    
    if(nrow(df) < 5){
        return(NULL)
    } else {
        
        f_df <- df %>% select(first_name, year_of_birth, matches('^f_.*')) %>%
            rename_at(vars(matches('^f_.*')), ~gsub("^f_","", .)) %>%
            mutate(gender = "Assigned Female at Birth") %>%
            mutate(color = neutral_colors_sampled[['f']]) %>%
            drop_na()
        
        m_df <- df %>% select(first_name, year_of_birth, matches('^m_.*')) %>%
            rename_at(vars(matches('^m_.*')), ~gsub("^m_","", .)) %>%
            mutate(gender = "Assigned Male at Birth") %>%
            mutate(color = neutral_colors_sampled[['m']]) %>%
            drop_na()
        
        if (nrow(f_df) < 3 | sum(f_df$estimated_count_alive) < 1000) {
            df_long <- m_df
        } else if (nrow(m_df) < 3 | sum(m_df$estimated_count_alive) < 1000) {
            df_long <- f_df
        } else {
            df_long <- bind_rows(m_df, f_df)
        }
        
        quartiles <- 
            bind_rows(
                df_long %>% group_by(gender) %>% slice_min(abs_25, n = 1L, with_ties = FALSE),
                df_long %>% group_by(gender) %>% slice_min(abs_50, n = 1L, with_ties = FALSE),
                df_long %>% group_by(gender) %>% slice_min(abs_75, n = 1L, with_ties = FALSE),
            ) %>%
            ungroup() %>%
            arrange(gender, year_of_birth)
        
        if(nrow(quartiles) == 3){
            quartiles <- quartiles %>%
            mutate(
                linetype = c('dotted', 'solid', 'dotted')
            )
        } else {
            quartiles <- quartiles %>%
            mutate(
                    linetype = rep(c('dotted', 'solid', 'dotted'), 2)
            )
        }
        
        p <- ggplot(df_long, aes(x = year_of_birth,
                                 y = estimated_prob_alive,
                                 color = color)) +
            # geom_smooth(aes(weight = estimated_count_alive), se = FALSE) +
            geom_point(aes(size = estimated_count_alive)) +
            geom_line(size = 0.5) +
            # geom_line(linejoin = 'round') +
            scale_color_identity() +
            scale_size(range = c(0.5, 6)) +
            scale_x_continuous(n.breaks = 10) +
            scale_y_continuous(limits = c(0, NA), labels = scales::label_percent(accuracy = 1)) +
            labs(
                x = "",
                y = paste0("Probability ", stringr::str_to_title(first_name), " Born in Year"),
                title = stringr::str_to_title(first_name),
                size = "Estimated Alive by Year"
            ) +
            geom_vline(data = quartiles, aes(xintercept = year_of_birth, linetype = linetype, color = color)) +
            geom_text(data = quartiles, aes(x = year_of_birth, label = year_of_birth, color = color), y = 0, hjust = 0, vjust = -0.5, angle = 90, size = rel(4), fontface = 'bold') +
            scale_linetype_identity() +
            lemon::facet_rep_grid(rows = vars(gender), scales = 'free_y', repeat.tick.labels = TRUE) +
            dark_theme_minimal(base_family = 'sans') +
            theme(
                legend.position = 'bottom',
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = rel(1.1), face = 'bold'),
                axis.text.x = element_text(angle = -90, size = rel(1.2), face = 'bold'),
                plot.title = element_text(
                    size = rel(2),
                    face = 'bold',
                    color = '#ffffff'
                ),
                plot.subtitle = element_text(
                    size = rel(1),
                    face = 'bold',
                    color = '#ffffff'
                ),
                strip.text = element_text(size = rel(1.1), face = 'bold', family = 'sans')
            )
        
        return(p)
        
    }
    
}


# UI ----------------------------------------------------------------------
ui <- fluidPage(
    tags$head(
        # Note the wrapping of the string in HTML()
        tags$style(HTML("
      body {
        background-color: black;
        color: white;
      }
      
      .shiny-input-container {
        background-color: black;
        color: white;
      }
      
      .form-control {
        background: black;
        background-color: black;
      }
                        
      .selectize-dropdown, .selectize-input, .selectize-input {
        background-color: black;
        background: black;
      }
                        "))
    ),
    # shinythemes::themeSelector(),
    
    # Application title
    # titlePanel("Guessing your birth year, with just your name!"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(width = 12, align = 'center',
        selectizeInput('first_name', label = "Enter a name: ", choices = character(0), multiple = FALSE)
        )
    ), 
    fluidRow(
        column(width = 12, align = 'center',
        plotOutput('age_plot', width = "80%")
        )
    )
)

# Server ------------------------------------------------------------------

server <- function(session, input, output) {
    
    updateSelectizeInput(session, "first_name", choices = first_name_choices, selected = "Colin", server = TRUE)

    output$age_plot <- renderPlot(
        plot_estimated_distribution(first_name = input$first_name)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
