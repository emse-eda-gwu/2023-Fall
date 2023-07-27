set.seed(5678)

# Load libraries and options
library(knitr)
library(here)
library(tidyverse)
library(fontawesome)
library(cowplot)
library(kableExtra)

options(dplyr.width = Inf)
options(knitr.kable.NA = '')

knitr::opts_chunk$set(
    message = FALSE,
    warning = FALSE,
    comment    = "#>",
    fig.retina = 3,
    fig.width  = 6,
    fig.height = 4,
    fig.show = "hold",
    fig.align  = "center",
    fig.path   = "figs/"
)

clean_schedule_name <- function(x) {
    x <- x %>%
        str_to_lower() %>%
        str_replace_all(" & ", "-") %>%
        str_replace_all(" ", "-")
    return(x)
}

# Load custom functions

get_schedule <- function() {
    
    # Get raw schedule
    schedule_url <- 'https://docs.google.com/spreadsheets/d/1heQ9ylgQ8Ok2MjIAGPRy8kdMnnyojuQFGxek9oFXF9Y/edit?usp=sharing'
    df <- gsheet::gsheet2tbl(schedule_url) %>% 
        mutate(
            class_stub  = clean_schedule_name(class_name),
            assign_stub = clean_schedule_name(assign_name),
            mini_stub   = clean_schedule_name(mini_name),
            final_stub  = clean_schedule_name(final_name)
        )
    
    # Weekly assignment vars
    assignments <- df %>% 
        filter(!is.na(assign_name)) %>% 
        mutate(
            assign_due_md = format(assign_due, format = "%b %d"),
            assign_stub = paste0(assign_n, "-", assign_stub)
        ) %>% 
        select(week, starts_with("assign_"))
    
    # Mini project vars
    mini <- df %>%
        filter(!is.na(mini_name)) %>% 
        mutate(
            mini_n = row_number(),
            mini_due_md = format(as.Date(mini_due), format = "%b %d"),
            mini_stub = paste0(mini_n, "-", mini_stub)
        ) %>% 
        select(week, starts_with("mini_"))
    
    # Final project vars
    final <- df %>%
        filter(!is.na(final_name)) %>% 
        mutate(
            final_n = row_number(),
            final_due_md = format(as.Date(final_due), format = "%b %d"),
            final_stub = paste0(final_n, "-", final_stub)
        ) %>% 
        select(week, starts_with("final_"))
    
    # Class vars
    class <- df %>%
        mutate(
            # Replace NA values with ""
            class_description = ifelse(
                is.na(class_description), "", class_description),
            class_stub = ifelse(
                is.na(class_n), class_stub, 
                paste0(class_n, "-", class_stub))
        ) %>% 
        select(week, starts_with("class_"))
    
    # Final schedule data frame
    schedule <- df %>% 
        select(week, date, theme, quiz) %>% 
        mutate(date_md = format(date, format = "%b %d")) %>% 
        left_join(class, by = "week") %>% 
        left_join(assignments, by = "week") %>% 
        left_join(mini, by = "week") %>% 
        left_join(final, by = "week")
    
    return(schedule)
    
}

get_schedule <- function() {
    
    # Icons
    # https://icons.getbootstrap.com/
    # fa <- list(
    #     class      = '<i class="bi-laptop-fill"></i>',
    #     assignment = '<i class="bi-pencil-fill"></i>',
    #     reading    = '<i class="bi-book-fill"></i>'
    # )
    
    schedule_raw <- read_csv(here::here('schedule.csv'))
    
    # Quiz vars
    quiz <- schedule_raw %>%
        mutate(
            quiz = ifelse(
                is.na(quiz),
                "",
                paste0('Quiz ', quiz, ":<br><em>", quiz_coverage, "</em>"))
        ) %>%
        select(week, quiz)
    
    # Weekly assignment vars
    assignments <- schedule_raw %>%
        mutate(
            due_assign = format(due_assign, format = "%b %d"),
            assignments = ifelse(
                is.na(due_assign),
                "",
                paste0(
                    '<a href="hw/', n_assign, "-", stub_assign, '.html"><b>HW ',
                    n_assign, "</b></a><br>Due: ", due_assign))
        ) %>%
        select(week, assignments)
    
    # Class vars
    class <- schedule_raw %>%
        mutate(
            description_class = ifelse(
                is.na(description_class),
                "",
                description_class),
            class = ifelse(
                is.na(stub_class),
                paste0("<b>", name_class, "</b><br>", description_class),
                paste0(
                    '<a href="class/', n_class, "-", stub_class, '.html"><b>',
                    name_class, "</b></a><br> ",
                    description_class)),
        ) %>%
        select(week, class)
    
    # Reading vars
    reading <- schedule_raw %>%
        select(week, ends_with("_reading"), reading) %>%
        rename(name = name_reading, stub = stub_reading) %>%
        mutate(
            name = str_split(name, '\n'),
            stub = str_split(stub, '\n')
        )
    
    # Fix reading names
    reading_root <- 'https://p4a.jhelvy.com/'
    reading$readings <- ""
    for (i in 1:nrow(reading)) {
        name <- reading[i,]$name[[1]]
        if (any(is.na(unlist(name)))) {
            result <- ''
        } else {
            stub <- reading[i,]$stub[[1]]
            result <- paste0(
                '<a href=', reading_root, stub ,'.html target="_blank"><b>',
                name, "</b></a>")
            result <- paste(result, collapse = '<br>')
            
        }
        reading$readings[i] <- result
    }
    reading$name <- NULL
    reading$stub <- NULL
    reading <- reading %>% 
        mutate(readings = ifelse(!is.na(reading), reading, readings))
    
    # Final schedule data frame
    schedule <- schedule_raw %>%
        select(week, date, n_assign, due_assign) %>%
        mutate(date_md = format(date, format = "%b %d")) %>%
        left_join(quiz, by = "week") %>%
        left_join(class, by = "week") %>%
        left_join(assignments, by = "week") %>%
        left_join(reading, by = "week") %>% 
        ungroup()
    
    return(schedule)
}
