#' Pull schedule data for a given year
#'
#' Create a list of data frames -- one for each year specified in the function
#'
#' @param year numeric
#'
#' @return list
#' @export
#' @examples
#' load_schedule(years = c(2020,2021))

load_schedule <- function(years) {

  if (!is.numeric(years)) {stop("Please enter the year of the season you want schedule data for.
                          Example: load_schedules(years = c(2019,2020,2021))")}

  links <- years %>%
    purrr::map(., function(x) {glue('https://www.baseball-reference.com/leagues/majors/{x}-schedule.shtml')}) %>%
    unlist(.)

  results <- links %>%
    purrr::map(., function(x) {

      content <- rvest::read_html(x)

      past_schedule_base <- content %>%
        rvest::html_elements(.,'.game , h3') %>%
        rvest::html_text() %>%
        as.data.frame(stingsAsFactors = FALSE) %>%
        rename('text' = 1) %>%
        mutate(
          text = tolower(as.character(text)),
          row = case_when(stringr::str_detect(as.character(text),"sunday|monday|tuesday|wednesday|thursday|friday|saturday|today's") ~ row_number(),
                          TRUE ~ as.integer(0)))

      past_schedule_dates <- past_schedule_base %>%
        filter(row > 0) %>%
        mutate(date = str_remove(text, "^[:alpha:]*\\,"),
               date = mdy(date),
               date = case_when(is.na(date) ~ Sys.Date(), TRUE ~ date)
        ) %>%
        select(-text)


      if (past_schedule_dates %>% filter(date == Sys.Date()) %>% count() %>% unlist() > 1 ) {

        stop(glue("Duplicate date(s) in the schedule.
                  link: ",x))

      } else {

      regex_gametime <- "[[:digit:]]*\\:[[:digit:]]*[:blank:]*am|pm[:blank:]*"  # example: "12:00 pm "
      regex_leadingblanks <- "^[:blank:]*"                                      # example: " "
      regex_teamname <- "[a-z|\\s]*"                                            # example: "seattle mariners"

      regex_score <- "\\([[:digit:]]*\\)"                                       # example: "(10)"
      regex_hometeamprefix <- "\\@[:blank:]*"                                   # example: "@ "
      regex_hometeampostfix <- "[:blank:]*(preview|boxscore)[:blank:]*$"        # example: " preview " or " boxscore "

      past_schedule_results <- past_schedule_base %>%
        mutate(row = if_else(row == 0, row_number(), as.integer(0))) %>%
        filter(row > 0) %>%
        mutate(text = str_remove_all(text, "\n*"),
               text = str_remove_all(text, "\\."),
               text = str_remove_all(text, "\\'"),

               away_team = case_when(str_detect(text,regex_gametime)
                                     ~ str_remove(str_extract(text, glue(regex_leadingblanks,regex_gametime,regex_teamname)), regex_gametime),
                                     TRUE ~ str_remove(str_extract(text, glue(regex_leadingblanks,regex_teamname)), regex_leadingblanks)),
               away_score = as.integer(str_remove_all(str_extract(text, regex_score),"\\(|\\)")),

               home_team = str_remove_all(str_extract(text, glue(regex_hometeamprefix, regex_teamname)), glue(regex_hometeamprefix,"|",regex_hometeampostfix)),
               home_score = as.integer(str_remove_all(str_extract(text, glue(regex_score,regex_hometeampostfix)) ,glue("\\(|\\)|",regex_hometeampostfix)))
        )

      for (r in seq_along(past_schedule_dates$date)) {

        if (sum(str_detect(names(past_schedule_results),"row_match"))==1) {

          past_schedule_results <- past_schedule_results %>%
            mutate(row_match = case_when(row > past_schedule_dates$row[r] ~ past_schedule_dates$row[r],
                                         TRUE ~ row_match))

        } else {

          past_schedule_results <- past_schedule_results %>%
            mutate(row_match = case_when(row > past_schedule_dates$row[r] ~ past_schedule_dates$row[r]))

        } #End if: row matches

      } # End loop: r

      Sys.sleep(2)

      past_schedule_results %>%
        left_join(., past_schedule_dates, by = c('row_match'='row')) %>%
        select(-c('row','row_match','text')) %>%
        mutate(away_win_loss = case_when(!is.na(away_score) & away_score > home_score ~ 'W',
                                         !is.na(away_score) & away_score == home_score ~ 'T',
                                         !is.na(away_score) & away_score < home_score ~ 'L'),

               home_win_loss = case_when(away_win_loss == 'L' ~ 'W',
                                         away_win_loss == 'T' ~ 'T',
                                         away_win_loss == 'W' ~ 'L'))

      } # End if: duplicate dates

    }) # End purrr map: results

  names(results) <- years

  results

} # End function: load schedules


