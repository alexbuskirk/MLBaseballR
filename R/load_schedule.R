#' Pull schedule data for a given year
#'
#' Create a list of data frames -- one for each year specified in the function
#'
#' @param years numeric
#'
#' @return list
#' @export
#' @examples
#' load_schedule(years = c(2020,2021))
#'
#' @importFrom rlang .data

load_schedule <- function(years) {

  if (!is.numeric(years)) {stop("Please enter the year of the season you want schedule data for.
                          Example: load_schedules(years = c(2019,2020,2021))")}

  links <- purrr::map(years, function(x) {
      glue::glue('https://www.baseball-reference.com/leagues/majors/{x}-schedule.shtml')
    }) %>%
    unlist()

  results <- purrr::map(links, function(x) {

      content <- rvest::read_html(x)

      past_schedule_base <- rvest::html_elements(content,'.game , h3') %>%
        rvest::html_text() %>%
        as.data.frame(stingsAsFactors = FALSE) %>%
        dplyr::rename('text' = 1) %>%
        dplyr::mutate(
          text = tolower(as.character(.data$text)),
          row = dplyr::case_when(stringr::str_detect(as.character(.data$text),"sunday|monday|tuesday|wednesday|thursday|friday|saturday|today's") ~ dplyr::row_number(),
                          TRUE ~ as.integer(0)))

      past_schedule_dates <- past_schedule_base %>%
        dplyr::filter(row > 0) %>%
        dplyr::mutate(date = stringr::str_remove(.data$text, "^[:alpha:]*\\,"),
               date = lubridate::mdy(.data$date),
               date = dplyr::case_when(is.na(.data$date) ~ Sys.Date(), TRUE ~ .data$date)
        ) %>%
        dplyr::select(-.data$text)


      if (past_schedule_dates %>% dplyr::filter(date == Sys.Date()) %>% dplyr::count() %>% unlist() > 1 ) {

        stop(glue::glue("Duplicate date(s) in the schedule.
                  link: ",x))

      } else {

      regex_gametime <- "[[:digit:]]*\\:[[:digit:]]*[:blank:]*am|pm[:blank:]*"  # example: "12:00 pm "
      regex_leadingblanks <- "^[:blank:]*"                                      # example: " "
      regex_teamname <- "[a-z|\\s]*"                                            # example: "seattle mariners"

      regex_score <- "\\([[:digit:]]*\\)"                                       # example: "(10)"
      regex_hometeamprefix <- "\\@[:blank:]*"                                   # example: "@ "
      regex_hometeampostfix <- "[:blank:]*(preview|boxscore)[:blank:]*$"        # example: " preview " or " boxscore "

      past_schedule_results <- past_schedule_base %>%
        dplyr::mutate(row = dplyr::if_else(row == 0, dplyr::row_number(), as.integer(0))) %>%
        dplyr::filter(row > 0) %>%
        dplyr::mutate(text = stringr::str_remove_all(.data$text, "\n*"),
               text = stringr::str_remove_all(.data$text, "\\."),
               text = stringr::str_remove_all(.data$text, "\\'"),

               away_team = dplyr::case_when(stringr::str_detect(.data$text,regex_gametime)
                                     ~ stringr::str_remove(stringr::str_extract(.data$text, glue::glue(regex_leadingblanks,regex_gametime,regex_teamname)), regex_gametime),
                                     TRUE ~ stringr::str_remove(stringr::str_extract(.data$text, glue::glue(regex_leadingblanks,regex_teamname)), regex_leadingblanks)),
               away_score = as.integer(stringr::str_remove_all(stringr::str_extract(.data$text, regex_score),"\\(|\\)")),

               home_team = stringr::str_remove_all(stringr::str_extract(.data$text, glue::glue(regex_hometeamprefix, regex_teamname)), glue::glue(regex_hometeamprefix,"|",regex_hometeampostfix)),
               home_score = as.integer(stringr::str_remove_all(stringr::str_extract(.data$text, glue::glue(regex_score,regex_hometeampostfix)) ,glue::glue("\\(|\\)|",regex_hometeampostfix)))
        )

      for (r in seq_along(past_schedule_dates$date)) {

        if (sum(stringr::str_detect(names(past_schedule_results),"row_match"))==1) {

          past_schedule_results <- past_schedule_results %>%
            dplyr::mutate(row_match = dplyr::case_when(.data$row > past_schedule_dates$row[r] ~ past_schedule_dates$row[r],
                                         TRUE ~ row_match))

        } else {

          past_schedule_results <- past_schedule_results %>%
            dplyr::mutate(row_match = dplyr::case_when(.data$row > past_schedule_dates$row[r] ~ past_schedule_dates$row[r]))

        } #End if: row matches

      } # End loop: r

      Sys.sleep(2)

        dplyr::left_join(past_schedule_results, past_schedule_dates, by = c('row_match'='row')) %>%
        dplyr::select(-c('row','row_match','text')) %>%
        dplyr::mutate(away_win_loss = dplyr::case_when(!is.na(.data$away_score) & .data$away_score > .data$home_score ~ 'W',
                                         !is.na(.data$away_score) & .data$away_score == .data$home_score ~ 'T',
                                         !is.na(.data$away_score) & .data$away_score < .data$home_score ~ 'L'),

               home_win_loss = dplyr::case_when(.data$away_win_loss == 'L' ~ 'W',
                                                .data$away_win_loss == 'T' ~ 'T',
                                                .data$away_win_loss == 'W' ~ 'L'))

      } # End if: duplicate dates

    }) # End purrr map: results

  names(results) <- years

  results

} # End function: load schedules


