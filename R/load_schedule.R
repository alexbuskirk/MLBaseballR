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

  # Check if years are numeric
  if (!is.numeric(years)) {stop("Please enter the year of the season you want schedule data for.
                          Example: load_schedules(years = c(2019,2020,2021))")}

  # Iterating through each year provided
  results <- purrr::map(years, function(yr) {

    # Creating the links
    link <- glue::glue('https://www.baseball-reference.com/leagues/majors/{yr}-schedule.shtml')

    content <- rvest::read_html(link)

    # Flagging rows with schedule dates
    schedule_base <- rvest::html_elements(content,'.game , h3') %>%
      rvest::html_text() %>%
      as.data.frame(stingsAsFactors = FALSE) %>%
      dplyr::rename('text' = 1) %>%
      dplyr::mutate(
        text = tolower(as.character(.data$text)),
        row = dplyr::case_when(stringr::str_detect(as.character(.data$text),"sunday|monday|tuesday|wednesday|thursday|friday|saturday|today's") ~ dplyr::row_number(),
                               TRUE ~ as.integer(0)))

    # check if the seasons requested are in the past or are still in progress
    if (stringr::str_detect(tolower(schedule_base$text), "today's games") %>% sum() > 0) {

      today_row <- schedule_base %>%
        dplyr::filter(row > 0) %>%
        dplyr::mutate(index = dplyr::row_number()) %>%
        dplyr::filter(stringr::str_detect(.data$text, "today's"))

      schedule_dates <- schedule_base %>%
        dplyr::filter(row > 0) %>%
        dplyr::mutate(date = stringr::str_remove(.data$text, "^[:alpha:]*\\,[:blank:]*"),
                      date = dplyr::case_when(stringr::str_detect(tolower(.data$text), "today's") ~ .data$date[today_row$index+1], TRUE ~ .data$date),
                      date = lubridate::mdy(.data$date),
                      date = dplyr::case_when(stringr::str_detect(tolower(.data$text), "today's") ~ .data$date[today_row$index+1]-1, TRUE ~ .data$date)
        ) %>%
        dplyr::select(-.data$text)

    } else {

      schedule_dates <- schedule_base %>%
        dplyr::filter(row > 0) %>%
        dplyr::mutate(date = stringr::str_remove(.data$text, "^[:alpha:]*\\,[:blank:]*"),
                      date = lubridate::mdy(.data$date)
        ) %>%
        dplyr::select(-.data$text)

    } # End if: current or past season

    # Check for duplicate dates
    if (schedule_dates %>% dplyr::group_by(date) %>% dplyr::count() %>% dplyr::filter(.data$n>1) %>% nrow() > 0) {

      stop(glue::glue("Duplicate date(s) in the schedule.
                  \nlink: ",link))

    } else {

      regex_gametime <- "[[:digit:]]*\\:[[:digit:]]*[:blank:]*am|pm[:blank:]*"  # example: "12:00 pm "
      regex_leadingblanks <- "^[:blank:]*"                                      # example: " "
      regex_teamname <- "[a-z|\\s]*"                                            # example: "seattle mariners"

      regex_score <- "\\([[:digit:]]*\\)"                                       # example: "(10)"
      regex_hometeamprefix <- "\\@[:blank:]*"                                   # example: "@ "
      regex_hometeampostfix <- "[:blank:]*(preview|boxscore)[:blank:]*$"        # example: " preview " or " boxscore "

      schedule_results <- schedule_base %>%
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

      # joining schedule dates and games/results by row index
      for (r in seq_along(schedule_dates$date)) {

        if (sum(stringr::str_detect(names(schedule_results),"row_match"))==1) {

          schedule_results <- schedule_results %>%
            dplyr::mutate(row_match = dplyr::case_when(.data$row > schedule_dates$row[r] ~ schedule_dates$row[r],
                                                       TRUE ~ row_match))

        } else {

          schedule_results <- schedule_results %>%
            dplyr::mutate(row_match = dplyr::case_when(.data$row > schedule_dates$row[r] ~ schedule_dates$row[r]))

        } #End if: row matches

      } # End loop: r

      # System sleep for random interval
      Sys.sleep(stats::runif(1, min = 2, max = 5))

      # Create final data set
      final_data <- dplyr::left_join(schedule_results, schedule_dates, by = c('row_match'='row')) %>%
        dplyr::select(-c('row','row_match','text')) %>%
        dplyr::mutate(away_win_loss = dplyr::case_when(!is.na(.data$away_score) & .data$away_score > .data$home_score ~ 'W',
                                                       !is.na(.data$away_score) & .data$away_score == .data$home_score ~ 'T',
                                                       !is.na(.data$away_score) & .data$away_score < .data$home_score ~ 'L'),

                      home_win_loss = dplyr::case_when(.data$away_win_loss == 'L' ~ 'W',
                                                       .data$away_win_loss == 'T' ~ 'T',
                                                       .data$away_win_loss == 'W' ~ 'L'),
                      season = yr)

    } # End if: duplicate dates


    # Final data quality checks
    if(

      !lubridate::is.Date(final_data$date)
      & !is.character(final_data$away_team)
      & !is.numeric(final_data$away_score)
      & !is.character(final_data$home_team)
      & !is.numeric(final_data$home_score)
      & !is.character(final_data$away_win_loss)
      & !is.character(final_data$home_win_loss)
      & !is.numeric(final_data$season)
      & length(final_data) == 8

    ) {

      message(
        glue::glue(
          "Please check for data type mismatches. See FALSE(s) below:",
          "\nTotal of 8 columns = ", length(final_data) == 8,
          "\ndate = ",lubridate::is.Date(final_data$date),
          "\naway_team = ",is.character(final_data$away_team),
          "\naway_score = ",is.numeric(final_data$away_score),
          "\nhome_team = ",is.character(final_data$home_team),
          "\nhome_score = ",is.numeric(final_data$home_score),
          "\naway_win_loss = ",is.character(final_data$away_win_loss),
          "\nhome_win_loss = ",is.character(final_data$home_win_loss),
          "\nseason = ",is.numeric(final_data$season)

        )
      )

    } else {

      message(glue::glue("{yr} schedule data has passed all quality checks"))

    } # End if: Final data quality checks

    final_data

  }) # End purrr map: results

  names(results) <- years

  results

} # End function: load schedules


