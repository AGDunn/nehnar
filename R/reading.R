# #############################################################################
#' Label a Row Based on Label Above or Label Below
#'
#' Alter labels in a data frame based on relative position of other labels in
#' the data frame.
#'
#' Assigns new values to selected rows of the variable label in a
#' data frame.  Pass it a character string for this_label and another for
#' label_checked.  It will try to assign the former based on the locations of
#' the latter.  For each row it checks in label, it will look at the value in 
#' label either in the row above or the row below, as instructed.  This is an
#' internal function used when reading in my book-reading history.
#' Where dates for start or end of reading are incomplete, the function
#' currently records an NA.
#'
#' @param my_data a data frame, which needs to include a variable called
#'   `label'".  Default NULL.
#' @param this_label a character string that you want to assign to one or more
#'   rows in my_data.  Default NULL.
#' @param label_checked a character string; this is the value the function
#'   looks for in label when deciding whether to change a given label to 
#'   this_label.  Default NULL.
#' @param check_above a boolean.  If TRUE, then the function will check whether
#'   to assign this_label to the current row by checking value of label_checked
#'   matches the label value in the row above.  If FALSE, it will check the row
#'   below instead.  Default TRUE.
#' @keywords label, data munging
#' @importFrom dplyr mutate
#' @importFrom dplyr lag
#' @importFrom dplyr case_when
#' @importFrom dplyr lead
#' @importFrom magrittr %>%
#' @return a data frame corresponding to my_data but with some row values in
#'   the label variable changed as requested.
add_pos_label <- function(my_data = NULL, this_label = NULL,
                          label_checked = NULL, check_above = TRUE){
  if (check_above) {
    my_data %>%
      mutate(
        label = case_when(
          lag(label, n = 1) == label_checked ~ this_label,
          TRUE ~ label
        )
      )
  } else {
    my_data %>%
      mutate(
        label = case_when(
          lead(label, n = 1) == label_checked ~ this_label,
          TRUE ~ label
        )
      )
  }
}
# #############################################################################

# #############################################################################
#' Read in My Book Notes
#' 
#' Reads in a plain text file of notes on books and returns a tidy data frame.
#'
#' Designed for my own format of notes on books, including when I read them.
#' Won't work on anything that doesn't match that formatting.  Creates
#' tolerably pretty variables.
#'
#' @param source_file path to the plain-text file to pull book notes from.
#'   Will only work if used on a file with the same layout as mine.
#'   Default NULL.
#' @param diagnose_speed a boolean; if TRUE, prints the time taken by the core
#'   (slow) loop in the function.  Default FALSE.
#' @keywords data munging, idiosyncratic
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_at
#' @importFrom dplyr select
#' @importFrom lubridate dmy
#' @importFrom magrittr %>%
#' @importFrom purrr quietly
#' @importFrom readr read_lines
#' @importFrom stats na.omit
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_squish
#' @importFrom stringr str_sub
#' @importFrom stringr str_to_title
#' @importFrom stringr str_trim
#' @importFrom tibble add_row
#' @importFrom tibble tibble
#' @importFrom tidyr separate
#' @importFrom tidyr spread
#' @return a data frame with bibliographic details of books as separate
#'   columns.  Also includes start and end dates of reading each book and
#'   keywords I've noted for each.
#' @export
read_book_notes <- function(source_file = NULL,
                            diagnose_speed = FALSE){

  # check for a source file ###################################################
  if (is.null(source_file)){
    stop("need to specify a source file")
  }
  # ###########################################################################

  # ZZZ todo block ############################################################
  # add some reading notes data for illustrative purposes.
  # ###########################################################################

  # ###########################################################################
  # read in the raw data as a vector, with each line a single entry in the
  # vector.  Bind this to another vector which will (eventually) hold the 
  # labels for the source data.  Result is a two-column (very long) tibble.
  len_read <- length(read_lines(source_file))

  my_reading <- tibble(
    label = rep("nope", len_read),
    raw_data = read_lines(source_file),
    item = rep(0, len_read)
  )

  # add blank line as the first line so the function can find the first
  # author_year_title line.
  my_reading <- my_reading %>%
    add_row(label = "nope", raw_data = "", item = 0, .before = 1)
  # ########################################################################### 

  # add labels based on contents of raw_data ################################## 
  # some data labels (blank lines, start & end dates, keywords) can be added
  # based on contents of raw_data.
  my_reading <- my_reading %>%
    mutate(
      label = case_when(
        # blank lines, start reading, end reading, keywords:
        str_detect(raw_data, "^$") ~ "blank",
        str_detect(raw_data, "^start:")  ~ "start_",
        str_detect(raw_data, "^finish:") ~ "finish_",
        str_detect(raw_data, "^KEY:")    ~ "keywords",
        TRUE ~ label
      )
    )
  # ###########################################################################
  
  # remove leading text from all start and finish data #######################
  # should remove the text before the date as well as any amount of leading
  # white space.
  my_reading <- my_reading %>%
    mutate(raw_data = case_when(
               str_detect(label, "start_") ~ str_remove(raw_data, "start: "),
               str_detect(label, "finish_") ~ str_remove(raw_data, "finish: "),
               TRUE ~ raw_data)
      ) %>%
    mutate(raw_data = case_when(
               str_detect(label, "start_") ~ str_trim(raw_data),
               str_detect(label, "finish_") ~ str_trim(raw_data),
               TRUE ~ raw_data)
    )
  # ###########################################################################

  # ###########################################################################

  # halt function if there are two blank lines in a row #######################
  # repeated blank lines mess up the labels
  checker <- rep(FALSE, length(my_reading$label))
  for (ind in seq_along(my_reading$label)) {
    checker[ind] <- 
      (my_reading$label[ind] == "blank" &&
      my_reading$label[ind] == my_reading$label[ind - 1])
  }
  
  # remove first line of checker, which will be NA because it doesn't have a
  # previous line to compare with.
  checker <- na.omit(checker)

  # report which lines (if any) are repeated blanks and stop()
  if (sum(checker) > 0) {
    print(paste0("there are ", sum(checker), " repeated blank lines"))
    faults <- paste(which(checker), collapse = ", ")
    stop("repeated blank lines:", faults)
  }
  rm(checker); rm(ind)
  # ###########################################################################

  # fix labels for books read more than once ##################################
  # A loop through the label column to add a number to the end of every start_
  # and finish_ label.
  read_counter <- 1
  for (ind in seq_along(my_reading$label)) {
    if (my_reading$label[ind] == "start_") {
      my_reading$label[ind] <- paste0(my_reading$label[ind], read_counter)
    } else if (my_reading$label[ind] == "finish_") {
      my_reading$label[ind] <- paste0(my_reading$label[ind], read_counter)
      read_counter <- read_counter + 1
    } else {
      read_counter <- 1
    }
  }
  rm(read_counter); rm(ind)
  # ###########################################################################

  # find and label the first author_year_title row per entry ##################
  # adds the label to the first line after each blank line.
  my_reading <- my_reading %>%
    add_pos_label(
      this_label = "author_year_title",
      label_checked = "blank"
    )
  # ###########################################################################

  # find and label place_publisher row ########################################
  # it's the row above the start_ row that isn't another finish_ or start_row
  my_reading <- my_reading %>%
    add_pos_label(
      check_above = FALSE,
      this_label = "place_publisher",
      label_checked = "start_1"
    )
  # ###########################################################################

  # label remaining author_year_title using position both ways ################
  # if a row is "nope" and it has "author_year_title", above, then it is set as
  # "author_year_title" as well; if a "nope" row has "publisher_place" below,
  # then the "nope" row is made into "author_year_title".  Passes this to a
  # second mutate that will transform a "nope" into "author_year_title" if it
  # has "author_year_title" both above and below.
  my_reading <- my_reading %>%
    mutate(
      label = case_when(
        label == "nope" & lag(label, n = 1) == "author_year_title"
          ~ "author_year_title",
        label == "nope" & lead(label, n = 1) == "place_publisher"
          ~ "author_year_title",
        TRUE ~ label
      )
    ) %>%
    mutate(
      label = case_when(
        lag(label, n = 1) == "author_year_title" &
          lead(label, n = 1) == "author_year_title"
          ~ "author_year_title",
        TRUE ~ label
      )
    )
  # This (probably) captures up to four lines of author_year_title properly.
  # ###########################################################################

  # label all remaining "nope" as "notes" ######################################
  my_reading <- my_reading %>%
    mutate(
      label = case_when(
       label == "nope" ~ "notes",
       TRUE ~ label
      )
    )
  # ###########################################################################

  # add a per-item index column ###############################################
  my_reading$item <- rep(0, length(my_reading$label))
  item_counter <- 0
  
  for (ind in seq_along(my_reading$item)) {
    if (my_reading$label[ind] == "blank") {
      item_counter <- item_counter + 1
    } else {
      my_reading$item[ind] <- item_counter
    }
  }
  rm(item_counter); rm(ind)
  # ###########################################################################

  # join data (e.g. notes) that are split across multiple rows ################
  # the for() loop is over every row except the final one.  The while() loop
  # inside it compares the current line to the one after it.  If they have the
  # same label, it adds the next line's data to the current line, then deletes
  # the next line.
  if (diagnose_speed) {
    start_of_joining <- Sys.time()
  }
  try(
    for (ind in seq_along(my_reading$label)) {
        while (my_reading$label[ind] == my_reading$label[ind + 1]) {
          my_reading$raw_data[ind] <- paste(
            my_reading$raw_data[ind], my_reading$raw_data[ind + 1]
          )
          my_reading <- my_reading[-(ind + 1), ]
        }
      }
  )
  if (diagnose_speed) {
    end_of_joining <- Sys.time()
  }
  # ZZZ works but produces an error message; using try() lets the rest of the
  # script execute.  Is there a way to suppress that error message?
  # ###########################################################################

  # remove blank lines and put columns in sensible order ######################
  my_reading <- my_reading %>%
    filter(label != "blank") %>%
    select(item, label, raw_data)
  # ###########################################################################

  # turn it into a standard-issue wide data frame #############################
  # putting the columns in a sensible order (it wants to do them
  # alphabetically) would mean having to explicitly name columns to select,
  # which would risk a silent error if a book were later started for a third
  # time.
  my_reading <- my_reading %>%
    spread(label, raw_data)
  # once it's in a more standard format, Tidyverse tools for working with
  # columns can be used.  Wickham has created pivot() as a successor function.
  # ###########################################################################

  # splitting author_year_title and cleaning the new cols #####################
  # Pattern-matches on the year, which we assume is four digits long; everything
  # before the year is assigned to new author column and everything after to new
  # title column.
  my_reading <- my_reading %>%
    mutate(
      author = str_squish(
        str_split_fixed(author_year_title, "\\d\\d\\d\\d", n = 2)[, 1]),
      year = str_extract(author_year_title, "\\d\\d\\d\\d"),
      title = str_squish(
        str_split_fixed(author_year_title, "\\d\\d\\d\\d", n = 2)[, 2])
    )
  # ZZZ this won't work on items with no publication date; fix with case_when()?
  # ###########################################################################

  # turn the start_ and finish_ variables into dates ##########################
  # Should now work for all start and finish variables.  Some of the parsing
  # errors will be due to bad data.
  # ZZZ is there a way to directly capture the parsing errors as a printed
  # output?
  # ZZZ is it worth making these quiet using quiet_dmy <- quietly(dmy)?
  # ZZZ these seem to be the same issue.  Make a diagnostics v quietly option
  # part of the existing diagnostics option.  Might need to use possibly()
  # instead.
  my_reading <- my_reading %>%
    mutate_at(vars(matches("start_")), dmy) %>%
    mutate_at(vars(matches("finish_")), dmy)
  # ###########################################################################

  # split out place and publisher #############################################
  # first turn place_publisher into 2 cols: place, publisher
  my_reading <- my_reading %>%
    separate(place_publisher,
      into = c("place", "publisher"),
      sep = "; "
    )
  
  # tidy place by removing excess spaces then setting "no location" to NA.
  my_reading <- my_reading %>%
    mutate(place = str_squish(place)) %>%
    mutate(place = case_when(
        place == "no location" ~ NA_character_,
        TRUE ~ place)
    )
  
  # tidy publisher by removing excess spaces then setting "no publisher" to NA.
  my_reading <- my_reading %>%
    mutate(publisher = str_squish(publisher)) %>%
    mutate(publisher = case_when(
        publisher == "no publisher" ~ NA_character_,
        TRUE ~ publisher)
    )
  # ###########################################################################

  # clean up keywords so it doesn't say "KEY: " ###############################
  # first mutate removes the initial "KEY: "; second mutate replaces each other
  # "KEY: " with a ", ".
  my_reading <- my_reading %>%
    mutate(keywords = str_remove(keywords, "^KEY: ")
    ) %>%
    mutate(keywords = str_replace_all(keywords, " *KEY: ", ", ")
    ) %>%
    mutate(keywords = str_replace_all(keywords, " ,", ",")
    )
  # ###########################################################################

  # format author and title variables #########################################
  # it's not perfect; str_to_title() doesn't know to leave certain words alone
  # when capitalising the first letter of each word.
  my_reading <- my_reading %>%
    mutate(
      author = str_to_title(author),
      title = str_to_title(title)
  )
  # ###########################################################################

  # remove author_year_title from finalised tibble ############################
  my_reading <- my_reading %>%
    select(-author_year_title)
  # ###########################################################################

  # produce outputs ###########################################################
  if (diagnose_speed) {
    print(paste("Joining data that were split across rows took",
                difftime(end_of_joining, start_of_joining, units = "secs"))
    )
  }
  return(my_reading)
  # ###########################################################################
}
# #############################################################################

# #############################################################################
#' Distinguish between Things Published in Same Year
#'
#' Given a dataframe with an identifier variable, returns the same dataframe
#' with a different letter appended to repeat values of that variable.
#' 
#' Some authors will produce more than one book (or other bibliography item) 
#' in a particular year.  When two or more such items are present, this
#' function will add a letter to the end of the date, starting at 'a' and
#' cycling through the (English) alphabet.  It'll do this to any character
#' variable, but this will result in silly outcomes unless the last part of the
#' string is a year.
#'
#' @param my_df a dataframe with an identifier that needs to be made unique.
#' @param my_id_col a character column in that dataframe to be altered so that
#'   all values in it are unique.
#' @keywords data munging, ID variables, bibliography,
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr enquo
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @return a dataframe identical to the one supplied to the function but with
#'   the identifier variable altered so that repeats of a given value are now
#'   distinguished by a letter appended to the string (e.g. if 'Smith 2010'
#'   appears twice in the original, there will now be a 'Smith 2010a' and a 
#'   'Smith 2010b'.
#' @export
append_id <- function(my_df, my_id_col){
  # setup: initial values #####################################################
  # quo() the column that's used
  quo_id <- enquo(my_id_col)
  # add a blank to the start of the R builtin constant 'letters'
  for_dupes <- c("", letters)
  # set the counter which controls the while() loop to 1.
  current_pass <- 1
  # ###########################################################################
  
  # setup: the dataframe ######################################################
  # copies the ID column into a new temporary column, then deletes original ID
  # column and sorts on the new one.
  my_df <- my_df %>%
    arrange(!!quo_id) %>%
    mutate(temp_id = !!quo_id, dupe_seq = 0) %>%
    select(-!!quo_id)

  # determine how many copies there are of the most common value for the ID
  # variable
  max_passes <- max(table(my_df$temp_id))
  # ###########################################################################

# tag first row of each ID that's non-unique ##################################
# finds the first instance of each duplicated ID value and sets the
# corresponding dupe_seq to 1.  Otherwise doesn't change dupe_seq.
my_df <- my_df %>%
  mutate(
    dupe_seq = case_when(
      temp_id == lead(temp_id, n = 1) &
        (temp_id != lag(temp_id, n = 1) | is.na(lag(temp_id, n = 1))) ~ 1,
      TRUE ~ dupe_seq
    )
  )
# ##############################################################################

# add tags to duplicates that aren't tagged yet ###############################
# checks current row ID matches previous row ID & that the previous row
# dupe_seq was just changed (either by the dupe_seq = 1 part, or the previous
# iteration of the while() loop.  Will run one less time than there are copies
# of the most common ID value.
while (current_pass < max_passes) {
  my_df <- my_df %>%
    mutate(
      dupe_seq = case_when(
        temp_id == lag(temp_id, n = 1) &
          lag(dupe_seq, n = 1) == current_pass ~ current_pass + 1,
        TRUE ~ dupe_seq
      )
  )

  current_pass <- current_pass + 1
}
# #############################################################################

# append letters ############################################################
# based on dupe_seq, assigns a letter to the end of each instance of a 
# non-unique ID value.
my_df <- my_df %>% 
mutate(temp_id = paste0(temp_id, for_dupes[dupe_seq + 1])
) %>%
mutate(!!quo_id := temp_id) %>%
select(-dupe_seq, -temp_id)
# ###########################################################################

  return(my_df)
}
# #############################################################################

# #############################################################################
#' Show Book Progress on a Particular Day
#' 
#' Shows book completion stats on a date specified by user.
#' 
#' Supplied with a dataframe of book notes and a date of interest, this
#' will return the dataframe supplied to it with additional columns to show how
#' long a read attempt has taken (and whether it has been completed or just set
#' with an end date equal to the current system date).
#' 
#' @importFrom dplyr bind_rows
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom tidyr gather
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr starts_with
#' @importFrom lubridate dmy
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#' @param my_data a dataframe of book notes.  Must include start and finish
#'   dates for reading attempts.  No default.
#' @param a_date a date or character object of the form 'day month year'.  
#'   default today's date.
#' @return A data frame with one row per read attempt.  Must include a numeric
#'   column 'item' with a unique entry per book.  Default NULL.
#' @export
check_book_progress <- function(my_data, a_date = Sys.Date()){
  # create reads_without_books tibble #########################################
  # Make a tibble that's tidy per read attempt and has no book info other
  # than item number.
  reads_without_books <- my_data %>%
    select(item, starts_with("start") | starts_with("finish")) %>%
    # Pivot to make it one end (a start or a finish) of a read attempt per row.
    pivot_longer(-item) %>%
    # str_extract() to make read attempt ID number from read attempts with any
    # number of digits.
    mutate(read_attempt = str_extract(name, "[0-9].*")) %>%
    # Remove read attempt id from start and finish.
    mutate(name = case_when(str_detect(name, "start_") ~ "start",
                            str_detect(name, "finish_") ~ "finish")
    ) %>%
    # Pivot (again) to make it tidy per read attempt.
    pivot_wider()
  # ###########################################################################
  
  # create books_without_reads tibble #########################################
  # Make a tibble that's just the book details and has no start or finish.
  books_without_reads <- my_data %>%
    select(- starts_with("start"), - starts_with("finish"))
  # ############################################################################
  
  # join reads_without books to books_without_reads ############################
  # Joining the two gives several rows of non-existent reads with NA start and
  # NA end.  These are removed after joining.
  tidy_per_read <- right_join(books_without_reads, reads_without_books) %>%
    filter(!is.na(start) | !is.na(finish))
  # ###########################################################################
  
  # filter out read attempts from after a_date ################################
  # ZZZ remove the conditional on finish; it's only here to make it safe to
  # test on the not-great data. 
  tidy_per_read <- tidy_per_read %>%
    filter(start <= a_date)
  # ###########################################################################
  
  # create finished and unfinished variables ##################################
  # unfinished and finished variables created by comparing a_date to the
  # finish date of the read.
  tidy_per_read <- tidy_per_read %>%
    mutate(unfinished = finish > a_date | is.na(finish)) %>%
    mutate(finished = !unfinished)
  # ###########################################################################
  
  # determine duration of read attempt in days ################################
  # Assign a fake finish date of today to the unfinished reads, then determine
  # duration of each read in days.
  tidy_per_read <- tidy_per_read %>%
    mutate(finish = case_when(unfinished ~ Sys.Date(),
                    TRUE ~ finish)
    ) %>%
    mutate(duration = finish - start)
  # ###########################################################################

  # output of function: tibble tidy per read attempt ##########################
  return(tidy_per_read)
  # ###########################################################################
}
# #############################################################################

# #############################################################################
#' Show the number of books read and not-yet-finished on a give date
#'
#' Given a target date, returns a tibble which shows how many read-throughs
#' of books were finished on before that date and how many were 'in 
#' progress' (i.e. unfinished) on that date.
#'
#' @param target_date a date; default is Sys.Date()
#' @param my_notes a dataframe of read attempts; default NULL
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom magrittr %>%
#' @keywords book notes, summary
#' @return a one-row tibble of two columns: the number of books finished on
#'   target_date, then the number unfinished on target_date.
#' @export
book_finish_rate <- function(target_date = Sys.Date(), my_notes = NULL){
  my_summary <- my_notes %>%
    mutate(
      finished = case_when(
        (finish <= target_date & !unfinished) ~ TRUE,
        TRUE ~ FALSE
      ),
      unfinished = case_when(
        start <= target_date &
          (finish > target_date | unfinished) ~ TRUE,
        TRUE ~ FALSE
      ),
    ) %>%
    # sum the finished counts and the unfinished counts
    summarise(total_finished = sum(finished),
      total_unfinished = sum(unfinished)
    )
  
    return(my_summary)
}
# #############################################################################

# #############################################################################
#' find the number of finished and unfinished book reads over sequence of dates
#'
#' Based on the start and finish dates in a book notes data frame, returns a
#' new data frame showing the number of books finished by each date and the
#' number of books still mid-read on each date.
#'
#' @param start_date a date or a string that can be parsed by lubridate's
#'   `dmy()`.  Default Sys.Date() - 7.
#' @param end_date a date or a string that can be parsed by lubridate's
#'   `dmy()`.  Default Sys.Date().
#' @param my_notes a book notes dataframe; default NULL
#' @importFrom lubridate dmy
#' @importFrom lubridate is.Date
#' @importFrom purrr pmap_dfr
#' @importFrom tibble as_tibble
#' @keywords book notes, summary
#' @export
book_finish_ratios <- function(start_date = (Sys.Date() - 7),
                                 end_date = Sys.Date(),
                                 my_notes = NULL){

  # convert string inputs to dmy() ############################################
  if (!is.Date(start_date)){
    start_date <- dmy(start_date)
  }
  
  if (!is.Date(end_date)){
    end_date <- dmy(end_date)
  }
  # ##########################################################################

  # create vector of dates to check over ######################################
    some_days <- seq.Date(
      from = start_date, to = end_date,
      by = 1
    )
  # ###########################################################################
    
  # map book_finish_rate onto dates and notes #################################
  holder <- pmap_dfr(list(some_days), book_finish_rate, my_notes)
  # ###########################################################################

  # form into tibble with the dates, return it ################################
  books_by_day <- as_tibble(cbind(some_days, holder))
  return(books_by_day)
  # ###########################################################################
}
# #############################################################################

# #############################################################################
#' get the page count for a book from notes on it
#' 
#' ZZZ block describe function
#' 
#' ZZZ big block describes function
#'
#' @param one at a time for inputs
#' @keywords book notes, page count
#' @importFrom stringr str_count
#' @importFrom stringr str_extract
#' @return describe the function's output
#' @export
get_page_count <- function(source_note = NULL){
  # Define the desired string, which is "page count: [:digit:]+" on a line in
  # the notes on its own.
  target_string <- "page count:[:blank:]+[:digit:]+"

  # Check if the notes looked at are just an NA; if so then return an integer
  # NA.
  if (is.na(source_note)) {
    return(as.integer(NA))
  }

  # Check how many times the target string appears in the notes for the book.
  # Anything other than 1 appearance is bad, so should return a numerical NA.
  targets_found <- str_count(source_note, target_string)
  if (targets_found == 0) {
    return(as.integer(NA))
  } else if (targets_found > 1) {
    return(as.integer(NA))
  }

  # Core loop: get page count from well-formatted notes. ----------------------
  # Extract the target string.
  extracted_string <- str_extract(source_note, target_string)

  # Extract the page count itself from the target string.
  page_count <- as.integer(str_extract(extracted_string, "[:digit:]+"))
  # ---------------------------------------------------------------------------

  # Return a page count value, or an NA if an error found.
  return(page_count)
}
# #############################################################################
