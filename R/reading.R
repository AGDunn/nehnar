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
# ###########################################################################

#' Read in My Book Notes
#' 
#' Reads in a plain text file of notes on books and returns a tidy data frame.
#'
#' Designed for my own format of notes on books, including when I read them.
#' Won't work on anything that doesn't match that formatting.  Currently does
#' the bare minimum of creating sensible variables.  Doesn't prettify anything
#' yet.
#'
#' @param source_file path to the plain-text file to pull book notes from.
#'   Will only work if used on a file with the same layout as mine.
#'   Default NULL.
#' @param remove_mess a boolean that determines whether the initial
#'   messy-looking variables are removed so that the final data frame produced
#'   is properly tidied.  Default TRUE.
#' @keywords data munging, idiosyncratic,
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr lag
#' @importFrom dplyr lead
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom lubridate dmy
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
                            remove_mess = TRUE){

  # check for a source file ###################################################
  if (is.null(source_file)){
    stop("need to specify a source file")
  }
  # ###########################################################################

# ZZZ todo block ############################################################
# time how long the whole thing and each step takes.
# make a keywords dataframe bit (as an option? default) to split out keywords;
# this will need to happen after the remove_mess step, if any.
# add some reading notes data for illustrative purposes.
# ###########################################################################

  # ###########################################################################
  # read in the raw data as a vector, with each line a single entry in the
  # vector.  Bind this to another vector which will (eventually) hold the 
  # labels for the source data.  Result is a two-column (very long) tibble.
  len_read <- length(read_lines(source_file))

  my_reading <- tibble(
    label = rep("nope", len_read),
    raw_data = read_lines(source_file)
  )

  # add blank line as the first line so the function can find the first
  # author_year_title line.
  my_reading <- my_reading %>%
    add_row(label = "nope", raw_data = "", .before = 1)
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
  # not sure how useful this is.
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
    spread(label, raw_data) %>%
    select(-item)
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

  # clean the start_ and finish_ variables into dates #########################
  # tried to be clever earlier with gettig it to dynamically go through column
  # names.  If more start or finish lines are added for any books, this will
  # need updating.  This also probably suppresses incomplete dates, which isn't
  # a great outcome.
  quiet_dmy <- quietly(dmy)
  my_reading <- my_reading %>%
    mutate(
      start_1 = str_remove(start_1, "start:"),
      start_2 = str_remove(start_2, "start:"),
      finish_1= str_remove(finish_1, "finish:"),
      finish_2= str_remove(finish_2, "finish:")
    ) %>%
    mutate(
      start_1 = str_trim(start_1),
      start_2 = str_trim(start_2),
      finish_1= str_trim(finish_1),
      finish_2= str_trim(finish_2)
    ) %>%
    mutate(
      start_1 = quiet_dmy(start_1)$result,
      start_2 = quiet_dmy(start_2)$result,
      finish_1= quiet_dmy(finish_1)$result,
      finish_2= quiet_dmy(finish_2)$result
    )
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

  # if told to, select only the tidy variables ################################
  # discard all the messy ones we started with.
  if (remove_mess) {
    my_reading <- my_reading %>%
      select(
        author, year, title,
        place, publisher,
        keywords,
        start_1, finish_1,
        start_2, finish_2,
        notes
      )
  }
  # ###########################################################################

  # finally, return the data frame
  return(my_reading)
}
# #############################################################################
