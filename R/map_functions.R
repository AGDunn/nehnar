#' Make Map with Some Areas Obscured
#'
#' This function will return a map of a country with
#' only certain regions visible, based on supplying
#' it a list of which regions should be shown.
#'
#' Originally, this function was to show the places we'd visit within a
#' particular country, at the level of the administrative subdivision.  The
#' terminology reflects this.  Before running this function, you'll need a
#' dataframe that has a variable of ISO_3166_A2 codes for regions.  The
#' function will grab the map data based on the country you ask for but in the 
#' next version it'll accept an existing sf shape instead.  Can return
#' `nation'-level of detail for UK but will retain their internal borders and
#' make a nonsense of things if more than one country is included in the map.
#' Uses plotly in the return of the map to allow tooltips and an easier-reading
#' map.
#' 
#' @param visited_places a vector of ISO_3166 values (2-letter country code
#'   followed by a dash then a 3-letter region code).  Default NULL.
#' @param countries a vector of 2-letter ISO codes that will be passed to
#'   ne_states().  Default, "everywhere", will try to generate the list of
#'   2-letter ISO codes based on the vector passed to visited_places.
#' @param show_unvisited boolean; show unvisited places instead of visited ones
#'   default FALSE.
#' @param group_London boolean; unite all London boroughs and the City into one
#'   map unit called `London'.  Default TRUE.
#' @param add_legend boolean; include a map legend. Default TRUE.
#' @param uk_countries boolean; display the four main subregions of the UK
#'   (England, Scotland, Northern Ireland and Wales).  Default FALSE.
#' @param just_London boolean; produce a zoomed-in map of the UK capital.  If
#'   true, overrides uk_countries and group_London, setting both to FALSE.
#'   Default FALSE.
#' @keywords ISO, maps, sf
#' @import sf
#' @import rgeos
#' @import ggplot2
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom plotly ggplotly
#' @importFrom rnaturalearth ne_states
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom stringr str_to_lower
#' @export
map_visited_regions <- function(visited_places = NULL,
                                countries = "everywhere", 
                                show_unvisited = FALSE, group_London = TRUE,
                                add_legend = TRUE, uk_countries = FALSE,
                                just_London = FALSE){

# ZZZ: stuff to do block ------------------------------------------------------
  # add warnings block for silly argument values
  # include ref to https://en.wikipedia.org/wiki/ISO_3166-2
  # add something to tie presence/absence of borders to uk_countries?
  # the show_unvisited variable should influence plot title.
  # if some of the 2-letter codes aren't in the data it will still run without
  #   complaint as long as at least one is.
  # add method to auto-choose countries if countries = NULL.
  # add option to use actual country names instead of 2-letter ISO codes.
  #   e.g. difference between `country' and `geounit' args in ne_states() is
  #   the former will give you all the colonies but the latter just mainland
  #   France.
  # add language-of-labels choice?
# -----------------------------------------------------------------------------

  # convert ISO column from factor to character if necessary
  if(is.factor(visited_places)){
    visited_places <- as.character(visited_places)
  }

  # strip out any whitespace in the ISO column
  visited_places <- str_replace_all(visited_places, " ", "")

  # take the vector of places which have been visited
  # this step is from when visited_places was a vector, so it's not really
  # needed now---unless we're going to make the show-or-don't-show decision
  # this way.
  show_these <- visited_places 

  # try to guess countries list if none explicitly supplied
  if (countries == "everywhere") {
    countries <- unique(tolower(
      str_split(visited_places, pattern = "-", simplify = TRUE)[, 1]
    ))
  }

  # get sf file of map information for country
  country_sf <- ne_states(
    iso_a2 = countries,
    returnclass = "sf")
  
  # check if London-only map requested.  If not, make bounding box for map.  If
  # so, make London-based bounding box and set both group_London and
  # uk_countries to FALSE.
  
  if(!just_London){
    full_bbox <- st_bbox(country_sf)
  } else {
    group_London = FALSE
    uk_countries = FALSE
    full_bbox <- st_bbox(country_sf %>%
      group_by(name) %>%
      filter(
          type == "City Corporation" |
          type == "London Borough" |
          type == "London Borough (royal)" |
          type == "London Borough (city)" 
      )
    )
  }
  
  
  # based on uk_countries, determine which variable will be copied into
  # check_inclusion, which will be used to filter map contents.  Also create
  # names and title for legend to match uk_countries option.  
  if(!uk_countries){
    if(group_London){
      country_sf <- country_sf %>%
        mutate(name = case_when(
          type == "City Corporation" ~ "London",
          type == "London Borough" ~ "London",
          type == "London Borough (royal)" ~ "London",
          type == "London Borough (city)" ~ "London",
          TRUE ~ as.character(name)
        ))
    }
    country_sf <- country_sf %>%
      group_by(name) %>%
      mutate(check_inclusion = iso_3166_2, name_of_level = name)
      leg_title <- ""
    if(just_London){
      country_sf <- country_sf %>%
        filter(
            type == "City Corporation" |
            type == "London Borough" |
            type == "London Borough (royal)" |
            type == "London Borough (city)" 
        )
    }
  } else {
    show_these <- str_replace_all(show_these, "GB-", "")
    country_sf <- country_sf %>%
      group_by(name) %>%
      mutate(check_inclusion = gu_a3, name_of_level = geonunit)
      leg_title <- "UK countries and province"
  } 


  # check whether show_unvisited == TRUE; will then exclude either the visited
  # or the unvisited regions from the map.  Does this by comparison of
  # check_inclusion against show_these.
  if(!show_unvisited){
    country_sf <- country_sf %>%
      filter(check_inclusion %in% show_these)
  } else {
    country_sf <- country_sf %>%
      filter(!check_inclusion %in% show_these)
  } 

  # make the map, with colours; check whether to add legend
  if(add_legend){
    my_map <- ggplotly(country_sf %>%
        ggplot() + 
          geom_sf(aes(fill = name_of_level, colour = name_of_level)) +
          coord_sf(
            xlim = c(full_bbox[1], full_bbox[3]),
            ylim = c(full_bbox[2], full_bbox[4])) +
          theme_bw() +
          scale_fill_discrete(name = leg_title) +
          scale_colour_discrete(guide = FALSE)
      )
  } else {
    my_map <- ggplotly(country_sf %>%
        ggplot() + 
          geom_sf(aes(fill = name_of_level, colour = name_of_level), 
            show.legend = FALSE) +
          coord_sf(
            xlim = c(full_bbox[1], full_bbox[3]),
            ylim = c(full_bbox[2], full_bbox[4])) +
          theme_bw()
      )
  }

  return(my_map)
}
