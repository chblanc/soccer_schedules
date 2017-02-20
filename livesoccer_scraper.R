#' ---
#' title: Soccer Schedule Scraper
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'  html_document:
#'    keep_md: true
#'    theme: lumen
#'    highlight: kate
#' ---

#+ message = FALSE, warning = FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(rvest)
library(stringr)
library(stringi)

#+ include=FALSE
# writeFiles = FALSE so that it doesn't rewrite our data
writeFiles <- FALSE

#+ echo = FALSE
# Create object with today's date
dateToday <- Sys.Date()

# how many days out do we want to search? 
lookForward <- 7

# read in the channel list, create unique list
channelList <- read_csv("misc/channellist_clean.csv")
channels <- unique(channelList$provider)

#' ## Getting Started
#' 
#' set the link to the website and define a series of dates for which we 
#' intend to grab schedules for.

# base link
baseLink <- "http://www.livesoccertv.com/schedules/"
dates <- seq.Date(dateToday, dateToday + lookForward, 1)


# homepage link
mainLink <- "http://www.livesoccertv.com/schedules/2017-02-25/"

# Read the html 
homepage <- read_html(mainLink)

# pull out the starting time for each game
startingTime <-  homepage %>%
  html_nodes(".ts") %>%
  html_text()

# pull out the channel where the game will be shown
tvChannel <- homepage %>%
  html_nodes('.timecell~ td+ td') %>%
  html_text()

# pull out the home and away team
game <- homepage %>%
  html_nodes(".timecell+ td") %>%
  html_text() %>%
  str_trim(side = 'both')

# put it together
schedule <- tibble(
  start_time = startingTime,
  matchup = game,
  showing_on = tvChannel
) %>%
  separate(matchup, c('home_team', 'away_team'), sep = 'vs')

#' Create a unique list of channels scraped from the website to get a sense
#' of whether they match up with the list provided by Kieran. 

# unique channels collected from livesoccer.com
onlineChannels <- lapply(tvChannel, function(x) strsplit(x, ',')) %>%
  flatten() %>%
  unlist() %>%
  str_trim(side = 'both') %>%
  unique()

# print both lists
sort(onlineChannels)
sort(channels)

#' Firstly, it appears as though the scraper is truncating the names of
#' some of the channels... no bueno.. in addition, there is some ambiguity
#' about the channels themselves. For examples, is *Azteca* the same
#' as *Azteca America*? Probably? But is there anyway to be sure?  
#' 
#' One possible solution is to cross-check the channel name with direct TVs
#' offical name of the channel (afterall, we do have the directtv channel)
#' 
#' 
#' another isssue is how to handle games which are labeled as 'repeats',
#' specifically: '*Repeat from Feb....'. This actually shouldn't be 
#' too difficult to pull out using regex.

#' ## write a function to collect schedules for multiple days
#' 

getHtml <- function(baseLink, dates) {
  Sys.sleep(round(runif(1, 1, 25)))
  linkList <- paste0(baseLink, dates, sep='/')
  htmlList <- map(linkList, read_html)

  return(htmlList)
}

getStartTime <- function(html) {
  
  startTime <- html %>%
    html_nodes(".ts") %>%
    html_text()
  return(startTime)
}

getTvChannel <- function(html) {
  
  tvChannel <- html %>%
    html_nodes('.timecell~ td+ td') %>%
    html_text()
  return(tvChannel)
}


getMatchup <- function(html) {
  
  matchup <- html %>%
    html_nodes(".timecell+ td") %>%
    html_text() %>%
    str_trim(side = 'both')
  return(matchup)
}

createSchedule <- function(startTime, matchUp, provider) {
  
  schedule <- tibble(
    start_time = startTime,
    matchup = matchUp,
    showing_on = provider
  ) %>%
    separate(matchup, c('home_team', 'away_team'), sep = 'vs')
  return(schedule)
}

#' Using PURRR
results <- tibble(schedule_date = dates,
       html=getHtml(baseLink, dates)) %>%
mutate(
  start = map(html, getStartTime),
  tv = map(html, getTvChannel),
  game = map(html, getMatchup),
  schedule = pmap(list(startTime=start, provider=tv,matchUp=game),createSchedule)
) %>%
  select(schedule_date, schedule) %>%
  unnest()

