library(RecordLinkage)
library(tidyverse)
library(stringdist)
library(here)
library(data.table)
suppressMessages(library(furrr))
library(microbenchmark)
here <- here()

plan(multisession, workers = 8)


# Load a list of files
xml_files <- list.files("./data", pattern = "*.xml", full.names = TRUE, recursive = T)

target_strings <- c("You're a liar", "You're lying", "I've never met such a big liar
in my life", "You are trying to lie your way out of
trouble again", "You're a pathological liar", "liar, liar, pants on fire!")

# Extract the text from the XML files (proof of concept for now...)
xml_files[1] %>%
    xml2::read_xml() %>%
    xml2::xml_text() %>%
    as.data.frame() %>%
    head()


# Try the string









# Set up main functions
getStringMatches <- function(file_text, target_string) {
    # target_string <- target_strings[1]

    res <- stringdist::afind(file_text, target_string, window = nchar(target_string), method = "running_cosine")
    location <- res$location
    distance <- res$distance
    match <- res$match
    context <- substr(file_text, as.integer(location) - 70, as.integer(location) + 70)
    res2 <- as.data.table(cbind(target_string, location, distance, match, context))
    return(res2)
}

getFullMatch <- function(file, target_strings) {
    # file <- xml_files[1]
    # file_text <- fread(file, sep = NULL, header = FALSE)
    file_text <- xml2::read_xml(file) %>%
        xml2::xml_text()

    res <- rbindlist(future_map(target_strings, ~ getStringMatches(file_text, .x)))
    res <- cbind(file, res)
    names(res) <- c("file", "target_string", "string_location", "string_distance", "matching_string", "context")
    return(res)
}

possibly_getFullMatch <- possibly(getFullMatch, otherwise = NA)
options(datatable.prettyprint.char = 20000L)

microbenchmark(
    all_res <- future_map_dfr(xml_files[1:100], ~ possibly_getFullMatch(.x, target_strings)),
    all_res <- map_df(xml_files[1:100], ~ possibly_getFullMatch(.x, target_strings)),
    times = 1
)
