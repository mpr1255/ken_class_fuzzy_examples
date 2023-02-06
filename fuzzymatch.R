library(RecordLinkage)
library(tidyverse)
library(stringdist)
library(here)
library(data.table)
suppressMessages(library(furrr))
library(microbenchmark)
here <- here()

plan(multisession, workers = 8)
# plan(multisession, workers = 1, gc = TRUE)
# plan()
# furrr::workers()
# furrr::furrr_options()


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

getFullText <- function(file) {
    file %>%
        xml2::read_xml() %>%
        xml2::xml_text()
}
# What happens if we try one string on one file?
stringdist::afind(target_strings[1], getFullText(xml_files[1]), window = nchar(target_strings[1]), method = "running_cosine")

# What happens if we try one string on all files? First, try a for loop
forLoop <- function(f_length) {
    for (i in 1:f_length) {
        file_text <- getFullText(xml_files[i])
        res <- stringdist::afind(file_text, target_strings[1], window = nchar(target_strings[1]), method = "running_cosine")
        location <- res$location
        distance <- res$distance
        match <- res$match
        context <- substr(file_text, as.integer(location) - 70, as.integer(location) + 70)
        res2 <- as.data.table(cbind(target_strings[1], location, distance, match, context))
        res2 <- cbind(xml_files[i], res2)
        names(res2) <- c("file", "target_string", "string_location", "string_distance", "matching_string", "context")
        if (i == 1) {
            all_res <- res2
        } else {
            all_res <- rbind(all_res, res2)
        }
    }
}

# But we need to run it on all strings, so let's try a nested for loop
nestedForLoop <- function(f_length) {
    for (i in 1:f_length) {
        file_text <- getFullText(xml_files[i])
        for (j in 1:length(target_strings)) {
            res <- stringdist::afind(file_text, target_strings[j], window = nchar(target_strings[j]), method = "running_cosine")
            location <- res$location
            distance <- res$distance
            match <- res$match
            context <- substr(file_text, as.integer(location) - 70, as.integer(location) + 70)
            res2 <- as.data.table(cbind(target_strings[j], location, distance, match, context))
            res2 <- cbind(xml_files[i], res2)
            names(res2) <- c("file", "target_string", "string_location", "string_distance", "matching_string", "context")
            if (i == 1 & j == 1) {
                all_res <- res2
            } else {
                all_res <- rbind(all_res, res2)
            }
        }
    }
    return(all_res)
}

# # Do the benchmarks
# microbenchmark(
#     forLoop(1),
#     nestedForLoop(1),
#     forLoop(10),
#     nfl_res <- nestedForLoop(10),
#     forLoop(length(xml_files)),
#     nfl_res <- nestedForLoop(length(xml_files)),
#     times = 1
# )


# Now let's try vectorise it.

# Set up main functions
getStringMatches <- function(file, target_string) {
    # target_string <- target_strings[1]
    file_text <- getFullText(file)
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

    res <- rbindlist(map(target_strings, ~ getStringMatches(file, .x)))
    res <- cbind(file, res)
    names(res) <- c("file", "target_string", "string_location", "string_distance", "matching_string", "context")
    return(res)
}

possibly_getFullMatch <- possibly(getFullMatch, otherwise = NA)
# options(datatable.prettyprint.char = 20000L)

# Now do the same benchmarks as the for loops
microbenchmark(
    null <- map_df(xml_files[1], ~ possibly_getFullMatch(.x, target_strings[1])),
    null <- future_map_dfr(xml_files[1], ~ possibly_getFullMatch(.x, target_strings[1])),
    null <- map_df(xml_files[1:10], ~ possibly_getFullMatch(.x, target_strings[1])),
    null <- future_map_dfr(xml_files[1:10], ~ possibly_getFullMatch(.x, target_strings)),
    null <- map_df(xml_files, ~ possibly_getFullMatch(.x, target_strings)),
    null <- future_map_dfr(xml_files, ~ possibly_getFullMatch(.x, target_strings)),
    times = 1
)

# These results indicate that it's actually only 3x faster to use the vectorised
# approach than using nested for loops. But what happens when you significantly
# increase the number of strings & files?

# But at this size, a single threaded functionalised approach is roughly the
# same as the nested for loop
# microbenchmark(
#     null <- map_dfr(xml_files, ~ possibly_getFullMatch(.x, target_strings)),
#     nfl_res <- nestedForLoop(length(xml_files)),
#     times = 1
# )

# This is how many items there were to iterate in the first go (cross2
# does the combinatorics for files/strings from two vectors)
cross2(target_strings, xml_files) %>%
    rbindlist() %>%
    nrow()

# Now let's try it with 5x the number of strings & files
target_strings2 <- rep(target_strings, 4)
xml_files2 <- rep(xml_files, 4)

# The number of items to iterate over is now many times greater
cross2(target_strings2, xml_files2) %>%
    rbindlist() %>%
    nrow()

# Update the for loop to use these expanded variables
nestedForLoop2 <- function(f_length) {
    # f_length <- length(xml_files2)
    for (i in 1:f_length) {
        file_text <- getFullText(xml_files2[i])
        for (j in 1:length(target_strings2)) {
            res <- stringdist::afind(file_text, target_strings2[j], window = nchar(target_strings2[j]), method = "running_cosine")
            location <- res$location
            distance <- res$distance
            match <- res$match
            context <- substr(file_text, as.integer(location) - 70, as.integer(location) + 70)
            res2 <- as.data.table(cbind(target_strings2[j], location, distance, match, context))
            res2 <- cbind(xml_files2[i], res2)
            names(res2) <- c("file", "target_string", "string_location", "string_distance", "matching_string", "context")
            if (i == 1 & j == 1) {
                all_res <- res2
            } else {
                all_res <- rbind(all_res, res2)
            }
        }
    }
    return(all_res)
}

microbenchmark(
    null <- future_map_dfr(xml_files2, ~ possibly_getFullMatch(.x, target_strings2)),
    nfl_res <- nestedForLoop2(length(xml_files2)),
    times = 1
)

# Notice how much simpler it was to update the functional approach? We only had
# to change variables in TWO places (xml_files2 & target_strings2) whereas with
# the for loop, we had to make alterations in a bunch of places. It's a lot
# easier to mess things up that way.