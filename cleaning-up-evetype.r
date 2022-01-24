Sys.setlocale(category = "LC_TIME", locale="en_US.UTF-8")
Sys.setlocale(category = "LC_MONETARY", locale="en_US.UTF-8")

library(R.utils)
library(dplyr)

# loading the data

data_file_path <- "repdata_data_StormData.csv.bz2"

raw_file <- bzfile(data_file_path)
## note this step can take some time
raw_data <- read.csv(raw_file, na.strings = "?")

dim(raw_data)
names(raw_data)

event_data <- select(raw_data, EVTYPE)

# basic preprocessing

## lowercase
event_data <- mutate(event_data, EVTYPE = tolower(EVTYPE))

## removing digits and punctuation
event_data <- mutate(event_data, EVTYPE = gsub("[[:digit:][:blank:][:punct:]+]", " ", EVTYPE))

## removing double spaces
event_data <- mutate(event_data, EVTYPE = gsub("\\s+", " ", EVTYPE))

## trim
event_data <- mutate(event_data, EVTYPE = gsub("^\\s+|\\s+$", "", EVTYPE))

## removing rows with empty EVTYPE
event_data <- subset(event_data, !is.na(raw_data$EVTYPE))

## cleaning up
rm(raw_file)
rm(raw_data)

# evaluating probability table

event_types_probability_df <- as.data.frame(table(event_data$EVTYPE))
event_types_probability_df <- arrange(event_types_probability_df, desc(Freq))
sum_of_freq <- sum(event_types_probability_df$Freq)
event_types_probability_df <- mutate(event_types_probability_df, probability = Freq/sum_of_freq)
dim(event_types_probability_df)

tail(event_types_probability_df[grep("thun", event_types_probability_df$Var1), ], 10)
head(event_types_probability_df[grep("thun", event_types_probability_df$Var1), ], 10)

event_types_probability_df <- mutate(event_types_probability_df, acc_sum = cumsum(probability))
event_types_probability_df <- event_types_probability_df[event_types_probability_df$acc_sum < .99,]

# defining a distance formula usinb both Levenshtein distance and frequency

distance <- function(query_string, event_class, event_class_probability) {
    event_class <- as.character(event_class)
    probability <- as.numeric(event_class_probability)

    distance <- adist(query_string, event_class)
    size1 <- nchar(query_string)
    size2 <- nchar(event_class)
    param.size <- max(size1, size2)
    result <- exp(distance / param.size) * exp(-sqrt(event_class_probability))
    return(result)
}

# relabeling dataset

threshold_other <- exp(.25)

key <- character()
value <- numeric()
cache <- data.frame(key, value, stringsAsFactors=FALSE)

find_new_event_type <- function(event) {
    event <- as.character(event)
    result <- event

    cached_value <- cache[cache$key==event,]
        
    if(nrow(cached_value) == 0) {

        distances <- unlist(
                mapply(
                    function(x, y) 
                        distance(event, x, y), 
                        event_types_probability_df$Var1, 
                        event_types_probability_df$probability
                )
            )

        if(length(distances) > 0) {
            min_distance <- min(distances)
            if(min_distance < threshold_other) {
                min_distance_index <- which.min(distances)
                result <- as.character(event_types_probability_df$Var1[min_distance_index])
            } else {
                result <- "other"
            }
        }
        cache[nrow(cache) + 1, ] <<- c(event, result)
    } else {
        result <- cached_value[1,]$value
    }

    return(result)
}

event_data$new_evetype <- unlist(lapply(event_data$EVTYPE, 
                                           function(x) find_new_event_type(x)))

# Check results

## total of registers actually relabeled
nrow(event_data[event_data$EVTYPE != event_data$new_evetype, ])

## number of unique new labels / clusters
unique_new_evetype < unique(event_data$new_evetype)
length(unique_new_evetype)
print(unique_new_evetype)

## cluster "thunderstorm wind"
length(unique(event_data[event_data$new_evetype == "thunderstorm wind", ]$EVTYPE))
head(sort(unique(event_data[event_data$new_evetype == "thunderstorm wind", ]$EVTYPE)), 20)

## cluster "tornado"
length(unique(event_data[event_data$new_evetype == "tornado", ]$EVTYPE))
head(sort(unique(event_data[event_data$new_evetype == "tornado", ]$EVTYPE)), 10)
