library(tidyverse)
library(gsheet)
source("utils/loadrawdata.R")
options("digits.secs"=6)

# Load All data from the following directory
M <- LoadFromDirectory("data_20231127", event = NULL, sample = NULL)

# todo: transition over to more memory-efficient data processing.

# Save Dataset as RDA
save(D, file = 'data_all_raw.rda', compress=TRUE)

####
# Meta Inspection
####

M = M %>% rename(Participant = i1,
                 Study = i2)

M = M %>% mutate(Participant = as.numeric(Participant))

# Filter out participant 0, who is just a test round.
excluded_participants = c(0)
M = M %>% filter(!Participant %in% excluded_participants)

# order as specified by actual treatment programs that ran (fb study)
M %>% filter(Study == "feedback") %>% group_by(SessionProgram) %>% summarize(
  Participants = paste(Participant, collapse=","),
  n = n()
)

# order as specified by actual treatment programs that ran (signifier study)
M %>% filter(Study == "signifier") %>% group_by(SessionProgram) %>% summarize(
  Participants = paste(Participant, collapse=","),
  n = n()
)


####
# Format columns
####

D = D %>% rename(Participant = i1,
                 Study = i2)

Sf <- Sf %>% left_join()

#writeLines(colnames(D), "colnames.txt")
col_formats = read.csv("wam_column.csv", sep=";")



D = D %>% 
  mutate_at(col_formats %>% pull(name), 
            ~ifelse(.x == "NULL", NA, .x)) %>%
  mutate_at(col_formats %>% filter(type=="numeric") %>% pull(name), 
            ~as.numeric(.x)) %>%
  mutate_at(col_formats %>% filter(type=="int") %>% pull(name), 
            ~as.integer(.x)) %>%
  mutate_at(col_formats %>% filter(type=="time") %>% pull(name), 
            ~as.POSIXct(.x, format = "%Y-%m-%d %H:%M:%OS")) 


####
# Divide into Signifier and Feedback Datasets
####

Df = D %>% filter(Study == "feedback")
Ds = D %>% filter(Study == "signifier")

####
# Load Google Sheets Data: Signifier
####
L2 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zIO96Miqkcs8eVEhIOl4ZNAv9UEz6eLxXjbFwD0R_rY/edit#gid=1570103017')

L2 %>% select(Condition, Comment, Participant) %>% filter(Participant < 25) %>%
  group_by(Participant,Condition) %>%
  summarize(
    order = paste(Comment, collapse=',')
  ) %>% group_by(Condition, order) %>%
  summarize(
    n = n()
  )

####
# Load Google Sheets Data: Feedback
####

L <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zIO96Miqkcs8eVEhIOl4ZNAv9UEz6eLxXjbFwD0R_rY/edit#gid=1857813124')

# order as specified by google sheet, through condition/algo
L %>% select(Condition, Algorithm, Participant) %>% filter(Participant < 14) %>%
  group_by(Participant,Condition) %>%
  summarize(
    order = paste(Algorithm, collapse=',')
  ) %>% group_by(Condition, order) %>%
  summarize(
    n = n()
  )

# order as specified by google sheet, through experience ID
L %>% select(Condition, `Experience ID (Algorithms)`, Participant) %>% filter(Participant < 20) %>%
  group_by(Participant,Condition) %>%
  summarize(
    order = paste(`Experience ID (Algorithms)`, collapse=',')
  ) %>% group_by(Condition, order) %>%
  summarize(
    n = n()
  )

# Mutate Easiest/Hardest
L = L %>% mutate(Easiest = ifelse(Easiest == "Yes",1,0),
                 Easiest = ifelse(is.na(Easiest),0,Easiest),
                 Hardest = ifelse(Hardest == "Yes",1,0),
                 Hardest = ifelse(is.na(Hardest),0,Hardest))

# Create Normalized versions of pacing, how much help, liked help

L = L %>% mutate(HowMuchHelpNormalized = `“How much did you feel the game helped you?”`,
                 HowMuchHelpNormalized = ((HowMuchHelpNormalized-1)/6),
                 LikedHelpNormalized = `“I liked how the game helped me.”`,
                 LikedHelpNormalized = ((LikedHelpNormalized-1)/6),
                 PacingNormalized = `"I felt the pacing of the game was"`,
                 PacingNormalized = ((PacingNormalized-1)/6),
                 IrritationNormalized = `"How irritated did you feel in this condition?"`,
                 IrritationNormalized = ((IrritationNormalized-1)/6))



####
# Save Final Data
####
#save(D, file = 'data_all.rda', compress=TRUE)
save(Df, file = 'data_feedback.rda', compress=TRUE)
save(Ds, file = 'data_signifier.rda', compress=TRUE)
