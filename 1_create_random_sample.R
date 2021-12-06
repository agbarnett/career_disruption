# 1_create_random_sample.R
# create a random sample and send initial and reminder emails
# October 2021
library(dplyr)
library(stringr)
library(readxl)
library(TeachingDemos)
char2seed('andorra')

## Section 1: create a random sample
N_sample = 384 # from sample size calculation
load('data/emails.RData') # 0_find_authors_pubmed.R
sample = sample_n(selected, replace=FALSE, size=N_sample) %>%
  mutate(email = str_squish(email),
         name = str_squish(name),
         responded = FALSE, # set everyone as non-responders for starters
         name = gsub(x=name, pattern='Ã­', replacement='í', perl=TRUE), # change a few letters that were badly encoded
         name = gsub(x=name, pattern='Ã©', replacement='e', perl=TRUE),
         name = gsub(x=name, pattern='Ã¡', replacement='a', perl=TRUE),
         name = gsub(x=name, pattern='Ãª', replacement='e', perl=TRUE),
         name = gsub(x=name, pattern='Ã¶', replacement='o', perl=TRUE)) 
# to find non-ascii letters
filter(sample, str_detect(name, '[^ -~]'))
# add me at the top as test case
me = data.frame(pmid='9999', name='Adrian Barnett', email='a.barnett@qut.edu.au', n=1, responded=FALSE)
sample = bind_rows(me, sample) %>%
  mutate(id = 1:n()) # for linking
N_sample = N_sample + 1

## Section 2: add personal link generated from Qualtrics (see 0_create_contacts_qualtrics.R and https://www.qualtrics.com/support/survey-platform/distributions-module/email-distribution/personal-links/)
links = read.csv('data/links_from_qualtrics.csv') %>%
  select(Link) %>%
  mutate(id = 1:n()) # incorrect code originally as emails were not in order in qualtrics file
sample = left_join(sample, links, by='id')

# make a few small changes that were flagged by initial responders, not for sharing on github
source('99_email_not_on_github.R')

# save the sample 
save(sample, file='data/random_sample.RData')

