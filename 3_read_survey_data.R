# 3_read_survey_data.R
# read the survey data from Qualtrics; 
# Export data from Qualtrics to Excel using text not numbers; add options of including randomised order and removing line breaks
# for sliders "respondents will need to move each slider bar at least slightly for the question to count as answered."
# November 2021
library(tidyr)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)
library(lubridate) # for time zone
source('99_functions.R')

## Section 1: read the data, exported from Qualtrics into Excel ##
# find the latest files
files = dir('data', pattern='^Career')
# loop through random and non-random
data = labels = NULL
for (file in c('random','non-random')){
  search = paste('\\-\\+', file, sep='')
  in_file = paste('data/', files[str_detect(string=files, pattern=search)], sep='')
  
  ## a) get the labels
  # get the variable names ...
  names = read_excel(in_file, col_names = FALSE, n_max = 1) %>% clean_names()
  names = janitor::make_clean_names(names)  
  if(file == 'random'){
    names[names == 'q9_4'] = 'q9_5'  # move comments question for random sample (to fit in email question)
  }
  # ... then get the labels
  in_labels = read_excel(in_file, col_names = FALSE, skip=1, n_max = 1)
  labs = as.character(matrix(in_labels))
  # make a frame of labels
  raw_labels = bind_cols(names=names, labels=labs) %>%
    filter(!str_detect(names, 'response_id'),
           !str_detect(names, 'user_language'),
           !str_detect(names, 'recipient'),
           !str_detect(names, 'policy'),
           !str_detect(names, 'sentiment')) 
  
  # b) get the raw data, using text
  raw = read_excel(in_file, col_names = FALSE, skip=2) %>%
    mutate(sample = CapStr(file)) # with first capital letter
  names(raw) = c(names, 'sample')
  ## get the number if they are from the random sample, used for matching elsewhere, e.g., 2_email_random_sample
  if(file == 'random'){
    raw = mutate(raw, id = as.numeric(str_remove_all(recipient_email, pattern='[^0-9]'))) # note this is a dummy email and I can just take number
  }
  
  # combine random and non-random samples
  data = bind_rows(data, raw)
  labels = bind_rows(labels, raw_labels)
}


# c) process the data
# remove variables not needed; additional stuff from Qualtrics
# removed q1_1 / q1_2 = consent, which has to be `yes`
data = select(data, -starts_with('recipient'), -status, -'ip_address', -'distribution_channel', -'external_reference', 
               -'user_language', -starts_with('location_'), -contains('sentiment'), -contains('policy'),
              -'q3_3_parent_topics', -'q3_3_topics', -'end_date', -'q1_1', -'q1_2') %>%
  mutate(ID = 1:n(), # simple number ID for matching below (also lowercase id)
         duration_mins = duration_in_seconds / 60,
         progress_cat = cut(progress, c(-0.001,5,50,75,100)), # progress percent as categories
         start_date = with_tz(start_date + 60*60*6, "Australia/Brisbane"), # change time zones (had to add six hours to get right time)
         start_date = as.Date(start_date), # do not need time
         q9_2 = ifelse(q9_2 == 'I use a different term (please specify)', 'I use a different term', q9_2), # shorten gender label
         ## remove carriage returns from text
         #
         q3_3 = str_replace_all(q3_3, "[\r\n]" , " "),
         q3_3 = str_squish(q3_3),
         #
         q4_3 = str_replace_all(q4_3, "[\r\n]" , " "),
         q4_3 = str_squish(q4_3),
         #
         q4_5 = str_replace_all(q4_5, "[\r\n]" , " "),
         q4_5 = str_squish(q4_5),
         #
         q4_7 = str_replace_all(q4_7, "[\r\n]" , " "),
         q4_7 = str_squish(q4_7),
         #
         q4_8 = str_replace_all(q4_8, "[\r\n]" , " "),
         q4_8 = str_squish(q4_8),
         #
         q6_2 = str_replace_all(q6_2, "[\r\n]" , " "),
         q6_2 = str_squish(q6_2),
         #
         q6_4 = str_replace_all(q6_4, "[\r\n]" , " "),
         q6_4 = str_squish(q6_4),
         #
         q6_6 = str_replace_all(q6_6, "[\r\n]" , " "),
         q6_6 = str_squish(q6_6),
         #
         q7_3 = str_replace_all(q7_3, "[\r\n]" , " "),
         q7_3 = str_squish(q7_3),
         #
         q8_2 = str_replace_all(q8_2, "[\r\n]" , " "),
         q8_2 = str_squish(q8_2),
         #
         q8_3 = str_replace_all(q8_3, "[\r\n]" , " "),
         q8_3 = str_squish(q8_3)
  )
# can delete first test answers (anything prior to Sunday 10 October)
data = filter(data,
              start_date >= as.Date('2021-10-10'))

## combine section 5 questions (participants were randomly allocated to one of four)
data = mutate(data,
              q5_label = case_when(
                hypothetical_nhmr_capplication_do == 'Q5.1' ~ 'Caring for an elderly relative',
                hypothetical_nhmr_capplication_do == 'Q5.2' ~ 'Caring for a child',
                hypothetical_nhmr_capplication_do == 'Q5.3' ~ 'A car accident',
                hypothetical_nhmr_capplication_do == 'Q5.4' ~ 'Severe depression'
              ),
              q5 = coalesce(q5_1, q5_2, q5_3, q5_4)) %>%
  select(-q5_1, -q5_2, -q5_3, -q5_4, -hypothetical_nhmr_capplication_do)

# make combined category for comfort
data = mutate(data, 
              comfort = case_when(
                q7_2 == 'Extremely comfortable' ~ 'Comfortable', 
                q7_2 == 'Somewhat comfortable' ~ 'Comfortable',
                q7_2 == 'Neither comfortable nor uncomfortable' ~ 'Neutral',
                q7_2 == 'Somewhat uncomfortable' ~ 'Uncomfortable',
                q7_2 == 'Extremely uncomfortable' ~ 'Uncomfortable'
              ))

### Process email using cipher (move a few from non-random to random)
source('99_cipher_not_share.R')

# remove people with zero progress ... 
cat('There were ', nrow(filter(data, progress==0)),' respondents with a zero progress.\n', sep='')
data = filter(data, progress > 0)
# ... and who only completed demographics
comment_questions = c('q3_3','q4_3','q4_5','q4_7','q4_8','q6_2','q6_4','q6_6','q7_3','q8_2','q8_3','q9_4','q9_5')
just_questions = select(data, 'ID', starts_with('q')) %>% # 
  select(-all_of(comment_questions), -q5_label) %>% # remove comments
  mutate_all(as.character) %>%
  pivot_longer(cols = -ID) %>%
  mutate(missing = is.na(value)) 
# overall missing
overall = group_by(just_questions, ID) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
# remove three demographic questions and then remove people if zero for all others
not_demographics = filter(just_questions, !str_detect(name, pattern='^q9')) %>%
  group_by(ID) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
to_exclude = unique(c(overall$ID, not_demographics$ID))
cat('There were ', length(to_exclude),' respondents who completed almost nothing\n', sep='')
data = filter(data, !ID %in% not_demographics$ID)

### labels ###

## export all categorical questions to create category ordering ##
for_export = export_levels(indata=data, inlabels=labels, questions = c('q2_2','q3_1','q3_2','q4_2','q7_2','q8_1','q9_1','q9_2','q9_3'))
write.csv(for_export, file='data/for_ordering.csv', row.names = FALSE)
# then read in ordering
ordering = read.csv('data/done_ordering.csv') %>%
  arrange(question, order)

# labels for generated variables
f1 = data.frame(names = 'duration_mins', labels='Duration (in minutes)')
f2 = data.frame(names = 'progress_cat', labels='Progress (%)')
f3 = data.frame(names = 'comfort', labels='Comfort reading career disruption sections')
f4 = data.frame(names = 'q5', labels='In this section, please consider the following hypothetical scenario and think about what you would write in the career disruption section. Imagine you had missed six months of full-time research in the previous year because of [randomised scenario]. You had no other personal or medical circumstances in the last five years. Would you write anything in the career disruption section about the six months spent away from research because of this issue?')
# just need one label
labels = bind_rows(labels, f1, f2, f3, f4) %>% 
  unique() %>%
  filter(!names %in% c('q1_1','status','ip_address','end_date','external_reference','location_latitude','location_longitude','distribution_channel'))

### NHMRC data ###
# from announcements (used data from latest year available); outcomes for competitive grants; recorded number of applicants
nhmrc = read.table(sep=',', header=TRUE, stringsAsFactors = FALSE, text='
year,type,var,applications
2020,Gender,Female,2339
2020,Gender,Male,2844
2020,Gender,Not Stated,38
2019,q9_1,Basic Science,2731
2019,q9_1,Clinical Medicine and Science,1856
2019,q9_1,Health Services Research,532
2019,q9_1,Public Health,691') %>%
  group_by(type) %>%
  mutate(p = prop.table(applications),
         p = p*100) %>% # make into percent
  ungroup()


### save ##
save(ordering, data, labels, nhmrc, file='data/AnalysisReady.RData')
