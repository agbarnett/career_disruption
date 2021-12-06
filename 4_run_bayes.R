# 4_run_bayes.R
# run bayesian model of career disruption times
# called by 4_survey_results.Rmd
# November 2021
library(R2WinBUGS)
bugs_location = "c:/Program Files/WinBUGS14" # location of the file WinBUGS14.exe

# if results already exist then don't re-run
if(length(dir('results', pattern='bayes_time_model')) == 1){
  load('results/bayes_time_model.RData')
}

if(length(dir('results', pattern='bayes_time_model')) == 0){
## prepare data
# add zero time for those who said no
for_model = dplyr::select(data, sample, q5_label, q5, q6_5_1) %>%
  filter(!is.na(q5_label),
         !is.na(q5)) %>%
  mutate(q6_5_1 = ifelse(q5=='No', 0, q6_5_1)) 
  
# a) with missing data
for_model_missing = mutate(for_model,
                           q6_5_1 = ifelse(is.na(q6_5_1) == TRUE, 6, q6_5_1) ) # if missing assume six months
# b) without missing data for slider
for_model = filter(for_model, !is.na(q6_5_1)) # remove missing
# create design matrix and data in Winbugs format
mat_missing = model.matrix(q6_5_1~ factor(sample) + factor(q5_label), data=for_model_missing)
mat = model.matrix(q6_5_1~ factor(sample) + factor(q5_label), data=for_model)
bdata_missing = list(N = nrow(mat_missing),
             time = for_model_missing$q6_5_1,
             sample = mat_missing[,2],
             scenario = mat_missing[,3:5])
bdata = list(N = nrow(mat),
             time = for_model$q6_5_1,
             sample = mat[,2],
             scenario = mat[,3:5])

# make winbugs model
bfile = 'time_model.txt'
bugs = file(bfile, 'w')
cat('model{
    for (i in 1:N){
      time[i] ~ dnorm(mu[i], tau)
      mu[i] <- beta[1] + beta[2]*sample[i] + beta[3]*scenario[i,1] + beta[4]*scenario[i,2] + beta[5]*scenario[i,3]
    }
    tau ~ dgamma(0.1, 0.1)
    for(k in 1:5){
      beta[k] ~ dnorm(0, 0.0001)
    }
    # predicted times for four scenarios
    pred[1] <- beta[1]
    for(k in 1:3){
      pred[k+1] <- beta[1] + beta[k+2]
    }
}', file=bugs)
close(bugs)

# MCMC parameters
MCMC = 5000
thin = 5
n.chains = 2

# initial values
inits = list(beta = c(mean(bdata$time),rep(0,4)), tau=1)  # sensible starting value for intercept
inits = rep(list(inits), n.chains) # repeat initial values per chain

# run winbugs
parameters = c('beta','tau','pred')
# a) ignoring missing
bugs.res = bugs(data=bdata, inits=inits, parameters=parameters, model.file=bfile, DIC=FALSE,
                    n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                    bugs.directory=bugs_location)
# b) imputing missing
bugs.res.missing = bugs(data=bdata_missing, inits=inits, parameters=parameters, model.file=bfile, DIC=FALSE,
                n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=87947, debug=FALSE,
                bugs.directory=bugs_location)

## calculate ranks for prediction
# a) ignoring missing
names = colnames(bugs.res$sims.matrix)
index = str_detect(pattern='pred', names)
res = bugs.res$sims.matrix[, index]
# maximum
which_row = res >= apply(res, 1, max, na.rm = TRUE) # which prediction is the max
was_max = colSums(which_row)
was_max = was_max / (MCMC*2) # change into probability
# minimum
which_row = res <= apply(res, 1, min, na.rm = TRUE) # which prediction is the min
was_min = colSums(which_row)
was_min = was_min / (MCMC*2) # change into probability
min_max = data.frame(scenario=1:4, min=was_min, max=was_max)
# b) imputing missing
names = colnames(bugs.res.missing$sims.matrix)
index = str_detect(pattern='pred', names)
res = bugs.res.missing$sims.matrix[, index]
# maximum
which_row = res >= apply(res, 1, max, na.rm = TRUE) # which prediction is the max
was_max = colSums(which_row)
was_max = was_max / (MCMC*2) # change into probability
# minimum
which_row = res <= apply(res, 1, min, na.rm = TRUE) # which prediction is the min
was_min = colSums(which_row)
was_min = was_min / (MCMC*2) # change into probability
min_max_missing = data.frame(scenario=1:4, min=was_min, max=was_max)

## stats
# a) ignoring missing
stats = data.frame(bugs.res$summary[str_detect(names, pattern='pred'), c(1,3,7)]) %>%
  clean_names() %>%
  mutate(scenario = 1:4) %>%
  left_join(min_max, by='scenario')
# b) imputing missing
stats_missing = data.frame(bugs.res.missing$summary[str_detect(names, pattern='pred'), c(1,3,7)]) %>%
  clean_names() %>%
  mutate(scenario = 1:4) %>%
  left_join(min_max_missing, by='scenario')
# combine
overall = bind_rows(stats, stats_missing, .id='missing')

save(overall, file='results/bayes_time_model.RData')
} # end of dir if