library(tidyverse)
library(pbapply)
library(data.table)

# probability of recount? -------------------------------------------------
sims <- read_csv('data/electoral_college_simulations.csv')

# percent of time that close states are decided by less than 0.5%
sims %>%
  mutate(recount = ifelse(abs(FL-0.5)<0.005 | 
                            abs(PA-0.5)<0.005 | 
                            abs(WI-0.5)<0.005 | 
                            abs(MI-0.5)<0.005 | 
                            abs(AZ-0.5)<0.005, 
                          1,0)) %>%
  summarise(recount_pct = sum(recount)/nrow(.))

# percent of time that the decisive state is decided by less than 0.5%
sims <- sims %>%
  gather(state,dem_vote,4:ncol(.)) %>%
  left_join(read_csv('data/state_evs.csv'))

## order the simulations by cumulative EV for winner
tp <- pblapply(1:max(sims$draw),
               cl = 12,
               function(x){
                 if(isTRUE(sims[sims$draw==x,]$dem_ev>=270)){
                   sims[sims$draw==x,] %>%
                     arrange(desc(dem_vote)) %>% 
                     mutate(cum_ev = cumsum(ev))
                 }else{
                   sims[sims$draw==x,] %>%
                     arrange(dem_vote) %>% 
                     mutate(cum_ev = cumsum(ev)) 
                 }
                 
               }) %>% 
  bind_rows

## percentage of sims where states that could flip the EC  are decided by less than 0.5%
recount_threshold <- tp %>%
  mutate(counterfactual = ifelse(dem_vote>0.5,dem_ev-ev,dem_ev+ev)) %>%
  select(draw,state,dem_vote,ev,dem_ev,counterfactual) %>%
  mutate(flips_outcome = ifelse((dem_ev>=270) != (counterfactual>=270),TRUE,FALSE)) %>%
  filter(flips_outcome) %>%
  mutate(in_recount_territory = ifelse(abs(dem_vote-0.5) < 0.005,TRUE,FALSE)) %>%
  filter(in_recount_territory) 

recount_threshold %>% pull(draw) %>% unique %>% length() / max(tp$draw)


# simulate mail-in ballot effects -----------------------------------------
# https://twitter.com/jon_m_rob/status/1289185824127569920/photo/1
# avg rejection in worst states is 4%, sd of 4
# others is closer to 2%, sd 1%

# pew: mail is 58-17 Biden, or 0.77
# https://www.pewresearch.org/politics/2020/08/13/views-of-the-2020-campaign-and-voting-in-november/pp_2020-08-13_voter-attitudes_3-05/

wasserman_rejections = function(by_mail=.39, # pct cast by mail
                                rejection=.04, # mail rejection rate
                                total_votes=120000000, # total votes in jurisdiction
                                dem_pct=.77, # pct of mail votes going to dem
                                pre_d_2way=.55){ # pct of all votes going to dem
  
  rejections = (((total_votes*by_mail)*rejection))
  
  pre_rej_vote = c(total_votes*pre_d_2way,total_votes*(1-pre_d_2way))
  
  post_rej_vote = c((total_votes*pre_d_2way) - ((rejections * dem_pct)),
                    (total_votes*(1-pre_d_2way)) - ((rejections * (1-dem_pct))))
  
  list(pre_rej_vote,
       pre_rej_vote/sum(pre_rej_vote),
       post_rej_vote,
       post_rej_vote/sum(post_rej_vote),
       c(((post_rej_vote/sum(post_rej_vote)) - (pre_rej_vote/sum(pre_rej_vote))))[1]
  )
}

wasserman_rejections()

# simulate a bunch of really bad mail dem deficits 
n_sims <- 1e05

# generate possible means for rejection
# national (unweighted) average is 2.9 in 2018

# generate individual draws
rejection_grid = tibble(by_mail = rnorm(n_sims,0.39,0.05),
                        rejection = rgamma(1e05, shape=2, rate=50),
                        pre_d_2way = rnorm(n_sims,mean(tp$natl_pop_vote),sd(tp$natl_pop_vote)),
                        dem_pct = pmin(rnorm(n_sims,mean(tp$natl_pop_vote),sd(tp$natl_pop_vote)) + rnorm(n_sims,0.25,0.05),1)
       )


# view
hist(rejection_grid$rejection,breaks=1000,xlim = c(0,0.2))
mean(rejection_grid$rejection)
median(rejection_grid$rejection)
sd(rejection_grid$rejection)
mean(rejection_grid$rejection>0.1) ; mean(rejection_grid$rejection>0.1)*51

hist(rejection_grid$by_mail)
hist(rejection_grid$pre_d_2way)
hist(rejection_grid$dem_pct)

# generate impact on d vote given all these parameters
rejection_simulations <- 
  pblapply(1:nrow(rejection_grid),
           cl=12,
           function(x){
             wasserman_rejections(by_mail = rejection_grid[x,]$by_mail,
                                  rejection = rejection_grid[x,]$rejection,
                                  pre_d_2way = rejection_grid[x,]$pre_d_2way,
                                  dem_pct = rejection_grid[x,]$dem_pct,
                                         )[[5]]})

rejection_simulations <- rejection_simulations %>% do.call('c',.)

hist(rejection_simulations,breaks=100)
mean(rejection_simulations)
median(rejection_simulations)

sum(rejection_simulations < -0.02) / length(rejection_simulations)
sum(rejection_simulations < -0.01) / length(rejection_simulations)
sum(rejection_simulations < -0.005) / length(rejection_simulations)

hist(rejection_grid$rejection,breaks=100)
hist(rejection_simulations,breaks=100)
plot(rejection_grid$rejection,rejection_simulations)

# add state-specific multipliers
# shirnk using james stein estimator
js_est0 <- function(y, y_bar, narm = TRUE){
  # from https://bookdown.org/content/922/james-stein.html
  # formula is est = y_bar + [1 - ( ((n-3)*sigma2) / (sum(y - ybar)^2) )] * (y - y_bar)
  n <- length(na.omit(y)) # number of observations
  
  sigma2 <- y*(1-y)/n
  
  l2y <- sum( (y-y_bar)^2, na.rm = narm)
  
  est <- y_bar + ( (1 - ( ((n - 3) * sigma2) / l2y) )  * (y - y_bar) )
  
  return(est)
  
}

z <- c(0.3, 0.35, 0.25, 0.3, 0.2, 0.32, 0.7)

js_est0(y = z, y_bar = mean(z))


mail_rejection <- read_csv('data/state_mail_rejection.csv') %>% 
  select(state,relative_national_wtd_avg) 
  
mail_rejection$relative_national_wtd_avg <- 
           js_est0(y = mail_rejection$relative_national_wtd_avg,  
                   y_bar = mean(mail_rejection$relative_national_wtd_avg))

tp <- tp %>%
  left_join(mail_rejection)

# pass those dem deficits to the actual electoral simulations with a function
calc_recount_pct_after_mail <- function(simulated_rejection_effect){
  tp %>%
    group_by(draw) %>%
    
    # multiply rejection effect based on state
    mutate(state_specific_rejection_effect = simulated_rejection_effect * relative_national_wtd_avg) %>%
    
    mutate(dem_vote_vbm_shift = dem_vote + simulated_rejection_effect,
           dem_ev_vbm_shift = sum((dem_vote_vbm_shift>0.5)*ev)) %>%
    
    ungroup() %>%
    
    mutate(counterfactual = ifelse(dem_vote>0.5, dem_ev-ev, dem_ev+ev),
           counterfactual2 = ifelse(dem_vote_vbm_shift>0.5, dem_ev_vbm_shift-ev, dem_ev_vbm_shift+ev)) %>%
    
    mutate(flips_outcome = ifelse((dem_ev>=270) != (counterfactual>=270) |
                                       (dem_ev>=270) != (counterfactual2>=270),
                                     TRUE,FALSE)) %>%
    
    filter(flips_outcome == TRUE) %>%
    
    mutate(in_recount_territory = ifelse(abs(dem_vote-0.5) < (0.005) | 
                                           abs((dem_vote + simulated_rejection_effect)-0.5) < (0.005),
                                         TRUE,FALSE)) %>%
    
    filter(in_recount_territory == TRUE) %>% 
    
    pull(draw) %>% unique %>% length() / length(unique(tp$draw))
}

calc_recount_pct_after_mail(0)

calc_recount_pct_after_mail(mean(rejection_simulations))

# apply that function to every possibility
recount_probability_vbm <- pblapply(sample(rejection_simulations,10000),
                                    cl=10,
                                    function(x){calc_recount_pct_after_mail(x)})

recount_probability_vbm <- recount_probability_vbm %>% do.call('c',.)

hist(recount_probability_vbm,breaks=100)
mean(recount_probability_vbm)
median(recount_probability_vbm)

# save data
tibble('Probability of a recount in at least one decisive state' = recount_probability_vbm,
       'Potential decrease in Democratic vote margin\nfrom issues with postal voting' = -sample(rejection_simulations,10000)*2) %>%
  filter(`Probability of a recount in at least one decisive state` <= 0.20,
         `Potential decrease in Democratic vote margin\nfrom issues with postal voting` <= 0.04) %>%
  gather(variable,value) %>% write_csv('raw_simulation_data.csv')

# make charts
read_csv('raw_simulation_data.csv') %>%
  ggplot(.,aes(x=value*100)) +
  geom_histogram(bins=30) +
  geom_vline(data=tibble(variable = c('Probability of a recount in at least one decisive state',
                                      'Potential decrease in Democratic vote margin\nfrom issues with postal voting'),
                         average = c(median(recount_probability_vbm),-median(rejection_simulations)*2)*100),
             aes(xintercept = average)) +
  facet_wrap(~variable,ncol=2,scales='free') +
  scale_x_continuous(labels=function(x){round(x)}) +
  scale_y_continuous(breaks=seq(0,10000,200),
                     labels=function(x){round(x/10000*100,2)},
                     position='right') +
  theme_minimal()   +
  labs(x='',
       y='Percentage of simulations',
       subtitle='Simulated effects of increased postal voting and ballot rejection-rates',
       title='To come')

ggsave('lead_note_chart.pdf',width=8,height=4)
