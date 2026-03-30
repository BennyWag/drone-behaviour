# packages and data -------------------------------------------------------

library(tidyverse)

#load

behaviour<-read.csv('data/behaviour_analysis_24-09-30.csv')

unique(behaviour$species)
unique(behaviour$coupe_ID)
unique(behaviour$observation_time)

#fix columns

behaviour_fix<-behaviour%>%
  mutate(species_fix = case_when(species == 'Kola' ~ 'Koala',
                                 grepl('Yellow', species) ~ 'Yellow-bellied glider',
                                 grepl('shtail', species) ~ 'Brushtail possum',
                                 grepl('Ringtail', species) ~ 'Ringtail possum',
                                 grepl('Ringstail', species) ~ 'Ringtail possum',
                                 grepl('Feathertail', species) ~ 'Feathertail glider',
                                 grepl('Kang', species) ~ 'Eastern grey kangaroo',
                                 grepl('Womb', species) ~ 'Wombat',
                                 grepl('Pygmy', species) ~ 'Eastern pygmy possum',
                                 grepl('know', species) ~ 'unknown',
                                 TRUE ~ species),
         species_fix = str_trim(species_fix, side = 'right'),
         date_fix = as.Date(survey_date, format = "%d/%m/%Y"),
         time_fix = hm(observation_time),
         coupe_ID_fix = str_remove(coupe_ID, "\\bCoupe\\b"),
         coupe_ID_fix = case_when(coupe_ID_fix == 'Coupe263-000-0109' ~ '263-000-0109',
                                  coupe_ID_fix == 'YellowControl-Report' ~ 'Yellow',
                                  TRUE ~ coupe_ID_fix),
         coupe_ID_fix = str_trim(coupe_ID_fix, side = 'left'),
         behaviour_2 = case_when(behaviour_2 == '' ~ NA,
                                 TRUE ~ behaviour_2),
         behaviour_3 = case_when(behaviour_3 == '' ~ NA,
                                 TRUE ~ behaviour_3))

#remove unknowns

behaviour_fix_2<-behaviour_fix%>%filter(species_fix != 'unknown')

#check 

table(behaviour_fix_2$behaviour_1)
table(behaviour_fix_2$behaviour_2)
table(behaviour_fix_2$behaviour_3)
table(behaviour_fix_2$changes_behaviour)
table(behaviour_fix_2$notices_drone)

#tidy up

colnames(behaviour_fix_2)

behaviour_tidy<-behaviour_fix_2%>%dplyr::select(site = coupe_ID_fix,
                                                date = date_fix,
                                                time = observation_time,
                                                n_obs,
                                                obs = observation,
                                                species = species_fix,
                                                notice = notices_drone,
                                                change = changes_behaviour,
                                                behaviour_1,
                                                behaviour_2,
                                                behaviour_3,
                                                comments)%>%
  as.tibble()

noticing<-table(behaviour_tidy$species, behaviour_tidy$notice)%>%as.data.frame()
behaviour<-table(behaviour_tidy$species, behaviour_tidy$behaviour_1)%>%as.data.frame()

# graphs ------------------------------------------------------------------

#filter for arboreals

behaviour_tidy_arb<-behaviour_tidy%>%filter(species %in% c('Greater Glider','Brushtail possum',
                                                           'Ringtail possum', 'Sugar glider',
                                                           'Koala','Eastern pygmy possum',
                                                           'Feathertail glider'))

#no of obs per behav


#simple
ggplot(behaviour_tidy_arb, aes(x = species, fill = behaviour_1)) +
  geom_bar(position = "dodge", color = 'darkgrey') +
  labs(x = "Species", y = "Number of Observations", fill = "Behaviour") +
  theme_bw()

#facet

ggplot(behaviour_tidy_arb, aes(x = behaviour_1, fill = behaviour_1)) +
  geom_bar(color = 'darkgrey') +
  labs(x = "Behaviour", y = "Number of Observations", fill = "Behaviour") +
  facet_wrap(~ species) +
  theme_bw() +
  theme(legend.position = "none")

# notices or not

ggplot(behaviour_tidy_arb, aes(x = notice, fill = notice)) +
  geom_bar(color = 'darkgrey') +
  labs(x = "Behaviour", y = "Number of Observations", fill = "Behaviour") +
  facet_wrap(~ species) +
  theme_bw() +
  theme(legend.position = "none")

#change

ggplot(behaviour_tidy_arb, aes(x = change, fill = change)) +
  geom_bar(color = 'darkgrey') +
  labs(x = "Behaviour", y = "Number of Observations", fill = "Behaviour") +
  facet_wrap(~ species) +
  theme_bw() +
  theme(legend.position = "none")
