# behav. analysis ---------------------------------------------------------

library(lme4)
library(vcd)
library(epitools)
library(reshape2)
library(broom)
library(emmeans)
library(sf)
library(raster)
library(ggmap)
library(ggspatial)
library(tidyverse)
library(ggalluvial)
library(patchwork)

# data load and clean -----------------------------------------------------

#drone

behaviour_drone<-read.csv('data/behaviour_final.csv')

unique(behaviour_drone$species)

behaviour_fix<-behaviour_drone%>%
  mutate(species_fix = case_when(species == 'Kola' ~ 'Koala',
                                 grepl('Yellow', species) ~ 'Yellow-bellied glider',
                                 grepl('shtail', species) ~ 'Brushtail possum',
                                 grepl('Brusthail', species) ~ 'Brushtail possum',
                                 grepl('Brusthtail', species) ~ 'Brushtail possum',
                                 grepl('Ringtail', species) ~ 'Common ringtail possum',
                                 grepl('Ringstail', species) ~ 'Common ringtail possum',
                                 grepl('Rintail', species) ~ 'Common ringtail possum',
                                 grepl('Feathertail', species) ~ 'Feathertail glider',
                                 grepl('Kang', species) ~ 'Eastern grey kangaroo',
                                 grepl('Womb', species) ~ 'Bare-nosed wombat',
                                 grepl('Pygmy', species) ~ 'Eastern pygmy possum',
                                 grepl('know', species) ~ 'unknown',
                                 grepl('Unkown', species) ~ 'unknown',
                                 grepl('Bird', species) ~ 'Bird spp.',
                                 grepl('Sugar', species) ~ 'Krefft`s glider',
                                 grepl('Fox', species) ~ 'Red fox',
                                 grepl('Owl', species) ~ 'Southern boobook',
                                 grepl('oobo', species) ~ 'Southern boobook',
                                 grepl('Bandicoot', species) ~ 'Long-nosed bandicoot',
                                 grepl('Leadbeaters', species) ~ 'Leadbeater`s possum',
                                 grepl('Greater', species) ~ 'Southern greater glider',
                                 grepl('Wallaby', species) ~ 'Swamp wallaby',
                                 grepl('Lyrebird', species) ~ 'Superb lyrebird',
                                 grepl('Magpie', species) ~ 'Australian magpie',
                                 grepl('Eagle', species) ~ 'Wedge-tailed eagle',
                                 grepl('Deer', species) ~ 'Sambar deer',
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
                                 TRUE ~ behaviour_3))%>%
  mutate(behaviour_2 = case_when(changes_behaviour == 'N' & is.na(behaviour_2) ~ behaviour_1,
                                 TRUE ~ behaviour_2))%>%
  mutate(pre_behav = case_when(behaviour_1 %in% c('idle_looking_at_camera', 'frozen') ~ 'Vigilant',
                               behaviour_1 == 'idle_looking_away' ~ 'Looking',
                               behaviour_1 == 'feeding' ~ 'Feeding',
                               behaviour_1 == 'moving' ~ 'Moving',
                               behaviour_1 %in% c('social', 'other (describe in description)') ~ 'Other',
                               TRUE ~ behaviour_1),
         post_behav = case_when(behaviour_2 %in% c('idle_looking_at_camera', 'frozen') ~ 'Vigilant',
                               behaviour_2 == 'idle_looking_away' ~ 'Looking',
                               behaviour_2 == 'feeding' ~ 'Feeding',
                               behaviour_2 == 'moving' ~ 'Moving',
                               behaviour_2 %in% c('social', 'other (describe in description)') ~ 'Other',
                               TRUE ~ behaviour_2))%>%
  drop_na(post_behav)%>%
  droplevels()
  

#unique(sort(behaviour_fix$species_fix))
#unique(behaviour_drone$behaviour_1)
unique(behaviour_fix$coupe_ID_fix)
table(behaviour_fix$coupe_ID_fix)#%>%as.data.frame()%>%summarise(mean = mean(Freq))
table(behaviour_fix$pre_behav)


#spotlighting data

spot_1<-read.csv('data/spotlighting/spotlighting_pre.csv')%>%
  mutate(site = paste0(transectID,'_',plotID))%>%
  filter(behaviour %in% c('idle', 'moving', 'feeding', 'other'))%>%
  select(site, date, species, behaviour)

spot_2<-read.csv('data/spotlighting/spotlighting_post_fixed.csv')%>%
  select(site = Site.Name, date, species, behaviour = behavior)

spot_3<-read_csv('data/spotlighting/gg_spot_behav_datacall.csv')%>%
  mutate(species = 'Greater glider')%>%
  select(site = `Site name`, date = Date, species,  behaviour = Behaviour)

spot_4<-read.csv('data/spotlighting/spotlighting_thermal.csv')%>%
  filter(obs_type == 'seen')%>%
  select(site = site_ID, date, species,  behaviour = behavior)%>%
  mutate(behaviour = case_when(behaviour %in% c('idle', 'Idle', 'staring', "staring ", 
                                                'watching observer', 'hiding', 
                                                'hiding in hollow', 'Staring at observer') ~ 'vigilant',
                               behaviour %in% c('Active', 'active', 'moving', 
                                                'Gliding', 'jumping', 'flying overhead',
                                                'gliding', 'climbing') ~ 'moving',
                               behaviour %in% c('sitting', 'resting') ~ 'other',
                               
                               TRUE ~ behaviour))

unique(spot_4$behaviour)

behaviour_spot<-bind_rows(spot_1, spot_2, spot_3, spot_4)%>%
  mutate(behaviour_fix = case_when(grepl('dle', behaviour) ~ 'Vigilant',
                                   grepl('vigilant', behaviour) ~ 'Vigilant',
                                   grepl('ving', behaviour) ~ 'Moving',
                                   grepl('eed', behaviour) ~ 'Feeding',
                                   grepl('ther', behaviour) ~ 'Other',
                                   TRUE ~ behaviour),
         species_fix = case_when(grepl('Dingo', species) ~ 'Dog',
                                 grepl('ing', species) ~ 'Common ringtail possum',
                                 grepl('rush', species) ~ 'Brushtail possum',
                                 grepl('reater', species) ~ 'Southern greater glider',
                                 grepl('ugar', species) ~ 'Krefft`s glider',
                                 grepl('Cat', species) ~ 'Cat',
                                 grepl('nightjar', species) ~ 'Australian owlet nightjar',
                                 grepl('Yellow', species) ~ 'Yellow-bellied glider',
                                 grepl('Deer', species) ~ 'Sambar deer',
                                 grepl('Feather', species) ~ 'Feathertail glider',
                                 grepl('Boobook', species) ~ 'Southern boobook',
                                 TRUE ~ species))%>%
  droplevels()


table(behaviour_spot$behaviour_fix)
unique(behaviour_spot$species_fix)
unique(behaviour_spot$site)
table(behaviour_spot$site)


# review: remove observations from observer 2 -----------------------------

spot_1<-read.csv('data/spotlighting/spotlighting_pre.csv')%>%
  mutate(site = paste0(transectID,'_',plotID))%>%
  filter(behaviour %in% c('idle', 'moving', 'feeding', 'other'))%>%
  select(site, date, species, behaviour)

spot_2<-read.csv('data/spotlighting/spotlighting_post_fixed.csv')%>%
  dplyr::filter((seen_by_obs1 == 1 & seen_by_obs2 == 0) |
                  (seen_by_obs1 == 0 & seen_by_obs2 == 1))%>%
  select(site = Site.Name, date, species, behaviour = behavior)

spot_3<-read_csv('data/spotlighting/gg_spot_behav_datacall_unique.csv')%>%
  mutate(species = 'Greater glider')%>%
  select(site = `Site name`, date = Date, species,  behaviour = Behaviour)

spot_4<-read.csv('data/spotlighting/spotlighting_thermal_unique.csv')%>%
  filter(obs_type == 'seen')%>%
  select(site = site_ID, date, species,  behaviour = behavior)%>%
  mutate(behaviour = case_when(behaviour %in% c('idle', 'Idle', 'staring', "staring ",
                                                'watching observer', 'hiding', 'Staring',
                                                'hiding in hollow', 'Staring at observer', 'looking') ~ 'vigilant',
                               behaviour %in% c('Active', 'active', 'moving',
                                                'Gliding', 'jumping', 'flying overhead',
                                                'gliding', 'climbing', 'Climbing', 'moving') ~ 'moving',
                               behaviour %in% c('sitting', 'resting') ~ 'other',

                               TRUE ~ behaviour))

table(spot_4$behaviour)

behaviour_spot<-bind_rows(spot_1, spot_2, spot_3, spot_4)%>%
  mutate(behaviour_fix = case_when(grepl('dle', behaviour) ~ 'Vigilant',
                                   grepl('vigilant', behaviour) ~ 'Vigilant',
                                   grepl('looking', behaviour) ~ 'Vigilant',
                                   grepl('Looking', behaviour) ~ 'Vigilant',
                                   grepl('Watching', behaviour) ~ 'Vigilant',
                                   grepl('Staring', behaviour) ~ 'Vigilant',
                                   grepl('moved from foliage', behaviour) ~ 'Vigilant',
                                   grepl('ving', behaviour) ~ 'Moving',
                                   grepl('Moved', behaviour) ~ 'Moving',
                                   grepl('Resting', behaviour) ~ 'Looking',
                                   grepl('faced away', behaviour) ~ 'Looking',
                                   grepl('eed', behaviour) ~ 'Feeding',
                                   grepl('ther', behaviour) ~ 'Other',
                                   TRUE ~ behaviour),
         species_fix = case_when(grepl('Dingo', species) ~ 'Dog',
                                 grepl('ing', species) ~ 'Common ringtail possum',
                                 grepl('rush', species) ~ 'Brushtail possum',
                                 grepl('reater', species) ~ 'Southern greater glider',
                                 grepl('ugar', species) ~ 'Krefft`s glider',
                                 grepl('Cat', species) ~ 'Cat',
                                 grepl('nightjar', species) ~ 'Australian owlet nightjar',
                                 grepl('Yellow', species) ~ 'Yellow-bellied glider',
                                 grepl('Deer', species) ~ 'Sambar deer',
                                 grepl('Feather', species) ~ 'Feathertail glider',
                                 grepl('Boobook', species) ~ 'Southern boobook',
                                 TRUE ~ species))%>%
  filter(behaviour_fix != 'N/A')%>%
  droplevels()

table(behaviour_spot$behaviour_fix)
unique(behaviour_spot$species_fix)
unique(behaviour_spot$site)
table(behaviour_spot$site)


# Exploration and visualisation -------------------------------------------

#check behav flow - alluvial graph for drone

flow_data <- behaviour_fix %>%
  group_by(pre_behav, notices_drone, post_behav) %>%
  summarise(count = n(), .groups="drop")%>%
  mutate(notices_drone = factor(notices_drone, levels = c("Y","N"), labels = c('Yes', 'No')),
         prop = count / sum(count)*100)%>%
  na.omit()%>%
  mutate(total = sum(count))%>%
  group_by(post_behav)%>%
  mutate(sum_behav = sum(count),
         prop_behav = sum_behav/total*100,
         post_behav_label = case_when(post_behav %in% c('Vigilant', 'Feeding', 'Looking', 'Moving')~ 
                                                          paste0(post_behav, '\n', " (", round(prop_behav,1), "%)"),
                                      TRUE ~ paste0(post_behav, " (", round(prop_behav,1), "%)")))%>%
  group_by(notices_drone)%>%
  mutate(sum_notes = sum(count),
         prop_notes = sum_notes/total*100,
         notices_drone_label = paste0(notices_drone, '\n', " (", round(prop_notes,1), "%)"),
         notices_drone_label = factor(notices_drone_label))%>%
  ungroup()

#simple % pre

flow_data_simp<-behaviour_fix %>%
  group_by(pre_behav) %>%
  summarise(count = n(), .groups="drop")%>%
  na.omit()%>%
  mutate(total = sum(count),
         frac = round(count/total*100))

#alluvial chart

#dir.create('figures')

ggplot(flow_data,
       aes(axis1 = pre_behav, 
           axis2 = notices_drone_label, 
           axis3 = post_behav_label,
           y = count)) +
  geom_alluvium(aes(fill = post_behav), width = 1/12) +
  geom_stratum(width = 1/5.5, 
               fill = "white", 
               color = "darkgrey") +
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)),
            angle = 0,
            size = 2.1,
            fontface = "bold") +
  scale_x_discrete(limits = c("Initial \nbehaviour", "Notices drone?", "Subsequent \nbehaviour"), expand = c(.05, .05)) +
  scale_fill_manual(values = c("Vigilant"="red", "Feeding"="gold", "Moving"="mediumseagreen", 'Looking' = 'lightblue',
                               "Other"="hotpink" )) +
  labs(y="No. of observations",
       x=NULL,
       fill = 'Final \nbehaviour')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

ggsave('1.alluvial.svg',path = 'figures/', width = 20, height = 25, units = 'cm', dpi = 600)


#observed behaviours spotlighting

behaviour_spot %>%
  count(behaviour_fix) %>%
  mutate(prop = n / sum(n))%>%
  ggplot(aes(x = fct_reorder(behaviour_fix, -prop), y = prop, fill = behaviour_fix)) +
  geom_col(color = "darkgrey", alpha = 0.5) +
  scale_y_continuous(labels = scales::percent,n.breaks = 10) +
  scale_fill_manual(values = c("Feeding"="gold", "Moving"="mediumseagreen", 'Looking' = 'lightblue',
                               "Other"="hotpink", "Vigilant"="red"))+
  labs(x = NULL, y = "Percentage of observations", fill = "Behaviour") +
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

ggsave('S2.spot_obs.svg',path = 'figures/', width = 20, height = 15, units = 'cm', dpi = 600)

#simple stats

spot_prop<-behaviour_spot %>%
  count(behaviour_fix) %>%
  mutate(prop = round(n / sum(n)*100))

# drone transition probabilities ------------------------------------------

#all obs

transition_all<-table(behaviour_fix$pre_behav, behaviour_fix$post_behav)%>%
  prop.table(., margin = 1)

tm_long <- melt(transition_all)%>%setNames(c("Pre", "Post", "Probability"))

transprop_a<-ggplot(tm_long, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold", limits = c(0, 1)) +
  #scale_y_discrete(limits = rev(unique(tm_long$Pre))) +  # flips y-axis
  scale_x_discrete(limits = rev(unique(tm_long$Post))) +  # flips x-axis
  labs(y="Initial behavior", x=NULL, 
       fill ='Transition prob.',
       title = 'All observations')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")


#only noticed

behaviour_fix_notcied<-behaviour_fix%>%
  filter(notices_drone == 'Y')

transition_notice<-table(behaviour_fix_notcied$pre_behav, behaviour_fix_notcied$post_behav)%>%
  prop.table(., margin = 1)

tm_long_notice <- melt(transition_notice)%>%setNames(c("Pre", "Post", "Probability"))

transprop_b<-ggplot(tm_long_notice, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold") +
  #scale_y_discrete(limits = rev(unique(tm_long$Pre))) +
  scale_x_discrete(limits = rev(unique(tm_long$Post))) +  # flips x-axis
  labs(y="Initial behavior", x="Subsequent behavior", 
       fill ='Transition prob.',
       title = 'Animals that noticed the drone')+
  coord_cartesian(expand = F)+
  theme_bw() +
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

transprop_a / transprop_b +
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")  

ggsave('2.trans_prop.svg',path = 'figures/', width = 15, height = 25, units = 'cm', dpi = 600)



# probabilities by season --------------------------------------------------------

behav_levels <- rev(c("Feeding", "Looking", "Moving", "Other", "Vigilant"))

behaviour_fix_breeding_season<-behaviour_fix%>%
  mutate(date = dmy(survey_date),
         month = month(survey_date),
         season = case_when(month %in% c(3,4,5) ~ 'autumn',
                            month %in% c(6,7,8) ~ 'winter',
                            month %in% c(9, 10, 11) ~ 'spring',
                            TRUE ~ 'summer'),
         pre_behav = factor(pre_behav, levels = behav_levels),
         post_behav = factor(post_behav, levels = behav_levels))%>%
  group_split(season)%>%
  setNames(c('autumn', 'spring', 'summer', 'winter'))

transition_autumn<-table(behaviour_fix_breeding_season$autumn$pre_behav, 
                         behaviour_fix_breeding_season$autumn$post_behav)%>%
  prop.table(., margin = 1)%>%
  melt()%>%
  setNames(c("Pre", "Post", "Probability"))

transition_spring<-table(behaviour_fix_breeding_season$spring$pre_behav, 
                         behaviour_fix_breeding_season$spring$post_behav)%>%
  
  prop.table(., margin = 1)

transition_spring[is.na(transition_spring)] <- 0

transition_spring<-transition_spring%>%
  melt()%>%
  setNames(c("Pre", "Post", "Probability"))

transition_summer<-table(behaviour_fix_breeding_season$summer$pre_behav, 
                         behaviour_fix_breeding_season$summer$post_behav)%>%
  prop.table(., margin = 1)

transition_summer[is.na(transition_summer)] <- 0

transition_summer<-transition_summer%>%
  melt()%>%
  setNames(c("Pre", "Post", "Probability"))

transition_winter<-table(behaviour_fix_breeding_season$winter$pre_behav, 
                         behaviour_fix_breeding_season$winter$post_behav)%>%
  prop.table(., margin = 1)%>%
  melt()%>%
  setNames(c("Pre", "Post", "Probability"))



trans_summer<-ggplot(transition_summer, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold", limits = c(0, 1)) +
  scale_y_discrete(limits = rev(unique(transition_summer$Pre))) +  
  labs(y="Initial behavior", x=NULL, 
       fill ='Transition prob.',
       title = 'Summer')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'), 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

trans_autumn<-ggplot(transition_autumn, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold", limits = c(0, 1)) +
  scale_y_discrete(limits = rev(unique(transition_autumn$Post))) +  
  labs(y=NULL, x=NULL, 
       fill ='Transition prob.',
       title = 'Autumn')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

trans_winter<-ggplot(transition_winter, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold", limits = c(0, 1)) +
  scale_y_discrete(limits = rev(unique(transition_winter$Post))) +  
  labs(y="Initial behavior", x="Subsequent behavior", 
       fill ='Transition prob.',
       title = 'Winter')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_text(size = 10,  color = 'black'),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")


trans_spring<-ggplot(transition_winter, aes(x=Post, y=Pre, fill=Probability)) +
  geom_tile(color = 'darkgrey') +
  geom_text(aes(label=sprintf("%.2f", Probability)), color="black") +
  scale_fill_gradient(low="beige", high="gold", limits = c(0, 1)) +
  scale_y_discrete(limits = rev(unique(trans_spring$Post))) +  
  labs(y=NULL, x="Subsequent behavior", 
       fill ='Transition prob.',
       title = 'Spring')+
  coord_cartesian(expand = F)+
  theme_bw()+
  theme(plot.title = element_text(size = 12, face='bold', color = 'black'),
        axis.title.y = element_text(size = 11, color = 'black'),
        axis.title.x = element_text(size = 11, color = 'black'), 
        axis.text.x = element_text(size = 10,  color = 'black'),
        axis.text.y = element_blank(),
        legend.title = element_text(size = 11, color = 'black'),
        legend.text = element_text(size = 10),
        legend.position="bottom")

(trans_summer + trans_autumn) /
  (trans_winter + trans_spring) +
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")  

ggsave('S3.trans_prop.svg',path = 'figures/', width = 20, height = 20, units = 'cm', dpi = 600)

# test for significant difference by season -------------------------------

drone_season<-behaviour_fix_breeding_season<-behaviour_fix%>%
  mutate(date = dmy(survey_date),
         month = month(survey_date),
         season = case_when(month %in% c(3,4,5) ~ 'autumn',
                            month %in% c(6,7,8) ~ 'winter',
                            month %in% c(9, 10, 11) ~ 'spring',
                            TRUE ~ 'summer'),
         pre_behav = factor(pre_behav, levels = behav_levels),
         post_behav = factor(post_behav, levels = behav_levels))%>%
  mutate(method = 'drone',
         behaviour = case_when(pre_behav == 'Adverse' | post_behav == 'Adverse' ~ 'adverse',
                               TRUE ~ 'natural'),
         adverse = case_when(behaviour == 'adverse' ~ 1,
                             TRUE ~ 0))%>%
  dplyr::select(method, 
         site = coupe_ID_fix, 
         date = survey_date, 
         species = species_fix, behaviour, adverse, season)

table(drone_season$season)

season_mod <- glm(adverse ~ season, data = drone_season, family = binomial)
summary(season_mod)

emmeans(season_mod, ~ season, type = "response")
emmeans(season_mod, pairwise ~ season, type = "response")

# clip length x adverse ---------------------------------------------------

drone_clip<-behaviour_fix%>%
  mutate(method = 'drone',
         behaviour = case_when(pre_behav == 'Adverse' | post_behav == 'Adverse' ~ 'adverse',
                               TRUE ~ 'natural'),
         adverse = case_when(behaviour == 'adverse' ~ 1,
                             TRUE ~ 0),
         clip_seconds = as.numeric(ms(clip_length)))%>%
  select(method, site = coupe_ID_fix, date = survey_date, species = species_fix, behaviour, adverse, clip_length, clip_seconds)

length_mod <- glm(adverse ~ clip_seconds, data = drone_clip, family = binomial)
summary(length_mod)

emmeans(length_mod, ~ clip_seconds, type = "response")

summary(drone_clip$clip_seconds)
hist(drone_clip$clip_seconds)

drone_clip_60<-drone_clip%>%filter(clip_seconds>60)%>%
  distinct(site, date, .keep_all = TRUE)

# chi-sq tests ------------------------------------------------------------

#combine survey method datasets

drone_chi<-behaviour_fix%>%
  mutate(method = 'drone',
         behaviour = case_when(pre_behav == 'Vigilant' | post_behav == 'Vigilant' ~ 'adverse',
                               TRUE ~ 'natural'))%>%
  select(method, site = coupe_ID_fix, date = survey_date, species = species_fix, behaviour)

spot_chi<-behaviour_spot%>%
  mutate(method = 'spotlighting',
         behaviour_binomial = case_when(behaviour_fix == 'Vigilant' ~ 'adverse',
                               TRUE ~ 'natural'))%>%
  select(method, site, date, species = species_fix, behaviour = behaviour_binomial)

behaviour_combined<-bind_rows(drone_chi, spot_chi)

#cont. table - both
conttab_both <- table(behaviour_combined$method, behaviour_combined$behaviour)

#tests
chi_comb<-chisq.test(conttab_both)
assocstats(conttab_both)

epitools::oddsratio(conttab_both)
epitools::oddsratio(conttab_both, rev = "rows")

#Animals exposed to spotlighting were significantly 
#more likely to exhibit adverse behaviors compared to drone surveys 
#(OR  18.5, 95% CI 14.4 - 24.0, p < 0.001)

#cont table - drone

drone_tab <- behaviour_combined%>%filter(method == 'drone')%>%
  select(method, behaviour)%>%
  table()

chisq.test(drone_tab)

#Only 142 (14%) of observations were adverse
#Most animals (845 = 86%) behaved naturally under drones.

# X is very large observed counts deviate strongly from equal proportions.
# p - value extremely strong evidence to reject the null hypothesis.

#cont table - spot

spot_tab <- behaviour_combined%>%filter(method == 'spotlighting')%>%
  select(method, behaviour)%>%
  table()

chisq.test(spot_tab)

#Adverse: 471/622 = 76%
#Natural: 151/622 = 24%
#huge chi-square with p < 0.001, meaning spotlighting elicits significantly more adverse than natural behaviors

# glms ---------------------------------------------------------------------

#simple

behaviour_combined_binomial<-behaviour_combined%>%
  mutate(adverse = case_when(behaviour == 'adverse' ~ 1,
                             TRUE ~ 0))

behav_mod <- glm(adverse ~ method, data = behaviour_combined_binomial, family = binomial)
summary(behav_mod)
emmeans(behav_mod, ~ method, type = "response")

exp(cbind(OR = coef(behav_mod), confint(behav_mod)))
tidy(behav_mod, exponentiate = TRUE, conf.int = TRUE)

#odds of adverse response under drone: 0.168 ~ 1:6 = 0.168/(1+0.168) = 14.4%
#spotlighting estimate = 18.56 - odds of adverse response are 18.6 times higher , 
#CIR not crossing - sign
#back transform - 0.168 8 18.56 = 3.12, 3.12/(1+3.12) = 75.7% probability of adverse effect

#by species

#retain only those with enough obs

table(behaviour_combined_binomial$method, behaviour_combined_binomial$species)

behaviour_combined_binomial_spp<-behaviour_combined_binomial%>%
  filter(species %in% c('Brushtail possum', 'Common ringtail possum', 'Krefft`s glider',
                        'Southern greater glider', 'Yellow-bellied glider'))

behav_mod_spp <- glm(adverse ~ method*species, 
                     data = behaviour_combined_binomial_spp, family = binomial)
summary(behav_mod_spp)

emmeans(behav_mod_spp, ~ method | species, type = "response")

#get OR
emmeans(behav_mod_spp, pairwise ~ method | species, type = "response")


# site map for appendix ---------------------------------------------------

# site map ----------------------------------------------------------------

#location of drone surveys

coupes<-st_read('data/spatial/polys_all.gpkg')%>%st_transform(crs = 4326)

#get all centroids

coupes_centroids<-st_centroid(coupes)

plot(coupes_centroids$geom, col = 'red', axes = T)

coupes_centroids$Long<-st_coordinates(coupes_centroids)[,1]
coupes_centroids$Lat<-st_coordinates(coupes_centroids)[,2]

#ground survey locations

spot_loc_1<-read.csv('data/spotlighting/spotlighting_pre.csv')%>%
  mutate(site = paste0(transectID,'_',plotID))%>%
  separate(col = location, into = c("lat", "long"), sep = ",", convert = TRUE)%>%
  filter(!(is.na(lat) & is.na(long)))%>%
  dplyr::select(site, lat, long)

spot_loc_2<-read.csv('data/spotlighting/spotlighting_post_fixed.csv')%>%
  dplyr::select(site = 1, lat = 29, long = 28)%>%
  mutate(lat = lat*(-1))

spot_loc_3<-read.csv('data/spotlighting/spotlighting_thermal.csv')%>%
  dplyr::select(site = site_ID, lat, long)

spot_loc_all<-bind_rows(spot_loc_1, spot_loc_2, spot_loc_3)

spot_loc_all_filter <- spot_loc_all %>%
  group_by(site) %>%
  summarise( lat = mean(lat, na.rm = TRUE),
             long = mean(long, na.rm = TRUE),
             .groups = "drop")%>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

spot_loc_all_filter$Long<-st_coordinates(spot_loc_all_filter)[,1]
spot_loc_all_filter$Lat<-st_coordinates(spot_loc_all_filter)[,2]


plot(spot_loc_all_filter$geometry, col = 'red', axes = T)
plot(coupes_centroids$geom, col = 'blue', add = T)

#make inset

aus<-st_read('data/spatial/Australia_proj.shp')%>%
  #filter(STATENAME == "Victoria")%>%
  st_transform(crs = 4326)

inset<-ggplot(data = aus, show.legend = "point")+
  geom_sf(fill = 'white', color = 'black')+
  geom_rect(xmin = extent(spot_loc_all_filter)[1], xmax = extent(spot_loc_all_filter)[2],
            ymin = extent(spot_loc_all_filter)[3], ymax = extent(spot_loc_all_filter)[4],
            color = 'red', fill = NA, size = 1.5)+
  labs(x = NULL, y = NULL, title = NULL)+
  coord_sf(expand = F)+
  theme_void()

register_stadiamaps('b911e5ce-ac12-4c16-8133-b30a12cb39a3', write = T)

baselayer <- get_stadiamap(bbox = c(left = extent(spot_loc_all_filter)[1],
                                    bottom = extent(spot_loc_all_filter)[3],
                                    right = extent(spot_loc_all_filter)[2],
                                    top = extent(spot_loc_all_filter)[4]),
                           maptype = "stamen_terrain", 
                           crop = F)

map<-ggmap(baselayer) + labs(x = "", y = "") +
  geom_point(data = coupes_centroids, aes(x = Long, y = Lat), 
             color = 'black', fill = 'dodgerblue', shape = 21, size = 5) +
  geom_point(data = spot_loc_all_filter, aes(x = Long, y = Lat), 
             color = 'black', fill = 'red', shape = 25, size = 2) +
  coord_sf(crs = 4326)+
  theme_bw()+
  theme(plot.title = element_text(size =18, face='bold'),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18), 
        axis.text.x = element_text(size = 15, face = 'bold', color = 'black'),
        axis.text.y = element_text(size = 15, face = 'bold', color = 'black'),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 15),
        legend.position = 'bottom',
        plot.margin = margin(0,1,0,0, unit = 'cm'))+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))+
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.0001, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)+
  annotation_scale(location = 'br', style = 'ticks', text_col = 'black')

map+annotation_custom(ggplotGrob(inset), xmin = 143.6, xmax = 145.3, ymin = -36.9, ymax = -37.3)

ggsave('S1.map.svg',path = 'figures', width = 40, height = 20,
       units = 'cm', dpi = 120)
