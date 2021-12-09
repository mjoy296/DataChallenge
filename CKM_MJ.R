library(dplyr)
library(tidyr)
library(ggplot2)
library(anytime)
library(forecast)
library(scales)
library(xts)
library(zoo)

options(scipen = 999)

setwd('C:/Users/mjoy/Documents/MarissaJCKMChallenge/')
comps = read.csv('./CompData.csv')

nrow(comps)
head(comps)

comps$component_name <- as.character(comps$component_name)

dup <- comps[duplicated(comps), ]

comps <- comps[!duplicated(comps), ]

#sort file by component names and it's timestamp
ordered = comps %>%
  arrange(component_name, change_ts)
#write.csv(ordered, "Ordered.csv")

#view location each component goes through
jkl <- comps %>% group_by(., component_type) %>% count(., location)
path0 <- c("YBSFJ_00_01", "JX3UO_01_01", "29MSJ_03_01", "ZZU3X_02_00", "B57X3_03_00", "7EFLOP_03_02", "assembly room")
path1 <- c("TG61A_00_02", "MRX5B_01_00", "HUX1L_02_02", "29MSJ_03_01", "B57X3_03_00", "assembly room")
path2 <- c("VUFVH_00_00", "5YU1V_01_02", "JX3UO_01_01", "ZAENM_02_01", "HUX1L_02_02","29MSJ_03_01", "7EFLOP_03_02", "assembly room")
path3<- c("GH9CV_00_03", "MRX5B_01_00", "5YU1V_01_02", "ZZU3X_02_00", "29MSJ_03_01","7EFLOP_03_02", "assembly_room")

#dataframe of final location of each component
l <- comps %>%
  select(., component_name, component_type, location, times) %>%
  arrange(., times) %>%
  group_by(., component_name) %>%
  filter(., times ==  max(times))

#datafrme of intial location of each compnent
m <- comps %>%
  select(., component_name, component_type, location, times) %>%
  arrange(., times) %>%
  group_by(., component_name) %>%
  filter(., times ==  min(times))

#combining dataframes
mm <- rbind(l,m)
m_loc <- mm %>% group_by(., component_type) %>% count(., location)

#get rate of outputs of each type
rates <- mm %>%
  group_by(., component_type) %>%
  mutate(., stat = ifelse(location == "assembly room", "end", "initial")) %>%
  count(., stat) %>%
  spread(., stat, n) %>%
  mutate(., rate_output = end/initial)

##graph to visualize rates 
ggplot(rates, aes(component_type, rate_output, label = percent(rate_output))) + 
  geom_bar(aes(fill = component_type), position = "dodge", stat = "identity") +
  geom_label() + scale_y_continuous(labels = scales::percent) + 
  xlab("Component Type") + ylab("Rate of Output") +
  ggtitle("Rate of Output by Comp. Type") +   theme(plot.title = element_text(hjust = 0.5))



#convert change_ts to timestamps
comps$times <- as.numeric(substr(comps$change_ts, 1, nchar(comps$change_ts) - 3))
comps$times <- anytime(comps$times) 
comps <- comps %>% mutate(., day = weekdays((comps$times)))
min(comps$times) #"2017-12-31 19:00:00 EST"
max(comps$times) #"2018-05-31 20:00:20 EDT"

#get average time it takes specific component to 
#go from first machine to assembly room 
name_diff <- comps %>%
  group_by(., component_name) %>%
  mutate(., diff = as.numeric(difftime(max(times), min(times))))
  
diffs <- name_diff %>%
  group_by(., component_type) %>%
  summarise(., avg = mean(diff)) %>%
  mutate(., avg = format(round(avg, 2), nsmall =2))
diffs

#graph to visualize avg time each component takes to get to assembly room
ggplot(diffs, aes(component_type, avg, label = avg)) + 
  geom_bar(aes(fill = component_type), position = "dodge", stat = "identity") +
  geom_label() + scale_fill_manual(values = c("skyblue", "royalblue", "blue", "navy")) +
  xlab("Component Type") + ylab("Hours to Completion") +
  ggtitle("Avg. Completion Time by Comp. Type") +   theme(plot.title = element_text(hjust = 0.5))


#First get the time spend by each component at a location
comps_timesSpendAtLocatoin <- comps %>%
  group_by(component_name) %>%
  mutate(timeSpendAtLocation = as.numeric(difftime(lead(times),times, units = "hours")))


comps_timesSpendAtLocatoin <- comps_timesSpendAtLocatoin[!duplicated(comps_timesSpendAtLocatoin), ]

c <- comps_timesSpendAtLocatoin %>% group_by(., times, location) %>%
         count(.,location)

ggplot(c, aes(n, as.Date(times))) +
  geom_line(aes(colour = location)) + scale_y_date("day")

loc_times <- comps %>% select(., location, times, component_type) %>%
  group_by(., location, times) %>%
  summarise(., n = n()) %>%
  spread(., location, n)

comp_finalt <- comps %>% select(., location, times, component_type) %>%
  filter(., location =="assembly room") %>%
  group_by(., component_type, times) %>%
  count(., component_type) %>%
  spread(., component_type, n, fill = 0)

rownames(comp_finalt) <- comp_finalt$times
comp_finalt$times <- NULL


#loop to check which times a unit was completed
t <- 1
tls = array()
for (x in 1:nrow(comp_finalt)){
  cs <- list(colSums(comp_finalt[c(1:x), ]))[1]
  for (y in cs){
    if (all(y >= t)){
      tls[t] <- anytime(rownames(comp_finalt)[x])
      t = t + 1
    }
  }
}

#get avg time it takes to assemble one unit 
tls <- as.POSIXct(tls, origin = "1960-10-01")
tls <- as.data.frame(tls)

tls$diff <-difftime(lead(tls$tls),tls$tls, unit = "hours")

tls %>% summarise(., avg = mean(diff, na.rm = TRUE))

xtsible(as.data.frame(comp_finalt))

dm <- as.xts(as.matrix(days))

c2 <- comps_timesSpendAtLocatoin %>% select(., times, component_name, location, timeSpendAtLocation)

c2_1 <- c2 %'>% filter(., location %in% target1) %>% spread(., location, timeSpendAtLocation)
c2_1 <- merge(c2_1, comps[c('component_name', 'component_type')], by = "component_name", all.x = TRUE)
c2_1 <- c2_1[!duplicated(c2_1), ]


c2_2 <- c2 %>% filter(., location %in% target2) %>% spread(., location, timeSpendAtLocation)
c2_2 <- merge(c2_2, comps[c('component_name', 'component_type')], by = "component_name", all.x = TRUE)
c2_2 <- c2_2[!duplicated(c2_2), ]


tsdata <- ts(comps_timesSpendAtLocatoin$change_ts, start = "2017-12-31 19:00:00")



#Group_by 'component_name' to find average time spend on a location by each component
atl <- comps_timesSpendAtLocatoin %>%
              group_by(component_name, location) %>%
              summarise(avgTimeComponentAtLocation = 
                                       mean(timeSpendAtLocation, na.rm = TRUE))



#Average time spend on a location by component_type and location  
loc_type <- comps_timesSpendAtLocatoin %>%
            group_by(location, component_type) %>%
             summarise(avgTimeatloc=
                         mean(timeSpendAtLocation, na.rm = TRUE))


comps %>%
  group_by(., component_name) %>%
  filter(., )



ar <- comps %>% 
  filter(., location== "assembly room") %>%
  select(., component_type, change_ts) %>%
  arrange(., change_ts)


daily <- comps %>% select(., component_name, location, component_type, day, status, change_ts)

daily_agg <- daily %>% group_by(., location, change_ts, component_type) %>% 
                count(., status)
  
daily3 <- daily_agg %>% filter(., component_type == "component_3", location %in% path3)

daily3
