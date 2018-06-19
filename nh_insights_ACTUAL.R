library(ggplot2)
library(gridExtra)
library(Hmisc)
library(miscTools)
library(sjPlot)
library(stringr)
library(tidyverse)

#-------------------------------------------------------------------------#
# BEN'S CODE #

nh <- read.csv("/Users/benjaminforleo/R/NH_cleaned.csv")
nh <- tbl_df(nh)

population <- read.csv("/Users/benjaminforleo/R/NH county population.csv")
population <- tbl_df(population)

### Section 1: Violations per Capita by County ###

speeding <- filter(nh,str_detect(violation, ".*Speeding"),out_of_state == FALSE)
speeding$violation <- "Speeding"

percapita <- speeding %>%
  select(county_name, violation)%>%
  group_by(county_name)%>%
  summarise(length(violation))

percapita <-merge(percapita,population)

colnames(percapita)[2] <- "speeding.violations"
colnames(percapita)[3] <- "total.population"
percapita

percapita <- mutate(percapita, prob.violation = speeding.violations/total.population)
percapita

graph <- ggplot(percapita, aes(x = county_name, y = prob.violation)) + geom_bar(stat = "identity", fill = 'deepskyblue3')
graph <- graph + theme_light()
graph <- graph + xlab("County Name") + ylab("Violations per Capita") + ggtitle("Violations per Capita by County")
graph <- graph + theme(plot.title = element_text(hjust = 0.5))
graph <- graph + theme(axis.text.x = element_text(angle = 90, hjust = 1))
graph <- graph + expand_limits(y = c(0, .20))
graph

### Section 2: Probability of ticket given violation by county, nh drivers ###

tickets <- speeding %>%
  select(county_name,stop_outcome) %>%
  filter(stop_outcome == "Ticket") %>%
  group_by(county_name) %>%
  summarise(length(stop_outcome))

colnames(tickets)[2] <- "speeding.tickets"

tickets <- merge(percapita, tickets)
tickets <- mutate(tickets, tickets.per.violation = speeding.tickets/speeding.violations)
tickets


graph <- ggplot(tickets, aes(x = county_name, y = tickets.per.violation)) + geom_bar(stat = "identity", fill = 'tomato')
graph <- graph + theme_light()
graph <- graph + xlab("County Name") + ylab("Probability (Ticket | Violation)") + ggtitle("Probability of Ticket Given Violation by County: NH Residents")
graph <- graph + theme(plot.title = element_text(hjust = 0.5))
graph <- graph + theme(axis.text.x = element_text(angle = 90, hjust = 1))
graph <- graph + expand_limits(y = c(0, .60))


### probability of ticket given violation by county: out of state drivers ###

speeding.out <- filter(nh,str_detect(violation, ".*Speeding"),out_of_state == TRUE)
speeding.out$violation <- "Speeding"

tickets.out <-speeding.out %>% 
  select(county_name,stop_outcome) %>% 
  filter(stop_outcome == "Ticket") %>% 
  group_by(county_name) %>% 
  summarise(length(stop_outcome))

stops.out <- speeding.out %>% 
  select(county_name,violation) %>% 
  group_by(county_name) %>% 
  summarise(length(violation))

stops.tickets.out <- merge(tickets.out, stops.out)

colnames(stops.tickets.out)[2] <- "speeding.tickets"
colnames(stops.tickets.out)[3] <- "violations"

stops.tickets.out <- mutate(stops.tickets.out, tickets.per.violation = speeding.tickets/violations)
stops.tickets.out

graph <- ggplot(stops.tickets.out, aes(x = county_name, y = tickets.per.violation)) + geom_bar(stat = "identity", fill = 'springgreen4')
graph <- graph + theme_light()
graph <- graph + xlab("County Name") + ylab("Probability (Ticket | Violation)") + ggtitle("Probability of Ticket Given Violation by County: \n Out of State Drivers")
graph <- graph + theme(plot.title = element_text(hjust = 0.5))
graph <- graph + theme(axis.text.x = element_text(angle = 90, hjust = 1))
graph <- graph + expand_limits(y = c(0, .60))


# graphing ticket probabilities in each county, separated by in/out of state

in.state.tickets <- select(tickets, county_name, tickets.per.violation)
out.state.tickets <- select(stops.tickets.out, county_name, tickets.per.violation)

out.of.state <- replicate(10, factor("out of state"))
in.state <- replicate(10, factor("in state"))

in.state.tickets <- add_column(in.state.tickets,in.state)
out.state.tickets <- add_column(out.state.tickets, out.of.state)

colnames(in.state.tickets)[3] <- "in/out of state"
colnames(out.state.tickets)[3] <- "in/out of state"

final <- rbind(in.state.tickets,out.state.tickets)

ggplot(final, aes(factor(county_name), `in/out of state`, fill = tickets.per.violation)) + 
  geom_col(position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

final.graph <-ggplot(data=final, aes(x=factor(county_name), y=tickets.per.violation, fill=`in/out of state`)) 
final.graph <- final.graph + geom_bar(stat="identity", position=position_dodge())
final.graph <- final.graph + theme_light()
final.graph <- final.graph + xlab("County Name") + ylab("Probability (Ticket | Violation)")
final.graph <- final.graph + ggtitle("Probability of Ticket Given Violation by County: \n In/Out of State Drivers")
final.graph <- final.graph + theme(plot.title = element_text(hjust = 0.5))
final.graph <- final.graph + theme(axis.text.x = element_text(angle = 90, hjust = 1))
final.graph <- final.graph + expand_limits(y = c(0, .70))






#-------------------------------------------------------------------------#
# SAM'S CODE #

# Frequency by day of Week
nh$DOW <- weekdays(nh$stop_date)
nh$DOW <- factor(nh$DOW, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
day_of_week <- ggplot(nh, aes(x=stop_time)) + geom_histogram(bins = 96, color = "deepskyblue3", fill="deepskyblue3") + facet_wrap(~DOW) + ggtitle('Stop Frequencies, by Time of Day') + theme(plot.title = element_text(hjust = 0.5), strip.background = element_rect(fill = "darkorange3")) + xlab('Stop Time') + ylab('Count')


# Density by day of week
day_of_week_density <- ggplot(nh, aes(x=stop_time, color=DOW, fill=DOW)) + theme_classic() + geom_density(alpha=0.075, size=0.85) + ggtitle('Stop Densities, by Time of Day') + theme(plot.title = element_text(hjust = 0.5)) + xlab('Stop Time') + ylab('Density')

# Density by weekends / weekdays
nh$Day_Type <- ifelse(nh$DOW %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
weekends_vs_weekdays <- ggplot(nh, aes(x=stop_time, color=Day_Type, fill = Day_Type)) + geom_density(alpha = 0.3, size=1.05) + scale_colour_manual(values = c('darkorange2', 'deepskyblue3')) + scale_fill_manual(values = c('darkorange2', 'deepskyblue3')) + ggtitle(label = 'Density Curve of Traffic Stops, by Time of Day') + theme(plot.title = element_text(hjust = 0.5)) + xlab(label = 'Time of Day') + ylab('Density') + theme_classic()


# Create and save contigency table (race X outcome)
pdf("race_outcome_contingency.pdf")
grid.table(table(nh$driver_race, nh$stop_outcome))
dev.off()
chisq.test(table(nh$driver_race, nh$stop_outcome))

# Create and save proportion table (race X outcome)
pdf("race_outcome_proportion.pdf")
grid.table(round(prop.table(table(nh$driver_race, nh$stop_outcome),1),2))
dev.off()








#-------------------------------------------------------------------------#
# CHAD'S CODE #

# finegrainlocation will be used to find interstate highway routes, Interstate 89,93,93,293, and 393 will be selected. 
# This will not include US numbered highways which sometimes run parallel to the interstates.
finegrainlocation <- select(nh,fine_grained_location)

# Grabs all values with 89/393/293/95 in the name of the location for pullover, regardless of direction or town
Interstate_89 <- filter(nh,str_detect(fine_grained_location,".*89"))
Interstate_393 <- filter(nh,str_detect(fine_grained_location,".*393"))
Interstate_93 <- filter(nh,str_detect(fine_grained_location,".* 93"))
Interstate_293 <- filter(nh,str_detect(fine_grained_location,".*293"))
Interstate_95 <- filter(nh,str_detect(fine_grained_location,".*95"))

#Adds a column for an identifier when creating a table
Interstate_95$highway_num <- 95
Interstate_93$highway_num <- 93
Interstate_89$highway_num <- 89
Interstate_293$highway_num <- 293
Interstate_393$highway_num <- 393

# Binds all of tables together
Interstates_All <- rbind(Interstate_293, Interstate_393, Interstate_89, Interstate_93, Interstate_95)

# Defines X as Interstates_All "stop outcome" by "highway number" our unique identifier we previously set
Interstate_contingency <- table(Interstates_All$stop_outcome, Interstates_All$highway_num)

prop_contingency <- prop.table(Interstate_contingency,2)
prop_contingency_rounded <- round(prop_contingency,4)
prop_contingency_rounded

pdf("outcome_table.pdf")
grid.table(Interstate_contingency)
dev.off()

pdf("outcome_prop_table.pdf")
grid.table(prop_contingency_rounded)
dev.off()







#-------------------------------------------------------------------------#
# JOANNA'S CODE #

# What percentage of people pulled over for speeding are male vs female?

# Filter the data to find records for Males and Females that were stopped for Speeding
MaleSpeeders<-filter(nh,violation == "Speeding" & driver_gender == "M")
FemaleSpeeders<-filter(nh,violation == "Speeding" & driver_gender == "F")

# Count the number of male and female speeders
NumberMaleSpeeders<-summarise(MaleSpeeders, n())
NumberFemaleSpeeders<-summarise(FemaleSpeeders, n())

# Calculate percentage of speeders that were male and female
PercentFemaleSpeeders<-NumberFemaleSpeeders/(NumberFemaleSpeeders+NumberMaleSpeeders)*100
PercentageMaleSpeeders<-(100-PercentFemaleSpeeders)


# -------------------
# What percentage of females/males who get pulled each type of stop outcome?

# Filter the data so that we only include entries in which gender was specified.
NH_filtered<-filter(nh, driver_gender == "M" | driver_gender == "F")

# If we run the code below, we only get a count of the males and females that received each outcome.
# however, since there were about twice as many males pulled over for speeding than females, this
# is not useful. It is better to look at proportions.
m<-ggplot(NH_filtered, aes(x = stop_outcome, fill = driver_gender)) + geom_bar(position = "dodge")
m<-m + ggtitle("Traffic Stop Outcomes by Sex")
m<-m + theme_bw()
m

# The package "sjPlot" allows us to look at the proportion of males and females that received each
# type of stop outcome. This is a more informative graph.
sjp.xtab(NH_filtered$stop_outcome,NH_filtered$driver_gender, show.values = F, show.total = FALSE, legend.title = "Driver gender", axis.titles = "Stop Outcome")