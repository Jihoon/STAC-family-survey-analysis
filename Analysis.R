### Get the data table ready
source("Process raw data.R")



### Descriptive analysis

attach(master)

# Contingency tables: Gender vs family type
fam.stat <- table(gender, status)[,c(2,4:5)]
write.table(fam.stat, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

# Contingency tables: Gender vs years by family type
master %>% filter(as.integer(status)==4 & origin!="Austrian") %>% count(gender, origin)
master %>% filter(as.integer(status)==5 & origin!="Austrian") %>% count(gender, origin)
master %>% filter(as.integer(status)==4 & origin!="Austrian") %>% count(gender, tot.duration)
master %>% filter(as.integer(status)==5 & origin!="Austrian") %>% count(gender, tot.duration)

# Family origin vs family type
fam.origin <- table(origin, status)[,4:5]
write.table(fam.origin, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)
# Work vs origin
master.fam %>% count(partner.work, origin) %>% spread(origin, n)
detach(master)

attach(master.fam)
fam.length <- data.frame(table(tot.duration, origin)[,2:3])
fam.num.child <- table(num_child, origin)
fam.job <- data.frame(table(challenge.job, origin)) %>% rename(How.challenging=challenge.job) %>% mutate(challenge="Partner's job")
fam.child <- data.frame(table(challenge.child, origin)) %>% rename(How.challenging=challenge.child) %>% mutate(challenge="Childcare")
fam.fin <- data.frame(table(challenge.fin, origin)) %>% rename(How.challenging=challenge.fin) %>% mutate(challenge="Financial")
fam.soc <- data.frame(table(challenge.soc, origin)) %>% rename(How.challenging=challenge.soc) %>% mutate(challenge="Social")
challenges <- rbind(fam.job, fam.child, fam.fin, fam.soc)
fam.num.child <- table(num_child, origin)
fam.num.child <- table(num_child, origin)
detach(master.fam)

# Length of stay
ggplot(master.fam, aes(x=origin, fill=tot.duration)) +
  geom_histogram(stat="count")

# Job type by origin (excluding voluntarily not working)
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(voluntary=="No" | is.na(voluntary)) %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=origin, fill=partner.work)) +
  geom_histogram(stat="count")+ 
  labs(x="Origin", y="", fill='Is the partner working?')

# Job type vs challenges
# Financial
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% 
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=partner.work, fill=challenge.fin)) +
  geom_histogram(stat="count")+ 
  labs(x="Job type", y="", fill='Financial challenge')
# Partner job
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% 
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=partner.work, fill=challenge.job)) +
  geom_histogram(stat="count")+ 
  labs(x="Job type", y="", fill='Career challenge')
# Childcare expenditure
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(!is.na(childcare.exp)) %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=partner.work, fill=childcare.exp)) +
  geom_histogram(stat="count")+ 
  labs(x="Job type", y="", fill='Childcare expenditure')


# Language
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(origin!="Austrian") %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=german, fill=partner.work)) +
  geom_histogram(stat="count")+ 
  labs(x="German", y="", fill='Is the partner working?')
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(origin!="Austrian") %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=german, fill=challenge.job)) +
  geom_histogram(stat="count")+ 
  labs(x="German", y="", fill='Career challenge')
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(origin!="Austrian") %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=german, fill=challenge.fin)) +
  geom_histogram(stat="count")+ 
  labs(x="German", y="", fill='Childcare challenge')
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(german=="No" | german=="Yes, myself") %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=german.study, fill=challenge.job)) +
  geom_histogram(stat="count")+ 
  labs(x="Partner learning German?", y="", fill='Job challenge')


library(scales)
ggplot(fam.length, aes(x=origin, y=Freq, fill=tot.duration)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.1)) + 
  labs(x="Origin", y="", fill='Length of stay in Austria')
 
ggplot(challenges, aes(x=origin, y=Freq, fill=How.challenging)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.1))  + 
  labs(x="", y="", fill='Perception') + facet_grid(~ challenge) +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(ncol = 1, title.position = "top"))


library(plotrix) # 3D pie
pie3D(master.fam$gender, labels = mydata$group, main = "An exploded 3D pie chart", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)
describe(master.fam)

### Overall challenges
ggplot(master.fam, aes(challenge.soc)) +
  # geom_bar(position = "dodge", stat="identity")
  geom_histogram(stat="count") + 
  facet_wrap( ~ origin) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(master.fam, aes(challenge.fin)) +
  # geom_bar(position = "dodge", stat="identity")
  geom_histogram(stat="count") + 
  facet_wrap( ~ origin) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(master.fam, aes(challenge.job)) +
  # geom_bar(position = "dodge", stat="identity")
  geom_histogram(stat="count") + 
  facet_wrap( ~ origin) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(master.fam.kid, aes(challenge.child)) +
  # geom_bar(position = "dodge", stat="identity")
  geom_histogram(stat="count") + 
  facet_wrap( ~ origin) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### Job issue
# master.fam.kid %>% group_by(origin) %>% summarise(cnt=n())
# master.fam.nokid %>% group_by(origin) %>% summarise(cnt=n())
# table(master.fam.kid$origin, master.fam.kid$partner.work) 
# table(master.fam.nokid$origin, master.fam.nokid$partner.work)

# Origin vs Spouse working
num.origin <- master.fam %>% group_by(origin) %>% summarise(cnt=n())
a <- master.fam %>% filter(searching=="Yes" | voluntary!="Yes")   # Those who want to work
table(a$origin)
job.origin <- data.frame(t(table(master.fam$origin, master.fam$partner.work) / num.origin$cnt)) 
names(job.origin) <- c("category", "origin", "rate")

ggplot(job.origin, aes(category, rate)) +
  geom_bar(aes(fill = origin), position = "dodge", stat="identity")

# Origin vs Spouse working in a relevant field
table(master.fam$origin, master.fam$relev.field)

# Childcare expense vs Spouse work
table(master.fam.kid$partner.work, master.fam.kid$childcare.exp)
table(master.fam.kid$origin, master.fam.kid$childcare.exp)
