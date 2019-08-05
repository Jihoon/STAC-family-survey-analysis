### Get the data table ready
source("Process raw data.R")



### Descriptive analysis

attach(master)

# Contingency tables: Gender vs family type
fam.stat <- table(gender, ftype)[,c(2,4:5)]
write.table(fam.stat, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)

# Contingency tables: Gender vs years by family type
master %>% filter(as.integer(ftype)==4 & origin!="Austrian") %>% count(gender, origin)
master %>% filter(as.integer(ftype)==5 & origin!="Austrian") %>% count(gender, origin)
master %>% filter(as.integer(ftype)==4 & origin!="Austrian") %>% count(gender, tot.duration)
master %>% filter(as.integer(ftype)==5 & origin!="Austrian") %>% count(gender, tot.duration)
master %>% filter(as.integer(ftype)==5 & origin!="Austrian") %>% count(search.time, origin)

# Family origin vs family type
fam.origin <- table(origin, ftype)[,4:5]
fam.origin.gender <- table(origin, gender)
write.table(fam.origin.gender, "clipboard", sep="\t", row.names = TRUE, col.names = TRUE)
# Work vs origin
master.fam %>% count(partner.work, origin) %>% spread(origin, n)
master.fam %>% filter(partner.work=="No") %>% count(origin, voluntary)
detach(master)

attach(master.fam)
fam.length <- data.frame(table(tot.duration, origin)[,2:3])
fam.length <- data.frame(table(tot.duration, origin, gender)[,2:3,])#
fam.length <- data.frame(table(tot.duration, origin, gender_factor)[,2:3,])#
fam.num.child <- table(num_child, origin)

fam.job   <- master.fam %>% count(challenge.job, origin, gender, gender_factor) %>% rename(How.challenging=challenge.job) %>% mutate(challenge="Partner's job") %>%
  mutate_cond(is.na(How.challenging), How.challenging="We don't have this problem.")
fam.child <- master.fam.kid %>% count(challenge.child, origin, gender, gender_factor) %>% rename(How.challenging=challenge.child) %>% 
  mutate(challenge="Childcare") %>% mutate_cond(is.na(How.challenging), How.challenging="We don't have this problem.")   # Just for families with kids
fam.fin   <- master.fam %>% count(challenge.fin, origin, gender, gender_factor) %>% rename(How.challenging=challenge.fin) %>% mutate(challenge="Financial")
fam.soc   <- master.fam %>% count(challenge.soc, origin, gender, gender_factor) %>% rename(How.challenging=challenge.soc) %>% mutate(challenge="Social")
challenges <- rbind(fam.job, fam.child, fam.fin, fam.soc) %>% filter(!is.na(How.challenging))

fam.num.child <- table(num_child, origin)
fam.num.child <- table(num_child, origin)
detach(master.fam)

# Length of stay
ggplot(master.fam, aes(x=origin, fill=tot.duration)) +
  geom_histogram(stat="count")

# Figure 4. Job type by origin (excluding voluntarily not working)
ggplot(master.fam %>% filter(partner.work!="I am divorced" & partner.work!="no partner" & partner.work!="retired") %>% filter(voluntary=="No" | is.na(voluntary)) %>%
         mutate_cond(partner.work=="Full time consultant ", partner.work="Yes, paid full-time"), 
       aes(x=origin, fill=partner.work)) +
  theme(axis.text = element_text(size=fontsize+4), axis.title = element_text(size=fontsize+7), axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size=fontsize+5), legend.text = element_text(size=fontsize+3), legend.title = element_text(size=fontsize+4))+ 
  geom_histogram(stat="count")+ 
  labs(x="Origin", y="Number of respondents", fill='Is the partner working?') + facet_grid(~ gender_factor) + 
  scale_fill_manual(values = c('red', "orange", rev(brewer.pal(9, "YlGnBu")[3:6])))

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
library(RColorBrewer)
ggplot(fam.length, aes(x=origin, y=Freq, fill=tot.duration)) +
  geom_bar(position = "fill",stat = "identity") +
  scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.1)) +
  labs(x="Origin", y="", fill='Length of stay in Austria')
 
# Fig 2. Length of stay
fontsize = 10
ggplot(fam.length, aes(x=origin, y=Freq, fill=tot.duration)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,18,2))  + 
  labs(x="Origin", y="Number of respondents", fill='Length of stay in Austria') + facet_grid(~ gender_factor) + 
  theme(axis.text = element_text(size=fontsize+4), axis.title = element_text(size=fontsize+7), 
        strip.text = element_text(size=fontsize+5), legend.text = element_text(size=fontsize+3), legend.title = element_text(size=fontsize+4)) + 
  scale_fill_manual(values = brewer.pal(9, "PuRd")[3:7])

ggplot(master.fam.kid %>% count(tot.duration, origin, gender) %>% filter(origin!="Austrian"), 
       aes(x=origin, y=n, fill=tot.duration)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,10,2))  + 
  labs(x="Origin", y="Number of respondents", fill='Length of stay in Austria') + facet_grid(~ gender)
ggplot(master.fam.nokid %>% count(tot.duration, origin, gender) %>% filter(origin!="Austrian"), 
       aes(x=origin, y=n, fill=tot.duration)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(breaks = seq(0,10,2))  + 
  labs(x="Origin", y="Number of respondents", fill='Length of stay in Austria') + facet_grid(~ gender)


library(RColorBrewer)
challenge.color = scale_fill_manual("Perception", values = c(rev(brewer.pal(11, "Spectral")[c(2,3,4,8,10)]), '#000000'))

# Challenge perception by issue by gender
ggplot(challenges %>% filter(gender!="Prefer not to say"), aes(x=origin, y=n, fill=How.challenging)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_y_continuous(labels = percent_format(), breaks = seq(0,1,.1))  +
  # geom_bar(stat = "identity") +
  # scale_y_continuous(breaks = seq(0,16,2))  + 
  labs(x="", y="Share of respondents", fill='Perception') + facet_grid(~ gender_factor) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size=fontsize+3), 
        strip.text = element_text(size=fontsize+2), legend.text = element_text(size=fontsize+2), legend.title = element_text(size=fontsize+3)) +
  guides(fill=FALSE) + challenge.color

# Figure 3. 
ggplot(challenges %>% filter(gender!="Prefer not to say"), aes(x=origin, y=n, fill=How.challenging)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,16,2))  + 
  labs(x="", y="Number of respondents", fill='Perception') + facet_grid(. ~ gender_factor + challenge) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_text(size=fontsize+3), 
        strip.text = element_text(size=fontsize+2), legend.text = element_text(size=fontsize+2), legend.title = element_text(size=fontsize+3)) +
  guides(fill = guide_legend(ncol = 1, title.position = "top")) + challenge.color

# Challenge perception by issue by employment (need more grouping for employment)
# ggplot(challenges %>% filter(gender!="Prefer not to say"), aes(x=origin, y=n, fill=How.challenging)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(breaks = seq(0,10,2))  + 
#   labs(x="", y="", fill='Perception') + facet_grid(~ gender + challenge) +
#   theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(ncol = 1, title.position = "top")) + challenge.color

# Challenge perception by issue only for families with kids
fam.job.kid   <- master.fam.kid %>% count(challenge.job, origin, gender) %>% rename(How.challenging=challenge.job) %>% mutate(challenge="Partner's job") %>%
  mutate_cond(is.na(How.challenging), How.challenging="We don't have this problem.")
fam.fin.kid   <- master.fam.kid %>% count(challenge.fin, origin, gender) %>% rename(How.challenging=challenge.fin) %>% mutate(challenge="Financial")
fam.soc.kid   <- master.fam.kid %>% count(challenge.soc, origin, gender) %>% rename(How.challenging=challenge.soc) %>% mutate(challenge="Social")
challenges.kid <- rbind(fam.job.kid, fam.child, fam.fin.kid, fam.soc.kid) %>% filter(!is.na(How.challenging))

# ggplot(challenges.kid %>% filter(gender!="Prefer not to say"), aes(x=origin, y=n, fill=How.challenging)) +
#   geom_bar(stat = "identity") +
#   scale_y_continuous(breaks = seq(0,10,2))  + 
#   labs(x="", y="", fill='Perception') + facet_grid(~ gender + challenge) +
#   theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
#   guides(fill = guide_legend(ncol = 1, title.position = "top")) + challenge.color

# Challenge perception by issue only for families without kids
fam.job.nokid   <- master.fam.nokid %>% count(challenge.job, origin, gender) %>% rename(How.challenging=challenge.job) %>% mutate(challenge="Partner's job") %>%
  mutate_cond(is.na(How.challenging), How.challenging="We don't have this problem.")
fam.fin.nokid   <- master.fam.nokid %>% count(challenge.fin, origin, gender) %>% rename(How.challenging=challenge.fin) %>% mutate(challenge="Financial")
fam.soc.nokid   <- master.fam.nokid %>% count(challenge.soc, origin, gender) %>% rename(How.challenging=challenge.soc) %>% mutate(challenge="Social")
challenges.nokid <- rbind(fam.job.nokid, fam.fin.nokid, fam.soc.nokid) %>% filter(!is.na(How.challenging))

ggplot(challenges.nokid %>% filter(gender!="Prefer not to say"), aes(x=origin, y=n, fill=How.challenging)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(0,10,2))  + 
  labs(x="", y="", fill='Perception') + facet_grid(~ gender + challenge) +
  theme(legend.position="bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(fill = guide_legend(ncol = 1, title.position = "top")) + challenge.color



# library(plotrix) # 3D pie
# pie3D(master.fam$gender, labels = mydata$group, main = "An exploded 3D pie chart", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)
# describe(master.fam)

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
t(table(master.fam$origin, master.fam$relev.field))

# Origin vs Spouse working in a relevant field
origin_job.barrier <- data.frame(rbind(
  t(table(master.fam$origin, master.fam$barrier.mult01)),
  t(table(master.fam$origin, master.fam$barrier.mult02)),
  t(table(master.fam$origin, master.fam$barrier.mult03)),
  t(table(master.fam$origin, master.fam$barrier.mult04)),
  t(table(master.fam$origin, master.fam$barrier.mult05))
)) %>% add_rownames("reason") %>% gather(key=origin, value=count, -reason) 
origin_job.barrier <- origin_job.barrier %>% 
  mutate(reason=factor(reason, levels=unique(origin_job.barrier$reason)[c(5, 1, 2, 4, 3)]),
         origin=factor(origin, levels=c("Austrian", "Other.EU", "Non.EU"))) 
origin_job.barrier$origin <- revalue(origin_job.barrier$origin, c("Other.EU"="Other-EU", "Non.EU"="Non-EU"))
           
# Figure 5. 
ggplot(origin_job.barrier, aes(origin, count)) +
  geom_bar(aes(fill = reason), position = "dodge", stat="identity") +
  labs(x="", y="Number of mentions", fill='Reasons')+ 
  theme(axis.text = element_text(size=fontsize+4), axis.title = element_text(size=fontsize+7), 
        strip.text = element_text(size=fontsize+5), legend.text = element_text(size=fontsize+3), legend.title = element_text(size=fontsize+4)) + 
  scale_fill_manual(values = c(brewer.pal(9, "YlOrRd")[c(7,4,5,3)], "darkgreen"))


# Childcare expense vs Spouse work
table(master.fam.kid$partner.work, master.fam.kid$childcare.exp)
table(master.fam.kid$origin, master.fam.kid$childcare.exp)

# Childcare challenge vs Spouse work
view(master.fam.kid %>% filter(as.numeric(challenge.child) <=3))
table(master.fam.kid$partner.work, master.fam.kid$challenge.child)

# Social challenges vs social circle
view(master.fam.kid %>% filter(as.numeric(challenge.child) <=3))
table(master.fam.kid$partner.work, master.fam.kid$challenge.child)

# Social challenges vs social circle
table(master.fam$tot.duration, master.fam$german.study)
