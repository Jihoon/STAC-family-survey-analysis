### Read in the raw data
raw <- xlsx::read.xlsx("Family well-being survey_10112017.xlsx", 1) 
# New name list for the variables
a <- c("timestamp", "association", "gender", "tot.duration", "status", "sep.duration", "sep.reason.mult", "sep.reason.open", "sep.join", 
                "child.duration", "child.age1", "child.age2", "child.age3", "child.age4", "child.age5", "stayhome", "nursingleave", 
                "childcare.info1", "childcare.info2", "childcare.info3", "childcare.info4", "childcare.info5",
                "childcare.exp", "fam.beihilf", "fam.bei.reason.open", "fam.bei.applic", 
                "child.ins.cover", "cover.reason", "pediatrician", "ped.reason.open",
                "insu.type", "ins.concern.open", 
                "Austrian", "EU.citizen", "german", "german.study", "german.reason.mult", "german.challenge.open", 
                "socializing", "circle.mult", "social.challenge.open", "partner.work", "search.time", "relev.field", "searching", "voluntary", 
                "barrier.mult", "search.challenge.open", 
                "challenge.soc", "challenge.fin", "challenge.job", "challenge.child", "flextime", "telecom", "dualcareer",
                "spec.measure.open", "comment.open")
names(raw) <- a



### Construct a master data table for analysis and change data types (factor -> character)
master <- raw %>% arrange(status) %>% 
  mutate_at(vars(ends_with("open")), as.character) %>% 
  mutate_at(vars(ends_with("mult")), as.character) %>% 
  mutate_at(vars(ends_with("mult")), strsplit, ";")



### Try to divide different groups of people based on the family structure and origin
fam.Austrian <- master %>% filter(status=="Yes, with children." & Austrian!="No")
fam.EU <- master %>% filter(status=="Yes, with children." & EU.citizen!="No")
fam.non.EU <- master %>% filter(status=="Yes, with children." & EU.citizen=="No")
noKid.Austrian <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & Austrian!="No")
noKid.EU <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & EU.citizen!="No")
noKid.non.EU <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & EU.citizen=="No")
