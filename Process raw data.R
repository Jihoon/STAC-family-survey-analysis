### Read in the raw data
raw <- read.xlsx("Family well-being survey_raw_final_startedCleaning_analysis.xlsx", 1) 
# New name list for the variables
a <- c("id", "timestamp", "association", "gender", "tot.duration", "status", "sep.duration", "sep.reason.mult", "sep.reason.open", "sep.join", 
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
  mutate_at(vars(ends_with("mult")), strsplit, ";") %>%
  rowwise() %>% 
  mutate(num_child = 5-(is.na(child.age1)+is.na(child.age2)+is.na(child.age3)+is.na(child.age4)+is.na(child.age5))) %>%
  ungroup() %>% 
  mutate(status = as.factor(status)) %>%
  mutate_at(vars(starts_with("challenge.")), function(x){factor(x, levels=c("Extremely serious. I can consider leaving because of this.",
                                                                            "Serious.",
                                                                            "Somewhat problematic.",
                                                                            "A minor problem.",
                                                                            "We don't have this problem."))}) %>%
  mutate(tot.duration=factor(tot.duration, levels=c("Less than a year", "1-3 years", "3-5 years", "5-10 years", "More than 10 years"))) %>%
  mutate(short = (tot.duration=="Less than a year" | tot.duration=="1-3 years")) %>%  # Doesn't mean much for Austrians
  mutate(origin = ifelse(Austrian!="No", 1, ifelse(EU.citizen!="No", 2, 3))) %>% 
  mutate(origin = factor(origin, labels=c("Austrian", "Other-EU", "Non-EU"))) %>%
  select(id, status, origin, short, everything()) 



### Separating the mult-type answers
mult.vars <- names(master %>% select(ends_with("mult")))
mults <- list()
for (i in 1:length(mult.vars)) {
  
  # Unnest those grouped strings for each mult var (end up with long table)
  mults[[i]] <- master %>% select(id, ends_with("mult")) %>% unnest_(mult.vars[i])
  
  # Spread (long to wide) the table. New columns are in the order of number of answers
  mults[[i]] <- mults[[i]] %>% 
    mutate(ord=factor(get(mult.vars[i]), 
                      levels=names(table(get(mult.vars[i])))[order(table(get(mult.vars[i])), decreasing=TRUE)], 
                      labels=1:length(table(get(mult.vars[i]))))) %>% 
    mutate_cond(is.na(ord), ord=1) %>% # To incorporate rows with NA for this mult variable
    mutate(ord=paste0(mult.vars[i], str_pad(ord, 2, pad="0"))) %>%  # To be used as names for the new columns
    spread(ord, get(mult.vars[i]))
  master <- left_join(master, mults[[i]])
}

# Erase the original mult vars
master <- select(master, -ends_with("mult"))


detach("package:stats", unload=TRUE)
###Separating master based on family status (in case we need to treat them differently. May not be used)
# Level 3 "single parent of a 2 year old" - not branched correctly, so need to be ignored.
master.single <- master %>% filter(as.integer(status)==1) %>% select_if(~sum(!is.na(.)) > 0)
master.sep.family <- master %>% filter(as.integer(status)==2) %>% select_if(~sum(!is.na(.)) > 0)
master.fam.nokid <- master %>% filter(as.integer(status)==4) 
master.fam.kid <- master %>% filter(as.integer(status)==5) %>%
  mutate_cond(is.na(tot.duration), tot.duration="More than 10 years") # One specific case
master.fam <- rbind(master.fam.nokid, master.fam.kid) %>% 
  mutate(cat = paste0(as.integer(status), as.integer(origin), as.integer(short))) %>%
  # mutate(short = (tot.duration=="Less than a year" | tot.duration=="1-3 years")) %>%
  select_if(~sum(!is.na(.)) > 0) %>%
  select(id, cat, status, origin, short, everything()) 




### Text mining? 
### Will not be used - Not giving a concrete result
library(tidytext)
library(janeaustenr)
library(topicmodels)
library(mallet)

word.freq <- master %>% select(comment.open) %>% unnest_tokens(word, comment.open) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>% filter(!is.na(word))

word.freq %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment) 

word.dtm <- word.freq %>% mutate(document=1) %>% cast_dtm(document, word, n)

ap_lda <- LDA(word.dtm, k = 2, control = list(seed = 1234))
ap_topics <- tidy(ap_lda, matrix = "beta")

ap_top_terms  <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  dplyr::filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>% arrange(log_ratio) %>% slice(c(1:10,(dim(beta_spread)-9):dim(beta_spread))) %>%
  ggplot(aes(x=log_ratio, y=term, fill="lightgreen")) + geom_bar()


# Tutorial
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment)

ap_td <- tidy(AssociatedPress)

mallet_model <- MalletLDA(num.topics = 4)
