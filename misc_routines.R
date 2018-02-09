
### Try to divide different groups of people based on the family structure and origin
fam.Austrian <- master %>% filter(status=="Yes, with children." & Austrian!="No")
fam.EU <- master %>% filter(status=="Yes, with children." & EU.citizen!="No")
fam.non.EU <- master %>% filter(status=="Yes, with children." & EU.citizen=="No")
noKid.Austrian <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & Austrian!="No")
noKid.EU <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & EU.citizen!="No")
noKid.non.EU <- master %>% filter(status=="Yes, I cohabit with a partner or other family members." & EU.citizen=="No")





mults1 <- master %>% select(id, ends_with("mult")) %>% unnest_(mult.vars[1])
mults1 <- master %>% select(id, ends_with("mult")) %>% unnest(german.reason.mult) 
mults1 <- master %>% select(id, ends_with("mult")) %>% unnest(circle.mult) 
mults1 <- master %>% select(id, ends_with("mult")) %>% unnest(barrier.mult) 

mults <- mults %>% 
  mutate(ord=factor(circle.mult, levels=names(table(circle.mult))[order(table(circle.mult), decreasing=TRUE)], labels=1:length(table(circle.mult)))) %>% 
  mutate(ord=paste0("circle", str_pad(ord, 2, pad="0"))) %>% 
  spread(ord, circle.mult)