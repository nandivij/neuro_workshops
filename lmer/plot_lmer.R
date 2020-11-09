adf <- read.csv('plot_lmer.csv')

str(adf)
adf$SID <- as.factor(adf$SID)
adf$gender <- as.factor(adf$gender)
adf$wave <- as.factor(adf$wave)
adf$age_c <- adf$age - mean(adf$age)
  
#lmer model
model <- lmer(pbip ~ gender + poly(age_c,2) + poly(age_c,2)*gender + (1 | SID), REML = FALSE, data=adf)

#update model formula without random effects and predict Y
fixed_form <- as.formula(lme4:::nobars(formula(model))[-2]) #this removes random effect from the model
mm<-model.matrix(fixed_form,adf) #this creates a matrix of values from adf to use when predicting Y 
adf$y<-as.numeric(mm%*%fixef(model)) #this predicts Y based on the previous matrix and the fixed_form without random effects

#bootstrapped confidence intervals
predFun<-function(.) mm%*%fixef(.) 
bb<-bootMer(model,FUN=predFun,nsim=1000,re.form=NA) #do this 1000 times
bb_se<-apply(bb$t,2,function(x) x[order(x)][c(25,975)]) #as we did this 1000 times the 95% CI will be bordered by the 25th and 975th value
adf$lwr<-bb_se[1,]
adf$upr<-bb_se[2,]

#plot
plot <- ggplot(adf, aes(x=age, y=pbip))+
  geom_line(aes(colour=gender, group=SID),size=.3,alpha=0.3)+
  geom_point(aes(colour=gender, group=SID),size=2,alpha=0.3)+
  geom_smooth(aes(x=age, y=y, colour=gender), method = "lm", 
              formula = as.formula(y ~ poly(x,2)),
              se = FALSE, size=1) +
  geom_ribbon(aes(y=y, ymin=lwr, ymax=upr, fill=gender), alpha=0.3) +
  theme_bw() +
  theme_minimal(base_size = 24, base_family = "Arial") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.position="right") + 
  facet_wrap(~gender)