
#########################################

library(ggplot2)
library(mgcv)
library(effects)

#########################################

## CONTINUOUS NORMING

set.seed(10)

N = 1250

age = runif(N,5,15)
y = 1 + 2*log(age-4.5) + rnorm(N,0,5)
plot(age,y)

group = cut(age,breaks=seq(5,15,1))
table(group)
df = data.frame(y,age,group)

fitAge = gam(y ~ s(age,k=3), data=df)
newdata = data.frame(age=seq(5.5,14.5,.1))

fitGroup = lm(y ~ group, data=df)
effGroup = data.frame(allEffects(fitGroup)$"group")

ymin = min(effGroup$lower); ymax = max(effGroup$upper)

######

# just plot

(justplot = ggplot()+
  coord_cartesian(ylim=c(ymin,ymax))+
  geom_point(data=effGroup,aes(x=group,y=fit),size=4)+
  geom_errorbar(data=effGroup,aes(x=group,ymin=lower,ymax=upper),width=.25,linewidth=0.6)+
  geom_line(data=effGroup,aes(x=group,y=fit,group=1),linewidth=0.6)+
  theme(text=element_text(size=25)) + 
  scale_y_continuous(breaks=seq(0,100,1)) +
  ylab("Punt. medio") + xlab("Età")+
  ggtitle("Statistiche descrittive")
)

######

# plot continuous 

effAge = data.frame(age=newdata$age,predict(fitAge,newdata=newdata,se.fit=T))
effAge$lower = effAge$fit + effAge$se.fit*qnorm(.025)
effAge$upper = effAge$fit + effAge$se.fit*qnorm(.975)
effAge$agerescaled = 1+(length(levels(as.factor(group)))-1)*((effAge$age-min(effAge$age))/(max(effAge$age)-min(effAge$age)))

(plotcontinuous = ggplot()+
  coord_cartesian(ylim=c(ymin,ymax))+
  geom_point(data=effGroup,aes(x=group,y=fit),size=4)+
  geom_errorbar(data=effGroup,aes(x=group,ymin=lower,ymax=upper),width=.25,linewidth=0.6)+
  geom_line(data=effGroup,aes(x=group,y=fit,group=1),linewidth=0.6)+
  geom_ribbon(data=effAge,aes(x=agerescaled,ymin=lower,ymax=upper),fill="blue",alpha=.2)+
  geom_line(data=effAge,aes(x=agerescaled,y=fit),color="blue",linewidth=1)+
  theme(text=element_text(size=25)) + 
  scale_y_continuous(breaks=seq(0,100,1)) +
  ylab("Punt. medio") + xlab("Età")+
    ggtitle("Statistiche descrittive e Continuous norming")
)

######

# plot discrete 

newdata = data.frame(age=seq(5.5,14.5,1))

effAgeD = data.frame(age=newdata$age,predict(fitAge,newdata=newdata,se.fit=T))
effAgeD$lower = effAgeD$fit + effAgeD$se.fit*qnorm(.025)
effAgeD$upper = effAgeD$fit + effAgeD$se.fit*qnorm(.975)
effAgeD$agerescaled = 1+(length(levels(as.factor(group)))-1)*((effAgeD$age-min(effAgeD$age))/(max(effAgeD$age)-min(effAgeD$age)))
effAgeD$group = factor(levels(df$group),levels=levels(df$group))

(plotdiscrete = ggplot()+
  coord_cartesian(ylim=c(ymin,ymax))+
  geom_point(data=effAgeD,aes(x=group,y=fit),size=4,color="blue")+
  geom_errorbar(data=effAgeD,aes(x=agerescaled,ymin=lower,ymax=upper),color="blue",width=0,linewidth=1)+
  geom_line(data=effAgeD,aes(x=agerescaled,y=fit),color="blue",linewidth=1)+
  theme(text=element_text(size=25)) + 
  scale_y_continuous(breaks=seq(0,100,1)) +
  ylab("Punt. medio") + xlab("Età")+
  ggtitle("Solo continuous norming")
)

#########################################

# plot discrete + continuous

(plotboth = ggplot()+
   geom_point(data=effAgeD,aes(x=group,y=fit),size=4,color="blue")+
   geom_errorbar(data=effAgeD,aes(x=agerescaled,ymin=lower,ymax=upper),color="blue",width=.2,linewidth=1.5,alpha=.5)+
   geom_ribbon(data=effAge,aes(x=agerescaled,ymin=lower,ymax=upper),fill="blue",alpha=.2)+
   geom_line(data=effAge,aes(x=agerescaled,y=fit),color="blue",linewidth=1)+
   geom_point(data=effGroup,aes(x=group,y=fit),size=4)+
   geom_errorbar(data=effGroup,aes(x=group,ymin=lower,ymax=upper),width=.25,linewidth=0.6)+
   geom_line(data=effGroup,aes(x=group,y=fit,group=1),linewidth=0.6)+   
   coord_cartesian(ylim=c(ymin,ymax))+
   theme(text=element_text(size=25)) + 
   scale_y_continuous(breaks=seq(0,100,1)) +
   ylab("Punt. medio") + xlab("Età")+
   ggtitle("Statistiche descrittive e Continuous norming")
)

#########################################

save.image("continuousNorming.RData")

#########################################
