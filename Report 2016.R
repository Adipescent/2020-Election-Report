library(lubridate) ##this is to change the format of date
current_address=getwd() 
polls_data_2016=read.csv(paste0(current_address,"/data/president_general_polls_sorted_end_date_2016.csv"))

index_date_aug_nov=which(polls_data_2016$enddate>="2016-08-01")


## 1a for Clinton
aug_date=mdy(polls_data_2016$enddate)
index_Minn=which(polls_data_2016$state=="Minnesota" & aug_date>="2016-08-01")
index_Minn
m1 = sum(polls_data_2016$total.clinton[index_Minn])
index_NC=which(polls_data_2016$state=="North Carolina" & aug_date>="2016-08-01")
m2 = sum(polls_data_2016$total.clinton[index_NC])
index_Fl=which(polls_data_2016$state=="Florida" & aug_date>="2016-08-01")
m3 = sum(polls_data_2016$total.clinton[index_Fl])


#1a for Trump
t1=sum(polls_data_2016$total.trump[index_Minn])
t2=sum(polls_data_2016$total.trump[index_NC])
t3=sum(polls_data_2016$total.trump[index_Fl])

#1a
m1/(m1+t1)-t1/(m1+t1) #Clinton win
m2/(m2+t2)-t2/(m2+t2) #Trump win
m3/(m3+t3)-t3/(m3+t3) #Trump win

##====================================================================================================================================
#1b
#Plot for Minnesota ##need from August 1 to November 8 


polls_data_2016$enddate
##the current date is month date year, the follow linesconvert to the typical year month date data

date_Minn <- mdy(polls_data_2016$enddate[index_Minn])
date_Minn
#plot the poll by Clinton and Trump over time in Minn
ylim_value=c(min(polls_data_2016$total.clinton[index_Minn],polls_data_2016$total.trump[index_Minn]),
             max(polls_data_2016$total.clinton[index_Minn],polls_data_2016$total.trump[index_Minn]))
plot(date_Minn,polls_data_2016$total.clinton[index_Minn],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Minnesota',ylim=ylim_value)
lines(date_Minn,polls_data_2016$total.trump[index_Minn],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_Minn,polls_data_2016$total.clinton[index_Minn]-polls_data_2016$total.trump[index_Minn],
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Minnesota')
abline(a=0,b=0)

#plot the percentage difference 
percentage_diff_Minn=(polls_data_2016$total.clinton[index_Minn]-polls_data_2016$total.trump[index_Minn])/(polls_data_2016$total.clinton[index_Minn]+polls_data_2016$total.trump[index_Minn])
plot(date_Minn,percentage_diff_Minn,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
abline(a=0,b=0)

#---------------------------------------------------------------------------------------------------------------------------------

#Plot for North Carolina
date_NC <- mdy(polls_data_2016$enddate[index_NC])
date_NC
#plot the poll by Clinton and Trump over time in NC
ylim_value=c(min(polls_data_2016$total.clinton[index_NC],polls_data_2016$total.trump[index_NC]),
             max(polls_data_2016$total.clinton[index_NC],polls_data_2016$total.trump[index_NC]))
plot(date_NC,polls_data_2016$total.clinton[index_NC],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='North Carolina',ylim=ylim_value)
lines(date_NC,polls_data_2016$total.trump[index_NC],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_NC,polls_data_2016$total.clinton[index_NC]-polls_data_2016$total.trump[index_NC],
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='North Carolina')
abline(a=0,b=0)

#plot the percentage difference 
percentage_diff_NC=(polls_data_2016$total.clinton[index_NC]-polls_data_2016$total.trump[index_NC])/(polls_data_2016$total.clinton[index_NC]+polls_data_2016$total.trump[index_NC])
plot(date_NC,percentage_diff_NC,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
abline(a=0,b=0)


#----------------------------------------------------------------------------------------------------------------------------------
#plot for Florida
date_Fl <- mdy(polls_data_2016$enddate[index_Fl])
date_Fl
#plot the poll by Clinton and Trump over time in Fl
ylim_value=c(min(polls_data_2016$total.clinton[index_Fl],polls_data_2016$total.trump[index_Fl]),
             max(polls_data_2016$total.clinton[index_Fl],polls_data_2016$total.trump[index_Fl]))
plot(date_Fl,polls_data_2016$total.clinton[index_Fl],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Florida',ylim=ylim_value)
lines(date_Fl,polls_data_2016$total.trump[index_Fl],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_Fl,polls_data_2016$total.clinton[index_Fl]-polls_data_2016$total.trump[index_Fl],
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Florida')
abline(a=0,b=0)

#plot the percentage difference 
percentage_diff_Fl=(polls_data_2016$total.clinton[index_Fl]-polls_data_2016$total.trump[index_Fl])/(polls_data_2016$total.clinton[index_Fl]+polls_data_2016$total.trump[index_Fl])
plot(date_Fl,percentage_diff_Fl,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
abline(a=0,b=0)


##ggplot will help draw graph with points you can use plot or ggplot

##===================================================================================================================================
#1c
#Minnesota
index_Minn=which(polls_data_2016$state=="Minnesota")
date_Minn <- mdy(polls_data_2016$enddate[index_Minn])

counts_Minn_for_lm <- data.frame(
  data_date = date_Minn,
  percentage_diff = (polls_data_2016$total.clinton[index_Minn]-polls_data_2016$total.trump[index_Minn])/(polls_data_2016$total.clinton[index_Minn]+polls_data_2016$total.trump[index_Minn])
)

index_date_oct_nov=which(counts_Minn_for_lm$data_date>="2016-10-01")

counts_Minn_for_lm_oct <- data.frame(
  data_date = date_Minn[index_date_oct_nov],
  percentage_diff =counts_Minn_for_lm$percentage_diff[index_date_oct_nov]
)

as.numeric(date_Minn)
lm_model_Minn=lm(percentage_diff~(data_date),data=counts_Minn_for_lm)
summary(lm_model_Minn)

lm_model_Minn_after_oct=lm(percentage_diff~(data_date),data=counts_Minn_for_lm_oct)
summary(lm_model_Minn_after_oct)

conf_interval_Minn_fitted_oct= predict(lm_model_Minn_after_oct, newdata=counts_Minn_for_lm_oct, interval="confidence", level = 0.95)
#plot
plot(counts_Minn_for_lm_oct$data_date,counts_Minn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
lines(counts_Minn_for_lm_oct$data_date,lm_model_Minn_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Minnesota')

#plot with 95% CI
plot(counts_Minn_for_lm_oct$data_date,counts_Minn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
polygon(c(rev(counts_Minn_for_lm_oct$data_date), counts_Minn_for_lm_oct$data_date), 
        c(rev(conf_interval_Minn_fitted_oct[,2]), conf_interval_Minn_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_Minn_for_lm_oct$data_date,counts_Minn_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
lines(counts_Minn_for_lm_oct$data_date,lm_model_Minn_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Minnesota')

#----------------------------------------------------------------------------------------------------------------------------
#North Carolina
index_NC=which(polls_data_2016$state=="North Carolina")
date_NC <- mdy(polls_data_2016$enddate[index_NC])

counts_NC_for_lm <- data.frame(
  data_date = date_NC,
  percentage_diff = (polls_data_2016$total.clinton[index_NC]-polls_data_2016$total.trump[index_NC])/(polls_data_2016$total.clinton[index_NC]+polls_data_2016$total.trump[index_NC])
)

index_date_oct_nov=which(counts_NC_for_lm$data_date>="2016-10-01")

counts_NC_for_lm_oct <- data.frame(
  data_date = date_NC[index_date_oct_nov],
  percentage_diff =counts_NC_for_lm$percentage_diff[index_date_oct_nov]
)

counts_NC_for_lm_oct$data_date

lm_model_NC_after_oct=lm(percentage_diff~(data_date),data=counts_NC_for_lm_oct)
summary(lm_model_NC_after_oct)

conf_interval_NC_fitted_oct= predict(lm_model_NC_after_oct, newdata=counts_NC_for_lm_oct, interval="confidence",
                                       level = 0.95)

##plot
plot(counts_NC_for_lm_oct$data_date,counts_NC_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_NC_for_lm_oct$data_date,lm_model_NC_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')

##this is a plot with 95% interval
plot(counts_NC_for_lm_oct$data_date,counts_NC_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_NC_for_lm_oct$data_date), counts_NC_for_lm_oct$data_date), 
        c(rev(conf_interval_NC_fitted_oct[,2]), conf_interval_NC_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_NC_for_lm_oct$data_date,counts_NC_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_NC_for_lm_oct$data_date,lm_model_NC_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')
##the interval does not contain zero, think about what assumption could go wrong

#-------------------------------------------------------------------------------------------------------------------------------
#Florida
index_Fl=which(polls_data_2016$state=="Florida")
date_Fl <- mdy(polls_data_2016$enddate[index_Fl])

counts_Fl_for_lm <- data.frame(
  data_date = date_Fl,
  percentage_diff = (polls_data_2016$total.clinton[index_Fl]-polls_data_2016$total.trump[index_Fl])/(polls_data_2016$total.clinton[index_Fl]+polls_data_2016$total.trump[index_Fl])
)

index_date_oct_nov=which(counts_Fl_for_lm$data_date>="2016-10-01")

counts_Fl_for_lm_oct <- data.frame(
  data_date = date_Fl[index_date_oct_nov],
  percentage_diff =counts_Fl_for_lm$percentage_diff[index_date_oct_nov]
)

counts_Fl_for_lm_oct$data_date

lm_model_Fl_after_oct=lm(percentage_diff~(data_date),data=counts_Fl_for_lm_oct)
summary(lm_model_Fl_after_oct)

conf_interval_Fl_fitted_oct= predict(lm_model_Fl_after_oct, newdata=counts_Fl_for_lm_oct, interval="confidence",
                                       level = 0.95)

##this is probably convincing, the lead from Clinton was clearly declining in Flslvannia between Oct and Nov
plot(counts_Fl_for_lm_oct$data_date,counts_Fl_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
lines(counts_Fl_for_lm_oct$data_date,lm_model_Fl_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Florida')

##this is a plot with 95% interval
plot(counts_Fl_for_lm_oct$data_date,counts_Fl_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
polygon(c(rev(counts_Fl_for_lm_oct$data_date), counts_Fl_for_lm_oct$data_date), 
        c(rev(conf_interval_Fl_fitted_oct[,2]), conf_interval_Fl_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_Fl_for_lm_oct$data_date,counts_Fl_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
lines(counts_Fl_for_lm_oct$data_date,lm_model_Fl_after_oct$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Florida')
##the interval does not contain zero, think about what assumption could go wrong


#-------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,3))
##Minn
plot(counts_Minn_for_lm_oct$data_date,counts_Minn_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
polygon(c(rev(counts_Minn_for_lm_oct$data_date), counts_Minn_for_lm_oct$data_date), 
        c(rev(conf_interval_Minn_fitted_oct[,2]), conf_interval_Minn_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_Minn_for_lm_oct$data_date,counts_Minn_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p')
lines(counts_Minn_for_lm_oct$data_date,lm_model_Minn_after_oct$fitted.values,
      col='black',pch=20,type='l')
##Fl
plot(counts_Fl_for_lm_oct$data_date,counts_Fl_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
polygon(c(rev(counts_Fl_for_lm_oct$data_date), counts_Fl_for_lm_oct$data_date), 
        c(rev(conf_interval_Fl_fitted_oct[,2]), conf_interval_Fl_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_Fl_for_lm_oct$data_date,counts_Fl_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p')
lines(counts_Fl_for_lm_oct$data_date,lm_model_Fl_after_oct$fitted.values,
      col='black',pch=20,type='l')
##NC
plot(counts_NC_for_lm_oct$data_date,counts_NC_for_lm_oct$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_NC_for_lm_oct$data_date), counts_NC_for_lm_oct$data_date), 
        c(rev(conf_interval_NC_fitted_oct[,2]), conf_interval_NC_fitted_oct[ ,3]), col = 'grey80', border = NA)
lines(counts_NC_for_lm_oct$data_date,counts_NC_for_lm_oct$percentage_diff,
      col='black',pch=20,type='p')
lines(counts_NC_for_lm_oct$data_date,lm_model_NC_after_oct$fitted.values,
      col='black',pch=20,type='l')

#close it
dev.off()

#The state with the smallest margin would probably be North Carolina
#==========================================================================================================================
#1d Florida had the smallest margin out of the three in the actual results (1.2 margin when searched up on wiki). 
#Trump had appealed to working-class voters which may have really helped him out in winning this state.
#Florida did vote for Democrats in the previous election but with scandals and rumors about Hillary Clinton's emails might have 
#changed their favor a bit. 

plot_usmap(data = state_poll_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")
#-------------------------------------------------------------------------------------------------------------------------------------------------

##make some plot use poll from Sep 1 to Nov 2
polls_data_2016_enddate=mdy(polls_data_2016$enddate)
polls_data_2016_after_sep=polls_data_2016[which(polls_data_2016_enddate>="2016-09-01"),]

poll_state_sum_clinton_2016=aggregate(polls_data_2016_after_sep$total.clinton, by=list(State=polls_data_2016_after_sep$state),FUN=sum)
poll_state_sum_trump_2016=aggregate(polls_data_2016_after_sep$total.trump, by=list(State=polls_data_2016_after_sep$state),FUN=sum)

poll_state_diff_percentage=poll_state_sum_clinton_2016
poll_state_diff_percentage[,2]=(poll_state_sum_clinton_2016[,2]-poll_state_sum_trump_2016[,2])/(poll_state_sum_clinton_2016[,2]+poll_state_sum_trump_2016[,2])
delete_index=which(levels(poll_state_diff_percentage[,1])=='U.S.')
poll_state_diff_percentage=poll_state_diff_percentage[-delete_index,]
poll_state_diff_percentage[,1]

## for convenience let's combine those states with several subregions in a state
poll_state_diff_percentage[c(21,22,31,32,33),1]
poll_state_diff_percentage=poll_state_diff_percentage[-c(21,22,31,32,33),] ##delete maine and nebraska subregion

library(usmap)
library(ggplot2)
##create a data frame

state_poll_2016 <- data.frame(
  state =poll_state_diff_percentage[,1],
  diff_percentage=poll_state_diff_percentage[,2]
)

##this is according to the poll
plot_usmap(data = state_poll_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")
##reality
library(politicaldata)

data(pres_results)

pred_results_2016=pres_results[which(pres_results$year==2016),]

state_truth_2016<- data.frame(
  state =pred_results_2016$state,
  diff_percentage=(pred_results_2016$dem-pred_results_2016$rep)/(pred_results_2016$dem+pred_results_2016$rep)
)

plot_usmap(data = state_truth_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")
