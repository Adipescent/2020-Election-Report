current_address=getwd() 
polls_data_2020=read.csv(paste0(current_address,"/data/president_polls_2020.csv"))
library(lubridate) ##this is to change the format of date

date_2020= mdy(polls_data_2020$end_date)
date_2020_latest_day=date_2020[1]
index_selected=which(date_2020>='2020-08-31' & date_2020 <='2020-10-25') 
polls_data_2020=polls_data_2020[index_selected,]  ###only work on the poll after Aug 1
##get  values with sample size unknown
index_na=which(is.na(polls_data_2020$sample_size)==T)
index_na
##only look at Biden or Trump 
polls_data_2020=polls_data_2020[which(polls_data_2020$answer=='Biden'|polls_data_2020$answer=='Trump'),]
# ##you may delete USC Dornsife/Los Angeles Times and Survey Monkey as their polls seem not disjoint
# polls_data_2020=polls_data_2020[which(polls_data_2020$pollster_id!=1610&polls_data_2020$pollster_id!=1193),]

##they do not match. Some poll has mistakes 
length(which(polls_data_2020$answer=='Biden'))
length(which(polls_data_2020$answer=='Trump'))

##now let's delete those poll that only contains one candidate
polls_data_2020_question_id_num=unique(polls_data_2020$question_id)

for(i in 1:length(unique(polls_data_2020$question_id)) ){
  index_set=which(polls_data_2020$question_id==polls_data_2020_question_id_num[i])
  if(length(index_set)!=2){
    polls_data_2020=polls_data_2020[-index_set,]
  }
}
##now they match 
length(which(polls_data_2020$answer=='Biden'))
length(which(polls_data_2020$answer=='Trump'))
#----------------------------------------------------------------------------------------------------------------------------------
##1. look at poll for Minnesota in 2020 
date_2020= mdy(polls_data_2020$end_date)
polls_data_2020$state
index_Minn_2020=which(polls_data_2020$state=="Minnesota")

polls_data_Minn_2020=polls_data_2020[index_Minn_2020,]
polls_data_Minn_2020$end_date=mdy(polls_data_Minn_2020$end_date)

##who is leading Minnesota
index_biden_Minn_2020=which(polls_data_Minn_2020$answer=='Biden')
index_trump_Minn_2020=which(polls_data_Minn_2020$answer=='Trump' )

polls_date_Minn_2020=polls_data_Minn_2020$end_date[index_biden_Minn_2020]

polls_date_Minn_2020

counts_biden_Minn_2020=polls_data_Minn_2020$pct[index_biden_Minn_2020]*polls_data_Minn_2020$sample_size[index_biden_Minn_2020]
counts_trump_Minn_2020=polls_data_Minn_2020$pct[index_trump_Minn_2020]*polls_data_Minn_2020$sample_size[index_trump_Minn_2020]

n1_2020_Minn=sum(counts_biden_Minn_2020)
n2_2020_Minn=sum(counts_trump_Minn_2020)

n1_2020_Minn
n2_2020_Minn

(n1_2020_Minn-n2_2020_Minn)/(n1_2020_Minn+n2_2020_Minn)

percentage_diff_Minn_2020 = (counts_biden_Minn_2020-counts_trump_Minn_2020)/(counts_biden_Minn_2020+counts_trump_Minn_2020)

##now let's load data for 2016

polls_data_2016=read.csv(paste0(current_address,"/data/president_general_polls_sorted_end_date_2016.csv"))
#polls_data_2016=read.csv(paste0(current_address,"/data/president_general_polls_2016.csv"))

#polls_data_2016$enddate
index_Minn=which(polls_data_2016$state=="Minnesota")

##get data from Minn
polls_data_Minn_2016=polls_data_2016[index_Minn,]
polls_data_Minn_2016$enddate=mdy(polls_data_Minn_2016$enddate)

##get data before Oct 17
date_2020_latest_day
index_selected_Minn_2016=which(polls_data_Minn_2016$enddate>='2016-08-31'& polls_data_Minn_2016$enddate<="2016-10-25") 

polls_data_Minn_2016=polls_data_Minn_2016[index_selected_Minn_2016,]

##the current date is month date year, the follow linesconvert to the typical year month date data
#date_Minn_2016 <- polls_data_Minn_2016$enddate
#date_Minn_2016

(polls_data_2016$total.clinton[index_Minn]-polls_data_2016$total.trump[index_Minn])/(polls_data_2016$total.clinton[index_Minn]+polls_data_2016$total.trump[index_Minn])

percentage_diff_Minn_2016=(polls_data_Minn_2016$total.clinton-polls_data_Minn_2016$total.trump)/(polls_data_Minn_2016$total.clinton+polls_data_Minn_2016$total.trump)

#note this one is increasing
polls_data_Minn_2016$enddate


#month_date_Minn_2016<-format(polls_data_Minn_2016$enddate, format="%m-%d")
##project to 2020
year(polls_data_Minn_2016$enddate)=2020 

ylim_Minn=c(min(percentage_diff_Minn_2016,percentage_diff_Minn_2020),max(percentage_diff_Minn_2016,percentage_diff_Minn_2020))
plot(polls_data_Minn_2016$enddate,percentage_diff_Minn_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Minnesota',ylim=ylim_Minn)
lines(polls_date_Minn_2020,percentage_diff_Minn_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_Minn_2016=lm(percentage_diff_Minn_2016~(polls_data_Minn_2016$enddate))
lm_model_Minn_2020=lm(percentage_diff_Minn_2020~(polls_date_Minn_2020))


conf_interval_Minn_fitted_2016= predict(lm_model_Minn_2016, newdata=polls_data_Minn_2016$enddate, interval="confidence",
                                        level = 0.95)
conf_interval_Minn_fitted_2020= predict(lm_model_Minn_2020, newdata=polls_date_Minn_2020, interval="confidence",
                                        level = 0.95)

###plot difference Minn
plot(polls_data_Minn_2016$enddate,percentage_diff_Minn_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Minnesota',ylim=ylim_Minn)
lines(polls_date_Minn_2020,percentage_diff_Minn_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))
polygon(c(rev(polls_data_Minn_2016$enddate), polls_data_Minn_2016$enddate), 
        c(rev(conf_interval_Minn_fitted_2016[,2]), conf_interval_Minn_fitted_2016[ ,3]), col = 'grey70', border = NA)
polygon(c(rev(polls_date_Minn_2020), polls_date_Minn_2020), 
        c(rev(conf_interval_Minn_fitted_2020[,2]), conf_interval_Minn_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(polls_data_Minn_2016$enddate,lm_model_Minn_2016$fitted.values,
      col='purple',pch=20,type='l')
lines(polls_date_Minn_2020,lm_model_Minn_2020$fitted.values,
      col='green',pch=20,type='l')
lines(polls_date_Minn_2020,percentage_diff_Minn_2020,
      col='green',pch=20,cex=1,type='p')
lines(polls_data_Minn_2016$enddate,percentage_diff_Minn_2016,
      col='purple',pch=18,cex=1,type='p')

##fitted value, end point
conf_interval_Minn_fitted_2020[1,1]  ##the date vector for 2020 is decreasing
conf_interval_Minn_fitted_2016[length(conf_interval_Minn_fitted_2016[,1]),1] ##the date vector for 2016 is increasing
#-------------------------------------------------------------------------------------------------------------------------
##1. look at poll for Florida in 2020 
date_2020= mdy(polls_data_2020$end_date)
polls_data_2020$state
index_Fl_2020=which(polls_data_2020$state=="Florida")

polls_data_Fl_2020=polls_data_2020[index_Fl_2020,]
polls_data_Fl_2020$end_date=mdy(polls_data_Fl_2020$end_date)



##who is leading Florida
index_biden_Fl_2020=which(polls_data_Fl_2020$answer=='Biden')
index_trump_Fl_2020=which(polls_data_Fl_2020$answer=='Trump' )

polls_date_Fl_2020=polls_data_Fl_2020$end_date[index_biden_Fl_2020]

#note this one is decreasing
polls_date_Fl_2020

counts_biden_Fl_2020=polls_data_Fl_2020$pct[index_biden_Fl_2020]*polls_data_Fl_2020$sample_size[index_biden_Fl_2020]
counts_trump_Fl_2020=polls_data_Fl_2020$pct[index_trump_Fl_2020]*polls_data_Fl_2020$sample_size[index_trump_Fl_2020]

n1_2020_Fl=sum(counts_biden_Fl_2020)
n2_2020_Fl=sum(counts_trump_Fl_2020)

n1_2020_Fl
n2_2020_Fl

(n1_2020_Fl-n2_2020_Fl)/(n1_2020_Fl+n2_2020_Fl)

percentage_diff_Fl_2020 = (counts_biden_Fl_2020-counts_trump_Fl_2020)/(counts_biden_Fl_2020+counts_trump_Fl_2020)

#Data for 2016

#polls_data_2016$enddate
index_Fl=which(polls_data_2016$state=="Florida")

##get data from Fl
polls_data_Fl_2016=polls_data_2016[index_Fl,]
polls_data_Fl_2016$enddate=mdy(polls_data_Fl_2016$enddate)

##get data before Oct 17
date_2020_latest_day
index_selected_Fl_2016=which(polls_data_Fl_2016$enddate>='2016-08-31'& polls_data_Fl_2016$enddate<="2016-10-25") 

polls_data_Fl_2016=polls_data_Fl_2016[index_selected_Fl_2016,]

##the current date is month date year, the follow linesconvert to the typical year month date data
#date_Fl_2016 <- polls_data_Fl_2016$enddate
#date_Fl_2016

(polls_data_2016$total.clinton[index_Fl]-polls_data_2016$total.trump[index_Fl])/(polls_data_2016$total.clinton[index_Fl]+polls_data_2016$total.trump[index_Fl])

percentage_diff_Fl_2016=(polls_data_Fl_2016$total.clinton-polls_data_Fl_2016$total.trump)/(polls_data_Fl_2016$total.clinton+polls_data_Fl_2016$total.trump)

#note this one is increasing
polls_data_Fl_2016$enddate


#month_date_Fl_2016<-format(polls_data_Fl_2016$enddate, format="%m-%d")
##project to 2020
year(polls_data_Fl_2016$enddate)=2020 

ylim_Fl=c(min(percentage_diff_Fl_2016,percentage_diff_Fl_2020),max(percentage_diff_Fl_2016,percentage_diff_Fl_2020))
plot(polls_data_Fl_2016$enddate,percentage_diff_Fl_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Florida',ylim=ylim_Fl)
lines(polls_date_Fl_2020,percentage_diff_Fl_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_Fl_2016=lm(percentage_diff_Fl_2016~(polls_data_Fl_2016$enddate))
lm_model_Fl_2020=lm(percentage_diff_Fl_2020~(polls_date_Fl_2020))


conf_interval_Fl_fitted_2016= predict(lm_model_Fl_2016, newdata=polls_data_Fl_2016$enddate, interval="confidence",
                                        level = 0.95)
conf_interval_Fl_fitted_2020= predict(lm_model_Fl_2020, newdata=polls_date_Fl_2020, interval="confidence",
                                        level = 0.95)

###plot difference Fl
plot(polls_data_Fl_2016$enddate,percentage_diff_Fl_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Florida',ylim=ylim_Fl)
lines(polls_date_Fl_2020,percentage_diff_Fl_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))
polygon(c(rev(polls_data_Fl_2016$enddate), polls_data_Fl_2016$enddate), 
        c(rev(conf_interval_Fl_fitted_2016[,2]), conf_interval_Fl_fitted_2016[ ,3]), col = 'grey70', border = NA)
polygon(c(rev(polls_date_Fl_2020), polls_date_Fl_2020), 
        c(rev(conf_interval_Fl_fitted_2020[,2]), conf_interval_Fl_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(polls_data_Fl_2016$enddate,lm_model_Fl_2016$fitted.values,
      col='purple',pch=20,type='l')
lines(polls_date_Fl_2020,lm_model_Fl_2020$fitted.values,
      col='green',pch=20,type='l')
lines(polls_date_Fl_2020,percentage_diff_Fl_2020,
      col='green',pch=20,cex=1,type='p')
lines(polls_data_Fl_2016$enddate,percentage_diff_Fl_2016,
      col='purple',pch=18,cex=1,type='p')

##fitted value, end point
conf_interval_Fl_fitted_2020[1,1]  ##the date vector for 2020 is decreasing
conf_interval_Fl_fitted_2016[length(conf_interval_Fl_fitted_2016[,1]),1] ##the date vector for 2016 is increasing
#----------------------------------------------------------------------------------------------------------------------------------
###let's take a look at NC

##1. look at poll for NC in 2020 
index_nc_2020=which(polls_data_2020$state=="North Carolina")

polls_data_nc_2020=polls_data_2020[index_nc_2020,]
polls_data_nc_2020$end_date=mdy(polls_data_nc_2020$end_date)


#note this one is decreasing
polls_date_nc_2020
##who is leading ncsylvania
index_biden_nc_2020=which(polls_data_nc_2020$answer=='Biden')
index_trump_nc_2020=which(polls_data_nc_2020$answer=='Trump' )


polls_date_nc_2020=polls_data_nc_2020$end_date[index_biden_nc_2020]


counts_biden_nc_2020=polls_data_nc_2020$pct[index_biden_nc_2020]*polls_data_nc_2020$sample_size[index_biden_nc_2020]
counts_trump_nc_2020=polls_data_nc_2020$pct[index_trump_nc_2020]*polls_data_nc_2020$sample_size[index_trump_nc_2020]

n1_2020_nc=sum(counts_biden_nc_2020)
n2_2020_nc=sum(counts_trump_nc_2020)

n1_2020_nc
n2_2020_nc

(n1_2020_nc-n2_2020_nc)/(n1_2020_nc+n2_2020_nc)

percentage_diff_nc_2020 = (counts_biden_nc_2020-counts_trump_nc_2020)/(counts_biden_nc_2020+counts_trump_nc_2020)


##now let's load data for 2016

#polls_data_2016$enddate
index_nc=which(polls_data_2016$state=="North Carolina")

##get data from nc
polls_data_nc_2016=polls_data_2016[index_nc,]
polls_data_nc_2016$enddate=mdy(polls_data_nc_2016$enddate)

##get data before Oct 17
date_2020_latest_day
index_selected_nc_2016=which(polls_data_nc_2016$enddate>='2016-08-31'& polls_data_nc_2016$enddate<="2016-10-25") 

polls_data_nc_2016=polls_data_nc_2016[index_selected_nc_2016,]

##the current date is month date year, the follow linesconvert to the typical year month date data
#date_nc_2016 <- polls_data_nc_2016$enddate
#date_nc_2016

(polls_data_2016$total.clinton[index_nc]-polls_data_2016$total.trump[index_nc])/(polls_data_2016$total.clinton[index_nc]+polls_data_2016$total.trump[index_nc])

percentage_diff_nc_2016=(polls_data_nc_2016$total.clinton-polls_data_nc_2016$total.trump)/(polls_data_nc_2016$total.clinton+polls_data_nc_2016$total.trump)

#note this one is increasing
polls_data_nc_2016$enddate


#month_date_nc_2016<-format(polls_data_nc_2016$enddate, format="%m-%d")
##project to 2020
year(polls_data_nc_2016$enddate)=2020 

ylim_nc=c(min(percentage_diff_nc_2016,percentage_diff_nc_2020),max(percentage_diff_nc_2016,percentage_diff_nc_2020))
plot(polls_data_nc_2016$enddate,percentage_diff_nc_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='North Carolina',ylim=ylim_nc)
lines(polls_date_nc_2020,percentage_diff_nc_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_nc_2016=lm(percentage_diff_nc_2016~(polls_data_nc_2016$enddate))
lm_model_nc_2020=lm(percentage_diff_nc_2020~(polls_date_nc_2020))


conf_interval_nc_fitted_2016= predict(lm_model_nc_2016, newdata=polls_data_nc_2016$enddate, interval="confidence",
                                      level = 0.95)
conf_interval_nc_fitted_2020= predict(lm_model_nc_2020, newdata=polls_date_nc_2020, interval="confidence",
                                      level = 0.95)

###plot difference nc
plot(polls_data_nc_2016$enddate,percentage_diff_nc_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='North Carolina',ylim=ylim_nc)
lines(polls_date_nc_2020,percentage_diff_nc_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))
polygon(c(rev(polls_data_nc_2016$enddate), polls_data_nc_2016$enddate), 
        c(rev(conf_interval_nc_fitted_2016[,2]), conf_interval_nc_fitted_2016[ ,3]), col = 'grey70', border = NA)
polygon(c(rev(polls_date_nc_2020), polls_date_nc_2020), 
        c(rev(conf_interval_nc_fitted_2020[,2]), conf_interval_nc_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(polls_data_nc_2016$enddate,lm_model_nc_2016$fitted.values,
      col='purple',pch=20,type='l')
lines(polls_date_nc_2020,lm_model_nc_2020$fitted.values,
      col='green',pch=20,type='l')
lines(polls_date_nc_2020,percentage_diff_nc_2020,
      col='green',pch=20,cex=1,type='p')
lines(polls_data_nc_2016$enddate,percentage_diff_nc_2016,
      col='purple',pch=18,cex=1,type='p')

##fitted value, end point
conf_interval_nc_fitted_2020[1,1]  ##the date vector for 2020 is decreasing
conf_interval_nc_fitted_2016[length(conf_interval_nc_fitted_2016[,1]),1] ##the date vector for 2016 is increasing


#-----------------------------------------------------------------------------------------------------------------------------

###compare average from Aug 31 to Oct 25

polls_data_2016_enddate=mdy(polls_data_2016$enddate)
polls_data_2016_after_sep=polls_data_2016[which(polls_data_2016_enddate>="2016-08-31"&polls_data_2016_enddate<="2016-10-25"),]

poll_state_sum_clinton_2016=aggregate(polls_data_2016_after_sep$total.clinton, by=list(State=polls_data_2016_after_sep$state),FUN=sum)
poll_state_sum_trump_2016=aggregate(polls_data_2016_after_sep$total.trump, by=list(State=polls_data_2016_after_sep$state),FUN=sum)

poll_state_diff_percentage=poll_state_sum_clinton_2016
poll_state_diff_percentage[,2]=(poll_state_sum_clinton_2016[,2]-poll_state_sum_trump_2016[,2])/(poll_state_sum_clinton_2016[,2]+poll_state_sum_trump_2016[,2])
delete_index=which(levels(poll_state_diff_percentage[,1])=='U.S.')
#poll_state_diff_percentage=poll_state_diff_percentage[-delete_index,]
#poll_state_diff_percentage[,1]

## for convenience let's combine those states with several subregions in a state
poll_state_diff_percentage[c(21,22,31,32,33),1]
poll_state_diff_percentage=poll_state_diff_percentage[-c(21,22,31,32,33),] ##delete maine and nebraska subregion

library(usmap)
library(ggplot2) ##note it needs this one

##create a data frame

state_poll_2016 <- data.frame(
  state =poll_state_diff_percentage[,1],
  diff_percentage=poll_state_diff_percentage[,2]
)




##let's look at 2020
index_selected=which(date_2020>='2020-08-31') 
polls_data_2020_after_sep=polls_data_2020[index_selected,]  ###only work on the poll after Aug 1


polls_data_2020_after_sep=polls_data_2020_after_sep[which(polls_data_2020$answer=='Biden'|polls_data_2020$answer=='Trump'),]

index_biden_2020=which(polls_data_2020_after_sep$answer=='Biden')
index_trump_2020=which(polls_data_2020_after_sep$answer=='Trump' )


counts_biden_2020=polls_data_2020$pct[index_biden_2020]*polls_data_2020$sample_size[index_biden_2020]
counts_trump_2020=polls_data_2020$pct[index_trump_2020]*polls_data_2020$sample_size[index_trump_2020]

##add two column
polls_data_2020$total.biden=rep(0,dim(polls_data_2020)[1])
polls_data_2020$total.trump=rep(0,dim(polls_data_2020)[1])

polls_data_2020$total.biden[index_biden_2020]=counts_biden_2020
polls_data_2020$total.trump[index_trump_2020]=counts_trump_2020

poll_state_sum_biden_2020=aggregate(polls_data_2020$total.biden, by=list(State=polls_data_2020$state),FUN=sum)
poll_state_sum_trump_2020=aggregate(polls_data_2020$total.trump, by=list(State=polls_data_2020$state),FUN=sum)

#delete the one with NA
poll_state_sum_biden_2020=poll_state_sum_biden_2020[-1,]
poll_state_sum_trump_2020=poll_state_sum_trump_2020[-1,]

##create a data frame
state_poll_2020 <- data.frame(
  state =poll_state_sum_biden_2020[,1],
  diff_percentage=(poll_state_sum_biden_2020[,2]-poll_state_sum_trump_2020[,2])/(poll_state_sum_biden_2020[,2]+poll_state_sum_trump_2020[,2])
)



limit_val=c(min(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage),
            max(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage))
##2016
plot_usmap(data = state_poll_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2016") 
##2020
plot_usmap(data = state_poll_2020, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2020") 

##difference between 2020 and 2016
##delete nebrask CD-1 and CD-3, as 2020 does not have it 
state_poll_2016$state
state_poll_2020$state
state_poll_2016=state_poll_2016[-c(31,33),]


state_poll_2020_2016_diff <- data.frame(
  state =state_poll_2020$state,diff=state_poll_2020$diff_percentage-state_poll_2016$diff_percentage)

plot_usmap(data = state_poll_2020_2016_diff, values = "diff", color = "black") +
  scale_fill_gradient2(name = "difference between 2016 and 2020 (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")+
  ggtitle("difference between 2020 and 2016") 
#4b Iowa, Texas, Ohio, Florida, Georgia, North Carolina, South Carolina, Missouri, Nebraska, Nevada, Pennsylvania
# While on the plot these may look like battleground states, it may be because of bias. Some of the states in the middle
# used to always be republican, yet it shows up as a battle ground state, such as Texas. Also this election and the last one, 
# did not feel like regular elections. Covid-19 may also be affecting who votes for who.

#4c The difference of the polls plot make the US seem like a very democratic country overall. Looking at the individual plots though,
#make 2016 seem very much in favor of Republican while, the 2020 polls seem to lean towards democratic. It seems like in 2020 there
#is a much bigger difference in favor of democrats, while some red states are turning into battleground states. 

#================================================================================================================================
#5

index_biden_Ga_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Georgia")
index_trump_Ga_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Georgia")

counts_biden_Ga_2020=polls_data_2020$pct[index_biden_Ga_2020]*polls_data_2020$sample_size[index_biden_Ga_2020]
counts_trump_Ga_2020=polls_data_2020$pct[index_trump_Ga_2020]*polls_data_2020$sample_size[index_trump_Ga_2020]

n1_2020_Ga=sum(counts_biden_Ga_2020)
n2_2020_Ga=sum(counts_trump_Ga_2020)

n1_2020_Ga
n2_2020_Ga

(n1_2020_Ga-n2_2020_Ga)/(n1_2020_Ga+n2_2020_Ga)

#plot the poll by Biden and Trump over time in Ga
ylim_value=c(min(counts_biden_Ga_2020,counts_trump_Ga_2020),
             max(counts_biden_Ga_2020,counts_trump_Ga_2020))
plot(date_2020[index_biden_Ga_2020],counts_biden_Ga_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Georgia',ylim=ylim_value)
lines(date_2020[index_trump_Ga_2020],counts_trump_Ga_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_2020[index_trump_Ga_2020],counts_biden_Ga_2020-counts_trump_Ga_2020,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Georgia')
abline(a=0,b=0)
##plot percentage
plot(date_2020[index_trump_Ga_2020],(counts_biden_Ga_2020-counts_trump_Ga_2020)/(counts_biden_Ga_2020+counts_trump_Ga_2020),
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Georgia')
abline(a=0,b=0)

##early date may not matter 
counts_Ga_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_Ga_2020],
  percentage_diff = (counts_biden_Ga_2020-counts_trump_Ga_2020)/(counts_biden_Ga_2020+counts_trump_Ga_2020)
)

lm_model_Ga_2020=lm(percentage_diff~(data_date),data=counts_Ga_for_lm_2020)
summary(lm_model_Ga_2020)

conf_interval_Ga_fitted_2020= predict(lm_model_Ga_2020, newdata=counts_Ga_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_Ga_for_lm_2020$data_date,counts_Ga_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')
polygon(c(rev(counts_Ga_for_lm_2020$data_date), counts_Ga_for_lm_2020$data_date), 
        c(rev(conf_interval_Ga_fitted_2020[,2]), conf_interval_Ga_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_Ga_for_lm_2020$data_date,lm_model_Ga_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Georgia')
lines(counts_Ga_for_lm_2020$data_date,counts_Ga_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Georgia')

#---------------------------------------------------------------------------------------------------------------------------
#Since Texas seemed to have a very small margin on the 2020 map, I decided to check it in detail. Looking closer I don't think this 
#state will be changing their electoral vote. There is a rather clear trend that is going downwards, however the actual range of the
#data is very small. A reason for this very close difference may be that its population has been increasing quite a lot changing
#the demographics and bringing along people with different ideas for their president.The very close margin may also be because
#a lot of people from California has also moved to Texas. 

#Wisconsin seems to be a state that may change its electoral candidate. Last election the Republican party won with a 
#very small margin. It may be that this election, the democrats will win. Looking at the US map of 2020 and 2016, the percentage
#difference looks almost the same, but it looks slightly more blue in this election. With Trump winning by a very small margin
#last election and the percentage difference looking more blue this election, it is very possible that Wisconson will change. 
 

#North Carolina may also change their electoral vote to democrat. Like Minnesota, this state also won by a relatively small margin. In the map it seems to be more slightly blue. 

#Florida also seems like it will change its electoral candidate from 2016. The percentage difference in 2016 has a very slight decrease
#meaning that Republicans were gaining ground, but in 2020, the trend is basically a straight line, but it is in the positive so it may
#mean that democrats will win in Florida. 

#Florida will probably not change look at iowa

#Pennsylvania also seems like a state that may change its candidate from the last election. In 2016 there was a rather clear downward
#trend in percentage, however during this year there is a somewhat increasing trend. 
#I think a lot of the reason for why these states may change their candidate is that they may not have been happy with the condition
#that the US is in right now and may be blaming President Trump for it. Some things like riots and the pandemic are very big influencers
#that may work against the Republican party, regardless of it is or it isn't their fault. 