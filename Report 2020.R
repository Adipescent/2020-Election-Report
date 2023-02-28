current_address=getwd()
polls_data_2020=read.csv(paste0(current_address,"/data/president_polls_2020.csv"))
library(lubridate)

date_2020= mdy(polls_data_2020$end_date)
date_2020_latest_day=date_2020[1]
index_selected=which(date_2020>='2020-08-01'& date_2020<='2020-10-25') 
#index_selected=which(date_2020>='2020-09-26'& date_2020<='2020-10-25') 
polls_data_2020=polls_data_2020[index_selected,] ## poll after august 1

index_na=which(is.na(polls_data_2020$sample_size)==T)
index_na

##only look at Biden or Trump 
polls_data_2020=polls_data_2020[which(polls_data_2020$answer=='Biden'|polls_data_2020$answer=='Trump'),]

##they do not match. Some poll has mistakes 
length(which(polls_data_2020$answer=='Biden'))
length(which(polls_data_2020$answer=='Trump'))
#delete polls with only one candidate
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


index_trump=which(polls_data_2020$answer=='Trump')
index_biden=which(polls_data_2020$answer=='Biden')
##total counts to trump
total_count_trump=sum(polls_data_2020$pct[index_trump]*polls_data_2020$sample_size[index_trump])
##total counts to biden 
total_count_biden=sum(polls_data_2020$pct[index_biden]*polls_data_2020$sample_size[index_biden])

#========================================================================================================================
#2a
##who is leading Minnesota
index_biden_Minn_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Minnesota")
index_trump_Minn_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Minnesota")

counts_biden_Minn_2020=polls_data_2020$pct[index_biden_Minn_2020]*polls_data_2020$sample_size[index_biden_Minn_2020]
counts_trump_Minn_2020=polls_data_2020$pct[index_trump_Minn_2020]*polls_data_2020$sample_size[index_trump_Minn_2020]

n1_2020_Minn=sum(counts_biden_Minn_2020) #1102429.45 may change with more data
n2_2020_Minn=sum(counts_trump_Minn_2020) #909879.17 may also change 

(n1_2020_Minn-n2_2020_Minn)/(n1_2020_Minn+n2_2020_Minn) # percent diff for Minnesota
#----------------------------------------------------------------------------------------------------------------------
##who is leading Florida
index_biden_Fl_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="Florida")
index_trump_Fl_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="Florida")

counts_biden_Fl_2020=polls_data_2020$pct[index_biden_Fl_2020]*polls_data_2020$sample_size[index_biden_Fl_2020]
counts_trump_Fl_2020=polls_data_2020$pct[index_trump_Fl_2020]*polls_data_2020$sample_size[index_trump_Fl_2020]

n1_2020_Fl=sum(counts_biden_Fl_2020) 
n2_2020_Fl=sum(counts_trump_Fl_2020) 

n1_2020_Fl
n2_2020_Fl

(n1_2020_Fl-n2_2020_Fl)/(n1_2020_Fl+n2_2020_Fl) #percent diff for Florida
#Biden is leading Florida
#--------------------------------------------------------------------------------------------------------------------
##who is leading North Carolina
index_biden_NC_2020=which(polls_data_2020$answer=='Biden' & polls_data_2020$state=="North Carolina")
index_trump_NC_2020=which(polls_data_2020$answer=='Trump' & polls_data_2020$state=="North Carolina")

counts_biden_NC_2020=polls_data_2020$pct[index_biden_NC_2020]*polls_data_2020$sample_size[index_biden_NC_2020]
counts_trump_NC_2020=polls_data_2020$pct[index_trump_NC_2020]*polls_data_2020$sample_size[index_trump_NC_2020]

n1_2020_NC=sum(counts_biden_NC_2020)#2429712
n2_2020_NC=sum(counts_trump_NC_2020)#2333135

n1_2020_NC
n2_2020_NC

(n1_2020_NC-n2_2020_NC)/(n1_2020_NC+n2_2020_NC) #percent diff for NC 
#Biden currently is ahead in all states, but may change with more data
#========================================================================================================================
#1b
#plot the poll by Biden and Trump over time in Minn
ylim_value=c(min(counts_biden_Minn_2020,counts_trump_Minn_2020),
             max(counts_biden_Minn_2020,counts_trump_Minn_2020))
plot(date_2020[index_biden_Minn_2020],counts_biden_Minn_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Minnesota',ylim=ylim_value)
lines(date_2020[index_trump_Minn_2020],counts_trump_Minn_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_2020[index_trump_Minn_2020],counts_biden_Minn_2020-counts_trump_Minn_2020,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Minnesota')
abline(a=0,b=0)
##plot percentage
plot(date_2020[index_trump_Minn_2020],(counts_biden_Minn_2020-counts_trump_Minn_2020)/(counts_biden_Minn_2020+counts_trump_Minn_2020),
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts %',main='Minnesota')
abline(a=0,b=0)

#--------------------------------------------------------------------------------------------------------------------------
#plot the poll by Biden and Trump over time in Fl
ylim_value=c(min(counts_biden_Fl_2020,counts_trump_Fl_2020),
             max(counts_biden_Fl_2020,counts_trump_Fl_2020))
plot(date_2020[index_biden_Fl_2020],counts_biden_Fl_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Florida',ylim=ylim_value)
lines(date_2020[index_trump_Fl_2020],counts_trump_Fl_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_2020[index_trump_Fl_2020],counts_biden_Fl_2020-counts_trump_Fl_2020,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='Florida')
abline(a=0,b=0)
##plot percentage
plot(date_2020[index_trump_Fl_2020],(counts_biden_Fl_2020-counts_trump_Fl_2020)/(counts_biden_Fl_2020+counts_trump_Fl_2020),
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts %',main='Florida')
abline(a=0,b=0)


#----------------------------------------------------------------------------------------------------------------------------
#plot the poll by Biden and Trump over time in NC
ylim_value=c(min(counts_biden_NC_2020,counts_trump_NC_2020),
             max(counts_biden_NC_2020,counts_trump_NC_2020))
plot(date_2020[index_biden_NC_2020],counts_biden_NC_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='North Carolina',ylim=ylim_value)
lines(date_2020[index_trump_NC_2020],counts_trump_NC_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#plot the difference 
plot(date_2020[index_trump_NC_2020],counts_biden_NC_2020-counts_trump_NC_2020,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts',main='North Carolina')
abline(a=0,b=0)
##plot percentage
plot(date_2020[index_trump_NC_2020],(counts_biden_NC_2020-counts_trump_NC_2020)/(counts_biden_NC_2020+counts_trump_NC_2020),
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts %',main='North Carolina')
abline(a=0,b=0)


#===========================================================================================================================
#2c
#Minnesota
counts_Minn_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_Minn_2020],
  percentage_diff = (counts_biden_Minn_2020-counts_trump_Minn_2020)/(counts_biden_Minn_2020+counts_trump_Minn_2020)
)

lm_model_Minn_2020=lm(percentage_diff~(data_date),data=counts_Minn_for_lm_2020)
summary(lm_model_Minn_2020)

conf_interval_Minn_fitted_2020= predict(lm_model_Minn_2020, newdata=counts_Minn_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_Minn_for_lm_2020$data_date,counts_Minn_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
polygon(c(rev(counts_Minn_for_lm_2020$data_date), counts_Minn_for_lm_2020$data_date), 
        c(rev(conf_interval_Minn_fitted_2020[,2]), conf_interval_Minn_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_Minn_for_lm_2020$data_date,lm_model_Minn_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Minnesota')
lines(counts_Minn_for_lm_2020$data_date,counts_Minn_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
#-----------------------------------------------------------------------------------------------------------------------------
#Florida
counts_Fl_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_Fl_2020],
  percentage_diff = (counts_biden_Fl_2020-counts_trump_Fl_2020)/(counts_biden_Fl_2020+counts_trump_Fl_2020)
)

lm_model_Fl_2020=lm(percentage_diff~(data_date),data=counts_Fl_for_lm_2020)
summary(lm_model_Fl_2020)

conf_interval_Fl_fitted_2020= predict(lm_model_Fl_2020, newdata=counts_Fl_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_Fl_for_lm_2020$data_date,counts_Fl_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
polygon(c(rev(counts_Fl_for_lm_2020$data_date), counts_Fl_for_lm_2020$data_date), 
        c(rev(conf_interval_Fl_fitted_2020[,2]), conf_interval_Fl_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_Fl_for_lm_2020$data_date,lm_model_Fl_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Florida')
lines(counts_Fl_for_lm_2020$data_date,counts_Fl_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
#------------------------------------------------------------------------------------------------------------------------------
#North Carolina
counts_NC_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_NC_2020],
  percentage_diff = (counts_biden_NC_2020-counts_trump_NC_2020)/(counts_biden_NC_2020+counts_trump_NC_2020)
)

lm_model_NC_2020=lm(percentage_diff~(data_date),data=counts_NC_for_lm_2020)
summary(lm_model_NC_2020)

conf_interval_NC_fitted_2020= predict(lm_model_NC_2020, newdata=counts_NC_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_NC_for_lm_2020$data_date,counts_NC_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_NC_for_lm_2020$data_date), counts_NC_for_lm_2020$data_date), 
        c(rev(conf_interval_NC_fitted_2020[,2]), conf_interval_NC_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_NC_for_lm_2020$data_date,lm_model_NC_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_NC_for_lm_2020$data_date,counts_NC_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
#North Carolina has the smallest margin.
#2d We dont get actual results until Nov 3.