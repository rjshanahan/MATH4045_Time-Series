#Richard Shanahan  
#https://github.com/rjshanahan  
#8 March 2016

###### MATH 4045: Time Series & Forecasting


# load required packages
library(psych)
library(forecast)
library(ggplot2)
library(reshape2)
library(dplyr)
library(devtools)


# source custom code for plots from GitHub Gist: https://gist.github.com/rjshanahan
# source_gist("e47c35277a36dca7189a")       #boxplot
# source_gist("7eed7f043c987f884748")       #facet wrap boxplot
# source_gist("40f46687d48030d40704")       #cluster plot


##### LOAD DATA #####

setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%d-%b-%Y %H:%M:%OS"))

theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top")


#import csvs assume the same format... preliminary calculations performed externally
#change input csv filename
mySeries <- read.csv('MYTIMESERIES.csv',
                     header=T,
                     sep=",",
                     quote='"',
                     colClasses=c(
                       'numeric',   # Sampling Period - Month/Year,
                       'numeric',   # value or year.squared
                       'numeric',   # value or ln(value)
                       'numeric',   # trend,
                       'numeric',   # res.trend,
                       'numeric',   # subtract_avg,
                       'numeric',   # moving_avg,
                       'numeric',   # exp_smooth,
                       'numeric',   # model_periodic,
                       'numeric'    # res.deseason
                     ),
                     strip.white=T,
                     stringsAsFactors=F,
                     fill=T)


#inspect
str(mySeries)
describe(mySeries)

#check for duplicate records based
nrow(unique(mySeries))

#check if there are any missing values
colSums(is.na(mySeries)) 

# assign id field for visualisations
mySeries$id <- 1:nrow(mySeries)



###### REGRESSION ###### 

regressor <- function(myY, myX, myData){
  require(stats)
  mylm <- lm(myY ~ myX,
             data=myData)
  return(mylm)
}

#change variables for 'myY' and 'myX' as required
mySeries_reg <- regressor(mySeries$id, mySeries[,3], mySeries)
str(mySeries_reg)

#add new variables for predicted valyes and residuals
mySeries$reg <- as.vector(mySeries_reg$fitted.values)
mySeries$res <- as.vector(mySeries_reg$residuals)




###### VISUALISATIONS ###### 

#auto bin ranges
bw <- round(diff(range(mySeries[,3])) / (2 * IQR(mySeries[,3]) / length(mySeries[,3])^(1/3)),0) * 15
bw.t <- round(diff(range(mySeries$res.trend)) / (2 * IQR(mySeries$res.trend) / length(mySeries$res.trend)^(1/3)),0) * 15
bw.s <- round(diff(range(mySeries$res.deseason)) / (2 * IQR(mySeries$res.deseason) / length(mySeries$res.deseason)^(1/3)),0) * 15

#histogram
ggplot(data = mySeries, 
       aes(x=mySeries[,3])) + 
  geom_histogram(binwidth=bw) +
  ggtitle(paste0(colnames(mySeries)[3], ' Histogram ', '(bin=', bw, ')')) +
  ggsave(paste0(colnames(mySeries)[3],' Histogram for ', 'value', '.png'))


ggplot(data = mySeries, 
       aes(x=mySeries$res.trend)) + 
  geom_histogram(binwidth=bw.t) +
  ggtitle(paste0(colnames(mySeries)[3], ' Histogram Detrended Residuals (bin=', bw.t,')')) +
  ggsave(paste0(colnames(mySeries)[3],' Histogram for ', 'mySeries$res.trend', '.png'))


ggplot(data = mySeries, 
       aes(x=mySeries$res.deseason)) + 
  geom_histogram(binwidth=bw.s) +
  ggtitle(paste0(colnames(mySeries)[3], ' Histogram Deseasoned Residuals (bin=', bw.s,')')) +
  ggsave(paste0(colnames(mySeries)[3],' Histogram for ', 'mySeries$res.deseason', '.png'))



#timeseries - TREND
ggplot(data = mySeries, 
       aes(x=id,
           colour='Trend')) + 
  geom_line(aes(y=mySeries[,3], colour='Series')) +
  geom_line(aes(y=(mySeries$trend), colour='trend.linear')) +
  #geom_line(aes(y=(mySeries$trend.sq), colour='trend.quadratic')) +
  #geom_smooth(method = "lm", se = F, formula=y ~ poly(x, 2, raw=TRUE)) +
  #geom_smooth(method = "lm", se = F) +
  ggtitle(paste0('Time Series Analysis for ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1])) +
  xlab('time') +
  ylab(colnames(mySeries)[3]) +
  scale_color_manual(values=c('Series'='blue',
                              #'trend.quadratic'='seagreen',
                              'trend.linear'='indianred'
  )) +
ggsave(paste0('Time Series Analysis for ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1], '.png') )

  

#timeseries - DETRENDED
ggplot(data = mySeries, 
       aes(x=id,
           colour='Seasonality')) + 
  geom_line(aes(y=mySeries$res.trend, colour='residuals')) +
  geom_line(aes(y=mySeries$subtract_avg, colour='subtract_avg')) +
  geom_line(aes(y=mySeries$exp_smooth, colour='exp_smooth')) +
  geom_line(aes(y=mySeries$moving_avg, colour='moving_avg')) +
  geom_line(aes(y=mySeries$model_periodic, colour='model_periodic')) +
  ggtitle(paste0('Time Series Analysis for DETRENDED ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1])) +
  xlab('time') +
  ylab(paste0('DETRENDED ',colnames(mySeries)[3])) +
  scale_color_manual(values=c('residuals'='blue',
                              'subtract_avg'='lightgray',
                              'exp_smooth'='lightgreen',
                              'moving_avg'='gold',
                              'model_periodic'='red'
                              )) +
  ggsave(paste0('Time Series Analysis for DETRENDED ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1], '.png'))



#timeseries - DESEASON
ggplot(data = mySeries, 
       aes(x=id,
           colour='Stationarity')) + 
  geom_line(aes(y=mySeries$res.deseason, colour='residuals_deseasoned')) +
  geom_line(aes(y=mySeries$res.trend, colour='residuals_detrended')) +
  ggtitle(paste0('Time Series Analysis: Residuals Comparison for ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1])) +
  xlab('time') +
  ylab(paste0('Residuals ',colnames(mySeries)[3])) +
  scale_color_manual(values=c('residuals_deseasoned'='blue',
                              'residuals_detrended'='indianred2'
  )) +
  ggsave(paste0('Time Series Analysis: Residuals Comparison for ',colnames(mySeries)[3],' with Time = ',colnames(mySeries)[1], '.png'))


#timeseries - residual QQ plot
ggplot(data = mySeries, 
       aes(sample=res.trend)) +
  stat_qq(colour='indianred2') +
  ggtitle(paste0('Q-Q Plot for ',colnames(mySeries)[3],' detrended residuals')) +
  ggsave(paste0('Q-Q Plot for ',colnames(mySeries)[3],' detrended residuals.png')) 


ggplot(data = mySeries, 
       aes(sample=res.deseason)) +
  stat_qq(colour='seagreen2') +
  ggtitle(paste0('Q-Q Plot for ',colnames(mySeries)[3],' deseasoned residuals'))  +
  ggsave(paste0('Q-Q Plot for ',colnames(mySeries)[3],' deseasoned residuals.png')) 




##### STATIONARITY ANALYSIS #####

# Once we have a stationary time series we can use these functions to determine whether to fit
# 1 A moving average process or;
# 2 An autoregressive process or;
# 3 An autoregressive integrated moving average process
# We use (1) when the SACF has spikes at lags 1,2,···,q and the SPACF dies down gradually.
# We use (2) when the SACF dies down gradually and the SPACF has spikes at lags 1,2,···,p.
# If neither occurs then we may be faced with a non-stationary times series or a requirement to use a combination of both, which is option (3).



#SAMPLE Autocovariance Function SAF
myacf <- acf(mySeries$res.deseason, 
             type = c("covariance"),
             #type = c("correlation", "covariance", "partial"),
             plot=F)

acfdf <- with(myacf, data.frame(lag, acf))

ggplot(data = acfdf, 
       aes(x = lag, 
           y = acf)) +
  geom_hline(aes(yintercept = 0), colour='red') +
  geom_hline(yintercept=c(0.05, -0.05), linetype="dashed", colour='gray') +
  geom_segment(mapping = aes(xend = lag, yend = 0), colour='blue') + 
  ggtitle(paste0(colnames(mySeries)[3], ' - Stationarity Analysis: ACF - ', myacf$type)) +
  ylab(paste0('ACF - ', myacf$type)) +
  #xlim(c(1,20)) +
  ggsave(paste0(colnames(mySeries)[3],' Stationarity Analysis: ACF - ', myacf$type, '.png'))



#SAMPLE Autocorrelation Function SACF
myacf <- acf(mySeries$res.deseason, 
             type = c("correlation"),
             #type = c("correlation", "covariance", "partial"),
             plot=F)

acfdf <- with(myacf, data.frame(lag, acf))

ggplot(data = acfdf, 
       aes(x = lag, 
           y = acf)) +
  geom_hline(aes(yintercept = 0), colour='red') +
  geom_hline(yintercept=c(0.05, -0.05), linetype="dashed", colour='gray') +
  geom_segment(mapping = aes(xend = lag, yend = 0), colour='blue') + 
  ggtitle(paste0(colnames(mySeries)[3], ' - Stationarity Analysis: ACF - ', myacf$type)) +
  ylab(paste0('ACF - ', myacf$type)) +
  xlim(c(1,20)) +
  ggsave(paste0(colnames(mySeries)[3],' Stationarity Analysis: ACF - ', myacf$type, '.png'))




#PARTIAL Autocorrelation Function PACF
myacf <- acf(mySeries$res.deseason, 
             type = c("partial"),
             #type = c("correlation", "covariance", "partial"),
             plot=F)

acfdf <- with(myacf, data.frame(lag, acf))

ggplot(data = acfdf, 
       aes(x = lag, 
           y = acf)) +
  geom_hline(aes(yintercept = 0), colour='red') +
  geom_hline(yintercept=c(0.05, -0.05), linetype="dashed", colour='gray') +
  geom_segment(mapping = aes(xend = lag, yend = 0), colour='blue') + 
  ggtitle(paste0(colnames(mySeries)[3], ' - Stationarity Analysis: ACF - ', myacf$type)) +
  ylab(paste0('ACF - ', myacf$type)) +
  #xlim(c(1,20)) +
  ggsave(paste0(colnames(mySeries)[3],' Stationarity Analysis: ACF - ', myacf$type,'.png'))




##### FIT ARIMA MODELS ####

#model functions

#this uses MLE
AR.1.ar <- ar(mySeries$res.deseason,
              method='yw')

#this uses OLS
AR.1.arOLS <- ar.ols(mySeries$res.deseason, 
                     order=1, 
                     demean=F, 
                     intercept=T)

#arima functions for AR(1)
AR.1.arima <- arima(mySeries$res.deseason, 
                    order = c(1,0,0))

#inc. constant
AR.1.arima.constant <- arima(mySeries$res.deseason, 
                             order = c(1,0,0), 
                             xreg=1:length(mySeries$res.deseason))

summary(AR.1.arima.constant)

acf(residuals(AR.1.arima.constant))

#auto selection from the 'forecast' package
auto.arima(mySeries$res.deseason)


#model diagnostics

#fit diagnostics - for ARIMA

png(paste(colnames(mySeries)[3], ' fit diagnostics.png'))
tsdiag(AR.1.arima.constant)
dev.off()

Box.test(AR.1.arima.constant$residuals,
         type="Ljung",lag=20)
#change lag for Jeans

#visualisations of residuals
AR.residuals <- data.frame(residuals(AR.1.arima.constant))

#histogram of AR residuals
ggplot(data = AR.residuals, 
       aes(x=c(AR.residuals$residuals.AR.1.arima.constant.))) + 
  geom_histogram(binwidth=bw.s*10) +
  ggtitle(paste0(colnames(mySeries)[3], ' Histogram AR(1) Residuals (bin=', bw.s*10,')')) +
  ggsave(paste0(colnames(mySeries)[3],' Histogram for ', 'res.AR', '.png'))

#timeseries - residual QQ plot
ggplot(data = AR.residuals, 
       aes(sample=residuals.AR.1.arima.constant.)) +
  stat_qq(colour='goldenrod') +
  ggtitle(paste0('Q-Q Plot for ',colnames(mySeries)[3],' AR residuals')) +
  ggsave(paste0('Q-Q Plot for ',colnames(mySeries)[3],' AR residuals.png')) 




