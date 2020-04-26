# set dir
setwd('/Users/Projects/datagen')

#========libraries and data load============
# load libraries
library('tidyverse')


# set seed for reproducibility
set.seed(2606)

# create empty df
df <- data.frame(regdate=as.Date(character()),
                      contactable=character(), 
                      customer=character(), 
                      stringsAsFactors=FALSE) 

# generate a sequence of dates
days <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="day")

# iterate over a sequence of dates and generate data for each
for (i in as.list(days)) {
  
  # generate daily new users
  newDailyUsers <- sample(10:1600, 1)
  
  # generate date of registration data
  regdate <- rep(i,newDailyUsers)
  
  # generate 'contactable' feature
  contactable <- sample(c(1,0), size = newDailyUsers, replace = TRUE, prob = c(.80, .20))
  
  # generate 'customer' feature
  customer <- sample(c(1,0), size = newDailyUsers, replace = TRUE, prob = c(.50, .50))

  res <- data.frame(regdate, contactable,customer)
  
  df <- rbind(df, res)
} 

# fill in not contactable, prospects, repeating customers
df$ncontactable <- ifelse(df$contactable==1,0,1)
df$ncustomer <- ifelse(df$customer==1,0,1)
df$repeating <- ifelse(df$customer==1, sample(c(1,0), length(which(df$customer==1)), 
                                              replace = TRUE, prob = c(.20, .80)),0)


df_totals <- df %>% group_by(regdate) %>% summarise(cont = sum(contactable), ncont = sum(ncontactable), 
                                       cust = sum(customer), prosp = sum(ncustomer), repc = sum(repeating))

df_totals <- df_totals %>% mutate(ftc = cust-repc, total_temp=cont+ncont)
df_totals <- df_totals %>% mutate(total_cs = cumsum(total_temp), total=total_cs+1000000)

# df_totals <- df_totals %>% mutate(ftc = round(cust*0.8), repc = cust-ftc)

head(df_totals)

# calculate cumulative sum
#df_totals <- df_totals %>% mutate(contactable_cs = cumsum(cont), noncontable_cs = cumsum(ncont),
#                                  customers_cs = cumsum(cust), prospects_cs = cumsum(prosp),
#                                  ftimecustomers_cs = cumsum(ftc), repcustomers_cs = cumsum(repc))


# write data for db growth and segments to file
write.csv(df_totals, file = "loreal_db_segments.csv")


#==========Generate Revenue by Channel================

# create empty df
revenue_by_channel <- data.frame(regdate=as.Date(character()),
                 revenue=numeric(), 
                 channel=character(), 
                 stringsAsFactors=FALSE) 


# create a list of channels
channels <- c('email_batch', 'email_triggers', 'email_service', 'sms', 'push')


# iterate over a sequence of dates and generate data for each
for (i in as.list(days)) {
      
  # generate date of registration data
  regdate <- rep(i,5)
      
  # generate email revenue
  email_batch_rev <- sample(0:30000, size=1)
  email_trigger_rev <- sample(0:30000, size=1)
  email_service_rev <- sample(0:10000, size=1)
  smsrev <- sample(0:3000, size=1)
  pushrev <- sample(0:30000, size=1)
  revenue <- c(email_batch_rev, email_trigger_rev, email_service_rev, smsrev, pushrev)
        
  res <- data.frame(regdate, revenue, channels)
        
  revenue_by_channel <- rbind(revenue_by_channel, res)
}

# write revenue by channel data to file
write.csv(revenue_by_channel, file = "loreal_revenue_by_channel.csv")


#==========Generate OR/CR for Channels================

# create empty df
orcr_by_channel <- data.frame(regdate=as.Date(character()),
                                 openrate=numeric(),
                                 clickrate=numeric(), 
                                 channel=character(), 
                                 stringsAsFactors=FALSE) 

# generate a sequence of dates
months <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="month")


# create a list of channels
channels <- c('email_batch', 'email_triggers', 'email_service', 'sms', 'push')


# iterate over a sequence of dates and generate data for each
for (i in as.list(months)) {
  
  # generate date of registration data
  regdate <- rep(i,5)
  
  # generate or/cr
  email_batch_or <- sample(5:18, size=1)
  email_trigger_or <- sample(10:25, size=1)
  email_service_or <- sample(15:30, size=1)
  sms_or <- NA
  push_or <- sample(10:30, size=1)
  openrate <- c(email_batch_or, email_trigger_or, email_service_or, sms_or, push_or)
  
  email_batch_cr <- sample(3:15, size=1)
  email_trigger_cr <- sample(5:20, size=1)
  email_service_cr <- sample(7:25, size=1)
  sms_cr <- sample(10:30, size=1)
  push_cr <- sample(20:35, size=1)
  clickrate <- c(email_batch_cr, email_trigger_cr, email_service_cr, sms_cr, push_cr)
  
  res <- data.frame(regdate, openrate, clickrate, channels)
  
  orcr_by_channel <- rbind(orcr_by_channel, res)
}

# write revenue by channel data to file
write.csv(orcr_by_channel, file = "loreal_orcr_by_channel.csv")


#==========TOP categories by revenue================

# create empty df
topcat_by_revenue <- data.frame(month=as.Date(character()),
                                 revenue=numeric(), 
                                 category=character(), 
                                 stringsAsFactors=FALSE) 

# generate a sequence of dates
months <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="month")


# create a list of channels
category <- c('Средства для бритья, муж', 'Средства после бритья, муж', 'Уход за кожей, муж', 
              'Уход за бородой и усами', 'Окрашивание волос', 'Шампуни', 'Бальзамы', 'Маски для волос', 
              'Макияж. Лицо', 'Макияж.Глаза', 'Макияж.Губы')


# iterate over a sequence of dates and generate data for each
for (i in as.list(months)) {
  
  # generate date of registration data
  regdate <- rep(i,11)
  
  # generate revenue for categories
  revenue <- sample(0:100000, size=11)
  
  res <- data.frame(regdate, revenue, category)
  
  topcat_by_revenue <- rbind(topcat_by_revenue, res)
}

# write revenue by channel data to file
write.csv(topcat_by_revenue, file = "loreal_topcat_by_revenue.csv")


#==========TOP products by revenue================

# create empty df
topprod_by_revenue <- data.frame(month=as.Date(character()),
                                revenue=numeric(), 
                                category=character(), 
                                stringsAsFactors=FALSE) 

# generate a sequence of dates
months <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="month")


# create a list of channels
product <- c('Пена для бритья Гидра Сенситив', 'Лосьон после бритья Гидра Энергетик', 
              'Очищающий гель для умывания с черным углем', 'Крем-гель для короткой бороды', 
              'Стойкая краска для волос, оттенок 2', 'Шампунь для волос Лаванда', 'Бальзам для волос Кориандр', 
              'Маска для волос Кориандр', 'Рассыпчатая матирующая пудра', 'Тушь для ресниц Взгляд Бэмби', 
              'Блеск для губ, оттенок 02')


# iterate over a sequence of dates and generate data for each
for (i in as.list(months)) {
  
  # generate date of registration data
  regdate <- rep(i,11)
  
  # generate revenue for categories
  revenue <- sample(0:30000, size=11)
  
  res <- data.frame(regdate, revenue, product)
  
  topprod_by_revenue <- rbind(topprod_by_revenue, res)
}

# write revenue by channel data to file
write.csv(topprod_by_revenue, file = "loreal_topprod_by_revenue.csv")


#==========Generate Costs data================

# create empty df
costs_by_channel <- data.frame(regdate=as.Date(character()),
                              creation=numeric(),
                              design=numeric(), 
                              sending=numeric(), 
                              channel=character(), 
                              stringsAsFactors=FALSE) 

# generate a sequence of dates
months <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="month")

# create a list of channels
channels <- c('email', 'sms', 'push')


# iterate over a sequence of dates and generate data for each
for (i in as.list(months)) {
  
  # generate date of registration data
  regdate <- rep(i,3)
  
  # generate costs
  email_design <- sample(30000:100000, size=1)
  sms_design <- sample(5000:10000, size=1)
  push_design <- sample(20000:50000, size=1)
  design <- c(email_design, sms_design, push_design)
  
  email_creation <- sample(20000:80000, size=1)
  sms_creation <- sample(5000:10000, size=1)
  push_creation <- sample(10000:30000, size=1)
  creation <- c(email_creation, sms_creation, push_creation)
  
  email_sending <- sample(10000:40000, size=1)
  sms_sending <- sample(3000:6000, size=1)
  push_sending <- sample(10000:30000, size=1)
  sending <- c(email_sending, sms_sending, push_sending)
  
  res <- data.frame(regdate, design, creation, sending, channels)
  
  costs_by_channel <- rbind(costs_by_channel, res)
}

# write costs by channel data to file
write.csv(costs_by_channel, file = "loreal_costs_by_channel.csv")

# Create data for cost per message viz
cost_per_message <- costs_by_channel %>% group_by(channels, regdate) %>% 
  mutate(sum_costs = design+creation+sending) %>% select(regdate, sum_costs, channels)

volume <- c()

# generate a sequence of dates
months <-seq(as.Date("2020/01/01"), as.Date("2020/12/31"), by="month")

# iterate over a sequence of dates and generate data for each
for (i in as.list(months)) {
  
  # generate date of registration data
  regdate <- rep(i,3)
  
  # generate data for sending volume
  email_volume <- sample(200000:300000, size=1)
  sms_volume <- sample(3000:5000, size=1)
  push_volume <- sample(50000:100000, size=1)
  esp_volume <- c(email_volume, sms_volume, push_volume)

  volume <- c(volume, esp_volume)
} 

cost_per_message$volume <- volume
cost_per_message <- cost_per_message %>% mutate(cpm = sum_costs/volume)

cost_per_message

# write sum_costs and volume data to file
write.csv(cost_per_message, file = "loreal_cost_per_message.csv")


costs_by_channel %>% group_by(channels) %>% summarise(des=sum(design), cre=sum(creation), sen=sum(sending))
