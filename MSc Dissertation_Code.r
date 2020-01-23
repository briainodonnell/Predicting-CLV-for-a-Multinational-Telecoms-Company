
#### Reading in necessary libraries
options(warn=-1)
library(BTYDplus)
library(BTYD)
library(plyr)
library(tidyverse)
library(lessR)
library(fst)
library(dplyr)
library(data.table)
library(eeptools)
library(mltools) 
library(gtools)
require("reticulate")
source_python("pickle_reader.py")
source("pnbd_functions.r")

# Reading in the pickle files
dataset_top_20 = read_pickle_file("C:/Users/briai/Documents/_Dissertation/Data/top_20.pkl")
dataset_top_35 = read_pickle_file("C:/Users/briai/Documents/_Dissertation/Data/top_35.pkl")
dataset_top_45 = read_pickle_file("C:/Users/briai/Documents/_Dissertation/Data/top_45.pkl")

# Merging files to create full dataset
dataset_full = rbind(dataset_top_20, dataset_top_35)
dataset_full = rbind(dataset_full, dataset_top_45)

# Relabelling column names
colnames(dataset_full) = c("cust","date","action")
dataset_full$date = as.Date(dataset_full$date)

#### Removing irrelevent "action" types from the dataset
nrow_original = nrow(dataset_full)
dataset_full = dataset_full[!grepl("BTL", dataset_full$action),]
dataset_full = dataset_full[!grepl("NEWCOM", dataset_full$action),]
dataset_full = dataset_full[!grepl("Credit U Transfer Fee", dataset_full$action),]
dataset_full = dataset_full[!grepl("INTL_CREDITU_CREDITOR", dataset_full$action),]
dataset_full = dataset_full[!grepl("Service 154- CUG Subscription Fee", dataset_full$action),]
dataset_full = dataset_full[!grepl("NULL", dataset_full$action),]
dataset_full = dataset_full[!grepl("SUBSCRIPTION - airtime", dataset_full$action),]
dataset_full = dataset_full[!grepl("MaintenanceFee", dataset_full$action),]
dataset_full = dataset_full[!grepl("SUBSCRIPTION - 7DayQuickPicksSimAcquisition", dataset_full$action),]
dataset_full = dataset_full[!grepl("Inactive Account Admin", dataset_full$action),]
dataset_full = dataset_full[!grepl("Jamaica Late Fee", dataset_full$action),]
nrow_filtered = nrow(dataset_full)
cat("No. of lost rows =", nrow_original - nrow_filtered)

# Adding a new column for transaction ID 
dataset_full$transaction_id = 1:nrow(dataset_full)

# Mapping the transaction sales figures for each transaction 
dataset_full_sales_mapping = read.csv("C:/Users/briai/Documents/_Dissertation/Data/dataset_full_sales_mapping.csv")
dataset_full = merge(x = dataset_full, y = dataset_full_sales_mapping, by = "transaction_id", all.x = TRUE)
dataset_full = subset(dataset_full, sales!="NA")

# Filtering out Action field and Transaction ID
dataset = dataset_full[,c("cust","date","sales")]

#### Merging all transactions that occured on the same date. The sales amounts are summed for all transactions occuring on same date
orig_nrow = nrow(dataset)
dataset = dc.MergeTransactionsOnSameDate(dataset)
after_nrow = nrow(dataset)
lost_rows = orig_nrow - after_nrow
cat("No. of lost rows =", lost_rows)

# Ordering the dataset based on the transaction date
dataset = dataset[order(as.Date(dataset$date, format="%d/%m/%Y")),]

# Mapping the date of the individual customers first transaction into the dataframe
first_date = aggregate(date ~ cust, dataset, function(x) x[1])
names(first_date) = c("cust", "first.date")
dataset = merge(dataset, first_date, by = "cust")

# Filtering for customers that had their first transaction before 1st May
dataset = dataset %>% filter(first.date < "2017-05-01")

# Counting the number of transactions per customer and merging into primary dataframe 
mapping_count = dataset %>% count(cust)
merged_df = merge(x = dataset, y = mapping_count, by = "cust", all.x = TRUE)

#### Filtering the dataframe for customers that had more than 11 transactions. We need more than 11 transactions to estimate "regularity" later in the thesis 
orig_nrow = nrow(dataset)
orig_ncust = length(unique(unlist(dataset[c("cust")])))
dataset = merged_df %>% filter(n > 11)
after_nrow = nrow(dataset)
after_ncust = length(unique(unlist(dataset[c("cust")])))
cat("No. of lost rows =", orig_nrow - after_nrow, "\n")
cat("No. of lost customers =", orig_ncust - after_ncust)

# Adding an ID column to the dataset and filtering for specific columns
dataset = transform(dataset, id=as.numeric(factor(cust)))
dataset = subset(dataset, select = c("date","id","sales","n"))
names(dataset) = c("date","cust","sales","n")

# Append dollar amount of customers first purchase
first_sales = aggregate(sales ~ cust, dataset[order(dataset$date),], function(x) x[1])
names(first_sales) = c("cust", "first.sales")
dataset = merge(dataset, first_sales, by = "cust")


#### Creating graphs of Incremental vs Cumulative no. transactions per week
op = par(mfrow = c(1, 2), mar = c(2.5, 2.5, 2.5, 2.5))
# Incremental number of transactions per week
weekly_inc_total = elog2inc(dataset, by = 7, first = TRUE)
weekly_inc_repeat = elog2inc(dataset, by = 7, first = FALSE)
plot(weekly_inc_total, typ = "l", frame = FALSE, main = "Incremental")
lines(weekly_inc_repeat, col = "red")
# Cumulative number of transactions per week
weekly_cum_total = elog2cum(dataset, by = 7, first = TRUE)
weekly_cum_repeat = elog2cum(dataset, by = 7, first = FALSE)
plot(weekly_cum_total, typ = "l", frame = FALSE, main = "Cumulative")
lines(weekly_cum_repeat, col = "red")
par(op)

#### Plotting the transaction timing patterns of customers 
# Plotting timing patterns for 30 customers with past / future animation   
set.seed(123)
plotTimingPatterns(dataset, n = 30, T.cal = "2018-07-31", headers = c("Past", "Future"), title = "Transaction Timing Patterns")
# Plotting timing patterns for 10 customers 
set.seed(300)
plotTimingPatterns(dataset, n = 10, T.cal = max(dataset$date), title = "Transaction Timing Patterns")

#### Creating a unique customer dataframe by removing duplicate customer entries
dataset_unique_cust = dataset[!duplicated(dataset$cust), ]

#### Plotting histogram of no. transactions per customer
# Setting plot dimensions 
set_plot_dimensions(8, 7)
hist(dataset_unique_cust$n, breaks=10, col="slategray2", border="gray45",
     main = "Distribution of No. Transactions per Customer", xlab = "No. Transactions per Customer", ylab = "Frequency",
     axes = TRUE, plot = TRUE, labels = TRUE)

# Summing total sales per customer
cust_sales = aggregate(dataset$sales, by=list(Category=dataset$cust), FUN=sum)

#### Plotting histogram of total sales per customer
# Setting plot dimensions 
set_plot_dimensions(8, 7)
hist(cust_sales$x, breaks=10, col="slategray2", border="gray45",
     main = "Distribution of Total Sales per Customer", xlab = "Total Sales per Customer", ylab = "Frequency",
     axes = TRUE, plot = TRUE, labels = TRUE)

# Determining the mean sales amount per customer
mean_cust_sales = aggregate(dataset$sales, list(dataset$cust), mean)
colnames(mean_cust_sales) = c("cust","mean_sales")
dataset_unique_cust = merge(dataset_unique_cust, mean_cust_sales, by = "cust")

# Calculating the number of the days each customer has been "alive"
cust_lifetime = dataset %>% group_by(cust) %>%
                                summarize(first = min(date), 
                                          last = max(date), 
                                          lifetime = as.numeric(difftime(last, first), units="days"))
dataset_unique_cust = merge(dataset_unique_cust, cust_lifetime, by = "cust")

# Plotting histogram of customer lifetimes
set_plot_dimensions(8, 7)
hist(dataset_unique_cust$lifetime, breaks=100, col="slategray2", border="gray45",
     main = "Distribution of Observed Customer Lifetime", xlab = "Observed Customer Lifetime in Days", ylab = "Frequency",
     axes = TRUE, plot = TRUE)

# Ordering dataframe by customer and date
ordered_dataset=dataset[with(dataset, order(cust, date)), ]

# Calculating intertransaction times
ordered_dataset=ordered_dataset %>% 
                    group_by(cust) %>%
                    mutate(intertransaction_time = date - lag(date))

# Converting intertransaction times in days to integer 
ordered_dataset$intertransaction_time = as.numeric(ordered_dataset$intertransaction_time)

# Plotting histogram of intertransaction times
set_plot_dimensions(8, 6)
hist(ordered_dataset$intertransaction_time, breaks=750, col="slategray2", border="gray45",
     main = "Distribution of Intertransaction Times", xlab = "Intertransaction Time in Days", ylab = "Frequency",
     axes = TRUE, plot = TRUE, xlim=c(0,30))

# Calculating the number of ITTs over 365 days
nrow(ordered_dataset %>% filter(intertransaction_time >= 365)) 

# Calculating the median ITT per customer
median_cust_itt = aggregate(ordered_dataset$intertransaction_time, list(ordered_dataset$cust), median, na.rm=TRUE, na.action=NULL)
colnames(median_cust_itt) = c("cust","median_itt")
dataset_unique_cust = merge(dataset_unique_cust, median_cust_itt, by = "cust")

# Calculating the mean ITT per customer
mean_cust_itt = aggregate(ordered_dataset$intertransaction_time, list(ordered_dataset$cust), mean, na.rm=TRUE, na.action=NULL)
colnames(mean_cust_itt) = c("cust","mean_itt")
dataset_unique_cust = merge(dataset_unique_cust, mean_cust_itt, by = "cust")

# Collecting Overall Summary Stats into a table 
rbind("No. Customers" = c("Summary" = length(unique(unlist(dataset[c("cust")])))),
      "No. Transactions" = round(nrow(dataset)))

# Viewing the overall Data Range
cat("Date Range:", "\n")
range(dataset$date)

# Summary Stats per Customer
rbind("Median" = c("No. Transactions per Cust" = round(median(dataset_unique_cust$n), 2), "Transaction Amt per Cust" = round(median(dataset_unique_cust$mean_sales), 2), "Days between Transactions per Cust" = round(median(dataset_unique_cust$mean_itt, na.rm=TRUE), 2)),
      "SD" = c(round(sd(dataset_unique_cust$n), 2), round(sd(dataset_unique_cust$mean_sales), 2), round(sd(dataset_unique_cust$mean_itt, na.rm=TRUE), 2)),
      "Min" = c(round(min(dataset_unique_cust$n), 2), round(min(dataset_unique_cust$mean_sales), 2), round(min(dataset_unique_cust$mean_itt, na.rm=TRUE), 2) ),
      "Max" = c(round(max(dataset_unique_cust$n), 2), round(max(dataset_unique_cust$mean_sales), 2), round(max(dataset_unique_cust$mean_itt, na.rm=TRUE), 2) ))

rbind("No. Transactions > 250" = c("No. Customers" = nrow(dataset_unique_cust %>% filter(n >= 250))),
      "No. Transactions 100-250" = nrow(dataset_unique_cust %>% filter((n >= 100) & (n < 250))),
      "No. Transactions 50-100" = nrow(dataset_unique_cust %>% filter((n >= 50) & (n < 100))), 
      "No. Transactions < 50" = nrow(dataset_unique_cust %>% filter((n < 50))))


##########################################
# Creating the Customer-by-Sufficient-Statistic Matrix
dataset_cbs = elog2cbs(dataset, units="day", T.cal=max(dataset$date), T.tot=max(dataset$date))
head(dataset_cbs)

# Creating the Customer-by-Sufficient-Statistic Matrix with a Holdout period 
dataset_cbs_holdout = elog2cbs(dataset, units="day", T.cal="2018-07-31", T.tot="2019-02-01")
head(dataset_cbs_holdout)

##########################################
# Estimating the parameters of the NBD model 
params.nbd = nbd.EstimateParameters(dataset_cbs)

# Calculting the mean and standard deviation of the ITTs from the parameters 
cat("Avg ITT =", params.nbd[2] / params.nbd[1], "days", "\n")
cat("Std Dev ITT =", params.nbd[2] / sqrt(params.nbd[1]), "days")

# Reestimating the parameters using the CBS with holdout
params.nbd.holdout = nbd.EstimateParameters(dataset_cbs_holdout)

# Predicting the number of transactions in the holdout period using estimated parameters 
dataset_cbs_holdout$xstar.nbd = nbd.ConditionalExpectedTransactions(params = params.nbd.holdout, 
                                                                    T.star = 182, x = dataset_cbs_holdout$x, 
                                                                    T.cal = dataset_cbs_holdout$T.cal)
cal.cbs = dataset_cbs_holdout[,c("x", "t.x", "T.cal")]
x.star = as.vector(dataset_cbs_holdout[,"x.star"])
nbd.x.star.pred = as.vector(dataset_cbs_holdout[,"xstar.nbd"])

# Creating table with expecyed frequency versus actual frequency
nbd.exp_vs_actual = nbd.FreqVsConditionalExpectedFrequency.new(params.nbd, T.star=182, cal.cbs, 
                                                               x.star, nbd.x.star.pred, censor=100)
nbd.exp_vs_actual_tens = sapply(seq(1,99,by=10),function(i) rowSums(nbd.exp_vs_actual[,i:(i+9)]))
nbd.exp_vs_actual_tens

##########################################
# Estimating the parameters of the PNBD model 
pnbd.draws = pnbd.mcmc.DrawParameters(dataset_cbs, mcmc=5000, burnin=500, thin=50, 
                                      chains=1, use_data_augmentation = TRUE)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
cohort.draws = pnbd.draws$level_2

# Compute median across draws
cohort.draws.median = apply(as.matrix(cohort.draws), 2, median)

# Collecting data into a dataframe
rbind("Median" = c(cohort.draws.median[1], cohort.draws.median[2], cohort.draws.median[3], cohort.draws.median[4]),
      "5% Credible Interval" = c(cred.int(as.matrix(cohort.draws)[,1])[1], cred.int(as.matrix(cohort.draws)[,2])[1], cred.int(as.matrix(cohort.draws)[,3])[1], cred.int(as.matrix(cohort.draws)[,4])[1]),
      "95% Credible Interval" = c(cred.int(as.matrix(cohort.draws)[,1])[2], cred.int(as.matrix(cohort.draws)[,2])[2], cred.int(as.matrix(cohort.draws)[,3])[2], cred.int(as.matrix(cohort.draws)[,4])[2]))

# Plotting the density and trace of each parameter  
plot(pnbd.draws$level_2)

# Creating dataframe with the gamma probability for each value of x 
dgamma_df1 = data.frame(x = seq(0, 0.6, by=0.01), prob = dgamma(x = seq(0, 0.6, by=0.01), shape = cohort.draws.median[1], rate = cohort.draws.median[2]))
dgamma_df1$lambda = 1 / dgamma_df1$x
dgamma_df1$prob = round(dgamma_df1$prob, digits=1)
dgamma_df1$lambda = round(dgamma_df1$lambda, digits=0)

# Calculating the estimates of the rate parameter and the mean / variance of the ITTs 
cat("Mean Estimate of Lambda =", cohort.draws.median[1] / cohort.draws.median[2], "\n")
cat("Avg ITT =", cohort.draws.median[2] / cohort.draws.median[1], "days", "\n")
cat("Std Dev ITT =", cohort.draws.median[2] / sqrt(cohort.draws.median[1]), "days")

# Plotting the distribution of the transaction rate Lambda
par(mar=c(7, 4, 8, 3)) # 15 line height for bottom margin
set_plot_dimensions(8, 7)
barplot(dgamma_df1$prob, names.arg=dgamma_df1$x, ylim=c(0,7), col="slategray2", xlab=expression(paste("Value of ", lambda)), ylab="Frequency", border="gray45", cex.lab=1.2)
title(expression(paste("Distribution of Transaction Rate ", lambda)), line = 5.5)
title("ITT in Days", line = 3.5, cex.main=0.8, font=1)
axis(3, at=dgamma_df1$x[seq(1, length(dgamma_df1$x), 2)]*122.5, labels=dgamma_df1$lambda[seq(1, length(dgamma_df1$lambda), 2)], line = 1, col = "black", font=1) 

# Creating dataframe with the gamma probability for each value of x 
dgamma_df2 = data.frame(x = seq(0, 0.002, by=0.0001), prob = dgamma(x = seq(0, 0.002, by=0.0001), shape = cohort.draws.median[3], rate = cohort.draws.median[4]))
dgamma_df2$mu = 1 / dgamma_df2$x
dgamma_df2$prob = round(dgamma_df2$prob, digits=1)
dgamma_df2$mu = round(dgamma_df2$mu, digits=0)

# Calculating the estimates of the rate parameter and the mean / variance of the death rate
cat("Mean Estimate of Mu =", 1/ (cohort.draws.median[4] / cohort.draws.median[3]), "\n")
cat("Avg Lifetime =", cohort.draws.median[4] / cohort.draws.median[3], "days", "\n")
cat("Std Dev Lifetime =", cohort.draws.median[4] / sqrt(cohort.draws.median[3]), "days")

# Plotting the distribution of the death rate Mu
par(mar=c(7, 4, 8, 3))
set_plot_dimensions(8, 7)
barplot(dgamma_df2$prob, names.arg=dgamma_df2$x, ylim=c(0,2000), col="slategray2", xlab=expression(paste("Value of ", mu)), ylab="Frequency", border="gray45", cex.lab=1.2)
title(expression(paste("Distribution of Dropout Rate ", mu)), line = 5.5, font.main=1)
title("Lifetime in Days", line = 3.8, cex.main=0.8, font=1)
axis(3, at=dgamma_df2$x[seq(1, length(dgamma_df2$x), 2)]*12600, labels=dgamma_df2$mu[seq(1, length(dgamma_df2$mu), 2)], line = 1, col = "black", font=1) 

# Estimating the parameters of the PNBD model using the holdout CBS Matrix
pnbd.draws.holdout = pnbd.mcmc.DrawParameters(dataset_cbs_holdout, mcmc=5000, burnin=500, thin=50, 
                                              chains=1, use_data_augmentation = TRUE)

# Sample Number Of Future Transactions Based On Drawn Parameters
pnbd.xstar.draws.holdout = mcmc.DrawFutureTransactions(dataset_cbs_holdout, pnbd.draws.holdout, T.star = dataset_cbs_holdout$T.star)

# Conditional expectations
dataset_cbs_holdout$xstar.pnbd.hb = apply(pnbd.xstar.draws.holdout, 2, median)   # Each column applies to a customer (takes the mean)
dataset_cbs_holdout$xstar.pnbd.hb.c1 = apply(pnbd.xstar.draws.holdout, 2, cred.int)[1,]
dataset_cbs_holdout$xstar.pnbd.hb.c2 = apply(pnbd.xstar.draws.holdout, 2, cred.int)[2,]

# P(active)
dataset_cbs_holdout$pactive.pnbd.hb = mcmc.PActive(pnbd.xstar.draws.holdout)
# P(alive)
dataset_cbs_holdout$palive.pnbd.hb = mcmc.PAlive(pnbd.draws.holdout)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
cohort.draws.holdout = pnbd.draws.holdout$level_2
# Compute median across draws
cohort.draws.holdout.median = apply(as.matrix(cohort.draws.holdout), 2, median)

# Collecting predictions for both models (PNBD and NBD into a dataframe)
rbind("Actuals" = c("Holdout" = sum(dataset_cbs_holdout$x.star)),
      "NBD" = round(sum(dataset_cbs_holdout$xstar.nbd)),
      "Pareto/NBD (HB)" = round(sum(dataset_cbs_holdout$xstar.pnbd.hb)))

# Instantiating model outputs 
cal.cbs = dataset_cbs_holdout[,c("x", "t.x", "T.cal")]
x.star = as.vector(dataset_cbs_holdout[,"x.star"])
nbd.x.star.pred = as.vector(dataset_cbs_holdout[,"xstar.nbd"])
pnbd.x.star.pred = as.vector(dataset_cbs_holdout[,"xstar.pnbd.hb"])
pnbd.x.star.pred.c1 = as.vector(dataset_cbs_holdout[,"xstar.pnbd.hb.c1"])
pnbd.x.star.pred.c2 = as.vector(dataset_cbs_holdout[,"xstar.pnbd.hb.c2"])

# Plotting the expected versus actual frequency of transactions for two models
set_plot_dimensions(8, 7)
comb_two.PlotFreqVsConditionalExpectedFrequency.new(T.star=182, cal.cbs, 
                                                    x.star, nbd.x.star.pred, pnbd.x.star.pred, pnbd.x.star.pred.c1,
                                                    pnbd.x.star.pred.c2, censor=100, 
                                                    xlab = "Calibration period transactions", 
                                                    ylab = "Holdout period transactions", 
                                                    xticklab = NULL, title = "Expectation No. Transactions in Holdout")

# Collecting the expected versus actual frequency of transactions for two models into a dataframe
exp_vs_actual.two_models = comb_two.FreqVsConditionalExpectedFrequency.new(T.star=182, cal.cbs, x.star, nbd.x.star.pred, 
                                                                           pnbd.x.star.pred, pnbd.x.star.pred.c1,
                                                                           pnbd.x.star.pred.c2, censor=100)
exp_vs_actual.two_models.tens = sapply(seq(1,99,by=10),function(i) rowSums(exp_vs_actual.two_models[,i:(i+9)]))
exp_vs_actual.two_models.tens

# Calculating the Mean Absolute Error for each model
mae.nbd = mae(dataset_cbs_holdout$x.star, dataset_cbs_holdout$xstar.nbd) 
mae.pnbd = mae(dataset_cbs_holdout$x.star, dataset_cbs_holdout$xstar.pnbd.hb)

# Calculating the Mean Squared Logarithmic Error for each model
msle.nbd = msle(preds = dataset_cbs_holdout$xstar.nbd, actuals = dataset_cbs_holdout$x.star)
msle.pnbd = msle(preds = dataset_cbs_holdout$xstar.pnbd.hb, actuals = dataset_cbs_holdout$x.star)

# Calculating the Root Mean Squared Error for each model
rmse.nbd = rmse(preds = dataset_cbs_holdout$xstar.nbd, actuals = dataset_cbs_holdout$x.star)
rmse.pnbd = rmse(preds = dataset_cbs_holdout$xstar.pnbd.hb, actuals = dataset_cbs_holdout$x.star)

# Combining each error metric into a dataframe
rbind("NBD" = c("MAE" = round(mae.nbd, 3), "MSLE" = round(msle.nbd, 4), "RMSE" = round(rmse.nbd, 4)),
      "Pareto/NBD" = c(round(mae.pnbd, 3), round(msle.pnbd, 3), round(rmse.pnbd, 3)))

# Calculating the difference the actual transactions in the holdout versus predicted
dataset_cbs_holdout$diff = dataset_cbs_holdout$x.star - dataset_cbs_holdout$xstar.pnbd.hb
dataset_cbs_holdout$diff.rank = rank(dataset_cbs_holdout$diff, na.last = TRUE, ties.method = c("random"))

# Getting a list of the top 10 worst overestimated customers 
customer_list = list()
for (rank in seq(from=1, to=10, by=1)) {
    cust = subset(dataset_cbs_holdout, diff.rank==rank)$cust
    customer_list[[rank]] = cust}

# Plotting the transaction timing patterns for the 10 worst overestimated customers
plotTimingPatterns.cust_list(dataset, n=10, customer_list, T.cal = "2018-07-31", headers = c("Past", "Future"), title = "Transaction Timing Patterns for Overestimated Customers")

# Calculating the difference the predicted transactions in the holdout versus actual
dataset_cbs_holdout$diff_v2 = dataset_cbs_holdout$xstar.pnbd.hb - dataset_cbs_holdout$x.star
dataset_cbs_holdout$diff.rank_v2 = rank(dataset_cbs_holdout$diff_v2, na.last = TRUE, ties.method = c("random"))

# Getting a list of the top 10 worst underestimated customers 
customer_list_v2 = list()
for (rank in seq(from=1, to=10, by=1)) {
    cust = subset(dataset_cbs_holdout, diff.rank_v2==rank)$cust
    customer_list_v2[[rank]] = cust}

# Plotting the transaction timing patterns for the 10 worst underestimated customers
plotTimingPatterns.cust_list(dataset, n=10, customer_list_v2, T.cal = "2018-07-31", headers = c("Past", "Future"), title = "Transaction Timing Patterns for Underestimated Customers")

# Getting a list of unique customer IDs 
unique_cust = unique(dataset[c("cust")])

# Estimating the regularity of all unqiue customers using the MLE method 
for (i in 1:nrow(unique_cust)) {
    cust_i = dataset[dataset$cust == i,]
    reg_cust_i = estimateRegularity(cust_i, method = "mle")
    unique_cust[i,"reg"] = reg_cust_i}
dataset = merge(x = dataset, y = unique_cust, by = "cust", all.x = TRUE)

# Plotting a histogram of each customers regularity
hist(unique_cust$reg, breaks=50, col="slategray2", border="gray45",
     main = "Distribution of Customer Regularity", xlab = "Regularity", ylab = "Frequency",
     axes = TRUE, plot = TRUE, labels = TRUE, xlim=c(0,10) )

# Creating empty dataframe to store results
total_error_df = data.frame()

dataset.high_regularity.max = dataset %>% filter(reg >= 2.6)
max_cust.high_regularity = nrow(unique(dataset.high_regularity.max[c("cust")]))


# Here we trying to find the optimal threshold value of reglarity to divide the dataset into high regularity and low regularity.
# The optimal threshold is the one which minimises the total error of all combined predictions
# Looping through each threshold value
for (threshold in seq(from=1.0, to=2.6, by=0.2)){

    # Splitting dataset into high regularity versus low regularity 
    dataset.low_regularity = dataset %>% filter(reg < threshold)
    dataset.high_regularity = dataset %>% filter(reg >= threshold)
    
    # Taking a random sample of the low regularity dataset
    dataset.low_reg.unique.cust = unique(dataset.low_regularity[c("cust")])
    dataset.low_reg.unique.cust.sample = dataset.low_reg.unique.cust[sample(nrow(dataset.low_reg.unique.cust), 2500), ]
    dataset.low_regularity = filter(dataset.low_regularity, cust %in% dataset.low_reg.unique.cust.sample)

    # Taking a random sample of the high regularity dataset
    dataset.high_reg.unique.cust = unique(dataset.high_regularity[c("cust")])
    dataset.high_reg.unique.cust.sample = dataset.high_reg.unique.cust[sample(nrow(dataset.high_reg.unique.cust), max_cust.high_regularity), ]
    dataset.high_regularity = filter(dataset.high_regularity, cust %in% dataset.high_reg.unique.cust.sample)
    
    # Calculating CBS matrix for each dataset
    dataset_low_reg_cbs = elog2cbs(dataset.low_regularity, units="day", T.cal = "2018-01-31", T.tot = "2019-02-01")
    dataset_high_reg_cbs = elog2cbs(dataset.high_regularity, units="day", T.cal = "2018-01-31", T.tot = "2019-02-01")
    
    ## Low Regularity Dataset
    # Drawing parameters for Pareto NBD model 
    pnbd.draws = pnbd.mcmc.DrawParameters(dataset_low_reg_cbs, chains=1)
    # Sample Number Of Future Transactions Based On Drawn Parameters
    pnbd.xstar.draws = mcmc.DrawFutureTransactions(dataset_low_reg_cbs, pnbd.draws, T.star = dataset_low_reg_cbs$T.star)
    # Conditional expectations
    dataset_low_reg_cbs$xstar.pnbd.hb = apply(pnbd.xstar.draws, 2, mean)   # Each column applies to a customer (takes the mean)

    ## High Regularity Dataset
    # Drawing parameters for Pareto GGG model
    pggg.draws = pggg.mcmc.DrawParameters(dataset_high_reg_cbs, chains=1)
    # Generate draws for holdout period
    pggg.xstar.draws = mcmc.DrawFutureTransactions(dataset_high_reg_cbs, pggg.draws, T.star=dataset_high_reg_cbs$T.star)
    # Conditional expectations
    dataset_high_reg_cbs$xstar.pggg = apply(pggg.xstar.draws, 2, mean)  # Taking the mean of each column 

    # Calculating error on a customer level for both datasets
    mae.pnbd.low_reg = mae(dataset_low_reg_cbs$x.star, dataset_low_reg_cbs$xstar.pnbd.hb)
    mae.pggg.high_reg = mae(dataset_high_reg_cbs$x.star, dataset_high_reg_cbs$xstar.pggg)
    total_error = mae.pnbd.low_reg + mae.pggg.high_reg    
    
    # Collecting results into a dataframe
    results = data.frame(threshold, total_error)
    total_error_df =  rbind(total_error_df, results)
}

# Finding optimal threshold to minimise error
optim_threshold = total_error_df[total_error_df$total_error == min(total_error_df[,"total_error"]), "threshold"]

optim_threshold = 2.4 # I ran the above code and arrived at this answer. We will arrive at slightly different values every time the code is run. For reproducibility, I have included this line.

# Creating the high regularity and low regularity datasets 
dataset.high_regularity = dataset %>% filter(reg >= optim_threshold)
dataset.low_regularity = dataset %>% filter(reg < optim_threshold)

# Tabulating the total number of transactions split by high regularity and low regularity
rbind("Total" = c("num_transactions" = nrow(dataset)),
      "Low Reg" = nrow(dataset.low_regularity),
      "High Reg" = nrow(dataset.high_regularity))

# Tabulating the total number of customers split by high regularity and low regularity
rbind("Total" = c("num_customers" = nrow(unique(dataset[c("cust")]))),
      "Low Reg" = nrow(unique(dataset.low_regularity[c("cust")])),
      "High Reg" = nrow(unique(dataset.high_regularity[c("cust")])))

# Combining the unique number of customers into a dataframe
cust_reg_df = data.frame( X1=c("All Customers"=nrow(unique(dataset[c("cust")])), 
                               "Low Reg Customers"=nrow(unique(dataset.low_regularity[c("cust")])), 
                               "High Reg Customers"=nrow(unique(dataset.high_regularity[c("cust")]))))

# Plotting the distribution of customers in high vs low regularity in a barchart
barplot(t(as.matrix(cust_reg_df)), beside=TRUE, main="Distribution of Customer Regularity",
       col=c("darkblue","slategray2","slategray2"), border="gray45", ylim=c(0,6000))


# Plotting box plots of the regularity for the entire dataset
(k.mle = estimateRegularity.new(dataset, method = "mle", plot = TRUE))
title("Maximum Likelihood Estimate of Regularity", line = 3, cex.main=1.2, font=1)
title("Entire Dataset", line = 1.5, cex.main=1.0, font=1)

# Plotting box plots of the regularity for the low regularity dataset
(k.mle = estimateRegularity.new(dataset.low_regularity, method = "mle", plot = TRUE))
title("Maximum Likelihood Estimate of Regularity", line = 3, cex.main=1.2, font=1)
title("Low Regularity Dataset", line = 1.5, cex.main=1.0, font=1)

# Plotting box plots of the regularity for the high regularity dataset
(k.mle = estimateRegularity.new(dataset.high_regularity, method = "mle", plot = TRUE))
title("Maximum Likelihood Estimate of Regularity", line = 3, cex.main=1.2, font=1)
title("High Regularity Dataset", line = 1.5, cex.main=1.0, font=1)

##########################################
# Creating the CBS Matrix with holdout for the High Regularity dataset
dataset_high_reg_cbs_holdout = elog2cbs(dataset.high_regularity, units="day", T.cal="2018-07-31", T.tot="2019-02-01")
head(dataset_high_reg_cbs_holdout)

# Creating the CBS Matrix with holdout for the Low Regularity dataset
dataset_low_reg_cbs_holdout = elog2cbs(dataset.low_regularity, units="day", T.cal="2018-07-31", T.tot="2019-02-01")
head(dataset_low_reg_cbs_holdout)

# Estimating the parameters for the high regularity dataset
pggg.draws = pggg.mcmc.DrawParameters(dataset_high_reg_cbs_holdout, mcmc=5000, burnin=500, thin=50, chains=1) 

# Generate draws for holdout period
pggg.xstar.draws = mcmc.DrawFutureTransactions(dataset_high_reg_cbs_holdout, pggg.draws, T.star=dataset_high_reg_cbs_holdout$T.star)

# Conditional expectations
dataset_high_reg_cbs_holdout$xstar.pggg = apply(pggg.xstar.draws, 2, median)  # Taking the median of each column 

# Calculating Error Metrics 
mae.high_reg.pggg = mae(dataset_high_reg_cbs_holdout$x.star, dataset_high_reg_cbs_holdout$xstar.pggg)
msle.high_reg.pggg = msle(preds = dataset_high_reg_cbs_holdout$xstar.pggg, actuals = dataset_high_reg_cbs_holdout$x.star)
rmse.high_reg.pggg = rmse(preds = dataset_high_reg_cbs_holdout$xstar.pggg, actuals = dataset_high_reg_cbs_holdout$x.star)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
pggg.cohort.draws = pggg.draws$level_2
# Compute median across draws
pggg.cohort.draws.median = apply(as.matrix(pggg.cohort.draws), 2, median)


plot(pggg.draws$level_2)

# Estimating the parameters for the high regularity dataset
pnbd.draws.high_reg.holdout = pnbd.mcmc.DrawParameters(dataset_high_reg_cbs_holdout, mcmc=5000, burnin=500, thin=50, 
                                                       chains=1, use_data_augmentation = TRUE)

# Sample Number Of Future Transactions Based On Drawn Parameters
pnbd.xstar.draws.high_reg.holdout = mcmc.DrawFutureTransactions(dataset_high_reg_cbs_holdout, pnbd.draws.high_reg.holdout, T.star = dataset_high_reg_cbs_holdout$T.star)

# Conditional expectations
dataset_high_reg_cbs_holdout$xstar.pnbd.hb = apply(pnbd.xstar.draws.high_reg.holdout, 2, median)   # Each column applies to a customer (takes the mean)

# Calculating Error Metrics
mae.high_reg.pnbd = mae(dataset_high_reg_cbs_holdout$x.star, dataset_high_reg_cbs_holdout$xstar.pnbd.hb)
msle.high_reg.pnbd = msle(preds = dataset_high_reg_cbs_holdout$xstar.pnbd.hb, actuals = dataset_high_reg_cbs_holdout$x.star)
rmse.high_reg.pnbd = rmse(preds = dataset_high_reg_cbs_holdout$xstar.pnbd.hb, actuals = dataset_high_reg_cbs_holdout$x.star)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
pnbd.cohort.high_reg.draws = pnbd.draws.high_reg.holdout$level_2
# Compute median across draws
pnbd.cohort.high_reg.draws.median = apply(as.matrix(pnbd.cohort.high_reg.draws), 2, median)

# Estimating the parameters for the NBD model on the high regularity dataset
params.nbd.high_reg.holdout = nbd.EstimateParameters(dataset_high_reg_cbs_holdout)

# Conditional expectations
dataset_high_reg_cbs_holdout$xstar.nbd = nbd.ConditionalExpectedTransactions(params=params.nbd.high_reg.holdout, 
                                                                             T.star=182, x=dataset_high_reg_cbs_holdout$x, 
                                                                             T.cal=dataset_high_reg_cbs_holdout$T.cal)
# Calculating Error Metrics
mae.high_reg.nbd = mae(dataset_high_reg_cbs_holdout$x.star, dataset_high_reg_cbs_holdout$xstar.nbd)
msle.high_reg.nbd = msle(preds = dataset_high_reg_cbs_holdout$xstar.nbd, actuals = dataset_high_reg_cbs_holdout$x.star)
rmse.high_reg.nbd = rmse(preds = dataset_high_reg_cbs_holdout$xstar.nbd, actuals = dataset_high_reg_cbs_holdout$x.star)

# Combining predictions into a dataframe 
rbind("Actuals - High Reg" = c("Holdout" = sum(dataset_high_reg_cbs_holdout$x.star)),
      "NBD - High Reg" = round(sum(dataset_high_reg_cbs_holdout$xstar.nbd)),
      "Pareto/NBD - High Reg" = round(sum(dataset_high_reg_cbs_holdout$xstar.pnbd.hb)),
      "Pareto/GGG - High Reg" = round(sum(dataset_high_reg_cbs_holdout$xstar.pggg)))

# Combining error metrics into a dataframe 
rbind("NBD" = c("MAE" = round(mae.high_reg.nbd, 4), "MSLE" = round(msle.high_reg.nbd, 4), "RMSE" = round(rmse.high_reg.nbd, 4)),
      "Pareto/NBD - High Reg" = c(round(mae.high_reg.pnbd, 4), round(msle.high_reg.pnbd, 4), round(rmse.high_reg.pnbd, 4)),
      "Pareto/GGG - High Reg" = c(round(mae.high_reg.pggg, 4), round(msle.high_reg.pggg, 4), round(rmse.high_reg.pggg, 4)))

# Instantiating metrics
cal.cbs.high_reg = dataset_high_reg_cbs_holdout[,c("x", "t.x", "T.cal")]
x.star.high_reg = as.vector(dataset_high_reg_cbs_holdout[,"x.star"])
nbd.x.star.pred.high_reg = as.vector(dataset_high_reg_cbs_holdout[,"xstar.nbd"])
pnbd.x.star.pred.high_reg = as.vector(dataset_high_reg_cbs_holdout[,"xstar.pnbd.hb"])
pggg.x.star.pred.high_reg = as.vector(dataset_high_reg_cbs_holdout[,"xstar.pggg"])

# Plotting expected vs actual frequencies for three different models
comb_three.PlotFreqVsConditionalExpectedFrequency.new(T.star=365, cal.cbs.high_reg, x.star.high_reg, nbd.x.star.pred.high_reg, 
                                                      pnbd.x.star.pred.high_reg, pggg.x.star.pred.high_reg, censor=100, 
                                                      xlab = "Calibration period transactions", 
                                                      ylab = "Holdout period transactions", 
                                                      xticklab = NULL, title = "Expectation No. Transactions in Holdout")

# Tabulating expected vs actual frequencies for three different models
exp_vs_actual.comb_three = comb_three.FreqVsConditionalExpectedFrequency.new(pnbd.params.high_reg, params.nbd.high_reg, 
                                                                             pggg.params.high_reg, T.star=365, cal.cbs.high_reg, 
                                                                             x.star.high_reg, nbd.x.star.pred.high_reg, 
                                                                             pnbd.x.star.pred.high_reg, pggg.x.star.pred.high_reg, 
                                                                             censor=100)
exp_vs_actual.comb_three.tens = sapply(seq(1,99,by=10),function(i) rowSums(exp_vs_actual.comb_three[,i:(i+9)]))
exp_vs_actual.comb_three.tens


# Creating CBS Matrix for High Regularity dataset
dataset_high_reg_cbs_holdout.vf = elog2cbs(dataset.high_regularity, units="day", T.cal="2018-07-31", T.tot="2019-02-01")
head(dataset_high_reg_cbs_holdout.vf)

# Creating CBS Matrix for Low Regularity dataset
dataset_low_reg_cbs_holdout.vf = elog2cbs(dataset.low_regularity, units="day", T.cal="2018-07-31", T.tot="2019-02-01")
head(dataset_low_reg_cbs_holdout.vf)

# Estimating PNBD parameters for low regularity dataset 
pnbd.draws.low_reg.holdout.vf = pnbd.mcmc.DrawParameters(dataset_low_reg_cbs_holdout.vf, mcmc=5000, burnin=500, thin=50, 
                                                         chains=1, use_data_augmentation = TRUE)

# Sample Number Of Future Transactions Based On Drawn Parameters
pnbd.xstar.draws.low_reg.holdout.vf = mcmc.DrawFutureTransactions(dataset_low_reg_cbs_holdout.vf, pnbd.draws.low_reg.holdout.vf, T.star = dataset_low_reg_cbs_holdout.vf$T.star)

# Conditional expectations
dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb = apply(pnbd.xstar.draws.low_reg.holdout.vf, 2, median)   # Each column applies to a customer (takes the mean)
dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb.c1 = apply(pnbd.xstar.draws.low_reg.holdout.vf, 2, cred.int)[1,]
dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb.c2 = apply(pnbd.xstar.draws.low_reg.holdout.vf, 2, cred.int)[2,]

# P(active)
dataset_low_reg_cbs_holdout.vf$pactive.pnbd.hb.vf = mcmc.PActive(pnbd.xstar.draws.low_reg.holdout.vf)
# P(alive)
dataset_low_reg_cbs_holdout.vf$palive.pnbd.hb.vf = mcmc.PAlive(pnbd.draws.low_reg.holdout.vf)

# Calculating Error Metrics
mae.low_reg.pnbd.vf = mae(dataset_low_reg_cbs_holdout.vf$x.star, dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb)
msle.low_reg.pnbd.vf = msle(preds = dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb, actuals = dataset_low_reg_cbs_holdout.vf$x.star)
rmse.low_reg.pnbd.vf = rmse(preds = dataset_low_reg_cbs_holdout.vf$xstar.pnbd.hb, actuals = dataset_low_reg_cbs_holdout.vf$x.star)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
pnbd.cohort.low_reg.draws.vf = pnbd.draws.low_reg.holdout.vf$level_2
# Compute median across draws
pnbd.cohort.low_reg.draws.median.vf = apply(as.matrix(pnbd.cohort.low_reg.draws.vf), 2, median)

pggg.draws.high_reg.vf = pggg.mcmc.DrawParameters(dataset_high_reg_cbs_holdout.vf, mcmc=5000, burnin=500, thin=50, chains=1) 

# Generate draws for holdout period
pggg.xstar.draws.high_reg.vf = mcmc.DrawFutureTransactions(dataset_high_reg_cbs_holdout.vf, pggg.draws.high_reg.vf, T.star=dataset_high_reg_cbs_holdout.vf$T.star)

# Conditional expectations
dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.vf = apply(pggg.xstar.draws.high_reg.vf, 2, median)  # Taking the median of each column 
dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.c1.vf = apply(pggg.xstar.draws.high_reg.vf, 2, cred.int)[1,]
dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.c2.vf = apply(pggg.xstar.draws.high_reg.vf, 2, cred.int)[2,]

# P(active)
dataset_high_reg_cbs_holdout.vf$pactive.pggg.high_reg.vf = mcmc.PActive(pggg.xstar.draws.high_reg.vf)
# P(alive)
dataset_high_reg_cbs_holdout.vf$palive.pggg.high_reg.vf = mcmc.PAlive(pggg.draws.high_reg.vf)

# Calculating Error Metrics
mae.high_reg.pggg.vf = mae(dataset_high_reg_cbs_holdout.vf$x.star, dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.vf)
msle.high_reg.pggg.vf = msle(preds = dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.vf, actuals = dataset_high_reg_cbs_holdout.vf$x.star)
rmse.high_reg.pggg.vf = rmse(preds = dataset_high_reg_cbs_holdout.vf$xstar.pggg.high_reg.vf, actuals = dataset_high_reg_cbs_holdout.vf$x.star)

# Convert cohort-level draws from coda::mcmc.list to a matrix, with each parameter becoming a column, and each draw a row
pggg.cohort.draws.high_reg.vf = pggg.draws.high_reg.vf$level_2
# Compute median across draws
pggg.cohort.draws.high_reg.median.vf = apply(as.matrix(pggg.cohort.draws.high_reg.vf), 2, median)

# Concatening both dataframes into one
dataset_cbs_holdout.vf = smartbind(dataset_low_reg_cbs_holdout.vf, dataset_high_reg_cbs_holdout.vf)

# Combining values from Low Reg and High Reg Datasets
dataset_cbs_holdout.vf$xstar.pnbd.pggg = rowSums(dataset_cbs_holdout.vf[,c("xstar.pnbd.hb", "xstar.pggg.high_reg.vf")], na.rm=TRUE)
dataset_cbs_holdout.vf$xstar.pnbd.pggg.c1 = rowSums(dataset_cbs_holdout.vf[,c("xstar.pnbd.hb.c1", "xstar.pggg.high_reg.c1.vf")], na.rm=TRUE)
dataset_cbs_holdout.vf$xstar.pnbd.pggg.c2 = rowSums(dataset_cbs_holdout.vf[,c("xstar.pnbd.hb.c2", "xstar.pggg.high_reg.c2.vf")], na.rm=TRUE)
dataset_cbs_holdout.vf$pactive.pnbd.pggg = rowSums(dataset_cbs_holdout.vf[,c("pactive.pnbd.hb.vf", "pactive.pggg.high_reg.vf")], na.rm=TRUE)
dataset_cbs_holdout.vf$palive.pnbd.pggg = rowSums(dataset_cbs_holdout.vf[,c("palive.pnbd.hb.vf", "palive.pggg.high_reg.vf")], na.rm=TRUE)

# Calculating Error Metrics
mae.pnbd.pggg.vf = mae(dataset_cbs_holdout.vf$x.star, dataset_cbs_holdout.vf$xstar.pnbd.pggg) 
msle.pnbd.pggg.vf = msle(preds = dataset_cbs_holdout.vf$xstar.pnbd.pggg, actuals = dataset_cbs_holdout.vf$x.star)
rmse.pnbd.pggg.vf = rmse(preds = dataset_cbs_holdout.vf$xstar.pnbd.pggg, actuals = dataset_cbs_holdout.vf$x.star)

pnbd.draws.holdout.vf = pnbd.mcmc.DrawParameters(dataset_cbs_holdout.vf, mcmc=5000, burnin=500, thin=50, 
                                                 chains=1, use_data_augmentation = TRUE)

# Sample Number Of Future Transactions Based On Drawn Parameters
pnbd.xstar.draws.holdout.vf = mcmc.DrawFutureTransactions(dataset_cbs_holdout.vf, pnbd.draws.holdout.vf, T.star = dataset_cbs_holdout.vf$T.star)

# Conditional expectations
dataset_cbs_holdout.vf$xstar.pnbd.hb = apply(pnbd.xstar.draws.holdout.vf, 2, median)   # Each column applies to a customer (takes the mean)
dataset_cbs_holdout.vf$xstar.pnbd.hb.c1 = apply(pnbd.xstar.draws.holdout.vf, 2, cred.int)[1,]
dataset_cbs_holdout.vf$xstar.pnbd.hb.c2 = apply(pnbd.xstar.draws.holdout.vf, 2, cred.int)[2,]

# Calculating MAE
mae.pnbd.vf = mae(dataset_cbs_holdout.vf$x.star, dataset_cbs_holdout.vf$xstar.pnbd.hb) 
msle.pnbd.vf = msle(preds = dataset_cbs_holdout.vf$xstar.pnbd.hb, actuals = dataset_cbs_holdout.vf$x.star)
rmse.pnbd.vf = rmse(preds = dataset_cbs_holdout.vf$xstar.pnbd.hb, actuals = dataset_cbs_holdout.vf$x.star)

pggg.draws.vf = pggg.mcmc.DrawParameters(dataset_cbs_holdout.vf, mcmc=5000, burnin=500, thin=50, chains=1) 

# Generate draws for holdout period
pggg.xstar.draws.vf = mcmc.DrawFutureTransactions(dataset_cbs_holdout.vf, pggg.draws.vf, T.star=dataset_cbs_holdout.vf$T.star)

# Conditional expectations
dataset_cbs_holdout.vf$xstar.pggg.vf = apply(pggg.xstar.draws.vf, 2, median)  # Taking the median of each column 

# Calculating Error Metrics 
mae.pggg.vf = mae(dataset_cbs_holdout.vf$x.star, dataset_cbs_holdout.vf$xstar.pggg.vf)
msle.pggg.vf = msle(preds = dataset_cbs_holdout.vf$xstar.pggg.vf, actuals = dataset_cbs_holdout.vf$x.star)
rmse.pggg.vf = rmse(preds = dataset_cbs_holdout.vf$xstar.pggg.vf, actuals = dataset_cbs_holdout.vf$x.star)

params.nbd.holdout.vf = nbd.EstimateParameters(dataset_cbs_holdout.vf)

dataset_cbs_holdout.vf$xstar.nbd = nbd.ConditionalExpectedTransactions(params = params.nbd.holdout.vf, 
                                                                       T.star = 182, x = dataset_cbs_holdout.vf$x, 
                                                                       T.cal = dataset_cbs_holdout.vf$T.cal)

# Calculating MAE
mae.nbd.vf = mae(dataset_cbs_holdout.vf$x.star, dataset_cbs_holdout.vf$xstar.nbd) 
msle.nbd.vf = msle(preds = dataset_cbs_holdout.vf$xstar.nbd, actuals = dataset_cbs_holdout.vf$x.star)
rmse.nbd.vf = rmse(preds = dataset_cbs_holdout.vf$xstar.nbd, actuals = dataset_cbs_holdout.vf$x.star)

rbind("Actuals" = c("Holdout" = sum(dataset_cbs_holdout.vf$x.star)),
      "NBD" = round(sum(dataset_cbs_holdout.vf$xstar.nbd)),
      "Pareto/NBD" = round(sum(dataset_cbs_holdout.vf$xstar.pnbd.hb)),
      "Pareto/GGG" = round(sum(dataset_cbs_holdout.vf$xstar.pggg.vf)),
      "Pareto/NBD & Pareto/GGG " = round(sum(dataset_cbs_holdout.vf$xstar.pnbd.pggg))
     )

rbind("NBD" = c("MAE" = round(mae.nbd.vf, 3), "MSLE" = round(msle.nbd.vf, 3), "RMSE" = round(rmse.nbd.vf, 3)),
      "Pareto/NBD" = c(round(mae.pnbd.vf, 3), round(msle.pnbd.vf, 3), round(rmse.pnbd.vf, 3) ),
      "Pareto/GGG" = c(round(mae.pggg.vf, 3), round(msle.pggg.vf, 3), round(rmse.pggg.vf, 3) ),
      "Pareto/NBD & Pareto/GGG" = c(round(mae.pnbd.pggg.vf, 3), round(msle.pnbd.pggg.vf, 3), round(rmse.pnbd.pggg.vf, 3))
     )

cal.cbs = dataset_cbs_holdout.vf[,c("x", "t.x", "T.cal")]
x.star = as.vector(dataset_cbs_holdout.vf[,"x.star"])
pnbd.x.star.pred = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.hb"])
pnbd.x.star.pred.c1 = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.hb.c1"])
pnbd.x.star.pred.c2 = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.hb.c2"])
pnbd.pggg.x.star.pred = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.pggg"])
pnbd.pggg.x.star.pred.c1 = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.pggg.c1"])
pnbd.pggg.x.star.pred.c2 = as.vector(dataset_cbs_holdout.vf[,"xstar.pnbd.pggg.c2"])

comb_four.PlotFreqVsConditionalExpectedFrequency.new(T.star=182, cal.cbs, 
                                                     x.star, 
                                                     pnbd.x.star.pred, pnbd.x.star.pred.c1, pnbd.x.star.pred.c2,
                                                     pnbd.pggg.x.star.pred, pnbd.pggg.x.star.pred.c1, pnbd.pggg.x.star.pred.c2, 
                                                     censor=100, xlab = "Calibration period transactions", 
                                                     ylab = "Holdout period transactions", 
                                                     xticklab = NULL, title = "Expectation No. Transactions in Holdout")

# Removing unnessecary columns
dataset_cbs_holdout.vff = subset(dataset_cbs_holdout.vf, select = -c(xstar.pnbd.hb, pactive.pnbd.hb.vf, palive.pnbd.hb.vf, xstar.pnbd.pggg.c1, xstar.pnbd.pggg.c2, xstar.pggg.high_reg.vf, xstar.pggg.high_reg.c1.vf, xstar.pggg.high_reg.c2.vf, xstar.pnbd.hb.c1, xstar.pnbd.hb.c2, pactive.pggg.high_reg.vf, palive.pggg.high_reg.vf, xstar.pggg.vf, xstar.nbd))
dataset_cbs_holdout.vff

dataset_low_reg_cbs.vf = elog2cbs(dataset.low_regularity, units="day", T.cal=max(dataset.low_regularity$date), T.tot=max(dataset.low_regularity$date))
head(dataset_low_reg_cbs.vf)

dataset_high_reg_cbs.vf = elog2cbs(dataset.high_regularity, units="day", T.cal=max(dataset.high_regularity$date), T.tot=max(dataset.high_regularity$date))
head(dataset_high_reg_cbs.vf)

# Sample Number Of Future Transactions Based On Drawn Parameters
pnbd.xstar.draws.low_reg.vf = mcmc.DrawFutureTransactions(dataset_low_reg_cbs.vf, pnbd.draws.low_reg.holdout.vf, T.star=365)

# Conditional expectations
dataset_low_reg_cbs.vf$xstar.pnbd.hb = apply(pnbd.xstar.draws.low_reg.vf, 2, median)   # Each column applies to a customer (takes the mean)
dataset_low_reg_cbs.vf$xstar.pnbd.hb.c1 = apply(pnbd.xstar.draws.low_reg.vf, 2, cred.int)[1,]
dataset_low_reg_cbs.vf$xstar.pnbd.hb.c2 = apply(pnbd.xstar.draws.low_reg.vf, 2, cred.int)[2,]

# P(active)
dataset_low_reg_cbs.vf$pactive.pnbd.hb = mcmc.PActive(pnbd.xstar.draws.low_reg.vf)
# P(alive)
dataset_low_reg_cbs.vf$palive.pnbd.hb = mcmc.PAlive(pnbd.draws.low_reg.holdout.vf)

# Sample Number Of Future Transactions Based On Drawn Parameters
pggg.xstar.draws.high_reg.vf = mcmc.DrawFutureTransactions(dataset_high_reg_cbs.vf, pggg.draws.high_reg.vf, T.star=365)

# Conditional expectations
dataset_high_reg_cbs.vf$xstar.pggg = apply(pggg.xstar.draws.high_reg.vf, 2, median)  # Taking the median of each column 
dataset_high_reg_cbs.vf$xstar.pggg.c1 = apply(pggg.xstar.draws.high_reg.vf, 2, cred.int)[1,]
dataset_high_reg_cbs.vf$xstar.pggg.c2 = apply(pggg.xstar.draws.high_reg.vf, 2, cred.int)[2,]

# P(active)
dataset_high_reg_cbs.vf$pactive.pggg = mcmc.PActive(pggg.xstar.draws.high_reg.vf)
# P(alive)
dataset_high_reg_cbs.vf$palive.pggg = mcmc.PAlive(pggg.draws.high_reg.vf)

# Concatening both dataframes into one
dataset_cbs.vf = smartbind(dataset_low_reg_cbs.vf, dataset_high_reg_cbs.vf)

# Combining values from Low Reg and High Reg Datasets
dataset_cbs.vf$xstar.pnbd.pggg = rowSums(dataset_cbs.vf[,c("xstar.pnbd.hb", "xstar.pggg")], na.rm=TRUE)
dataset_cbs.vf$xstar.pnbd.pggg.c1 = rowSums(dataset_cbs.vf[,c("xstar.pnbd.hb.c1", "xstar.pggg.c1")], na.rm=TRUE)
dataset_cbs.vf$xstar.pnbd.pggg.c2 = rowSums(dataset_cbs.vf[,c("xstar.pnbd.hb.c2", "xstar.pggg.c2")], na.rm=TRUE)

dataset_cbs.vf$pactive.pnbd.pggg = rowSums(dataset_cbs.vf[,c("pactive.pnbd.hb", "pactive.pggg")], na.rm=TRUE)
dataset_cbs.vf$palive.pnbd.pggg = rowSums(dataset_cbs.vf[,c("palive.pnbd.hb", "palive.pggg")], na.rm=TRUE)

# Removing unnessecary columns
dataset_cbs.vf = subset(dataset_cbs.vf, select = -c(xstar.pnbd.hb, pactive.pnbd.hb, palive.pnbd.hb, xstar.pggg, pactive.pggg, palive.pggg))

cust.mean.sales = subset(dataset_unique_cust, select = c(cust, mean_sales))
dataset_cbs.vf = merge(dataset_cbs.vf, cust.mean.sales, by = "cust")

dataset_cbs.vf$cust.value = dataset_cbs.vf$xstar.pnbd.pggg * dataset_cbs.vf$mean_sales

results = subset(dataset_cbs.vf, select = c(cust, xstar.pnbd.pggg, xstar.pnbd.pggg.c1, xstar.pnbd.pggg.c2, pactive.pnbd.pggg, palive.pnbd.pggg, cust.value))
head(results, 10)

rbind("Threshold = 0.95" = c("No. Customers Alive" = nrow(results %>% filter(palive.pnbd.pggg > 0.95))),
      "Threshold = 0.80" = nrow(results %>% filter(palive.pnbd.pggg > 0.80)),
      "Threshold = 0.60" = nrow(results %>% filter(palive.pnbd.pggg > 0.60)),
      "Threshold = 0.40" = nrow(results %>% filter(palive.pnbd.pggg > 0.40)),
      "Threshold = 0.20" = nrow(results %>% filter(palive.pnbd.pggg > 0.20)))

rbind("Threshold = 0.95" = c("No. Customers Active" = nrow(results %>% filter(pactive.pnbd.pggg > 0.95))),
      "Threshold = 0.80" = nrow(results %>% filter(pactive.pnbd.pggg > 0.80)),
      "Threshold = 0.60" = nrow(results %>% filter(pactive.pnbd.pggg > 0.60)),
      "Threshold = 0.40" = nrow(results %>% filter(pactive.pnbd.pggg > 0.40)),
      "Threshold = 0.20" = nrow(results %>% filter(pactive.pnbd.pggg > 0.20)))

sum(results$xstar.pnbd.pggg)
sum(results$xstar.pnbd.pggg.c1)
sum(results$xstar.pnbd.pggg.c2)

results %>% filter(palive.pnbd.pggg < 0.2) %>% select(cust)

dataset_cbs.vf$value.rank = rank(dataset_cbs.vf$cust.value, na.last=TRUE, ties.method=c("random"))

most.val.customer.list = list()
for (rank in seq(from=1, to=100, by=1)) {
    cust = subset(dataset_cbs.vf, value.rank==rank)$cust
    most.val.customer.list[[rank]] = cust
}
most.val.customer.list
