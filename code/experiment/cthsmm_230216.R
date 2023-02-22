#' ======================
#' Compare HRB_HW and HRB_LW among three countries 
#' 1. only keep data when action has changed
#' 2. Draw plots to show the 
#'  
#' 
#' ======================

library(dplyr)
library(data.table)
library(rpart)
library(rpart.plot)
library(ykang)
library(sqldf)
library(ggplot2)
library(mhsmm)

##### load data and preprocessing #####
## LW
tk_hrb_lw <- fread("data/transformed_distance/TK/TK_HRB_LW.csv")
tw_hrb_lw <- fread("data/transformed_distance/TW/TW_HRB_LW.csv")
us_hrb_lw <- fread("data/transformed_distance/US/US_HRB_LW.csv")

## LW
tk_hrb_lw$nation <- "tk"
tw_hrb_lw$nation <- "tw"
us_hrb_lw$nation <- "us"

hrb_lw <- rbind(tk_hrb_lw, tw_hrb_lw, us_hrb_lw)
hrb_lw$loading <- "lw"

## remove V1
hrb_lw <- hrb_lw %>% select(-V1)

hrb_lw_nation_user_count <- hrb_lw %>% group_by(nation, file_name) %>% count()
ggplot(hrb_lw_nation_user_count, aes(x = n, color = nation)) + geom_density() +
  theme(text = element_text(size = 18)) + ylim(0, 0.016) + xlim(130, 340) + ggtitle("Number of action change by user in HRB_LW")

table(hrb_lw$action.y); round(prop.table(table(hrb_lw$action.y)), 3)

table(hrb_lw$nation)
table(hrb_lw$nation, hrb_lw$action.y); round(prop.table(table(hrb_lw$nation, hrb_lw$action.y),margin = 1), 3)

## remove unused columns
data <- hrb_lw %>% select(-c(# is_engage_payload, n_correct, n_incorrect, 
  v1_target, v2_target, v3_target, v4_target, v5_target))

# data$payload_act <- as.factor(data$payload_act)
data$VHcollision <- ifelse(is.na(data$VHcollision) , 0, data$VHcollision)
data <- na.omit(data)

## remove "." from file_name -> didn't solve problem
data$file_name <- sapply(data$file_name, function(x) strsplit(x,  split = ".", fixed = T)[[1]][1])

data$time <- as.numeric(data$time)

data$dur_time <- data[, dur_time := shift(event_id, - 1) - event_id, by = file_name]
data[is.na(data$dur_time),]$dur_time <- 1 # impute the first record of each user

data$payload_no <- ifelse(data$payload_act == -1, 1, 0)
data$payload_check <- ifelse(data$payload_act == 0, 1, 0) 
data$payload_hit <- ifelse(data$payload_act == 1, 1, 0)
data$payload_safe <- ifelse(data$payload_act == 2, 1, 0)


data <- data %>% select(-c(payload_act, time, event_id, loading))

## predict Y in next second with X in current
data$next_action <- ""
data[1:nrow(data)-1]$next_action <- data[2:nrow(data)]$action.y

data <- data[1:nrow(data)-1,] # remove last observation
data$next_action <- as.factor(data$next_action)

##### end of load data and preprocessing #####

##### train CART #####
# outcome variable: action
# sample 70% data as training set
set.seed(1)

train_idx <- sample(seq(nrow(data)), size = 0.7 * nrow(data), replace = F)
test_idx <- setdiff(seq(nrow(data)), train_idx)

round(prop.table(table(data$action.y)), 3)
round(prop.table(table(data$next_action)), 3)

# Calculate case weight by training set
case_weight <- round(1/round(prop.table(table(data[train_idx,]$next_action)), 3), 2);case_weight

data$weight <- 1

data$weight <- ifelse(data$next_action == "replanPath", yes = case_weight[3], no = data$weight)
data$weight <- ifelse(data$next_action == "applyAutomation", yes = case_weight[1], no = data$weight)
data$weight <- ifelse(data$next_action == "engagePayload", yes = case_weight[2], no = data$weight)
data$weight <- ifelse(data$next_action == "watch", yes = case_weight[4], no = data$weight)

## Fit rpart
rpart_model <- rpart(next_action ~ . -file_name - dur_time - weight - action.y,
                     data = data, control = rpart.control(cp = 0.001), 
                     weights = data$weight)

rpart_model$cptable
rpart_model$cptable[,3] > rpart_model$cptable[, 4] - rpart_model$cptable[, 5] 
rpart_model$cptable[6] # best cp = 0.007889518

rpart_model_pruned <- prune(rpart_model, cp = 0.007889518)

print(rpart_model_pruned)

prp(rpart_model_pruned, cex=1.2)

rpart.rules(rpart_model_pruned)

## Inference
library(Metrics)

round(accuracy(data[train_idx,]$next_action,
               predicted = predict(rpart_model, data[train_idx,], type = "class",)), 4) # 0.601
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model, data[-train_idx,], type = "class",)), 4) # 0.5375

round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[train_idx,], type = "class",)), 4) # 0.5209
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[-train_idx,], type = "class",)), 4) # 0.5213

##### end of CART #####

##### ranger #####
library(ranger)

# lapply(data, function(col) sum(is.na(col)))
ranger_model <- ranger(next_action ~ . -file_name - event_id - time - weight - action.y,
                       data = data[train_idx,], importance = "impurity", case.weights = data[train_idx,]$weight)

importance(ranger_model)[order(importance(ranger_model), decreasing = T)][1:10]

ranger_train_pred <- predict(ranger_model, data[train_idx,], type = "response", )
round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(ranger_model, data[train_idx,], type = "response", )$prediction), 4) # 0.9443
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(ranger_model, data[-train_idx,], type = "response",)$prediction), 4) # 0.6382

##### end of ranger #####

##### CTHSMM #####
source("code/cthsmm/cthsmm.R")
source("code/cthsmm/cthsmm_eval.R")
source("code/cthsmm/cthsmm_viterbi.R")

lrb_lw_cthsmm <- cthsmm(rpart_tree = rpart_model_pruned, data = data, 
                        stateDuration_Col = "dur_time", obsColname = "next_action", 
                        aggDivisor = 1, ID_Col = "file_name", verbose = T)

prp(rpart_model_pruned, type = 2, extra = 4, cex = 0.8)
round(lrb_lw_cthsmm$stm, 4)
round(lrb_lw_cthsmm$om, 4)
max(lrb_lw_cthsmm$statePDF_7(1:max(lrb_lw_cthsmm$agg_data$Duration))) # max prob in all state = 0.7343513 (state 7)

summary(lrb_lw_cthsmm$statePDF_1(1:10))

##### plot cthsmm sojourn density #####

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_1(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 1") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_2(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 2") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_3(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 3") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_4(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 4") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_5(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 5") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_6(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 6") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")

ggplot(data = data.frame(x = 1:max(lrb_lw_cthsmm$agg_data$Duration),
                         InStateProb = lrb_lw_cthsmm$statePDF_7(1:max(lrb_lw_cthsmm$agg_data$Duration)))) +
  geom_line(mapping = aes(x = x, y = InStateProb)) + xlab("Duration, sec") + ggtitle("State 7") +
  ylim(c(0, 0.75)) + xlim(c(0, 20)) + theme(text = element_text(size = 18)) + 
  geom_hline(yintercept = 0) + geom_vline(xintercept = 1, linetype = "dotted")
##### end of plot cthsmm sojourn density #####

# saveRDS(lrb_lw_cthsmm, "model/lrb_lw_cthsmm_230217.rds")
lrb_lw_cthsmm <- readRDS("model/lrb_lw_cthsmm_230217.rds")

cthsmm_eval(cthsmm = lrb_lw_cthsmm, test_data = data, 
            obsColname = "next_action", ID_Col = "file_name", stateDuration_Col = "dur_time", aggDivisor = 1, viterbi_len = 10)

