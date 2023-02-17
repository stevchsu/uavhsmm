#' ======================
#' Compare HRB_HW and HRB_LW among three countries 
#' 1. only keep data when action has changed
#' 2. Draw plots to show the 
#'  
#' ======================
library(dplyr)
library(data.table)
library(rpart)
library(rpart.plot)
library(ykang)
library(sqldf)
library(ggplot2)

# load file
## HW
tk_hrb_hw <- fread("data/transformed_distance/TK/TK_HRB_HW.csv")
tw_hrb_hw <- fread("data/transformed_distance/TW/TW_HRB_HW.csv")
us_hrb_hw <- fread("data/transformed_distance/US/US_HRB_HW.csv")

## LW
tk_hrb_lw <- fread("data/transformed_distance/TK/TK_HRB_LW.csv")
tw_hrb_lw <- fread("data/transformed_distance/TW/TW_HRB_LW.csv")
us_hrb_lw <- fread("data/transformed_distance/US/US_HRB_LW.csv")

## HW 
tk_hrb_hw$nation <- "tk"
tw_hrb_hw$nation <- "tw"
us_hrb_hw$nation <- "us"

## LW
tk_hrb_lw$nation <- "tk"
tw_hrb_lw$nation <- "tw"
us_hrb_lw$nation <- "us"

hrb_hw <- rbind(tk_hrb_hw, tw_hrb_hw, us_hrb_hw)
hrb_hw$loading <- "hw"
hrb_lw <- rbind(tk_hrb_lw, tw_hrb_lw, us_hrb_lw)
hrb_lw$loading <- "lw"

## remove V1
hrb_hw <- hrb_hw %>% select(-V1)
hrb_lw <- hrb_lw %>% select(-V1)

hrb_hw_nation_user_count <- hrb_hw %>% group_by(nation, file_name) %>% count()
ggplot(hrb_hw_nation_user_count, aes(x = n, color = nation)) + geom_density() + 
  theme(text = element_text(size = 18)) + ylim(0, 0.016) + xlim(130, 340) + ggtitle("Number of action change by user in HRB_HW")

hrb_lw_nation_user_count <- hrb_lw %>% group_by(nation, file_name) %>% count()
ggplot(hrb_lw_nation_user_count, aes(x = n, color = nation)) + geom_density() +
  theme(text = element_text(size = 18)) + ylim(0, 0.016) + xlim(130, 340) + ggtitle("Number of action change by user in HRB_LW")


table(hrb_hw$action.y); round(prop.table(table(hrb_hw$action.y)), 3)
table(hrb_lw$action.y); round(prop.table(table(hrb_lw$action.y)), 3)

table(hrb_hw$nation)
table(hrb_hw$nation, hrb_hw$action.y); round(prop.table(table(hrb_hw$nation, hrb_hw$action.y),margin = 1), 3)

table(hrb_lw$nation)
table(hrb_lw$nation, hrb_lw$action.y); round(prop.table(table(hrb_lw$nation, hrb_lw$action.y),margin = 1), 3)

### statistical test

summary(aov(formula = n ~ nation, data = hrb_hw_nation_user_count))
summary(aov(formula = n ~ nation, data = hrb_lw_nation_user_count))

hrb <- rbind(hrb_hw, hrb_lw)

chisq.test(table(hrb$nation, hrb$action.y))
chisq.test(table(hrb$loading, hrb$action.y))

chisq.test(table(hrb_hw$nation, hrb_hw$action.y))
# X-squared = 69.57, df = 6, p-value = 5.009e-13
chisq.test(table(hrb_lw$nation, hrb_lw$action.y))
# X-squared = 24.99, df = 6, p-value = 0.0003429

chisq.test(rbind(table(hrb_hw[nation == "tk"]$action.y), table(hrb_lw[nation == "tk"]$action.y)))
# X-squared = 39.212, df = 3, p-value = 1.565e-08
chisq.test(rbind(table(hrb_hw[nation == "tw"]$action.y), table(hrb_lw[nation == "tw"]$action.y)))
# X-squared = 58.893, df = 3, p-value = 1.013e-12
chisq.test(rbind(table(hrb_hw[nation == "us"]$action.y), table(hrb_lw[nation == "us"]$action.y)))
# X-squared = 22.536, df = 3, p-value = 5.046e-05


## remove unused columns
data <- hrb_lw %>% select(-c(# is_engage_payload, n_correct, n_incorrect, 
                             v1_target, v2_target, v3_target, v4_target, v5_target))

data$payload_act <- as.factor(as.character(data$payload_act))
data$VHcollision <- ifelse(is.na(data$VHcollision) , 0, data$VHcollision)
data <- na.omit(data)

## predict Y in next second with X in current
data$next_action <- ""
data[1:nrow(data)-1]$next_action <- data[2:nrow(data)]$action.y

data <- data[1:nrow(data)-1,] # remove last observation
data$next_action <- as.factor(data$next_action)

##### train CART #####
# outcome variable: action
# sample 70% data as training set
set.seed(1)

train_idx <- sample(seq(nrow(data)), size = 0.7 * nrow(data), replace = F)
test_idx <- setdiff(seq(nrow(data)), train_idx)

round(prop.table(table(data$next_action)), 3)
# prop.table(table(data$next_action))

# Calculate case weight by training set
case_weight <- round(1/round(prop.table(table(data[train_idx,]$next_action)), 3), 2);case_weight

data$weight <- 1

data$weight <- ifelse(data$next_action == "replanPath", yes = case_weight[3], no = data$weight)
data$weight <- ifelse(data$next_action == "applyAutomation", yes = case_weight[1], no = data$weight)
data$weight <- ifelse(data$next_action == "engagePayload", yes = case_weight[2], no = data$weight)
data$weight <- ifelse(data$next_action == "watch", yes = case_weight[4], no = data$weight)


## Fit rpart
rpart_model <- rpart(next_action ~ . -file_name - event_id - time - weight - action.y,
                     data = data[train_idx,], control = rpart.control(cp = 0.001), 
                     weights = data[train_idx]$weight)

rpart_model$cptable
rpart_model$cptable[,3] > rpart_model$cptable[, 4] - rpart_model$cptable[, 5] 
rpart_model$cptable[5] # best cp = 0.005718471

rpart_model_pruned <- prune(rpart_model, cp = 0.005718471)

print(rpart_model_pruned)

prp(rpart_model)
prp(rpart_model_pruned, cex=1.2)

rpart.rules(rpart_model_pruned)

## Inference
library(Metrics)

round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(rpart_model, data[train_idx,], type = "class",)), 4)
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model, data[-train_idx,], type = "class",)), 4)

round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[train_idx,], type = "class",)), 4)
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[-train_idx,], type = "class",)), 4)


#### randomForest #####
library(randomForest)
library(ranger)

lapply(data, function(col) sum(is.na(col)))
ranger_model <- ranger(next_action ~ . -file_name - event_id - time - weight - action.y,
                         data = data[train_idx,], importance = "impurity", case.weights = data[train_idx,]$weight)

importance(ranger_model)[order(importance(ranger_model), decreasing = T)][1:10]

ranger_train_pred <- predict(ranger_model, data[train_idx,], type = "response", )
round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(ranger_model, data[train_idx,], type = "response", )$prediction), 4) # 0.9479
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(ranger_model, data[-train_idx,], type = "response",)$prediction), 4) # 0.5466


##### CTHSMM #####

# uav_cthsmm <- cthsmm(rpart_tree = rpart_model, data = data, 
#                      ID_Col = "file_name", stateDuration_Col = "event_id", obsColname = "next_action", aggDivisor = 3600,verbose = T)
# 
# sqlState = paste('select ', "file_name", ', ', "action.y", 
#                  ', sum(', "event_id" ,') / ', 3600,
#                  ' as Duration from data group by Agg_ID', sep = '');
# agg_data = sqldf(sqlState)
# data[,"action.y"]