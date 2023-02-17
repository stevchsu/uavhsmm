library(dplyr)
library(data.table)
library(plyr)
library(zoo)
library(rpart)
library(rpart.plot)
library(ykang)
library(sqldf)


##### Preprocessing #####
# load file
raw_data <- fread("data/data_20221107/TK/TK_HRB_HW.csv")

data <- raw_data %>% select(-V1)

# impute "watch" to the rows after which are "applyAutomation"
for (i in seq(nrow(data))) {
  # print(paste0("i = ", as.character(i)))
  # print(data[i, "action"])
  if (i >= 2) {
    if (data[i-1, "action"] == "applyAutomation") {
      # print(data[i-1])
      data[i, "action"] <- "end"
    }
  }
}

# replace empty action with NA
data$action <- ifelse(data$action == "", NA, data$action)

# summarize and dummy encode vehicle status into two variable, is_vehicle_wait and is_engage_payload
data$is_vehicle_wait <- ifelse(data$v1_status == 2 | data$v2_status == 2 | 
                                 data$v3_status == 2 | data$v4_status == 2 | 
                                 data$v5_status == 2, 1, 0)
data$is_engage_payload <- ifelse(data$v1_status == 3 | data$v2_status == 3 | 
                                   data$v3_status == 3 | data$v4_status == 3 | 
                                   data$v5_status == 3, 1, 0)
data <- data %>% select(-c(v1_status, v2_status, v3_status, v4_status, v5_status))

# impute action in each user's first event with "watch"
data$action <- ifelse(data$event_id == 0, "watch", data$action)

# impute NA in payload_act, collision, is_damaged
data$payload_act <- as.factor(ifelse(is.na(data$payload_act), 0, data$payload_act))
data$collision <- ifelse(is.na(data$collision), 0, data$collision)
data$is_damaged <- ifelse(is.na(data$is_damaged), 0, data$is_damaged)

# impute NA in action with LOCF
impute_action <- ddply(data[, c("file_name", "event_id", "time", "action")], 
                       ~file_name, na.locf)
data <- inner_join(data, impute_action, by = c("file_name", "time", "event_id"))

data <- data %>% relocate(c(file_name, event_id, time, action.x, action.y), 
                          setdiff(colnames(data), c("file_name", "event_id", "time", "action.x", "action.y")))

# replace level "end" with "watch" in action
data$action.y <- ifelse(data$action.y == "end", "watch", data$action.y)
data <- data %>% select(-action.x)
data$action.y <- as.factor(data$action.y)

##### end of section #####

##### calculate distance between vehicles #####
data$dist_v1_v2 <- sqrt((data$v1_x - data$v2_x)^2 + (data$v1_y - data$v2_y)^2)
data$dist_v1_v3 <- sqrt((data$v1_x - data$v3_x)^2 + (data$v1_y - data$v3_y)^2)
data$dist_v1_v4 <- sqrt((data$v1_x - data$v4_x)^2 + (data$v1_y - data$v4_y)^2)
data$dist_v1_v5 <- sqrt((data$v1_x - data$v5_x)^2 + (data$v1_y - data$v5_y)^2)

data$dist_v2_v3 <- sqrt((data$v2_x - data$v3_x)^2 + (data$v2_y - data$v3_y)^2)
data$dist_v2_v4 <- sqrt((data$v2_x - data$v4_x)^2 + (data$v2_y - data$v4_y)^2)
data$dist_v2_v5 <- sqrt((data$v2_x - data$v5_x)^2 + (data$v2_y - data$v5_y)^2)

data$dist_v3_v4 <- sqrt((data$v3_x - data$v4_x)^2 + (data$v3_y - data$v4_y)^2)
data$dist_v3_v5 <- sqrt((data$v3_x - data$v5_x)^2 + (data$v3_y - data$v5_y)^2)
data$dist_v4_v5 <- sqrt((data$v4_x - data$v5_x)^2 + (data$v4_y - data$v5_y)^2)
##### end of section #####

##### calculate distance between each vehicle and target #####
data$dist_v1_ta <- sqrt((data$v1_x - data$ta_x)^2 + (data$v1_y - data$ta_y)^2)
data$dist_v1_tb <- sqrt((data$v1_x - data$tb_x)^2 + (data$v1_y - data$tb_y)^2)
data$dist_v1_tc <- sqrt((data$v1_x - data$tc_x)^2 + (data$v1_y - data$tc_y)^2)
data$dist_v1_td <- sqrt((data$v1_x - data$td_x)^2 + (data$v1_y - data$td_y)^2)
data$dist_v1_te <- sqrt((data$v1_x - data$te_x)^2 + (data$v1_y - data$te_y)^2)
data$dist_v1_tf <- sqrt((data$v1_x - data$tf_x)^2 + (data$v1_y - data$tf_y)^2)
data$dist_v1_tg <- sqrt((data$v1_x - data$tg_x)^2 + (data$v1_y - data$tg_y)^2)

data$dist_v2_ta <- sqrt((data$v2_x - data$ta_x)^2 + (data$v2_y - data$ta_y)^2)
data$dist_v2_tb <- sqrt((data$v2_x - data$tb_x)^2 + (data$v2_y - data$tb_y)^2)
data$dist_v2_tc <- sqrt((data$v2_x - data$tc_x)^2 + (data$v2_y - data$tc_y)^2)
data$dist_v2_td <- sqrt((data$v2_x - data$td_x)^2 + (data$v2_y - data$td_y)^2)
data$dist_v2_te <- sqrt((data$v2_x - data$te_x)^2 + (data$v2_y - data$te_y)^2)
data$dist_v2_tf <- sqrt((data$v2_x - data$tf_x)^2 + (data$v2_y - data$tf_y)^2)
data$dist_v2_tg <- sqrt((data$v2_x - data$tg_x)^2 + (data$v2_y - data$tg_y)^2)

data$dist_v3_ta <- sqrt((data$v3_x - data$ta_x)^2 + (data$v3_y - data$ta_y)^2)
data$dist_v3_tb <- sqrt((data$v3_x - data$tb_x)^2 + (data$v3_y - data$tb_y)^2)
data$dist_v3_tc <- sqrt((data$v3_x - data$tc_x)^2 + (data$v3_y - data$tc_y)^2)
data$dist_v3_td <- sqrt((data$v3_x - data$td_x)^2 + (data$v3_y - data$td_y)^2)
data$dist_v3_te <- sqrt((data$v3_x - data$te_x)^2 + (data$v3_y - data$te_y)^2)
data$dist_v3_tf <- sqrt((data$v3_x - data$tf_x)^2 + (data$v3_y - data$tf_y)^2)
data$dist_v3_tg <- sqrt((data$v3_x - data$tg_x)^2 + (data$v3_y - data$tg_y)^2)

data$dist_v4_ta <- sqrt((data$v4_x - data$ta_x)^2 + (data$v4_y - data$ta_y)^2)
data$dist_v4_tb <- sqrt((data$v4_x - data$tb_x)^2 + (data$v4_y - data$tb_y)^2)
data$dist_v4_tc <- sqrt((data$v4_x - data$tc_x)^2 + (data$v4_y - data$tc_y)^2)
data$dist_v4_td <- sqrt((data$v4_x - data$td_x)^2 + (data$v4_y - data$td_y)^2)
data$dist_v4_te <- sqrt((data$v4_x - data$te_x)^2 + (data$v4_y - data$te_y)^2)
data$dist_v4_tf <- sqrt((data$v4_x - data$tf_x)^2 + (data$v4_y - data$tf_y)^2)
data$dist_v4_tg <- sqrt((data$v4_x - data$tg_x)^2 + (data$v4_y - data$tg_y)^2)

data$dist_v5_ta <- sqrt((data$v5_x - data$ta_x)^2 + (data$v5_y - data$ta_y)^2)
data$dist_v5_tb <- sqrt((data$v5_x - data$tb_x)^2 + (data$v5_y - data$tb_y)^2)
data$dist_v5_tc <- sqrt((data$v5_x - data$tc_x)^2 + (data$v5_y - data$tc_y)^2)
data$dist_v5_td <- sqrt((data$v5_x - data$td_x)^2 + (data$v5_y - data$td_y)^2)
data$dist_v5_te <- sqrt((data$v5_x - data$te_x)^2 + (data$v5_y - data$te_y)^2)
data$dist_v5_tf <- sqrt((data$v5_x - data$tf_x)^2 + (data$v5_y - data$tf_y)^2)
data$dist_v5_tg <- sqrt((data$v5_x - data$tg_x)^2 + (data$v5_y - data$tg_y)^2)
##### end of section #####

##### calculate distance between each vehicle and hazard #####
data$dist_v1_h1 <- sqrt((data$v1_x - data$h1_x)^2 + (data$v1_y - data$h1_y)^2)
data$dist_v1_h2 <- sqrt((data$v1_x - data$h2_x)^2 + (data$v1_y - data$h2_y)^2)
data$dist_v1_h3 <- sqrt((data$v1_x - data$h3_x)^2 + (data$v1_y - data$h3_y)^2)
data$dist_v1_h4 <- sqrt((data$v1_x - data$h4_x)^2 + (data$v1_y - data$h4_y)^2)
data$dist_v1_h5 <- sqrt((data$v1_x - data$h5_x)^2 + (data$v1_y - data$h5_y)^2)
data$dist_v1_h6 <- sqrt((data$v1_x - data$h6_x)^2 + (data$v1_y - data$h6_y)^2)
data$dist_v1_h7 <- sqrt((data$v1_x - data$h7_x)^2 + (data$v1_y - data$h7_y)^2)
data$dist_v1_h8 <- sqrt((data$v1_x - data$h8_x)^2 + (data$v1_y - data$h8_y)^2)
data$dist_v1_h9 <- sqrt((data$v1_x - data$h9_x)^2 + (data$v1_y - data$h9_y)^2)
data$dist_v1_h10 <- sqrt((data$v1_x - data$h10_x)^2 + (data$v1_y - data$h10_y)^2)

data$dist_v2_h1 <- sqrt((data$v2_x - data$h1_x)^2 + (data$v2_y - data$h1_y)^2)
data$dist_v2_h2 <- sqrt((data$v2_x - data$h2_x)^2 + (data$v2_y - data$h2_y)^2)
data$dist_v2_h3 <- sqrt((data$v2_x - data$h3_x)^2 + (data$v2_y - data$h3_y)^2)
data$dist_v2_h4 <- sqrt((data$v2_x - data$h4_x)^2 + (data$v2_y - data$h4_y)^2)
data$dist_v2_h5 <- sqrt((data$v2_x - data$h5_x)^2 + (data$v2_y - data$h5_y)^2)
data$dist_v2_h6 <- sqrt((data$v2_x - data$h6_x)^2 + (data$v2_y - data$h6_y)^2)
data$dist_v2_h7 <- sqrt((data$v2_x - data$h7_x)^2 + (data$v2_y - data$h7_y)^2)
data$dist_v2_h8 <- sqrt((data$v2_x - data$h8_x)^2 + (data$v2_y - data$h8_y)^2)
data$dist_v2_h9 <- sqrt((data$v2_x - data$h9_x)^2 + (data$v2_y - data$h9_y)^2)
data$dist_v2_h10 <- sqrt((data$v2_x - data$h10_x)^2 + (data$v2_y - data$h10_y)^2)

data$dist_v3_h1 <- sqrt((data$v3_x - data$h1_x)^2 + (data$v3_y - data$h1_y)^2)
data$dist_v3_h2 <- sqrt((data$v3_x - data$h2_x)^2 + (data$v3_y - data$h2_y)^2)
data$dist_v3_h3 <- sqrt((data$v3_x - data$h3_x)^2 + (data$v3_y - data$h3_y)^2)
data$dist_v3_h4 <- sqrt((data$v3_x - data$h4_x)^2 + (data$v3_y - data$h4_y)^2)
data$dist_v3_h5 <- sqrt((data$v3_x - data$h5_x)^2 + (data$v3_y - data$h5_y)^2)
data$dist_v3_h6 <- sqrt((data$v3_x - data$h6_x)^2 + (data$v3_y - data$h6_y)^2)
data$dist_v3_h7 <- sqrt((data$v3_x - data$h7_x)^2 + (data$v3_y - data$h7_y)^2)
data$dist_v3_h8 <- sqrt((data$v3_x - data$h8_x)^2 + (data$v3_y - data$h8_y)^2)
data$dist_v3_h9 <- sqrt((data$v3_x - data$h9_x)^2 + (data$v3_y - data$h9_y)^2)
data$dist_v3_h10 <- sqrt((data$v3_x - data$h10_x)^2 + (data$v3_y - data$h10_y)^2)

data$dist_v4_h1 <- sqrt((data$v4_x - data$h1_x)^2 + (data$v4_y - data$h1_y)^2)
data$dist_v4_h2 <- sqrt((data$v4_x - data$h2_x)^2 + (data$v4_y - data$h2_y)^2)
data$dist_v4_h3 <- sqrt((data$v4_x - data$h3_x)^2 + (data$v4_y - data$h3_y)^2)
data$dist_v4_h4 <- sqrt((data$v4_x - data$h4_x)^2 + (data$v4_y - data$h4_y)^2)
data$dist_v4_h5 <- sqrt((data$v4_x - data$h5_x)^2 + (data$v4_y - data$h5_y)^2)
data$dist_v4_h6 <- sqrt((data$v4_x - data$h6_x)^2 + (data$v4_y - data$h6_y)^2)
data$dist_v4_h7 <- sqrt((data$v4_x - data$h7_x)^2 + (data$v4_y - data$h7_y)^2)
data$dist_v4_h8 <- sqrt((data$v4_x - data$h8_x)^2 + (data$v4_y - data$h8_y)^2)
data$dist_v4_h9 <- sqrt((data$v4_x - data$h9_x)^2 + (data$v4_y - data$h9_y)^2)
data$dist_v4_h10 <- sqrt((data$v4_x - data$h10_x)^2 + (data$v4_y - data$h10_y)^2)

data$dist_v5_h1 <- sqrt((data$v5_x - data$h1_x)^2 + (data$v5_y - data$h1_y)^2)
data$dist_v5_h2 <- sqrt((data$v5_x - data$h2_x)^2 + (data$v5_y - data$h2_y)^2)
data$dist_v5_h3 <- sqrt((data$v5_x - data$h3_x)^2 + (data$v5_y - data$h3_y)^2)
data$dist_v5_h4 <- sqrt((data$v5_x - data$h4_x)^2 + (data$v5_y - data$h4_y)^2)
data$dist_v5_h5 <- sqrt((data$v5_x - data$h5_x)^2 + (data$v5_y - data$h5_y)^2)
data$dist_v5_h6 <- sqrt((data$v5_x - data$h6_x)^2 + (data$v5_y - data$h6_y)^2)
data$dist_v5_h7 <- sqrt((data$v5_x - data$h7_x)^2 + (data$v5_y - data$h7_y)^2)
data$dist_v5_h8 <- sqrt((data$v5_x - data$h8_x)^2 + (data$v5_y - data$h8_y)^2)
data$dist_v5_h9 <- sqrt((data$v5_x - data$h9_x)^2 + (data$v5_y - data$h9_y)^2)
data$dist_v5_h10 <- sqrt((data$v5_x - data$h10_x)^2 + (data$v5_y - data$h10_y)^2)
##### end of section #####

##### add new columns to represent the distance between vehicles and its target #####

dist_vehicle_target <- lapply(seq(nrow(data)), function(row_id) {
  return(data.frame(cbind(
    dist_v1_target = ifelse(data[row_id,]$v1_target == -1, 0, 
                            eval(parse(text = paste0("data[", row_id, ",]$dist_v1_t", tolower(data[row_id,]$v1_target))))),
    dist_v2_target = ifelse(data[row_id,]$v2_target == -1, 0, 
                            eval(parse(text = paste0("data[", row_id, ",]$dist_v2_t", tolower(data[row_id,]$v2_target))))),
    dist_v3_target = ifelse(data[row_id,]$v3_target == -1, 0, 
                            eval(parse(text = paste0("data[", row_id, ",]$dist_v3_t", tolower(data[row_id,]$v3_target))))),
    dist_v4_target = ifelse(data[row_id,]$v4_target == -1, 0, 
                            eval(parse(text = paste0("data[", row_id, ",]$dist_v4_t", tolower(data[row_id,]$v4_target))))),
    dist_v5_target = ifelse(data[row_id,]$v5_target == -1, 0, 
                            eval(parse(text = paste0("data[", row_id, ",]$dist_v5_t", tolower(data[row_id,]$v5_target)))))
    )))
})

dt_dist_vehicle_target <- rbindlist(dist_vehicle_target, use.names = T)
data <- cbind(data, dt_dist_vehicle_target)

##### end of section #####

data <- data %>% select(-c(v1_x, v1_y, v2_x, v2_y, v3_x, v3_y, v4_x, v4_y, v5_x, v5_y,
                           ta_x, ta_y, tb_x, tb_y, tc_x, tc_y, td_x, td_y, te_x, te_y, tf_x, tf_y, tg_x, tg_y,
                           h1_x, h1_y, h2_x, h2_y, h3_x, h3_y, h4_x, h4_y, h5_x, h5_y, 
                           h6_x, h6_y, h7_x, h7_y, h8_x, h8_y, h9_x, h9_y, h10_x, h10_y))

colnames(data)

data$min_VV_dist <- apply(data[, 18:27], 1, min)
data$max_VT_dist <- apply(data[, 28:62], 1, max)
# data$min_VT_dist <- apply(data[, 28:62], 1, min)
data$min_VH_dist <- apply(data[, 63:112], 1, min)


data <- data %>% select(-c(is_engage_payload, n_correct, n_incorrect,
                           v1_target, v2_target, v3_target, v4_target, v5_target,
                           # dist_v1_v2, dist_v1_v3, dist_v1_v4, dist_v1_v5, dist_v2_v3, dist_v2_v4, dist_v2_v5,
                           # dist_v3_v4, dist_v3_v5, dist_v4_v5
                           ))

## predict Y in next second with X in current
data$next_action <- ""
data[1:nrow(data)-1]$next_action <- data[2:nrow(data)]$action.y

data <- data[1:nrow(data)-1,] # remove last observation
data <- data %>% select(-c(action.y))
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
case_weight <- round(1/round(prop.table(table(data$next_action)), 3), 2);case_weight

data$weight <- 1

data$weight <- ifelse(data$next_action == "replanPath", yes = 7.87, no = data$weight)
data$weight <- ifelse(data$next_action == "applyAutomation", yes = 23.26, no = data$weight)
data$weight <- ifelse(data$next_action == "engagePayload", yes = 3.02, no = data$weight)
data$weight <- ifelse(data$next_action == "watch", yes = 2, no = data$weight)


# Fit rpart
rpart_model <- rpart(next_action ~ . -file_name - event_id - time - weight,
                     data = data[train_idx,], control = rpart.control(cp = 0.005), 
                     weights = data[train_idx,]$weight)

prp(rpart_model, cex=0.6)

printcp(rpart_model)
pruned_rpart_model <- prune.rpart(rpart_model, cp = 0.0043089)
prp(pruned_rpart_model, cex=0.6)


prp(rpart_model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2, cex=0.8)  

## Inference
library(Metrics)

round(accuracy(as.factor(data$action.y), 
               predicted = predict(rpart_model, data, type = "class",)), 4)
cbind(as.factor(data[1:100,]$action.y), predict(rpart_model, data[1:100,], type = "class"))

##### Ranger #####
library(ranger)

ranger_model <- ranger(next_action ~ . -file_name - event_id - time - weight,
                       data = data, case.weights = data$weight, importance = "impurity")

ranger_model$prediction.error
importance(ranger_model)[order(importance(ranger_model), decreasing = T)]

ranger_model$confusion.matrix

ranger_model$forest$split.varIDs[[1]][117]; length(ranger_model$forest$split.varIDs[[1]])
ranger_model$forest$split.values[[1]][117]; length(ranger_model$forest$split.values[[1]])


ranger_model$forest$independent.variable.names[1]



table(ranger_model$forest$split.varIDs[[1]])
table(colnames(data[, -c("file_name", "event_id", "time", "next_action")])[ranger_model$forest$split.varIDs[[1]]])


sapply(seq(1:500), function(i) max(ranger_model$forest$split.varIDs[[i]]))
# variabe ID are all the same in every trees

# variables of VV distance: 6:15 -> 5:14
# variables of VT distance: 16:50 -> 15-49
# variables of VH distance: 51:100 -> 50-99

split_value_tree_1 <- data.table(varID = ranger_model$forest$split.varIDs[[1]],
                                 value = ranger_model$forest$split.values[[1]])

# VV distance
summary(split_value_tree_1[split_value_tree_1$varID > 4 & split_value_tree_1$varID < 15,])
# VT distance 
summary(split_value_tree_1[split_value_tree_1$varID > 14 & split_value_tree_1$varID < 50,])
# VH distance
summary(split_value_tree_1[split_value_tree_1$varID > 49 & split_value_tree_1$varID < 100,])


##### randomForest #####
# library(randomForest)
# 
# rf_model <- randomForest(action.y ~ . -file_name - event_id - time - weight,
#                          data = data)

##### CTHSMM #####

uav_cthsmm <- cthsmm(rpart_tree = rpart_model, data = data, 
                    ID_Col = "file_name", stateDuration_Col = "event_id", obsColname = "next_action", aggDivisor = 3600,verbose = T)

sqlState = paste('select ', "file_name", ', ', "action.y", 
                 ', sum(', "event_id" ,') / ', 3600,
                 ' as Duration from data group by Agg_ID', sep = '');
agg_data = sqldf(sqlState)
data[,"action.y"]
