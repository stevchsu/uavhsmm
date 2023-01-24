#' ======================
#' 1. Calculate distance among vehicles, targets, and hazards
#' 2. Remove coordinate columns
#' ======================
library(dplyr)
library(data.table)
library(plyr)
library(zoo)
library(sqldf)

calculate_object_distance <- function(raw_file_path) {
  # read file
  raw_data <- fread(raw_file_path)
  
  # add nation attribute
  if (grepl(pattern = "TW", x = raw_file_path)) {
    raw_data$nation <- "tw"
  } else if (grepl(pattern = "TK", x = raw_file_path)) {
    raw_data$nation <- "tk"
  } else if (grepl(pattern = "US", x = raw_file_path)) {
    raw_data$nation <- "us"
  }
  
  # impute "watch" to the rows after which are "applyAutomation"
  for (i in seq(nrow(raw_data))) {
    if (i >= 2) {
      if (raw_data[i-1, "action"] == "applyAutomation") {
        raw_data[i, "action"] <- "end"
      }
    }
  }
  
  raw_data$action <- ifelse(raw_data$action == "", NA, raw_data$action)
  
  raw_data$is_vehicle_wait <- ifelse(raw_data$v1_status == 2 | raw_data$v2_status == 2 | 
                                       raw_data$v3_status == 2 | raw_data$v4_status == 2 | 
                                       raw_data$v5_status == 2, 1, 0)
  raw_data$is_engage_payload <- ifelse(raw_data$v1_status == 3 | raw_data$v2_status == 3 | 
                                         raw_data$v3_status == 3 | raw_data$v4_status == 3 | 
                                         raw_data$v5_status == 3, 1, 0)
  data <- raw_data %>% select(-c(v1_status, v2_status, v3_status, v4_status, v5_status))
  
  # impute action in each user's first event with "watch"
  data$action <- ifelse(data$event_id == 0, "watch", data$action)
  
  # impute NA in payload_act, collision, is_damaged
  data$payload_act <- as.factor(ifelse(is.na(data$payload_act), -1, data$payload_act))
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
  
  ## Keep data when 1). action has changed, 2). user performing payload action or 
  ## 3). the first record of each user
  reserve_id <- c(rep(FALSE, nrow(data)))
  reserve_id[nrow(data)] <- TRUE
  
  for (i in 2:nrow(data)-1) {
    # j <- i + 1;
    if (data[i,]$event_id == 0) { # condition 3
      reserve_id[i] <- TRUE;
    } else if (data[i, ]$payload_act != -1) { # condition 2
      reserve_id[i] <- TRUE;
    } else if (data[i-1,]$action.y != data[i,]$action.y | data[i,]$action.y != data[i+1, ]$action.y) { # condition 1
      reserve_id[i] <- TRUE
    }
  }
  data <- data[reserve_id, ]

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
  data$min_VV_dist <- apply(data[, which(colnames(data) == "dist_v1_v2"): (which(colnames(data) == "dist_v1_v2") + 9)], 1, min)
  data$min_VT_dist <- apply(data[, which(colnames(data) == "dist_v1_ta"): (which(colnames(data) == "dist_v1_ta") + 34)], 1, min)
  data$max_VT_dist <- apply(data[, which(colnames(data) == "dist_v1_ta"): (which(colnames(data) == "dist_v1_ta") + 34)], 1, max)
  data$min_VH_dist <- apply(data[, which(colnames(data) == "dist_v1_h1"): (which(colnames(data) == "dist_v1_h1") + 49)], 1, min)
  
  return(data)
}


## test case

# example <- calculate_object_distance('data/data_20221107/TK/TK_HRB_HW.csv')

# ##### Preprocessing #####
# # load file
# 
# tw_raw_data <- fread("data/data_20221107/TW/TW_HRB_HW.csv")
# tk_raw_data <- fread("data/data_20221107/TK/TK_HRB_HW.csv")
# us_raw_data <- fread("data/data_20221107/US/US_HRB_HW.csv")
# 
# tw_raw_data$nation <- "tw"
# tk_raw_data$nation <- "tk"
# us_raw_data$nation <- "us"
# 
# raw_data <- rbind(tw_raw_data, tk_raw_data, us_raw_data) %>% select(-V1)
# rm(list = c("tk_raw_data", "tw_raw_data", "us_raw_data"))
# 
# # impute "watch" to the rows after which are "applyAutomation"
# for (i in seq(nrow(raw_data))) {
#   # print(paste0("i = ", as.character(i)))
#   # print(data[i, "action"])
#   if (i >= 2) {
#     if (raw_data[i-1, "action"] == "applyAutomation") {
#       # print(data[i-1])
#       raw_data[i, "action"] <- "end"
#     }
#   }
# }
# 
# # replace empty action with NA
# raw_data$action <- ifelse(raw_data$action == "", NA, raw_data$action)
# 
# # summarize and dummy encode vehicle status into two variable, is_vehicle_wait and is_engage_payload
# raw_data$is_vehicle_wait <- ifelse(raw_data$v1_status == 2 | raw_data$v2_status == 2 | 
#                                      raw_data$v3_status == 2 | raw_data$v4_status == 2 | 
#                                      raw_data$v5_status == 2, 1, 0)
# raw_data$is_engage_payload <- ifelse(raw_data$v1_status == 3 | raw_data$v2_status == 3 | 
#                                        raw_data$v3_status == 3 | raw_data$v4_status == 3 | 
#                                        raw_data$v5_status == 3, 1, 0)
# data <- raw_data %>% select(-c(v1_status, v2_status, v3_status, v4_status, v5_status))
# 
# # impute action in each user's first event with "watch"
# data$action <- ifelse(data$event_id == 0, "watch", data$action)
# 
# # impute NA in payload_act, collision, is_damaged
# data$payload_act <- as.factor(ifelse(is.na(data$payload_act), -1, data$payload_act))
# data$collision <- ifelse(is.na(data$collision), 0, data$collision)
# data$is_damaged <- ifelse(is.na(data$is_damaged), 0, data$is_damaged)
# 
# # impute NA in action with LOCF
# impute_action <- ddply(data[, c("file_name", "event_id", "time", "action")], 
#                        ~file_name, na.locf)
# data <- inner_join(data, impute_action, by = c("file_name", "time", "event_id"))
# 
# data <- data %>% relocate(c(file_name, event_id, time, action.x, action.y), 
#                           setdiff(colnames(data), c("file_name", "event_id", "time", "action.x", "action.y")))
# 
# # replace level "end" with "watch" in action
# data$action.y <- ifelse(data$action.y == "end", "watch", data$action.y)
# data <- data %>% select(-action.x)
# data$action.y <- as.factor(data$action.y)
# 
# ## Keep data when 1). action has changed, 2). user performing payload action or 
# ## 3). the first record of each user
# reserve_id <- c(rep(FALSE, nrow(data)))
# reserve_id[nrow(data)] <- TRUE
# 
# for (i in 2:nrow(data)-1) {
#   # j <- i + 1;
#   if (data[i,]$event_id == 0) { # condition 3
#     reserve_id[i] <- TRUE;
#   } else if (data[i, ]$payload_act != -1) { # condition 2
#     reserve_id[i] <- TRUE;
#   } else if (data[i-1,]$action.y != data[i,]$action.y | data[i,]$action.y != data[i+1, ]$action.y) {
#     reserve_id[i] <- TRUE
#   }
# }
# 
# sum(reserve_id) # 11762
# table(data$action.y)
# table(data[reserve_id, ]$action.y)
# # sample size from 26955 to 6949
# data <- data[reserve_id, ]
# 
# table(data$nation)
# 
# detach("package:plyr", unload=TRUE)
# nation_user_count <- data %>% group_by(nation, file_name) %>% summarise(n = n())
# 
# ggplot(nation_user_count, aes(x = n, color = nation)) + geom_density() + theme(text = element_text(size = 18))
# 
# ##### end of section #####
# 
# ##### calculate distance between vehicles #####
# data$dist_v1_v2 <- sqrt((data$v1_x - data$v2_x)^2 + (data$v1_y - data$v2_y)^2)
# data$dist_v1_v3 <- sqrt((data$v1_x - data$v3_x)^2 + (data$v1_y - data$v3_y)^2)
# data$dist_v1_v4 <- sqrt((data$v1_x - data$v4_x)^2 + (data$v1_y - data$v4_y)^2)
# data$dist_v1_v5 <- sqrt((data$v1_x - data$v5_x)^2 + (data$v1_y - data$v5_y)^2)
# 
# data$dist_v2_v3 <- sqrt((data$v2_x - data$v3_x)^2 + (data$v2_y - data$v3_y)^2)
# data$dist_v2_v4 <- sqrt((data$v2_x - data$v4_x)^2 + (data$v2_y - data$v4_y)^2)
# data$dist_v2_v5 <- sqrt((data$v2_x - data$v5_x)^2 + (data$v2_y - data$v5_y)^2)
# 
# data$dist_v3_v4 <- sqrt((data$v3_x - data$v4_x)^2 + (data$v3_y - data$v4_y)^2)
# data$dist_v3_v5 <- sqrt((data$v3_x - data$v5_x)^2 + (data$v3_y - data$v5_y)^2)
# data$dist_v4_v5 <- sqrt((data$v4_x - data$v5_x)^2 + (data$v4_y - data$v5_y)^2)
# ##### end of section #####
# 
# ##### calculate distance between each vehicle and target #####
# data$dist_v1_ta <- sqrt((data$v1_x - data$ta_x)^2 + (data$v1_y - data$ta_y)^2)
# data$dist_v1_tb <- sqrt((data$v1_x - data$tb_x)^2 + (data$v1_y - data$tb_y)^2)
# data$dist_v1_tc <- sqrt((data$v1_x - data$tc_x)^2 + (data$v1_y - data$tc_y)^2)
# data$dist_v1_td <- sqrt((data$v1_x - data$td_x)^2 + (data$v1_y - data$td_y)^2)
# data$dist_v1_te <- sqrt((data$v1_x - data$te_x)^2 + (data$v1_y - data$te_y)^2)
# data$dist_v1_tf <- sqrt((data$v1_x - data$tf_x)^2 + (data$v1_y - data$tf_y)^2)
# data$dist_v1_tg <- sqrt((data$v1_x - data$tg_x)^2 + (data$v1_y - data$tg_y)^2)
# 
# data$dist_v2_ta <- sqrt((data$v2_x - data$ta_x)^2 + (data$v2_y - data$ta_y)^2)
# data$dist_v2_tb <- sqrt((data$v2_x - data$tb_x)^2 + (data$v2_y - data$tb_y)^2)
# data$dist_v2_tc <- sqrt((data$v2_x - data$tc_x)^2 + (data$v2_y - data$tc_y)^2)
# data$dist_v2_td <- sqrt((data$v2_x - data$td_x)^2 + (data$v2_y - data$td_y)^2)
# data$dist_v2_te <- sqrt((data$v2_x - data$te_x)^2 + (data$v2_y - data$te_y)^2)
# data$dist_v2_tf <- sqrt((data$v2_x - data$tf_x)^2 + (data$v2_y - data$tf_y)^2)
# data$dist_v2_tg <- sqrt((data$v2_x - data$tg_x)^2 + (data$v2_y - data$tg_y)^2)
# 
# data$dist_v3_ta <- sqrt((data$v3_x - data$ta_x)^2 + (data$v3_y - data$ta_y)^2)
# data$dist_v3_tb <- sqrt((data$v3_x - data$tb_x)^2 + (data$v3_y - data$tb_y)^2)
# data$dist_v3_tc <- sqrt((data$v3_x - data$tc_x)^2 + (data$v3_y - data$tc_y)^2)
# data$dist_v3_td <- sqrt((data$v3_x - data$td_x)^2 + (data$v3_y - data$td_y)^2)
# data$dist_v3_te <- sqrt((data$v3_x - data$te_x)^2 + (data$v3_y - data$te_y)^2)
# data$dist_v3_tf <- sqrt((data$v3_x - data$tf_x)^2 + (data$v3_y - data$tf_y)^2)
# data$dist_v3_tg <- sqrt((data$v3_x - data$tg_x)^2 + (data$v3_y - data$tg_y)^2)
# 
# data$dist_v4_ta <- sqrt((data$v4_x - data$ta_x)^2 + (data$v4_y - data$ta_y)^2)
# data$dist_v4_tb <- sqrt((data$v4_x - data$tb_x)^2 + (data$v4_y - data$tb_y)^2)
# data$dist_v4_tc <- sqrt((data$v4_x - data$tc_x)^2 + (data$v4_y - data$tc_y)^2)
# data$dist_v4_td <- sqrt((data$v4_x - data$td_x)^2 + (data$v4_y - data$td_y)^2)
# data$dist_v4_te <- sqrt((data$v4_x - data$te_x)^2 + (data$v4_y - data$te_y)^2)
# data$dist_v4_tf <- sqrt((data$v4_x - data$tf_x)^2 + (data$v4_y - data$tf_y)^2)
# data$dist_v4_tg <- sqrt((data$v4_x - data$tg_x)^2 + (data$v4_y - data$tg_y)^2)
# 
# data$dist_v5_ta <- sqrt((data$v5_x - data$ta_x)^2 + (data$v5_y - data$ta_y)^2)
# data$dist_v5_tb <- sqrt((data$v5_x - data$tb_x)^2 + (data$v5_y - data$tb_y)^2)
# data$dist_v5_tc <- sqrt((data$v5_x - data$tc_x)^2 + (data$v5_y - data$tc_y)^2)
# data$dist_v5_td <- sqrt((data$v5_x - data$td_x)^2 + (data$v5_y - data$td_y)^2)
# data$dist_v5_te <- sqrt((data$v5_x - data$te_x)^2 + (data$v5_y - data$te_y)^2)
# data$dist_v5_tf <- sqrt((data$v5_x - data$tf_x)^2 + (data$v5_y - data$tf_y)^2)
# data$dist_v5_tg <- sqrt((data$v5_x - data$tg_x)^2 + (data$v5_y - data$tg_y)^2)
# ##### end of section #####
# 
# ##### calculate distance between each vehicle and hazard #####
# data$dist_v1_h1 <- sqrt((data$v1_x - data$h1_x)^2 + (data$v1_y - data$h1_y)^2)
# data$dist_v1_h2 <- sqrt((data$v1_x - data$h2_x)^2 + (data$v1_y - data$h2_y)^2)
# data$dist_v1_h3 <- sqrt((data$v1_x - data$h3_x)^2 + (data$v1_y - data$h3_y)^2)
# data$dist_v1_h4 <- sqrt((data$v1_x - data$h4_x)^2 + (data$v1_y - data$h4_y)^2)
# data$dist_v1_h5 <- sqrt((data$v1_x - data$h5_x)^2 + (data$v1_y - data$h5_y)^2)
# data$dist_v1_h6 <- sqrt((data$v1_x - data$h6_x)^2 + (data$v1_y - data$h6_y)^2)
# data$dist_v1_h7 <- sqrt((data$v1_x - data$h7_x)^2 + (data$v1_y - data$h7_y)^2)
# data$dist_v1_h8 <- sqrt((data$v1_x - data$h8_x)^2 + (data$v1_y - data$h8_y)^2)
# data$dist_v1_h9 <- sqrt((data$v1_x - data$h9_x)^2 + (data$v1_y - data$h9_y)^2)
# data$dist_v1_h10 <- sqrt((data$v1_x - data$h10_x)^2 + (data$v1_y - data$h10_y)^2)
# 
# data$dist_v2_h1 <- sqrt((data$v2_x - data$h1_x)^2 + (data$v2_y - data$h1_y)^2)
# data$dist_v2_h2 <- sqrt((data$v2_x - data$h2_x)^2 + (data$v2_y - data$h2_y)^2)
# data$dist_v2_h3 <- sqrt((data$v2_x - data$h3_x)^2 + (data$v2_y - data$h3_y)^2)
# data$dist_v2_h4 <- sqrt((data$v2_x - data$h4_x)^2 + (data$v2_y - data$h4_y)^2)
# data$dist_v2_h5 <- sqrt((data$v2_x - data$h5_x)^2 + (data$v2_y - data$h5_y)^2)
# data$dist_v2_h6 <- sqrt((data$v2_x - data$h6_x)^2 + (data$v2_y - data$h6_y)^2)
# data$dist_v2_h7 <- sqrt((data$v2_x - data$h7_x)^2 + (data$v2_y - data$h7_y)^2)
# data$dist_v2_h8 <- sqrt((data$v2_x - data$h8_x)^2 + (data$v2_y - data$h8_y)^2)
# data$dist_v2_h9 <- sqrt((data$v2_x - data$h9_x)^2 + (data$v2_y - data$h9_y)^2)
# data$dist_v2_h10 <- sqrt((data$v2_x - data$h10_x)^2 + (data$v2_y - data$h10_y)^2)
# 
# data$dist_v3_h1 <- sqrt((data$v3_x - data$h1_x)^2 + (data$v3_y - data$h1_y)^2)
# data$dist_v3_h2 <- sqrt((data$v3_x - data$h2_x)^2 + (data$v3_y - data$h2_y)^2)
# data$dist_v3_h3 <- sqrt((data$v3_x - data$h3_x)^2 + (data$v3_y - data$h3_y)^2)
# data$dist_v3_h4 <- sqrt((data$v3_x - data$h4_x)^2 + (data$v3_y - data$h4_y)^2)
# data$dist_v3_h5 <- sqrt((data$v3_x - data$h5_x)^2 + (data$v3_y - data$h5_y)^2)
# data$dist_v3_h6 <- sqrt((data$v3_x - data$h6_x)^2 + (data$v3_y - data$h6_y)^2)
# data$dist_v3_h7 <- sqrt((data$v3_x - data$h7_x)^2 + (data$v3_y - data$h7_y)^2)
# data$dist_v3_h8 <- sqrt((data$v3_x - data$h8_x)^2 + (data$v3_y - data$h8_y)^2)
# data$dist_v3_h9 <- sqrt((data$v3_x - data$h9_x)^2 + (data$v3_y - data$h9_y)^2)
# data$dist_v3_h10 <- sqrt((data$v3_x - data$h10_x)^2 + (data$v3_y - data$h10_y)^2)
# 
# data$dist_v4_h1 <- sqrt((data$v4_x - data$h1_x)^2 + (data$v4_y - data$h1_y)^2)
# data$dist_v4_h2 <- sqrt((data$v4_x - data$h2_x)^2 + (data$v4_y - data$h2_y)^2)
# data$dist_v4_h3 <- sqrt((data$v4_x - data$h3_x)^2 + (data$v4_y - data$h3_y)^2)
# data$dist_v4_h4 <- sqrt((data$v4_x - data$h4_x)^2 + (data$v4_y - data$h4_y)^2)
# data$dist_v4_h5 <- sqrt((data$v4_x - data$h5_x)^2 + (data$v4_y - data$h5_y)^2)
# data$dist_v4_h6 <- sqrt((data$v4_x - data$h6_x)^2 + (data$v4_y - data$h6_y)^2)
# data$dist_v4_h7 <- sqrt((data$v4_x - data$h7_x)^2 + (data$v4_y - data$h7_y)^2)
# data$dist_v4_h8 <- sqrt((data$v4_x - data$h8_x)^2 + (data$v4_y - data$h8_y)^2)
# data$dist_v4_h9 <- sqrt((data$v4_x - data$h9_x)^2 + (data$v4_y - data$h9_y)^2)
# data$dist_v4_h10 <- sqrt((data$v4_x - data$h10_x)^2 + (data$v4_y - data$h10_y)^2)
# 
# data$dist_v5_h1 <- sqrt((data$v5_x - data$h1_x)^2 + (data$v5_y - data$h1_y)^2)
# data$dist_v5_h2 <- sqrt((data$v5_x - data$h2_x)^2 + (data$v5_y - data$h2_y)^2)
# data$dist_v5_h3 <- sqrt((data$v5_x - data$h3_x)^2 + (data$v5_y - data$h3_y)^2)
# data$dist_v5_h4 <- sqrt((data$v5_x - data$h4_x)^2 + (data$v5_y - data$h4_y)^2)
# data$dist_v5_h5 <- sqrt((data$v5_x - data$h5_x)^2 + (data$v5_y - data$h5_y)^2)
# data$dist_v5_h6 <- sqrt((data$v5_x - data$h6_x)^2 + (data$v5_y - data$h6_y)^2)
# data$dist_v5_h7 <- sqrt((data$v5_x - data$h7_x)^2 + (data$v5_y - data$h7_y)^2)
# data$dist_v5_h8 <- sqrt((data$v5_x - data$h8_x)^2 + (data$v5_y - data$h8_y)^2)
# data$dist_v5_h9 <- sqrt((data$v5_x - data$h9_x)^2 + (data$v5_y - data$h9_y)^2)
# data$dist_v5_h10 <- sqrt((data$v5_x - data$h10_x)^2 + (data$v5_y - data$h10_y)^2)
# ##### end of section #####
# 
# ##### add new columns to represent the distance between vehicles and its target #####
# 
# dist_vehicle_target <- lapply(seq(nrow(data)), function(row_id) {
#   return(data.frame(cbind(
#     dist_v1_target = ifelse(data[row_id,]$v1_target == -1, 0, 
#                             eval(parse(text = paste0("data[", row_id, ",]$dist_v1_t", tolower(data[row_id,]$v1_target))))),
#     dist_v2_target = ifelse(data[row_id,]$v2_target == -1, 0, 
#                             eval(parse(text = paste0("data[", row_id, ",]$dist_v2_t", tolower(data[row_id,]$v2_target))))),
#     dist_v3_target = ifelse(data[row_id,]$v3_target == -1, 0, 
#                             eval(parse(text = paste0("data[", row_id, ",]$dist_v3_t", tolower(data[row_id,]$v3_target))))),
#     dist_v4_target = ifelse(data[row_id,]$v4_target == -1, 0, 
#                             eval(parse(text = paste0("data[", row_id, ",]$dist_v4_t", tolower(data[row_id,]$v4_target))))),
#     dist_v5_target = ifelse(data[row_id,]$v5_target == -1, 0, 
#                             eval(parse(text = paste0("data[", row_id, ",]$dist_v5_t", tolower(data[row_id,]$v5_target)))))
#   )))
# })
# 
# dt_dist_vehicle_target <- rbindlist(dist_vehicle_target, use.names = T)
# data <- cbind(data, dt_dist_vehicle_target)
# 
# ##### end of section #####
# 
# data <- data %>% select(-c(v1_x, v1_y, v2_x, v2_y, v3_x, v3_y, v4_x, v4_y, v5_x, v5_y,
#                            ta_x, ta_y, tb_x, tb_y, tc_x, tc_y, td_x, td_y, te_x, te_y, tf_x, tf_y, tg_x, tg_y,
#                            h1_x, h1_y, h2_x, h2_y, h3_x, h3_y, h4_x, h4_y, h5_x, h5_y, 
#                            h6_x, h6_y, h7_x, h7_y, h8_x, h8_y, h9_x, h9_y, h10_x, h10_y))
# 
# colnames(data)
# 
# colnames(data)[which(colnames(data) == "dist_v1_v2") + 9]
# data$min_VV_dist <- apply(data[, which(colnames(data) == "dist_v1_v2"): (which(colnames(data) == "dist_v1_v2") + 9)], 1, min)
# data$min_VT_dist <- apply(data[, which(colnames(data) == "dist_v1_ta"): (which(colnames(data) == "dist_v1_ta") + 34)], 1, min)
# data$max_VT_dist <- apply(data[, which(colnames(data) == "dist_v1_ta"): (which(colnames(data) == "dist_v1_ta") + 34)], 1, max)
# data$min_VH_dist <- apply(data[, which(colnames(data) == "dist_v1_h1"): (which(colnames(data) == "dist_v1_h1") + 49)], 1, min)

