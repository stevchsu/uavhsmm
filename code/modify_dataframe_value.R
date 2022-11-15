library(dplyr)

uav_df <- as.data.frame(read.csv("data/TW/TW_LRB_LW.csv"))

uav_df$time <- as.numeric(strptime(uav_df$time, format = "%Y-%m-%d %H:%M:%S.000"))
uav_df$payload_act <- as.numeric(uav_df$payload_act)
uav_df$n_correct <- as.numeric(uav_df$n_correct)
uav_df$n_incorrect <- as.numeric(uav_df$n_incorrect)

# uav_df$payload_act[is.na(uav_df$payload_act)] <- -1

for (i in seq(nrow(uav_df))) {
  if ((uav_df[i, "payload_act"] == 1 || uav_df[i, "payload_act"] == 2) & !is.na(uav_df[i, "payload_act"])) {
    uav_df[i, "action"] <- "payload"
  }
}

##### 0606 adjustment #####
# uav_df$payload_act[is.na(uav_df$payload_act)] <- -1
# uav_df$payload_act <- as.factor(uav_df$payload_act)


##### end of 0606 adjustment #####

## filter out payload actions
# payload_df <- uav_df %>% filter(action == "payload")

payload_df <- uav_df %>% filter(!is.na(payload_act))

## impute NAs with 3 in payload_act to indicate another behavior (optional)
# payload_df$payload_act[is.na(payload_df$payload_act)] <- -1
# payload_df$payload_act <- as.factor(payload_df$payload_act)

# for (i in seq(nrow(payload_df))) {
#   if (i > 1 && payload_df[i, "payload_act"] == "-1") {
#     if (payload_df[i-1, "payload_act"] != "1" && payload_df[i-1, "payload_act"] != "2"){
#       payload_df[i, "payload_act"] <- payload_df[i-1, "payload_act"]
#     }
#   }
# }
# 
# payload_df <- payload_df %>% filter(payload_act != "-1")

