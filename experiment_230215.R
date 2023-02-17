#' apply LRB_LW data on CTHSMM
#' 
#' 2023/02/15

library(dplyr)
library(data.table)
library(rpart)
library(rpart.plot)
# library(ykang)
library(sqldf)
library(ggplot2)

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

data$payload_act <- as.factor(as.character(data$payload_act))
data$VHcollision <- ifelse(is.na(data$VHcollision) , 0, data$VHcollision)
data <- na.omit(data)

## predict Y in next second with X in current
data$next_action <- ""
data[1:nrow(data)-1]$next_action <- data[2:nrow(data)]$action.y

data <- data[1:nrow(data)-1,] # remove last observation
data$next_action <- as.factor(data$next_action)
##### end of loading and preprocessing #####

##### train rpart #####
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
rpart_model$cptable[5] # best cp = 0.005718909

rpart_model_pruned <- prune(rpart_model, cp = 0.005718909)

print(rpart_model_pruned)

prp(rpart_model)
prp(rpart_model_pruned, cex=1.2)

rpart.rules(rpart_model_pruned)

## Inference
library(Metrics)

round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(rpart_model, data[train_idx,], type = "class",)), 4) # 0.6013
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model, data[-train_idx,], type = "class",)), 4) # 0.5382

round(accuracy(data[train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[train_idx,], type = "class",)), 4) # 0.4664
round(accuracy(data[-train_idx,]$next_action, 
               predicted = predict(rpart_model_pruned, data[-train_idx,], type = "class",)), 4) # 0.4678

table(true = data[train_idx,]$next_action, 
      predicted = predict(rpart_model_pruned, data[train_idx,], type = "class",))
table(true = data[-train_idx,]$next_action, 
      predicted = predict(rpart_model_pruned, data[-train_idx,], type = "class",))
##### end of rpart #####

##### ranger #####
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
##### end of ranger #####

##### custom function #####
c_cthsmm = function(rpart_tree, data, stateDuration_Col, obsColname, aggDivisor, ID_Col = NULL, verbose = F){
  
  # Check if two sequence are character vector
  stopifnot(is.data.frame(data) );
  
  # print(paste0("ID_Col: ", ID_Col))
  
  # Initializing...
  if(verbose) print("Initializing...");
  cthsmm = list();
  
  # Number of Observations
  cthsmm$obs = attr(x=rpart_tree,'ylevels');
  
  cthsmm$numOfObs = length(cthsmm$obs);
  print(paste0("cthsmm$numOfObs = ", cthsmm$numOfObs))
  
  # Number of columns for the node information. Used to get the class probabilies in each node
  nodeInfoNumofCol = ncol(rpart_tree$frame$yval2);
  nodeInfo = subset(cbind(rpart_tree$frame[1], rpart_tree$frame$yval2[, (nodeInfoNumofCol - cthsmm$numOfObs):(nodeInfoNumofCol - 1) ]), var == '<leaf>');
  nodeInfo = nodeInfo[order(nodeInfo$var, as.integer(rownames(nodeInfo))),-1];
  colnames(nodeInfo) = cthsmm$obs;
  
  # n * 2 state to node number mapping data
  nodes = sort( as.numeric(rownames(subset(x=rpart_tree$frame[1], var == '<leaf>' ) )), decreasing=F);
  
  states = paste('s', 1:length(nodes), sep = '');
  
  # Number of states
  cthsmm$numOfStates = length(nodes);  
  
  # State rules extracted from classification tree
  if(verbose) print("State rule extration...") ;
  
  sr = matrix('',ncol = 1, nrow = cthsmm$numOfStates);
  for(i in 1:cthsmm$numOfStates){
    if(verbose) print(paste("State", i, "...") );
    
    # Get rules for each state. [-1] is used to remove "root" rules
    # Identify rules for those categorical predictors (e.g. gender = 'M')
    ruleset = unlist(path.rpart(rpart_tree,nodes[i], print.it=F))[-1];
    # Those cells with "=" are replaced with "==" and quotes with a categorical value
    ruleset_cat_idx = setdiff( 1:length(ruleset),  grep("(>=)|(>)|(<=)|(<)", ruleset))
    ruleset[ruleset_cat_idx] = sapply(ruleset[ruleset_cat_idx], 
                                      function(x) paste(paste(unlist(strsplit(x, "=")),collapse = '== "' ), '"', sep = ""));
    
    sr[i] = paste(ruleset, collapse = ' & ' , sep = '');   
  }
  if(verbose) print("Done.") ;
  
  if(verbose) print(sr);
  # State to Node mapping informatino with rules
  cthsmm$state2node = cbind(data.frame(list( nodeNum = nodes, stateName = states, stateRule = sr )), nodeInfo) ;  
  
  # Observation Matrix
  cthsmm$om = as.matrix(cthsmm$state2node[ cthsmm$obs ]);
  rownames(cthsmm$om) = cthsmm$state2node$stateName;
  
  # Number of states/rows in the training data
  lenOfStateSeq = nrow(data);
  cthsmm$stateSeq = integer(lenOfStateSeq);
  
  # Identify each row as state, then create the sequence of states.
  if(verbose) print("Convert each row into state number..." ) ;
  attach(data);
  for(i in 1:cthsmm$numOfStates){
    if(verbose) print(paste("State", i, "...") );
    # Assign rules and identify states.
    stateAssignExp = parse(text = paste("cthsmm$stateSeq[which(", sr[i], ")] = ", i , " " ,sep = '') ) ;
    #print(stateAssignExp);
    eval(stateAssignExp);    
  }  
  detach(data);
  if(verbose) print("Done." ) ;
  # Aggregate data
  data$actState = cthsmm$stateSeq;
  
  if(verbose) print("Aggregating data by ID, State, and Observation...") ;
  
  # Identical "ID", "State","Observation" with previous row/observation
  # data[2:nrow(data),'IdentPrevID'] = data[1:(nrow(data) - 1),ID_Col] == data[2:nrow(data),ID_Col];
  data[2:nrow(data),'IdentPrevID'] = data[[ID_Col]][1:(nrow(data) - 1)] == data[[ID_Col]][2:nrow(data)];
  data[2:nrow(data),'IdentPrevState'] = data[1:(nrow(data) - 1), 'actState'] == data[2:nrow(data), 'actState'];
  data[2:nrow(data),'IdentPrevObs'] = data[[obsColname]][1:(nrow(data) - 1)] == data[[obsColname]][2:nrow(data)];
  data[1,'IdentPrevID'] = T; data[1,'IdentPrevState'] = T; data[1,'IdentPrevObs'] = T; 
  data[,'Agg_ID'] = NA;
  
  # Assign a group ID to get sum of each ID-state-observation
  if(verbose) print("Assigning a group ID to get sum of each ID-state-observation ...") ;
  
  # To find duplicated ID + State + Observation value, encode, and specify new aggregation ID
  
  # dup_data = data[, c('IdentPrevID', 'IdentPrevState','IdentPrevObs')];
  # dup_data$dup = ifelse( dup_data$IdentPrevID & dup_data$IdentPrevState & dup_data$IdentPrevObs, F, T)
  # dup_data[1,"dup"] = T
  # 
  # repNewAggIDCount = rle(dup_data$dup)$length[c(F, T)] + 1;
  # print(rle(dup_data$dup))
  # numOfNewAggID = length(repNewAggIDCount);
  # dup_data$Agg_ID = rep( 1:numOfNewAggID, times = repNewAggIDCount);
  # cthsmm$dup_data = dup_data;
  # 
  # data$Agg_ID = cthsmm$dup_data$Agg_ID;
  data$dup = data$IdentPrevID & data$IdentPrevState & data$IdentPrevObs;
  
  # print(data$dup)
  
  tmpCt = integer(1); # Iterator
  for(i in 1:nrow(data)){
    if(data[i,"dup"] == TRUE){
      data[i,'Agg_ID'] = tmpCt;
    }else{
      tmpCt = tmpCt + 1;
      data[i,'Agg_ID'] = tmpCt;
      print(paste0("tmpCt = ", tmpCt))
      print(data[i,'Agg_ID'])
    }
  }
  
  print(data[, 'Agg_ID'])
  # tmpCt = 1; # Iterator
  # for(i in 1:nrow(data)){
  #   if(data[i,'IdentPrevID'] && data[i,'IdentPrevState'] && data[i,'IdentPrevObs']){
  #     data[i,'Agg_ID'] = tmpCt;
  #   }else{
  #     tmpCt = tmpCt + 1;
  #     data[i,'Agg_ID'] = tmpCt;
  #   }
  # }
  
  sqlState = paste('select ', ID_Col, ', actState as State,', obsColname, 
                   ', sum(', stateDuration_Col ,') / ', aggDivisor,
                   ' as Duration from data group by Agg_ID', sep = '');
  
  
  
  #################  
  #print(paste( "data[, list(", ID_Col, ", actState = State, ", obsColname, 
  #                                       ", Duration = sum(", stateDuration_Col, ") / ", aggDivisor, "),",
  #                                       "list(Agg_ID)]"  )  ) 
  #print(paste("aggregate(Duration ~ Agg_ID + ", ID_Col ,"  + ", obsColname, " + actState, data = data ", 
  #            ", function(x) sum(x) / ", aggDivisor, ")" ))
  
  #cthsmm$agg_data_ag = eval(parse(text = paste("aggregate(Duration ~ Agg_ID + ", ID_Col ,"  + ", obsColname, " + actState, data = data ", 
  #            ", function(x) sum(x) / ", aggDivisor, ")" ) ))
  
  #cthsmm$agg_data_dt = eval(parse(paste( "data[, list(", ID_Col, ", actState = State, ", obsColname, 
  
  #cthsmm$agg_data_dt = eval(parse(paste( "data[, list(", ID_Col, ", actState = State, ", obsColname, 
  #                                       ", Duration = sum(", stateDuration_Col, ") / ", aggDivisor, "),",
  #                                       "list(Agg_ID)]"  ) ))
  
  #print(sqlState)
  #   sqlState = paste('select ', ID_Col, ', actState as State,', obsColname, 
  #                    ', sum(', stateDuration_Col ,') / ', aggDivisor,
  #                    ' as Duration from data group by ', ID_Col, ', State, ', obsColname, sep = '');
  #print(sqlState);  
  
  ###################
  print(sqlState)
  cthsmm$agg_data = sqldf(sqlState)
  
  print(head(cthsmm$agg_data))
  print(tail(cthsmm$agg_data))
  #cthsmm$agg_data$Duration = ceiling(cthsmm$agqg_data$Duration);
  
  if(verbose) print("Computing State Transition Matrix ( createSTM() ) ...") ;
  # Create state transition matrix
  print(is.null(ID_Col))
  
  if(is.null(ID_Col)){
    stm = ykang::createSTM(seqOfSyms=cthsmm$agg_data$State,isSingleString=FALSE, 
                           uniqueSymbolSet=1:cthsmm$numOfStates, noAbsorbingStateProb=T ,
                           debugMode = T);
  }else{
    print(paste0("cthsmm$agg_data[[ID_Col]] = ", cthsmm$agg_data[[ID_Col]]))
    stm = createSTM(seqOfSyms=cthsmm$agg_data$State,isSingleString=FALSE, symbolIDList=cthsmm$agg_data[,ID_Col],
                           uniqueSymbolSet=1:cthsmm$numOfStates, noAbsorbingStateProb=T );    
  }
  
  dimnames(stm) = list(states, states);
  cthsmm$stm = stm;
  
  ## Create equally-probable initial matrix/Frequence Vector
  cthsmm$im = rep(1/cthsmm$numOfStates, cthsmm$numOfStates);
  #cthsmm$im = ykang::FV(seq = cthsmm$stateSeq, symSet = 1:cthsmm$numOfStates);
  
  # The state probability vector
  cthsmm$stProbVec = prop.table(table(cthsmm$agg_data$State));  
  
  # The State-Observation join probability matrix from emission/observation matrix
  
  cthsmm$stObsjointPM = cthsmm$om * 
    matrix(rep(cthsmm$im, cthsmm$numOfObs),ncol=cthsmm$numOfObs);
  #   cthsmm$stObsjointPM = cthsmm$om * 
  #     matrix(rep(cthsmm$stProbVec, cthsmm$numOfObs),ncol=cthsmm$numOfObs);
  
  # Mutual Information of State-Observation 
  cthsmm$MI_o = discreteMI( cthsmm$stObsjointPM, base = 2);
  
  # Probability density function for each state duration
  
  nonParamDists = NULL; # Non-parametric state duration distributions for mhsmm::hsmmspec()
  
  for(i in 1:cthsmm$numOfStates){
    # KDE with linear interpolation to estimate duration PDF of each state
    if(verbose) print(paste("Estimate PDF for duration of state", i, "..."));
    #print(paste("class = ", 
    #            class(base::subset(cthsmm$agg_data$Duration, subset=cthsmm$agg_data$State == i ) )) )
    #print(head(cthsmm$agg_data))  
    #inner_cthsmm <<- cthsmm;
    if(! is.null(class(base::subset(cthsmm$agg_data$Duration, subset=cthsmm$agg_data$State == i ))))
    {
      # Set to min-max Duration
      cthsmm[paste('statePDF_',i,sep='')] =
        list( ykang::kde2pdf(x=base::subset(cthsmm$agg_data$Duration,
                                            subset=cthsmm$agg_data$State == i ),
                             KDE_kernel="gaussian", spline_method=NULL,
                             from_x = 1, to_x = base::max(cthsmm$agg_data$Duration)));
      # cthsmm[paste('statePDF_',i,sep='')] = 
      #   list( ykang::kde2pdf(x=base::subset(cthsmm$agg_data$Duration,
      #                                       subset=cthsmm$agg_data$State == i ), 
      #                        KDE_kernel="gaussian", spline_method=NULL));
      
      
      tmpPDF = cthsmm[[paste('statePDF_',i,sep='')]];
      
      nonParamDists = cbind(nonParamDists, tmpPDF(1:max(cthsmm$agg_data$Duration) ) );
      
    }
  }
  #print(nonParamDists);  
  # Fit HSMM using mhsmm::hsmmspec()
  cthsmm$hsmm = hsmmspec(init = cthsmm$im, transition = cthsmm$stm,
                         parms.emission = list(om=cthsmm$om), 
                         dens.emission = dspec.hsmm, rand.emission=rspec.hsmm,
                         sojourn = list(d = nonParamDists, type = "nonparametric"));
  
  return( cthsmm );
  
}#end of function

createSTM = function(seqOfSyms, isSingleString = FALSE, uniqueSymbolSet = NA, 
                     noAbsorbingStateProb = FALSE, symbolIDList= NA, debugMode = FALSE){  
  
  print(paste0("in createSTM, seqOfSyms = ", seqOfSyms))
  # If seqOfSyms is a long string but isSingleString not clearly specified
  if( length(seqOfSyms) == 1 ) stopifnot(isSingleString)
  
  print(paste0("in createSTM, isSingleString = ", isSingleString))
  
  # if( isSingleString ) {
  #   seqOfSyms = unlist(strsplit(seqOfSyms,split="")); # split to a list of symbols
  # }
  # If the number of unique symbols are provided to create the matrix
  if(! is.na(uniqueSymbolSet[1])){
    
    # Set of unique symbols used in creating the matrix;
    uniSyms = sort(unique(uniqueSymbolSet)); 
    
    if(! all(unique(seqOfSyms) %in% uniSyms  )  ){
      stop('ykang::createSTM(): There is at least 1 symbol not in the provided symbol list.');
    }
  }else{    
    uniSyms = sort(unique(seqOfSyms));   
  }
  
  # Number of total symbols
  totalSyms = length(seqOfSyms);
  
  # The number of unique symbols
  numOfuniSyms = length(uniSyms);
  
  if(debugMode) print(paste("NOTE: The total number of symbols is ", numOfuniSyms))
  
  # The transition counter/probability matrix, Initialize all cell to 0
  tcm = matrix(0, nrow = numOfuniSyms, ncol=numOfuniSyms);
  
  # Set row & column names
  rownames(tcm) = uniSyms;
  colnames(tcm) = uniSyms;
  # Count each transitions
  for( i in 1:(totalSyms - 1) ){
    
    if( (  is.vector(symbolIDList) && (! is.na(symbolIDList)) )  ){
      #Check whether the length of sequence is equal to the length of ID list.
      stopifnot(length(symbolIDList) == length(seqOfSyms));
      
      print(paste0("symbolIDList = ", symbolIDList))
      print(paste0("symbolIDList[i] = ", symbolIDList[i], "; symbolIDList[i + 1] = ", symbolIDList[i + 1]))
      print(paste0("symbolIDList[i] == symbolIDList[i + 1] ", symbolIDList[i] == symbolIDList[i + 1]))
      
      if(symbolIDList[i] == symbolIDList[i + 1] ){
        tcm[seqOfSyms[i],seqOfSyms[i + 1]] =
          tcm[seqOfSyms[i],seqOfSyms[i + 1]] + 1;        
      }
      
    }else{
      tcm[seqOfSyms[i],seqOfSyms[i + 1]] =
        tcm[seqOfSyms[i],seqOfSyms[i + 1]] + 1;
    }
  }
  
  # If "noAbsorbingStateProb" is TRUE, then get STM without absorbing state probabilities
  if(noAbsorbingStateProb){
    diag(tcm) = 0;
  }
  
  # Calculate the percentage/probability of each cell/transition
  tcm = prop.table(tcm, margin = 1)
  
  # If there is no "OUT" transition, e.g. (a,b,c) 
  # but no c->(a,b,c), then set all zero
  if( debugMode && (NaN %in% tcm ) ) print("NOTE: NaN in any cell")  
  
  # if there's any NAN than set then as 0 
  tcm[is.nan(tcm)] = 0;
  
  return(tcm);
}

##### CTHSMM #####
data$time <- as.numeric(data$time)

lrb_lw_cthsmm <- c_cthsmm(rpart_tree = rpart_model_pruned, data = data, 
                          stateDuration_Col = "event_id", obsColname = "next_action", 
                          aggDivisor = 1, ID_Col = "file_name", verbose = T)



