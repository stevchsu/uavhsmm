#' Test and evaluate fitted Classification Tree Hidden Semi-Markov Model (CTHSMM).
#' @description A function the evaluate the performance of CTHSMM by applying it to a test dataset.
#' @author Yihuang Kang
#' @param cthsmm An "rpart" tree-based HSMM
#' @param test_data The testing data datset.
#' @param obsColname Target/outcome/Observation column name
#' @param ID_Col ID column name
#' @param stateDuration_Col State duration column name
#' @param aggDivisor The divisor for state duration aggregation sum().E.g. 360 min / "60" as 1 hr.  Divisor = 60
#' @param viterbi_len The length of Viterbi path.
#' @import c('sqldf')
#' @return Return An R list. The test dataset with predicted/actual state sequence, 
#' the hits/LMRL of all length and given (Viterbi) length.

library(reshape)

cthsmm_eval = function(cthsmm, test_data, obsColname, ID_Col, stateDuration_Col, aggDivisor, viterbi_len ){
  # Output the result
  out = list();
  
  # A list of IDs with their actual & predicted sequences
  ID_actPredSeq = list();
  
  # Get IDs with their length of sequences
  seqLenByID = aggregate(x=test_data[[ID_Col]],by=list('ID'=test_data[[ID_Col]]), FUN=length);
  colnames(seqLenByID) = c('ID','nseq');
  
  # Number of IDs/instances  
  numOfID = nrow(seqLenByID);
  
  # Create 2 column for hits and LMRL (logest matched run length);
  seqLenByID$hits = numeric(numOfID);
  seqLenByID$LMRL = numeric(numOfID);
  
  
  # Number of rows for all the testing dataset
  numOfRec = nrow(test_data);
  
  # Create a new column for the actual state identified by the state-spliting rules.
  test_data$actState = character(numOfRec);
  
  # Create a new column for the predicted state identified by the Viterbi algorithm  
  # test_data$predState = character(numOfRec);
  
  # Identify the state sequence using rules in CTHSMM  
  attach(test_data);
  for(i in 1:cthsmm$numOfStates){
    stateAssignExp = parse(text = paste("test_data$actState[which(", cthsmm$state2node$stateRule[i], ")] = 's", i , "' " ,sep = '') ) ;
    eval(stateAssignExp);    
  }  
  detach(test_data);
  
  
  # Aggregate data
  
  # Identical "ID", "State","Observation" with previous row/observation
  test_data[2:nrow(test_data),'IdentPrevID'] = test_data[[ID_Col]][1:(nrow(test_data) - 1)] == test_data[[ID_Col]][2:nrow(test_data)];
  test_data[2:nrow(test_data),'IdentPrevState'] = test_data[1:(nrow(test_data) - 1), 'actState'] == test_data[2:nrow(test_data), 'actState'];
  test_data[2:nrow(test_data),'IdentPrevObs'] = test_data[[obsColname]][1:(nrow(test_data) - 1)] == test_data[[obsColname]][2:nrow(test_data)];
  test_data[1,'IdentPrevID'] = T; test_data[1,'IdentPrevState'] = T; test_data[1,'IdentPrevObs'] = T; 
  test_data[,'Agg_ID'] = NA;
  
  # Assign a group ID to get sum of each ID-state-observation
  tmpCt = 1; # Iterator
  for(i in 1:nrow(test_data)){
    
    if(test_data[i,'IdentPrevID'] & test_data[i,'IdentPrevState'] & test_data[i,'IdentPrevObs']){
      test_data$Agg_ID[i] = tmpCt;
    }else{
      tmpCt = tmpCt + 1;
      test_data$Agg_ID[i] = tmpCt;
    }
  }
  
  #print(test_data[,c('actState','IdentPrevID','IdentPrevState','IdentPrevObs','IdentPrevID','Agg_ID')]);  
  #print(test_data[1:100,c(ID_Col,'actState', 'Duration', obsColname,'IdentPrevID', 'IdentPrevState' ,'IdentPrevObs', 'Agg_ID'  )]);
  
  sqlState = paste('select ', ID_Col, ', actState as State,', obsColname, 
                   ', sum(', stateDuration_Col ,') / ', aggDivisor,
                   ' as Duration from test_data group by Agg_ID', sep = '');
  # Aggregated dataset
  agg_data = sqldf(sqlState);
  
  # Round up duration
  # print(head(agg_data))
  agg_data$Duration = ceiling(agg_data$Duration);
  
  agg_data[2:nrow(agg_data),'IdentPrevID'] = agg_data[[ID_Col]][1:(nrow(agg_data) - 1)] == agg_data[[ID_Col]][2:nrow(agg_data)];
  agg_data[2:nrow(agg_data),'IdentPrevObs'] = agg_data[[obsColname]][1:(nrow(agg_data) - 1)] == agg_data[[obsColname]][2:nrow(agg_data)];
  agg_data[1,'IdentPrevID'] = T; agg_data[1,'IdentPrevObs'] = T; 
  agg_data[,'Agg_ID'] = NA;
  
  tmpCt = 1; # Iterator
  for(i in 1:nrow(agg_data)){
    if(agg_data[i,'IdentPrevID'] && agg_data[i,'IdentPrevObs']){
      agg_data[i,'Agg_ID'] = tmpCt;
    }else{
      tmpCt = tmpCt + 1;
      agg_data[i,'Agg_ID'] = tmpCt;
    }
  }
  
  sqlState = paste('select ', ID_Col, ', ', obsColname, 
                   ', sum(Duration) as obsDuration from agg_data group by Agg_ID' ,sep='');
  
  
  # Aggregated data by obervations
  agg_data_obs = sqldf(sqlState,stringsAsFactors=F);
  
  
  # print(agg_data);
  
  actPred_states = untable(df=agg_data[,c(ID_Col,'State')],num=agg_data[,'Duration']);
  colnames(actPred_states) = c('ID','actState');  
  
  # Unique IDs
  matches = list();
  matches$ID = unique(agg_data_obs[,ID_Col]);
  matches = as.data.frame(matches);
  
  obsSeq = NULL;
  predState = NULL;
  
  for(i in 1:nrow(matches)){
    tmpObsSeq = NULL;
    agg_data_obs_subset = subset(agg_data_obs, agg_data_obs$ID == matches[i,'ID']);
    
    for(j in 1:nrow(agg_data_obs_subset)){
      tmpObsSeq = c(tmpObsSeq, rep(agg_data_obs_subset[j, 'Location'], 
                                   agg_data_obs_subset[j, 'obsDuration']));
    }
    obsSeq = c(obsSeq,tmpObsSeq);
    predState = c(predState,  cthsmm_viterbi(cthsmm, tmpObsSeq));
    
  }
  
  actPred_states$Observation = obsSeq;
  actPred_states$predState = predState;  
  actPred_states$matched = actPred_states$actState == actPred_states$predState ;
  
  # List of ID, observation, actual and predicted states
  out$actPred_states = actPred_states;
  
  # Average hit/LMRL ratios for 1 to viterbi_len                                        
  avgHitRatios = numeric(viterbi_len);
  avgLMRLRatios = numeric(viterbi_len);
  
  for(k in 1:viterbi_len){
    hitRatios = numeric(nrow(matches));
    LMRLRatios = numeric(nrow(matches));  
    
    for(i in 1:nrow(matches )){
      
      # The indices of the sequence of an ID
      whichIDSeqIdx = which(actPred_states$ID == matches[i,'ID'])
      
      hitRatios[i] = sum(actPred_states[whichIDSeqIdx[1:k],'matched']) / k ;
      LMRLRatios[i] = longestMatchedRunLen(actPred_states[whichIDSeqIdx[1:k],'actState'],
                                           actPred_states[whichIDSeqIdx[1:k],'predState'],
                                           TRUE) / k ;      
    }
    avgHitRatios[k] = mean(hitRatios,na.rm=T);
    avgLMRLRatios[k] = mean(LMRLRatios,na.rm=T);    
  }
  
  out$avgHitRatios = avgHitRatios;
  out$avgLMRLRatios = avgLMRLRatios;
  
  
  return(out);
}
