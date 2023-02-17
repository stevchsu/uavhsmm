#' CTHSMM funciton
#' migrate from package "ykang"
#' Chan Hsu
#' 2023/02/16
#' 
cthsmm = function(rpart_tree, data, stateDuration_Col, obsColname, aggDivisor, ID_Col = NULL, verbose = F){
  
  # Check if two sequence are character vector
  stopifnot(is.data.frame(data) );
  
  # Initializing...
  if(verbose) print("Initializing...");
  cthsmm = list();
  
  # Number of Observations
  cthsmm$obs = attr(x=rpart_tree,'ylevels');
  cthsmm$numOfObs = length(cthsmm$obs);
  
  
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
  
  print("sr = ")
  if(verbose) print(sr);
  # State to Node mapping information with rules
  
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
  data[2:nrow(data),'IdentPrevID'] = data[1:(nrow(data) - 1),][[ID_Col]] == data[2:nrow(data),][[ID_Col]];
  data[2:nrow(data),'IdentPrevState'] = data[1:(nrow(data) - 1), 'actState'] == data[2:nrow(data), 'actState'];
  data[2:nrow(data),'IdentPrevObs'] = data[1:(nrow(data) - 1), ][[obsColname]] == data[2:nrow(data), ][[obsColname]];
  data[1,'IdentPrevID'] = T; data[1,'IdentPrevState'] = T; data[1,'IdentPrevObs'] = T; 
  data[,'Agg_ID'] = NA;
  # print(data$Agg_ID)
  
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
  
  tmpCt <- 1; # Iterator
  for(i in 1:nrow(data)){
    if(isTRUE(data$dup[i])){
      # data[i,'Agg_ID'] <- tmpCt;
      data$Agg_ID[i] <- as.numeric(tmpCt)
    }else{
      tmpCt <- tmpCt + 1;
      # print(paste0("tmpCt = ", tmpCt))
      # data[i,'Agg_ID'] <- tmpCt;
      data$Agg_ID[i] <- as.numeric(tmpCt)
    }
  }
  # print("after insert value")
  # print(data$Agg_ID)
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
  #print(sqlState)
  cthsmm$agg_data = sqldf(sqlState)
  #cthsmm$agg_data$Duration = ceiling(cthsmm$agqg_data$Duration);
  
  if(verbose) print("Computing State Transition Matrix ( createSTM() ) ...") ;
  # Create state transition matrix
  if(is.null(ID_Col)){
    stm = ykang::createSTM(seqOfSyms =cthsmm$agg_data$State, isSingleString=FALSE, 
                           uniqueSymbolSet=1:cthsmm$numOfStates, noAbsorbingStateProb=T );
  }else{
    stm = ykang::createSTM(seqOfSyms=cthsmm$agg_data$State, isSingleString=FALSE, symbolIDList=cthsmm$agg_data[,ID_Col],
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
