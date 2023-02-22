#' Compute Viterbi path/state sequence of an CTHSMM given observation sequence.
#' @description Compute Viterbi path/state sequence of an CTHSMM given 
#' observation sequence, by using mhsmm::predict.hsmmspec().
#' @author Yihuang Kang
#' @param cthsmm An CTHSMM from cthsmm().
#' @param obsSeq An observation sequence in characters.
#' @param maxStateTimeSpent Maximum number of time spent in a state. 
#' "M" in predict.hsmmspec().
#' @return Return a viterbi path/state sequence. E.g. 's1', 's2', 's3'...

cthsmm_viterbi = function(cthsmm, obsSeq, maxStateTimeSpent = NA ) {
  # Convert character observation sequence into numbers
  obsSeqNum = match(obsSeq, cthsmm$obs );
  
  # print("obsSeq = "); print(obsSeq)
  # print("cthsmm$obs = "); print(cthsmm$obs)
  # Length of observation sequences
  lenObsSeq = length(obsSeq);
  
  # print("lenObsSeq = "); print(lenObsSeq)
  print("obsSeqNum = "); print(obsSeqNum)
  print("data.frame(x = obsSeqNum, N = lenObsSeq) = "); print(data.frame(x = obsSeqNum, N = lenObsSeq))
  # Get Viterbi path/state sequence in numbers
  stateSeqNum = mhsmm::predict.hsmmspec(cthsmm$hsmm,
                                        data.frame(x = obsSeqNum, N = lenObsSeq), 
                                        method = "viterbi", M = maxStateTimeSpent)$s;
  print("end of viterbi")
  # State sequence in character/string: s1, s2, .... Run Length Encoding
  stateSeqRLE = rle(paste('s', stateSeqNum, sep = ''));
  
  stateSeq = list();
  stateSeq$stateDuration = stateSeqRLE$lengths;
  stateSeq$stateSequence = stateSeqRLE$values;
  
  print("Duration:"); print(stateSeq$stateDuration, sep="");
  print("State:"); print(paste(stateSeq$stateSequence, collapse = " -> "));
  
  return(stateSeq);
}