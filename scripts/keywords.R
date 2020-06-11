# These functions are copied from the STM package
# They are changed to return also the values, not just the keywords
# This way the values can be shown to the user, making interpretation of results easier

# My implementation of labelTopics()
topicKeywords <- function(model) {
  logbeta <- model$beta$logbeta[[1]]
  topics <- model$settings$dim$K
  
  prob <- calculateProbability(logbeta)
  frex <- calculateFrex(logbeta, wordcounts = model$settings$dim$wcounts$x)
  lift <- calculateLift(logbeta, wordcounts = model$settings$dim$wcounts$x)
  score <- calculateScore(logbeta)
  
  keywords <- lapply(1:topics, function(i) {
    data.frame(
      "prob" = prob[, i],
      "frex" = frex[, i],
      "lift" = lift[, i],
      "score" = score[, i])
  })
  
  for(i in 1:topics) {
    keywords[[i]]$prob.word <- model$vocab[keywords[[i]]$prob.word]
    keywords[[i]]$frex.word <- model$vocab[keywords[[i]]$frex.word]
    keywords[[i]]$lift.word <- model$vocab[keywords[[i]]$lift.word]
    keywords[[i]]$score.word <- model$vocab[keywords[[i]]$score.word]

  }
  
  names(keywords) = paste0("topic", 1:topics)

  keywords
}

calculateProbability <- function(logbeta) {
  rank <- apply(logbeta, 1, order, decreasing=TRUE)
  probability <- sapply(1:nrow(logbeta), function(i) list("value" = logbeta[i, rank[, i]], "word" = rank[, i]))
}
  
calculateFrex <- function(logbeta, w = 0.5, wordcounts = NULL) {
  excl <- t(t(logbeta) - col.lse(logbeta))
  if(!is.null(wordcounts)) {
    #if word counts provided calculate the shrinkage estimator
    excl <- safelog(sapply(1:ncol(excl), function(x) js.estimate(exp(excl[,x]), wordcounts[x])))
  } 
  freqscore <- apply(logbeta,1,data.table::frank)/ncol(logbeta)
  exclscore <- apply(excl,1,data.table::frank)/ncol(logbeta)
  frex <- 1/(w/freqscore + (1-w)/exclscore)
  
  rank <- apply(frex,2,order,decreasing=TRUE)
  frex <- sapply(1:ncol(frex), function(i) list("value" = frex[rank[, i], i], "word" = rank[, i]))
}

calculateLift <- function(logbeta, wordcounts) {
  emp.prob <- log(wordcounts) - log(sum(wordcounts))
  lift <- logbeta - rep(emp.prob, each=nrow(logbeta)) 
  rank <- apply(lift, 1, order, decreasing=TRUE)
  lift <- sapply(1:nrow(lift), function(i) list("value" = lift[i, rank[, i]], "word" = rank[, i]))
}

calculateScore <- function(logbeta) { 
  ldascore <- exp(logbeta)*(logbeta - rep(colMeans(logbeta), each=nrow(logbeta)))
  rank <- apply(ldascore, 1, order, decreasing=TRUE)
  score <- sapply(1:nrow(ldascore), function(i) list("value" = ldascore[i, rank[, i]], "word" = rank[, i]))
} 


js.estimate <- function(prob, ct) {
	if(ct<=1) {
		#basically if we only observe a count of 1
		#the variance goes to infinity and we get the uniform distribution.
		return(rep(1/length(prob), length(prob)))
	}
	# MLE of prob estimate
	mlvar <- prob*(1-prob)/(ct-1)
	unif <- rep(1/length(prob), length(prob)) 
	
	# Deviation from uniform
	deviation <- sum((prob-unif)^2)
	
	#take care of special case,if no difference it doesn't matter
	if(deviation==0) return(prob)
	
	lambda <- sum(mlvar)/deviation
	#if despite  our best efforts we ended up with an NaN number-just return the uniform distribution.
	if(is.nan(lambda)) return(unif)
	
	#truncate
	if(lambda>1) lambda <- 1
	if(lambda<0) lambda <- 0
	
	#Construct shrinkage estimator as convex combination of the two
	lambda*unif + (1 - lambda)*prob
}

logsoftmax <- function(x) {
	x - lse(x)
}

lse <- function(x) {
	matrixStats::logSumExp(x)
}

row.lse <- function(mat) {
	matrixStats::rowLogSumExps(mat)
}
col.lse <- function(mat) {
	matrixStats::colLogSumExps(mat)
}

softmax <- function(x) {
	exp(x - lse(x))
}

safelog <- function(x, min=-1000) {
	out <- log(x)
	out[which(out< min)] <- min
	out
}