#### Simulate some data
# nbObs <- 1000
nbStates <- 3
stateNames <- c("resting","foraging","transit")
dist <- list(step="gamma",angle="vm")

Par <- list(step=c(5,100,500,2,50,100),
            angle=c(0,0,0,9,0.5,6))

beta <- matrix(-1.5,1,nbStates*(nbStates-1))

# simDat <- simData(obsPerAnimal = nbObs,nbStates=nbStates,dist=dist,Par=Par,beta=beta,stateNames=stateNames,states=TRUE)

# fitDat <- simDat[-nrow(simDat),] # can't have missing covariates
# threshold <- 0.44
# fitDat$rest <- factor(as.numeric(fitDat$dive<threshold)) # covariate indicating when resting threshold is satisfied
# fitDat$dive <- NULL

#### Set up model
betaRef <- c(2,2,3) # make state 2 (foraging) the tpm reference for state 1 (resting)
beta0 <- matrix(c(-100, 0.5, -100, -1.5, -100, -1.5,
                  100,   0,  100,    0,  100,    0),2,nbStates*(nbStates-1),byrow=TRUE,dimnames=list(c("rest0","rest1"),c("1 -> 1", "1 -> 3", "2 -> 1", "2 -> 3", "3 -> 1", "3 -> 2")))

delta0 <- matrix(c( 100,  100,
                    -100, -100),2,nbStates-1,byrow=TRUE,dimnames=list(c("rest0","rest1"),stateNames[-1]))

formula <- ~ 0 + rest
formulaDelta <- ~ 0 + rest

#### check tpm
trProbs <- getTrProbs(tracks[,c("ID", "rest"),drop=FALSE],nbStates=nbStates,beta=beta0,formula=formula,betaRef=betaRef,stateNames=stateNames)
trProbs[,,which.min(tracks$rest)] # tpm when next step has rest=0
trProbs[,,which.max(tracks$rest)] # tpm when next step has rest=1

Par0 <- Par[c("step","angle")]
Par0$angle <- Par$angle[-(1:nbStates)]

#### Set knownStates and fixPar$beta for extra insurance (for even extra insurance could also set workBounds$beta to be [-Inf,<<100] or [>>-100,Inf] as appropriate for estimated tpm parameter)
knownStates <- rep(NA,nrow(tracks)-5)
knownStates[tracks$rest==1] <- 1
fixPar <- list(beta=beta0)
fixPar$beta[which(beta0 > -100 & beta0 < 100 & beta0!=0)] <- NA
workBounds <- list(delta=matrix(c(100-1.e-6,-Inf,100-1.e-6,-Inf,Inf,-100+1.e-6,Inf,-100+1.e-6),nrow=4,ncol=2))
fixPar$delta <- momentuHMM:::nw2w(delta0,workBounds$delta)
if(tracks$rest[1]==0){
  fixPar$delta[1,] <- NA
}

#### fit the model
fit <- fitHMM(tracks,nbStates=nbStates,dist=dist[c("step","angle")],Par0=Par0,beta0=beta0,formula=formula,formulaDelta=formulaDelta,betaRef=betaRef,fixPar=fixPar,workBounds=workBounds,knownStates=knownStates,stateNames=stateNames)
fit
plot(fit,plotCI=TRUE,ask=FALSE)
unique(viterbi(fit)[which(fitDat$rest==0)])
unique(viterbi(fit)[which(fitDat$rest==1)])
