libs <- c('shiny', 'shinyIncubator','plyr', 'reshape2')

lapply(libs, require, character.only = TRUE)


ppsy <- function(x, p) p[4] + (1- p[3] - p[4]) * pnorm(x, p[1], exp(p[2]))

# function to convert probabilities from the psychometric function 
probaTrans <- function(prob, lambda, gamma) (prob-gamma) / (1-gamma-lambda)

# Quantile function (inverse of the psychometric function)
qpsy <- function(prob, p) qnorm(probaTrans(prob, p[3], p[4]), p[1], exp(p[2]))

# random generation from the psychometric function 
rpsy <- function(x, p, nObs)
{
  prob <- ppsy(x, p)
  rbinom(prob, nObs, prob)
}

# Functions to generate the data and fit the psychometric function
# ----------------------------------------------------------------

# a function to generate a data set 
data.gen <- function(x, p, nObs) 
{
  nYes <- rpsy(x, p, nObs) # simulated number of correct responses
  nNo <- nObs-nYes	# simulated number of incorrect responses
  cbind(x, nYes, nNo)
}

# define the likelihood function
likelihood <- function(p, df, opts) {	
  psi <- ppsy(df[, 1], c(p, 0, 0))
  -sum(df[,2]*log(psi) + df[,3]*log(1-psi))
}

# A function to fit the psychometric function 
fitPsy <- function(df, opts) 
{optim(c(1, log(3)), likelihood, df=df, opts=opts)
}

# a function to extract the parameters of a psychometric function
getParam <- function(obj, opts) {
  out <- c(obj$par, 0, 0)
  names(out) <- c('alpha', 'log(beta)', 'lambda', 'gamma')
  return(out)
}
