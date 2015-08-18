# Help function
  applyB2P <- function(x){
    resB2P <- apply(coef(x),2 , B2P)
    tapply(resB2P, gl(ncol(resB2P), nrow(resB2P)), matrix, nrow=ncol(x$x), ncol=ncol(x$x))
  }

#' Combining Different EPPlab results
#' 
#' The function automatically combines the output of several \code{EPPlab} runs.
#' 
#' Instead of relying on the outcome of a single EPP-lab run one can use also this
#' function to run EPPlab with different indices and combine them then using the
#' \code{LDRTools::B2B} function. 
#' The different predefined ensembles are defined via the \code{type} option and
#' possible values are: \code{clust}, \code{fast}, \code{all}, \code{friedKurt}.
#' 
#' All tuning parameters are as described in \code{\link{REPPlab:EPPlab}}
#' 
#' @param x A numeric data frame or data matrix with at least two rows.
#' @param type The type of ensemble, see details.
#' @param ... Tuning paramters, see \code{\link{REPPlab:EPPlab}}.
#'
#' @return The whitened data.
#'
#' @author Daniel Fischer
#' @seealso \code{\link{LDRTools::B2B}}, \code{\link{LDRTools:AOP}}
#' @keywords multivariate
#' @examples
#' 
#' # more observations than variables
#' X <- matrix(rnorm(200),ncol=4)
 
#' 
#' @export EPPlab.ensemble
EPPlab.ensemble <- function(x, type, ...){
  # Check the input value
    type <- match.arg(type,c("clust","fast","all","friedKurt"))
  # Now go through the different cases
    if(type=="clust"){
      REPPres.fried <- EPPlab(sim.data, PPindex="Friedman", ...)
      REPPres.disc <- EPPlab(sim.data, PPindex="Dis", ...)
      REPPres.ft <- EPPlab(sim.data, PPindex="FriedmanTukey", ...)
      REPPres.kmax <- EPPlab(sim.data, PPindex="KurtosisMax", ...)
    } else if(type=="fast"){
      REPPres.fried <- EPPlab(sim.data, PPindex="Friedman", ...)
      REPPres.kmin <- EPPlab(sim.data, PPindex="KurtosisMin", ...)  
    } else if(type=="all"){
      REPPres.fried <- EPPlab(sim.data, PPindex="Friedman", ...)
      REPPres.disc <- EPPlab(sim.data, PPindex="Dis", ...)
      REPPres.ft <- EPPlab(sim.data, PPindex="FriedmanTukey", ...)
      REPPres.kmax <- EPPlab(sim.data, PPindex="KurtosisMax", ...)
      REPPres.kmin <- EPPlab(sim.data, PPindex="KurtosisMin", ...)  
    } else if(type=="friedKurt"){
      REPPres.fried <- EPPlab(sim.data, PPindex="Friedman", ...)
      REPPres.kmax <- EPPlab(sim.data, PPindex="KurtosisMax", ...)
      REPPres.kmin <- EPPlab(sim.data, PPindex="KurtosisMin", ...)    
    }
}

  
  resB2P.REPPres.disc <- apply(coef(REPPres.disc),2 , B2P)
  lresB2P.REPPres.disc <- tapply(resB2P.REPPres.disc, gl(ncol(resB2P.REPPres.disc), nrow(resB2P.REPPres.disc)), matrix, nrow=ncol(sim.data), ncol=ncol(sim.data))
  
  resB2P.REPPres.ft <- apply(coef(REPPres.ft),2 , B2P)
  lresB2P.REPPres.ft <- tapply(resB2P.REPPres.ft, gl(ncol(resB2P.REPPres.ft), nrow(resB2P.REPPres.ft)), matrix, nrow=ncol(sim.data), ncol=ncol(sim.data))
  
  resB2P.REPPres.kmax <- apply(coef(REPPres.kmax),2 , B2P)
  lresB2P.REPPres.kmax <- tapply(resB2P.REPPres.kmax, gl(ncol(resB2P.REPPres.kmax), nrow(resB2P.REPPres.kmax)), matrix, nrow=ncol(sim.data), ncol=ncol(sim.data))
  
  resB2P.REPPres.kmin <- apply(coef(REPPres.kmin),2 , B2P)
  lresB2P.REPPres.kmin <- tapply(resB2P.REPPres.kmin, gl(ncol(resB2P.REPPres.kmin), nrow(resB2P.REPPres.kmin)), matrix, nrow=ncol(sim.data), ncol=ncol(sim.data))
