# Helpfunction
  B2P_fixed <- function(x){tcrossprod(x)/sum(x^2)  }

#' Function to Combine epplab Objects
#' 
#' Function that automatically determines directions of interest. Several epplab objects 
#' can be combined
#' 
#' The parameter \code{x} can either be a single object or a list of epplab objects
#' 
#' Options for method are inverse, sqinverse and cum.
#'
#' @param x An object of class \code{epplab}. 
#' @param method The type of method, see details
#' @param percentage Threshold of explained variance
#' @return A list with class 'epplabagg' containing the following
#' components: 
#' \item{P}{something}
#' \item{O}{something}
#' \item{k}{something}
#' \item{eigenvalues}{something}
#' @author Anne Ruiz-Gazen, Daniel Fischer, Klaus Nordhausen
#' @seealso \code{\link{EPPlab}}
#' @references \cite{Cite What?}
#' @keywords multivariate
#' @examples
#' 
#'  library(tourr)
#'  data(olive)
#'  olivePP.kurt.max <-
#'    EPPlab(olive[,3:10],PPalg="PSO",PPindex="KurtosisMax",n.simu=10, maxiter=20)
#'  
#'  olivePP.fried <-
#'    EPPlab(olive[,3:10],PPalg="PSO",PPindex="Friedman",n.simu=10, maxiter=20)
#'  
#'  olivePPs <- list(olivePP.kurt.max, olivePP.fried)
#'  
#'  EPPlabAgg(olivePP.kurt.max)$k
#'  EPPlabAgg(olivePPs, "cum", 0.99)$k
#'  
#'  pairs(olivePP.kurt.max$x %*% EPPlabAgg(olivePPs, "cum", 0.99)$O,
#'        col=olive[,2], pch=olive[,1])
#'  
#'  
#'  olivAOP.sq <- EPPlabAgg(olivePPs, "inv")
#'  oliveProj <- olivePP.kurt.max$x %*% olivAOP.sq$O
#'  plot(density(oliveProj))
#'  rug(oliveProj[olive$region==1],col=1)
#'  rug(oliveProj[olive$region==2],col=2)
#'  rug(oliveProj[olive$region==3],col=3)
#' 
#' @export EPPlabAgg
EPPlabAgg <- function(x, method="cumulative", percentage=0.95){
  
      # Input checks
        method <- match.arg(method, c("inverse", "sq.inverse", "cumulative"))
        if(class(x)=="epplab") x <- list(x)  
      
      # Now combine the results
        switch(method,
               cumulative={
                # Store the the averages, initialize with 0
                  avgMatrix <- matrix(0, nrow=dim(x[[1]]$x)[2], ncol=dim(x[[1]]$x)[2])
                # Go through each REPPlab result object (using this option, length(x) is probably always equal to 1)
                  for(i in 1:length(x)){
                  # Now go through all directions 
                    for(dirRun in 1:dim(x[[i]]$PPdir)[2]){
                  # Now sum them up
                      avgMatrix <- avgMatrix + tcrossprod(x[[i]]$PPdir[, dirRun])
                    }
                  }
                # Divide by the summands to get the average
                  avgMatrix <- avgMatrix / (dim(x[[i]]$PPdir)[2] * length(x))
                # Calculate the eigenelements
                  eigmave<-eigen(avgMatrix) 
                  lmave<-eigmave$values
                  umave<-eigmave$vectors
                # eliminate the directions that are associated with less than '1-percentage'% of the information
                  takeThese <- 1:min(sum(((cumsum(lmave)/sum(lmave))<percentage))+1, length(lmave))
                  keepmave<-umave[,takeThese,drop=FALSE] 
                # project the data on the directions we keep
                  coord <- x[[i]]$x %*% keepmave           
                # Write out the results
                  res <- list(P=O2P(umave), O=keepmave, k= ncol(keepmave), eigen=lmave)      
                },
                {
                  B2P.output <-  list()
                  lresB2P <- list()
                  lresB2P.all <- c()
                  for(i in 1:length(x)){
                    B2P.output[[i]] <- apply(coef(x[[i]]),2 , B2P_fixed)
                    lresB2P[[i]] <- tapply(B2P.output[[i]], gl(ncol(B2P.output[[i]]), nrow(B2P.output[[i]])), matrix, nrow=dim(x[[1]]$x)[2], ncol=dim(x[[1]]$x)[2])
                    lresB2P.all <- c(lresB2P.all, lresB2P[[i]])
                  }
                  res <- AOP(lresB2P.all, weights = method)  
                })
      # Return the results
        res
}
