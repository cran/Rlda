\name{rlda.binomialVB}
\alias{rlda.binomialVB}
\title{LDA with binomial entry and Stick-Breaking prior and Variational Inference.}
\description{
  This method implements the Latent Dirichlet Allocation with
  Stick-Breaking prior for binomial data with Variational Inference.
  \code{rlda.binomialVB} works with frequency data.frame.
}
\usage{
  rlda.binomialVB(data, loc.id, n_community, alpha0, alpha1,
  gamma, maxit=1000, thresh=0.0001)
}
\arguments{
  \item{data}{A abundance data.frame where each row is a sampling unit
    (i.e. Plots, Locations, Time, etc.) and each column is a categorical
    type of element (i.e. Species, Firms, Issues, etc.).}
  \item{loc.id}{ID variable in data.frame.}
  \item{n_community}{Total number of communities to return. It must be less than
  the total number of columns inside the \code{data}.}
  \item{alpha0}{Hyperparameter associated with the Beta prior Beta(alpha0, alpha1).}
  \item{alpha1}{Hyperparameter associated with the Beta prior Beta(alpha0, alpha1).}
  \item{gamma}{Hyperparameter associated with the Stick-Breaking prior.}
  \item{maxit}{Maximum number of iterations.}
  \item{thresh}{Threshold for ELBO convergence.}
      }
    \value{
      A R List with three elements:
  \item{Theta}{The individual probability for each observation
  (ex: location) belong in each cluster (ex: community). It is a matrix
  with dimension equal \code{n_gibbs} by \code{nrow(data) * n_community}}
  \item{Phi}{The individual probability for each variable
  (ex: Specie) belong in each cluster (ex: community). It is a matrix
  with dimension equal \code{n_gibbs} by \code{ncol(data) * n_community}}
  \item{ELBO}{The vector ofELBO compute for
  each iteration.}
}
\details{
  \code{rlda.binomialVB} uses a modified Latent Dirichlet Allocation method
  to construct Mixed-Membership Clusters using Bayesian Variational Inference.
  The \code{data} must be a non-empty data.frame with the frequencies for each variable
  (column) in each observation (row).
}
\note{The \code{Theta} and \code{Phi} matrix can be obtained for the i-th gibbs
sampling using \code{matrix(Theta[i,], nrow = nrow(data), ncol = n_community)} and
\code{matrix(Phi[i,], nrow = n_community, ncol = ncol(data))}, respectively.}
\references{
  \itemize{
    \item
     Blei, David M., Andrew Y. Ng, and Michael I. Jordan.\cr
     "Latent dirichlet allocation." \emph{Journal of machine Learning research}
      3.Jan (2003): 993-1022.
      \url{http://www.jmlr.org/papers/volume3/blei03a/blei03a.pdf}
   \item
      Valle, Denis, et al.\cr
      "Decomposing biodiversity data using the Latent Dirichlet
      Allocation model, a probabilistic multivariate statistical
      method." \emph{Ecology letters} 17.12 (2014): 1591-1601.
  }
}
\author{
  \itemize{
    \item
      Pedro Albuquerque.\cr
      \email{pedroa@unb.br}\cr
      \url{http://pedrounb.blogspot.com/}
   \item
      Denis Valle.\cr
      \email{drvalle@ufl.edu}\cr
      \url{http://denisvalle.weebly.com/}
   \item
      Daijiang Li.\cr
      \email{daijianglee@gmail.com}\cr
      \url{http://daijiang.name}
  }
}
\seealso{\code{\link{rlda.multinomial}}, \code{\link{rlda.bernoulli}} }

\keyword{LDA}
\keyword{Mixed-Membership}
\keyword{Cluster}
\keyword{Binomial}
\keyword{Variational Inference}

\examples{
	\dontrun{
		library(Rlda)
		# Set seed
		set.seed(5874)
		# Hyperparameters for each prior distribution
		gamma  <- 0.01
		alpha0 <- 0.01
		alpha1 <- 0.01
		# Execute the LDA for the Binomial entry
		res<- rlda.binomialVB(data=tmp, loc.id='loc.id', n_community=10,
		alpha0=0.01, alpha1=0.99, gamma=0.1, maxit=100, thresh=0.0001)
	}
}

