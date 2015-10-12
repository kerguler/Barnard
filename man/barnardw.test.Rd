\name{barnardw.test}
\alias{barnardw.test}
\title{ Barnard's Unconditional Test with Wald Statistics}
\description{
Barnard's unconditional test for superiority applied to 2x2 contingency tables
using Wald statistics for the difference between two binomial proportions.
}
\usage{
barnardw.test(n1, n2, n3, n4, dp = 0.001, verbose = FALSE)
}
\arguments{
  \item{n1,n2,n3,n4}{ Elements of the 2x2 contingency table }
  \item{dp}{ The resolution to search for the nuisance parameter }
  \item{verbose}{ Extended output with 'wald.statistic.table' and 'nuisance.matrix' }
}

\value{
  \item{wald.statistic.table }{The contingency tables considered in the analysis represented by 'n1' and 'n2', their Wald statistics, and whether they are included in the one-sided (1), two-sided (2) tests, or not included at all (0)}
  \item{nuisance.matrix }{Nuisance parameters and the corresponding p-values for both one- and two-sided tests}
  \item{dp }{The resolution of the search space for the nuisance parameter}
  \item{contingency.matrix }{The observed 2x2 contingency table}
  \item{alternative }{One Sided or Two Sided}
  \item{wald.statistic }{The standardized difference between the observed proportions}
  \item{nuisance.parameter }{The nuisance parameter where the p-value is maximized}
  \item{p.value }{The p-value for the observed contingency table}
}
\details{
For a 2x2 contingency table, such as \eqn{X=[n_1,n_2;n_3,n_4]}, the normalized difference in proportions between the two categories, given in each column, can be written as
\deqn{T(X)=\frac{\frac{n_2}{n_2+n_4}-\frac{n_1}{n_1+n_3}}{\sqrt{\frac{(n_1+n_2)(n_3+n_4)}{(n_1+n_2+n_3+n_4)^2}(\frac{1}{n_1+n_3}+\frac{1}{n_2+n_4})}}}
This is referred to as the Wald statistic. The probability of observing \eqn{X} is
\deqn{P(X)=\frac{(n_1+n_3)!(n_2+n_4)!}{n_1!n_2!n_3!n_4!}\pi^{n_1+n_2}(1-\pi)^{n_3+n_4}}
where \eqn{\pi} is the unknown nuisance parameter.

Barnard's test considers all tables with category sizes \eqn{n_1+n_3} and \eqn{n_2+n_4} for a given \eqn{\pi}. The p-value is the sum of probabilities of the tables having Wald statistics in the rejection region, e.g. having significantly large difference in proportions for a two-sided test. The p-value of the test is the maximum p-value calculated over all \eqn{\pi} between 0 and 1.
}
\references{
\enumerate{
\item Barnard, G.A. (1945) A new test for 2x2 tables. \emph{Nature}, 156:177.
\item Barnard, G.A. (1947) Significance tests for 2x2 tables. \emph{Biometrika}, 34:123-138.
\item Cardillo G. (2009) MyBarnard: a very compact routine for Barnard's exact test on 2x2 matrix. URL \url{http://www.mathworks.com/matlabcentral/fileexchange/25760}
\item Galili T. (2010) URL \url{http://www.r-statistics.com/2010/02/barnards-exact-test-a-powerful-alternative-for-fishers-exact-test-implemented-in-r/}
\item Lin C.Y., Yang M.C. (2009) Improved p-value tests for comparing two independent binomial proportions. \emph{Communications in Statistics-Simulation and Computation}, 38(1):78-91.
\item Trujillo-Ortiz, A., R. Hernandez-Walls, A. Castro-Perez, L. Rodriguez-Cardozo N.A. Ramos-Delgado and R. Garcia-Sanchez. (2004). Barnardextest:Barnard's Exact Probability Test. A MATLAB file. [WWW document]. URL \url{http://www.mathworks.com/}
}
}
\author{ 
Kamil Erguler, Post-Doctoral Research Associate, Molecular Medicine Research Center, University of Cyprus \email{erguler.kamil@ucy.ac.cy}
}
\note{ I am indebted to Peter Calhoun for helping to test the performance and the accuracy of the code. The accuracy has been tested with respect to the existing MATLAB and R implementations as well as the results of StatXact. I have largely been influenced by the works of Trujillo-Ortiz etal. (2004), Cardillo G. (2009), and Galili T. (2010). }

\examples{
barnardw.test(8,14,1,3)

bt<-barnardw.test(8,14,1,3,verbose=TRUE)
plot(bt$nuisance.matrix[,1:2],t="l",xlab="nuisance parameter",ylab="p-value")

bt<-barnardw.test(40,14,10,30,verbose=TRUE)$wald.statistic.table
plot(bt[,1],bt[,2],col=hsv(bt[,4]/4,1,1),t="p",xlab="n1",ylab="n2")
}

\keyword{nonparametric}
\keyword{htest}

