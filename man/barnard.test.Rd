\name{barnard.test}
\alias{barnard.test}
\title{ Barnard's Unconditional Test}
\description{
Barnard's unconditional test for superiority applied to 2x2 contingency tables
using Score or Wald statistics for the difference between two binomial proportions.
}
\usage{
barnard.test(n1, n2, n3, n4, dp = 0.001, pooled = TRUE)
}
\arguments{
  \item{n1,n2,n3,n4}{ Elements of the 2x2 contingency table }
  \item{dp}{ The resolution to search for the nuisance parameter }
  \item{pooled}{ Z statistic with pooled (Score) or unpooled (Wald) variance }
}

\value{
  \item{statistic.table }{The contingency tables considered in the analysis represented by 'n1' and 'n2', their scores, and whether they are included in the one-sided (1), two-sided (2) tests, or not included at all (0)}
  \item{nuisance.matrix }{Nuisance parameters, \eqn{p}, and the corresponding p-values for both one- and two-sided tests}
  \item{dp }{The resolution of the search space for the nuisance parameter}
  \item{contingency.matrix }{The observed 2x2 contingency table}
  \item{alternative }{One sided or two sided test}
  \item{statistic }{The standardized difference between the observed proportions}
  \item{nuisance.parameter }{The nuisance parameter where the p-value is maximized}
  \item{p.value }{The p-value for the observed contingency table}
  \item{pooled }{Variance estimator of the Z statistic}
}
\details{
For a 2x2 contingency table, such as \eqn{X=[n_1,n_2;n_3,n_4]}, the normalized difference in proportions between the two categories, given in each column, can be written with pooled variance (Score statistic) as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\hat{p}(1-\hat{p})(\frac{1}{c_1}+\frac{1}{c_2})}},}
where \eqn{\hat{p}=(n_1+n_3)/(n_1+n_2+n_3+n_4)}, \eqn{\hat{p}_2=n_2/(n_2+n_4)}, \eqn{\hat{p}_1=n_1/(n_1+n_3)}, \eqn{c_1=n_1+n_3} and \eqn{c_2=n_2+n_4}. Alternatively, with unpooled variance (Wald statistic), the difference in proportions can we written as
\deqn{T(X)=\frac{\hat{p}_2-\hat{p}_1}{\sqrt{\frac{\hat{p}_1(1-\hat{p}_1)}{c_1}+\frac{\hat{p}_2(1-\hat{p}_2)}{c_2}}}.}
The probability of observing \eqn{X} is
\deqn{P(X)=\frac{c_1!c_2!}{n_1!n_2!n_3!n_4!}p^{n_1+n_2}(1-p)^{n_3+n_4},}
where \eqn{p} is the unknown nuisance parameter.

Barnard's test considers all tables with category sizes \eqn{c_1} and \eqn{c_2} for a given \eqn{p}. The p-value is the sum of probabilities of the tables having a score in the rejection region, e.g. having significantly large difference in proportions for a two-sided test. The p-value of the test is the maximum p-value calculated over all \eqn{p} between 0 and 1.
}
\references{
\enumerate{
\item Barnard, G.A. (1945) A new test for 2x2 tables. \emph{Nature}, 156:177.
\item Barnard, G.A. (1947) Significance tests for 2x2 tables. \emph{Biometrika}, 34:123-138.
\item Suissa, S. and Shuster, J. J. (1985), Exact Unconditional Sample Sizes for the 2x2 Binomial Trial, Journal of the Royal Statistical Society, Ser. A, 148, 317-327.
\item Cardillo G. (2009) MyBarnard: a very compact routine for Barnard's exact test on 2x2 matrix. URL \url{http://www.mathworks.com/matlabcentral/fileexchange/25760}
\item Galili T. (2010) URL \url{http://www.r-statistics.com/2010/02/barnards-exact-test-a-powerful-alternative-for-fishers-exact-test-implemented-in-r/}
\item Lin C.Y., Yang M.C. (2009) Improved p-value tests for comparing two independent binomial proportions. \emph{Communications in Statistics-Simulation and Computation}, 38(1):78-91.
\item Trujillo-Ortiz, A., R. Hernandez-Walls, A. Castro-Perez, L. Rodriguez-Cardozo N.A. Ramos-Delgado and R. Garcia-Sanchez. (2004). Barnardextest:Barnard's Exact Probability Test. A MATLAB file. [WWW document]. URL \url{http://www.mathworks.com/}
}
}
\author{ 
Kamil Erguler, Post-doctoral Fellow, EEWRC, The Cyprus Institute \email{k.erguler@cyi.ac.cy}
}
\note{ I am indebted to Peter Calhoun for helping to test the performance and the accuracy of the code. I also thank Rodrigo Duprat, Long Qu, and Nicolas Sounac for their valuable comments. The accuracy has been tested with respect to the existing MATLAB and R implementations as well as the results of StatXact. I have largely been influenced by the works of Trujillo-Ortiz etal. (2004), Cardillo G. (2009), and Galili T. (2010). }

\examples{
barnard.test(8,14,1,3)

## Plotting the search for the nuisance parameter for a one-sided test
bt<-barnard.test(8,14,1,3)
plot(bt$nuisance.matrix[,1:2],
     t="l",xlab="nuisance parameter",ylab="p-value")

## Plotting the tables included in the p-value
bt<-barnard.test(40,14,10,30)
bts<-bt$statistic.table
plot(bts[,1],bts[,2],
     col=hsv(bts[,4]/4,1,1),
     t="p",xlab="n1",ylab="n2")

## Plotting the difference between pooled and unpooled tests
bts<-barnard.test(40,14,10,30,pooled=TRUE)$statistic.table
btw<-barnard.test(40,14,10,30,pooled=FALSE)$statistic.table
plot(bts[,1],bts[,2],
     col=c("black","white")[1+as.numeric(bts[,4]==btw[,4])],
     t="p",xlab="n1",ylab="n2")
}

\keyword{nonparametric}
\keyword{htest}

