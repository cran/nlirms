
\name{enc.PIGA}
\alias{PIGA}
\alias{dPIGA}
\alias{enc.PIGA}
\title{Expected number of claims based on the Poisson-Inverse Gamma model}
\usage{
PIGA(k, mu, sigma)
dPIGA(k = 1, mu = .1, sigma = 2)
enc.PIGA(time = 2, claim = 1, mu = .1, sigma = 2)
}
\arguments{
\item{k}{vector of (non-negative integer) quantiles.}

\item{mu}{positive mean parameter of the Poisson-Inverse Gamma distribution that it wil be obtained from fitting Poisson-Inverse Gamma distribution to the claim frequency data.}

\item{sigma}{positive scale parameter of the Poisson-Inverse Gamma distribution that it will be obtained from fitting Poisson-Inverse Gamma distribution to the claim frequency data.}

\item{time}{time period to claims freuency rate-making.}

\item{claim}{total number of claims that a policyholder had in past years.}
}

\value{
enc.PIGA() function return the expected number of claims based on the Poisson-Inverse Gamma model.
 dPIGA() function return the probability density of Poisson-Inverse Gamma distribution.
}
\description{
enc.PIGA() function gives the expected number of claims for a policyholder in the next time (for example in next year) with regards to the number of claims history of this policyholder in past time, based on the Poisson-Inverse Gamma model.
}
\details{
Consider that the number of claims k, (k=0,1,...), given the parameter y, is distributed according to Poisson(y), where y is denoting the different underlyin risk of each policyholder to have an accident. if y following the Inverse Gamma distribution, y~IGA(mu, sigma), with Parameterization that E(y)=mu, Then by apply the Bayes theorem the  unconditional distribution of the number of claims k will be Poisson-Inverse Gamma distribution, PIGA~(mu, sigma), with probability density function as the following form:

              f(y)=2*alpha^[(k+sigma)/2]*besselK((4*alpha)^.5,k-sigma)/
                              [gamma(k+1)*gamma(sigma]

where alpha=mu*(sigma-1).let claim=k1+ ...+kt, is total number of claims that a policyholder had in t years, where ki is the number of claims that the policyholder had in the year i, (i=1, ..., t=time). by apply the Bayes theorem, the posterior structure function of y i.e. f(y|k1, ..., kt), for a policyholder with claim history k1,..., kt, following the Generalized Inverse Gaussian distribution, GIG(2*time, 2*mu*(sigma-1), claim-sigma-1). the expected number of claims based on the PIGA model is equal to the mean of this posteriori distribution.
}
\note{
in enc.PIGA() function mu must be grether than 0 and sigma must be grether than 1.
}
\author{
Saeed Mohammadpour (s.mohammadpour1111@gmail.com), Soodabeh Mohammadpoor Golojeh (s.mohammadpour@gmail.com)
}
\references{
Frangos, N. E., & Vrontos, S. D. (2001). Design of optimal bonus-malus systems with a frequency and a severity component on an individual basis in automobile insurance. ASTIN Bulletin: The Journal of the IAA, 31(1), 1-22.

Lemaire, J. (1995) Bonus-Malus Systems in Automobile Insurance, Kluwer Academic Publishers, Massachusetts.

MohammadPour, S., Saeedi, K., & Mahmoudvand, R. (2017). Bonus-Malus System Using Finite Mixture Models. Statistics, Optimization & Information Computing, 5(3), 179-187.

Najafabadi, A. T. P., & MohammadPour, S. (2017). A k-Inflated Negative Binomial Mixture Regression Model: Application to Rate--Making Systems. Asia-Pacific Journal of Risk and Insurance, 12.

Rigby, R. A., & Stasinopoulos, D. M. (2005). Generalized additive models for location, scale and shape. Journal of the Royal Statistical Society: Series C (Applied Statistics), 54(3), 507-554.

Stasinopoulos, D. M., Rigby, B. A., Akantziliotou, C., Heller, G., Ospina, R., & Motpan, N. (2010). gamlss. dist: Distributions to Be Used for GAMLSS Modelling. R package version, 4-0.

Stasinopoulos, D. M., & Rigby, R. A. (2007). Generalized additive models for location scale and shape (GAMLSS) in R. Journal of Statistical Software, 23(7), 1-46.
}
\examples{
dPIGA(k=1, mu=.1, sigma=2)
enc.PIGA(time = 2, claim = 1, mu = .1, sigma = 2)
time=1:5
enc.PIGA(time = time, claim = 1, mu = .1, sigma = 2)
}


