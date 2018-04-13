
\name{esc.EIGA}
\alias{EIGA}
\alias{dEIGA}
\alias{esc.EIGA}
\title{Expected severity of claims based on the Exponential-Inverse Gamma (Pareto) model}
\usage{
EIGA(x, mu, sigma)
dEIGA(x= 100, claim=1, mu = 50, sigma = 2)
esc.EIGA(sumsev=100, claim =1, mu =50 , sigma = 2)
}
\arguments{
\item{x}{vector of quantiles}

\item{mu}{positive mean parameter of the Exponential-Inverse Gamma (Pareto) distribution that it wil be obtained from fitting Exponential-Inverse Gamma (Pareto) distribution to the claim severity data.}

\item{sigma}{positive scale parameter of the Exponential-Inverse Gamma (Pareto) distribution that it will be obtained from fitting Exponential-Inverse Gamma (Pareto) distribution to the claim severity data.}

\item{sumsev}{sum severity of all claims that a policyholder had in past years}

\item{claim}{total number of claims that a policyholder had in past years}
}
\value{
esc.EIGA() function return the expected severity of next claim based on the Exponential-Inverse Gamma (Pareto) model.
 dEIGA() function return the probability density of Exponential-Inverse Gamma (Pareto) distribution.
}
\description{
esc.EIGA() function gives the expected severity of next claim for a policyholder with regards to the claims severity and freuency history of this policyholder in past time, based on the Exponential-Inverse Gamma (Pareto) model.
}
\details{
Consider that x be the size of the claim of each insured and z is the mean claim size for each insured, where conditional distribution of the size given the parameter z, is distributed according to exponential(z). if z following the Inverse Gamma distribution, z~IGA(mu, sigma), with Parameterization that E(z)=mu, Then by apply the Bayes theorem the  unconditional distribution of claim size x will be exponential-Inverse Gamma (Pareto) model, EIGA~(mu, sigma), with probability density function as the following form:

      f(x)=sigma*[(mu*(sigma-1))^sigma]/[(x+mu*(sigma-1))^(sigma+1)]

let claim=k1+ ...+kt, is the total number of claims and sumsev=x1+ ...+xclaim is the total amuont of claim size where xi is the amount of claim size in the claim i, (i=1, ..., i=claim). by apply the Bayes theorem, the posterior structure function of x given the claims size history of the policyholder i.e. f(x|x1, ..., xclaim), following the Inverse Gamma distribution, IGA(claim+sigma, sumsev*mu*(sigma-1). the expected claim severity  based on the EIGA model is equal to the mean of this posteriori distribution.
}
\note{
esc.EIGA() does not dependent to time. in esc.EIGA() function mu must be grether than 0 and sigma must be grether than 1.
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
esc.EIGA(sumsev=100 ,claim=1 , mu=50, sigma=2)
claim=0:5
esc.EIGA(sumsev=100 ,claim=claim , mu=50, sigma=2)
}
