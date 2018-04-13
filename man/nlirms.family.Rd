
\name{nlirms.family}
\alias{nlirms.family.default}
\alias{nlirms.family}
\alias{as.nlirms.family}
\title{Family objects for designing rate-making system}
\usage{
\method{nlirms.family}{default}(object, ...)
nlirms.family(object, ...)
as.nlirms.family(object)
}
\arguments{
\item{object}{a nlirms.family object}

\item{...}{for further arguments}
}
\description{
Current available distributions that can be used to designing of rate-making system .
}
\details{
there are several freuency distributions available to designing of rate-making system as follow:

Poissin-Gamma or Negative Binomial (PGA model)

Poissin-Inverse Gamma (PIGA model)

Poissin-Generalized Inverse Gaussian or Sichel (PGIG model)

Poissin-Inverse Gaussian (PGIG model reduce to the Poissin-Inverse Gaussian model for nu=-.5)

Poissin-Harmonic (PGIG model reduce to the Poissin-Harmonic model for nu=0)


there are several severity distributions available to designing of rate-making system as follow:

Exponential-Gamma (EGA model)

Exponential-Inverse Gamma or Pareto (EIGA model)

Exponential-Generalized Inverse Gaussian (EGIG model)

Exponential-Inverse Gaussian (EGIG model reduce to the Exponential-Inverse Gaussian model for nu=-.5)

Exponential-Harmonic (EGIG model reduce to the Exponential-Harmonic model for nu=0)
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
PGA(mu , sigma)
EGA(mu , sigma)
}
