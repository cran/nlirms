\name{nlirms-package}
\alias{nlirms-package}
\alias{nlirms}
\docType{package}
\title{Non-Life Insurance Rate-Making System}
\description{
nlirms package design the non-life insurance rate-making system based on the posteriori frequency and severity component. The non-life insurance rate-making system is a actuarial system for pricing of non-life insurance contract. This system determinates fair rates and fair premiums for each of policyholders based on the claims frequency and severity history of policyholders in last years.
}
\details{
usually a rate-making system is designed from three way:

1-rate-making system based on the freuency component

2-rate-making system based on the severity component

3-rate-making system based on the both freuency and severity component

nlirms package returns rate-Making system based on each of the three methods.   in current version of nlirms package, five model can be applied for frequency component and also five model can be applied for severity component.
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
# rate-Making system based on the posteriori freqquency component
rmspfc(time = 5, claim = 5, fmu = .2, fsigma = 2, family = "PGA")

# rate-Making system based on the posteriori severity component
rmspsc(time=5, claim=5, sumsev=100, smu = 50, ssigma = 2, family ="EGA")

# rate-Making system based on the posteriori frequency and severity component
rmspfsc(time=5 ,claim=5, fmu = .1, fsigma = 2, sumsev=100, smu = 50, ssigma
  = 3,family = list("PGA","EGA"))
}
