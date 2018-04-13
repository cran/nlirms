
\name{rmspfsc}
\alias{rmspfsc}
\alias{rmspfsc}
\title{Rate-making system based on the posteriori freuency and severity component}
\usage{
rmspfsc(time=5 ,claim=5, fmu = .1, fsigma = 2, fnu = 1,
sumsev=100, smu = 50, ssigma = 3, snu = 2, family = list("NO","NO"),
round=2 ,size=8, padlength=4, padwidth=2, ...)
}
\arguments{
\item{time}{time period to designing of rate-making system based on the posteriori freuency and severity component}

\item{claim}{number of claims to designing of rate-making system based on the posteriori freuency and severity component}

\item{sumsev}{sum severity of all claims to designing of rate-making system based on the posteriori freuency and severity component}

\item{fmu}{mu parameter of frequency model in designing of rate-making system}

\item{fsigma}{sigma parameter of frequency model in designing of rate-Making system}

\item{fnu}{nu parameter of frequency model in designing of rate-making-system}

\item{smu}{mu parameter of severity model in designing of rate-making-system}

\item{ssigma}{sigma parameter of severity model in designing of rate-making system}

\item{snu}{nu parameter of severity model in designing of rate-making system}

\item{family}{a vector of nlirms.family's object, which first argument is used to define the freuncey model and second argument is used to define the severity model to designing of rate-making system based on frequency and severity component.}

\item{round}{rounds the rate-making system values to the specified number of decimal places }

\item{size}{indicates the size of graphical table for rate-making system}

\item{padlength}{indicates the length of each graphical table cells}

\item{padwidth}{indicates the width of each graphical table cells}

\item{...}{for further arguments}
}
\value{
rmspfsc() function return the expected fair premiums of policyholders based on the different models.
}
\description{
rmspfsc() function gives the rate-making system based on the posteriori frequency and severity component. Values given by rmspsc() function is equal to with expected number of claims given by enc.family (i.e. enc.PGA, enc.PIGA, enc.PGIG) multiplication in expected severity of claims given by esc.family (i.e. esc.EGA, esc.EIGA, esc.EGIG) for different amounts of time and claim.
}
\details{
rmspfsc() function gives the rate-making system in the form of a table where each table cells is related to the one time and claim. for example if sumsev=100, then the cell with time=3 claim=2, shows the pure premium in next year for a ploicyholder that who had a two claim in past three years in which the total size of two claim was equal to 100.
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
# rate-making system based on the PGA model for frequency and EGA model for severity component
rmspfsc(time=5 ,claim=5, fmu = .1, fsigma = 2, fnu = 1, sumsev=100, smu = 50,
ssigma = 3, snu = 2, family = list("PGA","EGA"),round=2 ,size=8, padlength=4,
padwidth=2)
}
