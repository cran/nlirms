\name{rmspsc}
\alias{rmspsc}
\alias{rmspsc}
\title{Rate-making system based on the posteriori severity component}
\usage{
rmspsc(time=5, claim=5, sumsev=100, smu = 50, ssigma = 3, snu = 2,
family ="NO", round=2, size=8 , padlength=4, padwidth=2, ...)
}
\arguments{
\item{time}{time period to designing of rate-making system based on the posteriori freuency component}

\item{claim}{number of claims to designing of rate-making system based on the posteriori freuency component}

\item{sumsev}{sum severity of all claims to designing of rate-making system based on the posteriori freuency component}

\item{smu}{mu parameter of severity model in designing of rate-making system}

\item{ssigma}{sigma parameter of severity model in designing of rate-making system}

\item{snu}{nu parameter of severity model in designing of rate-making system}

\item{family}{a nlirms.family object, which is used to define the severity model to designing of rate-making system}

\item{round}{rounds the rate-making system values to the specified number of decimal places }

\item{size}{indicates the size of graphical table for rate-making system}

\item{padlength}{indicates the length of each graphical table cells}

\item{padwidth}{indicates the width of each graphical table cells}

\item{...}{for further arguments}
}
\value{
rmspsc() function return the expected severity of claims of policyholders based on the different models.
}
\description{
rmspsc() function gives the rate-making system based on the posteriori severity component. Values given by rmspsc() function is equal to with expected severity of claims given by esc.family (i.e. esc.EGA, esc.EIGA, esc.EGIG) for different amounts of claimS.
}
\details{
rmspfc() function gives the rate-making system in the form of a table where each table cells is related to the one claim. for example if sumsev=100, then the cell with claim=2, shows the expected severity of claims in next year for a ploicyholder that who had a two claim in past years in which the total size of two claim was equal to 100. Rate-Making system based on the severity component does not dependent to the time.
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
# rate-making system based on the Exponential-Gamma model for severity component
rmspsc(time=5, claim=5, sumsev=100, smu = 50, ssigma = 3, snu = 2, family
="EGA", round=2, size=8 , padlength=4, padwidth=2)

# rate-making system based on the Exponential-Inverse Gamma model for severity component
rmspsc(time=5, claim=5, sumsev=100, smu = 50, ssigma = 3, snu = 2, family
="EIGA", round=2, size=8 , padlength=4, padwidth=2)

# rate-making system based on the Exponential-Generalized Inverse Gaussian model for severity
rmspsc(time=5, claim=5, sumsev=100, smu = 50, ssigma = 3, snu = 2, family
="EGIG", round=2, size=8 , padlength=4, padwidth=2)
}
