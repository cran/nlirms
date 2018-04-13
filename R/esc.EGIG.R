#----------------------------------------------------------------------------------------
EGIG=
  function (x, mu, sigma, nu)
  {
    structure(list(family = c("EGIG", "Exponential- Generalized Inverse Gaussian model"),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 3,
                   type = "Continues", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 0),
                   x.valid = function(x) all(x > 0)),
              class = c("nlirms.family", "family"))
  }
#----------------------------------------------------------------------------------------
dEGIG=
  function (x=100, claim=1, mu=50, sigma=2, nu=2)
  {

    if (any(x < 0))
      stop(paste("x must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 1))
      stop(paste("sigma must be greater than 1 ", "\n", ""))

    if (any(claim==0)) {x=0} else {x=x}

    c=besselK(1/sigma,nu+1)/besselK(1/sigma,nu)
    alpha=(1/(sigma^2))+(2*x*c/(mu*sigma))

    d=c*((alpha*sigma^2)^((nu-1)/2))*besselK((alpha)^.5,nu-1)/
      (mu*besselK(1/sigma,nu))
    d
  }
#----------------------------------------------------------------------------------------
esc.EGIG=
  function (sumsev=100 ,claim=1 , mu=50, sigma=2, nu=2)
  {

    if (any(sumsev < 0))
      stop(paste("sumsev must be >=0", "\n", ""))
    if (any(claim < 0))
      stop(paste("claim must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))

    sumsev=ifelse (claim==0, 0, sumsev)

    c=besselK(1/sigma,nu+1)/besselK(1/sigma,nu)

    r1=c/(sigma*mu)
    r2=(mu/(c*sigma))+2*sumsev
    esc=((r2/r1)^.5)*
      (besselK((r1*r2)^.5,nu-claim+1)/besselK((r1*r2)^.5,nu-claim))
    esc=ifelse (claim==0,mu, esc)
    esc
  }
#----------------------------------------------------------------------------------------
