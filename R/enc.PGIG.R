#----------------------------------------------------------------------------------------

PGIG=
  function (k, mu, sigma, nu)
  {
    structure(list(family = c("PGIG", "Poisson-Generalized Inverse Gaussian model (Sichel)"),
                   parameters = list(mu = TRUE, sigma = TRUE, nu = TRUE), nopar = 3,
                   type = "Discrete", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 0),
                   k.valid = function(k) all(k > 0)),
              class = c("nlirms.family", "family"))
  }
#----------------------------------------------------------------------------------------
dPGIG=
  function (k=1, mu=.1, sigma=2, nu=1)
  {
    if (any(k < 0))
      stop(paste("k must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))

    c=besselK(1/sigma,nu+1)/besselK(1/sigma,nu)
    alpha=(1/(sigma^2))+(2*mu/(c*sigma))

    d1=((mu/c)^nu)/(factorial(k)*((alpha*sigma)^(nu+k)))
    d2= besselK(alpha, nu+k) /besselK(1/sigma, nu)
    d=d1*d2
    d
  }
#----------------------------------------------------------------------------------------
enc.PGIG=
  function (time = 2, claim = 1, mu =.1, sigma = 2,nu=1)
  {

    if (any(time < 0))
      stop(paste("time must be >=0", "\n", ""))
    if (any(claim < 0))
      stop(paste("claim must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))
    if (any(time==0 && claim>0))
      stop(paste("claim must be 0 in time=0 ", "\n", ""))

    c=besselK(1/sigma,nu+1)/besselK(1/sigma,nu)
    alpha=(1/(sigma^2))+(2*mu/(c*sigma))

    r1=mu/(c*sigma)
    r2=(c/(mu*sigma))+2*time
    enc=((r1/r2)^.5)*
      (besselK((r1*r2)^.5,nu+claim+1)/besselK((r1*r2)^.5,nu+claim))
    enc
  }
#----------------------------------------------------------------------------------------
