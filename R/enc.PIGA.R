#----------------------------------------------------------------------------------------

PIGA=
  function (k, mu, sigma)
  {
    structure(list(family = c("PIGA", "Poisson-Inverse Gamma model"),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
                   type = "Discrete", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 1),
                   k.valid = function(k) all(k > 0)),
              class = c("nlirms.family", "family"))
  }
#----------------------------------------------------------------------------------------
dPIGA=
  function (k=1, mu=.1, sigma=2)
  {
    if (any(k < 0))
      stop(paste("k must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 1))
      stop(paste("sigma must be greater than 1 ", "\n", ""))

    d=(2*((mu*(sigma-1))^((k+sigma)/2))*besselK((4*mu*(sigma-1))^.5,k-sigma))/
      (gamma(k+1)*gamma(sigma))
    d
  }
#----------------------------------------------------------------------------------------
enc.PIGA=
  function (time = 2, claim = 1, mu = .1, sigma = 2)
  {

    if (any(time < 0))
      stop(paste("time must be >=0", "\n", ""))
    if (any(claim < 0))
      stop(paste("claim must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 1))
      stop(paste("sigma must be greater than 1 ", "\n", ""))
    if (any(time==0 && claim>0))
      stop(paste("claim must be 0 in time=0 ", "\n", ""))

    enc<-((mu*(sigma-1)/time)^.5)*
      besselK((4*time*mu*(sigma-1))^.5,claim-sigma) /
      besselK((4*time*mu*(sigma-1))^.5,claim-sigma-1)
    enc=ifelse(time==0,mu,enc)
    enc
  }
#----------------------------------------------------------------------------------------
