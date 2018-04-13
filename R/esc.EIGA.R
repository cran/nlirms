#----------------------------------------------------------------------------------------
EIGA=
  function (x, mu, sigma)
  {
    structure(list(family = c("EIGA", "Exponential-Inveres Gamma model (Pareto)"),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
                   type = "Continues", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 1),
                   x.valid = function(x) all(x > 0)),
              class = c("nlirms.family", "family"))
  }
#----------------------------------------------------------------------------------------
dEIGA=
  function (x=100, claim=1, mu=50, sigma=2)
  {

    if (any(x < 0))
      stop(paste("x must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 1))
      stop(paste("sigma must be greater than 1 ", "\n", ""))

    if (any(claim==0)) {x=0} else {x=x}

    d=sigma*((mu*(sigma-1))^sigma)/
      ((x+mu*(sigma-1))^(sigma+1))
    d
  }
#----------------------------------------------------------------------------------------
esc.EIGA=
  function (sumsev=100, claim =1, mu =50 , sigma = 2)
  {

    if (any(sumsev < 0))
      stop(paste("sumsev must be >=0", "\n", ""))
    if (any(claim < 0))
      stop(paste("claim must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 1))
      stop(paste("sigma must be greater than 1 ", "\n", ""))

    sumsev=ifelse (claim==0, 0, sumsev)

    esc=(sumsev+mu*(sigma-1))/(claim+sigma-1)
    esc
  }
#----------------------------------------------------------------------------------------
