#------------------------------------------------------------------------------------------
PGA=
  function (k, mu, sigma)
  {
    structure(list(family = c("PGA", "Poisson-Gamma model (Negative Binomial)"),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
                   type = "Discrete", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 0),
                   k.valid = function(k) all(k > 0)),
              class = c("nlirms.family", "family"))
  }
#------------------------------------------------------------------------------------------
dPGA=
  function (k=1, mu=.1, sigma=2)
  {
    if (any(k < 0))
      stop(paste("k must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))

    d=(gamma(k+sigma)/(gamma(k+1)*gamma(k)))*
      ((mu/(mu+sigma))^k)*
      ((sigma/(mu+sigma))^sigma)
    d
  }
#------------------------------------------------------------------------------------------
enc.PGA=
  function (time = 2, claim = 1, mu = .1, sigma = 2)
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

    enc<-(claim+sigma)/(time+(sigma/mu))
    enc
  }
#------------------------------------------------------------------------------------------
