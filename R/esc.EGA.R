#----------------------------------------------------------------------------------------
EGA=
  function (x, mu, sigma)
  {
    structure(list(family = c("EGA", "Exponential- Gamma model"),
                   parameters = list(mu = TRUE, sigma = TRUE), nopar = 2,
                   type = "Continues", mu.valid = function(mu) all(mu > 0),
                   sigma.valid = function(sigma) all(sigma > 0),
                   x.valid = function(x) all(x > 0)),
              class = c("nlirms.family", "family"))
  }
#----------------------------------------------------------------------------------------
dEGA=
  function (x=100, claim=1, mu=50, sigma=2)
  {

    if (any(x < 0))
      stop(paste("x must be >=0", "\n", ""))
    if (any(mu <= 0))
      stop(paste("mu must be greater than 0 ", "\n", ""))
    if (any(sigma <= 0))
      stop(paste("sigma must be greater than 0 ", "\n", ""))

    if (any(claim==0)) {x=0} else {x=x}

    d=2*(((sigma*x)/mu)^((sigma+1)/2))*
      besselK((4*x*sigma/mu)^.5,sigma-1)/gamma(sigma)
    d
  }
#----------------------------------------------------------------------------------------
esc.EGA=
  function (sumsev=100, claim =1, mu =50 , sigma = 2)
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

    r1=2*sigma/mu
    r2=2*sumsev
    esc=((r2/r1)^.5)*
      (besselK((r1*r2)^.5,sigma-claim+1)/besselK((r1*r2)^.5,sigma-claim))
    esc=ifelse (claim==0,mu, esc)
    esc
  }
#----------------------------------------------------------------------------------------
