#----------------------------------------------------------------------------------------
nlirms.family.default=
  function (object, ...)
  {
    return(switch(data.class(object)[1],
      nlirms.family = object, `function` = nlirms.family(object()),
      character = nlirms.family(get(object)), name = nlirms.family(eval(object)),
      call = nlirms.family(eval(object)),
      stop("The object argument is invalid")))
  }
#----------------------------------------------------------------------------------------
nlirms.family=
  function (object, ...)
  { UseMethod("nlirms.family")
    object
  }
#----------------------------------------------------------------------------------------
as.nlirms.family=
  function (object){
    if (inherits(object, "nlirms.family")) object else nlirms.family(object)}
#----------------------------------------------------------------------------------------
