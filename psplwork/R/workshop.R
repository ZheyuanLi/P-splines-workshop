## [exported]
## prepare data for simple smoothing with mgcv::gam()
getAveBMI <- function (sex, replicate = FALSE) {
  ## age values
  mbmi.age <- .mbmiAve$age; fbmi.age <- .fbmiAve$age
  ## true curve
  mbmi.curve <- .mbmiAve$bmi.curve; fbmi.curve <- .fbmiAve$bmi.curve
  ## noise
  mbmi.err <- .mbmiAve$bmi.err; if (replicate) mbmi.err <- sample(mbmi.err)
  fbmi.err <- .fbmiAve$bmi.err; if (replicate) fbmi.err <- sample(fbmi.err)
  ## noisy observations (make noise larger)
  mbmi.obs <- mbmi.curve + 3 * mbmi.err; fbmi.obs <- fbmi.curve + 3 * fbmi.err
  ## prepare data
  if (sex == "M") {
    data.frame(age = mbmi.age, lage = log(mbmi.age),
               bmi = mbmi.obs, bmi.curve = mbmi.curve)
  } else if (sex == "F") {
    data.frame(age = fbmi.age, lage = log(fbmi.age),
               bmi = fbmi.obs, bmi.curve = fbmi.curve)
  } else if (sex == "A") {
    data.frame(age = c(mbmi.age, fbmi.age),
               lage = c(log(mbmi.age), log(fbmi.age)),
               bmi = c(mbmi.obs, fbmi.obs),
               bmi.curve = c(mbmi.curve, fbmi.curve),
               sex = rep.int(as.factor(c("M", "F")),
                             c(length(mbmi.age), length(fbmi.age))))
  } else {
    stop("'sex' must be 'M', 'F' or 'A'!")
  }
}

## [exported]
## prepare longitudinal data for smoothing with mgcv::gamm() or gamm4::gamm4()
getBMI100 <- function (sex) {
  if (sex == "M") {
    dat <- .mbmi100
    dat$id <- as.factor(dat$id)
    dat$lage <- log(dat$age)
  } else if (sex == "F") {
    dat <- .fbmi100
    dat$id <- as.factor(dat$id)
    dat$lage <- log(dat$age)
  } else if (sex == "A") {
    dat <- data.frame(age = c(.mbmi100$age, .fbmi100$age),
                      lage = log(c(.mbmi100$age, .fbmi100$age)),
                      bmi = c(.mbmi100$bmi, .fbmi100$bmi),
                      id = as.factor(c(.mbmi100$id, .fbmi100$id)),
                      sex = as.factor(rep.int(c("M", "F"),
                                              c(nrow(.mbmi100), nrow(.fbmi100)))))
  } else {
    stop("'sex' must be 'M', 'F' or 'A'!")
  }
  dat
}

## [exported]
## write a mgcv model formula for simple smoothing
## to be fed to mgcv::gam()
write_gam_formula <- function (log.age = TRUE, n.bs = 10,
                               penalized = TRUE, both.sex = FALSE) {
  x.var <- if (log.age) "lage" else "age"
  if (both.sex) {
    form <- sprintf("bmi ~ s(%s, by = sex, bs = 'ps', k = %d, fx = %s, m = c(2, 2))",
                    x.var, n.bs, !penalized)
  } else {
    form <- sprintf("bmi ~ s(%s, bs = 'ps', k = %d, fx = %s, m = c(2, 2))",
                    x.var, n.bs, !penalized)
  }
  form <- formula(form)
  environment(form) <- .GlobalEnv
  form
}

## [exported]
## write a mgcv model formula for smoothing longitudinal data
## to be fed to mgcv::gamm() or gamm4::gamm4()
write_gamm_formula <- function (log.age = TRUE, n.bs = 10, both.sex = FALSE) {
  ## name of the x-variable
  x.var <- if (log.age) "lage" else "age"
  ## specify model formula as a string
  if (both.sex) {
    s.pop <- sprintf("s(%s, by = sex, bs = 'ps', k = %d, m = c(2, 2))", x.var, n.bs)
  } else {
    s.pop <- sprintf("s(%s, bs = 'ps', k = %d, m = c(2, 2))", x.var, n.bs)
  }
  s.sub <- sprintf("s(%s, id, bs = 'fs', xt = 'ps', k = %d, m = c(2, 1))", x.var, n.bs)
  form <- formula(sprintf("bmi ~ %s + %s", s.pop, s.sub))
  ## convert string to formula
  environment(form) <- .GlobalEnv
  form
}
