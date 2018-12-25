


broom::tidy(model2, effects = "fixed",
            conf.int = TRUE,
            conf.method = "profile")
# 
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

stdCoef.merMod(model1)
stdCoef.merMod(model2)