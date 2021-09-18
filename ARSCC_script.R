# This is the ARSCC script file
#
# retrieving data
{
  # full data
  arscc_data <- read.csv(file="arscc_data.csv", header=TRUE, sep=",")
  #
  # currency crises only
  arscc_cur <- read.csv(file="arscc_cur.csv", header=TRUE, sep=",")
}
#
# preliminary checks 
{
# checking class bias
  table(arscc_data$regime_change)    # regime changes
  table(arscc_data$currency)         # currency crises
  table(arscc_cur$regime_change)     # regime changes during currency crises
}
#
# descriptive statistics
{
  library(psych)
  #
  # full data
  des_arscc_data <- arscc_data[- c(1, 2, 3, 4, 5, 9, 10, 14, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 37, 38, 40, 41, 42, 44, 45, 46, 48, 50, 52, 54, 56, 57, 58, 59, 60, 61, 62, 64, 66, 68, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87)]
  names(des_arscc_data) <- c("duration", "regime_change", "prevrc", "party", "military", "personal", "currency", "twin", "ers", "d_ers", "mi", "d_mi", "kaopen", "d_kaopen", "ers_mi", "mi_kaopen", "ers_kaopen", "gdppc", "ln_gdppc", "gdppcgr", "oil_gas_value_pc", "oilgas", "polity2", "polity2_avg", "fedrate", "europe", "latam", "mideast", "africa", "eastasia", "southasia")
  des_arscc_data = describe(des_arscc_data, fast=TRUE, omit=TRUE)
  des_arscc_data <- des_arscc_data[c(2, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 4, 5, 6, 1, 3, 25, 26, 27, 28, 29, 30, 31), -c(1, 7, 8)]
  #
  # currency crises
  des_arscc_cur <- arscc_cur[- c(1, 2, 3, 4, 5, 8, 10, 11, 15, 17, 18, 27, 29, 31, 33, 34, 35, 37, 38, 39, 40, 41, 42, 44, 45, 47, 48, 49, 50, 51, 52, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75)]
  des_arscc_cur = describe(des_arscc_cur, fast=TRUE, omit=TRUE)
  des_arscc_cur <- des_arscc_cur[c(2, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 4, 5, 6, 1, 3, 24, 25, 26, 27, 28, 29, 30), -c(1, 7, 8)]
  #
  # creating tables 4 and 5
  library(stargazer)
  stargazer(des_arscc_data, title="Table 4. Descriptive statistics for full dataset", summary=FALSE, type="html", out="arscc_table4.doc")
  stargazer(des_arscc_cur, title="Table 5. Descriptive statistics for 'currency crises' data", summary=FALSE, type="html", out="arscc_table5.doc")
  #
}
#
# PART 1: Testing hypotheses on the entire population of autocratic country-years
#
# step 1a: Crisis policy choices and autocratic regime change (individual indexes)
{
  library("stats")
  library("pglm")
  #
  # model 11: RE probit - change in capital account openness
  model11 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_kaopen + d_kaopen*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 12: RE probit - change in monetary independence
  model12 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_mi + d_mi*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 13: RE probit - change in exchange rate stability
  model13 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers + d_ers*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 14: RE probit - model with crisis policy indexes
  # model14 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_kaopen + d_kaopen*currency + d_mi + d_mi*currency + d_ers + d_ers*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 15: RE probit - capital account openness
  model15 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + kaopen + kaopen*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 16: RE probit - monetary independence
  model16 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + mi + mi*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 17: RE probit - exchange rate stability
  model17 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers + ers*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 18: RE probit - model with individual trilemma indexes
  # model18 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + kaopen + kaopen*currency + mi + mi*currency + ers*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  #
  # model diagnostics
  {
    # checking for multicollinearity
    # library(car)
    # lapply(list(model14, model18), car::vif)
  }
  #
}
#
# step 1b: Macroeconomic policy orientation and autocratic regime survival (composite indexes)
{
  #
  # model 21: RE probit - ers_mi (policy orientation of financially closed economy)
  model21 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers_mi + ers_mi*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 22: RE probit - mi_kaopen (policy orientation of floating exchange rate regime)
  model22 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + mi_kaopen + mi_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 23: RE probit - ers_kaopen (policy orientation of fixed exchange rate regime)
  model23 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers_kaopen + ers_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 24: RE probit - macroeconomic policy orientation
  # model24 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers_mi + ers_mi*currency + mi_kaopen + mi_kaopen*currency + ers_kaopen + ers_kaopen*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 25: RE probit - d_ers_mi (change in policy orientation of financially closed economy)
  model25 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers_mi + d_ers_mi*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  # model 26: RE probit - d_mi_kaopen (change in policy orientation of floating exchange rate regime)
  model26 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_mi_kaopen + d_mi_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 27: RE probit - d_ers_kaopen (change in policy orientation of fixed exchange rate regime)
  model27 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers_kaopen + d_ers_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  # model 28: RE probit - crisis macroeconomic policy orientation
  # model28 <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers_mi + d_ers_mi*currency + d_mi_kaopen + d_mi_kaopen*currency + d_ers_kaopen + d_ers_kaopen*currency, model="random", family=binomial(link="probit"), index=c("ccode", "year"), data=arscc_data)
  #
  # model diagnostics
  {
   # checking for multicollinearity
    # library(car)
    # lapply(list(model24, model28), car::vif)
  }
  #
}
#
# creating tables 1 and 6
{
  # functions for table creation
  {
  library(texreg)
  extract.pglm <- function (model, include.nobs = TRUE, include.loglik = TRUE, ...) {
    s <- summary(model, ...)
    coefficient.names <- rownames(s$estimate)
    coefficients <- s$estimate[, 1]
    standard.errors <- s$estimate[, 2]
    significance <- s$estimate[, 4]
    loglik.value <- s$loglik
    n <- nrow(model$model)
    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.loglik == TRUE) {
      gof <- c(gof, loglik.value)
      gof.names <- c(gof.names, "Log-Likelihood")
      gof.decimal <- c(gof.decimal, TRUE)
    }
    if (include.nobs == TRUE) {
      gof <- c(gof, n)
      gof.names <- c(gof.names, "Num. obs.")
      gof.decimal <- c(gof.decimal, FALSE)
    }
    tr <- createTexreg(coef.names = coefficient.names, coef = coefficients, 
                       se = standard.errors, pvalues = significance, gof.names = gof.names, 
                       gof = gof, gof.decimal = gof.decimal)
    return(tr)
  }
  setMethod("extract", signature = className("maxLik", "maxLik"), definition = extract.pglm)
  #
  }
  # creating table 1
  library("texreg")
  htmlreg(list(model11, model12, model13, model21, model22, model23), file="arscc_table1.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), caption.above=TRUE, caption="<b>Table 1. Trilemma policy choices and autocratic regime survival during currency crises: main models</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
  #
  # creating table 6  
  htmlreg(list(model15, model16, model17, model25, model26, model27), file="arscc_table6.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23), caption.above=TRUE, caption="<b>Table 6. Trilemma policy orientation and autocratic regime survival during currency crises: extra models</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
  #
}
#
# step 1c: Extra models: pooled probite & RE probit with regional dummies
{
  #
  # Pooled probit models 
  #
  model51a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_kaopen + d_kaopen*currency, family=binomial("probit"), data=arscc_data)
  model51b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + kaopen + kaopen*currency, family=binomial("probit"), data=arscc_data)
  model52a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_mi + d_mi*currency, family=binomial("probit"), data=arscc_data)
  model52b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + mi + mi*currency, family=binomial("probit"), data=arscc_data)
  model53a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers + d_ers*currency, family=binomial("probit"), data=arscc_data)
  model53b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers + ers*currency, family=binomial("probit"), data=arscc_data)
  model54a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers_mi + d_ers_mi*currency, family=binomial("probit"), data=arscc_data)
  model54b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers_mi + ers_mi*currency, family=binomial("probit"), data=arscc_data)
  model55a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_mi_kaopen + d_mi_kaopen*currency, family=binomial("probit"), data=arscc_data)
  model55b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + mi_kaopen + mi_kaopen*currency, family=binomial("probit"), data=arscc_data)
  model56a <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + d_ers_kaopen + d_ers_kaopen*currency, family=binomial("probit"), data=arscc_data)
  model56b <- glm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + currency + ers_kaopen + ers_kaopen*currency, family=binomial("probit"), data=arscc_data)
  #
  # Random effects models with regional dummies
  #
  model61a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_kaopen + d_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model61b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + kaopen + kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model62a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_mi + d_mi*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model62b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + mi + mi*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model63a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_ers + d_ers*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model63b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + ers + ers*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model64a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_ers_mi + d_ers_mi*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model64b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + ers_mi + ers_mi*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model65a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_mi_kaopen + d_mi_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model65b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + mi_kaopen + mi_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model66a <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + d_ers_kaopen + d_ers_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  model66b <- pglm (regime_change ~ ln_gdppc + gdppcgr + oilgas + prevrc + polity2_avg + party + military + personal + latam + mideast + africa + eastasia + southasia + currency + ers_kaopen + ers_kaopen*currency, model="random", family=binomial("probit"), index=c("ccode", "year"), data=arscc_data)
  #
  # creating tables 1a, 1b, 6a, 6b
  {
    library("texreg")
    htmlreg(list(model51a, model52a, model53a, model54b, model55b, model56b), file="arscc_table1b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), caption.above=TRUE, caption="<b>Table 1b. Trilemma policy choices and autocratic regime survival during currency crises (main models): Probit, pooled</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
    htmlreg(list(model61a, model62a, model63a, model64b, model65b, model66b), file="arscc_table1a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 18, 15, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28), caption.above=TRUE, caption="<b>Table 1a. Trilemma policy choices and autocratic regime survival during currency crises (main models): Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
    htmlreg(list(model51b, model52b, model53b, model54a, model55a, model56a), file="arscc_table6b.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22), caption.above=TRUE, caption="<b>Table 6b. Trilemma policy choices and autocratic regime survival during currency crises (extra models): Probit, pooled</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
    htmlreg(list(model61b, model62b, model63b, model64a, model65a, model66a), file="arscc_table6a.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), reorder.coef=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 18, 15, 16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28), caption.above=TRUE, caption="<b>Table 6a. Trilemma policy choices and autocratic regime survival during currency crises (extra models): Probit, RE</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
   }
#
}
#
#
# PART 2: Testing hypotheses on the subset of 'currency crises' only
#
# probit models of autocratic regime breakdown during currency crises
{
  # model 31: change in capital account openness
  model31 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_kaopen, family=binomial(link='probit'), data=arscc_cur)
  # model 32: change in monetary independence
  model32 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_mi, family=binomial(link='probit'), data=arscc_cur)
  # model 33: change in exchange rate stability
  model33 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_ers, family=binomial(link='probit'), data=arscc_cur)
  # model 34: onset policy orientation of 'financially closed economy'
  model34 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + ers_mi_onset, family=binomial(link='probit'), data=arscc_cur)
  # model 35: onset policy orientation of 'floating exchange rate'
  model35 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + mi_kaopen_onset, family=binomial(link='probit'), data=arscc_cur)
  # model 36: onset policy orientation of 'fixed exchange rate / currency board'
  model36 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + ers_kaopen_onset, family=binomial(link='probit'), data=arscc_cur)
  #
  # model 71: capital account openness onset
  model71 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + kaopen_onset, family=binomial(link='probit'), data=arscc_cur)
  # model 72: monetary independence onset
  model72 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + mi_onset, family=binomial(link='probit'), data=arscc_cur)
  # model 73: exchange rate stability onset
  model73 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + ers_onset, family=binomial(link='probit'), data=arscc_cur)
  # model 74: changes in policy orientation of 'financially closed economy'
  model74 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_ers_mi, family=binomial(link='probit'), data=arscc_cur)
  # model 75: changes in policy orientation of 'floating exchange rate'
  model75 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_mi_kaopen, family=binomial(link='probit'), data=arscc_cur)
  # model 76: changes in policy orientation of 'fixed exchange rate / currency board'
  model76 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + oilgas + prevrc + polity2_avg + party + military + personal + twin + d_ers_kaopen, family=binomial(link='probit'), data=arscc_cur)
  #
}
#
# creating tables 2 and 7
{
  library("texreg")
  htmlreg(list(model31, model32, model33, model34, model35, model36), file="arscc_table2.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), caption.above=TRUE, caption="<b>Table 2. Determinants of autocratic regime survival during currency crises</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
  htmlreg(list(model71, model72, model73, model74, model75, model76), file="arscc_table7.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"), caption.above=TRUE, caption="<b>Table 7. Determinants of autocratic regime survival during currency crises: extra models</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
}
#
# model diagnostics for step 2
{
# some extra checks
if(FALSE){
  #
  # jackknife estimation
  if (FALSE) {
    library(bootstrap)
    theta <- function(i, x, dat, coefficient){ coef(glm(i, data = dat[x,], family=binomial(link='probit')))[coefficient] }
    for (y in 1:9) {
      z=paste0("model",y)
      i=get(z)
      w=c('d_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_kaopen', 'kaopen_onset', 'd_ers', 'ers_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_ers_mi', 'ers_mi_onset', 'd_mi_kaopen', 'mi_kaopen_onset', 'd_ers_kaopen', 'ers_kaopen_onset')
      if (y>=1 & y<=3) { 
        a=getElement(cur_data, w[2*y-1])
        b=getElement(cur_data, w[2*y])
        v=paste0("res",y,"1")
        assign(v, jackknife(1:length(a), theta, i=i, dat=cur_data, coefficient=w[2*y-1]))
        v=paste0("res",y,"2")
        assign(v, jackknife(1:length(b), theta, i=i, dat=cur_data, coefficient=w[2*y]))
      } 
      else if (y>=4 & y<=6) {
        a=getElement(cur_data, w[4*y-9])
        b=getElement(cur_data, w[4*y-8])
        c=getElement(cur_data, w[4*y-7])
        d=getElement(cur_data, w[4*y-6])
        v=paste0("res",y,"1")
        assign(v, jackknife(1:length(a), theta, i=i, dat=cur_data, coefficient=w[4*y-9]))
        v=paste0("res",y,"2")
        assign(v, jackknife(1:length(b), theta, i=i, dat=cur_data, coefficient=w[4*y-8]))
        v=paste0("res",y,"3")
        assign(v, jackknife(1:length(c), theta, i=i, dat=cur_data, coefficient=w[4*y-7]))
        v=paste0("res",y,"4")
        assign(v, jackknife(1:length(d), theta, i=i, dat=cur_data, coefficient=w[4*y-6]))
      } 
      else {
        a=getElement(cur_data, w[2*y+5])
        b=getElement(cur_data, w[2*y+6])
        v=paste0("res",y,"1")
        assign(v, jackknife(1:length(a), theta, i=i, dat=cur_data, coefficient=w[2*y+5]))
        v=paste0("res",y,"2")
        assign(v, jackknife(1:length(b), theta, i=i, dat=cur_data, coefficient=w[2*y+6]))
      }
    }
    #
    # creating data frame with results (only independent variables are included)
    #
    model_names <- c ('model 1: d_kaopen', 'model 1: kaopen_onset', 'model 2: d_mi', 'model 2: mi_onset', 'model 3: d_ers', 'model 3: ers_onset', 'model 4: d_kaopen', 'model 4: kaopen_onset', 'model 4: d_mi', 'model 4: mi_onset', 'model 5: d_kaopen', 'model 5: kaopen_onset', 'model 5: d_ers', 'model 5: ers_onset', 'model 6: d_mi', 'model 6: mi_onset', 'model 6: d_ers', 'model 6: ers_onset', 'model 7: d_ers_mi', 'model 7: ers_mi_onset', 'model 8: d_mi_kaopen', 'model 8: mi_kaopen_onset', 'model 9: d_ers_kaopen', 'model 9: ers_kaopen_onset')
    estimate <- c (as.numeric(model1$coefficients['d_kaopen']), as.numeric(model1$coefficients['kaopen_onset']), as.numeric(model2$coefficients['d_mi']), as.numeric(model2$coefficients['mi_onset']), as.numeric(model3$coefficients['d_ers']), as.numeric(model3$coefficients['ers_onset']), as.numeric(model4$coefficients['d_kaopen']), as.numeric(model4$coefficients['kaopen_onset']), as.numeric(model4$coefficients['d_mi']), as.numeric(model4$coefficients['mi_onset']), as.numeric(model5$coefficients['d_kaopen']), as.numeric(model5$coefficients['kaopen_onset']), as.numeric(model5$coefficients['d_ers']), as.numeric(model5$coefficients['ers_onset']), as.numeric(model6$coefficients['d_mi']), as.numeric(model6$coefficients['mi_onset']), as.numeric(model6$coefficients['d_ers']), as.numeric(model6$coefficients['ers_onset']), as.numeric(model7$coefficients['d_ers_mi']), as.numeric(model7$coefficients['ers_mi_onset']), as.numeric(model8$coefficients['d_mi_kaopen']), as.numeric(model8$coefficients['mi_kaopen_onset']), as.numeric(model9$coefficients['d_ers_kaopen']), as.numeric(model9$coefficients['ers_kaopen_onset']))
    st_error <- c (summary(model1)$coefficients['d_kaopen', 2], summary(model1)$coefficients['kaopen_onset', 2], summary(model2)$coefficients['d_mi', 2], summary(model2)$coefficients['mi_onset', 2], summary(model3)$coefficients['d_ers', 2], summary(model3)$coefficients['ers_onset', 2], summary(model4)$coefficients['d_kaopen', 2], summary(model4)$coefficients['kaopen_onset', 2], summary(model4)$coefficients['d_mi', 2], summary(model4)$coefficients['mi_onset', 2], summary(model5)$coefficients['d_kaopen', 2], summary(model5)$coefficients['kaopen_onset', 2], summary(model5)$coefficients['d_ers', 2], summary(model5)$coefficients['ers_onset', 2], summary(model6)$coefficients['d_mi', 2], summary(model6)$coefficients['mi_onset', 2], summary(model6)$coefficients['d_ers', 2], summary(model6)$coefficients['ers_onset', 2], summary(model7)$coefficients['d_ers_mi', 2], summary(model7)$coefficients['ers_mi_onset', 2], summary(model8)$coefficients['d_mi_kaopen', 2], summary(model8)$coefficients['mi_kaopen_onset', 2], summary(model9)$coefficients['d_ers_kaopen', 2], summary(model9)$coefficients['ers_kaopen_onset', 2])
    jack_mean <- c (mean(res11$jack.values), mean(res12$jack.values), mean(res21$jack.values), mean(res22$jack.values), mean(res31$jack.values), mean(res32$jack.values), mean(res41$jack.values), mean(res42$jack.values), mean(res43$jack.values), mean(res44$jack.values), mean(res51$jack.values), mean(res52$jack.values), mean(res53$jack.values), mean(res54$jack.values), mean(res61$jack.values), mean(res62$jack.values), mean(res63$jack.values), mean(res64$jack.values), mean(res71$jack.values), mean(res72$jack.values), mean(res81$jack.values), mean(res82$jack.values), mean(res91$jack.values), mean(res92$jack.values))
    jack_max <- c (max(res11$jack.values), max(res12$jack.values), max(res21$jack.values), max(res22$jack.values), max(res31$jack.values), max(res32$jack.values), max(res41$jack.values), max(res42$jack.values), max(res43$jack.values), max(res44$jack.values), max(res51$jack.values), max(res52$jack.values), max(res53$jack.values), max(res54$jack.values), max(res61$jack.values), max(res62$jack.values), max(res63$jack.values), max(res64$jack.values), max(res71$jack.values), max(res72$jack.values), max(res81$jack.values), max(res82$jack.values), max(res91$jack.values), max(res92$jack.values))
    jack_min <- c (min(res11$jack.values), min(res12$jack.values), min(res21$jack.values), min(res22$jack.values), min(res31$jack.values), min(res32$jack.values), min(res41$jack.values), min(res42$jack.values), min(res43$jack.values), min(res44$jack.values), min(res51$jack.values), min(res52$jack.values), min(res53$jack.values), min(res54$jack.values), min(res61$jack.values), min(res62$jack.values), min(res63$jack.values), min(res64$jack.values), min(res71$jack.values), min(res72$jack.values), min(res81$jack.values), min(res82$jack.values), min(res91$jack.values), min(res92$jack.values))
    jack_st_error <- c(res11$jack.se, res12$jack.se, res21$jack.se, res22$jack.se, res31$jack.se, res32$jack.se, res41$jack.se, res42$jack.se, res43$jack.se, res44$jack.se, res51$jack.se, res52$jack.se, res53$jack.se, res54$jack.se, res61$jack.se, res62$jack.se, res63$jack.se, res64$jack.se, res71$jack.se, res72$jack.se, res81$jack.se, res82$jack.se, res91$jack.se, res92$jack.se)
    jack_bias <- c(as.numeric(res11$jack.bias), as.numeric(res12$jack.bias), as.numeric(res21$jack.bias), as.numeric(res22$jack.bias), as.numeric(res31$jack.bias), as.numeric(res32$jack.bias), as.numeric(res41$jack.bias), as.numeric(res42$jack.bias), as.numeric(res43$jack.bias), as.numeric(res44$jack.bias), as.numeric(res51$jack.bias), as.numeric(res52$jack.bias), as.numeric(res53$jack.bias), as.numeric(res54$jack.bias), as.numeric(res61$jack.bias), as.numeric(res62$jack.bias), as.numeric(res63$jack.bias), as.numeric(res64$jack.bias), as.numeric(res71$jack.bias), as.numeric(res72$jack.bias), as.numeric(res81$jack.bias), as.numeric(res82$jack.bias), as.numeric(res91$jack.bias), as.numeric(res92$jack.bias))
    jack_results <- data.frame(model_names, estimate, st_error, jack_mean, jack_max, jack_min, jack_st_error, jack_bias)
    #
    # library(knitr)
    # kable(jack_results, caption="Jackknife estimation results", format="html)
    #
    # creating table X
    stargazer(jack_results, title="Table X. Jackknife estimation results for autocratic regime breakdown during crises", rownames=FALSE, summary=FALSE, type="html", out="arscc_jack.doc")
  }
  #
  # checking for influential observations
  if (FALSE) {
    library(ggplot2)
    outliers_plot <- function(i,y,v) { 
      x=residuals(i, type="partial")
      assign(v, qplot(x, bins=30, main = paste0("Model ", y), xlab="residuals", ylab="count") + theme_bw(), envir = globalenv())
    # print(qplot(x, bins=30, main = paste0("Outliers: model ", y), xlab="residuals", ylab="count") + theme_bw())
    # Sys.sleep(1)
    }
    outliers_which <- function(i,x,dat) {
      missing=is.na(x)
      temp.data=subset(dat, !missing)
    # print(nrow(temp.data))
      assign("outlier", which(residuals(i, type="partial")==max(residuals(i, type="partial")), arr.ind=TRUE), envir=globalenv())
    # print(temp.data[outlier[1],c('cname', 'year')])
    # Sys.sleep(1)
    }
    for (y in 1:9) {
    # w=c("d_kaopen", "d_mi", "d_ers", "d_mi_kaopen", "d_ers_kaopen", "d_ers_mi", "d_ers_mi", "d_mi_kaopen", "d_ers_kaopen")
      w=c('d_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_kaopen', 'kaopen_onset', 'd_ers', 'ers_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_ers_mi', 'ers_mi_onset', 'd_mi_kaopen', 'mi_kaopen_onset', 'd_ers_kaopen', 'ers_kaopen_onset')
      z=paste0("model",y)
      i=get(z)
      v=paste0("plot",y)
      outliers_plot(i,y,v)
      if (y>=1 & y<=3) {
      # print(z)
        outliers_which(i,getElement(cur_data, w[2*y-1]),cur_data)
        outliers_which(i,getElement(cur_data, w[2*y]),cur_data)
      } 
      else if (y>=4 & y<=6) {
      # print(z)
        outliers_which(i,getElement(cur_data, w[4*y-9]),cur_data)
        outliers_which(i,getElement(cur_data, w[4*y-8]),cur_data)
        outliers_which(i,getElement(cur_data, w[4*y-7]),cur_data)
        outliers_which(i,getElement(cur_data, w[4*y-6]),cur_data)
      } 
      else {
      # print(z)
        outliers_which(i,getElement(cur_data, w[2*y+5]),cur_data)
        outliers_which(i,getElement(cur_data, w[2*y+6]),cur_data)
      }
    } 
    library(gridExtra)
    # gridExtra::grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3)
    ggsave("paper2_outliers_plot.png", gridExtra::arrangeGrob(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, ncol=3, top="Figure 3. Checking for influential outliers"))
    #
    # removing outliers for models where jackknife bias is large
    #
    remove_function <- function(y,k,n) {
      v=paste0("res",y,k)
    # print(v)
      x=get(v)
      t=get(v)
      m=Mod(as.numeric(x$jack.bias))
      temp.cur_data <- cur_data
      ro=paste0("jack_res",y,k)
      assign(ro, jackknife(1:length(a), theta, i=i, dat=temp.cur_data, coefficient=w[n]), envir=globalenv())
      while (Mod(as.numeric(t$jack.bias))>=m) {
        outliers_which(i,getElement(temp.cur_data,w[n]),temp.cur_data)
        temp.cur_data <- temp.cur_data[-c(outlier[1]),]
        a=getElement(temp.cur_data, w[n])
        vv=paste0("temp")
        assign(vv, jackknife(1:length(a), theta, i=i, dat=temp.cur_data, coefficient=w[n]), envir=globalenv())
        x=get(vv)
        if (Mod(as.numeric(x$jack.bias))<m) {
          t=get(vv)
          ro=paste0("jack_res",y,k)
          assign(ro, jackknife(1:length(a), theta, i=i, dat=temp.cur_data, coefficient=w[n]), envir=globalenv())
        }
       m=Mod(as.numeric(x$jack.bias))
      # print (as.numeric(x$jack.bias))
      # print (as.numeric(t$jack.bias))
      }
    }
    for (y in 1:9) {
      z=paste0("model",y)
      i=get(z)
      w=c('d_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_kaopen', 'kaopen_onset', 'd_mi', 'mi_onset', 'd_kaopen', 'kaopen_onset', 'd_ers', 'ers_onset', 'd_mi', 'mi_onset', 'd_ers', 'ers_onset', 'd_ers_mi', 'ers_mi_onset', 'd_mi_kaopen', 'mi_kaopen_onset', 'd_ers_kaopen', 'ers_kaopen_onset')
      if (y>=1 & y<=3) {
        remove_function(y,1,2*y-1)
        remove_function(y,2,2*y)
      } 
      else if (y>=4 & y<=6) {
        remove_function(y,1,4*y-9)
        remove_function(y,2,4*y-8)
        remove_function(y,3,4*y-7)
        remove_function(y,4,4*y-6)
      } 
      else if (y>=7 & y<=9) {
        remove_function(y,1,2*y+5)
        remove_function(y,2,2*y+6)
      }
    }
    # creating data frame with new results (after removing outliers)
    jack_mean_2 <- c (mean(jack_res11$jack.values), mean(jack_res12$jack.values), mean(jack_res21$jack.values), mean(jack_res22$jack.values), mean(jack_res31$jack.values), mean(jack_res32$jack.values), mean(jack_res41$jack.values), mean(jack_res42$jack.values), mean(jack_res43$jack.values), mean(jack_res44$jack.values), mean(jack_res51$jack.values), mean(jack_res52$jack.values), mean(jack_res53$jack.values), mean(jack_res54$jack.values), mean(jack_res61$jack.values), mean(jack_res62$jack.values), mean(jack_res63$jack.values), mean(jack_res64$jack.values), mean(jack_res71$jack.values), mean(jack_res72$jack.values), mean(jack_res81$jack.values), mean(jack_res82$jack.values), mean(jack_res91$jack.values), mean(jack_res92$jack.values))
    jack_max_2 <- c (max(jack_res11$jack.values), max(jack_res12$jack.values), max(jack_res21$jack.values), max(jack_res22$jack.values), max(jack_res31$jack.values), max(jack_res32$jack.values), max(jack_res41$jack.values), max(jack_res42$jack.values), max(jack_res43$jack.values), max(jack_res44$jack.values), max(jack_res51$jack.values), max(jack_res52$jack.values), max(jack_res53$jack.values), max(jack_res54$jack.values), max(jack_res61$jack.values), max(jack_res62$jack.values), max(jack_res63$jack.values), max(jack_res64$jack.values), max(jack_res71$jack.values), max(jack_res72$jack.values), max(jack_res81$jack.values), max(jack_res82$jack.values), max(jack_res91$jack.values), max(jack_res92$jack.values))
    jack_min_2 <- c (min(jack_res11$jack.values), min(jack_res12$jack.values), min(jack_res21$jack.values), min(jack_res22$jack.values), min(jack_res31$jack.values), min(jack_res32$jack.values), min(jack_res41$jack.values), min(jack_res42$jack.values), min(jack_res43$jack.values), min(jack_res44$jack.values), min(jack_res51$jack.values), min(jack_res52$jack.values), min(jack_res53$jack.values), min(jack_res54$jack.values), min(jack_res61$jack.values), min(jack_res62$jack.values), min(jack_res63$jack.values), min(jack_res64$jack.values), min(jack_res71$jack.values), min(jack_res72$jack.values), min(jack_res81$jack.values), min(jack_res82$jack.values), min(jack_res91$jack.values), min(jack_res92$jack.values))
    jack_st_error_2 <- c(jack_res11$jack.se, jack_res12$jack.se, jack_res21$jack.se, jack_res22$jack.se, jack_res31$jack.se, jack_res32$jack.se, jack_res41$jack.se, jack_res42$jack.se, jack_res43$jack.se, jack_res44$jack.se, jack_res51$jack.se, jack_res52$jack.se, jack_res53$jack.se, jack_res54$jack.se, jack_res61$jack.se, jack_res62$jack.se, jack_res63$jack.se, jack_res64$jack.se, jack_res71$jack.se, jack_res72$jack.se, jack_res81$jack.se, jack_res82$jack.se, jack_res91$jack.se, jack_res92$jack.se)
    jack_bias_2 <- c(as.numeric(jack_res11$jack.bias), as.numeric(jack_res12$jack.bias), as.numeric(jack_res21$jack.bias), as.numeric(jack_res22$jack.bias), as.numeric(jack_res31$jack.bias), as.numeric(jack_res32$jack.bias), as.numeric(jack_res41$jack.bias), as.numeric(jack_res42$jack.bias), as.numeric(jack_res43$jack.bias), as.numeric(jack_res44$jack.bias), as.numeric(jack_res51$jack.bias), as.numeric(jack_res52$jack.bias), as.numeric(jack_res53$jack.bias), as.numeric(jack_res54$jack.bias), as.numeric(jack_res61$jack.bias), as.numeric(jack_res62$jack.bias), as.numeric(jack_res63$jack.bias), as.numeric(jack_res64$jack.bias), as.numeric(jack_res71$jack.bias), as.numeric(jack_res72$jack.bias), as.numeric(jack_res81$jack.bias), as.numeric(jack_res82$jack.bias), as.numeric(jack_res91$jack.bias), as.numeric(jack_res92$jack.bias))
    jack_results_2 <- data.frame(model_names, estimate, st_error, jack_mean_2, jack_max_2, jack_min_2, jack_st_error_2, jack_bias_2)
    #
    # creating table X
    # stargazer(jack_results_2, title="Table X. Jackknife estimation results after removing outliers", rownames=FALSE, summary=FALSE, type="html", out="paper2_tableX.doc")
    #
  }
}
#
# bootstrap estimation
{
  library("boot")
  bootstrap <- function(formula, data, regressors) {
    dat <- data[regressors,]	
    reg <- glm(formula, data = dat, family=binomial(link='probit')) 
    return(coef(reg)) 
  }
  for (y in 1:6) {
    z=paste0("model3",y)
    w=get(z)
    v=paste0("bs.res",y)
    assign (v, boot(formula=w$formula, data=arscc_cur, statistic = bootstrap, R=1000))
  }
  #
  # creating data frame with bootstrap results (only independent variables are included)
  model_names <- c ('model 1: d_kaopen', 'model 2: d_mi', 'model 3: d_ers', 'model 4: ers_mi_onset', 'model 5: mi_kaopen_onset', 'model 6: ers_kaopen_onset')
  estimate <- c (as.numeric(model31$coefficients['d_kaopen']), as.numeric(model32$coefficients['d_mi']), as.numeric(model33$coefficients['d_ers']), as.numeric(model34$coefficients['ers_mi_onset']), as.numeric(model35$coefficients['mi_kaopen_onset']), as.numeric(model36$coefficients['ers_kaopen_onset']))
  st_error <- c (summary(model31)$coefficients['d_kaopen', 2], summary(model32)$coefficients['d_mi', 2], summary(model33)$coefficients['d_ers', 2], summary(model34)$coefficients['ers_mi_onset', 2], summary(model35)$coefficients['mi_kaopen_onset', 2], summary(model36)$coefficients['ers_kaopen_onset', 2])
  boot_median <- c (apply(bs.res1$t,2,median)[11], apply(bs.res2$t,2,median)[11], apply(bs.res3$t,2,median)[11], apply(bs.res4$t,2,median)[11], apply(bs.res5$t,2,median)[11], apply(bs.res6$t,2,median)[11])
  boot_se <- c (apply(bs.res1$t,2,sd)[11], apply(bs.res2$t,2,sd)[11], apply(bs.res3$t,2,sd)[11], apply(bs.res4$t,2,sd)[11], apply(bs.res5$t,2,sd)[11], apply(bs.res6$t,2,sd)[11])
  boot_bias <- c ((mean(bs.res1$t[,11]) - bs.res1$t0[11]), (mean(bs.res2$t[,11]) - bs.res2$t0[11]), (mean(bs.res3$t[,11]) - bs.res3$t0[11]), (mean(bs.res4$t[,11]) - bs.res4$t0[11]), (mean(bs.res5$t[,11]) - bs.res5$t0[11]), (mean(bs.res6$t[,11]) - bs.res6$t0[11]))
  boot_results <- data.frame(model_names, estimate, st_error, boot_median, boot_se, boot_bias)
  #
  # creating table 8
  stargazer(boot_results, title="Table 8. Bootstrap estimation results for autocratic regime breakdown during currency crises", rownames=FALSE, summary=FALSE, type="html", out="arscc_table8.doc")
  #
}
}
#
# creating figure 2
{
  library("jtools")
  library("ggplot2")
  library("margins")
  library("ggpubr")
  #
  figure2a = effect_plot(model36, pred=ers_kaopen_onset, interval = TRUE, int.width=0.95, plot.points = TRUE, jitter=0.1, data=arscc_cur) +
    labs(title="... commitment to a fixed exchange rate prior to a currency crisis") +
    theme(plot.title=element_text(size=10, face="bold"))
  figure2b = effect_plot(model31, pred=d_kaopen, interval = TRUE, int.width=0.95, plot.points = TRUE, jitter=0.15, data=arscc_cur) +
    labs(title="... a change in capital account openness during a currency crisis") +
    theme(plot.title=element_text(size=10, face="bold"))
  #
  figure2=ggarrange(figure2a, figure2b, ncol=2, nrow=1) +
    labs(title="Figure 2: Autocratic regime change probabilities given...") +
    theme(plot.title=element_text(size=12, face="bold"))
  figure2
  # cplot(model34, "d_kaopen", what="effect")
  # plot_summs(model21, model24, model27, scale = TRUE)
}
#
#
# PART 3: Addressing the problem of endogeneity between crisis policy response and autocratic regime stability
#
# models with instrumental variables: 2SLS, LIML, Fuller, currency crises dataset
#
{
  library("cragg")
  library("ivmodel")
  # currency crises sample
  arscc_cur <- read.csv(file="arscc_cur.csv", header=TRUE, sep=",")
  arscc_cur_short=arscc_cur[complete.cases(arscc_cur[, 25]), ]
  # first stage
  model41 = lm(d_kaopen ~  fedrate + d_fedrate + kaopen_onset + ln_gdppc_onset + gdppcgr_avg + polity2_avg + party + military + personal, data=arscc_cur_short)
  summary(model41)
  # Stock-Yogo test for weak instruments
  stock_yogo_test (D=~ d_kaopen, X=~ ln_gdppc_onset + gdppcgr_avg + polity2_avg + party + military + personal, Z=~ fedrate + d_fedrate + kaopen_onset, data = arscc_cur_short, size_bias="bias", B=0.05)
  # second stage with OLS & probit
  d_kaopen_pred<-fitted.values(model41)
  model42 <- lm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + polity2_avg + party + military + personal + kaopen_onset + d_kaopen_pred, data=arscc_cur_short)
  summary(model42)
  model43 <- glm(regime_change ~ ln_gdppc_onset + gdppcgr_avg + polity2_avg + party + military + personal + kaopen_onset + d_kaopen_pred, family=binomial(link='probit'), data=arscc_cur_short)
  summary(model43)
  # second stage: TSLS, LIML, Fuller
  Y = arscc_cur_short[ , "regime_change"]
  D = arscc_cur_short[ , "d_kaopen"]
  Zname = c("d_fedrate", "fedrate", "kaopen_onset")
  Z = arscc_cur_short[ , Zname]
  Xname = c("ln_gdppc_onset", "gdppcgr_avg", "polity2_avg", "party", "military", "personal", "kaopen_onset")
  X = arscc_cur_short[ ,Xname]
  model44 = ivmodel (Y=Y, D=D, Z=Z, X=X)
  summary(model44)
  coef.ivmodel(model44)
  coefOther(model44)
  #
}
#
# creating table 3
{
  library("texreg")
  htmlreg(list(model41, model42, model43), file="arscc_table3.doc", doctype=TRUE, digits=3, custom.model.names=c("(1)", "(2)", "(3)"), reorder.coef=c(1, 5, 6, 7, 8, 9, 10, 4, 2, 3, 11), caption.above=TRUE, caption="<b>Table 3. Capital account policy and autocratic regime survival during currency crises: Adressing the problem of endogeneity</b>", stars=c(0.01, 0.05, 0.1), custom.note="Notes: Cells contain parameter estimates and standard errors. <br> Significance levels: &#42;p&lt;0.1, &#42;&#42;p&lt;0.05, &#42;&#42;&#42;p&lt;0.01", inline.css=TRUE, html.tag=TRUE, head.tag=TRUE, body.tag=TRUE, single.row=TRUE)
}
#