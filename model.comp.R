# 时间：2017-09-23
# 地点：北京 asreml培训会
# 作者：邓飞
# 公司：北京维斯恩思软件有限责任公司
# 电话：010-62680244
# 邮箱：China@vsni.co.uk
# 本资料仅供学习，请勿用于商业用途，请勿传播。asreml软件学习QQ群：398671275


if(is.null(library(plyr))){install.packages("plyr")}
library(plyr)
model.comp <- function (m1 = NULL, m2 = NULL, Nml = NULL, mulM = NULL, LRT = NULL, 
          rdDF = NULL) 
{
  if (is.null(mulM)) 
    mulM = FALSE
  if (is.null(LRT)) 
    LRT = FALSE
  if (is.null(rdDF)) 
    rdDF = FALSE
  cat("Attension:\n")
  cat("Fixed factors should be the same!\n\n\n")
  Mnames <- NULL
  Npm <- 0
  if (mulM == TRUE) {
    Nmls = ceiling(length(Nml)/43)
    LogL = Pm = Nedf = 0
    for (i in 1:Nmls) {
      LogL[i] <- Nml[[2 + (i - 1) * 43]]
      Pm[i] = length(Nml[[3 + (i - 1) * 43]])
      Nedf[i] = length(Nml[[17 + (i - 1) * 43]])
    }
  }
  else {
    LogL = c(m1[[2]], m2[[2]])
    Pm = c(length(m1[[3]]), length(m2[[3]]))
    Nedf = c(m1[[17]], m2[[17]])
    Nmls = 2
    Mnames <- c(deparse(substitute(m1)), deparse(substitute(m2)))
  }
  df <- data.frame(LogL = LogL, Npm = Pm)
  AIC = 2 * (Pm - LogL)
  df$AIC <- AIC
  ifelse(mulM == TRUE, df$Model <- paste("m", 1:Nmls, sep = ""), 
         df$Model <- Mnames)
  df <- df[, c(4, 1:3)]
  BIC = -2 * LogL + Pm * log(Nedf)
  df$BIC = BIC
  b1 <- which.min(AIC)
  df$AIC.State <- ""
  df[b1, 6] <- "better"
  b2 <- which.min(BIC)
  df$BIC.State <- ""
  df[b2, 7] <- "better"
  df <- arrange(df, df$Npm)
  print(df)
  cat("-----------------------------\n")
  cat("Lower AIC and BIC is better model.\n\n")
  A <- combn(1:Nmls, 2)
  B <- Nmls * (Nmls - 1)/2
  if (LRT == TRUE) {
    cat("\n\n")
    cat("Attension: Please check every asreml results' length is 43;\n")
    cat("if the length < 43, put the object at the end of Nml.\n")
    cat("In the present, just allow one object's length < 43.")
    cat("\n=====================================")
    cat("\nLikelihood ratio test (LRT) results:\n\n")
    for (i in 1:B) {
      if (B > 1) 
        df1 <- df[A[, i], 1:4]
      else df1 <- df[1:2, 1:4]
      df1 <- arrange(df1, df1$Npm)
      DlogL = df1$LogL[2] - df1$LogL[1]
      Ddf = df1$Npm[2] - df1$Npm[1]
      pv <- ifelse(rdDF == TRUE, round(1 - pchisq(2 * DlogL, 
                                                  Ddf - 0.5), 3), round(1 - pchisq(2 * DlogL, Ddf), 
                                                                        3))
      df1$pv <- c(0, pv)
      names(df1)[5] <- "Pr(>F)"
      siglevel <- 0
      if (abs(pv) < 0.05) {
        siglevel <- "*"
      }
      else {
        siglevel <- "Not signif"
      }
      if (abs(pv) < 0.01) {
        siglevel <- "**"
      }
      if (abs(pv) < 0.001) {
        siglevel <- "***"
      }
      df1$Sig.level <- c(0, siglevel)
      df1[1, 5:6] <- ""
      df2 <- arrange(df1, df1$Model)
      cat("\nModel compared between ", df2$Model[1], "--", 
          df2$Model[2], ":\n")
      print(df1)
      cat("---------------")
      cat("\nSig.level: 0'***' 0.001 '**' 0.01 '*' 0.05 'Not signif' 1\n\n")
    }
    cat("=====================================")
    if (rdDF == TRUE) {
      cat("\nAttension: Ddf=Ddf-0.5. \n")
      cat("When for corr model, against +/-1. \n\n")
    }
    else {
      cat("\nAttension: Ddf did not minus 0.5. \n")
      cat("When for corr model, against 0. \n\n")
    }
  }
}
# 
# library(asreml)
# library(plyr)
# data(oats)
# head(oats)
# m1 <- asreml(yield ~ 1, random=~Nitrogen*Variety, data=oats)
# m2 <- asreml(yield ~ 1, random=~Nitrogen*Variety+Blocks, data=oats)
# m3 <- asreml(yield ~ 1, random=~Nitrogen*Variety+Blocks/Wplots, data=oats)
# 
# 
# # AIC BIC
# model.comp(m1)
# 
# # m1 VS m2
# model.comp(m1,m2)
# 
# # m1 VS m2 :LRT
# model.comp(m1,m2,LRT = T)
# 
# # m1,m2,m3: compare
# model.comp(Nml = c(m1,m2,m3),mulM = T)
# 
# # m1,m2,m3: LRT
# model.comp(Nml = c(m1,m2,m3),mulM = T,LRT = T)
