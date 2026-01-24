# undefined
# 著者: 軽部 将伍
# 日付: 

##############
## 予備設定 ##
##############

# ワークスペースを掃除する
rm(list=ls())

# ワーキングディレクトリ
## setwd("~/GoogleDrive/Lectures/Zemi_Meiji/ZemiPrivateData/analysis_codes/04_regression")
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # 自動設定
print(getwd() )

# パッケージ（追加機能）の追加
## エラーが出たら、install.packages("パッケージ名")でインストールすること。
library(ggplot2) # グラフ出力に使う。
# theme_set(theme_bw()) # Windowsの場合
# theme_set(theme_bw(base_family = "HiraKakuProN-W3")) # Macの場合
theme_set(theme_bw())
library(texreg) ## 回帰表の出力
library(htmltools) ## HTMLで表をプレビュー
library(estimatr) ## ロバスト標準誤差を使った回帰分析（lm_robust）の実行

##############################
## データセットのインポート ##
##############################

## データを読み込む
library(haven)
library(labelled)
d <- read_sav("experiment_origin_data.sav") 

## 最後まで回答完了している人だけをデータに残す

table(d$Progress)
d <- subset(d, Progress == 100)
nrow(d) ## 1191人の有効回答あり

# ##########
# ## 仮説 ##
# ##########

# ※このサンプルコードでは、H1とH5Aだけ仮検証。

# H1. （利便性向上仮説）オンライン投票の導⼊は、対面のみの投票と⽐較して、投票参加意向を⾼める。
# H2A. （政府信頼度仮説）政府への信頼度が客観的に高い場合、高くない場合と⽐較して、投票参加意向が高まる。
# H2B. （政府信頼度仮説）政府への信頼度が客観的に高い場合、高くない場合と⽐較して、オンライン投票の利⽤意向が高まる。
# H3A. （技術信頼度仮説）オンライン投票システムに対する技術的信頼度が客観的に高い場合、高くない場合と⽐較して、投票参加意向が高まる。
# H3B. （技術信頼度仮説）オンライン投票システムに対する技術的信頼度が客観的に高い場合、高くない場合と⽐較して、オンライン投票の利⽤意向が高まる。
# H4. 政府の信頼度が客観的に低い場合より高い場合に、H3の効果量がより大きくなる。
# H5A. 政治関心が低いほど、H1の効果量が大きくなる。
# H5B. SNSの使用頻度が多いほど、H1の効果量が大きくなる。
# H5C. 景気やくらし向きに対する政府の責任認識が強いほど、H2の効果量が大きくなる。


#変数###########################################################################

## 新しいデータセットを作成
## idで個人を特定
dn <- data.frame(id = d$ResponseId) #回答者ID

## 従属変数 ##
# 投票参加意向
dn$sankaiko <- d$exp2_q1a_1

# オンライン投票利用意向
dn$online_sankaiko <- d$exp2_q1b_3

# 独立変数
# オンライン投票導入ダミー(1=導入, 0=非導入)
dn$onlinevote <- as.numeric(d$exp2_onlinevote)

# 政府信頼高ダミー（1=信頼高、0=信頼低）
dn$govtH <- as.numeric(d$exp2_govtH)

# 技術信頼高ダミー（1=信頼高、0=信頼低/非導入）
dn$techH <- as.numeric(d$exp2_techH)


## 条件付け変数 ##

# 政治関心
dn$kanshin <- NA
dn$kanshin[which(d$polint%in%c(3,4))] <- 0
dn$kanshin[which(d$polint%in%c(2))] <- 0.5
dn$kanshin[which(d$polint%in%c(1))] <- 1
table(dn$kanshin, exclude=F)

# インターネット使用量
dn$useinternet <- d$useinternet

# 社会への暮らし向きに対する政府の責任
dn$gvtresp_1 <- 5 - d$gvtresp_1 #GK 1が責任があるなので、逆転させる

# 私生活への暮らし向きに対する政府の責任
dn$gvtresp_2 <- 5 - d$gvtresp_2 #GK 1が責任があるなので、逆転させる

##オンライン投票群のみの変数####################################################
# オンライン投票=1の群に限定
dn_online <- subset(dn, onlinevote == 1)

# 分析
print("\n=====仮説1====\n")
# H1. Y=投票参加意向、X=オンライン投票導入、Z=政府信頼高、技術信頼高
mh1 <- lm_robust(sankaiko ~ onlinevote + govtH + techH, data = dn)
## 簡易的な結果
screenreg(list(mh1), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

print("\n=====仮説2====\n")
# H2A: Y=投票参加意向、X=政府信頼高、Z=オンライン投票導入、技術信頼高
mh2a <- lm_robust(sankaiko ~ govtH + onlinevote + techH, data = dn)
# H2b: （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=政府信頼高、Z=技術信頼高
mh2b <- lm_robust(online_sankaiko ~ govtH + techH, data = dn_online)
# H2の簡易的な結果
screenreg(list(mh2a, mh2b), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

print("\n=====仮説3====\n")
# H3a. Y=投票参加意向、X=技術信頼高、Z=オンライン投票導入、政府信頼高
mh3a <- lm_robust(sankaiko ~ techH + onlinevote + govtH, data = dn)
# H3b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=技術信頼高、Z=政府信頼高
mh3b <- lm_robust(online_sankaiko ~ techH + govtH, data = dn_online)
# H3の簡易的な結果
screenreg(list(mh3a, mh3b), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

print("\n=====仮説4====\n")
# H4a. Y=投票参加意向、X=技術信頼高、M=政府信頼高、Z=オンライン投票導入
mh4a <- lm_robust(sankaiko ~ techH*govtH + onlinevote, data = dn)
# H4b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=技術信頼高, M=政府信頼高
mh4b <- lm_robust(online_sankaiko ~ techH*govtH, data = dn_online)
# H4の簡易的な結果
screenreg(list(mh4a, mh4b), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

print("\n=====仮説5====\n")
# H5a. Y=投票参加意向、X=オンライン投票導入、M=政治関心高、Z=政府信頼高、技術信頼高
mh5a <- lm_robust(sankaiko ~ onlinevote*kanshin + govtH + techH, data = dn)
# H5b. Y=投票参加意向、X=オンライン投票導入、M=SNSの使用頻度、Z=政府信頼高、技術信頼高
mh5b <- lm_robust(sankaiko ~ onlinevote*useinternet + govtH + techH, data = dn)
# H5c-a. Y=投票参加意向、X=政府信頼高、M=景気や暮らし向きに対する政府の責任認識、Z=オンライン投票導入、技術信頼高
mh5c_a <- lm_robust(sankaiko ~ govtH*gvtresp_1 + onlinevote + techH, data = dn)
# H5c-b. （オンライン投票導入=1にサンプルを限定）Y=オンライン投票利用意向、X=政府信頼高、M=景気や暮らし向きに対する政府の責任認識、Z=技術信頼高
mh5c_b <- lm_robust(online_sankaiko ~ govtH * gvtresp_1 + 
                      techH, data = dn_online)

# H5の簡易的な結果
screenreg(list(mh5a, mh5b, mh5c_a, mh5c_b), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")


print("\n=====発展的な仮説・子供の数====\n")
dn$marrykids <- d$marrykids
dn$marrykids[which(d$marrykids%in%c(1,4))] <- 0
dn$marrykids[which(d$marrykids%in%c(2,5))] <- 0.5
dn$marrykids[which(d$marrykids%in%c(3,6))] <- 1
mhex_marrykids <- lm_robust(sankaiko ~ onlinevote + govtH + techH + marrykids, data = dn)
# 簡易的な結果の表示
screenreg(list(mhex_marrykids), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")


# ここから先生記述
###########################################################
## 予測値の算出（必要なければ内容を変更しないこと）########
##（ここから）#############################################
genpr <- function(dpr, # 予測値算出用データ（元データ）
                  mpr, # 予測値算出用分析結果
                  setx = NULL, # 独立変数の名前  
                  setxvals = NULL, # 独立変数の値設定（numeric/character）
                  setxlabs=NULL, # 独立変数ラベル（カテゴリ変数の場合）
                  setm=NULL, # 条件付け変数の名前
                  setmvals=NULL, # 条件付変数のシミュレーション用値（list）
                  datalab=NULL) { # データにラベル
  
  ## 予測値算出用の元データの作成
  simdt <- na.omit(dpr[,all.vars(mpr$terms)])
  
  ## Xがセットされていない場合
  if (is.null(setx)) {
    simdt$NOXSET <- 1
    setx <- "NOXSET"
    setxvals <- 1
    if (is.null(setm)) stop("If setx is NULL, needs setm!")
  }
  
  ## 独立変数の値設定
  simx <- setxvals
  
  ## 予測用プロファイルの作成（条件付け変数なし）
  if (is.null(setm)) {
    
    ## プロファイル作成
    simv <- data.frame(simx = simx)
    
    ## 予測用プロファイルの作成（条件付け変数あり）
  } else {
    
    ## 条件付け変数の値設定
    simm <- sapply(setmvals, function(x) x[1])
    names(simm) <- NULL
    ## プロファイル作成
    simv <- data.frame(simx = rep(simx,each=length(simm)), 
                       simm = simm)
    
    ## 条件付け変数が2つ以上ある場合
    if (length(setm)>1) {
      for(i in 2:length(setm)) {
        ## 条件付け変数の値設定
        simmx <- sapply(setmvals, function(x) x[i])
        names(simmx) <- NULL
        simv[,paste0("simm",i)] <- simmx
      }
    }
    
  }
  
  ## 予測値の出力
  prout <- as.data.frame(t(apply(simv, 1, function(k) {
    
    ## 予測値算出用仮データ
    tmpdt <- simdt
    tmpdt[,setx] <- k[1] # 独立変数の値割り当て
    if (!is.null(setm)) {
      # 条件付け変数の値割り当て
      tmpdt[,setm[1]] <- k[2] 
      if (length(setm)>1) {
        for (i in 2:length(setm)) {
          tmpdt[,setm[i]] <- k[1+i]
        }
      }
    }
    ## 予測値算出
    tmp <- colMeans(as.data.frame(predict(mpr, newdata=tmpdt, se.fit=TRUE)))
    tmp <- c(k, tmp[1:2], 
             tmp[1]-tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.975,df=mpr$df[1]),
             tmp[1]-tmp[2]*qt(0.95,df=mpr$df[1]),
             tmp[1]+tmp[2]*qt(0.95,df=mpr$df[1]))
    ## データの列名割り当て
    if (is.null(setm)) {
      names(tmp) <- c("x","pr","se",
                      "lo95","up95","lo90","up90")
    } else {
      names(tmp) <- c("x",paste0("m",1:length(setm)),"pr","se",
                      "lo95","up95","lo90","up90")
    }
    return(tmp)
  })))
  ### 独立変数にラベルを割り当て（ある場合）
  if(!is.null(setxlabs)) {
    prout$x <- 
      factor(prout$x,levels=setxvals,labels=setxlabs)
  } 
  ### 条件付け変数にラベルを割り当て（ある場合）
  if (!is.null(setm)) {
    if (length(setm)==1) {
      prout$labelledm <- 
        factor(names(setmvals)[match(prout[,grep("^m",colnames(prout))],
                                     unlist(setmvals))],
               levels = names(setmvals))
    } else {
      prout$labelledm <- factor(names(setmvals), levels = names(setmvals))
    }
  }
  ### データ自体にラベルを割り当て（ある場合）
  if (!is.null(datalab)) {
    prout$datalab <- datalab
  }
  
  ## 結果を出力
  return(prout)
}
##（ここまで）#############################################


## H1 ##

## 予測値の算出
yosokuout <- genpr(dpr = dn, ## 実験データ
                   mpr = mh1, ## 回帰分析結果
                   setx = "onlinevote",  ## 独立変数名
                   setxvals = c(0,1)) ## 独立変数の任意の値
yosokuout　## pr列がxの値に対応するyの予測値平均

## プロットのモード設定
# "discrete"はXの値が少ないもしくはダミーの場合
# "continuous"はXの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル

## ラベル設定（setmode=="discrete"のときのみ適用）
setlabels <- c("導入なし(0)","導入あり(1)") ## setxvalsと対応させる
# setlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示

## 予測値平均をプロットする（半自動）####################
ggplot(yosokuout) + 
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95),
                  width=0.1, linewidth=0.5),
    geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90),
                  width=0, linewidth=2),
    geom_point(aes(x=as.factor(x), y=pr),color="white"),
    scale_x_discrete(labels=setlabels)) } } + 
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=x, ymin=lo95, ymax=up95), alpha=0.3),
    geom_ribbon(aes(x=x, ymin=lo90, ymax=up90), alpha=0.5),
    geom_line(aes(x=x, y=pr))) } } +
  labs(x=setxlab, y=paste0(setylab, "（予測値平均）"), 
       caption = paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                        "は、90％および95％信頼区間を示している。")) + 
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## (ここまで) ############################################

## グラフを任意の名前で保存
ggsave("h1_yosokuchi_plot.png", width = 6, height = 4)


## H5A ##

## 予測値の算出
yosokuout <- genpr(dpr = dn, ## 実験データ
                   mpr = mh5a, ## 回帰分析結果
                   setx = "onlinevote", ## 独立変数名
                   setxvals = c(0,1), ## 独立変数の任意の値
                   setm = "kanshin", ## 条件付け変数名
                   setmvals = list("関心低（0）" = 0, # 条件付けのラベルと値
                                   "関心中（0.5）" = 0.5, 
                                   "関心高（1）" = 1))
yosokuout ## pr列がxとmの値に対応するyの予測値平均

## プロットのモード設定
# "discrete"はXの値が少ないもしくはダミーの場合
# "continuous"はXの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル
setmlab <- "政治関心" ## M（条件付け変数）の変数ラベル

## ラベル設定（setmode=="discrete"のときのみ適用）
setlabels <- c("導入なし(0)","導入あり(1)") ## setxvalsと対応させる
# setlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示
setdodgewidth <- 0.3 ## 同じXの値を取るエラーバー同士の隙間の大きさ

## 予測値平均をプロットする（半自動）####################
ggplot(yosokuout) +
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95,
                      color = labelledm), width=0.1, linewidth=0.5,
                  position = position_dodge(width=setdodgewidth)),
    geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90,
                      color = labelledm),
                  width=0, linewidth=2,
                  position = position_dodge(width=setdodgewidth)),
    geom_point(aes(x=as.factor(x), y=pr,
                   shape = labelledm),color="white",
               position = position_dodge(width=setdodgewidth)),
    scale_x_discrete(labels = setlabels) ) } } +   
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=x, ymin=lo95, ymax=up95,
                    fill = labelledm), alpha=0.3),
    geom_ribbon(aes(x=x, ymin=lo90, ymax=up90,
                    fill = labelledm), alpha=0.5),
    geom_line(aes(x=x, y=pr, linetype=labelledm))) } } +
  scale_shape_discrete(name = setmlab) + 
  scale_linetype_discrete(name = setmlab) + 
  scale_color_brewer(name = setmlab, type = "qual", palette = 2) + 
  scale_fill_brewer(name = setmlab, type = "qual", palette = 2) + 
  labs(x=setxlab, y=paste0(setylab, "（予測値平均）"),
       caption = paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                        "は、90％および95％信頼区間を示している。")) +
  theme(legend.position="bottom",
        plot.subtitle = element_text(hjust=0.5))
## (ここまで) ############################################

## グラフを任意の名前で保存
ggsave("h5a_yosokuchi_plot.png", width = 6, height = 4)


###########################################################
## 限界効果の算出関数（必要なければ内容を変更しないこと）##
##（ここから）#############################################
intereff <- function(m, # 回帰モデルオブジェクト
                     main, # 独立変数
                     mod,　# 条件付け変数
                     modrange, # 条件付変数を動かす範囲
                     nsim = 30) { # 条件付変数で結果を出力する値の数
  modval = seq(modrange[1],modrange[2],length=nsim)
  mainmod = paste(main,mod,sep=":")
  if (!mainmod%in%rownames(vcov(m))) mainmod = paste(mod,main,sep=":")
  if ("df"%in%names(m)) {
    dfset <- m$df[1]
    # Assuming that df is the same across all.
    # CAUTION: The above is not true if lm_robust's se_type="CR2".
  } else {
    dfset <- df.residual(m)
  }
  cfset <- c(coef(m)[which(names(coef(m))==main)],
             coef(m)[which(names(coef(m))==mainmod)])
  vcset <- c(vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==main)],
             vcov(m)[which(rownames(vcov(m))==mainmod),
                     which(colnames(vcov(m))==mainmod)],
             vcov(m)[which(rownames(vcov(m))==main),
                     which(colnames(vcov(m))==mainmod)])
  
  out = data.frame(mod = modval,
                   est = cfset[1]+cfset[2]*modval,
                   se = sqrt(vcset[1]+modval^2*vcset[2]+2*modval*vcset[3]),
                   qt90 = qt(0.95,dfset),
                   qt95 = qt(0.975,dfset))
  out$lo90 = out$est-out$se*out$qt90
  out$up90 = out$est+out$se*out$qt90
  out$lo95 = out$est-out$se*out$qt95
  out$up95 = out$est+out$se*out$qt95
  out$pval = (1 - pt(abs(out$est/out$se),dfset))*2
  
  return(out)
}
##（ここまで）#############################################


## H5A ##

## 限界効果の出力
genkaiout <- intereff(m = mh5a, # 回帰モデルオブジェクト
                      main = "onlinevote", # 独立変数
                      mod = "kanshin", # 条件付け変数名
                      modrange = c(0,1), # 条件付け変数を動かす範囲
                      nsim=3) # 条件付け変数で結果を出力する値の数
genkaiout # estがmodに条件付けされたmainの係数

## プロットのモード設定
# "discrete"はMの値が少ないもしくはダミーの場合
# "continuous"はMの値が多くて連続している場合
setmode <- "discrete" 

## ラベル設定（共通）
setxlab <- "オンライン投票導入（実験条件）" ## Xの変数ラベル
setylab <- "投票参加意向" ## Yの変数ラベル
setmlab <- "政治関心" ## M（条件付け変数）の変数ラベル

## 条件付け変数のラベル設定（setmode=="discrete"のときのみ適用）
setmlabels <- c("関心低（0）","関心中（0.5）","関心高（1）") ## nsimの数と対応させる
# setmlabels <- waiver() ## わからないときはこれでsetxvalsをそのまま表示

## 限界効果プロット（半自動）####################
ggplot(genkaiout, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  {if (setmode=="discrete") { list(
    geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)),
                  width=0.1, linewidth=0.5),
    geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)),
                  width=0, linewidth=2),
    geom_point(aes(x=as.factor(mod)), color="white"),
    scale_x_discrete(labels=setmlabels)) } } +
  {if (setmode=="continuous") { list(
    geom_ribbon(aes(x=mod, ymin=lo95, ymax=up95), alpha=0.3),
    geom_ribbon(aes(x=mod, ymin=lo90, ymax=up90), alpha=0.5),
    geom_line(aes(x=mod, y=est))) } } +
  labs(subtitle=paste0("従属変数：",setylab),
       y=paste0(setxlab,"の限界効果"), x=setmlab,
       caption=paste0("注：",ifelse(setmode=="discrete","エラーバー","塗りつぶし"),
                      "は、90％および95％信頼区間を示している。")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
##（ここまで）###################################

## グラフを任意の名前で保存
ggsave("h5a_genkaikoka_plot.png", width = 6, height = 4)

#GK このセクションは実質的に使っていないのでコメントアウトします。
# ###########################################################
# ## H2 & H3: 予測値のプロット（信頼度の効果） ##############
# ###########################################################
# 
# # H2A. 政府信頼高が投票参加意向に与える影響
# yosoku_h2a <- genpr(dpr = dn, mpr = mh2a, 
#                     setx = "govtH", setxvals = c(0, 1))
# 
# 
# # H2B. 政府信頼高がオンライン投票利用意向に与える影響
# yosoku_h2b <- genpr(dpr = dn_online, mpr = mh2b, 
#                     setx = "govtH", setxvals = c(0, 1))
# 
# # H3A. 技術信頼高が投票参加意向に与える影響
# yosoku_h3a <- genpr(dpr = dn, mpr = mh3a, 
#                     setx = "techH", setxvals = c(0, 1))
# 
# # H3B. 技術信頼高がオンライン投票利用意向に与える影響
# yosoku_h3b <- genpr(dpr = dn_online, mpr = mh3b, 
#                     setx = "techH", setxvals = c(0, 1))
# 
# # グラフ保存の例 (H2Bを例に)
# setxlab <- "政府信頼度（実験条件）"
# setylab <- "オンライン投票利用意向"
# setlabels <- c("低信頼(0)", "高信頼(1)")
# # ここに前述のggplotコードを当てはめて ggsave("h2b_plot.png") とします。

###########################################################
## H2-H5#GK: グラフ作成と保存 ##############################
###########################################################

# プロット用設定（共通）
setmode <- "discrete"

# --- H2A: 政府信頼 -> 投票参加意向 ---
yosoku_h2a <- genpr(dpr = dn, mpr = mh2a, setx = "govtH", setxvals = c(0, 1))

ggplot(yosoku_h2a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="政府への信頼度", y="投票参加意向（予測値）", subtitle="H2Aの検証")
ggsave("h2a_plot.png", width = 6, height = 4)

# --- H2B: 政府信頼 -> オンライン利用意向 ---
yosoku_h2b <- genpr(dpr = dn_online, mpr = mh2b, setx = "govtH", setxvals = c(0, 1))

ggplot(yosoku_h2b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="政府への信頼度", y="オンライン投票利用意向（予測値）", subtitle="H2Bの検証")
ggsave("h2b_plot.png", width = 6, height = 4)

# --- H3A: 技術信頼 -> 投票参加意向 ---
yosoku_h3a <- genpr(dpr = dn, mpr = mh3a, setx = "techH", setxvals = c(0, 1))

ggplot(yosoku_h3a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="システムへの技術的信頼", y="投票参加意向（予測値）", subtitle="H3Aの検証")
ggsave("h3a_plot.png", width = 6, height = 4)

# --- H3B: 技術信頼 -> オンライン利用意向 ---
yosoku_h3b <- genpr(dpr = dn_online, mpr = mh3b, setx = "techH", setxvals = c(0, 1))

ggplot(yosoku_h3b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), width=0.1, linewidth=0.5) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(x), y=pr), color="white") +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  labs(x="システムへの技術的信頼", y="オンライン投票利用意向（予測値）", subtitle="H3Bの検証")
ggsave("h3b_plot.png", width = 6, height = 4)

#GK H4, H5B, H5C用の予測値プロットも出す

# --- H4A: 技術信頼*政府信頼 -> 投票参加意向 ---
yosoku_h4a <- genpr(dpr = dn, mpr = mh4a, setx = "techH", setxvals = c(0, 1),
                    setm = "govtH", setmvals = list("低（0）" = 0, "高（1）" = 1))

ggplot(yosoku_h4a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府信頼", type="qual", palette=2) + 
  scale_shape_discrete(name="政府信頼") +
  labs(x="システムへの技術的信頼", y="投票参加意向（予測値）", subtitle="H4Aの検証")
ggsave("h4a_plot.png", width = 6, height = 4)

# --- H4B: 技術信頼*政府信頼 -> オンライン利用意向 ---
yosoku_h4b <- genpr(dpr = dn_online, mpr = mh4b, setx = "techH", setxvals = c(0, 1),
                    setm = "govtH", setmvals = list("低（0）" = 0, "高（1）" = 1))

ggplot(yosoku_h4b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府信頼", type="qual", palette=2) + 
  scale_shape_discrete(name="政府信頼") +
  labs(x="システムへの技術的信頼", y="オンライン投票利用意向（予測値）", subtitle="H4Bの検証")
ggsave("h4b_plot.png", width = 6, height = 4)

# --- H5B: OL投票導入*ネット利用度 -> 投票参加意向 ---
quantile(dn$useinternet, probs=c(0.1,0.9)) #10%点と、90%点をとる
yosoku_h5b <- genpr(dpr = dn, mpr = mh5b, setx = "onlinevote", setxvals = c(0, 1),
                    setm = "useinternet", setmvals = list("２時間\n（4; 10%点）" = 4, "５時間以上\n（7; 90%点）" = 7))

ggplot(yosoku_h5b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("非導入(0)", "導入あり(1)")) +
  scale_color_brewer(name="ネット利用時間/日", type="qual", palette=2) + 
  scale_shape_discrete(name="ネット利用時間/日") +
  labs(x="オンライン投票導入の有無", y="投票参加意向（予測値）", subtitle="H5Bの検証")
ggsave("h5b_plot.png", width = 6, height = 4)

# --- H5C_A: 政府信頼*政府責任 -> 投票参加意向 ---
quantile(dn$gvtresp_1, probs=c(0.1,0.9)) #10%点と、90%点をとる
yosoku_h5c_a <- genpr(dpr = dn, mpr = mh5c_a, setx = "govtH", setxvals = c(0, 1),
                    setm = "gvtresp_1", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_a) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識") +
  labs(x="政府信頼", y="投票参加意向（予測値）", subtitle="H5C_Aの検証")
ggsave("h5c_a_plot.png", width = 6, height = 4)

# --- H5C_B: 政府信頼*政府責任 -> オンライン利用意向 ---
yosoku_h5c_b <- genpr(dpr = dn_online, mpr = mh5c_b, setx = "govtH", setxvals = c(0, 1),
                      setm = "gvtresp_1", setmvals = list("ある程度の責任\n（3; 10%点）" = 3, "大きな責任\n（4; 90%点）" = 4))

ggplot(yosoku_h5c_b) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95, color=labelledm), 
                width=0.1, linewidth=0.5, position = position_dodge(width=0.3)) +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90, color=labelledm), 
                width=0, linewidth=2, position = position_dodge(width=0.3)) +
  geom_point(aes(x=as.factor(x), y=pr, shape=labelledm), 
             color="white", position = position_dodge(width=0.3)) +
  scale_x_discrete(labels=c("低信頼(0)", "高信頼(1)")) +
  scale_color_brewer(name="政府責任認識", type="qual", palette=2) + 
  scale_shape_discrete(name="政府責任認識") +
  labs(x="政府信頼", y="オンライン投票利用意向（予測値）", subtitle="H5C_Bの検証")
ggsave("h5c_b_plot.png", width = 6, height = 4)


###########################################################
## H4: 限界効果のプロット（技術信頼 × 政府信頼） #########
###########################################################

## H4a: 政府信頼の高さによって、技術信頼の効果がどう変わるか
genkai_h4a <- intereff(m = mh4a, 
                       main = "techH",   # 興味のある主効果
                       mod = "govtH",    # 条件付け変数
                       modrange = c(0, 1), 
                       nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "技術信頼の効果量"
setmlab <- "政府信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4a）
ggplot(genkai_h4a, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="技術信頼の限界効果", x=setmlab)
ggsave("h4a_genkaikoka_plot.png", width = 6, height = 4)

#GK 一応、裏の分析も入れておきます（こちらの方が興味深いかもですね）
## H4a裏: 技術信頼の高さによって、政府信頼の効果がどう変わるか
genkai_h4aX <- intereff(m = mh4a, 
                       main = "govtH",   # 興味のある主効果
                       mod = "techH",    # 条件付け変数
                       modrange = c(0, 1), 
                       nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "政府信頼の効果量"
setmlab <- "技術信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4a）
ggplot(genkai_h4aX, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="政府信頼の限界効果", x=setmlab)
ggsave("h4aX_genkaikoka_plot.png", width = 6, height = 4)

#GK オンライン投票を従属変数とする分析を追加
## H4b: 政府信頼の高さによって、技術信頼の効果がどう変わるか
genkai_h4b <- intereff(m = mh4b, 
                       main = "techH",   # 興味のある主効果
                       mod = "govtH",    # 条件付け変数
                       modrange = c(0, 1), 
                       nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "技術信頼の効果量"
setmlab <- "政府信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4b）
ggplot(genkai_h4b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="技術信頼の限界効果", x=setmlab)
ggsave("h4b_genkaikoka_plot.png", width = 6, height = 4)

## H4b裏: 技術信頼の高さによって、政府信頼の効果がどう変わるか
genkai_h4bX <- intereff(m = mh4b, 
                        main = "govtH",   # 興味のある主効果
                        mod = "techH",    # 条件付け変数
                        modrange = c(0, 1), 
                        nsim = 2)         # ダミー変数なので2つでOK

# ラベル設定
setxlab <- "政府信頼の効果量"
setmlab <- "技術信頼度"
setmlabels <- c("低信頼（0）", "高信頼（1）")

## 限界効果プロット（H4a）
ggplot(genkai_h4bX, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="政府信頼の限界効果", x=setmlab)
ggsave("h4bX_genkaikoka_plot.png", width = 6, height = 4)


###########################################################
## H5B & H5C: 限界効果のプロット（インターネット・責任）##
###########################################################

## H5B: インターネット使用頻度によるオンライン投票導入の効果差
genkai_h5b <- intereff(m = mh5b, 
                       main = "onlinevote", 
                       mod = "useinternet", 
                       modrange = range(dn$useinternet, na.rm=TRUE), 
                       nsim = 6) #GK 実際には6つ存在するのでデータに合わせる # 低・中・高の3点抽出

#GK グラフを作成
# ラベル設定 　
setmlab <- "ネット利用時間／日"
setmlabels <- c("30m\n(2)", "1H\n(3)", "2H\n(4)", "3H\n(5)", "4H\n(6)", "5H+\n(7)")

## 限界効果プロット（H5C-a）
ggplot(genkai_h5b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="オンライン投票導入の限界効果", x=setmlab)
ggsave("h5b_genkaikoka_plot.png", width = 6, height = 4)

# グラフ保存 (h5b_genkaikoka_plot.png)

## H5C-a: 政府の責任認識による政府信頼の効果差
genkai_h5c_a <- intereff(m = mh5c_a, 
                         main = "govtH", 
                         mod = "gvtresp_1", 
                         modrange = range(dn$gvtresp_1, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
# setxlab <- "政府信頼の限界効果" #GK 使っていないのでコメントアウト
setmlab <- "政府の責任認識（社会）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-a）
ggplot(genkai_h5c_a, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", y="政府信頼の限界効果", x=setmlab)
ggsave("h5c_a_genkaikoka_plot.png", width = 6, height = 4)

#GK 追加
## H5C-b: 政府の責任認識による政府信頼の効果差
genkai_h5c_b <- intereff(m = mh5c_b, 
                         main = "govtH", 
                         mod = "gvtresp_1", 
                         modrange = range(dn$gvtresp_1, na.rm=TRUE), 
                         nsim = 4) #GK 実際には4つ値があるので4に変更

# ラベル設定
setmlab <- "政府の責任認識（社会）"
setmlabels <- c("あまりない\n(1)", "少し\n(2)", 
                "ある程度\n(3)", "大きな責任\n(4)")

## 限界効果プロット（H5C-a）
ggplot(genkai_h5c_b, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), width=0.1) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：オンライン投票利用意向", y="政府信頼の限界効果", x=setmlab)
ggsave("h5c_b_genkaikoka_plot.png", width = 6, height = 4)


###########################################################
## 子供の数の予測値プロット ##############################
###########################################################

# 予測値の算出
yosoku_marrykids <- genpr(dpr = dn, 
                          mpr = mhex_marrykids, 
                          setx = "marrykids", 
                          setxvals = c(0, 0.5, 1))

# ラベル設定
setxlab <- "子供の数"
setylab <- "投票参加意向"
setmlabels <- c("子供なし\n(0)", "子供1人\n(0.5)", "子供2人以上\n(1)")

# 予測値プロット
ggplot(yosoku_marrykids) + 
  geom_errorbar(aes(x=as.factor(x), ymin=lo95, ymax=up95), 
                width=0.1, linewidth=0.5, color="black") +
  geom_errorbar(aes(x=as.factor(x), ymin=lo90, ymax=up90), 
                width=0, linewidth=2, color="black") +
  geom_point(aes(x=as.factor(x), y=pr), 
             color="white", size=3, shape=21, fill="black") +
  scale_x_discrete(labels=setmlabels) +
  labs(x=setxlab, y=paste0(setylab, "（予測値平均）"),
       caption="注：エラーバーは、90％および95％信頼区間を示している。") +
  theme(plot.subtitle = element_text(hjust=0.5))
ggsave("marrykids_yosokuchi_plot.png", width = 6, height = 4)

###########################################################
## 子供の数とオンライン投票導入の交互作用モデル #########
###########################################################

# 交互作用項を含むモデルを推定
mhex_marrykids_int <- lm_robust(sankaiko ~ onlinevote*marrykids + govtH + techH, data = dn)

# 結果の表示
screenreg(list(mhex_marrykids_int), include.ci = FALSE,
          digits=3, single.row = FALSE, 
          stars = c(0.001,0.01,0.05,0.1), symbol="+")

###########################################################
## オンライン投票導入の限界効果（子供の数で条件付け） ##
###########################################################

# 限界効果の算出
genkai_marrykids <- intereff(m = mhex_marrykids_int, 
                             main = "onlinevote",  # 主効果
                             mod = "marrykids",    # 条件付け変数
                             modrange = c(0, 1),   # 子供の数の範囲
                             nsim = 3)             # 0, 0.5, 1の3点

# ラベル設定
setxlab <- "オンライン投票導入の効果量"
setmlab <- "子供の数"
setmlabels <- c("子供なし\n(0)", "子供1人\n(0.5)", "子供2人以上\n(1)")

# 限界効果プロット
ggplot(genkai_marrykids, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), 
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), 
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white", size=3, shape=21, fill="black") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", 
       y=setxlab, x=setmlab,
       caption="注：エラーバーは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
ggsave("marrykids_genkaikoka_plot.png", width = 6, height = 4)

###########################################################
## 子供の数の限界効果（オンライン投票導入で条件付け） ##
###########################################################

# 限界効果の算出（逆方向）
genkai_marrykids_rev <- intereff(m = mhex_marrykids_int, 
                                  main = "marrykids",  # 主効果
                                  mod = "onlinevote",  # 条件付け変数
                                  modrange = c(0, 1),  # オンライン投票導入の範囲
                                  nsim = 2)            # 0, 1の2点

# ラベル設定
setxlab <- "子供の数の効果量"
setmlab <- "オンライン投票導入"
setmlabels <- c("非導入\n(0)", "導入\n(1)")

# 限界効果プロット
ggplot(genkai_marrykids_rev, aes(y=est)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  geom_errorbar(aes(ymin=lo95, ymax=up95, x=as.factor(mod)), 
                width=0.1, linewidth=0.5) +
  geom_errorbar(aes(ymin=lo90, ymax=up90, x=as.factor(mod)), 
                width=0, linewidth=2) +
  geom_point(aes(x=as.factor(mod)), color="white", size=3, shape=21, fill="black") +
  scale_x_discrete(labels=setmlabels) +
  labs(subtitle="従属変数：投票参加意向", 
       y=setxlab, x=setmlab,
       caption="注：エラーバーは、90％および95％信頼区間を示している。") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.subtitle = element_text(hjust=0.5))
ggsave("marrykids_genkaikoka_rev_plot.png", width = 6, height = 4)