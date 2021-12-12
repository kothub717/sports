#データ加工
mergedsports <- merge(sports_new, sports, all=T) 
mergedsports <- merge(mergedsports, data, by = c("id"), all = T)
mergedsports <- mergedsports[, colnames(mergedsports) != "ID"]
mergedsports <- mergedsports %>% distinct(id, .keep_all = TRUE)
mergedsports2 <- mergedsports[,1:37]

mergedtaiiku <- merge(taiiku_new, taiiku, all=T) 
mergedtaiiku <- merge(mergedtaiiku, data, by = c("id"), all = T)
mergedtaiiku <- mergedtaiiku[, colnames(mergedtaiiku) != "ID"]
mergedtaiiku <- mergedtaiiku %>% distinct(id, .keep_all = TRUE)

mergedsskw <- merge(mergedsports2, mergedtaiiku, by = c("id"), all = T)

mergedsskw <- data_frame(mergedsskw)

mergedsskw <- relocate(mergedsskw, scluster, .before = NULL, .after = id)
mergedsskw <- relocate(mergedsskw, tcluster, .before = NULL, .after = scluster)

#csv書き出し
write.csv(mergedsports, file = "mergedsports.csv", fileEncoding = "SJIS")
write.csv(mergedtaiiku, file = "mergedtaiiku.csv", fileEncoding = "SJIS")
write.csv(mergedsskw, file = "mergedsskw.csv", fileEncoding = "SJIS")

##重回帰分析
library(car)
# 多重共線性のチェック
vif(model_PC1_s)
vif(model_PC2_s)
vif(model_PC1_t)
vif(model_PC2_t)
cor(model_PC1_s)

# スポーツ主成分1(好印象)
model_PC1_s <- lm(formula = PC1_s ? Q6_1 + Q1_2 + Q2_1 + Q2_7 + Q2_8 + Q2_13 + Q2_21 + 
                          Q2_69 + Q2_78 + nQ3level + Q14 + Q15 + Q17 + Q18 + Q24
                  , data = mergedsskw)
summary(model_PC1_s)
#1位：動物と遊ぶ生き物の世話(1.45526**)、運動スポーツをする(0.25482**)
#2位：体を動かして遊ぶ(0.36270*)、動物と遊ぶ生き物の世話(1.98543*)
#     運動スポーツをする(0.56732**)、勉強をする(0.43972*)
#3位：音楽を聴く(0.34645**)

# スポーツ主成分2(あまり良くない印象)
model_PC2_s <- lm(formula = PC2_s ? Q6_1 + Q1_2 + Q2_1 + Q2_7 + Q2_8 + Q2_13 + Q2_21 + 
                          Q2_69 + Q2_78 + nQ3level + Q14 + Q14SQ2 + Q15 + Q17 + Q18 
                  , data = mergedsskw)
summary(model_PC2_s)
#1位：何もしない(0.498741*)、出かける友達と出かける(1.171181*)
#     本を読む、勉強をする、音楽を聴く、芸術・創作活動、テレビDVDなどをみる
#     屋内で遊ぶ、ゲーム機
#2位：友達や家族と遊ぶ、本を読む、テレビDVD、屋内、ゲーム機、寝る
#3位：何もしない、人と会う(-0.18)

# 体育主成分1(あまり良くない印象)
model_PC1_t <- lm(formula = PC1_t ? Q6_1 + Q1_2 + Q2_1 + Q2_7 + Q2_8 + Q2_13 + Q2_21 + 
                          Q2_69 + Q2_78 + nQ3level + Q14 + Q14SQ2 + Q15 + Q17 + Q18 
                  , data = mergedsskw)
summary(model_PC1_t)
#1位：音楽を聴く、料理、食事
#3位：運動スポーツをする(-)、音楽を聴く(-)

# 体育主成分2(好印象)
model_PC2_t <- lm(formula = PC2_t ? Q6_1 + Q1_2 + Q2_1 + Q2_7 + Q2_8 + Q2_13 + Q2_21 + 
                          Q2_69 + Q2_78 + nQ3level + Q14 + Q14SQ2 + Q15 + Q17 + Q18 
                  , data = mergedsskw)
summary(model_PC2_t)
#1位：本を読む、勉強をする、音楽を聴く、テレビDVDを見る、屋内で遊ぶ、
#     ゲーム機はマイナス
#2位：本を読む、テレビ屋内、ゲーム機、寝るはマイナス
        
##書き出し
model.ans <- summary(model_PC1_s)
result_regress <- model.ans$coefficients #回帰係数を抽出
write.table(matrix(c("",colnames(result_regress)),nrow=1),"回帰分析.csv",append=T,quote=F,sep=",",row.names=F,col.names=F)
write.table(result_regress,"回帰分析.csv",append=T,quote=F,sep=",",row.names=T,col.names=F)

#Q6_1 何をやってもいい時間ができたときにやりたいこと1位
#Q1_2 性別
#Q2_1 サッカー
#Q2_7 キャッチボール
#Q2_8 バスケ
#Q2_13 バドミントン
#Q2_21 ドッジボール
#Q2_69 鬼ごっこ
#Q2_78 ぶらんこ
#nQ3level 実施頻度群
#Q14 学校の運動部や民間・地域のスポーツクラブの加入状況
#Q14SQ2 スポーツの習い事への月平均支出
#Q15 芸術・文化・学習関係の習いごとの月平均支出
#Q17 家族と運動・スポーツ・運動遊びをしているか
#Q18 子どもの1週間の朝食摂取頻度
#Q24 年収