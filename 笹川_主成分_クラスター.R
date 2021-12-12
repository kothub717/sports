#ライブラリ読み込み
library(tidyr)
library(dplyr)
library(fmsb)
library(RColorBrewer)

#データ読み込み
data <- read.csv("SSF_c_2019_likehate.csv", header = T, fileEncoding = "SJIS")

#スポーツに関するアンケートデータ切り出し
sports <- data[-1, 386:420]
ID <- data[-1, 1]
sports <- cbind(ID, sports)
colnames(sports) <- c("ID","気持ち良い_s", "得意_s", "苦手_s", "勝ち負け決まるのが面白い_s", "汗かくのが気持ち悪い_s",
                      "友達の様子が気になる_s", "楽しい_s", "タイムを計るのが嫌_s", "注意されることが多い_s",
                      "疲れる_s", "失敗することが多い_s", "できないと恥ずかしい_s", "やる気が出ない_s",
                      "一緒にできる友だちがいる_s", "先生やコーチに褒められる_s", "友達に褒められる_s", 
                      "友だちが応援してくれる_s", "上手な人と一緒に運動をしなければならない_s", 
                      "あきらめずに練習すればできる_s", "面倒_s", "先生が怒りやすい/コーチに怒られるのが嫌_s",
                      "上達が楽しい_s", "もっと上手くなりたい_s", "学ぶことが多い_s", "全国大会に行くのが楽しみ_s", 
                      "タイムを計るのが好き_s","好きなスポーツなら楽しい/好きなスポーツと嫌いなスポーツがある_s", 
                      "できないと責められる_s", "友達が増えた_s", "日焼けが良くない_s", "ゲームが楽しい_s",
                      "野球がなくてつまらない_s", "ルールを守らない人が気になる_s", "その他_s", "特になし_s")
head(sports)
class("sports")
sports <- lapply(sports, as.numeric)
sports <- data.frame(sports)
sports <- drop_na(sports)

#主成分分析
result <- prcomp(sports[, -1]) #主成分分析用にIDを削除
str(result)
summary(result)
screeplot(result)
result$rotation[, 1]
par(mar = c(31, 11, 7, 11), cex = 0.5)
barplot(result$rotation[, 1], las = 2, main = "スポーツ 主成分1")
result$rotation[, 2]
barplot(result$rotation[, 2], las = 2, main = "スポーツ 主成分2")

#主成分得点
result$x
result$x[,1:2]
pc_scores_s <- cbind(sports$ID, result$x[,1:2]) #主成分1と2
head(pc_scores_s)
colnames(pc_scores_s)[1] <- "ID"
pc_scores_s.df <- data.frame(pc_scores_s)
head(pc_scores_s.df)

#全部の変数でクラスター分析
sports_all <- sports[, -1]
data_dist <- dist(sports_all, method = "euclidean")
result3 <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result3, hang = -1, sub = "", xlab = "")
n <- 2
rect.hclust(result3, k = n)
data_cluster <- cutree(result3, k = n)
data_cluster
table(data_cluster)
sports_all$scluster <- data_cluster
str(sports_all)
data_mean <- sports_all %>%
  group_by(scluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4, vlcex = 2.6, title = "スポーツクラスター(変数all)") 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 1.5) 

#変数を削ってクラスター分析
sports_new <- sports[, -(20:36)]
sports_new <- sports_new[, -1]
data_dist <- dist(sports_new, method = "euclidean")
result3_new <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result3_new, hang = -1, sub = "", xlab = "")
n <- 2
rect.hclust(result3_new, k = n)
data_cluster <- cutree(result3_new, k = n)
data_cluster
table(data_cluster)
sports_new$scluster <- data_cluster
str(sports_new)
data_mean <- sports_new %>%
  group_by(scluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4, vlcex = 2.6, title = "スポーツクラスター") 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 1.5) 
#cluster1=友達が少なくスポーツが得意ではない、cluster2=友達が多くスポーツが得意

#体育に関するアンケートデータ切り出し
taiiku <- data[-1, 421:455]
taiiku <- cbind(ID, taiiku)
colnames(taiiku) <- c("ID","気持ち良い_t", "得意_t", "苦手_t", "勝ち負け決まるのが面白い_t", "汗かくのが気持ち悪い_t",
                      "友達の様子が気になる_t", "楽しい_t", "タイムを計るのが嫌_t", "注意されることが多い_t",
                      "疲れる_t", "失敗することが多い_t", "できないと恥ずかしい_t", "やる気が出ない_t",
                      "一緒にできる友だちがいる_t", "先生やコーチに褒められる_t", "友達に褒められる_t", 
                      "友だちが応援してくれる_t", "上手な人と一緒に運動をしなければならない_t", 
                      "あきらめずに練習すればできる_t", "面倒_t", "先生が怒りやすい/コーチに怒られるのが嫌_t",
                      "上達が楽しい_t", "もっと上手くなりたい_t", "学ぶことが多い_t", "全国大会に行くのが楽しみ_t", 
                      "タイムを計るのが好き_t","好きなスポーツなら楽しい/好きなスポーツと嫌いなスポーツがある_t", 
                      "できないと責められる_t", "友達が増えた_t", "日焼けが良くない_t", "ゲームが楽しい_t",
                      "野球がなくてつまらない_t", "ルールを守らない人が気になる_t", "その他_t", "特になし_t")
head(taiiku)
class("taiiku")
taiiku <- lapply(taiiku, as.numeric)
taiiku <- data.frame(taiiku)
taiiku <- drop_na(taiiku)

#主成分分析
result2 <- prcomp(taiiku[, -1]) #主成分分析用にIDを削除
str(result)
summary(result2)
screeplot(result2)
result2$rotation[, 1]
par(mar = c(31, 11, 7, 11), cex = 0.5)
barplot(result2$rotation[, 1], las = 2, main = "体育 主成分1")
result2$rotation[, 2]
barplot(result2$rotation[, 2], las = 2, main = "体育 主成分2")

#主成分得点
result2$x
result2$x[,1:2]
pc_scores_t <- cbind(taiiku$ID, result2$x[,1:2]) #主成分1と2
head(pc_scores_t)
colnames(pc_scores_t)[1] <- "ID"
pc_scores_t.df <- data.frame(pc_scores_t)
head(pc_scores_t.df)

##データ結合
mergedsskw <- merge(pc_scores_s.df, pc_scores_t.df, by = c("ID"), all = T)
mergedsskw <- merge(mergedsskw, sports, by = c("ID"), all = T)
mergedsskw <- merge(mergedsskw, taiiku, by = c("ID"), all = T)
mergedsskw <- merge(mergedsskw, data, by = c("ID"), all = T)
mergedsskw[1,] <- mergedsskw[1539,]
colnames(mergedsskw)[2] <- "PC1_s"
colnames(mergedsskw)[3] <- "PC2_s"
colnames(mergedsskw)[4] <- "PC1_t"
colnames(mergedsskw)[5] <- "PC2_t"
write.csv(mergedsskw, "mergedsskw.csv", fileEncoding = "CP932")


##スポーツのみ結合
mergedsports <- merge(sports, pc_scores_s.df, by = c("ID"), all = T) #主成分1と2をsportsとmerge
mergedsports <- merge(mergedsports, data, by = c("ID"), all = T) #上記をdataとmerge
mergedsports[1,] <- mergedsports[1539,]
write.csv(mergedsports, "mergedsports.csv", fileEncoding = "CP932")
head(result$x[,1:2])


##体育のみ結合
mergedtaiiku <- merge(taiiku, pc_scores_t.df, by = c("ID"), all = T) #主成分1と2をsportsとmerge
mergedtaiiku <- merge(mergedtaiiku, data, by = c("ID"), all = T) #上記をdataとmerge
mergedtaiiku[1,] <- mergedtaiiku[1539,]
write.csv(mergedtaiiku, "mergedtaiiku.csv", fileEncoding = "CP932")
head(result2$x[,1:2])


#全部の変数を使ってクラスター分析
taiiku_all <- taiiku[, -1]
data_dist <- dist(taiiku_all, method = "euclidean")
result4 <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result4, hang = -1, sub = "", xlab = "")
n <- 2
rect.hclust(result4, k = n)
data_cluster <- cutree(result4, k = n)
data_cluster
table(data_cluster)
taiiku_all$tcluster <- data_cluster
str(taiiku_all)
data_mean <- taiiku_all %>%
  group_by(tcluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4,title = "体育クラスター(変数all)") 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 3) 

#変数を削ってクラスター分析
taiiku_new <- taiiku[, -(20:36)]
taiiku_new <- taiiku_new[, -1]
data_dist <- dist(taiiku_new, method = "euclidean")
result4_new <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result4_new, hang = -1, sub = "", xlab = "")
n <- 2
rect.hclust(result4_new, k = n)
data_cluster <- cutree(result4_new, k = n)
data_cluster
table(data_cluster)
taiiku_new$tcluster <- data_cluster
str(taiiku_new)
data_mean <- taiiku_new %>%
  group_by(tcluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4, vlcex = 2.6, title = "体育クラスター") 
help("radarchart")
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 1.5) 
#cluster1=友達が少なく体育苦手、cluster2=友達がおり体育得意



data4 <- cbind(data2, data3)
#dim(sports)

#write.csv(sskw, file = "sskw.csv", fileEncoding = "SJIS")

#V386-V420
#V421-V455

#ライブラリ読み込み
library(tidyr)
library(dplyr)
library(fmsb)
library(RColorBrewer)

#データ読み込み
data <- read.csv("SSF_c_2019_likehate.csv", header = T, fileEncoding = "SJIS")

#データ切り出し
sskw <- data[, 386:455]
head(sskw)

#スポーツに関するアンケートデータ切り出し
data2 <- data[, 386:420]
colnames(data2) <- c("気持ち良い", "得意", "苦手", "勝ち負け決まるのが面白い", "汗かくのが気持ち悪い",
                     "友達の様子が気になる", "楽しい", "タイムを計るのが嫌", "注意されることが多い",
                     "疲れる", "失敗することが多い", "できないと恥ずかしい", "やる気が出ない",
                     "一緒にできる友だちがいる", "先生やコーチに褒められる", "友達に褒められる", 
                     "友だちが応援してくれる", "上手な人と一緒に運動をしなければならない", 
                     "あきらめずに練習すればできる", "面倒", "先生が怒りやすい/コーチに怒られるのが嫌",
                     "上達が楽しい", "もっと上手くなりたい", "学ぶことが多い", "全国大会に行くのが楽しみ", 
                     "タイムを計るのが好き","好きなスポーツなら楽しい/好きなスポーツと嫌いなスポーツがある", 
                     "できないと責められる", "友達が増えた", "日焼けが良くない", "ゲームが楽しい",
                     "野球がなくてつまらない", "ルールを守らない人が気になる", "その他", "特になし")
sports <- data2[-1, ]
head(sports)
class("sports")
sports <- lapply(sports, as.numeric)
sports <- data.frame(sports)
sports <- drop_na(sports)
#主成分分析
result <- prcomp(sports)
str(result)
summary(result)
screeplot(result)
result$rotation[, 1]
par(mar = c(31, 11, 7, 11), cex = 0.5)
barplot(result$rotation[, 1], las = 2, main = "スポーツ 主成分1")
result$rotation[, 2]
barplot(result$rotation[, 2], las = 2, main = "スポーツ 主成分2")
#クラスター分析
data_dist <- dist(sports, method = "euclidean")
result3 <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result3, hang = -1, sub = "", xlab = "")
n <- 3
rect.hclust(result3, k = n)
data_cluster <- cutree(result3, k = n)
data_cluster
table(data_cluster)
sports$cluster <- data_cluster
str(sports)
data_mean <- sports %>%
  group_by(cluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4) 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 0.8) 
#変数を削ってクラスター分析
sports_new <- sports[, -(20:36)]
data_dist <- dist(sports_new, method = "euclidean")
result3_new <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result3_new, hang = -1, sub = "", xlab = "")
n <- 3
rect.hclust(result3_new, k = n)
data_cluster <- cutree(result3_new, k = n)
data_cluster
table(data_cluster)
sports_new$cluster <- data_cluster
str(sports_new)
data_mean <- sports_new %>%
  group_by(cluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4) 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 2) 

#体育に関するアンケートデータ切り出し
data3 <- data[, 421:455]
colnames(data3) <- c("気持ち良い", "得意", "苦手", "勝ち負け決まるのが面白い", "汗かくのが気持ち悪い",
                     "友達の様子が気になる", "楽しい", "タイムを計るのが嫌", "注意されることが多い",
                     "疲れる", "失敗することが多い", "できないと恥ずかしい", "やる気が出ない",
                     "一緒にできる友だちがいる", "先生やコーチに褒められる", "友達に褒められる", 
                     "友だちが応援してくれる", "上手な人と一緒に運動をしなければならない", 
                     "あきらめずに練習すればできる", "面倒", "先生が怒りやすい/コーチに怒られるのが嫌",
                     "上達が楽しい", "もっと上手くなりたい", "学ぶことが多い", "全国大会に行くのが楽しみ", 
                     "タイムを計るのが好き","好きなスポーツなら楽しい/好きなスポーツと嫌いなスポーツがある", 
                     "できないと責められる", "友達が増えた", "日焼けが良くない", "ゲームが楽しい",
                     "野球がなくてつまらない", "ルールを守らない人が気になる", "その他", "特になし")
taiiku <- data3[-1, ]
head(taiiku)
class("taiiku")
taiiku <- lapply(taiiku, as.numeric)
taiiku <- data.frame(taiiku)
taiiku <- drop_na(taiiku)
#主要因分析
result2 <- prcomp(taiiku)
str(result2)
summary(result2)
screeplot(result2)
result2$rotation[, 1]
barplot(result2$rotation[, 1], las = 2, main = "体育 主成分1")
par(mar = c(31, 11, 7, 11), cex = 0.5)
result2$rotation[, 2]
barplot(result2$rotation[, 2], las = 2, main = "体育 主成分2")
par(mar = c(31, 11, 7, 11), cex = 0.5)
#クラスター分析
data_dist <- dist(taiiku, method = "euclidean")
result4 <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result4, hang = -1, sub = "", xlab = "")
n <- 4
rect.hclust(result4, k = n)
data_cluster <- cutree(result4, k = n)
data_cluster
table(data_cluster)
taiiku$cluster <- data_cluster
str(taiiku)
data_mean <- taiiku %>%
  group_by(cluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4) 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 1) 
#変数を削ってクラスター分析
taiiku_new <- taiiku[, -(20:36)]
data_dist <- dist(taiiku_new, method = "euclidean")
result4_new <- hclust(data_dist, method = "ward.D2")
par(mar = c(7, 11, 7, 11), cex = 1)
plot(result4_new, hang = -1, sub = "", xlab = "")
n <- 4
rect.hclust(result4_new, k = n)
data_cluster <- cutree(result4_new, k = n)
data_cluster
table(data_cluster)
taiiku_new$cluster <- data_cluster
str(taiiku_new)
data_mean <- taiiku_new %>%
  group_by(cluster) %>% 　#group_by(cluster) でクラスターごとにグループ化
  summarise_all(mean)　　#グループごと平均値算出
data_mean
data_max <- max(data_mean[, -1]) + 0.1 #グラフ範囲MAX．好みで適宜調整してください
data_min <- min(data_mean[, -1]) - 0.1 #グラフ範囲MIN．適宜調整してください
data_radarchart <- rbind(data_max, data_min, data_mean[, -1])
data_radarchart
par(mar = c(13, 13, 13, 13), cex = 0.35)
color_setting <- brewer.pal(n, "Set1")
radarchart(data_radarchart, pcol = color_setting, plwd = 4) 
legend('topright', legend = c(1:n),  col=color_setting, lty = 1:n, lwd = 4, cex = 2) 

#dim(sports)

#write.csv(sskw, file = "sskw.csv", fileEncoding = "SJIS")

#V386-V420
#V421-V455