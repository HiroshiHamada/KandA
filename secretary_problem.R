# nは候補者全体数．rは観察数
# アルゴリズムは以下の通り
# r人までは選ばず最大値だけ記憶する，
# r+1人目以降にその最大値を超えた最初の人を選ぶ


###### 関数selectの定義 ####### 
select <- function(n, r){
	applicants <- runif(n) # n人を数値化
	candidate <- max(applicants[1:r]) #r人までの暫定1位
	if(candidate == max(applicants)){
		selected <- 0
	}#最初のr人に1位が入ればreturnが0になる
	s <- r + 1 #whileループ用カウンタ
	if(applicants[s] > candidate) {
		selected <- applicants[s] #サーチ最初で超えたら代入
	} else {
		selected <- 0
		while(candidate != max(applicants) & applicants[s] < candidate & s<=n){
			s <- s+1
			selected <- applicants[s]# candidateよりもいい人をselectedに記録
		}#while ends
	}#if else ends
	if(selected == max(applicants)){1}else{0}# selectedの最初が1位か判定
}#return is 0 or 1


###### 動作確認 #######
select(10,3)

# 1000回繰り返して確率を計算
mean(replicate(1000,select(10,3)))

mean(replicate(1000,select(100,37)))

# 平均（成功確率）の分布
hist(
	replicate(100, mean(replicate(1000,select(1000,368))))
)


