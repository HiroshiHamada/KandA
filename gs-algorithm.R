###################################################
# GS Algorithm　概略
###################################################
#関数gsの使い方：
#関数gsは男性(proposer)の選好と女性(receiver)の選好を
#引数として，GS Algorithmに基づく男女のマッチングを出力します

# はじめにproposer, receiverの選好を，集団別にlist型で入力します．
# 例えば男性が求婚する時，proposerとして
# list(c(2, 3, 1), c(1, 2, 3), c(3, 1, 2)) を入力する
# これは
#男性1の選好は　　女2>女3>女1
#男性2の選好は　　女1>女2>女3
#男性3の選好は　　女3>女1>女2
#を意味します.
#同様にreceiverは,女性が持つ男性に対する選好のリストです．

#関数の内部でreceiverの選好をidではなく順位に変換します．
#例えば，ある女性の選好がc(2,3,1)   男2>男3>男1  のとき
#（男1は）3番目, （男2は）1番目,　（男3は）2番目 　という順位のベクトルに変換します．
#つまり　c(2, 3, 1)] -> c(3, 1, 2)　と変換します．
# receiver側で相手を留保するかどうかを順位を参照して決定するためです

########################################
# gs内のローカル変数の意味
########################################
#prefb:男性が持つ,女性に対する選好． prefg:女性が持つ,男性に対する選好．
#nextgirl:プロポーズする順番を記録．各男性が独立に持つ．振られる度に1増加
#tempboy:仮マッチした男性のリスト,最初はn+1(番兵)が入る．
#gid:男性がプロポーズする相手女性のid, 
#b:for用のカウンタ．男性1～nに対応
#s:現在プロポーズ中の男性id
#t:振られた男id．この値をsに代入する事で，
#「今回振られた男t」が自動的に次のsになる．


##################################
# 関数gsの定義
##################################
gs130<- function(proposer,receiver)
{ n<-length(proposer)
prefb = proposer #男性の好みは引数proposerをそのまま入力
z=receiver  #一旦，女性の選好リストを別のzに代入する
for(j in 1:n){
	for(i in 1:n){  
		z[[j]][i]=which(receiver[[j]]==i)}#for j
}#for i
prefg =z #女性の選好を順位表記にする
print("男性の選好") 
print(paste0(matrix(prefb))) 
print(paste0("女性の選好") )
print(paste0(matrix(receiver)) )
for(i in 1:n){
	prefg[[i]] = append(prefg[[i]], n + 1)
}#女性の選好に番兵n+1を追加
nextgirl = rep(1, n)  #個人別のプロポーズ用カウンタ．1位からスタートして，2位，3位と順番に進む
tempboy = rep(n + 1, n)  # while条件用　番人sentinel  
for(b in 1:n){
	s = b
	while(s != n + 1 ){ #sが1,2,3である間ループ*)
		k <- nextgirl[s]
		gid =prefb[[s]][k]#k=1からスタート
		if(prefg[[gid]][s] < prefg[[gid]][tempboy[gid]])#現在の相手順位＜仮の相手順位
		{	t = tempboy[gid]#仮の相手を捨てる
		print(c("t=",t))#初回はt=n+1，入れ替わるとt=以前に仮マッチした男性ID　
		tempboy[gid] = s # 仮の相手を入れ替え
		s = t  # 次のs(proposer)を指定する．
		} #if 条件が真の場合の実行文終わり
		else{#もしsが拒否されたらnextgirl[s]に1を足す
		nextgirl[s] <- nextgirl[s]+1
		}
	} # whileここで終わり
}#　for　ここで終わり

for(i in 1:n)#結果の表示
{print(paste0("男性",tempboy[i], "　と　女性",i))}
}#gs ここで終わり

###### random list generator
###### lg(n)は長さnのランダムリストをn個返す
lg <- function(n){
	x <- list()
	for(i in 1:n){
		x[[i]] <- sample(1:n)
	}
	return(x)
}


#############################################
# 計算例　
#############################################
for(i in 1:5){
	gs131(lg(3),lg(3))}


for(i in 1:5){
	gs131(lg(4),lg(4))}
