#
#  Copyright (C) 2015 Yunosuke Higashi
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.


library(tm)
library(arules)

#x回以下の単語を削除(行列の次元削減)
x<-0


get.tdm <- function(doc.vec){
    #前処理用文字列置換
    #doc.vec<-gsub("\ .+\\/.+\ "," ",doc.vec)#パス消去
    #doc.vec<-gsub("\\."," ",doc.vec)#ピリオド置換
    #doc.vec<-gsub("-"," ",doc.vec)
    #doc.vec<-gsub("_"," ",doc.vec)
    #doc.vec<-gsub("\n"," ",doc.vec)
    #doc.vec<-gsub("/"," ",doc.vec)
    #doc.vec<-gsub("\\\\"," ",doc.vec)#バックスラッシュ消去
    #doc.vec<-gsub("[^\x01-\x7E]"," ",doc.vec)#文字化け用
    
    doc.corpus <- Corpus(VectorSource(doc.vec))
    
    Control <- list(stopwords=T, removePunctuation =T, removeNumbers=T,bounds=list(local = c(1, Inf)),stemming = FALSE )
    
    #TermDocumentMatrix(doc.corpus,Control)
    doc.tdm <- TermDocumentMatrix(doc.corpus,Control)
    
    doc.tdm<-weightTfIdf(doc.tdm)
    #doc.tdm<-weightTf(doc.tdm)
    #doc.tdm<-weightSMART(doc.tdm)
    #doc.tdm<-weightBin(doc.tdm)
    return(doc.tdm)
}




get_corpus <-function(tdm){
    tdm.matrix <- as.matrix(tdm)
    #print(dim(tdm.matrix))
    tdm.counts <- rowSums(tdm.matrix)
    corpus <- data.frame(cbind(names(tdm.counts),as.numeric(tdm.counts)),stringsAsFactors = F)
    names(corpus) <- c("term","frequency")
    corpus$frequency <- as.numeric(corpus$frequency)
    
    #単語出現回数ファイル出力
    #write.table(corpus, file="xxxx-corpus.csv", sep=",",quote=F,row.names=F,col.names=T,fileEncoding="utf-8" )
    return (corpus)
}



check_stopwords <- function(df){#x回出現した単語をチェック
    g<-subset(df,frequency==x,c(term,frequency))
    g<-g$term
    g<-as.matrix(g)
    #print("以下stopwords")
    #print(t(g))
    return(g)
    
}
remove_stopwords<-function(g,m){#x回出現した単語を消去
    g<-as.vector(g)
    #print(g)
    p<-length(g)
    print(sprintf("計%d個の単語を削除します",p))
    b<-ncol(m)
    m <- m[, !(colnames(m) %in% c(g))]
    a<-ncol(m)
    print(sprintf("単語数が%dから%dに減りました",b,a))
    return(m)
}




make_tdm<-function(doc.vec){
    tdm<-get.tdm(doc.vec)#tdm作成
    tdm.df<-get_corpus(tdm)#ストップワード（1回のみ出現した単語）用データフレーム作成
    stop<-check_stopwords(tdm.df)#ストップワード抽出
    tdm<-remove_stopwords(stop,t(tdm))#ストップワード削除
    tdm<-as.matrix(tdm)
    return(tdm)
}

#summary，description合体用
get_paste_tdm<-function(sum,des){
    nr<-length(sum)
    print(nr)
    doc<-matrix(nrow=nr,ncol=1)
    for(i in 1:nr){
        doc[i,1]<-paste(sum[i],des[i],sep=" ")
    
    }
    return(doc)
}




xxx<-read.csv("../Dataset/xxx.csv",fileEncoding="utf-8",colClasses="character")
#summary+description合体(したければ)
#xxx.doc<-get_paste_tdm(xxx$Summary,xxx$Description)
#description+summaryTDM作成
#xxx.tdm<-make_tdm(xxx.doc)



#descriptionTDM作成
xxx.tdm<-make_tdm(xxx$Description)



#id抽出
xxx_issues<-xxx$BugId

#issue_idとTDMの合体
xxx.tdm<-cbind(xxx_issues,xxx.tdm)



#行列サイズ確認
print(dim(xxx.tdm))


#TDM出力
write.table( xxx.tdm, file="xxx-tdm.csv", sep=",", fileEncoding="utf-8",quote=F ,row.names=F)


