#R统计
install.packages('survival')
library(survival)
a <- read.table('~.txt',sep = "\t",header = TRUE)
head(a)
survdiff(Surv(time,status) ~ TP53_mutation , data = a)#组间比较
fit <- survfit(Surv(time,status) ~ TP53_mutation, 
               data = a)#估计
plot(fit,col = 2:3,lwd = 1.5,xlab = 'months',
     ylab = 'survival')
legend("topright",c("TP53 WT",'TP53 mutation'),
       col = 2:3,lty = 1,lwd = 1,5)
text(30 , 0.8 , "p=0.026")
#cox回归分析
data <- read.table('D:/share/q2/result/cox.txt',sep = "\t",header = TRUE)
coxph(Surv(followup_time, status) ~ gender + Tstage +
        Nstage + Mstage +age, data = data)
#~后面代表要检查的数据
#coef系数exp(coef)风险比se(coef)

#t检验
#正态总体均值的假设检验
data <- scan('sugar.txt')
t.test(data, m = 500,alternative = "less")#alt备择假设

w = scan("exh.txt")
t.test(w, m=20,alternative = "greater")
#双尾检验
#省略alter
t.test(w,m=20)

w = data.frame(value=runif(250,1,5),
               group=c(rep(1,150),rep(2,100)))
#w=read.table("drug.txt",header = T)
A <- w[w$group == 1,1]
B <- w[w$group == 2,1]
t.test(A,B,m=0,alternative = "greater")
#相关性分析
b = read.table("exp.txt",sep = "\t",header = T)
head(b)
cor.test(b$miRNA,b$mRNA)
plot(b$miRNA,b$mRNA,xlab = "miRNA expression",
     ylab = "mRNA expression")
?cor.test
