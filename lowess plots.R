
install.packages("patchwork")
library(rstatix)
library(patchwork)

###lowess plot and knowledge
know<-bank[,c(3,18:33)]
att<-bank[,c(3,6:17)]
att$meana<-apply(att[2:13], 1,mean,na.rm = TRUE)

var_names <- grep("know", names(know), value = TRUE)

for(var in var_names){
  know[[var]] <- ifelse(know[[var]] == 1, 1, 0)
}

know$meank<-apply(know[2:17],1,mean,na.rm=TRUE)

lowAK<-left_join(att,know,by = c("ID_formerge" = "ID_formerge"))

l<-lowess(lowAK$meank,lowAK$meana)


plot1<-plot(lowAK$meank,lowAK$meana,  main = "Lowess plot Attitude & Knowledge", xlab ="Knowledge", ylab = "Attitude")

lines(l, col="red")
lines(l, col = 4)


#Colombia

knowC<-colo[,c(3,18:33)]
attC<-colo[,c(3,6:17)]
attC$meana<-apply(attC[2:13], 1,mean,na.rm = TRUE)

var_names <- grep("know", names(knowC), value = TRUE)

for(var in var_names){
  knowC[[var]] <- ifelse(knowC[[var]] == 1, 1, 0)
}

knowC$meank<-apply(knowC[2:17],1,mean,na.rm=TRUE)

lowAKC<-left_join(attC,knowC,by = c("ID_formerge.1" = "ID_formerge.1"))

lc<-lowess(lowAKC$meank,lowAKC$meana)

plot(lowAKC$meank,lowAKC$meana, main = "Lowess plot Attitude & Knowledge", xlab ="Knowledge", ylab = "Attitude")

lines(l, col = 3)
lines(lc, col = 4)



#Nicaragua

knowN<-Nic[,c(3,18:33)]
attN<-Nic[,c(3,6:17)]
attN$meana<-apply(attN[2:13], 1,mean,na.rm = TRUE)

var_names <- grep("know", names(knowN), value = TRUE)

for(var in var_names){
  knowN[[var]] <- ifelse(knowN[[var]] == 1, 1, 0)
}

knowN$meank<-apply(knowN[2:17],1,mean,na.rm=TRUE)

lowAKN<-left_join(attN,knowN,by = c("ID_formerge.1" = "ID_formerge.1"))

ln<-lowess(lowAKN$meank,lowAKN$meana)

plot(lowAKN$meank,lowAKN$meana, main = "Lowess plot Attitude & Knowledge", xlab ="Knowledge", ylab = "Attitude")

lines(l, col = c('red'))
lines(lc, col = c('purple'))
lines(ln, col = c('steelblue'))
legend('topleft',
       col = c('red', 'purple', 'steelblue'),
       lwd = 2,
       c('Overall', 'Colombia', 'Nicaragua'))

#lowess plot knowledge and self efficacy

se<-bank[,c(3,34:45)]
se$meana<-apply(se[2:13], 1,mean,na.rm = TRUE)


lowseK<-left_join(se,know,by = c("ID_formerge" = "ID_formerge"))
lse<-lowess(lowseK$meank,lowseK$meana)

plot2<-plot(lowseK$meank,lowseK$meana, main = "Lowess plot Self efficacy & Knowledge", xlab ="Knowledge", ylab = "Self-Efficacy")
lines(lse, col = 2)


#colombia

seC<-colo[,c(3,34:45)]

seC$meana<-apply(seC[2:13], 1,mean,na.rm = TRUE)

lowseKc<-left_join(seC,knowC,by = c("ID_formerge.1" = "ID_formerge.1"))
lseC<-lowess(lowseKc$meank,lowseKc$meana)
plot(lowseKc$meank,lowseKc$meana, main = "Lowess plot Self efficacy & Knowledge")
lines(lseC, col = 3)

#Nicragua

seN<-Nic[,c(3,34:45)]

seN$meana<-apply(seN[2:13], 1,mean,na.rm = TRUE)

lowseKn<-left_join(seN,knowN,by = c("ID_formerge.1" = "ID_formerge.1"))
lseN<-lowess(lowseKn$meank,lowseKn$meana)
plot(lowseKn$meank,lowseKn$meana, main = "Lowess plot Self efficacy & Knowledge")
lines(lseN, col = 5)


lines(lse, col = c('red'))
lines(lseC, col = c('purple'))
lines(lseN, col = c('steelblue'))
legend('topleft',
       col = c('red', 'purple', 'steelblue'),
       lwd = 2,
       c('Overall', 'Colombia', 'Nicaragua'))

combined_plot <- ggarrange(plot1,
                           plot2,
                           nrow = 2,
                           ncol = 1)
combined_plot

init <- par(no.readonly=TRUE)

# specify that 4 graphs to be combined and filled by rows
par(mfrow = c(1, 2))

plot1<-plot(lowAK$meank,lowAK$meana,  main = "Plot A. Biobanking attitudes vs. knowledge ", xlab ="Knowledge", ylab = "Attitude")

lines(l, col = c('red'))
lines(lc, col = c('purple'))
lines(ln, col = c('steelblue'))
legend('topleft',
       col = c('red', 'purple', 'steelblue'),
       lwd = 2,
       c('Overall', 'Colombia', 'Nicaragua'))

plot2<-plot(lowseK$meank,lowseK$meana, main = "Plot B. Biobanking self-efficacy vs. knowledge", xlab ="Knowledge", ylab = "Self-Efficacy")

lines(lse, col = c('red'))
lines(lseC, col = c('purple'))
lines(lseN, col = c('steelblue'))
legend('topleft',
       col = c('red', 'purple', 'steelblue'),
       lwd = 2,
       c('Overall', 'Colombia', 'Nicaragua'))



