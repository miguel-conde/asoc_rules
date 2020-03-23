library(recosystem)

set.seed(123) # This is a randomized algorithm
train_set = data_file(system.file("dat", "smalltrain.txt", package = "recosystem"))
test_set  = data_file(system.file("dat", "smalltest.txt",  package = "recosystem"))
r = Reco()
opts = r$tune(train_set, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                     costp_l1 = 0, costq_l1 = 0,
                                     nthread = 1, niter = 10))
opts

r$train(train_set, opts = c(opts$min, nthread = 1, niter = 20))

## Write predictions to file
pred_file = tempfile()
r$predict(test_set, out_file(pred_file))

## prediction output generated at /tmp/RtmpPR5Vsw/filea441802b01
print(scan(pred_file, n = 10))

## Or, directly return an R vector
pred_rvec = r$predict(test_set, out_memory())
head(pred_rvec, 10)

## Write P and Q matrices to files
P_file = out_file(tempfile())
Q_file = out_file(tempfile())
r$output(P_file, Q_file)
head(read.table(P_file@dest, header = FALSE, sep = " "))
head(read.table(Q_file@dest, header = FALSE, sep = " "))

## Skip P and only export Q
r$output(out_nothing(), Q_file)

## Return P and Q in memory
res = r$output(out_memory(), out_memory())
head(res$P)
head(res$Q)

library(akmeans)

kk_clus <- akmeans(res$P[-220, ], d.metric = 2)
