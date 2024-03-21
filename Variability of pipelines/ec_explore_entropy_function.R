coin_toss <- c(rep("h",3), rep("t",3), rep("n",3)) # 9 times heads, 1 tail
probability_h <- sum(coin_toss=="h")/length(coin_toss)
probability_t <- sum(coin_toss=="t")/length(coin_toss)
probability_n <- sum(coin_toss=="n")/length(coin_toss)

info_h <- -log2(probability_h)#surprise or information for heads is 0.152
info_t <- -log2(probability_t)# surprise or information for tails is 3.32 because tails are rare but very informative
info_n <- -log2(probability_n)

# Shannon entropy would be then (total certainnes of the data)
probability_h*info_h + probability_t*info_t + probability_n*info_n# 
# The largest entropy exists if all events are equally likely

-sum(probability_h*log2(probability_h),probability_t*log2(probability_t))

factor_coin <- as.factor(coin_toss)
num_coin <- as.numeric(factor_coin)

entropy.empirical(c(probability_n,probability_t,probability_h), unit = "log2")

# ANOTHER ONE
buys <- c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no")

freqs <- table(buys)/length(buys)
prob_no <- sum(buys=="no")/length(buys)
prob_yes <- sum(buys=="yes")/length(buys)

# calculate shannon-entropy
-sum(freqs * log2(freqs))
#same as
-sum(prob_no*log2(prob_no),prob_yes*log2(prob_yes))
entropy(freqs, unit = "log2")



