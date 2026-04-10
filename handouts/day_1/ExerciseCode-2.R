cmTbm <- read.csv("Data/cmTbmData.csv", stringsAsFactors=TRUE)
cmTbm$log10.csfwcc <- log10(cmTbm$csfwcc+1)
cmTbm$sex <- factor(cmTbm$sex, labels=c("male","female"))

library(gtsummary)

tbl_summary(data=cmTbm, by=groupLong, include=c(age,bldwcc,csfwcc,sex,groupLong))

# tbl_summary(data=cmTbm, by=groupLong, include=c(age,bldwcc,csfwcc,sex,groupLong),
#             label=list(age~"age [years]", bldwcc~"white blood cells [count/mm3]",
#                        csfwcc~"white CSF cells [count/mm3]", sex~"Female"),
#             type = all_continuous() ~ "continuous2",
#     statistic=list(all_continuous()~ c("{median} ({p25}, {p75})","{mean} ({sd})")),
#             missing_text="missing",
#             value=list(sex~2))

tbl_summary(data=cmTbm, by=groupLong, include=c(age,bldwcc,csfwcc,sex,groupLong), 
            label=list(age~"age [years]", bldwcc~"white blood cells [count/mm3]",
                       csfwcc~"white CSF cells [count/mm3]", sex~"Female"),     
            type = all_continuous() ~ "continuous2",
    statistic=list(all_continuous()~ c("{median} ({p25}, {p75})","{mean} ({sd})")), 
            missing_text="missing", 
            value=list(sex~"female"))

library(ggplot2)

ggplot(cmTbm, aes(x = groupLong, y = csfwcc)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red') +
  theme_bw()
ggplot(cmTbm, aes(x = groupLong, y = log10.csfwcc)) +
  geom_boxplot() +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'red') +
  theme_bw()

ggplot(cmTbm) + 
  geom_freqpoly(aes(log10.csfwcc,colour=groupLong,after_stat(density)), 
                binwidth=0.25, linewidth=1, alpha=0.4) + theme_bw() 

ggplot(cmTbm, aes(x = log10.csfwcc, fill = groupLong)) +
  geom_density(position = 'identity', alpha = 0.4, adjust = 1.2) +
  theme_bw()

ggplot(cmTbm, aes(x = groupLong, y = log10.csfwcc, 
                  fill = groupLong, color = groupLong)) +
  geom_violin(adjust = 1.2) +
 facet_grid( . ~ sex ) +
  theme_bw()

ggplot(cmTbm, aes(x = groupLong, y = log10.csfwcc, 
                  fill = groupLong, color = groupLong)) +
  geom_violin(adjust = 1.2) +
  geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black') +
  facet_grid( . ~ sex ) +
  theme_bw()

library(ggrain)
ggplot(cmTbm, aes(x = groupLong, y = log10.csfwcc)) + geom_rain(alpha=0.3)  +  
  facet_grid( . ~ sex ) +
  xlab("") + ylab("10-log of CSF white cells [count/mm3]")+theme_bw()

ggplot(cmTbm, aes(bldwcc)) + geom_histogram(binwidth=1.5, boundary = 0) 

ggplot(cmTbm, aes(log10(bldwcc))) + geom_histogram(binwidth=0.10)

ggplot(cmTbm, aes(x = bldwcc, y = log10.csfwcc)) +
  geom_point() +
  theme_bw()

ggplot(cmTbm, aes(x = log10(bldwcc), y = log10.csfwcc)) +
  geom_point() +
  theme_bw()

cor.test(~csfwcc+bldwcc, data=cmTbm)
cor.test(~csfwcc+bldwcc, data=cmTbm, method="spearman")
cor.test(~log10.csfwcc+bldwcc, data=cmTbm)
cor.test(~log10.csfwcc+bldwcc, data=cmTbm, method="spearman")

ggplot(cmTbm, aes(x = bldwcc, y = log10.csfwcc)) +
  geom_point() +
  facet_grid( groupLong ~ sex ) +
  theme_bw()

ggplot(cmTbm, aes(x = log10(bldwcc), y = log10.csfwcc)) +
  geom_point() +
  facet_grid( groupLong ~ sex ) +
  theme_bw()

library(GGally)
ggpairs(select(cmTbm, age, log10.csfwcc, sex)) 
