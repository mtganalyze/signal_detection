### create first pick distirbution for RIX

source("helpers.R")

set_label <- "M19"

sd <- load_setdata(set_label)
rd <- load_cardratings(set_label)

msd <- attach_ratings(sd, rd)
fps <- compute_firstpicks(msd)

set.seed(100)
samplepack <- crackapack(msd)

save(samplepack, fps, file = "results/firstpicks.Rdata")
### copy this file into the static folder of the blog directory
