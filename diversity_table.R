library(tidyverse)
library(data.table)
knitr::opts_chunk$set(echo = TRUE)
dat <- as_tibble(read.csv("diversity_v.csv"))
dat <- dat[1:(nrow(dat) - 1), ]
conflicted::conflict_prefer("transpose", "data.table")

spp_names <- str_split(names(dat), "_")
spp_names <- map(spp_names, 2)
spp_names[[53]] <- "Total"
spp_names <- c("site", unlist(spp_names))
spp_names <- str_replace(spp_names, "\\.", " ")
names(dat) <- spp_names
t_dat <- transpose(dat)
names(t_dat) <- dat$site
rownames(t_dat) <- colnames(dat)
t_dat <- t_dat[2:nrow(t_dat), ]
t_dat <- t_dat %>% 
   mutate_all(as.numeric)
t_dat$Total <- rowSums(t_dat)
rownames(t_dat) <- spp_names[2:length(spp_names)]
t_dat$Species <- spp_names[2:length(spp_names)]
t_dat <- t_dat %>% 
   select(Species, everything())
write.csv(t_dat, "diversity_table.csv", row.names = FALSE, col.names = TRUE)