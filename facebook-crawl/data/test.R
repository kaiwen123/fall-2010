#!/usr/local/bin/R

require(rpart)
require(tree)

dat <- read.csv("f.vis.final.labeled.csv")

cls <- rpart(label ~ sex + networks + relationships + interested_in + bio + favorite_quotations + religious_views + political_views + friendlist + photos_albums + myposts + websites + address + im_screen_name + email + facebookuri + phone + family + brithday + current_city + hometown + activities + interests + employers + grad_school + college + high_school, data=dat)

plotcp(cls)
