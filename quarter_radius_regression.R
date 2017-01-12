ave_neigh_size_half <- read.csv("deviations_half_mile.csv",header=F)
names(ave_neigh_size_half) <- c("HOMEID", "aveNeighSize")
homes_with_pid <- read.csv("homes_OBJID_PID.csv")
homes_to_keep <- inner_join(ave_neigh_size_half,homes_with_pid, by=c("HOMEID"="OBJECTID"))
