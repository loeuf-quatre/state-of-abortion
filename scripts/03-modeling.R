###############################################################################
# Title: 03-modeling.R                                                        #
# Description:                                                                #
###############################################################################

# Housekeeping ----------------------------------------------------------------

temp_dir <- tempdir()

# Import data -----------------------------------------------------------------

# Abortion regulations ------------------------------------

regs <- osf_retrieve_file("62ffe26180b0d8046cfe9148")
regs <- osf_download(regs, temp_dir, conflicts = "overwrite")

regs <- read.csv(regs$local_path)

# Demographics --------------------------------------------

# Polling -------------------------------------------------



# Model data ------------------------------------------------------------------

# Multiple correspondence analysis ------------------------

regs <- tibble::column_to_rownames(regs, "state")

regs_mca <- FactoMineR::MCA(regs, ncp = 10, graph = FALSE)

factoextra::fviz_screeplot(regs_mca)

"
~4 dimensions (~50% variance) look reasonable
"

# https://osf.io/kthnf/
# https://osf.io/2aczd/



factoextra::fviz_contrib(regs_mca, "var", axes = 1, top = 10)
factoextra::fviz_mca_var(regs_mca, "var", axes = c(1, 2))
factoextra::fviz_mca_biplot(regs_mca, axes = c(1, 2))

loads <- regs_mca$ind$coord[, 1:4] %>%
  data.frame() %>%
  clean_names()

factoextra::fviz_nbclust(loads, FUNcluster = factoextra::hcut, method = "wss")

d <- dist(loads)
hc <- hclust(d, method = "ward.D")
dend <- as.dendrogram(hc)

dend %>% color_branches(k = 5) %>% color_labels(k = 5) %>% set("labels_cex", .7) %>% set("branches_lwd", .5) %>% as.ggdend() %>% ggplot(horiz = TRUE)

regs_tidy$cluster <- cutree(hc1, k = 4)

km <- kmeans(loads[, -1], 8)
loads$clu <- km$cluster
loads$state <- ar$state

