install.packages("rinat")
install.packages("spocc")
library(rinat)
library(spocc)
m_obs <- get_inat_obs(query="Monarch Butterfly")
get_inat_obs_id(m_obs$id[1])
