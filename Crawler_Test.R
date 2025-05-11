
library(tidyverse)
library(Rcrawler)
library(visNetwork)


# run crawler

# capturing website url and generating homepage url

website_url <- "https://theseocorner.com" %>% str_replace("/$","")
homepage_url <- website_url %>% str_replace("(?<=//.{1,1000})/.+$","")
connections_limit <- 20

# crawling the site

Rcrawler(Website = homepage_url,
         no_cores = 4,
         no_conn = 4 ,
         NetworkData = TRUE,
         NetwExtLinks = TRUE,
         statslinks = TRUE,
         MaxDepth = 10,
         saveOnDisk = FALSE
         )

# setting up nodes according to options

nodes <- data.frame(NetwIndex) %>%
  rowid_to_column("id") %>%
  rename(Url = NetwIndex) %>%
  mutate(label = Url) %>%
  select(-Url) %>%
  filter(str_detect(label,"https")) %>% # capturing https only
  mutate(label = str_replace(label,"/$","")) %>% # removing trailing slashes
  filter(label != homepage_url) %>% # removing homepage
  distinct(label, .keep_all = TRUE) %>%
  mutate(group = ifelse(str_detect(label,homepage_url),"Internal","External"))

# setting up edges

edges_raw <- NetwEdges %>%
  select(From,To) %>%
  rename(from = From,
         to = To)

edges_connection_limit <- edges_raw %>%
  group_by(to) %>%
  summarise(connection_count = n()) %>%
  filter(connection_count <= connections_limit)

edges <- edges_raw %>%
  mutate(arrows = "to") %>%
  filter(to %in% edges_connection_limit$to)

nodes_final <- nodes %>%
  filter(id %in% edges$from | id %in% edges$to)

# generating the network

network <- visNetwork(nodes_final, edges) %>%
  visIgraphLayout(physics = FALSE) #%>%
  #visPhysics(solver = "barnesHut",barnesHut = list(gravitationalConstant = -10000))

network
