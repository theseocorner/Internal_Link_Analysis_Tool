## app.R ##
library(tidyverse)
library(Rcrawler)
library(visNetwork)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(fresh)
library(DT)
library(shinyBS)
library(faq)
library(igraph)

# source modified crawler------------

print(getwd())

# non docker source

source("Rcrawler_modified.R")

# docker source

#source("./home/shiny-app/Rcrawler_modified.R")

# setting theme-------------------

dark_purple = "#350e58"
purple = "#481376"
orange = "#e54715"
beige = "#e5d2ba"
black = "#191919"

theseocorner_theme <- create_theme(
  adminlte_color(light_blue = "#e5d2ba"),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#350e58",
    dark_hover_bg = "#191919",
    dark_color = "#e54715"
  ),
  adminlte_global(
    content_bg = "#481376",
    box_bg = "#e54715",
    info_box_bg = "#e54715"
  )
)


#UI--------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Internal Link Analysis Tool",
                  #titleWidth = 300
                  tags$li(
                    a(
                      href = "https://theseocorner.com",
                      img(
                        src = "logo.png",
                        title = "The SEO Corner",
                        height = "20px"
                      ),
                      style = "padding-top:0px: padding-bottom:0px;"
                    ),
                    class = "dropdown"
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Internal Link Analysis",
        tabName = "link_analysis",
        icon = icon("fas fa-link")
      ),
      menuItem(
        "Guidance",
        tabName = "guidance",
        icon = icon("fas fa-info")
      ),
      menuItem(
        "FAQs",
        tabName = "faq",
        icon = icon("fas fa-question")
      ),
      textInput(
        "website_url",
        label = div("Website URL", icon("fas fa-question-circle")),
        value = "",
        width = NULL,
        placeholder = "https://theseocorner.com"
      ) %>% tipify("Type in the URL of the page of the site you want to crawl", placement = "bottom"),
      actionButton(
        inputId = "start_analysis",
        label = "Start Analysis",
        icon = icon("fas fa-play")
      ),
      sliderInput(
        "connection_limit",
        label = div("Connection Limit", icon("fas fa-question-circle")),
        min = 0,
        max = 100,
        value = c(20)
      ) %>% tipify(
        "Remove pages with more incoming links than you specify. Useful for removing navigational pages e.g. About Us etc.",
        placement = "bottom"
      ),
      sliderInput(
        "crawl_depth",
        label = div("Crawl Depth", icon("fas fa-question-circle")),
        min = 1,
        max = 11,
        value = c(11)
      ) %>% tipify(
        "Specify how many levels need to be crawled. E.g. Crawl Depth of 1 will show only the links from the initial page. Depth of 2 will show all pages linked to from the initial page and all pages linked to from those pages etc.",
        placement = "bottom"
      ),
      radioButtons(
        "homepage_filter",
        label = div("Remove the Hompage Links", icon("fas fa-question-circle")),
        choices = c("Yes" = "yes",
                    "No" = "no"),
        selected = "yes"
      ) %>% tipify(
        "Remove homepage from the analysis. Useful for zeroing in on content pages and cleaning up the network.",
        placement = "bottom"
      ),
      radioButtons(
        "internal_filter",
        label = div("Remove External Links", icon("fas fa-question-circle")),
        choices = c("Yes" = "yes",
                    "No" = "no"),
        selected = "no"
      ) %>% tipify(
        "Remove links linking out from your chosen website to external domains.",
        placement = "bottom"
      ),
      textAreaInput(
        "url_component_filter",
        label = div(
          "List All URL Components to Exclude (Comma Delimited)",
          icon("fas fa-question-circle")
        ),
        placeholder = "terms-and-conditions,contact-us"
      ) %>% tipify(
        "Remove all pages that contain specified terms in URLs. Useful for removing cookie pages, about pages etc.",
        placement = "bottom"
      )
    )
  ),
  dashboardBody(
    use_theme(theseocorner_theme),
    # stylesheet
    tags$head(tags$style(
      HTML(
        '
        .skin-blue .main-header .logo {
          color:#191919;
        }
        .skin-blue .main-header .navbar .sidebar-toggle {
          background-color: #e5d2ba;
          color:#191919;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #e5d2ba;
          color:#191919;
        }
        .skin-blue .main-header .navbar .sidebar-toggle:hover {
          background-color: #e54715;
          color:#191919;
        }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a {
        border-left-color: #e54715;
        background-color: #e54715;
        color:#FFFFFF;
        }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li>a {
        color:#FFFFFF;
        }
        .skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:active>a {
        color:#191919;
        border-left-color: #e54715;
        background-color: #e54715;
        }
        .irs--shiny .irs-bar {
        background: #e54715;
        border-top: #e54715;
        border-bottom: #e54715;
        }
        .irs--shiny .irs-single {
        color: #191919;
        background-color: #e54715;
        }
        .btn-default {
        color: #191919;
        background-color: #e54715;
        border: #e54715
        }
        .btn-default:hover {
        color: #FFFFFF;
        background-color: #e54715;
        border: #e54715
        }
        .btn-default:focus {
        color: #FFFFFF;
        background-color: #e54715;
        border: #e54715
        }
        .datatables {
        background-color: #e54715;
        color: #191919
        }
        .dataTables_scroll {
        background-color: #350e58;
        color: #FFFFFF
        }
        .faqcollapsible {
        color: #191919;
        background-color: #e5d2ba;
        }
        .faq-package .active, .faqcollapsible:hover {
        color: #FFFFFF;
        background-color: #e54715;
        }
        .faq-expand-all:hover {
        color: #FFFFFF;
        background-color: #e54715;
        }
        .faq-expand-all {
        color: #191919;
        background-color: #e5d2ba;
        }
        .faqcontent {
        color: #FFFFFF;
        background-color: #350e58 !important;
        }
        .faqwrapper {
        background-color: #e5d2ba;
        border-bottom: #e5d2ba;
        }
        h2 {
        color: #e54715;
        }
        a {
        color:#e54715
        }

      '
      )
    )),
    tabItems(
      # First tab content
      tabItem(tabName = "link_analysis",
              fluidRow(
                column(6,
                       withSpinner(
                         visNetworkOutput("internal_links_network", height = "800px"),
                         color = getOption("spinner.color", default = "#e54715")
                       )),
                column(6,
                       dataTableOutput("link_count_table"))
              )),
      
      # Second tab content
      tabItem(tabName = "guidance",
              span(
                markdown(
                  "

# <span style='color:#e54715'>Internal Link Analysis Tool Guidance</span>

Thank you for using our internal link analysis tool. To help you with your analysis,
we’ve prepared a handy guide that walks you through the most important features of the tool
and also covers some ways you can use it to supercharge your website's SEO.

We have divided the guide into multiple sections. The initial sections explain how to use the tool,
while the later sections explore different applications of the Internal Link Analysis Tool.

## <span style='color:#e54715'>How to Use the Internal Link Analysis Tool - The Basics</span>

The tool enables you to visualise the internal linking structure of virtually any website that doesn't specifically block crawlers.

First, let's go over the key components of the tool:

![Key Components of the Internal Link Analysis Tool](Internal-Linking-Analysis-Tool-Overview-Image-Annotated.png)

1. **Website URL** – Insert the URL of the website you wish to analyse.
   Note that this URL can be a homepage if you're looking to run a full site link analysis,
   or a specific page if you're investigating a certain part of the internal link structure.

2. **Connection Limit Slider** – Use this slider to filter out nodes with too many links pointing to them.
   This is very useful for bulk filtering out main navigational pages, such as blog page URLs or footer links.
   Setting the slider to 0 will remove the limits entirely.

3. **Crawl Depth** – This slider governs how many levels of links to crawl.
   For example, a crawl depth of 1 will show only the pages linked from the initial page.
   A crawl depth of 2 will include those pages and the ones they link to, and so on.
   Note: You will need to re-run the crawl after adjusting the slider by pressing the Start Analysis button.

4. **Remove the Homepage Links** – Use these radio buttons to filter out the homepage of the analysed website.
   This can help clean up the network and make clusters more visible.

5. **Remove External Links** – Use these radio buttons to remove links leading to other domains.
   Ideal if you want to focus solely on internal links.

6. **List All URL Components to Exclude** – Enter terms (comma-separated) to filter out specific URLs.
   You can exclude exact pages, pages containing certain terms (e.g. `terms-and-conditions`),
   or entire URL stems (e.g. `https://theseocorner.com/case-studies`).

7. **Internal Link Network Map** – A map representing the website's internal link structure.
   Scroll to zoom in/out. As you zoom, you’ll see individual page URLs represented by nodes.
   Click a node to highlight it and its neighbours. Nodes can be dragged as needed.

8. **Node Selector** – Use this field to highlight and select nodes of interest.

9. **URL Table** – Displays all URLs in the network.
   Clicking a row will highlight the corresponding node in the network.

10. **Inbound Link Count Column** – Shows how many other pages link **to** each URL.
    Click the column heading to sort ascending/descending.
    Easily identify which URLs receive the most or fewest links.

11. **Outbound Link Count Column** – Shows how many other pages are linked **from** each URL.
    Sortable like the inbound column, so you can identify pages with the most or fewest outbound links.

12. **URL Search Bar** – Search for specific URLs in the network.

13. **Copy and Export to CSV Buttons** – Export your current table view to CSV or copy it to your clipboard.

## <span style='color:#e54715'>Basic Workflow</span>

Every analysis starts by entering the URL, setting the desired crawl depth, and hitting the Start Analysis button.
The crawler opens the initial URL and finds links on that page, then follows those links and continues analysing subsequent pages.
This continues until the entire site is analysed or the specified crawl depth is reached.

**Note: This process can take a while, especially for larger sites, so be patient.**

Once crawling is complete, a network map will load, and the summary table will update with the site's URLs.
If the network looks clean, you can begin the analysis. If it's messy, try the following:

* Lower the connection limit to filter out heavily linked navigational pages
* Remove external links
* Remove homepage links
* Use the exclusion box in the sidebar to filter out unnecessary navigational pages

As shown below, the example network is very messy. Let’s clean it up using the tips above.

![Example of a messy network](messy-network-example.png)

After applying the tips, the network looks much cleaner and far more usable:

![Example of a clean network after applying the above tips](clean-network-example.png)

Now we can begin the analysis.

## <span style='color:#e54715'>Useful Applications of the Internal Link Analysis Tool - The Basics</span>

Now that you know how to use the tool, let’s explore some practical applications for analysing your own website or competitors’.

### <span style='color:#e54715'>Spot Internal Linking Opportunities</span>

One key use of the tool is identifying articles with few internal links, which could benefit from more thorough linking.

To summarise:

* Look for lone nodes. Click to highlight and examine what links to them.
  This may highlight opportunities to create topical clusters.

* Use the URL table to identify pages with the fewest inbound links.
  Click those URLs to highlight them in the network map.

### <span style='color:#e54715'>Understand How Your Competitors Structure Their Sites</span>

You can also use the tool to examine how competitors structure their websites.
Try crawling specific sections with a low crawl depth to uncover topical clusters and structural patterns.

To do this:

* Enter your competitor’s URL
* Use the network map and URL table search to navigate and highlight key pages

## <span style='color:#e54715'>Issues and Bug Reporting</span>

We're always on the lookout for new feature suggestions and feedback to improve the tool.
If you encounter bugs or have ideas for enhancements, please visit [our contact page](https://theseocorner.com/contact-us) and drop us a message.

    "
                ),
                style = paste0("color:", beige)
              )),
      tabItem(tabName = "faq",
              fluidRow(faq::faq(
                data = data.frame(
                  question = c("1. What is the purpose of the Internal Link Analysis Tool?",
                               "2. Can I use this tool on any website?",
                               "3. Why is my network graph so messy?",
                               "4. How do I find pages with poor internal linking?",
                               "5. What’s the difference between crawl depth and connection limit?",
                               "6. Do you collect and store the crawl data?",
                               "7. Where do I report feature suggestions and issues/ bugs?"
                               ),
                  answer = c("The Internal Link Analysis Tool helps you visualise the structure of internal links on a website. This lets you identify key pages, detect underlinked content, and better understand how link equity flows through your site—crucial for SEO performance and content discoverability.",
                             "Yes, you can use it on almost any website that does not block crawlers (e.g. via robots.txt). You can analyse your own site or review a competitor's internal linking structure for strategic insights.",
                             "A cluttered graph usually means there are many interconnected navigational or footer links. To clean it up:
                             
                             <ul> <li>Lower the connection limit</li>
                             <li>Remove homepage or external links</li>
                             <li>Use the URL exclusion filter to omit common navigational elements</li></ul>",
                             "You can:
                             
                             <ul> <li>Look for isolated nodes in the network map (indicating few or no internal links)</li>
                             <li>Sort the URL table by Inbound Link Count to find pages with the fewest internal links</li></ul>
                             
                             These pages might need additional links pointing to them to improve SEO visibility.",
                             "<ul> <li>Crawl Depth controls how many levels deep the crawler will follow links from the initial page</li>.
                             <li>Connection Limit filters out nodes (pages) with too many incoming or outgoing links, often navigational pages that clutter the network graph.</li></ul>",
                             "No, we do not store the data from your crawl requests. You can find out more about our policies in our <a href='https://theseocorner.com/privacy-policy'>privacy policy</a> document.",
                             "We're always on the lookout for new feature suggestions and feedback to improve the tool. If you encounter bugs or have ideas for enhancements, please visit <a href='https://theseocorner.com/contact-us'>our contact page</a> and drop us a message."
                             ),
                  elementId = "faq",
                  faqtitle = "Frequently Asked Questions"
                )
                
              )))
    )
  )
)

# Server----------------------

server <- function(input, output, session) {
  
  # process input on excluded url values
  
  url_component_filter_processed <- reactive({
    if (input$url_component_filter == "") {
      # plugging long empty space to avoid errors
      
      url_component_filter_processed <-
        "                                                                    "
      
    } else {
      url_component_filter_processed <-
        str_split(input$url_component_filter, ",|, | ,")[[1]] %>%
        str_trim(., side = "both")
    }
    
    return(url_component_filter_processed)
    
  })
  
  # setting checkpoint value to link events and tables selection value to link to network
  
  values <- reactiveValues(checkpoint = 0,
                           NetwEdges = NULL,
                           NetwIndex = NULL,
                           INDEX = NULL)
  
  # Run analysis on button press
  
  observeEvent(input$start_analysis, {
    withProgress(min = 0, max = 100, {
      values$checkpoint <- values$checkpoint + 1
      
      setProgress(value = 10, message = "Setting checkpoints")
      
      Sys.sleep(1)
      
      # run the analysis script
      
      # run crawler
      
      # capturing website url and generating homepage url
      
      website_url <- tolower(input$website_url) %>% str_replace("/$", "")
      homepage_url <-
        website_url %>% str_replace("(?<=//.{1,1000})/.+$", "")
      connections_limit <-
        ifelse(input$connection_limit > 0,
               input$connection_limit,
               99999999999999999)
      
      setProgress(value = 23, message = "Setting key assumptions")
      
      Sys.sleep(1)
      
      setProgress(value = 28, message = "Checking Intenet access")
      
      # tryCatch({
      #   print(readLines("https://httpbin.org/get"))
      # }, error = function(e) {
      #   print("Internet access error:")
      #   print(e)
      # })
      
      setProgress(value = 32, message = "Crawling the site. This may take a while...")
      
      # crawling the site
      
      tryCatch({
        crawl_data <- Rcrawler_modified(
          Website = website_url,
          no_cores = 4,
          no_conn = 4 ,
          NetworkData = TRUE,
          NetwExtLinks = TRUE,
          statslinks = TRUE,
          MaxDepth = input$crawl_depth - 1,
          saveOnDisk = FALSE
        )
      }, error = function(e) {
        showNotification("Error. Did you input a correct URL?", type = "error")
      })
      
      
      values$checkpoint <- values$checkpoint + 1
      
      # passing reactive values for use in later functions
      
      values$NetwEdges <- crawl_data$NetwEdges
      values$NetwIndex <- crawl_data$NetwIndex
      values$INDEX <- crawl_data$INDEX
      
      setProgress(value = 100, message = "Finished crawling, creating charts.")
      
      
      
    })
    
  })
  
  network_generated <- reactive({
    
    # converting reactive values to local objects
    
    NetwEdges <- values$NetwEdges
    NetwIndex <- values$NetwIndex
    INDEX <- values$INDEX
    
    
    # setting the key option values
    
    values$checkpoint
    website_url <- tolower(input$website_url) %>% str_replace("/$", "")
    homepage_url <-
      website_url %>% str_replace("(?<=//.{1,1000})/.+$", "")
    connections_limit <-
      ifelse(input$connection_limit > 0,
             input$connection_limit,
             99999999999999999)
    
    if (!is.null(NetwEdges)) {
      # setting up nodes according to options
      
      if (input$homepage_filter == "yes") {
        nodes <- data.frame(NetwIndex) %>%
          rowid_to_column("id") %>%
          rename(Url = NetwIndex) %>%
          mutate(label = Url) %>%
          select(-Url) %>%
          filter(str_detect(label, "https")) %>% # capturing https only
          mutate(label = str_replace(label, "/$", "")) %>% # removing trailing slashes
          filter(label != homepage_url) %>% # removing homepage
          distinct(label, .keep_all = TRUE) %>%
          mutate(group = ifelse(
            str_detect(label, homepage_url),
            "Internal",
            "External"
          )) %>%
          mutate(color = ifelse(group == "Internal", "#e54715", "#e5d2ba"))
        
      } else {
        nodes <- data.frame(NetwIndex) %>%
          rowid_to_column("id") %>%
          rename(Url = NetwIndex) %>%
          mutate(label = Url) %>%
          select(-Url) %>%
          filter(str_detect(label, "https")) %>% # capturing https only
          mutate(label = str_replace(label, "/$", "")) %>% # removing trailing slashes
          #filter(label != homepage_url) %>% # removing homepage
          distinct(label, .keep_all = TRUE) %>%
          mutate(group = ifelse(
            str_detect(label, homepage_url),
            "Internal",
            "External"
          )) %>%
          mutate(color = ifelse(group == "Internal", "#e54715", "#e5d2ba"))
        
        
        
      }
      
      if (input$internal_filter == "yes") {
        nodes <- nodes %>%
          #filter(id %in% edges$from | id %in% edges$to) %>%
          filter(group == "Internal") %>%
          filter(!str_detect(
            label,
            paste(url_component_filter_processed(), collapse = "|")
          ))
        
      } else {
        nodes <- nodes %>%
          #filter(id %in% edges$from | id %in% edges$to) %>%
          filter(!str_detect(
            label,
            paste(url_component_filter_processed(), collapse = "|")
          ))
        
      }
      
      
      # setting up edges
      
      edges_raw <- NetwEdges %>%
        select(From, To) %>%
        rename(from = From,
               to = To) %>%
        filter(from %in% nodes$id & to %in% nodes$id)
      
      edges_connection_limit <- edges_raw %>%
        group_by(to) %>%
        summarise(connection_count = n()) %>%
        filter(connection_count <= connections_limit)
      
      edges <- edges_raw %>%
        mutate(arrows = "to") %>%
        filter(to %in% edges_connection_limit$to)
      
      # setting up final nodes based on filtered edges and whether external links are desired
      
      
      if (input$internal_filter == "yes") {
        nodes_final <- nodes %>%
          filter(id %in% edges$from | id %in% edges$to) #%>%
        #filter(group == "Internal") %>%
        #filter(!str_detect(label,paste(url_component_filter_processed(),collapse = "|")))
        
      } else {
        nodes_final <- nodes %>%
          filter(id %in% edges$from | id %in% edges$to) #%>%
        #filter(!str_detect(label,paste(url_component_filter_processed(),collapse = "|")))
        
      }
      
      # generating the network. Different process depending on if node was selected on table
      
      if (!is.null(selected_url())) {
        #print(link_count_table()[[input$link_count_table_rows_selected,"URL"]])
        
        network <- visNetwork(nodes_final, edges) %>%
          visIgraphLayout(physics = FALSE) %>%
          visNodes(font = list(color = "#e5d2ba")) %>%
          visOptions(
            selectedBy = list(variable = "label",
                              selected = selected_url()),
            # nodesIdSelection = list(enabled = TRUE,
            #                                  selected = selected_url()
            #                                  ),
            highlightNearest = list(enabled = TRUE,
                                    degree = 0)
          )
        
        
      } else {
        network <- visNetwork(nodes_final, edges) %>%
          visIgraphLayout(physics = FALSE) %>%
          visNodes(font = list(color = "#e5d2ba")) %>%
          visOptions(
            selectedBy = list(variable = "label"),
            # nodesIdSelection = list(enabled = TRUE
            #                                  ),
            highlightNearest = list(enabled = TRUE,
                                    degree = 0)
          )
        
        
      }
      
    } else {
      
    }
    
  })
  
  link_count_table <- reactive({
    
    # converting reactive values to local objects
    
    NetwEdges <- values$NetwEdges
    NetwIndex <- values$NetwIndex
    INDEX <- values$INDEX
    
    # responsive vars
    
    values$checkpoint
    
    # setting the key option values
    
    
    website_url <- input$website_url %>% str_replace("/$", "")
    homepage_url <-
      website_url %>% str_replace("(?<=//.{1,1000})/.+$", "")
    connections_limit <-
      ifelse(input$connection_limit > 0,
             input$connection_limit,
             99999999999999999)
    
    if (!is.null(NetwEdges)) {
      # setting up nodes according to options
      
      if (input$homepage_filter == "yes") {
        nodes <- data.frame(NetwIndex) %>%
          rowid_to_column("id") %>%
          rename(Url = NetwIndex) %>%
          mutate(label = Url) %>%
          select(-Url) %>%
          filter(str_detect(label, "https")) %>% # capturing https only
          mutate(label = str_replace(label, "/$", "")) %>% # removing trailing slashes
          filter(label != homepage_url) %>% # removing homepage
          distinct(label, .keep_all = TRUE) %>%
          mutate(group = ifelse(
            str_detect(label, homepage_url),
            "Internal",
            "External"
          )) %>%
          mutate(color = ifelse(group == "Internal", "#e54715", "#e5d2ba"))
        
      } else {
        nodes <- data.frame(NetwIndex) %>%
          rowid_to_column("id") %>%
          rename(Url = NetwIndex) %>%
          mutate(label = Url) %>%
          select(-Url) %>%
          filter(str_detect(label, "https")) %>% # capturing https only
          mutate(label = str_replace(label, "/$", "")) %>% # removing trailing slashes
          #filter(label != homepage_url) %>% # removing homepage
          distinct(label, .keep_all = TRUE) %>%
          mutate(group = ifelse(
            str_detect(label, homepage_url),
            "Internal",
            "External"
          )) %>%
          mutate(color = ifelse(group == "Internal", "#e54715", "#e5d2ba"))
        
        
        
      }
      
      if (input$internal_filter == "yes") {
        nodes <- nodes %>%
          #filter(id %in% edges$from | id %in% edges$to) %>%
          filter(group == "Internal") %>%
          filter(!str_detect(
            label,
            paste(url_component_filter_processed(), collapse = "|")
          ))
        
      } else {
        nodes <- nodes %>%
          #filter(id %in% edges$from | id %in% edges$to) %>%
          filter(!str_detect(
            label,
            paste(url_component_filter_processed(), collapse = "|")
          ))
        
      }
      
      
      # setting up edges
      
      edges_raw <- NetwEdges %>%
        select(From, To) %>%
        rename(from = From,
               to = To) %>%
        filter(from %in% nodes$id & to %in% nodes$id)
      
      edges_connection_limit <- edges_raw %>%
        group_by(to) %>%
        summarise(connection_count = n()) %>%
        filter(connection_count <= connections_limit)
      
      edges <- edges_raw %>%
        mutate(arrows = "to") %>%
        filter(to %in% edges_connection_limit$to)
      
      # setting up final nodes based on filtered edges and whether external links are desired
      
      
      if (input$internal_filter == "yes") {
        nodes_final <- nodes %>%
          filter(id %in% edges$from | id %in% edges$to) #%>%
        #filter(group == "Internal") %>%
        #filter(!str_detect(label,paste(url_component_filter_processed(),collapse = "|")))
        
      } else {
        nodes_final <- nodes %>%
          filter(id %in% edges$from | id %in% edges$to) #%>%
        #filter(!str_detect(label,paste(url_component_filter_processed(),collapse = "|")))
        
      }
      
      # generating the network table
      
      # counting up inbound links satisfying filters
      
      inbound_link_count_table <- edges %>%
        left_join(.,
                  select(.data = nodes_final, id, label),
                  by = c("from" = "id")) %>%
        rename(from_url = label) %>%
        left_join(., select(.data = nodes_final, id, label), by = c("to" = "id")) %>%
        rename(to_url = label) %>%
        filter(!is.na(from_url) & !is.na(to_url)) %>%
        group_by(to_url, to) %>%
        summarise(inbound_link_count = n()) %>%
        select(to_url, inbound_link_count) %>%
        rename(url = to_url)
      
      # counting up outbound links satisfying filters
      
      outbound_link_count_table <- edges %>%
        left_join(.,
                  select(.data = nodes_final, id, label),
                  by = c("from" = "id")) %>%
        rename(from_url = label) %>%
        left_join(., select(.data = nodes_final, id, label), by = c("to" = "id")) %>%
        rename(to_url = label) %>%
        filter(!is.na(from_url) & !is.na(to_url)) %>%
        group_by(from_url, from) %>%
        summarise(outbound_link_count = n()) %>%
        select(from_url, outbound_link_count) %>%
        rename(url = from_url)
      
      # creating full table
      
      link_count_table <-
        full_join(inbound_link_count_table,
                  outbound_link_count_table,
                  by = "url") %>%
        mutate(
          inbound_link_count = ifelse(is.na(inbound_link_count), 0, inbound_link_count),
          outbound_link_count = ifelse(is.na(outbound_link_count), 0, outbound_link_count)
        )
      
      
    } else {
      link_count_table <-
        data.frame(
          url = "example.com",
          inbound_link_count = 0,
          outbound_link_count = 0
        )
      
    }
    
    return(link_count_table)
    
  })
  
  # logic for selecting rows from table to highlight in visnetwork
  
  selected_url <- reactive({
    if (!is.null(input$link_count_table_rows_selected)) {
      selected_url <-
        link_count_table()[[input$link_count_table_rows_selected, 1]]
      
    } else {
      selected_url <- NULL
      
    }
    #print(selected_url)
    return(selected_url)
    
  })
  
  
  
  output$internal_links_network <-
    renderVisNetwork(network_generated())
  output$link_count_table <-
    renderDataTable(
      link_count_table() %>% rename(
        URL = url,
        `Inbound Link Count` = inbound_link_count,
        `Outbound Link Count` = outbound_link_count
      ),
      extensions = 'Buttons',
      selection = 'single',
      options = list(
        scrollX = TRUE,
        dom = 'lfrtiBp',
        buttons = c('copy', 'csv'),
        pageLength = 10,
        lengthMenu = c(10, 20, 50, 100, 500)
      ),
      rownames = FALSE
    )

}

shinyApp(ui, server)