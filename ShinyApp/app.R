library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse)
library(jsonlite)
library(tidygraph)
library(ggraph)
library(SmartEDA)
library(igraph)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(visNetwork)
library(patchwork)

# --- Data Preparation ---
MC3 <- fromJSON("data/MC3_graph.json")
mc3_nodes <- as_tibble(MC3$nodes)
mc3_edges <- as_tibble(MC3$edges)

mc3_nodes_cleaned <- mc3_nodes %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-thing_collected)

mc3_edges_cleaned <- mc3_edges %>%
  rename(from_id = source, to_id = target) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  filter(from_id %in% mc3_nodes_cleaned$id,
         to_id %in% mc3_nodes_cleaned$id) %>%
  filter(!is.na(from_id), !is.na(to_id))

node_index_lookup <- mc3_nodes_cleaned %>%
  mutate(.row_id = row_number()) %>%
  select(id, .row_id)

mc3_edges_indexed <- mc3_edges_cleaned %>%
  left_join(node_index_lookup, by = c("from_id" = "id")) %>%
  rename(from = .row_id) %>%
  left_join(node_index_lookup, by = c("to_id" = "id")) %>%
  rename(to = .row_id) %>%
  select(from, to, is_inferred, type) %>%
  filter(!is.na(from) & !is.na(to))

used_node_indices <- sort(unique(c(mc3_edges_indexed$from, mc3_edges_indexed$to)))
mc3_nodes_final <- mc3_nodes_cleaned %>%
  slice(used_node_indices) %>%
  mutate(new_index = row_number())

old_to_new_index <- tibble(
  old_index = used_node_indices,
  new_index = seq_along(used_node_indices)
)

mc3_edges_final <- mc3_edges_indexed %>%
  left_join(old_to_new_index, by = c("from" = "old_index")) %>%
  rename(from_new = new_index) %>%
  left_join(old_to_new_index, by = c("to" = "old_index")) %>%
  rename(to_new = new_index) %>%
  select(from = from_new, to = to_new, is_inferred, type)

mc3_graph <- tbl_graph(
  nodes = mc3_nodes_final,
  edges = mc3_edges_final,
  directed = TRUE
)

comm_nodes <- mc3_nodes_final %>%
  filter(type == "Event", sub_type == "Communication") %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  filter(!is.na(timestamp)) %>%
  mutate(date = as.Date(timestamp),
         hour = hour(timestamp))

# --- sidebar menu text wrapping ---
css_code <- "
.main-sidebar .sidebar-menu > li > a {
  white-space: normal !important;
  height: auto !important;
  line-height: 1.2 !important;
  padding: 10px 15px !important;
}
"

# --- UI ---
ui <- dashboardPage(
  dashboardHeader(
    title = "Group 8 Project: VAST Challenge 2025 Mini-Challenge 3",
    titleWidth = 500
  ),
  dashboardSidebar(
    width = 320,
    tags$head(tags$style(HTML(css_code))),
    # --- Add image at the top of the sidebar ---
    tags$div(
      style = "text-align:center; padding:20px 0 10px 0;",
      tags$img(src = "Logo.png", height = "180px", style = "max-width:95%;")
    ),
    sidebarMenu(
      id = "menu",
      menuItem("Communication Patterns Analysis", icon = icon("comments"), startExpanded = TRUE,
               menuSubItem("Daily", tabName = "daily"),
               menuSubItem("Hourly", tabName = "hourly"),
               menuSubItem("Heatmap", tabName = "heatmap"),
               menuSubItem("Messages", tabName = "messages"),
               menuSubItem("Network Graph", tabName = "network")
      ),
      # --- Clustering Section ---
      menuItem("Clustering", icon = icon("project-diagram"), startExpanded = TRUE,
               menuSubItem("Louvain Network", tabName = "louvain"),
               menuSubItem("Infomap Network", tabName = "infomap"),
               menuSubItem("Algorithm Comparison", tabName = "algorithm"),
               menuSubItem("Performance Analysis", tabName = "performance")
      )
    )
  ),
  dashboardBody(
    shinyDashboardThemes(theme = "blue_gradient"),
    # Only show filters for Communication Patterns Analysis tabs
    conditionalPanel(
      condition = "['daily','hourly','heatmap','messages','network'].includes(input.menu)",
      fluidRow(
        column(width = 9),
        column(width = 3, align = "right",
               dateRangeInput("date_filter", "Date Range:",
                              start = min(comm_nodes$date),
                              end = max(comm_nodes$date)),
               sliderInput("hour_filter", "Hour Range:",
                           min = 0, max = 23, value = c(0, 23))
        )
      )
    ),
    tabItems(
      tabItem(tabName = "daily",
              box(width = 12, plotOutput("daily_plot"))
      ),
      tabItem(tabName = "hourly",
              box(width = 12, plotOutput("hourly_plot"))
      ),
      tabItem(tabName = "heatmap",
              box(width = 12, plotlyOutput("heatmap"))
      ),
      tabItem(tabName = "messages",
              box(width = 12, DTOutput("messages_table"))
      ),
      tabItem(tabName = "network",
              box(width = 12, visNetworkOutput("network_plot", height = "800px"))
      ),
      # --- Clustering Tabs ---
      tabItem(tabName = "louvain",
              box(width = 12,
                  h3("🥇 Louvain Clustering - Best Performance"),
                  visNetworkOutput("louvain_network", height = "800px"))
      ),
      tabItem(tabName = "infomap",
              box(width = 12,
                  h3("📊 Infomap Clustering - High Granularity"),
                  visNetworkOutput("infomap_network", height = "800px"))
      ),
      tabItem(tabName = "algorithm",
              box(width = 12,
                  h3("📈 Algorithm Performance Comparison"),
                  plotOutput("algorithm_plot", height = "500px"))
      ),
      tabItem(tabName = "performance",
              box(width = 12,
                  h3("📋 Interactive Performance Analysis"),
                  DTOutput("performance_table"))
      )
    ),
    # --- Make dashboard body white ---
    tags$head(tags$style(HTML('
      .content-wrapper, .right-side {
        background: #fff !important;
        background-color: #fff !important;
      }
      .box, .box-body {
        background: #fff !important;
        border: none !important;
        box-shadow: none !important;
      }
      .content-wrapper {
        min-height: 100vh !important;
      }
      html, body {
        height: 100% !important;
      }
    ')))
  )
)

# --- Server ---
server <- function(input, output, session) {
  # --- Communication Visualisations ---
  filtered_data <- reactive({
    comm_nodes %>%
      filter(date >= input$date_filter[1],
             date <= input$date_filter[2],
             hour >= input$hour_filter[1],
             hour <= input$hour_filter[2])
  })
  
  output$daily_plot <- renderPlot({
    daily_counts <- filtered_data() %>%
      group_by(date) %>%
      summarise(comm_count = n())
    ggplot(daily_counts, aes(x = date, y = comm_count)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = comm_count), vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      scale_x_date(date_labels = "%Y-%m-%d", expand = expansion(mult = c(0.01, 0.1))) +
      labs(title = "Daily Communication Volume", x = "Date", y = "Count") +
      theme_minimal()
  })
  
  output$hourly_plot <- renderPlot({
    hourly_totals <- filtered_data() %>%
      group_by(hour) %>%
      summarise(comm_count = n())
    ggplot(hourly_totals, aes(x = hour, y = comm_count)) +
      geom_col(fill = "steelblue") +
      geom_text(aes(label = comm_count), vjust = -0.5, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
      labs(title = "Hourly Communication Volume", x = "Hour", y = "Count") +
      theme_minimal()
  })
  
  output$heatmap <- renderPlotly({
    hourly_counts <- filtered_data() %>%
      group_by(date, hour) %>%
      summarise(comm_count = n(), .groups = "drop")
    p <- ggplot(hourly_counts, aes(
      x = hour, y = date, fill = comm_count,
      text = paste("Date:", date,
                   "<br>Hour:", hour,
                   "<br>Communications:", comm_count))) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "firebrick") +
      scale_y_date(date_labels = "%Y-%m-%d") +
      labs(title = "Hourly Communication Patterns", x = "Hour", y = "Date") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  
  output$messages_table <- renderDT({
    filtered_data() %>%
      arrange(desc(timestamp)) %>%
      mutate(Time = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
      select(Time, Content = content)
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    scrollX = TRUE,
    columnDefs = list(
      list(width = '20%', targets = 0),
      list(width = '80%', targets = 1)
    )
  ),
  rownames = FALSE
  )
  
  output$network_plot <- renderVisNetwork({
    req(filtered_data())
    peak_comm_ids <- filtered_data() %>% pull(new_index)
    peak_edges <- mc3_edges_final %>%
      filter(from %in% peak_comm_ids | to %in% peak_comm_ids)
    used_indices <- unique(c(peak_edges$from, peak_edges$to))
    peak_nodes_sub <- mc3_nodes_final %>%
      filter(new_index %in% used_indices) %>%
      mutate(sub_index = row_number())
    peak_edges_sub <- peak_edges %>%
      left_join(select(peak_nodes_sub, old_index = new_index, from_sub = sub_index),
                by = c("from" = "old_index")) %>%
      left_join(select(peak_nodes_sub, old_index = new_index, to_sub = sub_index),
                by = c("to" = "old_index")) %>%
      select(from = from_sub, to = to_sub, is_inferred, type) %>%
      filter(!is.na(from) & !is.na(to)) %>%
      mutate(id = row_number(),
             arrows = ifelse(is_inferred, "to;from", "to"),
             dashes = is_inferred,
             title = paste("Edge type:", type))
    nodes_vis <- peak_nodes_sub %>%
      mutate(
        id = sub_index,
        label = paste0(label, "\n(", type, ")"),
        group = type,
        title = paste0(
          "Name: ", label,
          "\nType: ", type,
          if ("sub_type" %in% names(.)) paste0("\nSubtype: ", sub_type) else "",
          "\nConnections: ", purrr::map_dbl(sub_index, ~sum(peak_edges_sub$from == .x | peak_edges_sub$to == .x))
        )
      )
    visNetwork(nodes_vis, peak_edges_sub, height = "800px", width = "100%") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 2, hover = TRUE),
        nodesIdSelection = TRUE,
        selectedBy = list(variable = "group", multiple = TRUE)
      ) %>%
      visPhysics(stabilization = TRUE) %>%
      visInteraction(navigationButtons = TRUE) %>%
      visLegend()
  })
  
  # --- Clustering Section ---
  entity_nodes <- mc3_nodes_final %>%
    filter(type == "Entity", sub_type %in% c("Person", "Vessel")) %>%
    select(id, name, sub_type)
  
  entity_nodes <- entity_nodes %>%
    mutate(
      group = case_when(
        name %in% c("Green Guardians", "Reef Guardian", "EcoVigil", "Sentinel") ~ "Environmentalism",
        name %in% c("Sailor Shift", "Sailor Shifts Team") ~ "Sailor Shift",
        name %in% c("Mariners Dream", "Marlin", "Recreational Fishing Boats") ~ "Fishing/Leisure",
        name %in% c("Serenity", "Horizon", "Osprey", "Remora", "Neptune", "Mako") ~ "Tourism/Leisure",
        name %in% c("Boss", "Mrs. Money", "The Middleman") ~ "Suspicious",
        TRUE ~ sub_type
      )
    )
  
  valid_ids <- entity_nodes$id
  comm_event_ids <- mc3_nodes_final %>%
    filter(type == "Event", sub_type == "Communication") %>%
    pull(id)
  
  edge_lookup <- tibble(
    index = mc3_nodes_final$new_index,
    id = mc3_nodes_final$id
  )
  
  mc3_edges_for_events <- mc3_edges_final %>%
    left_join(edge_lookup, by = c("from" = "index")) %>%
    rename(from_id = id) %>%
    left_join(edge_lookup, by = c("to" = "index")) %>%
    rename(to_id = id)
  
  sent_edges <- mc3_edges_for_events %>%
    filter(type == "sent", to_id %in% comm_event_ids) %>%
    select(event_id = to_id, entity_id = from_id)
  
  received_edges <- mc3_edges_for_events %>%
    filter(type == "received", from_id %in% comm_event_ids) %>%
    select(event_id = from_id, entity_id = to_id)
  
  entity_event_edges <- bind_rows(sent_edges, received_edges)
  
  entity_pairs <- entity_event_edges %>%
    inner_join(entity_event_edges, by = "event_id") %>%
    filter(entity_id.x != entity_id.y) %>%
    distinct(entity_id.x, entity_id.y) %>%
    rename(source = entity_id.x, target = entity_id.y) %>%
    filter(source %in% valid_ids & target %in% valid_ids)
  
  graph_tbl <- tbl_graph(nodes = entity_nodes, edges = entity_pairs, directed = FALSE)
  graph_igraph <- as.igraph(graph_tbl)
  
  cluster_infomap <- cluster_infomap(graph_igraph)
  cluster_louvain <- cluster_louvain(graph_igraph)
  cluster_walktrap <- cluster_walktrap(graph_igraph, steps = 10)
  cluster_edge_betweenness <- cluster_edge_betweenness(graph_igraph, directed = FALSE)
  
  if (!is_connected(graph_igraph)) {
    comp_info <- components(graph_igraph)
    largest_comp <- which.max(comp_info$csize)
    node_indices <- which(comp_info$membership == largest_comp)
    graph_connected <- induced_subgraph(graph_igraph, node_indices)
    cluster_spinglass <- cluster_spinglass(graph_connected)
    spinglass_vec <- rep(NA, vcount(graph_igraph))
    spinglass_vec[node_indices] <- membership(cluster_spinglass)
  } else {
    cluster_spinglass <- cluster_spinglass(graph_igraph)
    spinglass_vec <- membership(cluster_spinglass)
  }
  
  entity_nodes <- entity_nodes %>%
    mutate(
      infomap_cluster = as.factor(membership(cluster_infomap)),
      louvain_cluster = as.factor(membership(cluster_louvain)),
      walktrap_cluster = as.factor(membership(cluster_walktrap)),
      edge_betweenness_cluster = as.factor(membership(cluster_edge_betweenness)),
      spinglass_cluster = as.factor(spinglass_vec)
    )
  
  cluster_metrics <- tibble(
    Algorithm = c("Infomap", "Louvain", "Walktrap", "Edge Betweenness", "Spinglass"),
    Clusters = c(
      length(unique(membership(cluster_infomap))),
      length(unique(membership(cluster_louvain))),
      length(unique(membership(cluster_walktrap))),
      length(unique(membership(cluster_edge_betweenness))),
      length(unique(membership(cluster_spinglass)))
    ),
    Modularity = c(
      modularity(cluster_infomap),
      modularity(cluster_louvain),
      modularity(cluster_walktrap),
      modularity(cluster_edge_betweenness),
      modularity(cluster_spinglass)
    )
  )
  
  node_id_map <- graph_tbl %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(node_index = row_number()) %>%
    select(node_index, id)
  
  edges_vis <- graph_tbl %>%
    activate(edges) %>%
    as_tibble() %>%
    left_join(node_id_map, by = c("from" = "node_index")) %>%
    rename(from_id = id) %>%
    left_join(node_id_map, by = c("to" = "node_index")) %>%
    rename(to_id = id) %>%
    select(from = from_id, to = to_id)
  
  nodes_vis_louvain <- entity_nodes %>%
    mutate(
      id = as.character(id),
      label = name,
      group = factor(louvain_cluster, levels = sort(as.numeric(unique(louvain_cluster)))),
      shape = ifelse(sub_type == "Person", "dot", "triangle"),
      title = paste0(
        "<div style='color:white;'>",
        "<b>", name, "</b><br>",
        "Type: ", sub_type, "<br>",
        "Louvain Cluster: ", louvain_cluster,
        "</div>"
      )
    ) %>%
    select(id, label, group, shape, title)
  
  nodes_vis_infomap <- entity_nodes %>%
    mutate(
      id = as.character(id),
      label = name,
      group = factor(infomap_cluster, levels = sort(as.numeric(unique(infomap_cluster)))),
      shape = ifelse(sub_type == "Person", "dot", "triangle"),
      title = paste0(
        "<div style='color:white;'>",
        "<b>", name, "</b><br>",
        "Type: ", sub_type, "<br>",
        "Infomap Cluster: ", infomap_cluster,
        "</div>"
      )
    ) %>%
    select(id, label, group, shape, title)
  
  output$louvain_network <- renderVisNetwork({
    visNetwork(nodes_vis_louvain, edges_vis, width = "100%", height = "800px") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visNodes(font = list(size = 16)) %>%
      visEdges(color = list(color = "rgba(150,150,150,0.3)")) %>%
      visInteraction(
        tooltipStyle = 'background-color: black; color: white; border-radius: 5px; padding: 8px;'
      ) %>%
      visOptions(
        highlightNearest = TRUE,
        selectedBy = list(variable = "group", main = "Select Cluster:", sort = TRUE)
      ) %>%
      visLegend(position = "right", main = "Louvain Clusters (Optimal)")
  })
  
  output$infomap_network <- renderVisNetwork({
    visNetwork(nodes_vis_infomap, edges_vis, width = "100%", height = "800px") %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visNodes(font = list(size = 16)) %>%
      visEdges(color = list(color = "rgba(150,150,150,0.3)")) %>%
      visInteraction(
        tooltipStyle = 'background-color: black; color: white; border-radius: 5px; padding: 8px;'
      ) %>%
      visOptions(
        highlightNearest = TRUE,
        selectedBy = list(variable = "group", main = "Select Cluster:", sort = TRUE)
      ) %>%
      visLegend(position = "right", main = "Infomap Clusters")
  })
  
  output$algorithm_plot <- renderPlot({
    p1 <- ggplot(cluster_metrics, aes(x = reorder(Algorithm, Clusters), y = Clusters, fill = Algorithm)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = Clusters), vjust = -0.5, size = 3, fontface = "bold") +
      labs(title = "Number of Clusters by Algorithm",
           x = "Algorithm", y = "Number of Clusters") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    p2 <- ggplot(cluster_metrics, aes(x = reorder(Algorithm, -Modularity), y = Modularity, fill = Algorithm)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = round(Modularity, 3)), vjust = -0.5, size = 3, fontface = "bold") +
      labs(title = "Modularity Score by Algorithm",
           x = "Algorithm", y = "Modularity Score") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    p1 + p2 + plot_layout(ncol = 2)
  })
  
  output$performance_table <- renderDT({
    performance_table <- cluster_metrics %>%
      arrange(desc(Modularity)) %>%
      mutate(
        Rank = row_number(),
        Modularity_Score = round(Modularity, 3),
        Efficiency = case_when(
          Modularity > 0.35 ~ "Excellent",
          Modularity > 0.25 ~ "Good",
          TRUE ~ "Fair"
        )
      )
    
    datatable(performance_table,
              options = list(pageLength = 10, dom = 't'),
              caption = "🏆 Algorithm Performance Comparison") %>%
      formatStyle("Efficiency",
                  backgroundColor = styleEqual(
                    c("Excellent", "Good", "Fair"),
                    c("#d4edda", "#fff3cd", "#f8d7da")
                  ))
  })
}

shinyApp(ui, server)
