#' Server function for the networkViewer app
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 50*1024^2)
  # get options
  testing <- shiny::getShinyOption("testing", default = FALSE)
  debug   <- shiny::getShinyOption("debug", default = FALSE)

  data_list <- uploadGraphServer(id = "GraphData", debug = debug)
  output$nodes <- renderTable(data_list$nodes()[1:5,])
  output$edges <- renderTable(data_list$edges()[1:5,])

  # Overview
  overview_list <- networkOverviewServer(
    "network_overview",
    nodes = data_list$nodes,
    edges = data_list$edges,
    debug
  )
  output$cl_nodes <- DT::renderDataTable(overview_list$overview_nodes())
  output$cl_edges <- DT::renderDataTable(overview_list$overview_edges())

  # Cluster View
  subset_list <- subsetGraphServer(
    id = "cluster_closeup",
    nodes = data_list$nodes,
    edges = data_list$edges,
    debug = debug
  )

  # TODO: remove genes with all zeros
  rnaseq_data_list <- uploadRNASeqServer(id = "rnaseqData", testing = testing,
                                         debug = debug)

  output$samples <- renderTable({
    req(rnaseq_data_list$sample_info())
    samples <- rnaseq_data_list$sample_info()
    max_width <- 5
    n_cols <- ifelse(ncol(samples) > max_width, max_width, ncol(samples))
    samples[1:5,seq_len(n_cols)]
  })
  output$counts <- renderTable({
    req(rnaseq_data_list$counts())
    counts <- rnaseq_data_list$counts()
    max_width <- 10
    n_cols <- ifelse(ncol(counts) > max_width, max_width, ncol(counts))
    counts[1:5,seq_len(n_cols)]
  })
  output$metadata <- renderTable({
    req(rnaseq_data_list$gene_metadata())
    gene_metadata <- rnaseq_data_list$gene_metadata()
    max_width <- 5
    n_cols <- ifelse(ncol(gene_metadata) > max_width, max_width, ncol(gene_metadata))
    gene_metadata[1:5,seq_len(n_cols)]
  })

  selected_gene <- countPlotServer(
    id = "gene1",
    counts = rnaseq_data_list$counts,
    sample_info = rnaseq_data_list$sample_info,
    gene_metadata = rnaseq_data_list$gene_metadata,
    debug = debug
  )

  plot_data_list <- countXYScatterPlotServer(
    "xy_scatter",
    counts = rnaseq_data_list$counts,
    sample_info = rnaseq_data_list$sample_info,
    gene_metadata = rnaseq_data_list$gene_metadata,
    gene1 = selected_gene$gene1,
    debug = debug
  )

  output$count_data <- renderDT(
    plot_data_list$plot_data(),
    options = list(pageLength = nrow(plot_data_list$plot_data()))
  )

}
