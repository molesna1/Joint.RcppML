#' Plot UMAP of RcppML sample embeddings
#'
#' Function to easily plot the output of RcppML in a UMAP, using RcppML object and a group label of choice
#'
#'
#' @param data: model: Output of RcppML
#' group_labels: A list or single vector of group_labels, properly aligned to the NMF input matrix
#' custom_colors: Optional for specific colors. Can either be a single vector 
#' Ex. c("GroupA" = "#FF5733", "GroupB" = "#33FF57", "GroupC" = "#3357FF")
#' or a list Ex. list(c("GroupA" = "#FF5733", "GroupB" = "#33FF57"),  # Colors for first group
#' c("Type1" = "#E41A1C", "Type2" = "#377EB8", "Type3" = "#4DAF4A")  # Colors for second group
#' @return UMAP plot(s) of sample embeddings for RcppML object
#' @export

nmf_umap <- function(model, group_labels, custom_colors = NULL) {
  # Transpose the H matrix from the NMF model
  H_flip <- t(model$h)
  H_flip <- as.data.frame(H_flip)
  
  # Run UMAP
  umap_embedding <- uwot::umap(H_flip, 
                               n_neighbors = 15,
                               min_dist = 0.1,
                               n_components = 2)
  
  # Check if group_labels is a list
  if (is.list(group_labels) && !is.data.frame(group_labels)) {
    # Create a list to store plots
    plot_list <- list()
    
    # Generate a plot for each set of group labels
    for (i in seq_along(group_labels)) {
      # Create a data frame for plotting
      umap_df <- data.frame(
        UMAP1 = umap_embedding[,1],
        UMAP2 = umap_embedding[,2],
        Group = group_labels[[i]])
      
      # Create the plot
      plot <- ggplot2::ggplot(umap_df, ggplot2::aes(x = UMAP1, y = UMAP2, color = Group)) +
        ggplot2::geom_point(size = 3, alpha = 1) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "right") +
        ggplot2::labs(
          title = paste0("UMAP Visualization of NMF Factors (Group ", i, ")"),
          x = "UMAP1",
          y = "UMAP2")
      
      # Add custom colors if provided
      if (!is.null(custom_colors)) {
        # If custom_colors is a list, use the corresponding entry
        if (is.list(custom_colors)) {
          if (i <= length(custom_colors)) {
            group_colors <- custom_colors[[i]]
            if (!is.null(group_colors) && length(group_colors) > 0) {
              # Check if it's a named vector
              if (!is.null(names(group_colors))) {
                plot <- plot + ggplot2::scale_color_manual(values = group_colors)
              } else {
                # If it's not a named vector, try to match by position
                unique_groups <- unique(group_labels[[i]])
                if (length(group_colors) >= length(unique_groups)) {
                  named_colors <- group_colors[1:length(unique_groups)]
                  names(named_colors) <- unique_groups
                  plot <- plot + ggplot2::scale_color_manual(values = named_colors)
                } } } } }}
      
      plot_list[[i]] <- plot }
    
    return(plot_list)
    
  } else {
    # Create a data frame for plotting with a single vector of group labels
    umap_df <- data.frame(
      UMAP1 = umap_embedding[,1],
      UMAP2 = umap_embedding[,2],
      Group = group_labels)
    
    # Create the plot
    umap_plot <- ggplot2::ggplot(umap_df, ggplot2::aes(x = UMAP1, y = UMAP2, color = Group)) +
      ggplot2::geom_point(size = 3, alpha = 1) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position = "right") +
      ggplot2::labs(
        title = "UMAP Visualization of NMF Factors",
        x = "UMAP1",
        y = "UMAP2")
    
    # Add custom colors if provided
    if (!is.null(custom_colors) && !is.list(custom_colors)) {
      # For a single plot with custom colors
      if (!is.null(names(custom_colors))) {
        # If custom_colors is a named vector, use it directly
        umap_plot <- umap_plot + ggplot2::scale_color_manual(values = custom_colors)
      } else {
        # If it's not a named vector, try to match by position
        unique_groups <- unique(group_labels)
        if (length(custom_colors) >= length(unique_groups)) {
          named_colors <- custom_colors[1:length(unique_groups)]
          names(named_colors) <- unique_groups
          umap_plot <- umap_plot + ggplot2::scale_color_manual(values = named_colors)
        } } }
    
    return(umap_plot)
  }
}