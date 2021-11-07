#' Print the attributes of a variable, followed by its frequency table
#' This is useful for describing self-report items
#'
#' @param items A dataframe of self-report items. If items is not a dataframe,
#' then the function will try to coerce it.
#'
#' @param item_number Boolean - if TRUE, then print a number before each item.
#'
#' @return Returns NULL silently.
#' @export
#'
#' @examples
#' \dontrun{
#' print_attr_then_table(hl_data$rrs1)
#' }
print_attr_then_table <- function(items, item_number = TRUE) {

  items <- as.data.frame(items)

  for(i in seq_along(items)){

    # Print item number if requested
    if (item_number) {
      cat(c("Item#", i, "\n"))}

    # Print item name if non-null
    if (!is.null(attributes(items[i])$names)){
      cat(c("Name:", attributes(items[i])$names))
    }

    # Print variable label if non-null
    if (!is.null(attributes(items)$variable.labels[i])){
      cat(c("Label:", attributes(items)$variable.labels[i], "\n"))
    }

    # Print frequency table
    print(table(items[i], useNA = "always"))

    # Add whitespace
    writeLines("")

    invisible(NULL)

  }
}

