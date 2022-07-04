create_datatable <- function(reactive_data) {
    return(DT::renderDataTable(
        reactive_data(),
        extensions = c("Buttons", "Scroller"),
        options = list(
            scrollX = TRUE,
            deferRender = TRUE,
            scrollY = 200,
            scroller = TRUE,
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
        )
    ))
}
