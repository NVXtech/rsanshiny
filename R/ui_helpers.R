create_datatable <- function(dados) {
    return(
        DT::renderDataTable(
            dados(),
            extensions = c("Buttons", "Scroller"),
            options = list(
                scrollX = TRUE,
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                dom = "Bfrtip",
                buttons = list(
                    "copy",
                    list(
                        extend = "excel",
                        text = "Download",
                        filename = "investimento"
                    )
                )
            )
        )
    )
}
