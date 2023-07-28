
pageruiInput = function(inputId, page_current = NULL, pages_total = NULL) {
  # construct the pager-ui framework
  tagList(
    # root pager-ui node
    div(
      id = inputId,
      class = 'pager-ui',
      
      # container for hidden numeric fields
      div(
        class = 'hidden',
        
        # numeric input to store current page
        tags$input(
          id = paste(inputId, 'page_current', sep='__'),
          class = 'page-current',
          type = 'number',
          value = 1,
          min = 1,
          max = 1
        ),
        
        # numeric input to store total pages
        tags$input(
          id = paste(inputId, 'pages_total', sep='__'),
          class = 'pages-total',
          type = 'number',
          value =  0,
          min = 0,
          max =  0
        )
      ),
      
      # container for pager button groups
      div(
        class = 'page-buttons',
        
        # prev/next buttons
        span(
          class = 'page-button-group-prev-next btn-group',
          tags$button(
            id = paste(inputId, 'page-prev-button', sep='__'),
            class = 'page-prev-button btn btn-default',
            'Prev'
          ),
          tags$button(
            id = paste(inputId, 'page-next-button', sep='__'),
            class = 'page-next-button btn btn-default',
            'Next'
          )
        ),
        
        # page number buttons
        # dynamically generated via javascript
        # span(
        #   class = 'page-button-group-numbers btn-group'
        # ),
        
        # javascript assets
        htmlDependency(
          name = 'html_dependency',
          version = '1.0',
          package = 'packageName',
          src = 'assets',
          script = c(
            'js/input_binding_pager-ui.js',
            'js/underscore-min.js',
            'js/underscore-min.map'
          )
        )
        
        ##
      )
    )
  )
}

updatePageruiInput = function(session, inputId, page_current = NULL, pages_total = NULL) {
  message = shiny:::dropNulls(list(
    page_current = shiny:::formatNoSci(page_current),
    pages_total = shiny:::formatNoSci(pages_total)
  ))
  
  session$sendInputMessage(inputId, message)
}
