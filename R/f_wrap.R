#' @export
#'

f_wrap <- function(type, cols, rows) {
  if(is.null(cols) & is.null(rows)){
    return()
  }
  switch(type,
    "grid" = {
      if (is.null(cols)) { # Rows만 있을 때
        return(facet_grid(rows = vars(get(rows))))
      } else if (is.null(rows)) { # Cols만 있을 때
        return(facet_grid(cols = vars(get(cols))))
      } else { # 둘 다 NULL이 아닐 때
        return(facet_grid(cols = vars(get(cols)), rows = vars(get(rows))))
      }
    },
    "wrap" = {
      return(facet_wrap(vars(get(cols)))) # wrap인 경우 cols에만 변수 입력
    }
  )
}
