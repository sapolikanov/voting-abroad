# Functions

resizebox.stargazer = function(..., tab.width = "!", tab.height = "!")
{
  require(stringr) 
  res = capture.output(stargazer::stargazer(...))
  tab.width = tab.width
  tab.height = tab.height
  res = prepend(res, "}", before = length(res))
  
  res = c(res[1:str_which(res, "^\\\\begin\\{tabular\\}.*")-1],
          paste0("\\resizebox*{",tab.width,"}{",tab.height,"}{%"),
          res[str_which(res, "^\\\\begin\\{tabular\\}.*"):length(res)]
  )
  cat(res, sep = "\n")
}
