#
# Returns a table with information about the objects of R's global environment (their type and memory size).
#
memory_summary = function(object_names = NULL, sorting = "increasing"){
  
  objs = object_names
  
  if(is.null(object_names)) objs = as.character(ls(envir = .GlobalEnv))
  
  memory = lapply(
    objs, function(x) eval(parse(text=paste0("object.size(", x, ")")))
  )
  
  memory_bytes = as.numeric(memory)
  
  ord = 1:length(memory_bytes)
  
  if(sorting %in% c("decreasing", "increasing")) ord = order(
    memory_bytes, 
    decreasing = ifelse(sorting == "decreasing", TRUE, FALSE))
  
  memory_MB = lapply(
    memory, function(x) format(x, standard = "SI", units = "MB"))
  
  types = sapply(
    objs, function(x) eval(parse(text=paste0("class(", x, ")"))))
  
  result = data.frame(
    object = objs, 
    type = as.character(types), 
    memory = as.character(memory_MB))
  return(result[ord,])
}
#
