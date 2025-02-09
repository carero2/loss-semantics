require(tidyverse)
require(reticulate)
require(glue)
require(crayon)

clean_up = function(x){
  x %>% 
    str_remove_all("⁄⁄.") %>% 
    str_remove("[:punct:]$") %>% 
    str_remove("\\([^\\)]+") %>% 
    str_remove("\\)") %>% 
    str_squish()}

check_multi = function(x){
  words = str_extract_all(x, "[:alpha:]+")[[1]]
  all(hunspell::hunspell_check(words))
}



use_condaenv("base")
hub = import("huggingface_hub")


correct_llama = function(words, sleep = .5, system, instruct, token){
  client = hub$InferenceClient(token = token,
                               model = "meta-llama/Meta-Llama-3.1-70B-Instruct", 
                               headers = list("X-use-cache" = "false"))
  
  template = "<|begin_of_text|><|start_header_id|>system<|end_header_id|>{system}<|eot_id|><|start_header_id|>user<|end_header_id|>{user}<|eot_id|><|start_header_id|>assistant<|end_header_id|>"
  
  responses = c()
  for(i in 1:length(words)){
    Sys.sleep(sleep)
    WORD = words[i]
    user = glue(instruct)
    prompt = glue(template)
    
    out = NA
    while(is.na(out)){
      out = client$text_generation(prompt, 
                                   max_new_tokens = as.integer(50),
                                   do_sample = FALSE,
                                   temperature = 0.001) |> try()
      if(class(out) == "try-error"){
        cat("\n\nsleeping 5\n\n")
        Sys.sleep(5)
        if(str_detect(out, "You reached PRO hourly usage limit")) {
          cat("Sleeping at", as.character(Sys.time()))
          Sys.sleep(60 * 60)
          }
        out = NA
        }
      }
    responses[i] = out 
    cat(red(paste(i, " - ", WORD)),":\n\n", str_remove_all(out, "\n"), "\n\n\n", sep="")
  }
  
  responses
}


normalize_llama = function(words, sleep = .5, system, instruct, token){
  
  template = "<|begin_of_text|><|start_header_id|>system<|end_header_id|>{system}<|eot_id|><|start_header_id|>user<|end_header_id|>{user}<|eot_id|><|start_header_id|>assistant<|end_header_id|>"
  client = hub$InferenceClient(token = token,
                               model = "meta-llama/Meta-Llama-3.1-70B-Instruct", 
                               headers = list("X-use-cache" = "false"))
  
  responses = c()
  for(i in 1:length(words)){
    Sys.sleep(sleep)
    WORD = words[i]
    user = glue(instruct)
    prompt = glue(template)
    
    out = NA
    while(is.na(out)){
      out = client$text_generation(prompt, 
                                   max_new_tokens = as.integer(10),
                                   do_sample = FALSE) |> try()
      if(class(out) == "try-error"){
        cat("\n\nsleeping 5\n\n")
        Sys.sleep(5)
        if(str_detect(out, "You reached PRO hourly usage limit")) {
          cat("Sleeping at", as.character(Sys.time()))
          Sys.sleep(60 * 60)
        }
        out = NA
      } else if(length(unlist(strsplit(out, split = " "))) > 3){
        out = NA
      }
    }
    responses[i] = out 
    cat(red(paste(i, " - ", WORD)),":\n\n", str_remove_all(out, "\n"), "\n\n\n", sep="")
  }
  
  responses
}

single_use_llama = function(system, instruct, token){
  
  template = "<|begin_of_text|><|start_header_id|>system<|end_header_id|>{system}<|eot_id|><|start_header_id|>user<|end_header_id|>{user}<|eot_id|><|start_header_id|>assistant<|end_header_id|>"
  client = hub$InferenceClient(token = token,
                               model = "meta-llama/Meta-Llama-3.1-70B-Instruct", 
                               headers = list("X-use-cache" = "false"))
  user = glue(instruct)
  prompt = glue(template)
  
  out = NA
  while(is.na(out)){
    out = client$text_generation(prompt, 
                                 max_new_tokens = as.integer(500),
                                 do_sample = FALSE) |> try()
    if(class(out) == "try-error"){
      cat("\n\nsleeping 5\n\n")
      Sys.sleep(5)
      if(str_detect(out, "You reached PRO hourly usage limit")) {
        cat("Sleeping at", as.character(Sys.time()))
        Sys.sleep(60 * 60)
      }
      out = NA
    }
  }
  out
}
