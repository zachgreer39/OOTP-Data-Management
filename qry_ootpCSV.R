library(tidyverse);
setwd("~/Desktop/github/OOTP-Data-Management");

declare_path=function(){
  
  print("Open OOTP Saved Games Folder")
  Sys.sleep(1)
  games_path=rstudioapi::selectDirectory(caption="Open OOTP Saved Games Folder")
  print(str_subset(dir(games_path), "\\.lg$"))
  file_name=readline(prompt="Enter file name: ")
  paste0(games_path, "/", file_name, "/import_export")
  
};

ootpPATH=declare_path();

ootpCSV=function(csv_name){
  
  file_name=str_remove(str_extract(ootpPATH, 
                                   "saved_games/.*\\.lg"), 
                       "saved_games/")
  as.data.frame(cbind(read.csv(paste0(ootpPATH, "/csv/", csv_name)), 
                      file_name))
  
};
