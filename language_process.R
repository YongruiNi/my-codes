library(stringr)
data_dir = "C:/Users/Administrator.SC-201301200557/Desktop/4/messy/"
dirs = list.files(data_dir, full.names = TRUE)

read_post<-function(file){
  post = readLines(file, encoding = "UTF-8")
  txt = str_c(post, collapse = "\n")
  price<-parse_price(txt)
  return(txt)
}

read_all_posts=function(directory)
{
  allposts=lapply(dirs,function(directory) {
    files=list.files(directory, full.names = TRUE)
    posts=sapply(files, read_post)
    tprices=sapply(files, read_tprice)
    atprices=sapply(files, read_atprice)
    pets=sapply(files,read_pet)
    ac=sapply(files,read_ac)
    heat=sapply(files,read_heater)
    data.frame(text = posts, region = basename(directory),tprice=tprices,atprice=atprices,pet=pets, ac=ac,heat=heat)
  })
  posts_df = do.call(rbind, allposts)
}


###############################ATPRICE####################################################################
read_atprice<-function(file){
  post = readLines(file, encoding = "UTF-8")
  txt = str_c(post, collapse = "\n")
  price<-at_price(txt)
  return(price)
}
at_price<-function(txt){
  txt_paste<-paste0(txt,collapse=" ") #I'm going to do this to avoid NA redundancy
  attprice<-unlist(str_extract_all(txt_paste,"Price\\: \\$[0-9.,]+"))
  #If multiple elements are returned, select the last element
  if(length(attprice)>0){
    attprice<-attprice[length(attprice)]
    #If we got the price, let's tidy it until we have just a number
    attprice<-gsub("Price: \\$","",attprice)
  }
  else{
    attprice<-NA
  }
  #Next steps, extract a title price IF it is present
  title<-txt[1]
  tprice<-gsub("\\$","",str_extract(title,"\\$[0-9,.]+"))
  
  return(attprice)
}




#############################Tprice######################################################################################
read_tprice<-function(file){
  post = readLines(file, encoding = "UTF-8")
  txt = str_c(post, collapse = "\n")
  price<-t_price(txt)
  return(price)
}
t_price<-function(txt){
  txt_paste<-paste0(txt,collapse=" ") #I'm going to do this to avoid NA redundancy
  attprice<-unlist(str_extract_all(txt_paste,"Price\\: \\$[0-9.,]+"))
  #If multiple elements are returned, select the last element
  if(length(attprice)>0){
    attprice<-attprice[length(attprice)]
    #If we got the price, let's tidy it until we have just a number
    attprice<-gsub("Price: \\$","",attprice)
  }
  else{
    attprice<-NA
  }
  #Next steps, extract a title price IF it is present
  title<-txt[1]
  tprice<-gsub("\\$","",str_extract(title,"\\$[0-9,.]+"))
  
  return(tprice)
}
######################pets#####################################################################################
pet_phrases<-"(pet|dog|cat|animal|fish|bird|hamster|parrot|horse)[es]{0,2} "

pets_yes<-"friendly|rent|deposit|max|weight|allowed|ok|welcome"
pets_no<-"no"
pets_unf<-paste0("(",c(paste("no",pet_phrases),paste(pet_phrases,"not allowed"),paste(pet_phrases,"not permitted")),")",collapse="|")

#We need to find instances where pets are being mentioned to narrow down our pet search

parse_pets<-function(txt){
  pets<-NA
  pet_search<-grep(pet_phrases,tolower(txt))
  if(length(pet_search)>0){
    pets<-txt[pet_search]
    #Let's try to detect paws-itive posts
    is_pos<-grepl(pets_yes,tolower(pets))
    #Let's try to build a basic way to detect 'no pets'
    is_neg<-grepl(pets_unf,pets)
    if(any(is_pos)&all(is_neg)==FALSE){
      pets<-"friendly"
    }
    else if(any(is_neg)){
      pets<-"unfriendly"
    }
  }
  return(pets)
}

#########################################AC/fireplace########################################################################

ac_str<-"(ac|air condiciton)[es]{0,1} "

read_ac<-function(file){
  post = readLines(file, encoding = "UTF-8")
  txt = str_c(post, collapse = "\n")
  ac<-parse_ac(txt)
  return(ac)
}

parse_ac<-function(txt){
  ac_text<-grep(ac_str,tolower(txt))
  if(length(ac_text)>0){
    return("Yes")
    }
  else{
    return("NA")
  }
}

################################################AC##########################################################################
heat_str<-"(fireplace|heat|heater|wood-burning stoves)[es]{0,1} "

read_heater<-function(file){
  post = readLines(file, encoding = "UTF-8")
  txt = str_c(post, collapse = "\n")
  heat<-parse_heat(txt)
  return(heat)
}

parse_heat<-function(txt){
  heat_text<-grep(heat_str,tolower(txt))
  if(length(heat_text)>0){
    return("Yes")
  }
  else{
    return("NA")
  }
}
