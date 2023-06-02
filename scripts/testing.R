# Getting People out of LIRE/LIST
LIST
mapview(LIST)



library(jsonlite)
library(purrr)



#Following tidyverse_wiki https://tidyr.tidyverse.org/reference/unnest_wider.html
LIST$people

parsedLIST <- LIST %>% 
  select(`LIST-ID`, `EDH-ID`, people) %>% 
  unnest_wider(people, names_sep = "_") %>%  # put each person in a separate column
  filter(!is.na(people_1)) %>% 
  pivot_longer(cols = c(people_1:people_9), #  put each person in a separate row
               names_to = "people",
               values_drop_na = T) %>% 
  mutate(peoplenew = paste0("[",value,"]")) %>% 
  mutate(peoplenew = map(peoplenew, ~ jsonlite::fromJSON(.) %>% as.data.frame())) %>% 
  unnest_wider(peoplenew) %>% 
  unnest_wider(persname) %>% 
  select(-value) %>% 
  rename(id = `@{http://www.w3.org/XML/1998/namespace}id`,
         sex = `@sex`,
         person_url = `@ref`)
  
LISTpeople <- parsedLIST %>% 
  unnest_wider(name, names_sep = "_") %>% 
  unnest(name_1, names_sep = "_") %>% 
  pivot_wider(names_from =`name_1_@type`,values_from = `name_1_#text`)# %>% 
  # select(-`name_@type`) %>% 
  # select(-`name_#text`)
  # glimpse()

# LIRE test - FAILS on apostrophies
filteredLIRE <- LIRE %>% 
  select(`LIST.ID`, `EDH.ID`, people) %>% 
  filter(!is.na(EDH.ID)) %>% 
  mutate(people = str_replace_all(people, c("'"="\""))) %>% 
  mutate(people = str_replace_all(people, c("\"["= "'[", "]\""="]'"))) %>% 
  glimpse()

filteredLIRE %>%
  unnest_wider(people, names_sep = "_") %>% 
  glimpse()# put each person in a separate column
  filter(!is.na(people_1)) %>% 
  pivot_longer(cols = c(people_1:people_9), #  put each person in a separate row
               names_to = "people",
               values_drop_na = T) %>% 
  mutate(peoplenew = paste0("[",value,"]")) %>% 
  mutate(peoplenew = map(peoplenew, ~ jsonlite::fromJSON(.) %>% as.data.frame())) %>% 
  unnest_wider(peoplenew) %>% 
  unnest_wider(persname) %>% 
  select(-value)


parsedtest <- purrr::map(paste0("[",test$people,"]"), jsonlite::fromJSON)

# 3 solutions, that partially work
unlisted <- LIST %>% 
  pull(people) %>% 
  unlist()

parsedJSON <- purrr::map(paste0("[",unlisted,"]"), jsonlite::fromJSON)
parsedJSON <- cbind(df, do.call(plyr::rbind.fill, lapply(paste0("[",unlisted,"]"), function(x) jsonlite::fromJSON(x))))
parsedJSON <- rbind(lapply(paste0("[",unlisted,"]"), function(x) jsonlite::fromJSON(x))) # added rbind

class(parsedJSON[[1]])
df <- bind_rows(unlisted, .id = "column_label")

# Got a list! But now what?
as_tibble(parsedJSON)

#%>% 
  unpack(name)
  unnest_wider(name, simplify = FALSE) %>% 
  unnest_longer((c(person_id,name, cognomen, nomen, supernomen, gender, praenomen, `age: years`, `age: months`, `age: days`, `age: hours`, status, occupation, tribus, origo)))

# converting the list to dataframe
parsedJSON2 <-lapply(paste0("[",parsedJSON,"]"), function(x) jsonlite::fromJSON(x))

do.call(rbind.data.frame, parsedJSON)

# Single record to dataframe
sample <- LIRE$people[994:999]
sample <- LIST$people[994:999]

class(sample) # is a list

sample <- "{'persname': {'name: [{'@type': 'praenomen', '#text': '[-]'}, {'@type': 'nomen', '#text': '[---]'}, {'@type': 'cognomen', '#text': '[---]'}]}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059333_1'}, {'persname': {'name': [{'@type': 'praenomen', '#text': 'L.'}, {'@type': 'nomen', '#text': 'Septimius'}, {'@type': 'cognomen', '#text': 'Severus Pertinax'}]}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059333_2'}, {'persname': {'name': [{'@type': 'praenomen', '#text': '[-]'}, {'@type': 'nomen', '#text': '[---]'}, {'@type': 'cognomen', '#text': '[---]'}]}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059333_3'}, {'persname': {'name': {'@type': 'cognomen', '#text': 'Clemens'}}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059333_4'}"
parsedjson <- fromJSON(sample) # does not work because of swapped apostrophe and quotes

# this works! Note the inverted apostrophe and quotes!!
json_string <- '[{"persname": {"name": [{"@type": "praenomen", "#text": "C."}, {"@type": "nomen", "#text": "Iulius"}, {"@type": "cognomen", "#text": "Capito*"}]}, "@sex": "M", "@{http://www.w3.org/XML/1998/namespace}id": "HD057920_1"}, {"persname": {"name": [{"@type": "praenomen", "#text": "C."}, {"@type": "nomen", "#text": "Iulius"}, {"@type": "cognomen", "#text": "Maximus+"}]}, "@sex": "M", "@{http://www.w3.org/XML/1998/namespace}id": "HD057920_2"}]'
parsedjson <- fromJSON(json_string)

#############################

# ChatGPS suggests:
# library(reticulate)
# 
# # Python dictionary string
# sample <- "[{'persname': {'name': [{'@type': 'praenomen', '#text': 'P.++'}, {'@type': 'nomen', '#text': 'Septimius+++'}, {'@type': 'cognomen', '#text': 'Geta+++'}], '@ref': 'https://www.wikidata.org/wiki/Q183089'}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059338_1'}, {'persname': {'name': [{'@type': 'praenomen', '#text': 'L.'}, {'@type': 'nomen', '#text': 'Septimius'}, {'@type': 'cognomen', '#text': 'Severus Pertinax'}]}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059338_2'}, {'persname': {'name': [{'@type': 'praenomen', '#text': 'M.'}, {'@type': 'nomen', '#text': 'Iunius'}, {'@type': 'cognomen', '#text': 'Punicus'}]}, '@sex': 'M', '@{http://www.w3.org/XML/1998/namespace}id': 'HD059338_3'}]"
# 
# # Use reticulate to parse the Python dictionary in R
# py_dict <- reticulate::py_eval(sample)
# 
# # Convert the Python dictionary to a named list in R
# named_list <- as.list(py_dict)
# 
# # Access specific elements from the named list
# person1 <- named_list[[1]]$persname$name
# person2 <- named_list[[2]]$persname$name
# person3 <- named_list[[3]]$persname$name
# 
# # Print the extracted values
# print(person1)
# print(person2)
# print(person3)