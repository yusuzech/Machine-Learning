#Create timeTracker class --------------
timeTracker <- setClass(Class = "timeTracker",
                         slots = c(
                             time = "POSIXct",
                             event = "character",
                             recordTable = "data.frame",
                             showPrint = "logical"
                             
                         ),
                         prototype = list(
                             time = c(),
                             event = c(),
                             recordTable = data.frame(event = character(),
                                                     start = .POSIXct(character()),
                                                     end = .POSIXct(character()),
                                                     time_elasped = numeric(),
                                                     stringsAsFactors = FALSE),
                             showPrint = TRUE
                         ),
                         validity = function(object){
                             if(!any(class(object@time) %in% c("POSIXct","POSIXt"))){
                                 return("Time is not in POSIXct or POSIXt format")
                             }
                             if(class(object@event) != "character"){
                                 return("event should be a character")
                             }
                             if(class(object@recordTable) != "data.frame"){
                                 return("recordTable should be a data frame")
                             }
                             if(class(object@showPrint) != "logical"){
                                 return("showPrint should be a logical")
                             }
                             return(TRUE)
                         })

#updateTracker method ----------------
setGeneric(name = "updateTracker",
           def = function(theObject,eventName){
               standardGeneric("updateTracker")
           })

setMethod(f = "updateTracker",
          signature = "timeTracker",
          definition = function(theObject,eventName){
              #get default setting for printing
              showPrint <- theObject@showPrint
              #get current time
              current_time <- Sys.time()
              #detect if event exists
              theTable <- theObject@recordTable
              if(any(eventName == theTable$event)){
                  #if event exists and end time doesn't exist, add stop time and calculate time time_elasped in seconds
                  if(is.na(as.character(theTable[theTable$event == eventName,][["end"]]))){
                      current_row <- theTable[theTable$event == eventName,]
                      theTable[theTable$event == eventName,][["end"]] <- current_time
                      diff_sec <- as.numeric(current_time - current_row[["start"]])
                      theTable[theTable$event == eventName,][["time_elasped"]] <- diff_sec
                      #optional printing
                      if(showPrint){
                          out_msg <- paste0("Create end time for event: '", eventName,"'\n",
                                            "time elapsed: ",as.character(as.numeric(round(diff_sec,2)))," seconds.")
                          writeLines(out_msg)
                      }
                  } else {
                      #warning!
                      #if event exists but end is already filled, reset that row and print a warning
                      theTable[theTable$event == eventName,] <- data.frame(event = eventName,
                                                                           start = current_time,
                                                                           end = .POSIXct(character(1)),
                                                                           time_elasped = numeric(1),
                                                                           stringsAsFactors = FALSE)
                      out_msg <- paste0("End time for Event: '", eventName, "' already exists. Resetting record.")
                      warning(out_msg)
                  }
                  
              } else {
                  #if not exist create event and start time
                  #warning!
                  #mention user that record for other event is not complete if end time is na
                  if(nrow(theTable) > 0 & any(is.na(as.character(theTable$end)))){
                      bool_incomplete_records <- is.na(as.character(theTable$end))
                      out_msg <- paste0("Event: '",theTable$event[is.na(as.character(theTable$end))],
                                        "' do not have end time.\n make sure to run timeTracker twice to record an event.")
                      warning(out_msg)
                  }
                  #nothing happens, initialize a record
                  current_row <- data.frame(event = eventName,
                                            start = current_time,
                                            end = .POSIXct(character(1)),
                                            time_elasped = numeric(1),
                                            stringsAsFactors = FALSE)
                  theTable <- rbind(theTable,current_row)
                  if(showPrint){
                      out_msg <- paste0("Create start time for event: '", eventName,"'")
                      writeLines(out_msg)
                  }
              }
              #modify values ----------
              theObject@time <- current_time
              theObject@event <- eventName
              theObject@recordTable <- theTable
              #optional printing
              if(showPrint){
                  out_msg <- paste0("current time: ", as.character(current_time))
                  writeLines(out_msg)
              }
              #check if the object is valid
              validObject(theObject)
              return(theObject)
          })

#deleteTracker method ----------------
setGeneric(name = "deleteTracker",
           def = function(theObject,eventName){
               standardGeneric("deleteTracker")
           })

setMethod(f = "deleteTracker",
          signature = "timeTracker",
          definition = function(theObject,eventName){
              bool_row <- theObject@recordTable$event %in% eventName
              theObject@recordTable <-  theObject@recordTable[!bool_row,]
              validObject(theObject)
              return(theObject)
          })

#getTracker method ---------------------------
#though you can get the record table
setGeneric(name = "getTracker",
           def = function(theObject,eventName){
               standardGeneric("getTracker")
           })
#print method for timeTracker ------------------
#get time tracker table as a data.frame
setMethod(f = "getTracker",
          signature = "timeTracker",
          definition = function(theObject){
              return(theObject@recordTable)
          })

#testing -----------
timer1 <- timeTracker()
timer1 <- updateTracker(timer1,"a")
Sys.sleep(sample(1:3,1))
timer1 <- updateTracker(timer1,"a")
Sys.sleep(sample(1:3,1))
timer1 <- updateTracker(timer1,"b")
Sys.sleep(sample(1:3,1))
timer1 <- updateTracker(timer1,"c")
Sys.sleep(sample(1:3,1))
timer1 <- deleteTracker(timer1,"b")
getTracker(timer1)
