#Create timeTracker class --------------
timeTracker <- setClass(Class = "timeTracker",
                         slots = c(
                             time = "POSIXct",
                             event = "character",
                             eventTable = "data.frame",
                             showPrint = "logical"
                             
                         ),
                         prototype = list(
                             time = c(),
                             event = c(),
                             eventTable = data.frame(event = character(),
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
                             if(class(object@eventTable) != "data.frame"){
                                 return("eventTable should be a data frame")
                             }
                             if(class(object@showPrint) != "logical"){
                                 return("showPrint should be a logical")
                             }
                             return(TRUE)
                         })

#startTracker method --------------
setGeneric(name = "startTracker",
           def = function(theObject,eventName){
               standardGeneric("startTracker")
           })

setMethod(f = "startTracker",
          signature = "timeTracker",
          definition = function(theObject,eventName){
              #inputs
              theTable <- theObject@eventTable
              showPrint <- theObject@showPrint
              current_time <- Sys.time()
              #create that record/row
              newRow <- data.frame(event = eventName,
                                   start = current_time,
                                   end = .POSIXct(character(1)),
                                   time_elasped = numeric(1),
                                   stringsAsFactors = FALSE)
              #detect if event already exist
              if(any(theTable$event %in% eventName) ){
                  #optional printing
                  if(showPrint){
                      out_msg <- paste0("Event: '",eventName,"' already exists. Overwriting previous one.\n")
                      writeLines(out_msg)
                  }
                  #replace the row in dataframe with new row
                  boolTargetRow <- theObject@eventTable$event == eventName
                  theObject@eventTable[boolTargetRow,] <- newRow
              } else {
                  #append new row
                  theObject@eventTable <- rbind(theTable,newRow)
              }
              theObject@time <- current_time
              theObject@event <- eventName
              validObject(theObject)
              return(theObject)
          })
#stopTracker method ----------------

setGeneric(name = "stopTracker",
           def = function(theObject,eventName){
               standardGeneric("stopTracker")
           })

setMethod(f = "stopTracker",
          signature = "timeTracker",
          definition = function(theObject,eventName){
              #inputs
              theTable <- theObject@eventTable
              showPrint <- theObject@showPrint
              current_time <- Sys.time()
              #detect if event already exists
              if(any(theTable$event %in% eventName)){
                  #detect if end time for event already exist
                  end_exist <- !is.na(as.character(theTable[theTable$event == eventName,][["end"]]))
                  if(end_exist){
                      #optional printing
                      if(showPrint){
                          out_msg <- paste0("Event: '",eventName,"' already has end. Overwriting previous record.\n")
                          writeLines(out_msg)
                      }
                  }
                  #modify the end anyway
                  boolEventRow <- theTable$event == eventName
                  startTime <- theObject@eventTable[boolEventRow,][["start"]]
                  timeElapsed <- as.numeric(current_time - startTime)
                  theObject@eventTable[boolEventRow,][["end"]] <- current_time
                  theObject@eventTable[boolEventRow,][["time_elasped"]] <- timeElapsed
              } else {
                  stop("Event: '",eventName,"'"," doesn't exist. Record won't be created.\n")
              }
              #optional printing
              if(showPrint){
                  out_msg <- paste0("For event: '",eventName,"'. ",round(timeElapsed,2)," seconds elapsed.\n")
                  writeLines(out_msg)
              }
              theObject@time <- current_time
              theObject@event <- eventName
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
              bool_row <- theObject@eventTable$event %in% eventName
              theObject@eventTable <-  theObject@eventTable[!bool_row,]
              validObject(theObject)
              return(theObject)
          })

#getTracker method ---------------------------
#though you can get the record table
setGeneric(name = "getTracker",
           def = function(theObject,eventName){
               standardGeneric("getTracker")
           })
#get time tracker table as a data.frame
setMethod(f = "getTracker",
          signature = "timeTracker",
          definition = function(theObject){
              validObject(theObject)
              return(theObject@eventTable)
          })

#custom print method S4 ----------------
setMethod(f = "show",
          signature = "timeTracker",
          definition = function(object){
              if(nrow(object@eventTable) == 0){
                  writeLines("Current timeTracker is empty.\n")
              } else {
                  current_table <- object@eventTable
                  out_msg <- paste0("Current event: '",object@event,"' at ",as.character(object@time),".\n")
                  writeLines(out_msg)
                  out_msg <- paste0("Current eventTable has ",nrow(current_table)," records:")
                  writeLines(out_msg)
                  print(current_table)
                  out_msg <- paste0("An object of class: '", class(object),"'")
                  writeLines(out_msg)
              }
          })

#test

timer <- timeTracker()
timer@showPrint <- FALSE
timer <- startTracker(timer,"a")
timer <- startTracker(timer,"a")
timer <- stopTracker(timer,"a")
timer <- stopTracker(timer,"a")
timer <- startTracker(timer,"b")
timer <- stopTracker(timer,"b")
timer <- startTracker(timer,"d")
timer <- startTracker(timer,"e")
timer <- stopTracker(timer,"e")
timer <- stopTracker(timer,"d")

timer <- deleteTracker(timer,"a")
getTracker(timer)
