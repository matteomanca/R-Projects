#setwd('/Users/Matteo/Documents/Projects/R')
#source('tagFreq.R')
#tagSim('hetrec2011-delicious-2k/user_tag_test.dat')

#resSim('hetrec2011-delicious-2k/user_taggedbookmarks_test.dat')
#install.packages('amap')

library('amap')
#library('data.table')
tagSim <- function(user_tag_fileName){        
        #create dataframe from file
        userTagDF <- read.table(user_tag_fileName, header = TRUE, sep=";")        

        #create user tag frequencies table
        freqTable <- table(userTagDF)  
        
        numberOfUsers <- length(unique(userTagDF$userID))
      
        #freqTable
        #compute the perason correlation between each pair of users (each row of the table. Remember: each row represents the frequancies related to a given user, each columns is related to a given tag)
        userSimMatrix = as.matrix(Dist(freqTable, method = "pearson", diag = FALSE, upper = FALSE, nbproc=5))
        #traingMatrix = userSimMatrix[lower.tri(userSimMatrix, diag = TRUE)]
        #write(traingMatrix, file = "hetrec2011-delicious-2k/outFiles/userSims.dat", sep = ";") #, ncolumns = numberOfUsers 
        
        write.table(format(userSimMatrix,digit=3), file = "hetrec2011-delicious-2k/outFiles/userDist-tag-based.dat", sep = ";" ) #, ncolumns = numberOfUsers  
        #userSimMatrix
 
}


resSim <- function(user_taggedbookmarks_fileName){        
        #load dataframe from file
        user_taggedbookmarks <- read.table(user_taggedbookmarks_fileName, header = TRUE, sep="\t")        
        #head(user_taggedbookmarks)
        #head(user_taggedbookmarks$userID)
        
        #vector of columns that we are going to use in htis function
        keepsCol <- c("userID","bookmarkID")
        
        #new dataframe considering just the selected columns
        userBookmarkDF = user_taggedbookmarks[keepsCol]
        #head(userBookmarkDF)
        
        #table containing frequencies for each user and for each resources
        freqTable <- table(userBookmarkDF)  
        
        #transform the frequencies table in a binary table with value 1 if the user considered the resource 0 otherwise
        freqTable[freqTable > 0] <- 1
#        print(freqTable)
        
        userSimMatrix = as.matrix(Dist(freqTable, method = "binary", diag = FALSE, upper = FALSE, nbproc=5))
#        userSimMatrix
        
        write.table(format(userSimMatrix,digit=3), file = "hetrec2011-delicious-2k/outFiles/userDist-resource-based.dat", sep = ";" ) #, ncolumns = numberOfUsers  
        
}

buildRecommendations <- function(){ 
        tbsim <- read.table('hetrec2011-delicious-2k/outFiles/ok_userDist-tag-based.dat', header = TRUE, sep=";", row.names=1)        
        rbsim <- read.table('hetrec2011-delicious-2k/outFiles/ok_userDist-resource-based.dat', header = TRUE, sep=";", row.names=1)    
        ##delete the upper side and the diagonal; we have a simetric matrix so we can consider just  a triangular (either the lower or the upper )
        tbsim[upper.tri(tbsim, diag = TRUE)] = -5      ##-5 is just a convention
        rbsim[upper.tri(rbsim, diag = TRUE)] = -5      ##-5 is just a convention
        userIDs = as.numeric(row.names(tbsim))  #retrieve the userIDs
        
        threshold = seq(0.1, 1, by=0.1)
        
        
        ##Load connections by dataset
        alldata_connections <- read.table('hetrec2011-delicious-2k/user_contacts.dat', header = TRUE, sep="\t")        
        
        #vector of columns that we are going to use in htis function
        keepsCol <- c("userID","contactID")
        
        #new dataframe considering just the selected columns
        connectionsDF = as.matrix(alldata_connections[keepsCol])
        res = data.frame()  
       # setnames(res, new = c("ts","ui","tp","fp","precision"))
        #names(res) <- c("ts","ui","tp","fp","precision")
        for (ts in threshold) {
                for (ui in threshold) {
                        #I will use < becouse the matrixes contain the distances and not the similarities (or use 1 - dist)
                        UserPairsIndexes = which(1- tbsim > ts & 1 - rbsim > ui & rbsim != -5, arr.ind = TRUE) #retrieve the matrix index (and not the row/column names) that would be the userIDs                 
                        recommendations = matrix(userIDs[UserPairsIndexes],ncol=2)                        
                        filename = paste("rec",as.character(ts), as.character(ui),".txt", sep="_")
                        write.table(recommendations, file = paste("hetrec2011-delicious-2k/outFiles/recs/", filename,sep=""), sep = ";" ) #, ncolumns = numberOfUsers  
                        
                        #####
                        #check wich row of M1 is in M2 
                        #####
                        connectionsDFK = setkey(data.table(connectionsDF))  ###connections in the dataset
                        recommendationsK = setkey(data.table(recommendations))  ####recommendations
                        found = connectionsDFK[recommendationsK,which=TRUE]
                        tp = sum( !is.na( found ) )
                        fp = sum( is.na( found ) )
                        precision = tp/(tp+fp)
                        newRes = c(ts,ui,tp,fp,precision)
                        res <- rbind(res,newRes)
                        #print(res)
                }
        }    
       
        plot("Jaccard","Precision",xlim = c(0,1),ylim = c(0,1),type = "n")
        
        cl <- rainbow(10)
        #x = res[1:10,2]
       #imseq = seq(1,100,by=10)
       #clindex=1;
      # for(im in imseq){
               #y= res[im:im+9,5]
                y= res[1:10,5]
               lines(seq(0.1, 1, by=0.1),y,col = cl[1],type = 'b')       
                #clindex=clindex + 1
        #}
       y= res[11:20,5]
       lines(seq(0.1, 1, by=0.1),y,col = cl[2],type = 'b')       
      y= res[21:30,5]
      lines(seq(0.1, 1, by=0.1),y,col = cl[3],type = 'b')       
   
}


#head(userTagDF)
#names(userTagDF)
#print(userTagDF$userID[1])
#print(userTagDF$tagID[1])
#out_user_tag_DF <- data.frame()
#numberOfUsers <- length(unique(userTagDF$userID))
#numberOfTags <- length(unique(userTagDF$tagID))

#print(numberOfUsers)
#print(numberOfTags)


#print(unique(userTagDF$userID))



#print(length(unique(userTagDF$userID)))
#print(length(unique(userTagDF$tagID)))

#print(max(userTagDF$tagID))
#print(max(userTagDF$userID))



#######
#create a matrix initialized with all 0s. eanch row is a user and each column is a tag
#the matrix will contain the frequancy for each user and each tag
#######       
#out_user_tag_matrix <- matrix(rep(0, numberOfUsers*numberOfTags), numberOfUsers, numberOfTags)
#out_user_tag_matrix <- matrix()
#rownames(out_user_tag_matrix) <- unique(userTagDF$userID) #assign names (userID) to rows 
#colnames(out_user_tag_matrix) <- unique(userTagDF$tagID) #assign names (tagID) to columns
# for(i in 1: nrow(userTagDF)){  
#        print(i)
#               out_user_tag_matrix[as.character(userTagDF$userID[i]),as.character(userTagDF$tagID[i])] <- out_user_tag_matrix[as.character(userTagDF$userID[i]),as.character(userTagDF$tagID[i])] + 1
#      }

#userTagFreqDF =  as.data.frame(freqTable)

#out_user_tag_matrix = table(userTagDF)
#dim(out_user_tag_matrix)

#userTagFreqDF
#out_user_tag_matrix

#which(W > 3, arr.ind = TRUE)

#tbsim <- read.table('hetrec2011-delicious-2k/outFiles/userDist-tag-based.dat', header = TRUE, sep=";", row.names=1)        
#rbsim <- read.table('hetrec2011-delicious-2k/outFiles/userDist-resource-based.dat', header = TRUE, sep=";", row.names=1)    
##delete the upper side and the diagonal; we have a simetric matrix so we can consider just  a triangular (either the lower or the upper )
#tbsim[upper.tri(tbsim, diag = TRUE)] = -5      ##-5 is just a convention
#rbsim[upper.tri(rbsim, diag = TRUE)] = -5      ##-5 is just a convention
#UserPairsIndexes = which(tbsim < 0.5 & rbsim < 0.5 & rbsim != -5, arr.ind = TRUE) #retrieve the matrix index and not the row/column names that would be the userIDs


#userIDs = as.numeric(row.names(tbsim))  #retrieve the userIDs
#userIDs[UserPairsIndexes[1,1]]      ##user 1 
#userIDs[UserPairsIndexes[1,2]]      ##user 2 


#######USER 1 and USER 2 is a recommendation to obtain all recommendations
#userIDs[UserPairsIndexes[i,1]]      ##user 1 
#userIDs[UserPairsIndexes[i,2]]      ##user 2 

##########freqTable[freqTable > 0] <- 1

#y = userIDs[UserPairsIndexes]
#y = matrix(userIDs[UserPairsIndexes],ncol=2)

#####
#check wich row of M1 is in M2 
#####
#M1 = setkey(data.table(m1))  ###connections in the dataset
#M2 = setkey(data.table(m2))  ####recommendations
#M1[M2,which=TRUE]

