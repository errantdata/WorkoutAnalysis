#This script will read in the workout data (primary key = workout, features=date, category, etc)
#and reorganize it into a daily data (primary key = date, features = workout information, injury information

#Read in the available data
dat=read.csv(file="FitNotes_Export_2016_09_22_14_49_18.csv", stringsAsFactors = F)
entries=unique(dat$Date) #Unique days recorded
dat[is.na(dat)]=0

#Create names of features for daily data
names.workout=unique(dat$Exercise)
#Concatenate with features of exercises - this has to be done in steps because append(x,y)
#adds y to the end of x and takes no more input. Will include all possible features and remove excess later
names=append(paste(names.workout,"Weight", sep = " "),paste(names.workout,"Reps", sep = " ")) #will report sqrt of sum of squares
names=append(names,paste(names.workout,"ctheta_w", sep = " ")) #reports the cosine of the "angle" between reps and weight so "pseudo-work" can be computed
names=append(names,paste(names.workout,"Distance", sep = " "))
names=sort(append(names,paste(names.workout, "Time", sep = " "))) #Sort alphabetically because I'm OCD
names=append("Day",names)
names=append(names,"Injured?") #0=no, 1=yes - always best to make categorical variables numerical if there's a natural mapping to do so
names=append(names,"Injured Next Week?") #0=no, 1=yes - this makes it easier to see immediately if a workout may have contributed to injury

#Initialize the new data frame
days=data.frame(matrix(0,nrow=length(entries),ncol=length(names)))
colnames(days)=names
days$Day=as.Date(entries)

#Now populate each column for a given day and cycle through the days
for(x in seq(nrow(days)))    #Starting with looping over the active days
      {       #Now loop over the workouts for that day
         daily.workout=split(split(dat, f = dat$Date)[[x]],split(dat, f = dat$Date)[[x]]$Exercise)
         for(y in seq(length(daily.workout)))
                {
                  names.workout=unique(daily.workout[[y]]$Exercise) #Get the name of the y-th workout for the x-th day
                  category=unique(daily.workout[[y]]$Category)      #Get the category of the workout

                  weight=sqrt(sum(daily.workout[[y]]$Weight..lbs.^2))                                      #Sqrt of sum of squares of weight
                  reps=sqrt(sum(daily.workout[[y]]$Rep^2))                                                 #Sqrt of sum of squares of reps
                  ctheta= if(weight>0 & reps>0){sum(daily.workout[[y]]$Weight..lbs. * daily.workout[[y]]$Reps )  /(weight*reps)}
                    else {ctheta=0}                                                                        #Cosine of angle between reps and weight

                  distance=sqrt(sum(daily.workout[[y]]$Distance^2))                                        #Sqrt of sum of squares of distance
                  time=sqrt(sum(as.numeric(daily.workout[[y]]$Time)^2))                                    #Sqrt of sum of squares of time
                  if(is.na(time)){time=0}

                  days[x, colnames(days)==paste(names.workout,"Weight", sep = " ")]=weight
                  days[x, colnames(days)==paste(names.workout,"Reps", sep = " ")]=reps
                  days[x, colnames(days)==paste(names.workout,"ctheta_w", sep = " ")]=ctheta              #For some reason, this keeps returning a dimensionality error...will debug soon
                  days[x, colnames(days)==paste(names.workout,"Distance", sep = " ")]=distance
                  days[x, colnames(days)==paste(names.workout,"Time", sep = " ")]=time
                }
}

days=days[,colSums(days != 0) > 0]  #Trim the fat - we don't care about features that have all 0s