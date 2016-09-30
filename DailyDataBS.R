#This script will read in the workout data (primary key = workout, features=date, category, etc)
#and reorganize it into a daily data (primary key = date, features = workout information, injury information

#Read in the available data
dat=read.csv(file="FitNotes_Export_2016_09_22_14_49_18.csv", stringsAsFactors = F)
# convert dates to date - see if this helps with NAs by coercion
dat$Date=as.Date(dat$Date)
entries=unique(dat$Date) #Unique days recorded

# function to convert dat$Time to Minutes
mins <- function(x){
    # input: vector of strings in "hh:mm:ss" format representing duration,
    #   (not time of day)
    # output: vector with duration in minutes, numerical.
    # will return NA if x = ""

    # set up numeric vector to store results
    # (if store them in x they are converted back to character!)
    z <- numeric()

    for (i in 1:length(x)) {
        # Split the string at each ":"
        y <- strsplit(x, ":")
        # # [[1]]
        # # [1] "01" "02" "03"

        # assign the pieces of splitstring to appropriate variables and
        # convert to numeric
        hours <- as.numeric(y[[i]][1])
        minutes <- as.numeric(y[[i]][2])
        seconds <- as.numeric(y[[i]][3])

        #calculate the time in minutes
        z[i] <- ((hours*60) + (minutes) + (seconds/60))
        next
    }
    z
}
# use mins to convert dat$Time to mins
dat$Time <- mins(dat$Time)

#dat[is.na(dat)]=0

#Create names of features for daily data
names.workout=unique(dat$Exercise)
#convert names so can be used as column headings (get rid of spaces, "-", "(", etc)
names.workout=make.names(names.workout, unique = TRUE)
#Concatenate with features of exercises - this has to be done in steps because append(x,y)
#adds y to the end of x and takes no more input. Will include all possible features and remove excess later
#changed names to names.workout.full b/c 'names' is a function and can cause confusion
names.workout.full=append(paste(names.workout,"Weight", sep = "."),paste(names.workout,"Reps", sep = ".")) #will report sqrt of sum of squares
names.workout.full=append(names.workout.full,paste(names.workout,"ctheta_w", sep = ".")) #reports the cosine of the "angle" between reps and weight so "pseudo-work" can be computed
names.workout.full=append(names.workout.full,paste(names.workout,"Distance", sep = "."))
names.workout.full=sort(append(names.workout.full,paste(names.workout, "Time", sep = "."))) #Sort alphabetically because I'm OCD
names.workout.full=append("Day",names.workout.full)
names.workout.full=append(names.workout.full,"Injured") #0=no, 1=yes - always best to make categorical variables numerical if there's a natural mapping to do so
names.workout.full=append(names.workout.full,"Injured.Next.Week") #0=no, 1=yes - this makes it easier to see immediately if a workout may have contributed to injury

#Initialize the new data frame
days=data.frame(matrix(0,nrow=length(entries),ncol=length(names.workout.full)), stringsAsFactors = F)
# my attempt.. not working... there has to be an easier way!
#days <- as.data.frame(setNames(1:length(names.workout.full), names.workout.full), stringsAsFactors = F)

colnames(days)=names.workout.full
days$Day=as.Date(entries)

# all days column types are currently numeric.. double check that won't cause problems with code below

#Now populate each column for a given day and cycle through the days
for(x in seq(nrow(days)))    #Starting with looping over the active days
{       #Now loop over the workouts for that day
    daily.workout=split(split(dat, f = dat$Date)[[x]],split(dat, f = dat$Date)[[x]]$Exercise)
#   daily.workout is a list of data frames.  each exercise done that day is a data frame
#   with Date, Exercise, Category, Weight, Reps, Distance.Unit, Time, and Comment
#   all the exercise names in the daily.workout list and daily.workout$(exercisename w/ spaces, etc)$Exercise still have spaces and punctuation
    for(y in seq(length(daily.workout)))
    {
#       names.workout previously used for vector of unique values of exercise names - ok to overwrite?
        names.workout=make.names(unique(daily.workout[[y]]$Exercise)) #Get the name of the y-th workout for the x-th day, remove spaces and special chars
        category=unique(daily.workout[[y]]$Category)      #Get the category of the workout

        weight=sqrt(sum(daily.workout[[y]]$Weight..lbs.^2))                                      #Sqrt of sum of squares of weight - NAs will result where weight not entered
        reps=sqrt(sum(daily.workout[[y]]$Rep^2))                                                 #Sqrt of sum of squares of reps
#       ctheta will produce error if NA in weight or reps... it can't evaluate if first part is true or false if one is NA
        ctheta= if(weight>0 & reps>0){sum(daily.workout[[y]]$Weight..lbs. * daily.workout[[y]]$Reps )  /(weight*reps)}
        else {ctheta=0}                                                                        #Cosine of angle between reps and weight
#       distance unit - sometimes meters, sometimes miles.
        distance=sqrt(sum(daily.workout[[y]]$Distance^2))                                        #Sqrt of sum of squares of distance
#       Frequently distance is NA
        if(is.na(distance)){distance=0}
        time=sqrt(sum(as.numeric(daily.workout[[y]]$Time)^2))                                    #Sqrt of sum of squares of time
#       time frequently NA
        if(is.na(time)){time=0}

        days[x, colnames(days)==paste(names.workout,"Weight", sep = ".")]=weight
        days[x, colnames(days)==paste(names.workout,"Reps", sep = ".")]=reps
        days[x, colnames(days)==paste(names.workout,"ctheta_w", sep = ".")]=ctheta              #For some reason, this keeps returning a dimensionality error...will debug soon
        days[x, colnames(days)==paste(names.workout,"Distance", sep = ".")]=distance
        days[x, colnames(days)==paste(names.workout,"Time", sep = ".")]=time
    }
}