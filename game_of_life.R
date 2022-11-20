

# https://statisticsblog.com/2010/05/05/game-of-life-in-r/
par(mar=c(0,0,0,0))
library(abind)
library(gdata)
library(fields)
library(grDevices)
library(caTools)
#
times = 50
myWidth = 10
myHeight = 10
#   
# Set the 3D array of rules to determine transitions for each cell.
rulesArray = NULL
for(i in 1:9) {
  toBind <- matrix(sample(c(0,1),replace=T,(myWidth*myHeight)),ncol=myWidth)
  rulesArray <- abind(rulesArray, toBind, along=3)
}
#
first = T
frames = array(0, c(myWidth, myHeight, times))
for(i in 1:times) {
  if(first) {
    forFrame <- sample(c(0,1),replace=T,(myWidth*myHeight))
    first = F
  } else {
    # Convert last frame data to matrix to make comparing adjacent cells easier
    forFrame <- matrix(forFrame, ncol=myWidth)
    newFrame <- forFrame
    #
    for(m in 1:myHeight) {
      for(n in 1:myWidth) {
        adjLiving = 1 # cuz we start with array index 1
        #
        # Find out how many adjacent are living
        if(m > 1 && n > 1) {
          # Look at top left
          adjLiving = adjLiving + forFrame[(m-1),(n-1)]
        }
        if(m > 1) {
          # Look at top center
          adjLiving = adjLiving + forFrame[(m-1),(n)]
        }
        if(m > 1 && n < myWidth) {
          # Look at top right
          adjLiving = adjLiving + forFrame[(m-1),(n+1)]
        }
        if(n > 1) {
          # Look at left
          adjLiving = adjLiving + forFrame[(m),(n-1)]
        }
        if(n < myWidth) {
          # Look at right
          adjLiving = adjLiving + forFrame[(m),(n+1)]
        }
        if(m < myHeight && n > 1) {
          # Look at bottom left
          adjLiving = adjLiving + forFrame[(m+1),(n-1)]
        }
        if(m < myHeight) {
          # Look at bottom center
          adjLiving = adjLiving + forFrame[(m+1),(n)]
        }
        if(m < myHeight && n < myWidth) {
          # Look at bottom right
          adjLiving = adjLiving + forFrame[(m+1),(n+1)]
        }
        #
        # Find the corresponding rule for this cell
        newStatus = rulesArray[m,n,adjLiving]
        #		
        # Update the status of this cell depending on the rule and number of living adjacent
        newFrame[m,n] = newStatus			
      }
    }
    #		
    # Pull it out of a matrix
    forFrame = unmatrix(newFrame)	
  }
  frames[,,i] <- forFrame
}
write.gif(frames, "gameOfLifeRevisited.gif", col=c("#FFFF00", "#000000") , delay=150)

