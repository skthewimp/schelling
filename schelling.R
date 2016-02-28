require(reshape)
require(ggplot2)

relmat <- function(mtr,movex,movey)
  # move given matrix mtr by given number of steps in X and Y direction and return new matrix
  # if it spills outside of the matrix, return NA 
{
  mtr2 <- matrix(ncol=nrow(mtr)+2*abs(movex),nrow=ncol(mtr)+2*abs(movey)) # auxiliary matrix
  mtr2[abs(movey) + movey + (1:nrow(mtr)),abs(movex)+movex + (1:ncol(mtr))] <- mtr
  return(mtr2[abs(movey)+1:nrow(mtr),abs(movex)+1:ncol(mtr)])
}

# list out all squares that are friendly for 1s and those friendly for 0s
# list out all empty squares friendly for 1s, all empty squares friendly for 0s
# find all "uncomfortable cells" and in sequence find one that is friendly for it and put it there
# we need list of empty cells which is dynamically updated in this process, so that we can send these
# uncomfortable cells there

countneighbours <- function(mtr, value, lonelyok = F) # count how many neighbours of each cell of mtr is equal to value
  # lonelyok is a flag that says whether people are okay having no neighbours at all. 
{
  counts <- array(dim=c(8,nrow(mtr),ncol(mtr)))
  compare <- matrix(rep(value,length(mtr)),nrow = nrow(mtr)) # comparison matrix
  counts[1,,] <- compare==relmat(mtr,-1,-1)
  counts[2,,] <- compare==relmat(mtr,-1,0)
  counts[3,,] <- compare==relmat(mtr,-1,1)
  counts[4,,] <- compare==relmat(mtr,0,-1)
  counts[5,,] <- compare==relmat(mtr,0,1)
  counts[6,,] <- compare==relmat(mtr,1,-1)
  counts[7,,] <- compare==relmat(mtr,1,0)
  counts[8,,] <- compare==relmat(mtr,1,1)
  counts[is.na(counts)] <- lonelyok # all blanks and edge cases are counted as hostile
  numneigh <- apply(counts,2:3,sum)
  return(numneigh)
}

reconfigure <- function(mtr,minimum,maxswitch=NA,lonelyok=F) # given a matrix and minimum number of like neighbours, reconfigure it
{
  emptycells <- is.na(mtr)
  #values <- unique(as.vector(mtr)) # how many possible kinds of people? 
  #friendlies <- array(dim=c(length(values),nrow(mtr),ncol(mtr))) # friendly cells for each 
  friendly0 <- countneighbours(mtr,0,lonelyok) >= minimum #cells that are friendly to 0
  friendly1 <- countneighbours(mtr,1,lonelyok) >= minimum #cells that are friendly to 1
  displace <- !is.na(mtr) & ( (mtr==1 & !friendly1) | (mtr==0 & !friendly0) ) # a cell in a hostile territory 
  dps <- melt(displace)
  dps <- dps[dps$value,]
  if(nrow(dps)==0)
    return(list(mtr,F))
  numline <- min(nrow(dps),maxswitch,na.rm=T)
  change <- F
  for(i in 1:numline)
  {
    rnd <- sample(1:nrow(dps),1)
    if(!is.na(mtr[dps$X1[i],dps$X2[i]]) & mtr[dps$X1[i],dps$X2[i]]==0) # the chosen cell has a 0
      newcell <- emptycells & friendly0 else
        newcell <- emptycells & friendly1 
    nc2 <- melt(newcell)
    nc2 <- nc2[nc2$value,]
    if(nrow(nc2)>0) # there is some place this guy can go
    {
      change <- T
      dest <- sample(1:nrow(nc2),1) # choose an empty fiendly cell at random
      mtr[nc2$X1[dest],nc2$X2[dest]] <- mtr[dps$X1[i],dps$X2[i]] # send this there
      mtr[dps$X1[i],dps$X2[i]] <- NA # this cell is now empty
      emptycells[nc2$X1[dest],nc2$X2[dest]] <- F # someone's occupied this now
      emptycells[dps$X1[i],dps$X2[i]] <- T # this cell is now empty
    }
  }
  return(list(mtr,change))  
}

showgrid <- function(mtr) # plot the grid
{
  #mtr[is.na(mtr)] <- 2 
  mtr2 <- melt(mtr)
  mtr2$value <- as.factor(mtr2$value)
  #levels(mtr2$value) <- c("Type1","Type2","Empty")
  p <- ggplot(mtr2,aes(x=X1,y=X2,fill=value)) + geom_tile() + scale_fill_brewer(palette="Set1") + theme_bw(24)
  p <- p + scale_x_continuous("",breaks=c()) + scale_y_continuous("",breaks=c()) + coord_equal()
  print(p)
}

simulate <- function(gridsize,num1,num2,numiter=100,threshold=3,maxswitch=NA,
                     filename='schellingsim.pdf',singlegraph=F,printevery=1,lonelyok=F)
{
  numempty <- gridsize*gridsize - num1 - num2 
  vec <- c(rep(0,num1),rep(1,num2),rep(NA,numempty))
  mtr <- matrix(vec[sample(1:length(vec),length(vec),F)],nrow=gridsize)
  if(!singlegraph) pdf(filename,10,10)
  if(!singlegraph) showgrid(mtr)
  mtr3 <- melt(mtr)
  mtr3$Iteration <- 0
  for(i in 1:numiter)
  {
    mtr2 <- reconfigure(mtr,threshold,maxswitch,lonelyok)
    if(!mtr2[[2]])
    {
      mtr1 <- melt(mtr)
      mtr1$Iteration <- i 
      mtr3 <- rbind(mtr3,mtr1)
      if(!singlegraph) showgrid(mtr)
      break
    } 
    mtr <- mtr2[[1]]
    if(i %% printevery == 0) # print every ith graph
    {
      mtr1 <- melt(mtr)
      mtr1$Iteration <- i 
      mtr3 <- rbind(mtr3,mtr1)
      if(!singlegraph) showgrid(mtr)
    }
  }
  if(singlegraph)
  {
    mtr3$value <- factor(mtr3$value)
    p <- ggplot(mtr3,aes(x=X1,y=X2,fill=value)) + geom_tile() + scale_fill_brewer(palette="Set1") + theme_bw(24)
    p <- p + scale_x_continuous("",breaks=c()) + scale_y_continuous("",breaks=c()) + facet_wrap(~Iteration)
    p <- p + theme(legend.position='none')
    print(p)
  }
  if(!singlegraph) dev.off() 
}
  