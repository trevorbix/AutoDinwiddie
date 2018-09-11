

shinyServer(function(input, output, session) {
  
  ## - SET CSV UPLOAD MAX SIZE
  options(shiny.maxRequestSize=30*1024^2) 
  
  rv <- reactiveValues()
  
  observeEvent(input$file1,{
    rv$NewDataFile <- read.csv(file=input$file1$datapath,stringsAsFactors = FALSE)
  })
  
  observeEvent(rv$NewDataFile,{
  DailyProjections <- rv$NewDataFile
    
    DailyProjections$MoneyBallProjBM <- 3*DailyProjections$threes+
      2*DailyProjections$twos + 
      1*DailyProjections$free.throws + 
      1.2*DailyProjections$rebounds + 
      1.5*DailyProjections$assists + 
      3*DailyProjections$blocks + 
      3*DailyProjections$steals -
      1*DailyProjections$turnovers 
    
    
    
    rv$PlayerTable <- as.data.frame(cbind(Initial_Names = DailyProjections %>% 
                                            select(last_name,first_name,MoneyBallProjBM,team,
                                                   moneyball_positions,status,
                                                   opponent,minutes,moneyball_price,odds_spread) %>% 
                                                  as.data.frame(),
                                          Import = shinyInput(FUN=checkboxInput,
                                                              id="i_",
                                                              num=length(rv$NewDataFile$first_name))))
                                                                      
                colnames(rv$PlayerTable) <- c("last_name","first_name","MoneyBallProjBM",
                                              "Team","moneyball_positions","Status",
                                              "Opp","ExpMin","moneyball_price","Odds","Keep")
                
               
})

output$playertable <- renderDT(rv$PlayerTable,
                                  caption = 'Fade',
                                  options = list(
                                    autoWidth = TRUE,
                                    orderClasses = TRUE,
                                    # stateSave = TRUE, 
                                    scrollX = TRUE,
                                    pageLength = 20,
                                    lengthChange = FALSE,
                                    drawCallback= JS(
                                      'function(settings) {
                                     Shiny.bindAll(this.api().table().node());}')
                                  ),
                                  selection='none',
                                  escape=F
)


#Optimise. 

observeEvent(input$optimise,{

#Subset the data to players that arent faded. 
DailyProjections <<- rv$PlayerTable

Information = data.frame()
Information[1,1] = "PG"
Information[1,2] = 2
Information[2,1] = "SG"
Information[2,2] = 2
Information[3,1] = "SF"
Information[3,2] = 2
Information[4,1] = "PF"
Information[4,2] = 2
Information[5,1] = "C"
Information[5,2] = 1
TeamCount = 0
FinalTeams = list()
PlayerProj = list()
PlayerPosition = list()
EliteCount = numeric(46)
SecondaryCount = numeric(46)
OtherCount = numeric(46)
OverallPrice = numeric(46)

A <- matrix(0, nrow = 6, ncol = nrow(DailyProjections))
for (i in 1:nrow(DailyProjections)){
  A[which(as.character(DailyProjections[i,"moneyball_positions"])==Information[,1]),i]<-1
}
A[6, ] <- DailyProjections$moneyball_price
const.mat <- A
objective.in <- DailyProjections$MoneyBallProjBM
const.dir <- c("==", "==", "==", "==","==", "<=")
const.rhs <- c(Information[1,2], Information[2,2],
               Information[3,2],Information[4,2],Information[5,2],60000)



solRaw <- lp(direction = "max", objective.in,     # maximize objective function
             const.mat, const.dir, const.rhs,     # constraints
             all.bin = TRUE,use.rw = TRUE, num.bin.solns=1,compute.sens = 1) # use binary variables      only
TeamCount = TeamCount+1
FinalTeams[[TeamCount]]=cbind(as.character(DailyProjections[which(solRaw$solution==1),c("first_name")]),as.character(DailyProjections[which(solRaw$solution==1),c("last_name")]))
PlayerPosition[[TeamCount]] = as.character(DailyProjections[which(solRaw$solution==1),c("moneyball_positions")])
PlayerProj[[TeamCount]] = as.character(DailyProjections[which(solRaw$solution==1),c("MoneyBallProjBM")])

OtherCount[[TeamCount]] = solRaw$objval
OverallPrice[1] = sum(DailyProjections[which(solRaw$solution==1),c("moneyball_price")])

for (i in 1:9){
  DailyProjectionsTemp = DailyProjections[-which(solRaw$solution==1)[i],]
  A <- matrix(0, nrow = 6, ncol = nrow(DailyProjectionsTemp))
  for (j in 1:nrow(DailyProjectionsTemp)){
    A[which(as.character(DailyProjectionsTemp[j,"moneyball_positions"])==Information[,1]),j]<-1
  }
  A[6, ] <- DailyProjectionsTemp$moneyball_price
  const.mat <- A
  objective.in <- DailyProjectionsTemp$MoneyBallProjBM
  sol <- lp(direction = "max", objective.in,     # maximize objective function
            const.mat, const.dir, const.rhs,     # constraints
            all.bin = TRUE,use.rw = TRUE, num.bin.solns=1,compute.sens = 1) # use binary variables
  
  TeamCount = TeamCount+1
  FinalTeams[[TeamCount]]=cbind(as.character(DailyProjectionsTemp[which(sol$solution==1),c("first_name")]),as.character(DailyProjectionsTemp[which(sol$solution==1),c("last_name")]))
  PlayerPosition[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_positions")])
  PlayerProj[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("MoneyBallProjBM")])
  
  OtherCount[[TeamCount]] = sol$objval
  OverallPrice[TeamCount] = sum(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_price")])
}

for (i in 1:8){
  for (ii in (i+1):9){
    
    DailyProjectionsTemp = DailyProjections[-which(solRaw$solution==1)[c(i,ii)],]
    A <- matrix(0, nrow = 6, ncol = nrow(DailyProjectionsTemp))
    for (j in 1:nrow(DailyProjectionsTemp)){
      A[which(as.character(DailyProjectionsTemp[j,"moneyball_positions"])==Information[,1]),j]<-1
    }
    A[6, ] <- DailyProjectionsTemp$moneyball_price
    const.mat <- A
    objective.in <- DailyProjectionsTemp$MoneyBallProjBM
    sol <- lp(direction = "max", objective.in,     # maximize objective function
              const.mat, const.dir, const.rhs,     # constraints
              all.bin = TRUE,use.rw = TRUE, num.bin.solns=1,compute.sens = 1) # use binary variables
    TeamCount = TeamCount+1
    FinalTeams[[TeamCount]]=cbind(as.character(DailyProjectionsTemp[which(sol$solution==1),c("first_name")]),as.character(DailyProjectionsTemp[which(sol$solution==1),c("last_name")]))   
    PlayerPosition[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_positions")])
    PlayerProj[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("MoneyBallProjBM")])
    
    OtherCount[[TeamCount]] = sol$objval
    OverallPrice[TeamCount] = sum(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_price")])
  }
  
}

for (i in 1:7){
  for (ii in (i+1):8){
    for (iii in (ii+1):9){
      DailyProjectionsTemp = DailyProjections[-which(solRaw$solution==1)[c(i,ii,iii)],]
      A <- matrix(0, nrow = 6, ncol = nrow(DailyProjectionsTemp))
      for (j in 1:nrow(DailyProjectionsTemp)){
        A[which(as.character(DailyProjectionsTemp[j,"moneyball_positions"])==Information[,1]),j]<-1
      }
      A[6, ] <- DailyProjectionsTemp$moneyball_price
      const.mat <- A
      objective.in <- DailyProjectionsTemp$MoneyBallProjBM
      sol <- lp(direction = "max", objective.in,     # maximize objective function
                const.mat, const.dir, const.rhs,     # constraints
                all.bin = TRUE,use.rw = TRUE, num.bin.solns=1,compute.sens = 1) # use binary variables
      TeamCount = TeamCount+1
      FinalTeams[[TeamCount]]=cbind(as.character(DailyProjectionsTemp[which(sol$solution==1),c("first_name")]),as.character(DailyProjectionsTemp[which(sol$solution==1),c("last_name")]))   
      PlayerPosition[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_positions")])
      PlayerProj[[TeamCount]] = as.character(DailyProjectionsTemp[which(sol$solution==1),c("MoneyBallProjBM")])
      
      OtherCount[[TeamCount]] = sol$objval
      OverallPrice[TeamCount] = sum(DailyProjectionsTemp[which(sol$solution==1),c("moneyball_price")])
    }
    
  }
}

TotalCount = !duplicated(FinalTeams)
for (i in 1:130){
  if (i == 1){
    PlayerPosition[[1]][PlayerPosition[[1]]=="PG"]="Z"
    FinalTeamsPlayers <- as.data.frame(cbind(FinalTeams[[1]],TotalCount[[1]],1,PlayerPosition[[1]],PlayerProj[[1]],OverallPrice[1]))
    FinalTeamsPlayers <- FinalTeamsPlayers[order(FinalTeamsPlayers$V5,decreasing = TRUE),]
    levels(FinalTeamsPlayers$V5)[levels(FinalTeamsPlayers$V5)=="Z"] <- "PG"
    colnames(FinalTeamsPlayers) <- c("First","Last","Put in?","IterationNumber","Position","Value","OverallPrice")
  }else{
    PlayerPosition[[i]][PlayerPosition[[i]]=="PG"]="Z"  
    FinalTeamsPlayersTemp <- as.data.frame(cbind(FinalTeams[[i]],TotalCount[[i]],i,PlayerPosition[[i]],PlayerProj[[i]],OverallPrice[i]))
    FinalTeamsPlayersTemp <- FinalTeamsPlayersTemp[order(FinalTeamsPlayersTemp$V5,decreasing = TRUE),]
    levels(FinalTeamsPlayersTemp$V5)[levels(FinalTeamsPlayersTemp$V5)=="Z"] <- "PG"
    colnames(FinalTeamsPlayersTemp) <- c("First","Last","Put in?","IterationNumber","Position","Value","OverallPrice")
    FinalTeamsPlayers <- rbind(FinalTeamsPlayers,FinalTeamsPlayersTemp)
  }
}

Assigned = character(130*9)
Counter = 0
for (i in 1:130){
  if (!duplicated(FinalTeams)[i]==TRUE){
    Counter = Counter+1
    if (Counter>0&Counter<=15){
      Assigned[((i-1)*9)+1] = paste('LORD BIX',Counter)
      Assigned[(i*9)] = paste('LORD BIX',Counter)
    }
    if(Counter>15&Counter<=30){
       Assigned[((i-1)*9)+1] = paste('TOMMYWOMMY',Counter-15)
       Assigned[(i*9)] = paste('LW',Counter-15)
     }
    if(Counter>30&Counter<=45){
      Assigned[((i-1)*9)+1] = paste('THE CURSE',Counter-25)
      Assigned[i*9] = paste('THE CURSE',Counter-25)
    }
  }
  
}

rv$FinalTeamsPlayers <- cbind(FinalTeamsPlayers,Assigned)
  
})

output$opts <- renderDT(rv$FinalTeamsPlayers,
                               caption = 'Teams',
                               options = list(
                                 autoWidth = TRUE,
                                 orderClasses = TRUE,
                                 # stateSave = TRUE, 
                                 scrollX = TRUE,
                                 pageLength = 20,
                                 lengthChange = FALSE,
                                 drawCallback= JS(
                                   'function(settings) {
                                   Shiny.bindAll(this.api().table().node());}')
                                 ),
                               selection='none',
                               escape=F
                               )
#Player Statistics Function

PlayerStats <- function(final) {
  
  rv$UniqueTeamsNo <- final %>% filter(`Put in?`=="TRUE") %>% nrow()
  rv$UniqueTeamsNo <- rv$UniqueTeamsNo/9
  
  UniqueTeams <- rv$FinalTeamsPlayers %>% filter(`Put in?`=="TRUE")
  
  rv$PlayerCount <- UniqueTeams %>% group_by(First,Last) %>% summarise(count=n()) %>% arrange(-count)

  rv$PlayerCount$Percentage <- rv$PlayerCount$count/rv$UniqueTeamsNo
}

# Calculate Exposure Per Team. 

# TeamStats <- function(final) {
#   
#     UniqueTeams <- rv$FinalTeamsPlayers %>% filter(`Put in?`=="TRUE")
#   
#   rv$PlayerCount <- UniqueTeams %>% group_by(First,Last) %>% summarise(count=n()) %>% arrange(-count)
#   
#   rv$PlayerCount$Percentage <- rv$PlayerCount$count/rv$UniqueTeamsNo
# }

#Activate when calculate button is pressed. 
observeEvent(input$calculate,{
  req(rv$FinalTeamsPlayers)
  PlayerStats(rv$FinalTeamsPlayers)
  # TeamStats(rv$FinalTeamsPlayers)
})


output$ExposurePlot <- renderPlot({
   req(rv$PlayerCount)
 
   rv$PlayerCount %>%
    ggplot(aes(x=reorder(paste(First,Last,sep=" "),Percentage), y=Percentage))+
    geom_bar(stat="identity",fill="red") +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)+
    coord_flip() +
    ylab("Exposure") +
    xlab("")
    })

})
