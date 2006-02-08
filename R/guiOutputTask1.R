"guiOutputTask1" <-
function(n,alpha,t,lowerBounds,upperBounds,probDifference,probExit,BoundsSymmetry,spendingFunctionUsed)
{
  #Set Toplevel
  outTask1Toplevel <- tktoplevel()
  
  if(!BoundsSymmetry==3)
  {
    tkwm.title(outTask1Toplevel,paste("-1-    n=",n,  " ,alpha=",alpha[1]))
  }
  else
  {
    tkwm.title(outTask1Toplevel,paste("-1-    ,n=",n,  " ,alphaUPPER=",alpha[1]," ,alphaLOWER=",alpha[2]))
  }
  
  #Define main Frame
  OutputTask1 <- tkframe(outTask1Toplevel)

  #Define subframes
  staticFrame <- tkframe(OutputTask1,relief="groove",borderwidth=0)
  dynamicFrame <- tkframe(OutputTask1,relief="groove",borderwidth=0)
  parametersFrame <- tkframe(staticFrame,relief="groove",borderwidth=0)
  numbersFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  timesFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  lowerBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  upperBoundsFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  probDiffFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
  probExitFrame <- tkframe(dynamicFrame,relief="groove",borderwidth=2)
    
  #create label with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("n=",n)),sticky="w")  
  if(!BoundsSymmetry==3)
  {
    tkgrid( tklabel(parametersFrame, text=paste("alpha=",alpha[1])),sticky="w")  
  }
  else
  {
    tkgrid( tklabel(parametersFrame, text=paste("alpha - Upper Bounds=",alpha[1])),sticky="w") 
    tkgrid( tklabel(parametersFrame, text=paste("alpha - Lower Bounds=",alpha[2])),sticky="w")  
    
  }
    
  ##names of spending functions that could have been used
  FunctionNames <- c("O'Brien-Fleming Type","Pocock Type","Power family: alpha* t^phi",
                       "Hwang-Shih-DeCani fammily","Pocock - the real Pocock Bounds")
  if(!BoundsSymmetry==3)
  {
    tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[spendingFunctionUsed[1]])),sticky="w") 
  }
  else
  { 
    tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNames[spendingFunctionUsed[1]])),sticky="w")
    tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNames[spendingFunctionUsed[2]])),sticky="w")      
  }
      
  #create head labels
  tkgrid( tklabel(numbersFrame, text="n    "),sticky="w")  
  tkgrid( tklabel(timesFrame, text="Times   "),sticky="w")
  tkgrid( tklabel(lowerBoundsFrame, text="Lower Bounds  "),sticky="w")
  tkgrid( tklabel(upperBoundsFrame, text="Upper Bounds  "),sticky="w")
  tkgrid( tklabel(probDiffFrame, text="alpha[i]-alpha[i-1]  "),sticky="w")
  tkgrid( tklabel(probExitFrame, text="cumulative alpha  "),sticky="w")
     
  #create labels with results
  for(i in 1:n)
  {
    tkgrid( tklabel(numbersFrame, text=as.character(i)),sticky="w")  
    tkgrid( tklabel(timesFrame, text=as.character(round(t[i],digits=3))),sticky="w")
    tkgrid( tklabel(lowerBoundsFrame, text=as.character(round(lowerBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(upperBoundsFrame, text=as.character(round(upperBounds[i],digits=4))),sticky="w")
    tkgrid( tklabel(probDiffFrame, text=as.character(round(probDifference[i],digits=10))),sticky="w")
    tkgrid( tklabel(probExitFrame, text=as.character(round(probExit[i],digits=10))),sticky="w")
  }
  tkgrid( tklabel(dynamicFrame, text=""))   #blank line
  
  #put frames together
  tkgrid(parametersFrame,sticky="w")
  tkgrid(numbersFrame, timesFrame, lowerBoundsFrame, upperBoundsFrame, probDiffFrame, probExitFrame, sticky="w")
  tkgrid(staticFrame,sticky="w")
  tkgrid(dynamicFrame,sticky="w")
  
  
  
  ###########################################################################  
  ##function handles click onto button to show results of bounds in a graph##
  ###########################################################################
  onShowGraph <- function()
  {
    ## if one-Sided-Test we won´t see negative Z-Values
      if(BoundsSymmetry==1)
      {
        xCoordinate<-t
        yCoordinate<-upperBounds

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-1-  ,n=",n,"  ,",FunctionNames[spendingFunctionUsed[1]]),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4,
             xlab="Times",ylab="Standarized Z-Value",ylim=c(0,4))

        ##...then add lines between them
        lines(t,upperBounds,col="blue")
      }

      else
      {
        xCoordinate<-c(t,t)
        yCoordinate<-c(lowerBounds,upperBounds)

        ## first plotting bounds as points...
        plot(xCoordinate,yCoordinate,main=paste("-1-  ,n=",n,"  ,upper=",FunctionNames[spendingFunctionUsed[1]]," ,lower=",FunctionNames[spendingFunctionUsed[2]]),
             pch=21,bg="green",font=4,font.axis=4,font.lab=4,font.main=4,
             xlab="Times",ylab="Standarized Z-Value",ylim=c(-4,4))

        ##...then add lines between them
        lines(t,lowerBounds,col="blue")
        lines(t,upperBounds,col="blue")
      }
  }
  
  ################################################################## 
  ## function handles click onto button to save results in a file ##
  ##################################################################
  onSave <- function()
  {
     #create file variable  
     fileName <- tclvalue(tkgetSaveFile(initialfile=".html",filetypes="{{html Files} {.html}} {{All files} *}"))
     if (fileName=="") return;
     
     #open file
     zz <- file(fileName,"w")
     
     #output will be written in HTML
     cat("<html> <body> \n",file = zz)
     
     #output n
     cat("n=",n,"<br> \n",file = zz)
     
     ##ouput alpha
     if(BoundsSymmetry==1)
     {
       cat("alpha =",alpha[1],"<br>\n",file = zz)
     }
     else
     {
       cat("Upper alpha = ",alpha[1],"<br>\n",file = zz)
       cat("Lower alpha = ",alpha[2],"<br>\n",file = zz)
     } 

     ##output names of spending functions that were used
     FunctionNames <- c("O'Brien-Fleming Type","Pocock Type","Power family: alpha* t^phi",
                        "Hwang-Shih-DeCani fammily","Pocock - the real Pocock Bounds")
     if(BoundsSymmetry==1)
     {
       cat("<b>",FunctionNames[spendingFunctionUsed[1]],"</b>"," was used as spending Function.","<br>\n",file = zz)
     }
     else
     {       
       cat("Spending Function for UPPER Bound:","<b>",FunctionNames[spendingFunctionUsed[1]],"</b>","<br>\n",file = zz)
       cat("Spending Function for LOWER Bound:","<b>",FunctionNames[spendingFunctionUsed[2]],"</b>","<br>\n",file = zz)
     } 
    
     ##output the bounds
     cat("<br>\n",file = zz)
     cat("<table border=\"3\"> \n",file = zz)
     cat("<tr> \n",file = zz)
     cat("<td>Times &#160</td>  <td>Lower Bounds &#160</td>  <td>Upper Bounds &#160</td> \n",file = zz)
     cat("<td>alpha[i]-alpha[i-1] &#160</td>  <td>cumulative alpha &#160</td> \n",file = zz)
     cat("</tr> \n",file = zz)
     
     for(i in 1:n)
     {
       cat("<tr> \n",file = zz)
       cat("<td>",round(t[i],digits=3),"</td>",   "<td>",round(lowerBounds[i],digits=4),"</td>",   "<td>",round(upperBounds[i],digits=4),"</td>", 
           "<td>",round(probDifference[i],digits=10),"</td>",   "<td>",round(probExit[i],digits=10),"</td> \n",file = zz )
       cat("</tr> \n",file = zz)    
     }
         
     cat("</table> \n",file = zz)
     cat("</body> </html> \n",file = zz)
     close(zz)
  }
   
  
  #frame for the buttons
  buttonFrame<-tkframe(OutputTask1,relief="groove",borderwidth=0)

  #button to show graphic
  showGraph.button <-tkbutton(buttonFrame,text="  Show Graph  ",command=onShowGraph)
    
  #button to save in file
  save.button <-tkbutton(buttonFrame,text="  Save to File  ",command=onSave)

  #grid buttons
  tkgrid( tklabel(buttonFrame, text=""))   #blank line  
  tkgrid(showGraph.button,tklabel(buttonFrame, text="            "),save.button,sticky="we")
  tkgrid(buttonFrame)
  tkgrid( tklabel(buttonFrame, text=""))   #blank line  
  
  #grid allover frame and focus
  tkgrid(OutputTask1,sticky="w")
  tkfocus(outTask1Toplevel)
  
}#end <--*function(...)*
