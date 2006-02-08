"guiOutputTask4" <-
function(n,confidenceLevel,secondTimeScaleIsUsed,t,t2,t2max,lowerBounds,upperBounds,BoundsSymmetry,
         enterBoundsManually,alpha,confidenceIntervall,spendingFunctionUsed,Zvalue)
{
  
  
  #Set Toplevel
  outTask4Toplevel <- tktoplevel()
  tkwm.title(outTask4Toplevel,paste("-4-    n=",n," ,Z value=",Zvalue))
  
  #Define main Frame
  OutputTask4 <- tkframe(outTask4Toplevel)
  
  #Define subframes
  parametersFrame <- tkframe(OutputTask4,relief="groove",borderwidth=0)
  
  #create labels with parameter values:
  tkgrid( tklabel(parametersFrame, text=paste("n=",n)),sticky="w")  
  
  ##names of spending functions that could have been used
  FunctionNames <- c("O'Brien-Fleming Type","Pocock Type","Power family: alpha* t^phi",
                       "Hwang-Shih-DeCani fammily","Pocock - the real Pocock Bounds")
  if(!BoundsSymmetry==3)
  {
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="manually entered Bounds"),sticky="w") 
    }
    else
    {
      tkgrid( tklabel(parametersFrame, text=paste("Function: ",FunctionNames[spendingFunctionUsed[1]])),sticky="w") 
    }
  }
  else
  { 
    if(enterBoundsManually)
    {
      tkgrid( tklabel(parametersFrame, text="Upper Bounds: manually entered "),sticky="w")
      tkgrid( tklabel(parametersFrame, text="Lower Bounds: manually entered "),sticky="w")      
    }
    else
    {
      tkgrid( tklabel(parametersFrame, text=paste("Function - Upper Bounds: ",FunctionNames[spendingFunctionUsed[1]])),sticky="w")
      tkgrid( tklabel(parametersFrame, text=paste("Function - Lower Bounds: ",FunctionNames[spendingFunctionUsed[2]])),sticky="w")      
    }
  }


  #confidence- level and intervall 
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line 
  tkgrid( tklabel(parametersFrame, text=paste("Confidence Level= ",confidenceLevel,"%")),sticky="w") 
  tkgrid( tklabel(parametersFrame, text=paste("Confidence Intervall= <",round(confidenceIntervall[1],digits=5)," , ",
                                                                      round(confidenceIntervall[2],digits=5),">")),sticky="w") 
  tkgrid( tklabel(parametersFrame, text="Drift is equal to the expectation of the Z statistic when time=1."),sticky="w") 
  if(t2max!=0)
  {
    tkgrid( tklabel(parametersFrame, text=paste("Maximum Information=",t2max)),sticky="w") 
  }
  
  tkgrid( tklabel(parametersFrame, text=""),sticky="w") #blank line 
  tkgrid(parametersFrame,sticky="w")
 
   
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
        plot(xCoordinate,yCoordinate,main=paste("-4-  ,n=",n,"  ,",FunctionNames[spendingFunctionUsed[1]]),
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
        plot(xCoordinate,yCoordinate,main=paste("-4-  ,n=",n,"  ,upper=",FunctionNames[spendingFunctionUsed[1]]," ,lower=",FunctionNames[spendingFunctionUsed[2]]),
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
     
     #output will be writed in HTML
     cat("<html> <body> \n",file = zz)
     
     #output n
     cat("n=",n,"<br> \n",file = zz)
     
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
    
     #confidence- level and intervall 
     cat("<br>Confidence Level= ",confidenceLevel,"% <br>\n",file = zz)
     cat("Confidence Intervall= <",round(confidenceIntervall[1],digits=5)," , ",
                                   round(confidenceIntervall[2],digits=5),"> <br>\n",file = zz)
     cat("Drift is equal to the expectation of the Z statistic when time=1.<br> \n",file=zz)
     if(t2max!=0)
     {
       cat("Maximum Information=",t2max,"<br> \n",file=zz)
     }
             
     cat("</body> </html> \n",file = zz)
     close(zz)
  }
   
  
  #frame for the buttons
  buttonFrame<-tkframe(OutputTask4,relief="groove",borderwidth=0)

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
  tkgrid(OutputTask4,sticky="w")
  tkfocus(outTask4Toplevel)


   
}#end <--*function(...)*
