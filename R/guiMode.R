"guiMode" <-
function()
{
  require(tcltk)

  taskWindow<-tktoplevel()
  tkwm.title(taskWindow,"Choose a Task")
  listBoxTasks<-tklistbox(taskWindow,height=4,width=40,selectmode="single",background="white")
  tkgrid(tklabel(taskWindow,text="Select a Task!"))
  tkgrid(listBoxTasks)
  tasks <- c("-1- Compute bounds","-2- Compute drift given power and bounds",
    "-3- Compute probabilities given bounds and drift.","-4- Compute confidence interval.")
  for (i in (1:4))
  {
    tkinsert(listBoxTasks,"end",tasks[i])
  }
  tkselection.set(listBoxTasks,0)  # Default task is Task -1-.  Indexing starts at zero.

  OnOKtaskWindow <- function()
  {
    taskChoice <- as.numeric(tkcurselection(listBoxTasks))+1
    
    #call according function
    switch(taskChoice, guiInputTask1(), guiInputTask2(), guiInputTask3(), guiInputTask4() )
   
  }
  OK.but <-tkbutton(taskWindow,text="   OK   ",command=OnOKtaskWindow)
  tkgrid(OK.but)
  tkfocus(taskWindow)

}
