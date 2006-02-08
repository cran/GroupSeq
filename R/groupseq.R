"groupseq" <-
function( mode=chosenMode )
{
  #default is gui mode
  if(missing(mode))
  {
    guiMode()
  }
  
  # "c" is console mode
  else if(mode=="c")
       {
         consoleMode()
       }
       # "g" is gui mode
       else if(mode=="g")
       {
         guiMode()
       }
       else
       {
         cat("Wrong call of groupseq() \n")
       }
}