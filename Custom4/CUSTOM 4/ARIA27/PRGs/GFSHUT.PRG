*E300492,1 Hesham El-Sheltawi (Start)  
*FUNCTION gfShutDown
DO CASE
  CASE RDLEVEL()>1 AND (TYPE('lcProgName')='C' AND !EMPTY(lcProgName))
    *B801006,1 Hesham El-Sheltawi 03/13/97 (Begin) 
    PRIVATE lcOldShut,llFlag,lcReadwind
    lcOldShut = ON('SHUT')
    ON SHUTDOWN llFlag =.T.
    lcReadwind = WOUTPUT()
    *B801006,1 Hesham El-Sheltawi 03/13/97 (End) 
      = gfMsgBox('TR','You have to close all the running programs'+;
         ' before closing the system','\?\!\<Ok','ALERT')
    *B801006,1 return the value of on shutdown 
    *B801006,1 then activate the program window if there is one
    ON SHUTDOWN &lcOldShut  
    IF !EMPTY(lcReadwind) AND WEXIST(lcReadwind)
      ACTIVATE WINDOW (lcReadwind)
    ENDIF
    *B801006,1 Hesham El-Sheltawi 03/13/97 (End) 
  CASE RDLEVEL()=1

     IF WEXIST('GWCCONTRL1') AND WREAD('GWCCONTRL1') AND glToolActv
  *     ACTIVATE WINDOW GWCCONTRL1
  *     _CUROBJ = OBJNUM(pbCls)
       KEYBOARD "{ALT+X}" 
       KEYBOARD "{ALT+X}"      
     ELSE
       DO gpExit
     ENDIF  
  OTHERWISE
    ON SHUT
    QUIT   
ENDCASE
*E300492,1 Hesham El-Sheltawi (End)
