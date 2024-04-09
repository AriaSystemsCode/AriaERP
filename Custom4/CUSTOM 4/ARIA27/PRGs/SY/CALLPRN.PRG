*:************************************************************************
*: Program file  : CALLPRN.PRG
*: Program desc. : Main system
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
IF _WINDOWS
 DEFINE WINDOW O_home IN SCREEN;
    FROM SROWS()-1, 0 TO SROWS()-1, SCOLS()-1;
    NONE COLOR SCHEME 24
  ACTIVATE WINDOW O_HOME NOSHOW            
  =gfWinFn('MAXIMIZE')
  READ TIMEOUT 3
*  =gfWinFn('PRINTERS')
*  WAIT ""
*  READ CYCLE TIMEOUT 0

**  IF  READKEY()<>151
**    wait str(readkey()) window
**    EXIT
**  ELSE
**    ??CHR(7)
**    LOOP
**  ENDIF
**ENDDO   

*  IF readkey()=20
*    EXIT
*  ELSE
*    ??CHR(7)  
*  ENDIF
*ENDDO  

  RELEASE WINDOW O_Home
*    RETURN
ENDIF