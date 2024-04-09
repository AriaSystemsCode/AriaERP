*:************************************************************************
*:  Program File: add_scale_line_for_materials.prg.PRG
*:  Module      : IC
*:  Type        : Fix program
*:  Customer    : Maged
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 10/23/2013
*:  Reference   : R13 issues
************************************************************************************************
* this is a fix program to add a line to the SCALE table with type = 'S' and scale = '*'
CLEAR

SET EXCLUSIVE OFF
SET DELETED ON 

lcPath = GETDIR('','Aria4xp Sysfiles folder')
IF EMPTY(lcPath )
  lfMsg('Program cancelled')
  RETURN 
ENDIF 
IF !FILE(lcPath + 'SYCCOMP.DBF')
  lfMsg('The selected path is not valid SYSFILES folder')
  RETURN 
ENDIF
CLOSE ALL 

USE (lcPath + 'SYCCOMP.DBF')
SCAN 
  lcPath = ADDBS(ALLTRIM(SYCCOMP.CCOM_DDIR))
  SELECT 0
  USE (lcPath+'scale') ORDER scale
  IF !SEEK('S*  ')
    APPEND BLANK
    REPLACE TYPE      WITH 'S' ;
            SCALE     WITH '*' ;
            CSCL_DESC WITH 'Nosize    ' ;
            CNT       WITH 1 ;
            SZ1       WITH 'Size1'
  ENDIF 
  USE IN SCALE
ENDSCAN 


************************************************************
*! Name      : lfMsg
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/13/2013
*! Purpose   : messagebox function
************************************************************
FUNCTION lfMsg
PARAMETERS lcmsg
MESSAGEBOX(lcmsg,0,'Aria')
*- End of lfMsg.
