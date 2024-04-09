*:************************************************************************
*:
*: Procedure file: GLSELCD.PRG
*:
*:         System: ARIA BUSINESS SYSTEMS
*:         Module: General Ledger
*:         Author: Mohamed Hassan Mohamed
*:      Copyright (c) 
*:  Last modified: 01/17/1994
*:
*:  Procs & Fncts: lfwLine
*:               : lfvSelect
*:               : lfvAll
*:               : lfvInvert
*:               : lfvNone
*:               
*:      Documented  /  /   at   :  
*:************************************************************************
*E300683,5 AHMED 06/10/97 Add prgs directory path to the calling of programs
*
PARAMETERS lcTempName,lcType,llFirstSel

lcCodes    = ' '  && Variable to hold the fields of the list
lcIndExp1  = ' '  && Variable to hold the index expression of the GLAUTHD
lcTagExp1  = ' '  && Variable to hold the tag expression of the GLAUTHD

** Variable to hold the title of the screen **
lcTitle    = IIF(lcType = 'A','Select Allocation Codes','Select Recurring Codes ')

lnRecNo    = 0  && Variable to hold the record no
lsCodes    = 1  && Variable to hold the list number

SELECT GLAUTHD

lcIndExp1= SYS(14,VAL(SYS(21)))
lcTagExp1= ' TAG '+SYS(22) + IIF('DESC' $ SET('ORDER'),' DESC','')

IF PARAMETERS() > 2 .OR. RECCOUNT(lcTempName) = 0
  SELECT *,' ' AS 'CINCLUDE',' ' AS 'CSELECT';
    FROM GLAUTHD;
   WHERE GLAUTHD.CAUTTYPE = lcType;
    INTO DBF &gcWorkDir.&lcTempName
ENDIF

SELECT (lcTempName)
INDEX ON &lcIndExp1.&lcTagExp1

IF !WEXIST('AWDGLSELCD')
  DO CASE
    CASE _DOS
      lcCodes ="&lcTempName..cAutCode+'     '+&lcTempName..cInclude+'    '+SUBSTR(&lcTempName..cAutDes,1,24)+' '+&lcTempName..cAutRef"
    CASE _WINDOWS
      lcCodes ="&lcTempName..cAutCode+SPACE(2)+&lcTempName..cInclude+SPACE(3)+SUBSTR(&lcTempName..cAutDes,1,24)+SPACE(1)+&lcTempName..cAutRef"
  ENDCASE  
ENDIF

lsCodes = 1

*E300683,5 Call *.SPR from screens directory
* DO GLSELCD.SPR 
DO (gcScrDir + gcWinAppl + '\GLSELCD.SPR')
*E300683,5 end   

*!**************************************************************************
*!
*!      Procedure: lfwCodes
*!
*!**************************************************************************
*
FUNCTION lfwCodes

IF EMPTY(&lcTempName..cInclude)
*  SHOW GET pbSelect,1 PROMPT "\<Select"
  SHOW GET pbSelect,1 PROMPT lcTSelect
ELSE
*  SHOW GET pbSelect,1 PROMPT "\<Unselect"
  SHOW GET pbSelect,1 PROMPT lcTUnSelct
ENDIF

*!**************************************************************************
*!
*!      Function: lfvSelect
*!
*!**************************************************************************
*
FUNCTION lfvSelect

IF EMPTY(&lcTempName..cInclude)
  REPLACE &lcTempName..cInclude WITH 'û'
ELSE
  REPLACE &lcTempName..cInclude WITH ' '
ENDIF

_CUROBJ = OBJNUM(lsCodes)
SHOW GET lsCodes
=lfwCodes()

*!**************************************************************************
*!
*!      Function: lfvAll
*!
*!**************************************************************************
*
FUNCTION lfvAll

SELECT (lcTempName)
GO TOP

** Select all the records with status = 'Ready' **
REPLACE ALL cInclude WITH 'û' 

_CUROBJ = OBJNUM(lsCodes)
SHOW GET lsCodes
=lfwCodes()

*!**************************************************************************
*!
*!      Function: lfvInvert
*!
*!**************************************************************************
*
FUNCTION lfvInvert

SELECT (lcTempName)
GO TOP

** Select all the records with status = 'Ready' & not selected befor 
** and unselect the records witch was selected And status = 'Ready'
REPLACE ALL cInclude WITH IIF(cInClude='û',' ','û') 

_CUROBJ = OBJNUM(lsCodes)
SHOW GET lsCodes
=lfwCodes()

*!**************************************************************************
*!
*!      Function: lfvNone
*!
*!**************************************************************************
*
FUNCTION lfvNone

SELECT (lcTempName)
GO TOP

** Unselect the records **
REPLACE ALL cInclude WITH ' '

_CUROBJ = OBJNUM(lsCodes)
SHOW GET lsCodes
=lfwCodes()

*!**************************************************************************
*!
*!      Procedure: lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

SELECT (lcTempName)

SCAN FOR cSelect <> cInClude 
  REPLACE &lcTempName..cSelect WITH cInClude
  llUpdate = .T.
ENDSCAN
