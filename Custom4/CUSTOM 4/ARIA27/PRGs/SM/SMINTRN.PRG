*:************************************************************************
*:
*: Procedure file: SMINTRN.PRG 
*:                 (Internationals)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300266,7 Reham On 08/23/95
*E300266,7 Add new system field "lEuropCom" in the "SYCINT.DBF".
*E300266,7 Add a record in the screen objects file for this field.
*E300266,7 Add a check box in the SMINTRN.SCX to validate the European
*E300266,7 community field.
*E300294,1 Hesham El-Sheltawi 30/09/1995
*E300294,1 Adding currency code for the countory to be use in 
*E300294,1 the initialization of the company information.
*B600801,1 M.H 01/14/96 Fix the bug of order not found.
*B601012,1 REN 03/31/96 Fixed : the follwing bug :
*B601012,1              Browse (through browsing button), 
*B601012,1              Select
*B601012,1              Close --> We still get a browse and we cannot
*B601012,1              exit the field until we select an ID.
*B800919,1 M.H 01/15/97 Fix the order of the address.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B602463,1 Sameh 03/09/99 Change currency display and storage to 3  
*B602463,1                characters in all files/screens of the system.
*:************************************************************************
EXTERNAL ARRAY laData,laKeyField,laDefProc
DECLARE laKeyField [1,4],laAdr[6,3],laYPos[3],laOrder[6,2]
 
laKeyField[1,1] = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CCONTCODE'
laKeyField[1,4] = 1

lcOrder      = " "
lcLabelOrd   = " "
ibOrder      = 1
llBrowse     = .F.
*E300294,1
lcOldCurr    = ''

IF !gfSetup()    
  RETURN
ENDIF  

laDefProc[7] = .F.
laDefProc[9] = .F.

IF !WEXIST(lcBaseWind)
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  
  SELECT SYDFIELD
  FOR lnCount = 1 TO 6
    laAdr[lnCount,1] = lnCount
    laAdr[lnCount,2] = "CADDRESS"+ALLTRIM(STR(lnCount))
    IF SEEK(laAdr[lnCount,2])
      laAdr[lnCount,3] = nfld_wdth
    ELSE
      laAdr[lnCount,3] = 1
    ENDIF
  ENDFOR
  laData[3]  = "Part 1"
  laData[4]  = "Part 2"
  laData[5]  = "Part 3"
  laData[6]  = "Part 4"
  laData[7]  = "Part 5"
  laData[8]  = "Part 6"
*--HESHAM EL-SHELTAWI  
  *laData[9]  = 3
  *laData[10] = 4
  *laData[11] = 5
  *laData[12] = laAdr[3,3]
  *laData[13] = laAdr[4,3]
  *laData[14] = laAdr[5,3]
*--HESHAM EL-SHELTAWI
  *** Put the order values in the array. ***
  *=lfFillOrd()
  
  lcOrder      = laOrder[1,1]
  lcLabelOrd   = laOrder[1,2]
  ibOrder      = 1
ENDIF

SELECT SYCINT

*E300683,4 Call *.SPR from screens directory
* DO Smintrn.SPR 
DO (gcScrDir + gcWinAppl + '\Smintrn.SPR')
*E300683,4 end    
*!**************************************************************************
*!
*!      Procedure: lpShow
*!
*!**************************************************************************
*
PROCEDURE lpShow

DO CASE
  CASE laScrMode[1]
    laData[3]  = "Part 1"
    laData[4]  = "Part 2"
    laData[5]  = "Part 3"
    laData[6]  = "Part 4"
    laData[7]  = "Part 5"
    laData[8]  = "Part 6"
*--HESHAM EL-SHELTAWI    
    *laData[9]  = 3
    *laData[10] = 4
    *laData[11] = 5
    *laData[12] = laAdr[3,3]
    *laData[13] = laAdr[4,3]
    *laData[14] = laAdr[5,3]
    
    ** Put defaults in the popup hold the parts objects. **
*--HESHAM EL-SHELTAWI    
    *=lfFillOrd()
    lcOrder    = laOrder[1,1]
    lcLabelOrd = laOrder[1,2]
  CASE laScrMode[2]
*--HESHAM EL-SHELTAWI  
    *=lfFillOrd()
*    lcLabelOrd = ALLTRIM(STR(laData[9]))+;
                 ALLTRIM(STR(laData[10]))+;
                 ALLTRIM(STR(laData[11]))
    
    lnOrder    = IIF(ASCAN(laOrder,lcLabelOrd) > 0,;
                     IIF(ASUBSCRIPT(laOrder,ASCAN(laOrder,lcLabelOrd),1) > 0,;
                     ASUBSCRIPT(laOrder,ASCAN(laOrder,lcLabelOrd),1),1),1)
    DO CASE
      CASE _DOS
        lcOrder = laOrder[lnOrder,1]
      CASE _WINDOWS
        ibOrder = lnOrder
    ENDCASE
  CASE laScrMode[4]
    ** Put defaults in the popup hold the parts objects. **
    *=lfFillOrd()
*--HESHAM EL-SHELTAWI    
    laData[9]   = 30
    laData[10]  = 30     
    laData[11]  = 15   
    laData[12]  = 3   
    laData[13]  = 10   
    laData[14]  = 20   
    
    laData[15]  = 1   
    laData[16]  = 2   
    laData[17]  = 3   
    laData[18]  = 3   
    laData[19]  = 3   
    laData[20]  = 4
        
    lcOrder    = laOrder[1,1]
    lcLabelOrd = laOrder[1,2]
    
    ** Put default for locals screen.
	*B602463,1 Changing the default 'USDLR' from 5 to 3 Char. so it will 
	*B602463,1 be 'USD' instead
    *laData[28]='USDLR'
    laData[28]='USD'
    *B602463,1 end
    ladata[22]=LOOKUP(SYCCURR.CCURRSMBL,laData[28],syccurr.ccurrcode,'Ccurrcode')     
    laData[21]=IIF(!EMPTY(laData[21]),ALLTRIM(laData[21]),SET("CURRENCY"))
    laData[22]=IIF(!EMPTY(laData[22]),ALLTRIM(laData[22]),SET("CURRENCY",2))
    laData[23]=IIF(!EMPTY(laData[23]),ALLTRIM(laData[23]),SET("DATE"))
    laData[24]=IIF(!EMPTY(laData[24]),ALLTRIM(laData[24]),SET("CENTURY"))
    laData[25]=IIF(!EMPTY(laData[25]),ALLTRIM(laData[25]),SET("SEPARATOR"))
    laData[26]=IIF(!EMPTY(laData[26]),ALLTRIM(laData[26]),gcTelTemp)
ENDCASE

DO CASE
  CASE _DOS
    =lfRefresh()
  CASE _WINDOWS
    SHOW GET ibOrder
ENDCASE
 
*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

IF llBrowse .OR. (!EMPTY(laData[1]) AND LASTKEY()= 13)
  =gfSeekRec()
ENDIF

*B601012,1 Correct the spelling of llBrowse variable
*llBroswe = .F.
llBrowse = .F.
*B601012,1 end.

*!********************************************************************
*!
*!              Function: lfvLabel
*!
*!********************************************************************
*
FUNCTION lfvLabel

IF ASCAN(laOrder,lcOrder) > 0
  lnOrder = IIF(ASUBSCRIPT(laOrder,ASCAN(laOrder,lcOrder),1) > 0,;
                ASUBSCRIPT(laOrder,ASCAN(laOrder,lcOrder),1),1)
ELSE
  lnOrder = 1
ENDIF

*=lfFillOrd()

DO CASE
  CASE _DOS
    lcOrder = laOrder[lnOrder,1]
    =lfRefresh()
  CASE _WINDOWS
    ibOrder = lnOrder
    SHOW GET ibOrder
ENDCASE

*!********************************************************************
*!
*!              Function: lfvOrder
*!
*!********************************************************************
*
FUNCTION lfvOrder

*=lfFillOrd()

DO CASE
  CASE _DOS
    lcLabelOrd = gfActPop(9,35,14,73,'laOrder',2,1,@lcOrder,gcBaseWind)
    =lfRefresh()
  CASE _WINDOWS
    lcLabelOrd = laOrder[ibOrder,2]
    SHOW GET ibOrder
ENDCASE

*laData[9]  = VAL(SUBSTR(lcLabelOrd,1,1))
*laData[10] = VAL(SUBSTR(lcLabelOrd,2,1))
*laData[11] = VAL(SUBSTR(lcLabelOrd,3,1))

*!********************************************************************
*!
*!              Function: lfvLocals
*!
*!********************************************************************
*
FUNCTION lfvLocals

*E300683,4 Call programs from PRGS directory
*DO SMLOCALS.PRG WITH "U"
DO (gcAppHome + gcWinAppl + '\SMLOCALS ') WITH "U"
*E300683,4 end

*!**************************************************************************
*!
*!      Function: lpSavScr
*!
*!**************************************************************************
*
FUNCTION lpSavScr

IF EMPTY(laData[2])
  *** You cannot leave the country description empty. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00196B00000","DIALOG")
  llCSave = .F.
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

SELECT sycInt
IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO
=gfAdd_Info()

IF laScrMode[3]
  *E301077,78 Hesham (Start)
  =gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID')
  *SELECT sycComp
  *E301077,78 Hesham (End)  
  IF SEEK(gcAct_Comp)
    IF SYCCOMP.CCONT_CODE = laData[1]
      IF !EMPTY(laData[24]) AND !EMPTY(laData[21]) AND !EMPTY(laData[23])
        SET CENTURY  &laData[24]
        SET CURRENCY &laData[21]
        SET DATE TO ALLTRIM(laData[23])
      ELSE
        SET CENTURY OFF
        SET CURRENCY LEFT
        SET CURRENCY TO   
        SET DATE TO AMERICAN
      ENDIF  
      SET CURRENCY TO IIF(!EMPTY(laData[22]),laData[22],"")
      SET SEPARATOR TO IIF(!EMPTY(laData[25]),laData[25],',')
      gcTelTemp  = IIF(!EMPTY(laData[26]),laData[26],gcTelTemp)
      gnPhonSize = LEN(ALLTRIM(laData[26]))
    ENDIF
  ENDIF
ENDIF

SELECT sycInt

*!********************************************************************
*!
*!              Procedure: lpDelScr
*!
*!********************************************************************
*
PROCEDURE lpDelScr
EXTERNAL ARRAY laScrMode
*E301077,78 Hesham (Start)
=gfOpenFile(gcSysHome+'SYCCOMP','CCOMP_ID')
*IF !USED('SYCCOMP')
*  USE (gcSysHome+'SYCCOMP') IN (SELECT(0))
*ENDIF
*SELECT SYCCOMP
*E301077,78 Hesham (End)  

LOCATE FOR ccont_code=laData[1]
IF FOUND() OR laData[1] = 'USA' OR laData[1] = 'ENG'
  ** 'You cannot delete this record' message
   =gfModalGen('INM00242B00000','ALERT',IIF(laData[1]='USA' OR laData[1] = 'ENG','This record has been defined by the system.',;
   'This Code is currently used by one or more company(s).'))
    SELECT sycInt
ELSE
  lcSavAlias=SELECT(0)
  *** Delete the international record from the sycInt file
  SELECT sycInt
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
  GATHER FROM laData FIELDS &lcScFields MEMO 
  DELETE
  SELECT(lcSavAlias)
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF  

*B600801,1 M.H 01/14/96 Fix the bug of order not found.
SELECT SYCINT
*B600801,1 End.

*!********************************************************************
*!
*!              Function: lfFillOrd
*!
*!********************************************************************
*
FUNCTION lfFillOrd

laOrder[1,1] = IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))+","+;
               IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))+","+;
               IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))
laOrder[1,2] = "345"

laOrder[2,1] = IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))+","+;
               IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))+","+;
               IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))
laOrder[2,2] = "354"

laOrder[3,1] = IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))+","+;
               IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))+","+;
               IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))
laOrder[3,2] = "435"

laOrder[4,1] = IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))+","+;
               IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))+","+;
               IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))
*B800919,1 M.H Begin.
*laOrder[4,2] = "453"
laOrder[4,2] = "534"
*B800919,1 M.H End.

laOrder[5,1] = IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))+","+;
               IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))+","+;
               IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))
laOrder[5,2] = "543"

laOrder[6,1] = IIF(EMPTY(laData[7]),"Part 5",ALLTRIM(laData[7]))+","+;
               IIF(EMPTY(laData[5]),"Part 3",ALLTRIM(laData[5]))+","+;
               IIF(EMPTY(laData[6]),"Part 4",ALLTRIM(laData[6]))
*B800919,1 M.H Begin.
*laOrder[6,2] = "534"
laOrder[6,2] = "453"
*B800919,1 M.H End.

*!**************************************************************************
*!
*!      Function: lfDispLine
*!
*!**************************************************************************
*
*--HESHAM EL-SHELTAWI
FUNCTION lfDispLine
PARAMETERS lnLineNo
lcRetVal=''
IF !laScrMode[1]
  FOR lnCount = 15 TO 20
    IF laData[lnCount] = lnLineNo
      IF !EMPTY(laData[lnCount-12])
        lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR(laData[lnCount-12],1,laData[lnCount-6]),laData[lnCount-6],"*")
      ELSE  
        lcRetVal=lcRetVal+IIF(EMPTY(lcRetVal),'',',')+PADR(SUBSTR('Part'+STR(lnCount-14,1),1,laData[lnCount-6]),laData[lnCount-6],"*")      
      ENDIF  
    ENDIF
  ENDFOR
ENDIF  
RETURN lcRetVal

*E300294,1  start functions

*!**************************************************************************
*!
*!      Function: lfwCurrency
*!
*!**************************************************************************
*
FUNCTION lfwCurrency
lcOldCurr=laData[28]

*!**************************************************************************
*!
*!      Function: lfvCurrency
*!
*!**************************************************************************
*
FUNCTION lfvCurrency
IF (!EMPTY(laData[28]) AND !SEEK(laData[28],'SYCCURR')) OR llBrowse 
  PRIVATE lcBrFields,lcFile_ttl,lcSelect
  lcSelect = SELECT()
  SELECT SYCCURR
  lcBrFields=gfDbfField('SYCCURR')
  DIMENSION laData1[1]
  STORE '' TO laData1
  lcFile_ttl    = "Currency"
  =gfBrows(.F.,"cCurrCode","laData1")
  laData[28]=laData1[1]
  SELECT (lcSelect)
  SHOW GET laData[28]
ENDIF
IF EMPTY(laData[28])
  laData[28] = lcOldCurr
ENDIF
IF laData[28] <> lcOldCurr
  ladata[22]=LOOKUP(SYCCURR.CCURRSMBL,laData[28],syccurr.ccurrcode,'Ccurrcode') 
ENDIF    
llBrowse = .F.
=lfRefresh(WOUTPUT())

