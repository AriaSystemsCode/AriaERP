*:***************************************************************************
*: Program file  : SOMACN
*: Program desc. : Material Order Confirmation
*: For Report    : SOMACNA.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes,gfADel , gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep,lfsChOrder
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : All IF llFrTime Blocks executed one time in the option grid seasson.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time. 
*:***************************************************************************
*: Example : DO SOMACN
*:***************************************************************************
*: This Program is due to E126322 ...
*:***************************************************************************
*: Modifications:
*:E126324,1 MHM 03/27/2005 handle printing from material sala order screen
*:***************************************************************************

*E126324 MHM to handle printing from material sala order screen[Start]

IF TYPE('lcNewMPos') = 'C' .AND. USED(lcNewMPos) 
  IF  RECCOUNT(lcNewMPos) = 1 .AND. !EMPTY(&lcNewMPos..CMORDER)
    lcRpExp  = lcRpExp + [ AND SEEK(CMORDER,lcNewMPos)]
  ENDIF
ENDIF
**E126324 [END]

lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llPrintBox = .F.                        && Box around Notes.

llPrtCmpdt = gfGetMemVar('M_CMPDOLN')  && Checking Comp. Date by Order line. used in the complete date in the frx form A.

llSalsOrd = .T.          && llarpinv it will be a commen variable.

*-- Note Variables [end]
IF !llFrTime
  lcFormName = lfGetForm()
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
ENDIF

llNoRec= .T.
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)


llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = llRpOrdLnt AND llRpOrdNot  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval 
llTitle    = .T.                        && Flag to print Detail header.

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF

lcTempOrd = gfTempName()
*--E301168 HDM create temp file for ordline file [End]
*--- Relation between opened files [begin]
*-- Note that the files was opened in Rep. Gen.
SELECT MASOHDR
SET RELATION TO cordtype+cmorder INTO MASOLIN ADDITIVE

*--E301168 HDM create temp file for ordline file [Start]
DIMENSION laFileStru[1]
SELECT MASOLIN
= AFIELDS(laFilStruc)

CREATE TABLE (gcWorkDir+lcTempOrd) FROM ARRAY laFilStruc

SELECT (lcTempOrd)
IF lcRpSortBy = 'F'
  INDEX ON CORDTYPE + CMORDER + STORE + FABRIC + COLOR TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
ELSE
  INDEX ON CORDTYPE + CMORDER + STORE + STR(LINENO,6) TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
ENDIF

*--check Status
lcRpExp = lcRpExp + " AND (IIF(lcRpOrdSta = 'A' , !(MASOHDR.STATUS $ 'XC') , MASOHDR.STATUS = lcRpOrdSta ))"
lcRpExp = lcRpExp + " AND (IIF(lcRpOrdTyp = 'A' , .T. , MASOHDR.CORDTYPE = lcRpOrdTyp ))"

SELECT MASOHDR
SCAN FOR &lcRpExp
  SELE MASOLIN
  SCAN REST WHILE CORDTYPE+cMORDER = MASOHDR.CORDTYPE+MASOHDR.CMORDER
    SCATT MEMVAR MEMO
    INSERT INTO (lcTempOrd) FROM MEMVAR
  ENDSCAN
ENDSCAN

*--E301168 HDM create temp file for ordline file [End]
GO TOP
SELECT MASOHDR
SET RELATION OFF INTO MASOLIN

SELECT MASOHDR

SET RELATION TO cordtype+ cMorder INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcTempOrd)
SET FILTER TO FABQty != 0
SET RELATION TO 'M'+FABRIC INTO OBJLINK_A ADDITIVE
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE

SELECT OBJLINK_A
SET RELATION TO cobject_id INTO OBJECTS_A ADDITIVE
*--- Relation between opened files [end]

IF llFrTime
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laCompAdd = ''                    && Array to hold the Company address
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  *----E500320,1 NAD [Begin] Get the company fax no. to print it 
  lcCompFax  = cCom_fax              && Variable to hold the Company Fax
  *----E500320,1 NAD [END]
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Load Company address.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
ENDIF

lcSkipExpr  = [&lcTempOrd]

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created 
*--           : one for every optional grid seasson run.
lcNoteLns = IIF(EMPTY(lcNoteLns),gfTempName(),lcNoteLns)

*-- if you don't find temp. file, create it if you have both types of notes.
IF !USED(lcNoteLns) AND llPrntBoth
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

IF llPrntBoth
  SELECT (lcTempOrd)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE

  lcSkipExpr = [&lcTempOrd,&lcNoteLns]
ENDIF

*-- if we are in case of print both types of notes.  [begin]

*-- Select Master report file.
SELECT MASOHDR
SET SKIP TO &lcSkipExpr

lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcTempOrd) = lnLastRec ,.T.),.T.)]

lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)

=lfOptProg()

IF llIsAparel
  =gfSetDevice()
  DO EVAL('lcPrgName')
  IF !llNoRec
    DO ENDREPORT
  ENDIF
ELSE
  *--
  lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
  *--
  IF llSalsOrd
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ENDIF

ENDIF  

SELECT MASOHDR

SET RELATION TO


IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (gcWorkDir+lcTempOrd+'.DBF')
  ERASE (gcWorkDir+lcTempOrd+'.CDX')
ENDIF
*-- end of Report code.
*-- Function section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcLogoPic = gfTempName()
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*-- end of lfGetLogo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/20/2005
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : SORDCONA.FRX [Header Band]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSolSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr

lnSavAlias = SELECT(0)

lcStore = &lcTempOrd..Store

= gfRltFld(MASOHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(MASOHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(MASOHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(MASOHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(MASOHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF MASOHDR.MULTI = 'Y'
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")
  IF ALLTRIM(MASOHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  

ENDIF

lcSolTName = BTName
lcShpTName = IIF(MASOHDR.Alt_ShpTo , MASOHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF MASOHDR.Alt_ShpTo
  SELECT MASOHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else

  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)

*-- end of lfSolSpAdr.


*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/20/2005
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SOMACNA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes    = lfNoteData()     && Note Data.

IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
ELSE
  lcTitle    =""
ENDIF

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name        : lfNoteHead
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/20/2005
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteHead()
*!*************************************************************
FUNCTION lfNoteHead
lcNoteHead = ''
*-- If you have order lines.
IF MASOHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE 
       CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'
        
      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('V'+CMOrder,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(MASOHdr.cOrdType='O','Order Notepad',IIF(MASOHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')

    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'

      CASE llRpOrdNot AND SEEK('V'+CMOrder,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))


        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(MASOHdr.cOrdType='O','Order Notepad',IIF(MASOHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- end of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 03/20/2005
*! Purpose     : Function to fill the approparate Note Data Field in report.
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteData()
*!*************************************************************
FUNCTION lfNoteData
lcNoteData  = ''
lcPrntNote = ''

*-- If you have order lines.
IF MASOHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote

      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('V'+CMOrder,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE

      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote

      CASE llRpOrdNot AND SEEK('V'+CMOrder,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
        *B802642,1  ABD [ end ]
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- end of lfNoteData.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function to Update the End of Group flag 
*!*************************************************************
*! Called from : SORDCONA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.
RETURN '    '
*-- end of lfEndGroup.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

PRIVATE lnPos1

*IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  llRpOrdLnt = .F.
  lnPos1 = ASCAN(laOgObjType,'llRpOrdLnt')
  IF lnPos1 > 0
    lnPos1= ASUBSCRIPT(laOgObjType,lnPos1,1)
    laOGObjCnt[lnPos1] = .F.
    =lfOGShowGet('llRpOrdLnt')  && Disable the Print order line notepad Object
  ENDIF
ENDIF    && End of IF


SET ORDER TO MASOHdR IN MASOHdR  && To use it to validate orders in option grid.
*-- end of lfwRepWhen.


*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder


PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcRpOrdTyp+lcObj , 'MASOHDR'))

  SELECT MASOHdR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "cmOrder   :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "FABOPEN   :R :H= 'Open Amt. '"  

  lcFile_Ttl = "Orders..."

  lcBrowCond = [FOR (IIF(lcRpOrdSta = 'A' , !(MASOHDR.STATUS $ "XC") , MASOHDR.STATUS = lcRpOrdSta ))]
  
  = gfBrows(lcBrowCond,'CMORDER','laTemp')  
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvOrder.

*!*************************************************************
*! Name      : lfAprvFlag
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function that control appearance of 
*!           : Approval objects in SORDCONA.FRX
*!*************************************************************
*! Called from : [Option Grid] "Approval Object" in Header Band.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfAprvFlag()
*!*************************************************************
FUNCTION lfAprvFlag
llAprvFlag = !EMPTY(MASOHDR.Approval)
RETURN llAprvFlag
*-- end of lfAprvFlag.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Called from : [SORDCONA.FRX, ORDER GROUP HEADER BAND] 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.

SELECT (lcTempOrd)

lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = Store
LOCATE REST FOR ( cordtype+cmorder+store+Fabric+Color+STR(lineno,6) > MASOHDR.cordtype + MASOHDR.CMorder + lcThStore)
IF (Cmorder != MASOHDR.CMorder) OR (store != lcThStore)
  SKIP -1
ENDIF   

lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF


SELECT (lcThAlias)            && Restore Alias.
RETURN ''
*-- end of lfLastRec.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep
*-- Close temp. opended files, if it used.
IF USED(lcLogoPic)
 USE IN (lcLogoPic)
ENDIF

IF USED(lcNoteLns)
 USE IN (lcNoteLns)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfvType
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Transaction Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
*
FUNCTION lfvType

IF lcRpOrdTyp $ "TC"

  lcRpEDIFlt = "B"
  CLEAR READ 
ELSE

  IF (laOldVal = 3) .OR. (laOldVal = 2)

    lcRpEDIFlt = "B"
    CLEAR READ 
  ENDIF  
ENDIF
llClearSel = .T.

*-- end of lfvType.
*!*************************************************************
*! Name      : lfModeVld
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Report Mode Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfModeVld()
*!*************************************************************

FUNCTION lfModeVld
CLEAR READ
*-- end of lfModeVld.

*!*************************************************************
*! Name      : lfvOrdSta
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Order Status Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvOrdSta()
*!*************************************************************

FUNCTION lfvOrdSta

llClearSel = .T.

*-- end of lfvOrdSta.

*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************

FUNCTION lfSortDumy

DIMENSION laSortDesc[2,1] , laSortVal[2,1]
laSortDesc[1] = 'Fabric'
laSortDesc[2] = 'Line Number'

laSortVal[1] = 'F'
laSortVal[2] = 'L'

*--The End

*!*************************************************************
*! Name      : lfvPrntDtl
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Change the sort by option to Pack when Print 
*!             Pack Detail is "No".
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrntDtl()
*!*************************************************************
*! C200343,1

FUNCTION lfvPrntDtl

laSortDesc[1] = 'Line Number'
laSortDesc[2] = 'Fabric'

laSortVal[1] = 'L'
laSortVal[2] = 'F'

*!*************************************************************
*! Name      : lfSROrder
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/20/2005
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSROrder()
*!*************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm
llChOrder = .T.
*-- Set Relation with customer file.
DO CASE
  CASE lcParm = 'S'
    SELECT MASOHDR
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET ORDER TO Customer IN Customer
    SET RELATION TO &lcCustRel INTO CUSTOMER && To customer file.

    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
         SET FILTER TO IIF(lcRpOrdSta= 'A', !(MASOHDR.STATUS $ "XC"), MASOHDR.STATUS = lcRpOrdSta) 

         LOCATE
         
      CASE lcRpOrdTyp = "O" 
         SET FILTER TO (CORDTYPE + CMORDER = "O") AND IIF(lcRpOrdSta= 'A', !(MASOHDR.STATUS $ "XC")  ,;
            MASOHDR.STATUS = lcRpOrdSta) 
         
            LOCATE FOR CORDTYPE+CMORDER = "O"

      CASE lcRpOrdTyp = "C"
         SET FILTER TO (CORDTYPE + CMORDER = "C") AND IIF(lcRpOrdSta= 'A', !(MASOHDR.STATUS $ "XC")  ,;
            MASOHDR.STATUS = lcRpOrdSta) 
         
         LOCATE FOR CORDTYPE+CMORDER = "C"

    ENDCASE                  
  
  CASE lcParm = 'R'
    SELECT MASOHDR
    SET RELATION OFF INTO CUSTOMER && To customer file.
    llClearOrd = .F.
ENDCASE
*-- end of lfsChOrder.

