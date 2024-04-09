*:***************************************************************************
*: Program file  : Soorcnfa
*: Program desc. : Order Confirmation
*: For Report    : SOORCNfa.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Abdel Naby (AAN)
*: Date 		 : 06/12/2000
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
*: Notes   : All IF llFrTime Blocks executed one time in the option grid session.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time. 
*:***************************************************************************
*: Example : DO Soorcnfa
*:***************************************************************************
*: This Program is due to C# 101967 ...
*: Modification:
*: B606986,1 ABD 03/02/2003 Fix problem that when privew the 
*: B606986,1 ABD            report the progrem clear the selaction.
*:***************************************************************************

*B606986,1 ABD Stop calling this function , no need to call it, this
*B606986,1 ABD function clear the selection for every time run the report. [Begin]
* =lfvType() WHEN lfwOldVal()
*B606986,1 ABD [End]

lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llPrintBox = .F.                        && Box around Notes.
*-- Note Variables [end]
lcRpName = "SOORCNFA"
llNoRec= .T.
llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = llRpOrdLnt AND llRpOrdNot  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval 
llTitle    = .T.                        && Flag to print Detail header.

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = gfItemMask('HI')        && Title of the style.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  lcObj_Id  = ''                       && Object Identification in Objlink file.
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF
lcTempOrd = gfTempName()
*--- Relation between opened files [begin]
*-- Note that the files was opened in Rep. Gen.
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO Ordline ADDITIVE

DIMENSION laFileStru[1]
SELECT ORDLINE
= AFIELDS(laFilStruc)

CREATE TABLE (gcWorkDir+lcTempOrd) FROM ARRAY laFilStruc
IF lcRpOrdTyp = "T"
  lcTempRcp = gfTempName()   && Temp file to collect data for recap
  lcTempRpt = gfTempName()   && Temp file to collect DOS resords for recap report

  CREATE TABLE (gcWorkDir+lcTempRcp) (Account C(5),Style C(19),Price N(12,2),Qty1 N(7),Qty2 N(7),;
  Qty3 N(7),Qty4 N(7),Qty5 N(7),Qty6 N(7),Qty7 N(7),Qty8 N(7),TotQty N(8), Amount N(13,2),;
  nSRP N(8,2),Scale C(3),note1 c(30),note2 c(20))
  INDEX ON Account+Style TAG (lcTempRcp)
ENDIF

SELECT (lcTempOrd)
IF lcRpSortBy = 'S'
  INDEX ON CORDTYPE + ORDER + STORE + STYLE TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
ELSE
  INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
ENDIF
SELECT ORDHDR

lcEdiExpr = IIF(lcRpEDIFlt='O',[!ORDHDR.lEDIOrder],IIF(lcRpEDIFlt='E',[ORDHDR.lEDIOrder],""))
IF !EMPTY(lcEdiExpr)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcEdiExpr
ENDIF

SCAN FOR &lcRpExp
  SELECT ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SCATT MEMVAR MEMO
    INSERT INTO (lcTempOrd) FROM MEMVAR
    IF lcRpOrdTyp = "T"
      SELECT (lcTempRcp)
      IF !SEEK(m.Account+m.Style)
        INSERT INTO (lcTempRcp) (Account,Style,Price,nSRP,Scale) VALUES ;
                                (m.Account,m.Style,m.Price,m.nSugRetPri,m.Scale)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
              Qty2   WITH Qty2   + m.Qty2 ,;
              Qty3   WITH Qty3   + m.Qty3 ,;
              Qty4   WITH Qty4   + m.Qty4 ,;
              Qty5   WITH Qty5   + m.Qty5 ,;
              Qty6   WITH Qty6   + m.Qty6 ,;
              Qty7   WITH Qty7   + m.Qty7 ,;
              Qty8   WITH Qty8   + m.Qty8 ,;
              TotQty WITH TotQty + m.TotQty ,;
              Amount WITH Amount + m.TotQty*m.Price
    ENDIF
  ENDSCAN
ENDSCAN

GO TOP
SELECT ORDHDR
SET RELATION OFF INTO ORDLINE

SELECT ORDHDR
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT (lcTempOrd)
SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

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
  lcCompFax  = cCom_fax              && Variable to hold the Company Fax
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
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcTempOrd) = lnLastRec ,.T.),.T.)]

lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)
IF lcRpOrdTyp <> "T" OR !llRcpOnly 
  lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
  DO gfDispRe WITH EVAL('lcRpName') , 'FOR ' + lcRpExp
ENDIF

SELECT ORDHDR

SET RELATION TO

IF lcRpOrdTyp = "T"
  lcRecName = "SOORCNRF"
  lcPrgName  = lcRecName
  SELECT (lctemprcp)
  SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  SET RELATION TO Style INTO Style ADDITIVE
  SET RELATION TO 'M'+Account INTO Customer ADDITIVE
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcRecName,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
  SELECT (lctemprcp)
  GO TOP
  DO gfDispRe WITH EVAL('lcRecName')
ENDIF

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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
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
*! Developer   : Ahmed Abdel Naby (AAN)
*! Date        : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
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

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
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
*! Developer   : Ahmed Abdel Naby (AAN)
*! Date        : 06/12/2000
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
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
*! Developer   : Ahmed Abdel Naby (AAN)
*! Date        : 06/12/2000
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
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
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE 
       CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'
        
      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
           
        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')


    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'

      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- end of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Ahmed Abdel Naby (AAN)
*! Date        : 06/12/2000
*! Purpose     : Function to fill the approparate Note Data Field in report.
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
IF ORDHDR.LastLine > 0
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

      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
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

      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- end of lfNoteData.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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

SET ORDER TO ORDHDR IN ORDHDR  && To use it to validate orders in option grid.
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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

*IF Statment to check if we are going to Browse

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpOrdTyp = 'A'  ,;
  SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') ,;
  SEEK(lcRpOrdTyp+lcObj , 'ORDHDR')))
  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = IIF(lcRpOrdTyp = 'A',"Orders/Contracts",IIF(lcRpOrdTyp='O',"Orders...",;
                   IIF(lcRpOrdTyp='T',"EDI Temporary Orders","Contracts...")))

  lcBrowCond = [FOR (IIF(lcRpOrdSta = 'A' , !(ORDHDR.STATUS $ "XC") , ;
               ORDHDR.STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
               ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >= 1) AND ] + ;
               [ IIF(lcRpOrdTyp = "A",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
               [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E' , ;
               ORDHDR.LEDIORDER,.T.)))]
  
  = gfBrows(lcBrowCond,'ORDER','laTemp')  
  *IF The user selected a record
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
llAprvFlag = !EMPTY(OrdHdr.Approval)
RETURN llAprvFlag
*-- end of lfAprvFlag.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Calculate last Record in order details.
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
LOCATE REST FOR ( cordtype+order+store+style+STR(lineno,6) > OrdHdr.cordtype + OrdHdr.order + lcThStore)
IF (order != OrdHdr.order) OR (store != lcThStore)
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Name      : lfsChOrder
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Example   : =lfsChOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsChOrder
PARAMETERS lcParm
DO CASE 
  CASE lcParm = 'S'
    SELECT ORDHDR
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
         SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
           IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))

         LOCATE
         
      CASE lcRpOrdTyp = "O" 
         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         
            LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
         SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         
         LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T" 
         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
            ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         LOCATE FOR CORDTYPE+ORDER = "T"

    ENDCASE                  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
    llClearSel = .F.
ENDCASE
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfvType
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
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
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Order Status Validation
*! Job No.   : E301343,1 
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
*! Name      : lfGetSt
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 06/12/2000
*! Purpose   : Get EDI Status
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSt()
*!*************************************************************
FUNCTION lfGetSt

PRIVATE lcRetStat
lcRetStat = SPACE(0)
IF EVAL(lcTempOrd +'.cOrdType')#'T' .OR. ORDHDR.MON_FLG # 'G'
  RETURN ''
ENDIF

DO CASE
  CASE EVAL(lcTempOrd +'.CLINESTAT')='AI'
    lcRetStat = 'New Item'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='DI'
    lcRetStat = 'Deleted Item'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='QD'  
    lcRetStat = 'Quantity Decreased'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='QI'
    lcRetStat = 'Quantity Increased'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='PC'  
    lcRetStat = 'Price Changed'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='PQ'  
    lcRetStat = 'Quantity Changed'
ENDCASE
RETURN lcRetStat
