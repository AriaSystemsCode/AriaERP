*:***********************************************************************************************
*:  Program Name : ALPKTKEF.PRG
*:         System: ARIA 2.7
*:         Module: ALLOCATION (AL).
*:  Program desc.: Picking Ticket Form For EILEEN FISHER
*:         Author: Hend Ghanem (HBG)
*:      Copyright (c) 
*:  Last modified: 28/03/2001
*:***********************************************************************************************
*: For Custom# C102097
*:***********************************************************************************************
*:C102097,8 AAN 06/13/2001 Update the report.
*:B804412,1 AAN 09/19/2001 Add priority, merchandise total and payment type to the report.
*:B605054,1 BWA 10/25/2001 Fix the bug of printing the order line in case diffrent from allocation.[FIX IN FRX]
*:B605556,1 BWA 02/24/2002 Fix the bug of printing the name of the shipto and soldto not completely.[FIX IN FRX]
*:B605631,1 BWA 03/04/2002 Fix the bug of the printing is very slow.
*:***********************************************************************************************
DECLARE laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
*=lfSolSpAdr()
*=lfGetAddr()
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
lcShipVia = ''         && Variable to hold the Ship Via Description
ldShipDate = {}
lcNToRet = ''
*C102097,8 AAN Create new temp. file and collect the data from old temp.[begin].
= lfCrTmpFls()
= lfNewColet()
SELECT (lcOrdLTmp)
SET RELATION TO PikTkt + Order + cGrupDetal +STR(LineNo,6) INTO (lcTmpOrdL) ADDITIVE
*C102097,8 AAN Create new temp. file and collect the data from old temp.[end].
IF !llRpOrdLNt
  SELECT (lcTmpOrdL)
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF  
=lfGetScale()
SELECT (lcTmpOrdL)
GO TOP
SET RELATION TO PIKTKT INTO (lcScale) ADDITIVE
=gfOpenFile(gcDataDir+'INVHDR','Invhdra','SH')
*C102097,8 AAN Open the report with new temp.[begin].
SELECT (lcOrdLTmp)
GO TOP
*C102097,8 AAN Open the report with new temp.[end].

*!**************************************************************************
*! Name      : lfAdrShift
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : Shift the add. if there is any empty one
*!**************************************************************************
*! Example   : = lfAdrShift()
*!**************************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam.,1)
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- End of lfAdrShift.

*!**************************************************************************
*! Name      : lfShipDate
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : get the Ship Date
*!**************************************************************************
*! Example   : = lfShipDate()
*!**************************************************************************
*
FUNCTION lfShipDate
PARAMETER llDummy

lcCurrAlis = ALIAS()

SELECT INVHDR
IF SEEK(ORDHDR.Account)
  SCAN REST WHILE account+invoice = ORDHDR.Account
    IF ORDER = ORDHDR.Order
      ldShipDate = shipdate
      EXIT
    ELSE
      ldShipDate = {}   
    ENDIF
  ENDSCAN
ELSE
  ldShipDate = {}
ENDIF  

SELECT (lcCurrAlis)

RETURN ''
*-- End of lfShipDate

*!**************************************************************************
*! Name      : lfCOUTTOT
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : get the Total Qty
*!**************************************************************************
*! Example   : = lfShipDate()
*!**************************************************************************
*
FUNCTION lfCOUTTOT
PARAMETER llDummy
PRIVATE lnOldAls

lnOldAls = SELECT(0)
lcDummy = lcDummy + (TotPik - (Pik7+Pik8))
*:B804412,1 AAN Add payment type to the report [Begin].

*B605631,1 BWA 03/04/2002 Fix the bug of the printing is very slow.[START]
*lcTerm = gfCodDes(ORDHDR.ctermcode,'cTermCode')
*B605631,1 BWA 03/04/2002.[END]

*:B804412,1 AAN Add payment type to the report [End].
SELECT(lnOldAls)

*!**************************************************************************
*! Name      : lfRest
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : Rest the Total Qty
*!**************************************************************************
*! Example   : = lfShipDate()
*!**************************************************************************
*
FUNCTION lfRest
PARAMETER llDummy

lcDummy = 0
lcNToRet = ''

llEndGroup = .F.


*!**************************************************************************
*! Name      : lfGetScale
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : Get Scales
*!**************************************************************************
*! Example   : = lfGetScale()
*!**************************************************************************
*
FUNCTION lfGetScale
PRIVATE lcScale1 , lcScale2 , lcKey 

lnI = 1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'PIKTKT'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 6
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZF6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS1'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS2'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS3'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS4'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS5'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0


lnI = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnI,4]
laFileStru[lnI,1] = 'SZS6'
laFileStru[lnI,2] = 'C'
laFileStru[lnI,3] = 5
laFileStru[lnI,4] = 0

DIMENSION laIndx[1,2]
laIndx[1,1] = "PIKTKT"
laIndx[1,2] = lcScale

=gfCrtTmp(lcScale,@laFileStru,@laIndx)


SELECT (lcTmpOrdL)
LOCATE
lcPikTkt = ''
SCAN
  IF lcPikTkt <> PIKTKT
    lcPikTkt = PIKTKT
    SELECT (lcScale)
    APPEND BLANK
    SELECT (lcTmpOrdL)
    lcScale1 = SCALE
    lcScale2 = SPACE(1)
    lcKey = PIKTKT 
    LOCATE REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = lcKey ;
                FOR SCALE <> lcScale1
    IF PikTkt + Order + cGrupDetal + STR(LineNo , 6) = lcKey
      lcScale2 = SCALE
    *B804412,1 HBG 24/09/2001 Fix bug of not printing al scales [Begin]
    ELSE
      SKIP -1  
    *B804412,1 [End] 
    ENDIF
    IF SEEK('S'+lcScale1,'SCALE')
      *aan[begin]
      SELECT (lcScale)
      *aan[end]
      FOR lnI = 1 TO 6
        lcI = STR(lnI,1)
        *aan[Begin]
        REPLACE PikTkt  WITH lcPikTkt,;
                SzF&lcI WITH SCALE.SZ&lcI
        *aan[end]
      ENDFOR
    ENDIF
    IF lcScale1 <> lcScale2 AND SEEK('S'+lcScale2,'SCALE')
      FOR lnI = 1 TO 6
        lcI = STR(lnI,1)
        REPLACE SzS&lcI WITH SCALE.SZ&lcI
      ENDFOR
    ENDIF
  ENDIF  
ENDSCAN

*!**************************************************************************
*! Name      : lfRetNPad
*! Developer : Hend Ghanem (HBG)
*! Date      : 28/03/2001
*! Purpose   : Return Notepad
*!**************************************************************************
*! Example   : = lfRetNPad()
*!**************************************************************************
*
FUNCTION lfRetNPad
PARAMETER lcDummy
*IF &lcTmpOrdL..cGrupDetal='H' AND llRpOrdNot 
IF &lcOrdLTmp..cGrupDetal='H' AND llRpOrdNot 
  =SEEK('B'+&lcOrdLTmp..Order ,'NOTEPAD') 
  IF !EMPTY(ALLTRIM(NOTEPAD.mNotes))
    FOR ln=1 TO MEMLINES(NOTEPAD.mNotes)
      IF !EMPTY(mline(NOTEPAD.mNotes,ln))
        lcNToRet = lcNToRet+IIF(!EMPTY(lcNToRet),CHR(13),'')+ALLTRIM(mline(NOTEPAD.mNotes,ln))
      ENDIF
    ENDFOR
  ENDIF
ENDIF
lcNToRet = ALLTRIM(lcNToRet)
*:****************************************************************
*: Name        : lfCrTmpFls
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to Create temp files.
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfCrTmpFls ()
*:****************************************************************
*: Refer to C102097,8 
FUNCTION lfCrTmpFls

PRIVATE lnOldAls
lnOldAls = SELECT(0)

*-- Create the Temp file that we will collect the data on it.
SELECT (lcTmpOrdL)
= AFIELDS (laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 2, 4]

laFileStru[lnFileStru+1,1] = 'cStyType'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'cPty'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 3
laFileStru[lnFileStru+2,4] = 0

CREATE TABLE (gcWorkDir+lcOrdLTmp) FROM ARRAY laFileStru
INDEX ON PikTkt + cStyType+STYLE+ cGrupDetal  TAG (lcOrdLTmp)
SELECT (lnOldAls)

*-- End OF lfCrTmpFls.
*:****************************************************************
*: Name        : lfNewColet
*: Developer   : Ahmed Abdel Naby - (AAN)
*: Date        : 05/28/2001
*: Purpose     : Function to collect data on the new criteria
*:****************************************************************
*: Called from : Program.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfNewColet()
*:****************************************************************
*: Refer to C102097,8 
FUNCTION lfNewColet
PRIVATE llFlat,llHang
STORE .F. TO llHang , llFlat
SELECT (lcTmpOrdL)
GO TOP
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO (lcOrdLTmp) FROM MEMVAR
  SELECT (lcOrdLTmp)
  REPLACE cPty WITH OrdHdr.Priority
  IF SEEK(Style,'Style')
    REPLACE cStyType WITH Style.ChanFld
    IF Style.ChanFld = "H"
      llHang = .T.
    ENDIF
    IF Style.ChanFld = "F"
      llFlat = .T.
    ENDIF
  ELSE
    IF llFlat AND llHang
      REPLACE cStyType WITH "H"
      SCATTER MEMVAR MEMO
      INSER INTO (lcOrdLTmp) FROM MEMVAR
      REPLACE cStyType WITH "F"
    ELSE
      IF llFlat
        REPLACE cStyType WITH "F"
      ELSE
        IF llHang
          REPLACE cStyType WITH "H"
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
SELECT (lcOrdLTmp)
*-- End Of lfNewColet.

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 08/06/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
FUNCTION lfGetAddr
PARAMETER llDummy

lcCurrAlis = ALIAS()
laSoldTo = ''
laShipTo = ''


=SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store),'Customer')

lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSolTName = CUSTOMER.BTName


= SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')

IF Customer.Type = 'S' AND Customer.billto = 'M' 
  GO TOP IN 'Customer'
  = SEEK('M'+ PIKTKT.Account ,'Customer')
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , 1)
ELSE
  IF Customer.Type = 'S' AND Customer.billto = 'A'
    laSoldTo[1] = Customer.caddress12
    laSoldTo[2] = Customer.caddress22
    laSoldTo[3] = Alltrim(Customer.caddress32)+','+Alltrim(Customer.caddress42)+','+Alltrim(Customer.caddress52)
    laSoldTo[4] = Customer.caddress62
    *laSoldTo[5] = Customer.caddress62
  ELSE 
    = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , 1)
  ENDIF  
ENDIF  


=lfAdrShift('laSoldTo')



IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  
ELSE    && Else
  = SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')
  
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ELSE
    =SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store))
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  
  *--HDM B802479,1 [Start] Get Ship To Phone#
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)

  *--HDM B802479,1 [End]
  
ENDIF    && End of IF
=lfAdrShift('laShipTo')

SELECT (lcCurrAlis)

RETURN ''
*-- End of lfGetAddr
