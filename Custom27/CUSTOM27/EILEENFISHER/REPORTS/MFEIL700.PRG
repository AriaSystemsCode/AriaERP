*:*************************************************************
*: Program file  : MFEIL700.PRG
*: Program desc. : Customized temporary receiving recap report.
*: For screen    : None.
*:        System : Aria Apparel System.
*:        Module : Manufacturing.
*:      Customer : Eileen Fisher.
*:     Developer : Khalid Mohi El-Din Mohamed (KHM)
*:*************************************************************
*: Calls : 
*:         Procedures : 
*:         Functions  : lfPrepair()
*:                      lfvTmpNRng()
*:                      lfBrowTmp()
*:                      lfSetFltr()
*:                      lfBldTmp()
*:                      lfGetOpn()
*:                      lfGetDif()
*:                      lfAdRec()
*:                      lfPrtRep()
*:                      lfPrtHdr()
*:                      lfPrtDet()
*:                      lfPrtQty()
*:                      lfAddArr()
*:*************************************************************
*: Passed Parameters  : llRef
*:*************************************************************
*: Report Layout..
*:00 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1...
*:01 EIL700                                                                                             DATE : ±±\±±\±±
*:02 RECEIVING NUMBER : ±±±±±±                      ±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±                      TIME : ±±±±±±±±
*:03 STATUS           : ±±±±±±±±                    RECEIVING REPORT RECAP BY STYLE                     PAGE : ±±      
*:04 RECEIVING DATE   : ±±\±±\±±                                                                        USER : ±±±±±±±±
*:05                     NOTE: THE "*******" MEANS THAT THESE COLOR/DYELOT IS NOT ON THIS CUTTKT#                      
*:06 ******************************************************************************************************************
*:07                                                                                       WIP DIFF                    
*:08 CUTTKT STYLE        COLOR  D T   QTY1   QTY2   QTY3   QTY4   QTY5 TOTQTY   QTY1   QTY2   QTY3   QTY4   QTY5 TOTQTY 
*:00 ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....
*:09 ******************************************************************************************************************
*:10 ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:00 0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1...
*:11 ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:.. ------------------------------------------------------------------------------------------------------------------
*:.. SUBTOTAL BY STYLE ±±±±±±±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±±
*:.. ------------------------------------------------------------------------------------------------------------------
*:.. ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:.. ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:.. ------------------------------------------------------------------------------------------------------------------
*:.. SUBTOTAL BY STYLE ±±±±±±±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±±
*:.. ------------------------------------------------------------------------------------------------------------------
*:.. ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:.. ±±±±±± ±±±±±±±±±±±± ±±±±±± ± ± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± 
*:.. ==================================================================================================================
*:.. TOTAL                          ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±± ±±±±±±
*:.. ==================================================================================================================
*:*************************************************************
*Modifications
*C102333,1 MHM 05/29/2001 Make the report work off PO's Too     
*B605294,1 KHM 01/02/2002 Printing 2 Char of dyelot instead of one.
*:*************************************************************

PARAMETERS llRef

*C102333,1 [start] define lcModl variable to hold the suitable value 
*C102333,1 according to the running module PO/MF
lcModl = IIF(gcAct_Appl = "PO",'I','M')
*C102333,1 MHM 05/29/2001 [end]

lcRepTmp  = gfTempName()
lcWipDif  = lcRpWipDif
lnTotNum  = 00
lnRow     = 00
lnMaxRow  = 55
lnPrtdSiz = 05
lnRepWid  = (31+7+7+((lnPrtdSiz*7)*2)-1)


*-- Variables to hold the non major segments.
STORE 0 TO lnClrPo,lnColorLen

*-- To get the lenght of the style major
lnMajLen = LEN(gfItemMask('PM'))
*-- To get the non major segments.
=lfNonMaj()

IF lfPrepair()
  IF lfSetFltr() AND lfBldTmp()
    = lfPrtRep()
    = EndReport()
    SET DEVICE TO SCREEN
  ELSE
    *=gfModalGen('INM38155B00000','DIALOG' )
    = gfModalGen('TRM00052B00000','DIALOG' )
  ENDIF  
ENDIF

*!*************************************************************
*! Name      : lfPrepair
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : To prepair the program to run.
*!*************************************************************
*: Example   : = lfPrepair()
*:*************************************************************
FUNCTION lfPrepair
PRIVATE llRet

= gfOpenFile (gcDataDir+"CtKtRcvH","CtKtRcvH","SH")
GOTO TOP
IF EOF("CtKtRcvH")
  =gfModalGen('INM38169B00000','DIALOG' )
  llRet = .F.
ELSE
  = gfOpenFile (gcDataDir+"CtKtRcvL", "CtKtRcvL", "SH")
  = gfOpenFile (gcDataDir+"CutTktL" , "CutTktL" , "SH")

  *C102333,1 MHM 05/29/2001 open posln file[start]
  = gfOpenFile (gcDataDir+"PosLn" , "PosLn" , "SH")
  *C102333,1 MHM 05/29/2001 [end]
  llRet = .T.
ENDIF

RETURN llRet


*!*************************************************************
*! Name      : lfvTmpNRng
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Validate the entered temp receive numbers.
*!*************************************************************
*! Calls     :      Functions  : lfvTmpNRng()
*!*************************************************************
FUNCTION lfvTmpNRng
PRIVATE lcCurVar, lnAlias, lcOldVal

*C102333,1 [start] define lcModl variable to hold the suitable value 
*C102333,1 according to the running module PO/MF
lcModl = IIF(gcAct_Appl = "PO",'I','M')
*C102333,1 MHM 05/29/2001 [end]   

lnAlias  = SELECT()

lcCurVar = SYS(18)
lcOldVal = &lcCurVar
SELECT CtKtRcvH
*IF !EMPTY(&lcCurVar) AND !SEEK('M'+&lcCurVar)
IF !EMPTY(&lcCurVar) AND !SEEK(lcModl+&lcCurVar)
  lcRetVal = lfBrowTmp()
  &lcCurVar = lcRetVal
ENDIF

SELECT (lnAlias)

*!*************************************************************
*! Name      : lfBrowTmp
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Browse the temp receive header file.
*!*************************************************************
*! Calls     :      Functions  : lfBrowTmp()
*!*************************************************************
*! Example : llNothing = lfBrowTmp()
*!*************************************************************
FUNCTION lfBrowTmp
PRIVATE lcBrFields,lnAlias,lcTitle,laBrowVal
DIMENSION laBrowVal[1]
laBrowVal = SPACE(6)
lnAlias    = SELECT()
lcBrFields = [TmpRcvNum:8:H="Receiv#",cDesc:H="Description",cStatus:H="Status"]

*C102333,1 MHM 05/29/2001 change variable to work with both PO and MF[Start]
*IF AriaBrow(cType = "M","Temporary Receiving Numbers",;
             gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,;
             .F.,.F.,"CtKtRcvH.TmpRcvNum","laBrowVal")
SELECT CtKtRcvH
IF AriaBrow([FOR cType = lcModl],"Temporary Receiving Numbers",;
             gnBrFSRow1, gnBrFSCol1,gnBrFSRow2, gnBrFSCol2,;
             .F.,.F.,"CtKtRcvH.TmpRcvNum","laBrowVal")
*C102333,1 MHM 05/29/2001 [end]

ENDIF
SELECT(lnAlias)
RETURN laBrowVal[1]

*!*************************************************************
*! Name      : lfSetFltr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Set the filter to the temp receive header file.
*!*************************************************************
*! Example : llNothing = lfSetFltr()
*!*************************************************************
FUNCTION lfSetFltr
PRIVATE  lnAlias

IF llRef
  lnAlias  = SELECT()
  SELECT CtKtRcvH

  *C102333,1 MHM change variable to work with both PO and MF[Start]
  *COUNT ALL FOR cType+TmpRcvNum = 'M'+lcTmpRcvNo TO lnTotNum
  *SET FILTER TO cType+TmpRcvNum = 'M'+lcTmpRcvNo
  COUNT ALL FOR cType+TmpRcvNum = lcModl +lcTmpRcvNo TO lnTotNum
  SET FILTER TO cType+TmpRcvNum = lcModl +lcTmpRcvNo
  *C102333,1 MHM 05/29/2001 [End]
  
  GOTO TOP
  SELECT(lnAlias)
  RETURN
ENDIF

lnAlias  = SELECT()
SELECT CtKtRcvH
COUNT ALL FOR &lcRpExp TO lnTotNum
SET FILTER TO &lcRpExp
GOTO TOP
SELECT(lnAlias)

RETURN !EOF("CtKtRcvH")

*!*************************************************************
*! Name      : lfBldTmp
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Build the used temp file.
*!*************************************************************
*! Example   : lfBldTmp()
*!*************************************************************
FUNCTION lfBldTmp
PRIVATE lnAlias

CREATE CURSOR (lcRepTmp) ;
              (cTmpRcvNum C(6), cStyle   C(12), cColor  C(6),;
               cWareCode  C(6), cDyelot  C(10), cCutTkt C(6),;
               cTranCd    C(3), nQty1   N(6,0), nQty2 N(6,0),;
               nQty3    N(6,0), nQty4   N(6,0), nQty5 N(6,0),;
               nQty6    N(6,0), nQty7   N(6,0), nQty8 N(6,0),;
               nDif1    N(6,0), nDif2   N(6,0), nDif3 N(6,0),;
               nDif4    N(6,0), nDif5   N(6,0), nDif6 N(6,0),;
               nDif7    N(6,0), nDif8   N(6,0), lNewRec L   ,; 
               cStatus  C(1)  , dDate   D)
INDEX ON cTmpRcvNum+cStyle+cColor+cWareCode+cDyelot+cCutTkt+;
         cTranCd+STR(RECNO(),6);
     TAG (lcRepTmp)

lnAlias  = SELECT()
lnCurNum = 0
DIMENSION laOrgn[8], laDiff[8], laTmpRcv[8]

SELECT CtKtRcvH
SCAN
  lnCurNum = lnCurNum + 1
  *C102333,1 MHM change variable to work with both PO and MF[Start]
  *IF SEEK('M'+CtKtRcvH.TmpRcvNum, "CtKtRcvL")
  IF SEEK(lcModl+CtKtRcvH.TmpRcvNum, "CtKtRcvL")
  *--C102333,1 MHM 05/29/2001 [End]
    SELECT CtKtRcvL
    *--C102333,1 MHM  change variable to work with both PO and MF[Start]
    *SCAN REST WHILE Ctype+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+;
    *           STR(nLineNo,6)+STR(LineNo,6)+TranCd = 'M'+CtKtRcvH.TmpRcvNum
    SCAN REST WHILE Ctype+TmpRcvNum+Cuttkt+Style+Dyelot+cCarton+;
               STR(nLineNo,6)+STR(LineNo,6)+TranCd = lcModl+CtKtRcvH.TmpRcvNum
  *C102333,1 MHM change variable to work with both PO and MF[End]

      STORE 0 TO laOrgn, laDiff, laTmpRcv

      lcTmpExp  = TmpRcvNum+SUBSTR(Style,1,lnMajLen)+SUBSTR(Style,lnClrPo,lnColorLen)+cWareCode+Dyelot+CutTkt+;
                  TranCd
      *C102333,1 MHM change the expression to work with both PO and MF[Start]
      *lcMstExp  = CutTkt+Style+Dyelot
      lcMstExp = IIF(lcModl = 'M',CutTkt+Style+Dyelot,"P"+CutTkt+Style)
      *C102333,1 MHM 05/29/2001 [End]
      
      llNewRec  = lfGetOpn()
      SCATTER FIELDS LIKE Qty* TO laTmpRcv
      IF lfGetDif()
        llNothing = lfAdRec()
      ENDIF  
    ENDSCAN  
  ENDIF
ENDSCAN
GOTO TOP IN (lcRepTmp)
SELECT(lnAlias)

RETURN !EOF(lcRepTmp)

*!*************************************************************
*! Name      : lfGetOpn
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Computes the open quantities for any 
*!             C.T.\Sty\Clr\WareHouse\Dyelot record from the 
*!             master cutting ticket file.
*!*************************************************************
*! Example   : lfGetOpn()
*!*************************************************************
FUNCTION lfGetOpn
PRIVATE lnAlias, llRetVal

lnAlias  = SELECT()
llRetVal = .T.
*C102333,1 MHM change the data collecting to work with both PO and MF[Start]
*SELECT CutTktL
*IF SEEK(lcMstExp)
*  SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = lcMstExp
*    SCATTER FIELDS LIKE Qty* TO laQty
*    lnAdd     = IIF(TranCd = "1", 1, -1)
*    llNoThing = lfAddArr(@laOrgn, "laQty", lnAdd)
*    laQty     = 0
*  ENDSCAN
*  llRetVal = .F.  
*ENDIF
IF lcModl = 'M'
  SELECT CutTktL
  IF SEEK(lcMstExp)
    SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = lcMstExp
      SCATTER FIELDS LIKE Qty* TO laQty
      lnAdd     = IIF(TranCd = "1", 1, -1)
      llNoThing = lfAddArr(@laOrgn, "laQty", lnAdd)
      laQty     = 0
    ENDSCAN
    llRetVal = .F.  
  ENDIF
ELSE
  SELECT Posln
  IF SEEK(lcMstExp)
    llChkScn = .F.
    SCAN REST WHILE "P"+Po+Style+STR(lineno,6)+TranCd = lcMstExp  FOR Dyelot = CtKtRcvL.Dyelot
      llChkScn = .T.
      IF TranCd = '3'
        LOOP
      ENDIF
      SCATTER FIELDS LIKE Qty* TO laQty
      lnAdd     = IIF(TranCd = "1", 1, -1)
      llNoThing = lfAddArr(@laOrgn, "laQty", lnAdd)
      laQty     = 0
    ENDSCAN
    IF llChkScn
      llRetVal = .F.  
    ENDIF  
  ENDIF
ENDIF  
*C102333,1 MHM 05/29/2001 [End]

SELECT(lnAlias)

RETURN (llRetVal)

*!*************************************************************
*! Name      : lfGetDif
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Computes the difference between the qty from 
*!             the temp receive file with the open qty from 
*!             the master cutting ticket file.
*!*************************************************************
*! Example   : lfGetDif()
*!*************************************************************
FUNCTION lfGetDif
PRIVATE lnAlias, llRetValue

llNothing = ACOPY(laTmpRcv, laCurRcv)
IF SEEK(lcTmpExp, lcRepTmp)
  lnAlias = SELECT()
  SELECT (lcRepTmp)
  SCATTER FIELDS LIKE nQty* TO laOldRcv
  llNothing = lfAddArr(@laCurRcv, "laOldRcv", 1)
  SELECT (lnAlias)
ENDIF
llNothing  = ACOPY(laOrgn, laDiff)

FOR lnI = 1 TO ALEN(laDiff)
  laDiff[lnI] = MAX(laDiff[lnI],0)
ENDFOR

llNothing  = lfAddArr(@laDiff, "laCurRcv", -1)
llRetValue = .T.

IF lcWipDif <> "B"
  llRetValue = (lcWipDif = "P")
  FOR lnI = 1 TO lnPrtdSiz
    IF laDiff[lnI] < 0
      llRetValue = !llRetValue
      EXIT
    ENDIF
  ENDFOR
ENDIF  

RETURN (llRetValue)

*!*************************************************************
*! Name      : lfAdRec
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Update or add a new record to the temp file 
*!             used in the report (RepTmp).
*!*************************************************************
*! Example   : lfAdRec()
*!*************************************************************
FUNCTION lfAdRec
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcRepTmp)
IF !SEEK(lcTmpExp)
  APPEND BLANK

  REPLACE cTmpRcvNum WITH CtKtRcvL.TmpRcvNum ,;
          cStyle     WITH SUBSTR(CtKtRcvL.Style,1,lnMajLen),;
          cColor     WITH SUBSTR(CtKtRcvL.Style,lnClrPo,lnColorLen),;
          cWareCode  WITH CtKtRcvL.cWareCode ,;
          cDyelot    WITH CtKtRcvL.Dyelot    ,;
          cCutTkt    WITH CtKtRcvL.CutTkt    ,;
          cTranCd    WITH CtKtRcvL.TranCd
ENDIF
REPLACE cStatus WITH CtKtRcvH.cStatus        ,;
        dDate   WITH CtKtRcvH.dDate          ,;
        lNewRec WITH llNewRec
SCATTER FIELDS LIKE nQty* TO laRcvQty
= lfAddArr(@laRcvQty, "laTmpRcv", 1)
GATHER FROM laDiff FIELDS LIKE nDif*
GATHER FROM laRcvQty FIELDS LIKE nQty*
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfPrtRep
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Start printing the report.
*!*************************************************************
*! Example   : =lfPrtRep()
*!*************************************************************
FUNCTION lfPrtRep
PRIVATE lnAlias

DIMENSION  laStyQty[8], laStyDif[8], laTmpQty[8], laTmpDif[8]
STORE 0 TO laStyQty, laStyDif, laTmpQty, laTmpDif
lnPageNo = 0
lnAlias  = SELECT()
lnTotRec = RECCOUNT(lcRepTmp)
lnCurRec = 0
SELECT (lcRepTmp)
GOTO TOP
WAIT WINDOW "Report printing ! < SPACE BAR > to abort" NOWAIT
CLEAR TYPEAHEAD
SET DEVICE TO PRINT
DO WHILE !EOF(lcRepTmp) AND INKEY() <> 32
  
  *C102333,1 MHM 05/29/2001 (Begin) To print the correct totals for each batch
  DIMENSION  laStyQty[8], laStyDif[8], laTmpQty[8], laTmpDif[8]
  STORE 0 TO laStyQty, laStyDif, laTmpQty, laTmpDif
  *C102333,1 MHM 05/29/2001 (End)
  
  lcCurTmp  = &lcRepTmp..cTmpRcvNum
  lcPrompt  = "Temporary Receiving Number : "+lcCurTmp
  llNothing = lfPrtHdr()
  llNothing = lfPrtDet()
  llNothing = lfPrtQty("=",@laTmpQty, @laTmpDif)
ENDDO
WAIT CLEAR
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfPrtHdr
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Prints the report's header.
*!*************************************************************
*! Example   : =lfPrtHdr()
*!*************************************************************
FUNCTION lfPrtHdr
PRIVATE lcNote, lcUser, lcStatus, lcHdr1, lcHdr2, lcHdr3, lcHdr4

lcRepName  = "EIL700"
lcDate     = "DATE : " + DTOC(DATE())
lcLine1    = "RECEIVING NUMBER : " + PADR(cTmpRcvNum,6)
lcTime     = "TIME : " + TIME()
lcStatus   = SUBSTR("OPEN"+SPACE(4)+"APPROVEDPOSTED",(ATC(cStatus,"AP")*8)+1,8)
lnPageNo   = lnPageNo + 1
lcPageNo   = "PAGE : " + PADR(ALLTRIM(STR(lnPageNo)),8)
lcLine2    = PADR("STATUS",17)+":"+SPACE(1)+lcStatus
lcRepHdr   = "RECEIVING REPORT RECAP BY STYLE"
lcLine3    = PADR("RECEIVING DATE",17)+":"+SPACE(1)+DTOC(dDate)
lcUser     = IIF(EMPTY(gcUser_Id),SPACE(15),"USER : "+PADR(ALLTRIM(gcUser_Id),8))

*C102333,1 MHM 05/29/2001 (Begin) To print CutTkt in case of MF and PO in case of PO
*lcNote     = "NOTE: THE '***' MEANS THAT THIS COLOR/DYELOT IS NOT ON THIS CUTTKT#"
lcNote     = "NOTE: THE '***' MEANS THAT THIS COLOR/DYELOT IS NOT ON THIS "+IIF(lcModl = 'M',"CUTTKT#","PO#")
*C102333,1 MHM 05/29/2001 (End)

lcSep      = REPLICATE("*",lnRepWid)
lcWipDiff  = "WIP DIFFERENCE"
lcDetTitle = SPACE(0)
FOR lnI = 1 TO 2
  FOR lnSize = 1 TO lnPrtdSiz
    lcDetTitle = lcDetTitle + PADL("QTY"+ALLTRIM(STR(lnSize)),7)
  ENDFOR
  lcDetTitle = lcDetTitle + " TOTQTY"
ENDFOR  

@ 01,00 SAY lcRepName+SPACE(lnRepWid-(LEN(lcRepName)+LEN(lcDate)))+lcDate
@ 02,00 SAY lcLine1+PADC(ALLTRIM(qCompany),lnRepWid-(LEN(lcLine1)+LEN(lcTime)))+lcTime
@ 03,00 SAY lcLine2+PADC(lcRepHdr,lnRepWid-(LEN(lcLine2)+LEN(lcPageNo)))+lcPageNo
@ 04,00 SAY lcLine3+SPACE(lnRepWid-(LEN(lcLine3)+LEN(lcUser)))+lcUser
@ 05,00 SAY PADC(lcNote,lnRepWid)
@ 06,00 SAY lcSep
@ 07,00 SAY SPACE(31+7+(lnPrtdSiz*7))+PADC(lcWipDiff,lnRepWid-(31+7+(lnPrtdSiz*7)))

*C102333,1 MHM 05/29/2001 change header to work with both PO and MF[Start]
*@ 08,00 SAY "CUTTKT STYLE        COLOR  D T" + lcDetTitle
IF lcModl = "M"
  @ 08,00 SAY "CUTTKT STYLE        COLOR  D T" + lcDetTitle
ELSE
  @ 08,00 SAY "PO     STYLE        COLOR  D T" + lcDetTitle
ENDIF
*C102333,1 MHM 05/29/2001 [End]
@ 09,00 SAY lcSep

lnRow = 10

*!*************************************************************
*! Name      : lfPrtDet
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Prints the report's details.
*!*************************************************************
*! Example   : =lfPrtDet()
*!*************************************************************
FUNCTION lfPrtDet

STORE 0 TO laStyQty, laStyDif
SELECT (lcRepTmp)
DO WHILE cTmpRcvNum = lcCurTmp
  lcStyle = cStyle
  SCAN REST WHILE cTmpRcvNum+cStyle = lcCurTmp+lcStyle AND lnRow<=lnMaxRow
    @ lnRow,00 SAY cCutTkt
    @ lnRow,07 SAY cStyle
    @ lnRow,20 SAY cColor
    
    *B605294,1 KHM 01/02/2002 (Begin) Printing 2 char instead of 1
    *@ lnRow,27 SAY SUBSTR(ALLTRIM(cDyelot),1,1)
    @ lnRow,27 SAY SUBSTR(ALLTRIM(cDyelot),1,2)
    *B605294,1 KHM 01/02/2002 (End)
    
    IF lcModl = "M"    
      @ lnRow,29 SAY SUBSTR("RDC",VAL(ALLTRIM(cTranCd))-1,1)
    ELSE
      @ lnRow,29 SAY IIF(ALLTRIM(cTranCd) = '2','R',;
                     IIF(ALLTRIM(cTranCd) = '4','D','C'))
    ENDIF
      
    SCATTER FIELDS LIKE nQty* TO laQty
    SCATTER FIELDS LIKE nDif* TO laDif
    llNothing = lfPrtQty("",@laQty, @laDif)
    llNothing = lfAddArr(@laStyQty, "laQty",1)
    llNothing = lfAddArr(@laStyDif, "laDif",1)
    lnCurRec  = lnCurRec + 1
  ENDSCAN
  IF lcCurTmp+lcStyle <> cTmpRcvNum+cStyle 
    llNothing = lfPrtQty("-",@laStyQty, @laStyDif)
    llNothing = lfAddArr(@laTmpQty, "laStyQty", 1)
    llNothing = lfAddArr(@laTmpDif, "laStyDif", 1)
    laStyQty  = 0
    laStyDif  = 0
  ELSE  
    llNothing = lfPrtHdr()
  ENDIF
ENDDO

*!*************************************************************
*! Name      : lfPrtDet
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Prints the report's quantities (in the lines,
*!             in the subtotals and in the grand totals)
*!*************************************************************
*: Passed Parameters  : lcSep  : The printed seperator.
*:                      laQty1 : Array holds the 1st quantities.
*:                      laQty2 : Array holds the 2nd quantities.
*:*************************************************************
*: Example : llNothing = lfPrtQty("","","")
*:*************************************************************
FUNCTION lfPrtQty
PARAMETERS lcSep, laQty1, laQty2
PRIVATE    lcToSay

IF !EMPTY(lcSep)
  lcToSay = IIF(lcSep="-","SUBTOTAL BY STYLE:"+ALLTRIM(lcStyle),"GRAND TOTAL")
  @ lnRow,00 SAY REPLICATE(lcSep,lnRepWid)
  lnRow = lnRow + 1
  @ lnRow,000 SAY lcToSay
ENDIF

lcToSay = ""
FOR lnI = 1 TO 2
  lnTot = 0
  FOR lnCol = 1 TO lnPrtdSiz
    lcArray = "laQty" + ALLTRIM(STR(lnI)) + "[lnCol]"
    lcToSay = lcToSay + STR(&lcArray,7)
    lnTot   = lnTot   + &lcArray
  ENDFOR
  lcToSay = lcToSay + STR(lnTot,7)
ENDFOR
@ lnRow,30 SAY lcToSay + IIF(lNewRec AND EMPTY(lcSep)," ***","")

IF !EMPTY(lcSep)
  lnRow = lnRow + 1
  @ lnRow,00 SAY REPLICATE(lcSep,lnRepWid)
ENDIF

lnRow = lnRow + 1

*!*************************************************************
*! Name      : lfPrtDet
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : Adds two arrays and returns the result in the 
*!             first passed array.
*!*************************************************************
*: Passed Parameters  : laAr1    : Pointer to the first array.
*:                      laAr2    : The 2nd array name.
*:                      lnAction : Action to be taken..
*:                                 +1 ÄÄÄÄÄÄÄÄÄÄ Add
*:                                 -1 ÄÄÄÄÄÄÄÄÄÄ Subtract.
*:*************************************************************
*: Example : llNothing = lfAddArr(@laArr1, "laArray2", 1)
*:*************************************************************
FUNCTION lfAddArr
PARAMETERS laAr1, laAr2, lnAction

FOR lnLoop = 1 TO ALEN(laAr1)
  laAr1[lnLoop] = laAr1[lnLoop] + (&laAr2[lnLoop] * lnAction)
ENDFOR
RETURN .T.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 11/22/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
R_WIDTH = "W"

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Khalid Mohi El-Din Mohamed (KHM)
*! Date      : 03/21/1999
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnNonMajPo = 0
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      llStopConc = .T.
    ENDIF
  ENDIF
ENDFOR
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''