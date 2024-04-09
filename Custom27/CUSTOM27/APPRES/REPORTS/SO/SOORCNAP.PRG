*:***************************************************************************
*: Program file  : SOORCNAP.PRG
*: Program desc. : Order Confirmation for APP10
*: For Report    : SOORCNAP.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Ahmed Abdel Naby (AAN)
*: Reference     : C102402,1 / C102402,4
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO Soorcnap
*:***************************************************************************
*:C102402,4 SSE 09/02/2001 Add total pages at page header.
*:C121868,1 BWA 04/28/2004 Add 2 new filters to the grid.
*:B132204,1 TMI 06/19/2006 Do not take into considration the completed lines to get a correct page count
*:B608050,1 NNA 04/17/2007 Fixing the page count specially if you have i.e an order with 14 line
*:B608050,1 NNA            it'll count pages to 3 pages and this not correct because it only 2 pages
*:B608050,3 TMI fixing another way
*:B608050,4 TMI 08/26/2007 use NNA way with changes
*:***************************************************************************

PRIVATE lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen,lnScale,lnSeq
PRIVATE lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

STORE 0  TO lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen,lnScale,lnSeq
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

* Do not print the Order Lines notes. this becasue the customer wants to print the 11 size 
* sizes on one line although these 11 sizes have been created thru SO entry screen in two 
* different lines and if each line has it's own notes. So while collecting the data of those two 
* lines we do not know which notes should be taken. Because of this we will always ignore
* this option. 
llRpOrdLnt = .F.
llPrntBoth = .F. 

=lfEvalSegs()

*C102402,4 Remove relation to Temp Order Line file. [Begin]
SELECT (lcTempOrd)
SET RELATION OFF INTO OBJLINK_A
SET RELATION OFF INTO SCALE
SET RELATION OFF INTO Style

SELECT ORDHDR
SET SKIP TO
SET RELATION OFF INTO (lcTempOrd)

*C121868,1 BWA 04/28/2004 open the invline file to get from it the shipping values.
*C121868,1 BWA 04/28/2004 Add new function to add the complete orders to the filter.[START]
IF !USED(lcInvLinA)
  =gfOpenFile(gcDataDir+"INVLINE","INVLINEO",'SH', @lcInvLinA)
ENDIF

*--BASSEM 2/6/2004
*IF lcRpOrdSap $ "ACHB"
IF lcRpOrdSap $ "AOCHB"
*--BASSEM 2/6/2004

  PRIVATE llHoldOld
  llHoldOld  = llprntboth
  llprntboth = .F.

*--BASSEM 2/6/2004
*  IF lcRpOrdSap = "C"
*--BASSEM 2/6/2004

    SELECT (lcTempOrd)
    ZAP
    IF TYPE('lcTempRcp') <> "U"
      SELECT (lcTempRcp)
      ZAP
    ENDIF
*--BASSEM 2/6/2004
*  ENDIF
*--BASSEM 2/6/2004

  SELECT (lcTempOrd)
  SET FILTER TO

  =lfOrdComp()                && Function to add the complete orders to the filter.
  llprntboth = llHoldOld
ENDIF
*C121868,1 BWA 04/28/2004.[END]

*-- Create the modified OrdLine with the new structure.
*-- I think creating 11 sizes will require TotQty field to be 8 Char instead of 7.
SELECT (lcTempOrd)
= AFIELDS(laFileStru)
PRIVATE lnRows
lnRows = AFIELDS(laFileStru)
DIMENSION laFileStru[lnRows + 15 , 4]
*C102402,4 Remove relation to Temp Order Line file. [End]

laFileStru[lnRows+1 ,1] = 'Qty9'
laFileStru[lnRows+1,2] = 'N'
laFileStru[lnRows+1,3] = 6
laFileStru[lnRows+1,4] = 0

laFileStru[lnRows+2 ,1] = 'Qty10'
laFileStru[lnRows+2,2] = 'N'
laFileStru[lnRows+2,3] = 6
laFileStru[lnRows+2,4] = 0

laFileStru[lnRows+3 ,1] = 'Qty11'
laFileStru[lnRows+3,2] = 'N'
laFileStru[lnRows+3,3] = 6
laFileStru[lnRows+3,4] = 0

laFileStru[lnRows+4 ,1] = 'Sz1'
laFileStru[lnRows+4,2] = 'C'
laFileStru[lnRows+4,3] = 5
laFileStru[lnRows+4,4] = 0

laFileStru[lnRows+5 ,1] = 'Sz2'
laFileStru[lnRows+5,2] = 'C'
laFileStru[lnRows+5,3] = 5
laFileStru[lnRows+5,4] = 0

laFileStru[lnRows+6 ,1] = 'Sz3'
laFileStru[lnRows+6,2] = 'C'
laFileStru[lnRows+6,3] = 5
laFileStru[lnRows+6,4] = 0

laFileStru[lnRows+7 ,1] = 'Sz4'
laFileStru[lnRows+7,2] = 'C'
laFileStru[lnRows+7,3] = 5
laFileStru[lnRows+7,4] = 0

laFileStru[lnRows+8 ,1] = 'Sz5'
laFileStru[lnRows+8,2] = 'C'
laFileStru[lnRows+8,3] = 5
laFileStru[lnRows+8,4] = 0

laFileStru[lnRows+9 ,1] = 'Sz6'
laFileStru[lnRows+9,2] = 'C'
laFileStru[lnRows+9,3] = 5
laFileStru[lnRows+9,4] = 0

laFileStru[lnRows+10,1] = 'Sz7'
laFileStru[lnRows+10,2] = 'C'
laFileStru[lnRows+10,3] = 5
laFileStru[lnRows+10,4] = 0

laFileStru[lnRows+11,1] = 'Sz8'
laFileStru[lnRows+11,2] = 'C'
laFileStru[lnRows+11,3] = 5
laFileStru[lnRows+11,4] = 0

laFileStru[lnRows+12,1] = 'Sz9'
laFileStru[lnRows+12,2] = 'C'
laFileStru[lnRows+12,3] = 5
laFileStru[lnRows+12,4] = 0

laFileStru[lnRows+13,1] = 'Sz10'
laFileStru[lnRows+13,2] = 'C'
laFileStru[lnRows+13,3] = 5
laFileStru[lnRows+13,4] = 0

laFileStru[lnRows+14 ,1] = 'Sz11'
laFileStru[lnRows+14,2] = 'C'
laFileStru[lnRows+14,3] = 5
laFileStru[lnRows+14,4] = 0

laFileStru[lnRows+15 ,1] = 'nPages'
laFileStru[lnRows+15,2] = 'N'
laFileStru[lnRows+15,3] = 3
laFileStru[lnRows+15,4] = 0

CREATE TABLE (gcWorkDir+lcTmpOrd) FROM ARRAY laFileStru

*C102402,4 Create Index according to Sort by option. [Begin]
PRIVATE lcKeyValue
IF lcRpSortBy = 'S'
  INDEX ON CORDTYPE + Order + STORE + STYLE TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
  lcKeyValue = "LEFT(&lcTempOrd..STYLE,lnMajorLen+lnColorLen+1)"
ELSE
  INDEX ON CORDTYPE + Order + STORE + STR(LINENO,6) TAG (lcTempOrd)
  SET ORDER TO TAG (lcTempOrd)
  lcKeyValue = "STR(&lcTempOrd..LineNo,6)"
ENDIF
*C102402,4 Create Index according to Sort by option. [End]

SELECT (lcTempOrd)
LOCATE
PRIVATE lnScale , lnPageCnt , lnRemLine , lcOrdType , lcOrder , lcStore , lcEvalKey
PRIVATE lcCount , lnCount , lcCnt , llNewLine , lnSizeDecr , llInsidNot
STORE '' TO lcOrdType , lcOrder , lcStore , lcEvalKey
lcOrdType = &lcTempOrd..cOrdType
lcOrder = &lcTempOrd..Order
lcStore = &lcTempOrd..Store

lnSizeDecr = IIF(llSizes,56.67,19)
STORE .F. TO llInsidNot , llNewLine
lnScale = 0
lnPageCnt = 1
*B608050,3 TMI 05/13/2007 [Start] line need 56.67 , the page needs 56.67 * 7
*lnRemLine = 342
lnRemLine = CEILING(56.67 * 7)
*B608050,3 TMI [End  ] 
*B132204,1  TMI [Start] Do not take into considration the completed lines 
*SCAN  
SCAN FOR TOTQTY <> 0
  *B132204,1  TMI [End  ] 
  SCATTER MEMVAR MEMO  
  SELECT Scale
  =SEEK('S'+&lcTempOrd..Scale)
  lnScale = Scale.Cnt
  SELECT(lcTmpOrd)
  IF SEEK(&lcTempOrd..cOrdType+&lcTempOrd..Order+&lcTempOrd..Store+&lcKeyValue.)
    llNewLine = .F.
  ELSE
    llNewLine = .T.
    APPEND BLANK
    GATHER MEMVAR MEMO    
    REPLACE TotQty WITH 0
    REPLACE Qty1 WITH 0
    REPLACE Qty2 WITH 0
    REPLACE Qty3 WITH 0
    REPLACE Qty4 WITH 0
    REPLACE Qty5 WITH 0
    REPLACE Qty6 WITH 0
    REPLACE Qty7 WITH 0
    REPLACE Qty8 WITH 0                            
    lnCnt = 0
  ENDIF
  REPLACE TotQty WITH TotQty+&lcTempOrd..TotQty
  DO lfvGetScl
  *FOR lnCount = 1 TO lnScale
    *lnCnt = lnCnt + 1
    *lcCnt = ALLTRIM(STR(lnCnt))
    *lcCount = ALLTRIM(STR(lnCount))
    *REPLACE Sz&lcCnt WITH Scale.Sz&lcCount
    *REPLACE Qty&lcCnt WITH &lcTempOrd..Qty&lcCount
  *ENDFOR
  
  IF lcOrder == &lcTmpOrd..Order
    IF lcStore == &lcTmpOrd..Store
      IF llNewLine
        =lfGetRem(@lnRemLine)
        =lfChkNewPg()
      ENDIF  
    ELSE
      DO lpOrdChang
      IF llNewLine
        =lfGetRem(@lnRemLine)
        =lfChkNewPg()
      ENDIF  
    ENDIF  
  ELSE    
    DO lpOrdChang
    IF llNewLine
      =lfGetRem(@lnRemLine)
      =lfChkNewPg()
    ENDIF  
  ENDIF
ENDSCAN

DO lpOrdChang   && call it after scan finishes as it is considered a new page


lcTempOrd = lcTmpOrd
SELECT (lcTempOrd)
LOCATE

SELECT (lcTempOrd)

*C121868,1 BWA 04/28/2004 Comment this line due to useing the complete orders in this custom progrom.[START]
*IF lcRpQuSt = "O"
  SET FILTER TO TotQty != 0
*ENDIF
*C121868,1 BWA 04/28/2004.[END]

SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT ORDHDR
SET RELATION TO cOrdType + Order INTO (lcTempOrd) ADDITIVE
lcSkipExpr  = [&lcTempOrd]

*-- Select Master report file.
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
*-- End of custom program.

*!***************************************************************************
*! Name      : lpOrdChang
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/02/2001
*! Purpose   : Calculate number of pages when order is changed.
*!***************************************************************************
*! Reference : C102402,4
*!***************************************************************************
*! Example   : DO lpOrdChang
*!***************************************************************************
*
PROCEDURE lpOrdChang

IF llRpOrdNot AND SEEK('B'+IIF(lcOrdType='T','T','')+lcOrder,'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
  llInsidNot = .T.
  
  FOR lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)  
    lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
    IF LEFT(lcCurrLine,1) # '*'
      =lfGetRem(@lnRemLine,.T.)
      =lfChkNewPg()
    ENDIF
  ENDFOR
ENDIF

llInsidNot = .F.

IF SEEK(lcOrdType + lcOrder ,'OrdHdr')
  IF !EMPTY(OrdHdr.Note1)
    lnRemLine = lnRemLine - 18
  ENDIF

  IF !EMPTY(OrdHdr.Note2)
    lnRemLine = lnRemLine - 18
  ENDIF        
ENDIF
    
lcEvalKey = EVALUATE("lcOrdType+lcOrder+lcStore")
=lfAddPages(lnPageCnt)
lcOrdType = &lcTmpOrd..cOrdType
lcOrder = &lcTmpOrd..Order
lcStore = &lcTmpOrd..Store
lnPageCnt = 1
*B608050,3 TMI 05/13/2007 [Start] line need 56.67 , the page needs 56.67 * 7
*lnRemLine = 342
lnRemLine = CEILING(56.67 * 7)
*B608050,3 TMI [End  ] 
*-- End of lpOrdChang.    

*!***************************************************************************
*! Name      : lfAddPages
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/02/2001
*! Purpose   : Get the number of pages for each Po.
*!***************************************************************************
*! Reference : C102402,4
*!***************************************************************************
*! Example   : =lfAddPages(lnPageCnt)
*!***************************************************************************
*
FUNCTION lfAddPages
PARAMETERS lnPageCnt

PRIVATE lcAlias , lnRecNo
lcAlias = ALIAS()
SELECT (lcTmpOrd)
lnRecNo = RECNO()

*B608050,1 TMI 05/13/2007 [Start] Comment this code, fixing the problem another way
**B608050,1 NNA 04/17/2007 (Begin) every page of the form take 7 lines(if there are no notes) so I'm check
**B608050,1                if print notes=No and the mod of the records count divide by 7=0 and in the same time
**B608050,1                lnpagecnt<> MOD(KEY_COUNT,7) then in this case I consider that lnpagecnt is not correct
**B608050,1                and assign MOD(KEY_COUNT,7) to lnpagecnt
*COUNT TO KEY_COUNT FOR cOrdType + Order + Store = lcEvalKey
*IF !llRpOrdNot AND MOD(KEY_COUNT,7)=0 AND lnPageCnt <> (KEY_COUNT/7)
*  lnPageCnt=KEY_COUNT/7
*ENDIF
*B608050,1 NNA (End)
*B608050,1 TMI 05/13/2007 [End  ] Comment this code, fixing the problem another way

*B608050,4 TMI [Start] ignore the other fixes, use the count of the number of lines needed to be printed
*- Count the number of lines included in the temp file such that 7 lines per page
COUNT TO KEY_COUNT FOR cOrdType + Order + Store = lcEvalKey

=SEEK(lcEvalKey)
*- Get number of notepad lines
lnLines = IIF(SEEK('B'+&lcTmpOrd..ORDER,'NOTEPAD') , MEMLINES(NOTEPAD.MNOTES) , 0 )

*- the page can hold 22 note lines , a ordhdr.note1 , 2 is equivelent to 1
lnNote = IIF( LEFT(ALLTRIM(OrdHdr.NOTE1),1)<> '*' AND !EMPTY(OrdHdr.NOTE1) ,1,0) + ;
         IIF( LEFT(ALLTRIM(OrdHdr.NOTE2),1)<> '*' AND !EMPTY(OrdHdr.NOTE2) ,1,0)

lnPageCnt = CEILING(KEY_COUNT/7 + (lnLines+lnNote)/22)
*B608050,4 TMI [End  ] 
 
=SEEK(lcEvalKey)
REPLACE REST nPages WITH lnPageCnt WHILE cOrdType + Order + Store = lcEvalKey

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpOrd))
  GO lnRecNo IN (lcTmpOrd)    && Restore Record #
ENDIF
SELECT (lcAlias)
*-- End of lfAddPages.

*!***************************************************************************
*! Name      : lfGetRem
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/02/2001
*! Purpose   : Get the remaining number of lines in the page.
*!***************************************************************************
*! Reference : C102402,4
*!***************************************************************************
*! Example   : =lfGetRem(@lnRemLine)
*!***************************************************************************
*
FUNCTION lfGetRem
PARAMETERS lnRemLine,llIsNotes

IF llIsNotes
  lnRemLine = lnRemLine - 17.1
ELSE
  lnRemLine = lnRemLine - lnSizeDecr
ENDIF
*-- End of lfGetRem.

*!***************************************************************************
*! Name      : lfChkNewPg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 09/02/2001
*! Purpose   : Check the remainder to add new page or not.
*!***************************************************************************
*! Reference : C102402,4
*!***************************************************************************
*! Example   : =lfChkNewPg()
*!***************************************************************************
*
FUNCTION lfChkNewPg
IF lnRemLine < 0
  lnPageCnt = lnPageCnt + 1
  *B608050,3 TMI 05/13/2007 [Start] line need 56.67 , the page needs 56.67 * 7
  *lnRemLine = 342
  lnRemLine = CEILING(56.67 * 7)
  *B608050,3 TMI [End  ] 
  =lfGetRem(@lnRemLine , llInsidNot)
ENDIF
*-- End of lfChkNewPg.

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 17/08/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- End of lfEvalSegs.

*:***************************************************************************
*: Name        : lfvGetScl
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 17/08/2000
*: Purpose     : 
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfvGetScl()
*:***************************************************************************
FUNCTION lfvGetScl
SELECT Scale
LOCATE
lnSeq = 0
lnScale = 0
lnCount = 0

SCAN FOR Type+LEFT(Scale,1)+Prepak = 'S'+LEFT(&lcTempOrd..Scale,1)
  IF SCALE == &lcTempOrd..Scale
    lnScale = Scale.Cnt
    SELECT(lcTmpOrd)
    FOR lnCount = 1 TO lnScale
      lnCnt = lnSeq+lnCount
      lcCount = ALLTRIM(STR(lnCount))
      lcCnt = ALLTRIM(STR(lnCnt))
      REPLACE Sz&lcCnt WITH Scale.Sz&lcCount
      REPLACE &lcTmpOrd..Qty&lcCnt WITH &lcTmpOrd..Qty&lcCnt + &lcTempOrd..Qty&lcCount

      *C121868,1 BWA 04/28/2004 Add the shipping values from the invline in case Complete selected.[START]
	  IF lcRpQuSt = "C"
	    PRIVATE lcALiasInv
	    lcALiasInv = ALIAS()
	    SELECT (lcInvLinA)
	    *ashraf

        *sameh on 04/17/2004
	    *IF SEEK(&lcTmpOrd..ORDER+STR(&lcTmpOrd..LineNo,6) , lcInvLinA)
	    *  SCAN WHILE &lcInvLinA..ORDER+STR(LineNo,6) = &lcTmpOrd..ORDER+STR(&lcTmpOrd..LineNo,6) AND &lcInvLinA..SCALE = &lcTempOrd..SCALE
	    IF SEEK(&lcTempOrd..ORDER+STR(&lcTempOrd..LineNo,6) , lcInvLinA)
	      SCAN WHILE &lcInvLinA..ORDER+STR(LineNo,6) = &lcTempOrd..ORDER+STR(&lcTempOrd..LineNo,6) AND &lcInvLinA..SCALE = &lcTempOrd..SCALE

			REPLACE &lcTmpOrd..Qty&lcCnt WITH &lcTmpOrd..Qty&lcCnt + &lcInvLinA..Qty&lcCount
			REPLACE &lcTmpOrd..TotQty    WITH &lcTmpOrd..TotQty + &lcInvLinA..Qty&lcCount
	      ENDSCAN
	    ENDIF
	    SELECT (lcALiasInv)
	  ENDIF
      *C121868,1 BWA 04/28/2004.[END]

    ENDFOR
    lnSeq = lnSeq + lnScale
  ELSE
    lnScale = Scale.Cnt
    SELECT(lcTmpOrd)    
    FOR lnCount = 1 TO lnScale
      lnCnt = lnSeq+lnCount
      lcCount = ALLTRIM(STR(lnCount))
      lcCnt = ALLTRIM(STR(lnCnt))
      REPLACE Sz&lcCnt WITH Scale.Sz&lcCount
    ENDFOR
    lnSeq = lnSeq + Scale.Cnt
  ENDIF
ENDSCAN

*--End of lfvGetScl.
*C121868,1 BWA 04/28/2004 Add new function to add the complete orders to the filter.[START]
*:***************************************************************************
*: Name        : lfOrdComp
*: Developer   : BASSEM RAAFAT ERNEST(BWA)
*: Date        : 04/21/2004
*: Purpose     : Function to add the complete orders to the filter.
*:***************************************************************************
*: Called from : SOORCNAP.PRG
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfOrdComp()
*:***************************************************************************
FUNCTION lfOrdComp

SELECT ORDHDR
LOCATE

IF lcRpOrdSap $ "AC"
  IF lcRpOrdSap = "A"
  *--bas
    *lcRpExp = STRTRAN(lcRpExp , "!(OrdHdr.Status $ 'CX')" , [ OrdHdr.Status = "C" ] )
    lcRpExp = STRTRAN(lcRpExp , "!(OrdHdr.Status $ 'CX')" , "!(OrdHdr.Status $ 'X')"  )
  *--bas
  ELSE
    lcRpExp = STRTRAN(lcRpExp , "OrdHdr.Status = lcRpOrdSta" , [ OrdHdr.Status = "C" ])
  ENDIF
ENDIF

*--To ba for all the cases.
lcRpExp = STRTRAN(lcRpExp , "lcRpOrdSta" , "lcRpOrdSap")
LOCATE

SCAN FOR &lcRpExp
  SELECT ORDLINE
  IF SEEK(ORDHDR.CORDTYPE + ORDHDR.ORDER , 'ORDLINE')
    SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTempOrd) FROM MEMVAR
      IF lcRpOrdTyp = "T"
        SELECT (lcTempRcp)
        IF !SEEK(m.Account+m.Style)
          INSERT INTO (lcTempRcp) (Account,Style,nSRP,Scale) VALUES ;
                                  (m.Account,m.Style,m.nSugRetPri,m.Scale)
        ENDIF
        REPLACE Qty1   WITH Qty1   + m.Qty1           ,;
                Qty2   WITH Qty2   + m.Qty2           ,;
                Qty3   WITH Qty3   + m.Qty3           ,;
                Qty4   WITH Qty4   + m.Qty4           ,;
                Qty5   WITH Qty5   + m.Qty5           ,;
                Qty6   WITH Qty6   + m.Qty6           ,;
                Qty7   WITH Qty7   + m.Qty7           ,;
                Qty8   WITH Qty8   + m.Qty8           ,;
                TotQty WITH TotQty + m.TotQty         ,;
                Amount WITH Amount + m.TotQty*m.Price
      ENDIF
    ENDSCAN
  ENDIF
ENDSCAN

IF lcRpOrdSap = "A"
  lcRpExp = STRTRAN(lcRpExp , [OrdHdr.Status = "C"] , [ !(OrdHdr.Status $ 'X') ] )
ENDIF


*--End of lfOrdComp.
*C121868,1 BWA 04/28/2004.[END]