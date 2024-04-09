
*:***************************************************************************
*: Program file  : ARPINVSO.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR SOCCER SELECT.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C101258)
*: Date          : 04/22/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102128
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVSO
*:***************************************************************************
           *--Section of Initializing the variables --*
lcTax      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
lcTaxDesc  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
lcTaxRate  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lcTaxMeth  = gfGetMemVar('M_TAX_METH',gcAct_Comp)

SELECT INVHDR
LOCATE FOR &lcRpExp
lcTmpHInv = gfTempName()

IF !FOUND()
  WAIT WINDOW "No Records Selected" NOWAIT
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*--Function to create the temp. file for the selected criteria.
STORE "" TO lcInvFile
STORE '' TO lcShipVia , lcTermBm
STORE .F. TO llSlctInv , llSlctAl , llFlge
=lfSlctInv()
SET DEVICE TO PRINT

*--THE COLOR LENGTH
STORE 0 TO lnClrLnSo , lnClrPosSo
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnSo  = LEN(laItemSeg[lnCount,3])
    lnClrPosSo = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenth = LEN(gfItemMask('PM'))

IF llTeamInv
  IF !USED(lcConsInvL)
    =gfOpenFile(gcDataDir+"ConsInvL","ConsInvL", 'SH', @lcConsInvL, .T.)
  ENDIF  
  lnPackage = 0
ENDIF

*-- Openinig the necessary files.
IF !USED(lcOrdLine)
  =gfOpenFile(gcDataDir+"Ordline","Ordlinst", 'SH', @lcOrdLine, .T.)
ENDIF  

*-- Initializing the necessary variables.
lcInv      = SPACE(6)
TmpInvLine = gfTempName()
TmpLines   = gfTempName()
llFirst    = .T.
DIMENSION laScale[4,9]
laScale    = ''

*-- The temporary file that will be used to in the printing.
CREATE CURSOR(TmpInvLine);
  (cInvoice C(6), nLineNo N(6)  , cStyle C(12)  , cColor C(6), cStyDesc C(20),;
   cScale C(1)  , nQty1 N(5)    , nQty2 N(5)    , nQty3 N(5) , nQty4 N(5)    ,;
   nQty5 N(5)   , nQty6 N(5)    , nQty7 N(5)    , nQty8 N(5) , nTotQty N(6)  ,;
   nPrice N(8,2), cPack_Id C(22), cBackOrd C(12), mNotes M   , lPrintSKU L   ,;
   lPrintNot L  , lPrintBO L    , lStyHdr L     , lStyle L   , lColor L      ,;
   cType C(6))

IF llTeamInv
  INDEX ON cInvoice+cType+cStyle+cColor+STR(nLineNo,6) TAG (TmpInvLine) OF (TmpInvLine)
ELSE
  INDEX ON cInvoice+cStyle+cColor+STR(nLineNo,6) TAG (TmpInvLine) OF (TmpInvLine)
ENDIF

*-- To get all the lines of the selected invoices sorted by invoice+style
SET TALK ON
IF llSlctAl
  IF llFlge
    SELECT * FROM INVLINE WHERE INVLINE.INVOICE IN (SELECT INVOICE FROM (lcTmpHInv)) INTO CURSOR (TmpLines)
  ELSE
    SELECT * FROM INVLINE WHERE INVLINE.INVOICE = INVOICE INTO CURSOR (TMPLINES)
  ENDIF
ELSE
  SELECT * FROM INVLINE WHERE INVLINE.INVOICE IN (SELECT INVOICE FROM (lcInvFile)) INTO CURSOR (TmpLines)
ENDIF
SET TALK OFF
INDEX ON INVOICE + STYLE TAG (TmpLines) OF (TmpLines)
  
*-- Saving the record pointer of the InvHdr and InvHTemp files.
SELECT InvHdr
lnRecInv = RECNO()

*-- Building the temporary file.
SELECT (TmpLines)
GOTO TOP
DO WHILE !EOF()
  lcInvNo = Invoice
  IF llTeamInv
    lnPackage = 0
    =lfvByTeam(lcInvNo)
  ENDIF

  =SEEK(lcInvNo,'InvHdr')

  DO WHILE Invoice+Style = lcInvNo
    lnNoOfLins = 0
    lcStyle    = LEFT(Style,lnLenth)
    SCAN REST WHILE Invoice+LEFT(Style,lnLenth) = lcInvNo+lcStyle
      WAIT WINDOW "Invoice #: " + lcInvNo + " Style : " + lcStyle NOWAIT
      lnNoOfLins = lnNoOfLins + 1
      =SEEK(LEFT(Style,lnLenth),'Style')
      =SEEK('S'+Scale,'Scale')
      DO CASE
        CASE lnNoOfLins = 1
          *-- To add the first line of each style in the temporary file.
          =lfUpd1stLn()

          *-- Record for the style description.
          APPEND BLANK
          REPLACE cInvoice  WITH &TmpLines..Invoice ,;
                  nLineNo   WITH &TmpLines..LineNo  ,;
                  cStyle    WITH LEFT(&TmpLines..Style,lnLenth)   ,;
                  cColor    WITH SUBSTR(&TmpLines..Style,lnClrPosSo,lnClrLnSo),;
                  cStyDesc  WITH Style.Desc         ,;
                  lStyHdr   WITH .T.                ,;
                  lPrintBO  WITH .F.                ,;
                  lStyle    WITH .F.                ,;
                  lColor    WITH .F.                ,;
                  lPrintSKU WITH .F.                ,;
                  lPrintNot WITH .F.                ,;
                  lPrintBO  WITH .F.
          
          IF llTeamInv
            REPLACE cType WITH 'ZZZZZZ'
          ENDIF
        
          *-- To add a record for the back order qty if any.
          =lfAddBkOrd()

          *-- To add SKus records for each style color
          IF SEEK('S'+&TmpLines..Account+LEFT(&TmpLines..Style,lnLenth) ,'Spck_Lin')
            =lfGetKus(&TmpLines..Account,&TmpLines..Style)
          ENDIF

        OTHERWISE

          *-- To add the first line of each style in the temporary file.
          =lfUpd1stLn()
          REPLACE lStyle WITH .F.
          
          IF llTeamInv
            REPLACE cType WITH 'ZZZZZZ'
          ENDIF

          *-- To add a record for the back order qty if any.
          =lfAddBkOrd()

          *-- To add SKus records for each style color
          IF SEEK('S'+&TmpLines..Account+LEFT(&TmpLines..Style,lnLenth),'Spck_Lin')
            =lfGetKus(&TmpLines..Account,&TmpLines..Style)
          ENDIF

      ENDCASE

      SELECT (TmpLines)
    ENDSCAN  
  ENDDO

  IF SEEK("O" + InvHdr.Order + InvHdr.Store , lcOrdLine)
    SELECT (lcOrdLine)
    SCAN WHILE CORDTYPE + ORDER + STORE = "O" + InvHdr.Order + InvHdr.Store FOR TotQty > 0
      =SEEK(LEFT(Style,lnLenth),'Style')
      =SEEK('S'+Scale,'Scale') 

      IF IIF(llTeamInv,!SEEK(InvHdr.Invoice+'ZZZZZZ'+LEFT(Style,lnLenth) ,TmpInvLine),;
                       !SEEK(InvHdr.Invoice+LEFT(Style,lnLenth),TmpInvLine))

        SELECT (TmpInvLine)
        APPEND BLANK
        REPLACE cInvoice  WITH InvHdr.Invoice     ,;
                nLineNo   WITH &lcOrdLine..LineNo ,;
                cStyle    WITH LEFT(&lcOrdLine..Style,lnLenth),;
                cStyDesc  WITH Style.Desc         ,;
                lStyHdr   WITH .F.                ,;
                lPrintBO  WITH .F.                ,;
                lStyle    WITH .T.                ,;
                lColor    WITH .T.                ,;
                lPrintSKU WITH .F.                ,;
                lPrintNot WITH .F.                ,;
                lPrintBO  WITH .F.

        IF llTeamInv
          REPLACE cType WITH 'ZZZZZZ'
        ENDIF
        
        FOR lnCounter = 1 TO Scale.Cnt
          lcCounter = STR(lnCounter,1)
          REPLACE nQty&lcCounter WITH 0
        ENDFOR
        REPLACE nTotQty WITH 0

        *-- Record for the style description.
        APPEND BLANK
        REPLACE cInvoice  WITH InvHdr.Invoice     ,;
                nLineNo   WITH &lcOrdLine..LineNo ,;
                cStyle    WITH LEFT(&lcOrdLine..Style,lnLenth)  ,;
                cColor    WITH SUBSTR(&lcOrdLine..Style,lnClrPosSo,lnClrLnSo),;
                cStyDesc  WITH Style.Desc         ,;
                lStyHdr   WITH .T.                ,;
                lPrintBO  WITH .F.                ,;
                lStyle    WITH .F.                ,;
                lColor    WITH .F.                ,;
                lPrintSKU WITH .F.                ,;
                lPrintNot WITH .F.                ,;
                lPrintBO  WITH .F.               

        IF llTeamInv
          REPLACE cType WITH 'ZZZZZZ'
        ENDIF
        
        *-- Back order record
        SELECT (TmpInvLine)
        APPEND BLANK
        REPLACE cInvoice   WITH InvHdr.Invoice     ,;
                nLineNo    WITH &lcOrdLine..LineNo ,;
                cStyle     WITH LEFT(&lcOrdLine..Style,lnLenth)  ,;
                cColor     WITH SUBSTR(&lcOrdLine..Style,lnClrPosSo,lnClrLnSo),;
                cStyDesc   WITH Style.Desc         ,;
                cScale     WITH &lcOrdLine..Scale  ,;
                lPrintSKU  WITH .F.                ,;
                lPrintNot  WITH .F.                ,;
                lColor     WITH .T.

        REPLACE cBackOrd  WITH "** BK ORD **" ,;
                lPrintBO  WITH .T.            ,;
                lStyHdr   WITH .F.            ,; 
                lStyle    WITH .F.

        IF llTeamInv
          REPLACE cType WITH 'ZZZZZZ'
        ENDIF

        FOR lnCounter = 1 TO Scale.Cnt
          lcCounter = STR(lnCounter,1)
          REPLACE nQty&lcCounter WITH &lcOrdLine..Qty&lcCounter
        ENDFOR
      ENDIF

      IF IIF(llTeamInv,!SEEK(InvHdr.Invoice+'ZZZZZZ'+ LEFT(&lcOrdLine..Style,lnLenth) + SUBSTR(&lcOrdLine..Style,lnClrPosSo,lnClrLnSo) ,TmpInvLine),;
         !SEEK(InvHdr.Invoice + LEFT(&lcOrdLine..Style,lnLenth) + SUBSTR(&lcOrdLine..Style,lnClrPosSo,lnClrLnSo) ,TmpInvLine)) AND &lcOrdLine..TotQty > 0

        SELECT (TmpInvLine)
        APPEND BLANK
        REPLACE cInvoice   WITH InvHdr.Invoice     ,;
                nLineNo    WITH &lcOrdLine..LineNo ,;
                cStyle     WITH LEFT(&lcOrdLine..Style,lnLenth)  ,;
                cColor     WITH SUBSTR(&lcOrdLine..Style,lnClrPosSo,lnClrLnSo),;
                cStyDesc   WITH Style.Desc         ,;
                cScale     WITH &lcOrdLine..Scale  ,;
                lPrintSKU  WITH .F.                ,;
                lPrintNot  WITH .F.                ,;
                lColor     WITH .T.

        IF llTeamInv
          REPLACE cType WITH 'ZZZZZZ'
        ENDIF

        REPLACE cBackOrd  WITH "** BK ORD **" ,;
                lPrintBO  WITH .T.            ,;
                lStyHdr   WITH .F.            ,; 
                lStyle    WITH .F.

        FOR lnCounter = 1 TO Scale.Cnt
          lcCounter = STR(lnCounter,1)
          REPLACE nQty&lcCounter WITH &lcOrdLine..Qty&lcCounter
        ENDFOR
      ENDIF
    ENDSCAN
  ENDIF

*-- Record for the notepad or to determine the end of the invoice.  
  SELECT (TmpInvLine)
  APPEND BLANK
  IF SEEK('C'+InvHdr.Invoice,'NotePad') AND llRpInvNot
    REPLACE mNotes WITH NotePad.mNotes
  ENDIF

  REPLACE cInvoice  WITH InvHdr.Invoice ,;
          cStyle    WITH "ZZZZZZZZZZZZ" ,;
          cColor    WITH "ZZZZZZ"       ,;
          lPrintNot WITH .T.

  IF llTeamInv
    REPLACE cType WITH 'ZZZZZZ'
  ENDIF

  SELECT InvHdr
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  SELECT (TmpLines)
ENDDO

SELECT InvHdr
IF RECCOUNT() >= lnRecInv
  GOTO lnRecInv
ENDIF

*--Section to break the main relation for the INVHDR file.
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO Ordhdr
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE
*--Section to break the main relation for the INVHDR file.

*-- Setting the relations.
SELECT (TmpInvLine)
SET RELATION TO cInvoice INTO InvHdr ADDITIVE
SET RELATION TO "S"+cSCALE INTO SCALE ADDITIVE
SET RELATION TO cSTYLE INTO STYLE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(InvHdr.Store),"M"+InvHdr.Account,"S"+Account+Store) INTO Customer ADDITIVE
SET RELATION TO Order INTO OrdHdr ADDITIVE

SELECT (TmpInvLine)
GOTO TOP
DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfGtScal
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To get the scale size titles.
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : lcInvNumb
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGtScal
PARAMETERS lcDummy
PRIVATE lnAlias,lnArryNo,lcScales

lcInvNumb = EVAL(TmpInvLine+'.cInvoice')
lnAlias = SELECT(0)
IF llTeamInv
  STORE " " to laScale
  lnArryNo = 1
  lcScales = '4'
  laScale[lnArryNo,1] = '4'
  =SEEK('S'+'4','Scale')
  FOR lnCnt = 1 TO Scale.Cnt
    lcCnt = STR(lnCnt,1)
    laScale[lnArryNo,lnCnt+1] = Scale.Sz&lcCnt
  ENDFOR
ELSE
  lnArryNo = 0
  lcScales = ''
ENDIF

IF lcInv <> lcInvNumb
  IF !llTeamInv
    STORE " " to laScale
  ENDIF
  lcInv  = lcInvNumb
  SELECT InvLine
  =SEEK(lcInvNumb)
  SCAN WHILE Invoice+STR(LineNo,6) = lcInvNumb
    IF !(Scale $ lcScales)
      lnArryNo = lnArryNo + 1
      lcScales = lcScales + Scale + ','
      IF lnArryNo <= 4
        laScale[lnArryNo,1] = Scale
        =SEEK('S'+Scale,'Scale')
        FOR lnCnt = 1 TO Scale.Cnt
          lcCnt = STR(lnCnt,1)
          laScale[lnArryNo,lnCnt+1] = Scale.Sz&lcCnt
        ENDFOR
      ENDIF  
    ENDIF
  ENDSCAN
ENDIF
SELECT(lnAlias)
RETURN ''

*--End of lfGtScal.
*!*************************************************************
*! Name      : lfDiv
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To get the div.long name
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*FUNCTION lfDiv

lnRecNu = RECNO('Code')
lcDivLName=IIF(SEEK('D'+InvHdr.Division,'Code'),Code.DivLName,SPACE(01))
IF BETWEEN(lnRecNu,1,RECCOUNT('Code'))
  GOTO lnRecNu in Code
ENDIF
IF !EMPTY(lcDivLName) 
  RETURN(UPPER(lcDivLName))
ELSE  
  RETURN(UPPER(QCompany))
ENDIF

*--End of lfDiv.
*!*************************************************************
*! Name      : lfPrnHdr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To print the right StAddr2 and StCity+StState+StZip
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrnHdr
PARAMETERS lcType

*-- The case of printing the StAddr2
IF lcType = "A"
  RETURN IIF(!EOF('CUST_A'),IIF(!EMPTY(CUST_A.STaddr2),cust_A.staddr2,;
  cust_A.stcity+' '+cust_A.ststate+' '+cust_A.stzip),;
  IIF(!EMPTY(CUSTOMER.STaddr2),customer.staddr2,;
  customer.stcity+' '+customer.ststate+' '+customer.stzip))
ELSE

  *-- The case of printing the StCity+StState+StZip
  RETURN IIF(!EOF('CUST_A'),IIF(!EMPTY(CUST_A.STaddr2),;
  Cust_A.StCity+' '+Cust_A.StState+' '+CUST_A.StZip,' '),;
  IIF(!EMPTY(CUSTOMER.STaddr2),;
  Customer.Stcity+' '+Customer.Ststate+' '+Customer.StZip,' '))
ENDIF  

*--End of lfPrnHdr.
*!*************************************************************
*! Name      : lfUpd1stLn
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To add the first line of each style in the temporary file.
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfUpd1stLn

SELECT (TmpInvLine)
APPEND BLANK
REPLACE cInvoice   WITH &TmpLines..Invoice ,;
        nLineNo    WITH &TmpLines..LineNo  ,;
        cStyle     WITH LEFT(&TmpLines..Style,lnLenth),;
        cColor     WITH SUBSTR(&TmpLines..Style,lnClrPosSo,lnClrLnSo),;
        cStyDesc   WITH Style.Desc         ,;
        cScale     WITH &TmpLines..Scale   ,;
        nQty1      WITH &TmpLines..Qty1    ,;
        nQty2      WITH &TmpLines..Qty2    ,;
        nQty3      WITH &TmpLines..Qty3    ,;
        nQty4      WITH &TmpLines..Qty4    ,;
        nQty5      WITH &TmpLines..Qty5    ,;
        nQty6      WITH &TmpLines..Qty6    ,;
        nQty7      WITH &TmpLines..Qty7    ,;
        nQty8      WITH &TmpLines..Qty8    ,;              
        nTotQty    WITH &TmpLines..TotQty  ,;
        nPrice     WITH &TmpLines..Price   ,;
        lPrintSKU  WITH .F.                ,;
        lPrintNot  WITH .F.                ,;
        lPrintBO   WITH .F.                ,;
        lStyHdr    WITH .F.                ,;
        lStyle     WITH .T.                ,;
        lColor     WITH .T.

IF llTeamInv
  REPLACE cType WITH 'ZZZZZZ',;
          nPrice WITH IIF(INLIST(Style.cstygroup,'JY','SH','SO'),0,nPrice)
ENDIF  

*--End of lfUpd1stLn.
*!*************************************************************
*! Name      : lfAddBkOrd
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To add a record for the back order qty of any.
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfAddBkOrd

IF SEEK("O" + InvHdr.Order + InvHdr.Store+LEFT(&TmpLines..Style,lnLenth),lcOrdLine) AND &lcOrdLine..TotQty > 0

  SELECT (TmpInvLine)
  SCATTER MEMVAR MEMO
  INSERT INTO (TmpInvLine) FROM MEMVAR

  REPLACE cBackOrd  WITH "** BK ORD **" ,;
          lPrintBO  WITH .T.            ,;
          lStyHdr   WITH .F.            ,; 
          lStyle    WITH .F.            ,;
          lColor    WITH .T.

  FOR lnCounter = 1 TO Scale.Cnt
     lcCounter = STR(lnCounter,1)
     REPLACE nQty&lcCounter WITH &lcOrdLine..Qty&lcCounter
  ENDFOR
  
  IF llTeamInv
    REPLACE cType WITH 'ZZZZZZ',;
            nPrice WITH IIF(INLIST(Style.cstygroup,'JY','SH','SO'),0,nPrice)
  ENDIF
ENDIF

*--End of lfAddBkOrd.
*!*************************************************************
*! Name      : lfGetKus
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To add a record for Style/Color Skus.
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetKus
PARAMETERS lcAcc,lcSty
PRIVATE laSku

DIMENSION laSku(8,2)
laSku     = ''
lnCustRec = RECNO("Customer")
lcSkuTmpl = IIF(SEEK('M'+lcAcc,'Customer') AND !EMPTY(Customer.SkuTmpl),;
                Customer.SkuTmpl,'DEF')

IF BETWEEN(lnCustRec,1,RECCOUNT('CUSTOMER'))
  GOTO lnCustRec IN Customer
ENDIF

IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  lnDime2 = SkuTmpl.Len4
ELSE
  lnDime1 = 8
  lnDime2 = 8
ENDIF 

SELECT Spck_Lin
lnCount = 0   
lnQtyn  = 1
=SEEK('S'+lcAcc+lcSty)
SCAN REST WHILE Type+Account+Style = 'S'+lcAcc+lcSty
  lnCount = lnCount + 1
  FOR lnQtyn = 1 TO 8
    IF EVAL('Spck_Lin.Qty'+ALLTRIM(STR(lnQtyn))) = 1
      laSku(lnCount, 1) = STR(lnQtyn)
      laSku(lnCount, 2) = "Size "+STR(lnQtyn,1)+":"+ALLTRIM(Spck_Lin.Pack_Id)
      EXIT
    ENDIF
  ENDFOR
ENDSCAN
=ASORT(laSku,1)

FOR lnArryNo = 1 TO ALEN(laSku,1)
  IF !EMPTY(laSku[lnArryNo,2])
    SELECT (TmpInvLine)
    APPEND BLANK
    REPLACE cInvoice   WITH &TmpLines..Invoice ,;
            nLineNo    WITH &TmpLines..LineNo  ,;
            cStyle     WITH LEFT(&TmpLines..Style,lnLenth) ,;
            cColor     WITH SUBSTR(&TmpLines..Style,lnClrPosSo,lnClrLnSo),;
            cPack_Id   WITH laSku[lnArryNo,2]  ,; 
            lPrintSKU  WITH .T.                ,;
            lPrintNot  WITH .F.                ,;
            lPrintBO   WITH .F.                ,;
            lStyHdr    WITH .F.                ,;
            lStyle     WITH .F.                ,;
            lColor     WITH .F.
    
    IF llTeamInv
       REPLACE cType WITH 'ZZZZZZ'
    ENDIF
  ENDIF
ENDFOR

*--End of lfGetKus.
*!*************************************************************
*! Name      : lfvByTeam
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To create the package #
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfvByTeam
PARAMETER lcInvoice

PRIVATE lnAlias
lnAlias = SELECT()

IF SEEK(lcInvoice,'ConsInvL')
  SELECT ConsInvL
  DO WHILE Invoice+Store+Order+Style+STR(LineNo,6) = lcInvoice
    lcStore = Store
    STORE '' TO lcStyle,lcPackgDsc,lcJYSty,lcSHSty

    SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = lcInvoice+lcStore
      WAIT WINDOW 'Creating package for Invoice #: ' +lcInvoice NOWAIT

      =SEEK(Style ,'Style')
      IF Style.cstygroup = "JY" AND EMPTY(lcStyle)
        lcJYSty = Style
        lcSHSty = lfGetSHSty(lcInvoice,lcStore)

        lcStyle = PADR('0'+SUBSTR(Style,2,2)+SUBSTR(lcSHSty,1,2),12,' ')
        =SEEK(Style ,'Style')

        IF ConsInvL.Qty1 + ConsInvL.Qty2 + ConsInvL.Qty3 > 0
          SELECT(TmpInvLine)
          IF !SEEK(lcInvoice+PADL(lnPackage,5,'0') +'A' + lcStyle)
            lnPackage = lnPackage + 1
            APPEND BLANK
          ENDIF
          REPLACE cInvoice  WITH lcInvoice              ,;
                  cStyle    WITH lcStyle                ,;
                  cScale    WITH Style.Scale            ,;
                  cColor    WITH "N/A"                  ,;
                  nQty1     WITH nQty1 + ConsInvL.Qty1  ,;
                  nQty2     WITH nQty2 + ConsInvL.Qty2  ,;
                  nQty3     WITH nQty3 + ConsInvL.Qty3  ,;
                  nTotQty   WITH nTotQty+ConsInvL.Qty1+ConsInvL.Qty2+ConsInvL.Qty3,;
                  nPrice    WITH ConsInvL.Price         ,;
                  lPrintSKU WITH .F.                    ,;
                  lPrintNot WITH .F.                    ,;
                  lPrintBO  WITH .F.                    ,;
                  lStyHdr   WITH .F.                    ,;
                  lStyle    WITH .T.                    ,;
                  lColor    WITH .T.                    ,;
                  cType     WITH PADL(lnPackage,5,'0')+'A'
          ENDIF
        ENDIF
        IF ConsInvL.Qty4+ConsInvL.Qty5+ConsInvL.Qty6+ConsInvL.Qty7+ConsInvL.Qty8 > 0
          IF Style.cstygroup = "JY" AND !EMPTY(lcStyle)
            SELECT(TmpInvLine)
           IF !SEEK(lcInvoice+PADL(lnPackage,5,'0')+'B'+lcStyle)
             APPEND BLANK
           ENDIF
           REPLACE cInvoice  WITH lcInvoice               ,;
                   cStyle    WITH lcStyle                 ,;        
                   cScale    WITH Style.Scale             ,;
                   cColor    WITH "N/A"                   ,;
                   cStyDesc  WITH "#"+ALLTRIM(ConsInvL.Style) + " PKG/#"+ALLTRIM(lcSHSty)+" SHORT",;
                   nQty4     WITH nQty4 + ConsInvL.Qty4   ,;
                   nQty5     WITH nQty5 + ConsInvL.Qty5   ,;
                   nQty6     WITH nQty6 + ConsInvL.Qty6   ,;
                   nQty7     WITH nQty7 + ConsInvL.Qty7   ,;
                   nQty8     WITH nQty8 + ConsInvL.Qty8   ,;
                   nTotQty   WITH nTotQty +ConsInvL.Qty4 + ConsInvL.Qty5 + ConsInvL.Qty6 + ConsInvL.Qty7 + ConsInvL.Qty8,;
                   nPrice    WITH ConsInvL.Price         ,;
                   lPrintSKU WITH .F.                    ,;
                   lPrintNot WITH .F.                    ,;
                   lPrintBO  WITH .F.                    ,;
                   lStyHdr   WITH .T.                    ,;
                   lStyle    WITH .F.                    ,;
                   lColor    WITH .T.                    ,;
                   cType     WITH PADL(lnPackage,5,'0')+'B'
          ENDIF
        ENDIF
    ENDSCAN
  ENDDO
ENDIF
WAIT CLEAR
SELECT(lnAlias)

*--End of lfvByTeam.
*!*************************************************************
*! Name      : lfGetSHSty
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : To get the style code of SH group
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetSHSty
PARAMETERS lcInv,lcStor

PRIVATE lnAlias
lnAlias = SELECT()

lcRetStySH = ''
SELECT (lcConsInvL)
SEEK lcInv + lcStor
SCAN REST WHILE Invoice+Store+order+Style+STR(lineno,6) = lcInv+lcStor
  =SEEK(Style,'Style')
  IF Style.cstygroup = 'SH'
    lcRetStySH = Style
    EXIT
  ENDIF
ENDSCAN

SELECT(lnAlias)
RETURN lcRetStySH

*--End of lfGetSHSty.
*!*************************************************************
*! Name      : lfSolShp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 04/22/2001
*! Purpose   : Function to Get the Sold to & Ship to Address
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfSolShp
PARAMETER lcSolShp
PRIVATE lcAlias , lcInvHdr

STORE '' TO lcShipVia , lcTermBm

lcInvHdr = SELECT(0)
SELECT INVHDR

DECLARE laSoldTo[5,1] , laShipTo[5,1] , laFactor[5,1] , laDivLName[1,2]

laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address
laFactor = ''

IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
=lfAdrShift('laSoldTo')

*B606908,1 Get Order number from Invoice. [Begin]
*IF OrdHdr.Alt_ShpTo
IF !EMPTY(InvHdr.Order) AND SEEK('O'+InvHdr.Order,'OrdHdr') AND OrdHdr.Alt_ShpTo
*B606908,1 Get Order number from Invoice. [End]

  lcShpTName  = OrdHdr.STName  
  laShipTo[1] = OrdHdr.cAddress1
  laShipTo[2] = OrdHdr.cAddress2
  laShipTo[3] = OrdHdr.cAddress3
  laShipTo[4] = OrdHdr.cAddress4
  laShipTo[5] = OrdHdr.cAddress5
ELSE 
  lnCUSRec = 0
  lcAlias = SELECT(0)
  SELECT CUSTOMER
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
  ENDIF

  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
  SELECT(lcAlias)
ENDIF

=lfAdrShift('laShipTo')

=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTermBm = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

SELECT(lcInvHdr)
RETURN ''

*-- End of lfSolShp.
*!*************************************************************
*! Name      : lfSlctInv
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/22/2001
*! Purpose   : Function to check if there is selected critira.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSlctInv
PRIVATE lnAlias

lnPosition = ASUBSCRIPT(laOGVrFlt,ASCAN(laOGVrFlt,'INVHDR.INVOICE   '),1)
IF lnPosition > 0
  lcInvFile = laOGVrFlt[lnPosition,6]
  llSlctInv = IIF(!EMPTY(lcInvFile) .AND. USED(lcInvFile) .AND. RECCOUNT(lcInvFile) > 0 , .T. , .F. )

  IF !llSlctInv
    llSlctAl = .T.         && In case the user select all records.

    =lfvGtFltr()
    IF llFlge
      lnAlias = SELECT(0)
      CREATE CURSOR(lcTmpHInv) (Invoice C(6))
      SELECT INVHDR
      SET SKIP TO
      SCAN FOR &lcRpExp
        INSERT INTO (lcTmpHInv) (Invoice) VALUES (INVHDR.INVOICE)
      ENDSCAN
      SELECT (lcTmpHInv)
      LOCATE
      SELECT(lnAlias)
    ENDIF
  ENDIF
ENDIF

*--End of lfSlctInv.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/22/2001
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

FOR lnCount = 1 TO 6
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(&lcArrayNam)
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*-- End of lfAdrShift.
*!*************************************************************
*! Name      : lfvGtFltr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 04/22/2001
*! Purpose   : Check for the criteria.
*!*************************************************************
*! Called from : ARPINVSO.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvGtFltr()
*!*************************************************************
FUNCTION lfvGtFltr

lnPostDat = AT('(BETWEEN(DTOS(INVHDR.INVDATE)',LCRPEXP)
IF lnPostDat > 0
  llFlge = .T.
ENDIF

lnPosAccnt = AT('INVHDR.ACCOUNT',LCRPEXP)
IF lnPosAccnt > 0
  llFlge = .T.
ENDIF

*--End of lfvGtFltr.