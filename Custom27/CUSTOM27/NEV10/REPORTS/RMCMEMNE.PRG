*:***************************************************************************
*: Program file  : RMCMEMNE.PRG
*: Program desc. : CUSTOMIZED CREDIT MEMO FOR NEVICA.
*: Date          : 08/07/2002
*: System        : Aria Advantage Series.
*: Module        : RETURN MERCHANDISE (RM)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C200377
*: 
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO RMCMEMNE
*:***************************************************************************
*: Modifications :
***************************************************************************
*B606484,1 BWA 09/24/2002 Fix some bugs in the form written in the specs.
*:***************************************************************************

*B606484,1 BWA 09/24/2002 Print the line like the notepad not wrapped.[START]
lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 170
*B606484,1 BWA 09/24/2002.[END]

*--Creat table hold the color and its descreption.
=lfNewLins()

STORE SPACE(0) TO lcCrMemo , lcFrstClr , lcFrstClr2 , lcFrstClr3
STORE .F. TO llPrnSmPad , llNewMemo
STORE 0 TO lnTotCrmmo
lcVatNo = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*--Open the SYCINT file to check the Europcom if it is .T. ot not.
IF !USED('SYCINT')
  =gfOpenFile(gcDataDir+"SYCINT","CCONTCODE", 'SH')
ENDIF

*--Section of the color description.
SELECT (lcRetLine)
LOCATE
SCAN

  IF lcCrMemo # EVAL(lcRetLine+'.CRMEMO')
    =SEEK(CRMEMO , 'RETHDR')
    lcCrMemo = CRMEMO
    llPrnSmPad = .F.
    lnTotCrmmo = 0
    STORE SPACE(0) TO lcFrstClr , lcFrstClr2 , lcFrstClr3
  ENDIF

  lnTotCrmmo = lnTotCrmmo + TOTQTY

  IF !SEEK(SUBSTR(STYLE,lnClrPosR1,lnClrLnR1) , lcTmpClor ) AND !EMPTY(EVAL(lcRetLine+'.ACCOUNT'))
    SELECT (lcTmpClor)
    APPEND BLANK
    REPLACE COLOR WITH SUBSTR(&lcRetLine..STYLE,lnClrPosR1,lnClrLnR1) ;
            Desc  WITH ALLTRIM(gfCodDes(SUBSTR(&lcRetLine..STYLE,lnClrPosR1,lnClrLnR1) , 'COLOR'))
  ENDIF

  *--Copy the records from the (lcRetLine) to (lcTmpLins) to be able to creat the new records.
  SELECT (lcRetLine)
  SCATTER MEMVAR MEMO

  SELECT (lcTmpLins)
  APPEND BLANK
  GATHER MEMVAR MEMO

  IF ALLTRIM(CUSTOMER.CCONT_CODE) $ "FR,CH" AND !EMPTY(EVAL(lcRetLine+'.ACCOUNT'))
    llPrnSmPad = .T.
    IF lcFrstClr # LEFT(&lcRetLine..STYLE,lnLenthR1)
      lcFrstClr = LEFT(&lcRetLine..STYLE,lnLenthR1)
      REPLACE llPrn2Lins WITH .T.
    ENDIF
  ENDIF

  IF !(ALLTRIM(CUSTOMER.CCONT_CODE) $ "GB,ENG") AND !EMPTY(EVAL(lcRetLine+'.ACCOUNT'))
    IF lcFrstClr3 # LEFT(&lcRetLine..STYLE,lnLenthR1)
      lcFrstClr3 = LEFT(&lcRetLine..STYLE,lnLenthR1)
      REPLACE llPrnGbEng WITH .T.
    ENDIF
  ENDIF

  *--To print the line of the "Commodity Code: (STYLE .CCOMCODE) Weight: (STYLE.WEIGHT)".
  IF SEEK(CUSTOMER.CCONT_CODE , 'SYCINT') AND SYCINT.lEuropcom AND !(ALLTRIM(CUSTOMER.CCONT_CODE) $ "FR,CH")
    IF lcFrstClr2 # LEFT(&lcRetLine..STYLE,lnLenthR1)
      lcFrstClr2 = LEFT(&lcRetLine..STYLE,lnLenthR1)
      REPLACE llComWght WITH .T.
    ENDIF
  ENDIF

  *--Section to add the notes record if there is no record in the temp. file
  IF llRpOrdNot AND ALLTRIM(CUSTOMER.CCONT_CODE) $ "FR,CH"
    SELECT (lcRetLine)
    lcAccount = &lcRetLine..Account
    SKIP 1
    IF lcCrMemo # EVAL(lcRetLine+'.CRMEMO') AND !EMPTY(lcAccount)
      SELECT (lcTmpLins)
      APPEND BLANK
      REPLACE CrMemo   WITH lcCrMemo ,;
              lEndLine WITH .T.
      llNewMemo = .T.
    ENDIF
    SELECT (lcRetLine)
    SKIP -1
    SELECT (lcTmpLins)
  ENDIF

  *--Section to the Notepad Template from the SM module.
  IF EMPTY(EVAL(lcRetLine+'.ACCOUNT')) .OR. llNewMemo
    llNewMemo = .F.
    IF SEEK("T" + "CREDIT FRANCE" , 'NOTEPAD') AND llPrnSmPad
      lnMemLins = MEMLINES(NOTEPAD.MNOTES)
      DECLARE laNote[lnMemLins + 3 , 1]
      STORE "" TO laNote
      FOR lnLop = 1 TO lnMemLins + 3
        IF lnLop = 1 OR lnLop = 2 OR lnLop = 3
          laNote[lnLop,1] = IIF(lnLop = 1 , SPACE(10)                     ,;
                            IIF(lnLop = 2 , SPACE(82) + REPLICATE('=',25) ,;
                            IIF(llPrnSmPad, SPACE(82) + 'Quant. Totale' + SPACE(10) + REPLICATE('-',10) + SPACE(25) + ALLTRIM(STR(lnTotCrmmo)) ,;
                                            SPACE(82) + 'Quant. Totale' + SPACE(10) + REPLICATE('-',10) + SPACE(25) + ALLTRIM(STR(lnTotCrmmo)) )))
        ELSE
          laNote[lnLop,1] = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnLop-2))
        ENDIF
      ENDFOR
      SELECT (lcTmpLins)
      FOR lnLop = 1 TO lnMemLins + 3
        IF lnLop = lnMemLins + 3
          REPLACE MNOTES WITH laNote[lnLop] ADDITIVE
        ELSE
          REPLACE MNOTES WITH laNote[lnLop] + CHR(13) + CHR(10) ADDITIVE
        ENDIF
      ENDFOR

      *--To hide the Totqty in the FRX.
      REPLACE llPrntNo WITH .T.
    ENDIF
  ENDIF

  IF ALLTRIM(CUSTOMER.CCONT_CODE) = "FR" AND !EMPTY(EVAL(lcRetLine+'.ACCOUNT'))
    IF SEEK(&lcRetLine..CRMEMO + CUSTOMER.CCONT_CODE + LEFT(&lcRetLine..STYLE,lnLenthR1))
      REPLACE TotQty WITH TotQty + m.TotQty
    ELSE
      *--StyleIndex for the index.
      *--Style      for the printing.
      APPEND BLANK
      GATHER MEMVAR MEMO
      m.STYLE = LEFT(&lcRetLine..STYLE,lnLenthR1)
      REPLACE CcomCode   WITH "FR"           ,;
              LlPrnFr    WITH .T.            ,;
              llNoTVA    WITH .T.            ,;
              DscCcomcod WITH STYLE.CCOMCODE ,;
              StyleIndex WITH LEFT(&lcRetLine..STYLE,lnLenthR1)
    ENDIF
  ENDIF
ENDSCAN

*--Section for the total line in case CUSTOMER.CCONT_CODE = "FR".
SELECT (lcTmpLins)
LOCATE
STORE SPACE(0) TO lcCrMemo
SET RELATION TO STYLE INTO STYLE ADDITIVE

lcCHrEnd = REPLICATE('Z',lnLenthR1-1) + "W"
SCAN REST WHILE CRMEMO + CcomCode + DscCcomcod + StyleIndex = "" FOR CcomCode = "FR"
  IF StyleIndex # lcCHrEnd
    IF lcCrMemo # EVAL(lcTmpLins+'.CRMEMO')
      PRIVATE lnRecTot
      lnRecTot = RECNO()
      lcCrMemo = CRMEMO
      lnTotqty = TOTQTY
      lnStyWgt = STYLE.NSTYWEIGHT
      lnPrice  = Price
      APPEND BLANK

      REPLACE CRMEMO     WITH lcCrMemo            ,;
              CcomCode   WITH "FR"                ,;
              TOTQTY     WITH lnTotqty            ,;
              llNoTVA    WITH .T.                 ,;
              StyleIndex WITH lcCHrEnd            ,;
              TotFrCom   WITH lnStyWgt * lnTotqty ,;
              TotFrVal   WITH lnTotqty * lnPrice  ,;
              DscCcomcod WITH "ZZZZZZZZZZZZZZZWWWW9"

      IF BETWEEN(lnRecTot , 1 , RECNO(lcTmpLins))
        GOTO lnRecTot IN (lcTmpLins)
       ENDIF
    ELSE
      PRIVATE lcRecnoT
      lcRecnoT = RECNO()
      lnTotqty = TOTQTY
      lnStyWgt = STYLE.NSTYWEIGHT
      lnPrice  = Price
      
      IF SEEK(lcCrMemo + CcomCode + lcCHrEnd)
        REPLACE TOTQTY   WITH TOTQTY + lnTotqty                ,;
                TotFrCom WITH TotFrCom + (lnStyWgt * lnTotqty) ,;
                TotFrVal WITH TotFrVal + (lnTotqty * lnPrice)
      ENDIF

      IF BETWEEN(lcRecnoT , 1 , RECNO(lcTmpLins))
        GOTO lcRecnoT IN (lcTmpLins)
       ENDIF

    ENDIF
  ENDIF
ENDSCAN
SET RELATION OFF INTO STYLE

*--To sort the Style Ccomcode.
SET ORDER TO TAG lcTmpLins1

*--Section break the old relation.
SELECT RETHDR
SET RELATION OFF INTO (lcRetLine)
SELECT (lcRetLine)
SET RELATION OFF INTO STYLE

*--Section change the name of the new temp. file to the temp file from the master program.
lcNmToRtrn = lcRetLine
lcRetLine  = lcTmpLins

*--Section creat the new relation.
SELECT RETHDR
SET RELATION TO CrMemo INTO (lcRetLine) ADDITIVE
SET SKIP TO (lcRetLine)

SELECT (lcRetLine)
SET RELATION TO STYLE INTO STYLE ADDITIVE

LOCATE
SET RELATION TO SUBSTR(&lcRetLine..STYLE,lnClrPosR1,lnClrLnR1) INTO (lcTmpClor) ADDITIVE

SELECT RETHDR
LOCATE
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

SET DEVICE TO SCREEN
llRmMemo = .F.

*B606484,1 BWA 09/24/2002 Print the line like the notepad not wrapped.[START]
SET MEMOWIDTH TO lnOldMemW
*B606484,1 BWA 09/24/2002.[END]

*--Get the original file the name of the file with the new collecting data.
lcRetLine = lcNmToRtrn

*--Function to delete all the temp files to prevent the bug of "Many files open"
DIMENSION laFileName[2]
laFileName[1] = lcTmpLins
laFileName[2] = lcTmpClor

=lfBasToClr(@laFileName , 'A')

                       *-- End of the Program --*
*!*************************************************************
*! Name        : lfNewLins
*! Developer   : BASSEM RAFAAT ERNEST(BWA)
*! Date        : 08/07/2002
*! Purpose     : Function to creat the new temp. file.
*!*************************************************************
*! Called from : RMCMEMNE.PRG
*!*************************************************************
*! Calls       : 
*! Procedures  : 
*! Functions   : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : =lfNewLins()
*!*************************************************************
FUNCTION lfNewLins

PRIVATE lcAlasNew
lcAlasNew = SELECT(0)

*--Creat table hold the color and its descreption.
CREATE Table (lcTmpClor) (Color C(lnClrLnR1) , Desc C(30))
INDEX ON Color TAG Color OF (gcWorkDir + lcTmpClor + ".CDX")

*--Creat table hold the lines and the CcomCode value.
SELECT (lcRetLine)
= AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru + 11, 4]

laFileStru[lnFileStru + 1 ,1] = 'CcomCode'
laFileStru[lnFileStru + 1 ,2] = 'C'
laFileStru[lnFileStru + 1 ,3] = 6
laFileStru[lnFileStru + 1 ,4] = 0

laFileStru[lnFileStru + 2 ,1] = 'LlPrnFr'
laFileStru[lnFileStru + 2 ,2] = 'L'
laFileStru[lnFileStru + 2 ,3] = 1
laFileStru[lnFileStru + 2 ,4] = 0

*--This field used in the index to calculate the totqty for the same style.
laFileStru[lnFileStru + 3 ,1] = 'StyleIndex'
laFileStru[lnFileStru + 3 ,2] = 'C'
laFileStru[lnFileStru + 3 ,3] = lnLenthR1
laFileStru[lnFileStru + 3 ,4] = 0

*--To print the 2 lines in case the Customer.ccont_code = "FR" or "CH".
laFileStru[lnFileStru + 4 ,1] = 'llPrn2Lins'
laFileStru[lnFileStru + 4 ,2] = 'L'
laFileStru[lnFileStru + 4 ,3] = 1
laFileStru[lnFileStru + 4 ,4] = 0

*--To prevent printing the Totqty after the Sm notepad.
laFileStru[lnFileStru + 5 ,1] = 'llPrntNo'
laFileStru[lnFileStru + 5 ,2] = 'L'
laFileStru[lnFileStru + 5 ,3] = 1
laFileStru[lnFileStru + 5 ,4] = 0

*--To prevent printing the T.V.A BOX.
laFileStru[lnFileStru + 6 ,1] = 'llNoTVA'
laFileStru[lnFileStru + 6 ,2] = 'L'
laFileStru[lnFileStru + 6 ,3] = 1
laFileStru[lnFileStru + 6 ,4] = 0

*--To print the line of the "Commodity Code: (STYLE.CCOMCODE) Weight: (STYLE.WEIGHT)".
laFileStru[lnFileStru + 7 ,1] = 'llComWght'
laFileStru[lnFileStru + 7 ,2] = 'L'
laFileStru[lnFileStru + 7 ,3] = 1
laFileStru[lnFileStru + 7 ,4] = 0

*--Field collect in it the total of the last page in case CUSTOMER.CCONT_CODE = "FR"
laFileStru[lnFileStru + 8 ,1] = 'TotFrCom'
laFileStru[lnFileStru + 8 ,2] = 'N'
laFileStru[lnFileStru + 8 ,3] = 10
laFileStru[lnFileStru + 8 ,4] = 2

*--Field collect in it the total of the last page in case CUSTOMER.CCONT_CODE = "FR"
laFileStru[lnFileStru + 9 ,1] = 'TotFrVal'
laFileStru[lnFileStru + 9 ,2] = 'N'
laFileStru[lnFileStru + 9 ,3] = 10
laFileStru[lnFileStru + 9 ,4] = 2

*--To print the 2 lines in case the Customer.ccont_code # "GB" or "ENG".
laFileStru[lnFileStru + 10 ,1] = 'llPrnGbEng"'
laFileStru[lnFileStru + 10 ,2] = 'L'
laFileStru[lnFileStru + 10 ,3] = 1
laFileStru[lnFileStru + 10 ,4] = 0

*--To add in it the desc of the Style.Ccomcode to use it in the index.
laFileStru[lnFileStru + 11 ,1] = 'DscCcomcod'
laFileStru[lnFileStru + 11 ,2] = 'C'
laFileStru[lnFileStru + 11 ,3] = 20
laFileStru[lnFileStru + 11 ,4] = 0

CREATE TABLE (gcWorkDir+lcTmpLins) FROM ARRAY laFileStru
INDEX ON CRMEMO + CcomCode + DscCcomcod + StyleIndex TAG lcTmpLins1
INDEX ON CRMEMO + CcomCode + StyleIndex TAG (lcTmpLins)

SELECT(lcAlasNew)
*--End of lfNewLins.
*!*************************************************************
*! Name        : lfHeadNe
*! Developer   : BASSEM RAFAAT ERNEST(BWA)
*! Date        : 08/07/2002
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMCMEMNE.FRX [Header Band]
*!*************************************************************
*! Calls       : 
*! Procedures  : ....
*! Functions   : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : =lfHeadNe()
*!*************************************************************
FUNCTION lfHeadNe
PARAMETER lcReturn

lcAlias = ALIAS()
llEndGroup = .F.
= lfSolSpAdr()  && Calculate [Sold To and Ship To Data]

*-- Calculate (R/A # , Order #, Invoice #) data [Begin]
laMemoData[1,1] = IIF(!EMPTY(RetHdr.RaNo),'R/A #','')
laMemoData[1,2] = RetHdr.RaNo

laMemoData[2,1] = IIF(!EMPTY(RetHdr.Order) , IIF( ALLTRIM(CUSTOMER.CCONT_CODE)  $ "FR , CH" , 'Commande No' , 'Order #' ) ,'')
laMemoData[2,2] = RetHdr.Order

laMemoData[3,1] = IIF(!EMPTY(RetHdr.Invoice) , IIF( ALLTRIM(CUSTOMER.CCONT_CODE)  $ "FR , CH" , 'Facture #' , 'Invoice #' ) ,'')
laMemoData[3,2] = RetHdr.Invoice

*-- Shift empty data
FOR lnMemoData = 1 TO 3
  IF EMPTY(laMemoData[lnMemoData,1])
    = ADEL(laMemoData,lnMemoData)
  ENDIF
ENDFOR

*-- Replace logical items with ''
FOR lnMemoData = 1 TO 6
  IF TYPE('laMemoData[lnMemoData]') $ "UL"
    laMemoData[lnMemoData] = ''
  ENDIF
ENDFOR

*-- Add ":" to the end of 2nd and 3rd titles.
FOR lnMemoData = 2 TO 3
  IF !EMPTY(laMemoData[lnMemoData,1])
    laMemoData[lnMemoData,1] = laMemoData[lnMemoData,1] + ;
                               SPACE(16 - LEN(laMemoData[lnMemoData,1])) + ":"
  ENDIF
ENDFOR
*-- Calculate (R/A # , Order #, Invoice #) data [End]

SELECT (lcAlias)
RETURN ''

*--End of lfHeadNe.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/07/2002
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.