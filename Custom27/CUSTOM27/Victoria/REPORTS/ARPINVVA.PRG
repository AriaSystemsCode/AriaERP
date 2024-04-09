*:***************************************************************************
*: Program file  : ARPINVVA.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR VICTORIA ASHLEY.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100883)
*! Date          : 09/18/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101986
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVVA
*:***************************************************************************
     *-- Section initial the variables of the program --*
DIMENSION laSku1[8]
STORE "" TO laSku1 , lcStrToPr1
     *-- Section initial the variables of the program --*

     *-- Section collect the data and select the index to the temp. file --*
SELECT InvLine.* ;
 FROM  InvLine,InvHdr;
 WHERE InvHdr.Invoice = InvLine.Invoice ;
 AND   &lcRpExp INTO TABLE (gcWorkDir+lcTmpLin)

INDEX ON Invoice + STR(LINENO,6) TAG (lcTmpLin)
     *-- End section collect the data and select the index to the temp. file --*

     *-- Section break the relatoins --* 
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
     *-- End section break the relatoins --* 

SELECT (lcTmpLin)
SET ORDER TO 1
SELECT CODES
SET ORDER TO CODES IN CODES


     *-- Section create the relatoins of the program --* 
SELECT (lcTmpLin)
SET RELATION TO INVOICE  INTO InvHdr ADDI
SET RELATION TO IIF(!EMPTY(&lcTmpLin..ALTSTYLE) , &lcTmpLin..ALTSTYLE ,&lcTmpLin..Style) INTO STYLE ADDITIVE
SET RELATION TO "S"+SCALE INTO SCALE ADDI

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO ORDHDR ADDITIVE
     *-- End section create the relatoins of the program --* 

SELECT (lcTmpLin)
lnCopies = IIF(gcDevice <> 'SCREEN',5,1)
FOR lnRepeat = 1 to lnCopies
  GO TOP
  DO gfDispRe WITH EVAL('lcFormName')
ENDFOR
SET DEVICE TO SCREEN
llarpinv = .F.

*!*************************************************************
*! Name      : lfGetSKuNo
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/18/2000
*! Purpose   : Function to get the Skus of a style
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  lfGetSKuNo()
*!*************************************************************
FUNCTION lfGetSKuNo
PARAMETERS lcDumdy
PRIVATE lnPrevAl

STORE "" TO laSku1 , lcStrToPr1


IF ! SEEK('S'+&lcTmpLin..Account+&lcTmpLin..Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF

lnPrevAl = SELECT (0)
SELECT Spck_Lin
IF !EMPTY(PACK_ID)

  lnI = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  
  lcStrToPr1 = SUBSTR(Pack_Id,1,lnDime1)
  
   SCAN REST WHILE Type+Account+Style = 'S'+&lcTmpLin..Account+&lcTmpLin..Style ;
        FOR IIF(EMPTY(lcStrToPr1),'.T.', Pack_Id = lcStrToPr1) .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku1[lnX]= lcStrToPr1 + SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
ENDIF

SELECT (lnPrevAl)
IF EMPTY(lcStrToPr1)
  STORE SPACE(0) TO laSku1
  llPrtSku = .F.
ELSE
  llPrtSku = .T.
ENDIF

RETURN ''
*-- End of lfGetSKuNo.
*!*************************************************************
*! Name      : lfGetSuplir
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 09/18/2000
*! Purpose   : Function to get the supplier.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  lfGetSuplir()
*!*************************************************************
FUNCTION lfGetSuplir
PARAMETERS lcDumdy
PRIVATE lcAlias , lnRecno

lcAlias = SELECT(0)
lnRecno = RECNO(lcAlias)

SELECT CUSTOMER
IF SEEK('M' + INVHDR.ACCOUNT)
  SCAN REST WHILE Type + Account + Store = '' ;
       FOR CUSTOMER.ACCOUNT = INVHDR.ACCOUNT
    lcDumdy = CUSTOMER.Duns
    IF !EMPTY(lcDumdy)
      EXIT
    ENDIF
  ENDSCAN
ENDIF


SELECT(lcAlias)
GOTO lnRecno IN (lcAlias)
RETURN lcDumdy
*-- End of lfGetSuplir.