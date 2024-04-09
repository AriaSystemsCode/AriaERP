*:***************************************************************************
*: Program file  : SOALE20.PRG
*: Program desc. : CUSTOMIZED SALES ORDER MATERIAL STATUS REPORT FOR ALENA LTD.
*! Date          : 02/19/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C200150
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOALE20.PRG
*:***************************************************************************
*:B604480,1 SSE 06/06/2001 Fix the following problems :
*:B604480,1                1) Include Trims.
*:B604480,1                2) Does not Print PO details for all colors.
*:B604480,1                3) Does not Print the Qty O/S correctly.
*:B604480,1                4) Column Qty O/S is truncated in FRX.
*:C200230,1                Add some fields in report also remove O/S Qty.
*:B605666,1 RAE 03/19/2002 Fix the problem of reporting only the first PO from several
*:                         PO's for the same material/color which exist for the SO in MAPOALO.
*:B605855,1 TMI 04/14/2002 Fix the bug that required quantity is douplicated for trims
*:***************************************************************************
*
*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

lnOldMemW = SET("MEMOWIDTH")
SET MEMOWIDTH TO 100

*-- if the user change filter criteria then we must collect the data again.
IF llOGFltCh
  *--Check for he records.
  SELECT ORDHDR
  LOCATE FOR &lcRpexp
  IF EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF

  *--Open the custom file MAPOALO
  IF !USED('MAPOALO')
    =gfOpenFile(gcDataDir+'MAPOALO', gcDataDir + 'MAPOSO','SH')
  ENDIF

  *--Get the date to print in the title
  =lfPrnDate()

  *--Function to make the relations between the files and function to collect the data.
  =lfCreatTmp()
  =lfTmpIndex()
  =lfRelation()
  =lfColctDat()

  SELECT (lcHeader)
  IF lcRpSrtOrd = 'D'
    *B605855,4 TMI [START] Add "Material + BomClr + PoNo " to the index
    *INDEX ON DTOS(StartDat) + Sorder + Type TAG StartDat OF (gcWorkDir+lcHeader)
    INDEX ON DTOS(StartDat) + Sorder + Type + Material + BomClr + PoNo TAG StartDat OF (gcWorkDir+lcHeader)
    *B605855,4 TMI [END  ] Add "Material + BomClr + PoNo " to the index
  ELSE
    SET ORDER TO TAG Sorder
  ENDIF

  SELECT (lcHeader)
  GOTO BOTTOM
  IF RECCOUNT() = 0 .OR. EOF()
    *---Text : 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  LOCATE
ENDIF

*B605855,4 TMI [START] Add a function to adjust layout printing
=lfLayout()
*B605855,4 TMI [END  ] Add a function to adjust layout printing

SELECT (lcHeader)
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm')
SELECT MAPOALO
SET RELATION TO
SET CENTURY &lcCentury
SET MEMOWIDTH TO lnOldMemW
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to creat the temp. file hold the data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

*--Creat table for the trasactions.

*C200230,1 Add 2 fields to be added to report. [Begin] 
*CREATE CURSOR (lcHeader) (Sorder C(6)   , Btname C(30)     , StartDat D(8)  , CompltDat D(8) ,;
*                          DocketNo C(6) , Styles C(12)     , Material C(19) , BomDesc C(20)  ,;
*                          BomClr C(6)   , Required N(15,3) , Stock N(15,3)  , PoNo C(6)      ,;
*                          Vendor C(8)   , PofComplt D(8)   , QtyOS N(15,3)  , llPrint L(1)   ,;
*                          Type C(1)     , StyleMemo M(10))
CREATE CURSOR (lcHeader) (Sorder C(6)   , Btname C(30)     , StartDat D(8)  , CompltDat D(8) ,;
                          DocketNo C(6) , Styles C(12)     , Material C(19) , BomDesc C(20)  ,;
                          BomClr C(6)   , Required N(15,3) , Stock N(15,3)  , PoNo C(6)      ,;
                          Vendor C(8)   , PofComplt D(8)   , QtyOS N(15,3)  , llPrint L(1)   ,;
                          Type C(1)     , StyleMemo M(10) , nQtyOrd N(11,3) , nQtyRec N(11,3))
*C200230,1 Add 2 fields to be added to report. [End]

ZAP

*C200230,1 Make new index on Temp file. [Begin]
*B605855,1 TMI [START] Add the "PoNo" field to the index "MatColor"
*INDEX ON Sorder + Material + BomClr TAG MatColor OF (gcWorkDir+lcHeader)
INDEX ON Sorder + Material + BomClr + PoNo TAG MatColor OF (gcWorkDir+lcHeader)
*B605855,1 TMI [END  ] Add the "PoNo" field to the index "MatColor"
*C200230,1 Make new index on Temp file. [End]

*B605855,4 TMI [START] Add fabric and fabric-color to the index sorder
*INDEX ON Sorder + Type TAG Sorder OF (gcWorkDir+lcHeader)
INDEX ON Sorder + Type + Material + BomClr + PoNo TAG Sorder OF (gcWorkDir+lcHeader)
*B605855,4 TMI [END  ] Add fabric and fabric-color to the index sorder
INDEX ON Sorder + Material + Type TAG lcMatral OF (gcWorkDir+lcHeader)
*--End of lfCreatTmp.

*!*************************************************************
*! Name      : lfTmpIndex
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to creat temp. index for the cutpick file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTmpIndex

SELECT CUTPICK
lcOrderPik = ORDER()
IF !FILE(gcWorkDir + lcCutPick + '.CDX')
  INDEX ON TRANCD+ORDER+STYLE TAG cStyle OF (gcWorkDir + lcCutPick + '.CDX')
ELSE
  SET ORDER TO TAG cStyle
ENDIF

*--End of lfTmpIndex.

*!*************************************************************
*! Name      : lfRelation
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to create the relations of the program.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRelation

SELECT ORDHDR
SET RELATION TO "O" + ORDHDR.ORDER INTO ORDLINE ADDITIVE
SET RELATION TO IIF(ORDHDR.STORE = SPACE(8),'M'+ ORDHDR.ACCOUNT,;
                'S'+ ORDHDR.ACCOUNT+ORDHDR.STORE) INTO CUSTOMER ADDITIVE
SELECT ORDLINE
SET RELATION TO "2" + ORDHDR.ORDER + LEFT(STYLE,12) INTO CUTPICK ADDITIVE

SELECT MAPOALO
IF !('"P"+MAPOALO.POMAT' $ SET('RELATION'))
  SET RELATION TO "P"+MAPOALO.POMAT INTO POFHDR ADDITIVE
  SET RELATION TO MAPOALO.FABRIC + MAPOALO.COLOR + "P" + MAPOALO.POMAT INTO POFLN ADDITIVE
ENDIF  

SELECT CUTPICK
SET RELATION TO "I"+ CUTPICK.ctktno INTO CTKTBOM ADDITIVE

*--End of lfRelation.

*!*************************************************************
*! Name      : lfcolctDat
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to Collect the date.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat

lcAlias = ALIAS()
STORE SPACE(0) TO lcStyMajEn , lcOrderEn , lcStBomItm , lcStyNnMaj
STORE .T. TO llChngOrd , llPrnLin , llDeltRec

*C200230,1 Add a variable for saving tag name. [Begin]
PRIVATE lcTagName , lcFileName
STORE SPACE(0) TO lcTagName , lcFileName
*C200230,1 Add a variable for saving tag name. [End]

*B604480,1 Go top in Material PO Custom file. [Begin]
SELECT MAPOALO
LOCATE
*B604480,1 Go top in Material PO Custom file. [End]

SELECT ORDHDR
SCAN FOR &lcRpexp
  SELECT ORDLINE  
  SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = "O" + ORDHDR.ORDER
    WAIT WINDOW "Collecting data for order# : " + Order NOWAIT
    *--Case the sales order has many lines with the same so #.
    IF lcOrderEn # ORDLINE.ORDER
      *--Case the sales order has header and didn't has lines,we must delete the header record.
      IF llDeltRec AND !EMPTY(lcOrderEn)
        IF SEEK(lcOrderEn+SPACE(19)+'H' ,lcHeader)
          lcAlasBDl = ALIAS()
          SELECT (lcHeader)
          DELETE
          SELECT (lcAlasBDl)
        ENDIF
      ENDIF
      STORE .T. TO llChngOrd , llPrnLin , llDeltRec
      STORE SPACE(0) TO lcStyMajEn , lcStyNnMaj
    ENDIF
    *--Section for insert the records of the header lines in the header Temp. file.
    IF lcStyMajEn # LEFT(ORDLINE.STYLE,12)
      IF llChngOrd
        *B605666,1 RAE [START] Print only SO's which assigned to PO.
        IF SEEK(ORDHDR.ORDER,'MAPOALO')
          *B605666,1 RAE [END]
          INSERT INTO (lcHeader) (Sorder , Btname , StartDat , CompltDat , Type , Styles , llPrint);
                                 VALUES ;
                                 (ORDHDR.ORDER , CUSTOMER.BTNAME , ORDHDR.START , ORDHDR.COMPLETE , 'H' ,;
                                  LEFT(ORDLINE.STYLE,12) , llChngOrd)
          REPLACE &lcHeader..DocketNo WITH IIF(EOF('CUTPICK') , SPACE(1) , CUTPICK.CTKTNO)
          lcMmoStl = LEFT(ORDLINE.STYLE,12)
          REPLACE &lcHeader..StyleMemo WITH lcMmoStl ADDITIVE
          llChngOrd = .F.
        ENDIF
      ELSE
        *--Case the sales order has many styles.
        lcRecNo1 = RECNO(lcHeader)
        IF SEEK(ORDHDR.ORDER+SPACE(19)+'H' ,lcHeader)
          REPLACE &lcHeader..StyleMemo WITH ' / ' + LEFT(ORDLINE.STYLE,12) ADDITIVE
        ENDIF
        IF BETWEEN(lcRecNo1,1,RECCOUNT(lcHeader))
          GOTO lcRecNo1 IN (lcHeader)
        ELSE
          GO BOTT
        ENDIF      
      ENDIF
    ENDIF
    lcStyNnMaj = SUBSTR(ORDLINE.STYLE,14,6)
    *--Function to collect the lines.
    =lfColLine()
    lcStyMajEn = LEFT(ORDLINE.STYLE,12)
    lcOrderEn  = ORDLINE.ORDER
  ENDSCAN
ENDSCAN

*--Case the sales order has header and didn't has lines,
*--we must delete the header record for the last record 
*--because in this case we get out of the scan.
IF llDeltRec
  IF SEEK(lcOrderEn+SPACE(19)+'H' ,lcHeader)
    SELECT (lcHeader)
    DELETE
  ENDIF
ENDIF

SELECT(lcAlias)

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfColLine
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to Collect the lines.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColLine

STORE SPACE(0) TO LcBomItem , lnStockEn, lcIClr
lcAlias = ALIAS()

*B605855,1 TMI [START] Comment This fuction and rewrite its contents.
*SELECT BOM
*IF SEEK(PADR(LEFT(ORDLINE.STYLE,12),19,' '))
*  *B604480,1 Include the Trims. [Begin]
*  *SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = PADR(LEFT(ORDLINE.STYLE,12),19,' ') FOR ccatgtyp = 'F'
*  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = PADR(LEFT(ORDLINE.STYLE,12),19,' ') FOR ccatgtyp $ 'FT'
*  *B604480,1 Include the Trims. [End]
*    LcBomItem = BOM.ITEM
*    lcIClr    = Bom.Iclr
*    IF SUBSTR(BOM.cItmmask,14,6) = "******"
*      *--The condition of the CTKTBOM.issue_qty = 0 
*      *--if no didn't get this line and if there is no cutpick record we get the lines.
*      *B605666,1 RAE [START]
*      *IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+ SUBSTR(ORDLINE.STYLE,14,6) ,;
*         'CTKTBOM') .AND. CTKTBOM.issue_qty = 0 ) .OR. EOF('CUTPICK') .OR. !EOF('CUTPICK')
*         
*      IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+ lcIClr,;
*         'CTKTBOM') .AND. CTKTBOM.issue_qty = 0) .OR. EMPTY(CUTPICK.ctktno)
*      *B605666,1 RAE [END]
*        LcBomItem = BOM.ITEM
*        lcIClr    = Bom.Iclr
*        *C200230,1 Check for existance of this line in Temp file. [Begin]
*        *IF !SEEK(ORDHDR.ORDER + BOM.ITEM , lcHeader ) .OR. lcStyNnMaj # &lcHeader..BomClr
*        lcFileName = ALIAS()
*        SELECT (lcHeader)
*        lcTagName = TAG()
*        SET ORDER TO TAG MatColor
*        IF !SEEK(OrdHdr.Order+Bom.Item + Bom.IClr) AND (!SEEK(ORDHDR.ORDER + BOM.ITEM , lcHeader ) .OR. lcStyNnMaj # &lcHeader..BomClr)
*          SET ORDER TO TAG (lcTagName)
*          *C200230,1 Check for existance of this line in Temp file. [End]
*          
*          *B605666,1 RAE [START] don't Reporting line for SO if it hasn't a record in MAPOALO.
*          *INSERT INTO (lcHeader) (Sorder   , StartDat , Type , Material , BomDesc , BomClr ,;
*                                  Required  , Stock    , llPrint);
*                                  VALUES ;
*                              (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , Bom.Desc ,;
*                             IIF(Bom.IClr = "******" , SUBSTR(ORDLINE.STYLE,14,6) , Bom.IClr),;
*                                  lfRequred() , lfStock() , llPrnLin)
*          *B605666,1 RAE [END]
*          
*          *C200230,1 Remove the condition of Stock Qty equal zero. [Begin]
*          *IF lnStockEn = 0
*          *C200230,1 Remove the condition of Stock Qty equal zero. [End]
*  
*          *C102500,4 03/01/2001 Go to the correct record   [Start]
*          *GOTO TOP IN MAPOALO
*          *IF !EOF('MAPOALO')
*
*          *B604480,1 Seek in Material Purchase Line. [Begin]
*          *IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "MAPOALO")
*          *IF EOF('MAPOALO')
*          *=SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) , 'MAPOALO')
*          =SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + lcIClr , 'MAPOALO')
*          
*          *ENDIF
*          
*          *B605666,1 RAE [START] Get all PO's for the same material/color which exist for the SO in MAPOALO.
*          SELECT MAPOALO
*          *SCAN REST WHILE order+fabric+color = ORDHDR.ORDER + ;
*                          LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6)               
*
*          SCAN REST WHILE order+fabric+color = ORDHDR.ORDER + ;
*                          LEFT(LcBomItem,7) + lcIClr
*
*
*            INSERT INTO (lcHeader) (Sorder, StartDat , Type , Material , BomDesc , BomClr ,;
*                                    Required , Stock , llPrint);
*                                    VALUES ;
*                                    (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , Bom.Desc , ;
*                                    IIF(Bom.IClr = "******" , lcIClr , Bom.IClr),;
*                                    lfRequred() , lfStock() , llPrnLin)                          
*            *B605666,1 RAE [END]
*
*            *C200230,1 Reject cancelled PO. [Begin]
*            *IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "POFLN") 
*            IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "POFLN") AND PoFHdr.Status <> "X"
*              =SEEK(MAPOALO.FABRIC + MAPOALO.COLOR,'Fabric')
*              *C200230,1 Reject cancelled PO. [End]
*            
*              *B604480,1 Seek in Material Purchase Line. [End]
*
*              *C102500,4 03/01/2001 Go to the correct record   [End]
*            
*              *B605666,1 RAE [START] Stop lfPoNo() function.
*              *REPLACE &lcHeader..PoNo      WITH lfPoNo()          ,;
*                       &lcHeader..Vendor    WITH POFHDR.VENDOR     ,;
*                       &lcHeader..PofComplt WITH POFHDR.COMPLETE   ,;
*                       &lcHeader..QtyOS     WITH IIF(POFLN.nfabtotqty > 0 , lfOtoS() , 0)
*                      
*              REPLACE &lcHeader..PoNo      WITH MAPOALO.PoMat     ,;
*                      &lcHeader..Vendor    WITH POFHDR.VENDOR     ,;
*                      &lcHeader..PofComplt WITH POFHDR.COMPLETE   ,;
*                      &lcHeader..QtyOS     WITH IIF(POFLN.nfabtotqty > 0 , lfOtoS() , 0)
*              *B605666,1 RAE [END]
*            ENDIF
*
*            *C200230,1 Remove the condition of Stock Qty equal zero. [Begin]            
*            *ENDIF
*            *C200230,1 Remove the condition of Stock Qty equal zero. [End]
*                    
*            llPrnLin  = .F.
*            llDeltRec = .F.
*          *B605666,1 RAE [START] End SCAN.
*          ENDSCAN
*          llPrnLin  = .F.
*          llDeltRec = .F.
*          *B605666,1 RAE [END]
*        ELSE
*          REPLACE &lcHeader..Required WITH &lcHeader..Required + lfRequred()
*        ENDIF
*        
*        *C200230,1 Restore settings. [Begin]
*        SELECT (lcFileName)
*        *C200230,1 Restore settings. [End]
*      *ENDIF  
*      ENDIF
*    ELSE
*
*      *--The condition of the CTKTBOM.issue_qty = 0 
*      *--if no didn't get this line and if there is no cutpick record we get the lines.
*      *B605666,1 RAE [START]
*      LcBomItem = BOM.ITEM
*      lcIClr    = Bom.Iclr
*      IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+;
*         lcIClr , 'CTKTBOM') .AND. CTKTBOM.issue_qty = 0 ).OR. EMPTY(CUTPICK.ctktno)
*
*      *IF (SEEK('I'+CUTPICK.ctktno+PADR(LEFT(BOM.ITEM,12),19,' ')+;
*         SUBSTR(ORDLINE.STYLE,14,6) , 'CTKTBOM') .AND. CTKTBOM.issue_qty = 0 )
*       *B605666,1 RAE [END]
*        LcBomItem = BOM.ITEM
*        lcIClr    = Bom.Iclr
*        IF lcStyNnMaj = SUBSTR(BOM.cItmmask,14,6))
*                   
*          *C200230,1 Check for existance of this line in Temp file. [Begin]
*          *IF !SEEK(ORDHDR.ORDER + BOM.ITEM , lcHeader ) .OR. (lcStyNnMaj = SUBSTR(BOM.cItmmask,14,6))
*          lcFileName = ALIAS()
*          SELECT (lcHeader)
*          lcTagName = TAG()
*          SET ORDER TO TAG MatColor
*          
*          *B605666,1 RAE [START]
*          *IF !SEEK(OrdHdr.Order+Bom.Item + Bom.IClr) AND !SEEK(ORDHDR.ORDER + BOM.ITEM , lcHeader ) .OR. (lcStyNnMaj # SUBSTR(BOM.cItmmask,14,6))
*          IF !SEEK(OrdHdr.Order+Bom.Item + Bom.IClr) AND !SEEK(ORDHDR.ORDER + BOM.ITEM +BOM.IClr, lcHeader ) .OR. (lcStyNnMaj # SUBSTR(BOM.cItmmask,14,6))
*          *B605666,1 RAE [END]
*          
*            SET ORDER TO TAG (lcTagName)
*            *C200230,1 Check for existance of this line in Temp file. [End]
*            
*            *B605666,1 RAE [START] Reporting line for SO if it hasn't a record in MAPOALO.
*            *INSERT INTO (lcHeader) (Sorder   , StartDat , Type , Material , BomDesc , BomClr ,;
*                                    Required , Stock    , llPrint);
*                                    VALUES ;
*                                    (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , Bom.Desc , Bom.IClr ,;
*                                    lfRequred() , lfStock() , llPrnLin)
*            *B605666,1 RAE [END]
*
*            *C200230,1 Remove the condition of Stock Qty equal zero. [Begin]
*            *IF lnStockEn = 0
*            *C200230,1 Remove the condition of Stock Qty equal zero. [End]
*            
*            *C102500,4 03/01/2001 Go to the correct record   [Start]
*            *GOTO TOP IN MAPOALO
*            *IF !EOF('MAPOALO')
*            
*            *B604480,1 Seek in Material Purchase Line. [Begin]
*            *IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "MAPOALO")
*            *IF EOF('MAPOALO')
*            *=SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) , 'MAPOALO')
*            =SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + lcIClr , 'MAPOALO')
*            
*            *ENDIF
*          
*            *B605666,1 RAE [START] Get all PO's for the same material/color which exist for the SO in MAPOALO.
*            SELECT MAPOALO
*            *SCAN REST WHILE order+fabric+color = ORDHDR.ORDER + ;
*                            LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6)               
*
*            SCAN REST WHILE order+fabric+color = ORDHDR.ORDER + ;
*                            LEFT(LcBomItem,7) + lcIClr
*
*
*              INSERT INTO (lcHeader) (Sorder, StartDat , Type , Material , BomDesc , BomClr ,;
*                                      Required , Stock , llPrint);
*                                      VALUES ;
*                                      (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , Bom.Desc , ;
*                                      IIF(Bom.IClr = "******" , lcIClr , Bom.IClr),;
*                                      lfRequred() , lfStock() , llPrnLin)                          
*            *B605666,1 RAE [END]
*
*
*            *C200230,1 Reject cancelled PO. [Begin]
*            *IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "POFLN")
*              IF SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT, "POFLN") AND PoFHdr.Status <> "X"
*                =SEEK(MAPOALO.FABRIC + MAPOALO.COLOR,'Fabric')
*                *C200230,1 Reject cancelled PO. [End]
*
*                *B604480,1 Seek in Material Purchase Line. [End]
*          
*                *C102500,4 03/01/2001 Go to the correct record   [End]
*              
*                *B605666,1 RAE [START] Stop lfPoNo() function.
*                *REPLACE &lcHeader..PoNo      WITH lfPoNo()          ,;
*                         &lcHeader..Vendor    WITH POFHDR.VENDOR     ,;
*                         &lcHeader..PofComplt WITH POFHDR.COMPLETE   ,;
*                         &lcHeader..QtyOS     WITH IIF(POFLN.TOTQTY > 0 , lfOtoS() , 0)
*                REPLACE &lcHeader..PoNo      WITH MAPOALO.PoMat     ,;
*                        &lcHeader..Vendor    WITH POFHDR.VENDOR     ,;
*                        &lcHeader..PofComplt WITH POFHDR.COMPLETE   ,;
*                        &lcHeader..QtyOS     WITH IIF(POFLN.nfabtotqty > 0 , lfOtoS() , 0)
*                *B605666,1 RAE [END]
*
*              ENDIF
*
*              *C200230,1 Remove the condition of Stock Qty equal zero. [Begin]
*              SELECT (lcHeader)
*              SET ORDER TO TAG (lcTagName)
*              SELECT (lcFileName)
*              *ENDIF
*              *C200230,1 Remove the condition of Stock Qty equal zero. [End]
*
*              llPrnLin  = .F.
*              llDeltRec = .F.
*            *B605666,1 RAE [START] End SCAN
*            ENDSCAN
*            llPrnLin  = .F.
*            llDeltRec = .F.
*            *B605666,1 RAE [END]
*          ELSE
*            IF lcStyNnMaj  = SUBSTR(BOM.cItmmask,14,6)
*              lcTmpSo = SORDER
*              lcTmpSty = Styles
*              lcFabric = Material
*              lcbomclr = bomclr
*              REPLACE &lcHeader..Required WITH &lcHeader..Required + lfRequred();
*               FOR SORDER+Styles+Material+bomclr = lcTmpSo+lcTmpSty+lcFabric+lcbomclr
*              LOCATE FOR SORDER+Styles+Material+bomclr = lcTmpSo+lcTmpSty+lcFabric+lcbomclr
*            ENDIF
*          ENDIF
*         
*          *C200230,1 Restore settings. [Begin]
*          SELECT (lcFileName)
*          *C200230,1 Restore settings. [End]
*        ENDIF
*      ENDIF
*    ENDIF
*  ENDSCAN
*ENDIF
*
*SELECT (lcAlias)

*-- Start here tmi modifications B605855,1
SELECT (lcHeader)
lcTagName = TAG()
SET ORDER TO TAG MatColor IN (lcHeader)
SELECT BOM
IF SEEK(PADR(LEFT(ORDLINE.STYLE,12),19,' ') , 'BOM' )
  SCAN REST WHILE citmmajor+typ+citmmask+mfgcode+item+iclr = PADR(LEFT(ORDLINE.STYLE,12),19,' ') ;
            FOR ccatgtyp $ 'FT'

    DO CASE
      CASE SUBSTR(citmmask,14,6) = "******"  AND Bom.IClr = "******"     && Same AS
           lcIClr = SUBSTR(ORDLINE.STYLE,14,6) 
           
      CASE SUBSTR(citmmask,14,6) = "******"  AND Bom.IClr # "******"     && Constant
           lcIClr = Bom.IClr
                  
      CASE SUBSTR(citmmask,14,6) # "******"  AND Bom.IClr # "******"     && Different 
           IF SUBSTR(ORDLINE.STYLE,14,6)  <> SUBSTR(citmmask,14,6)
             LOOP
           ENDIF 
           lcIClr = Bom.IClr
    ENDCASE

    LcBomItem = BOM.ITEM

    m.Requred = IIF(!SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + lcIClr , 'MAPOALO') ,lfRequred(),0)
    IF !SEEK(ORDHDR.ORDER+BOM.ITEM+lcIClr+MAPOALO.POMAT,lcHeader)
      =SEEK(LEFT(BOM.ITEM,7)+lcIClr,'Fabric')
      llPrnLin  = .F. 
      INSERT INTO (lcHeader) (Sorder, StartDat , Type , Material , BomDesc , ;
                              BomClr , Required , Stock ,;
                              PoNo,Vendor,PofComplt,llPrint);
                  VALUES ;
                             (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , BOM.Desc , ;
                              lcIClr, m.Requred , lfStock() ,;
                              MAPOALO.PoMat,POFHDR.VENDOR,POFHDR.COMPLETE,llPrnLin)                              
      REPLACE &lcHeader..QtyOS WITH IIF( POFLN.nfabtotqty > 0 , lfOtoS() , 0 )      
    ELSE    
      REPLACE &lcHeader..Required WITH &lcHeader..Required + m.Requred
    ENDIF
    
    SELECT MAPOALO
    IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + lcIClr , 'MAPOALO')
      SCAN REST WHILE order+fabric+color = ORDHDR.ORDER+LEFT(LcBomItem,7)+lcIClr &&;FOR PoFHdr.Status <> "X"
    
        IF !SEEK(ORDHDR.ORDER+BOM.ITEM+lcIClr+MAPOALO.POMAT,lcHeader)
          =SEEK(MAPOALO.FABRIC+MAPOALO.COLOR,'Fabric')
          llPrnLin  = .F. && TMI *B605855,4
          INSERT INTO (lcHeader) (Sorder, StartDat , Type , Material , BomDesc , ;
                                  BomClr , Required , Stock ,;
                                  PoNo,Vendor,PofComplt,llPrint);
                      VALUES ;
                                 (ORDHDR.ORDER , ORDHDR.START ,  'L' , BOM.ITEM , BOM.Desc , ;
                                  lcIClr, lfRequred() , lfStock() ,;
                                  MAPOALO.PoMat,POFHDR.VENDOR,POFHDR.COMPLETE,llPrnLin)
                                  
          REPLACE &lcHeader..QtyOS WITH IIF( POFLN.nfabtotqty > 0 , lfOtoS() , 0 )
          
        ELSE
        
          REPLACE &lcHeader..Required WITH &lcHeader..Required + lfRequred()
          
        ENDIF
      ENDSCAN
      llPrnLin  = .F.
      llDeltRec = .F.
    ENDIF
  ENDSCAN
ENDIF

SET ORDER TO TAG &lcTagName IN (lcHeader)
SELECT (lcAlias)
*B605855,1 TMI [END  ]
*--End of lfColLine.

*!*************************************************************
*! Name      : lfRequred
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to collect the Required quantity.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfRequred

STORE 0 TO lnReqedEn
lcAlias = ALIAS()
*B605855,1 TMI [START] Get the required quantity
* IF EOF('CUTPICK')
*   lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
* ELSE
*   IF EOF('CTKTBOM')
*    lnReqedEn = ORDLINE.TOTQTY * BOM.NBOMTOTQTY
*   ELSE
*     lnReqedEn = CTKTBOM.REQ_QTY
*   ENDIF
* ENDIF
*--If there is a cutpick record for the sales order then print from the cutpick only where no issued
*  quantities, if there is no cutpick record for the sales order then calculate quanity required
*  from the style bom records relating to the sales order
PRIVATE lnCount,lcCnt
IF !EOF('CUTPICK') AND SEEK('I'+CUTPICK.ctktno+BOM.ITEM+lcIClr,'CTKTBOM')
  IF CTKTBOM.issue_qty = 0
    lnReqedEn = lfSzTot( CTKTBOM.UNTQTY )
  ENDIF  
ELSE
  lnReqedEn = lfSzTot( BOM.NBOMTOTQTY )
ENDIF
*B605855,1 TMI [END  ] Get the required quantity
SELECT (lcAlias)
RETURN lnReqedEn

*--End of lfRequred.
*:**************************************************************************
*:* Name        : lfSzTot
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/16/2002
*:* Purpose     : Get requirement based on sizes
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSzTot()
*:* Refer To    : B605749,4
*:***************************************************************************
FUNCTION lfSzTot
PARAMETERS lnUntQty
PRIVATE lnReqedEn
lnReqedEn = 0
IF BOM.LBASONSIZ 
  FOR lnCount = 1 TO 8
    lcCnt = STR(lnCount,1)
    lnReqedEn = lnReqedEn + IIF( lcCnt $ BOM.MSIZES , ORDLINE.QTY&lcCnt , 0 ) * lnUntQty
  ENDFOR
ELSE
  lnReqedEn = ORDLINE.TOTQTY*BOM.NBOMTOTQTY    
ENDIF
RETURN lnReqedEn
*-- end of lfSzTot.

*!*************************************************************
*! Name      : lfStock
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to collect the Stock quantity.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfStock

STORE 0 TO lnStockEn
lcAlias = ALIAS()

SELECT FABRIC
*IF SEEK(LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) , 'FABRIC' )
IF SEEK(LEFT(LcBomItem,7) + lcIClr , 'FABRIC' )


  *SCAN REST WHILE FABRIC+COLOR = LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6)
  SCAN REST WHILE FABRIC+COLOR = LEFT(LcBomItem,7) + lcIClr
    lnStockEn = lnStockEn + FABRIC.ONHAND
  ENDSCAN
ENDIF

SELECT (lcAlias)
RETURN lnStockEn

*--End of lfStock.
*!*************************************************************
*! Name      : lfPoNo
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to get the PoNo from the custome file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*B605666,1 RAE [START] Stop lfPoNo() function.
*FUNCTION lfPoNo

*STORE SPACE(0) TO lcPoNoEn
*lcAlias = ALIAS()

*SELECT MAPOALO
*IF SEEK(ORDHDR.ORDER + LEFT(LcBomItem,7) + SUBSTR(ORDLINE.STYLE,14,6) , 'MAPOALO')
*  lcPoNoEn = MAPOALO.PoMat
*ENDIF

*SELECT (lcAlias)
*RETURN lcPoNoEn
*B605666,1 RAE [END]

*--End of lfPoNo.
*!*************************************************************
*! Name      : lfOtoS
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to get open to sell.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!C102500,4 03/01/2001 Go to the correct record   [Start]
FUNCTION lfOtoS

lcAlias = ALIAS()
STORE 0 TO lnPoOtoS
SELECT POFLN
=SEEK(MAPOALO.FABRIC + MAPOALO.COLOR + "P" +  MAPOALO.POMAT)
SCAN REST WHILE fabric+color+cmattype+pomat+trancd = MAPOALO.FABRIC + MAPOALO.COLOR + "P" + MAPOALO.POMAT
  IF Trancd = '1'
    lnPoOtoS = lnPoOtoS + POFLN.nfabtotqty
    
    *C200230,1 Get the quantity ordered. [Begin]
    REPLACE &lcHeader..nQtyOrd WITH &lcHeader..nQtyOrd + (PoFLn.nfabtotqty * Fabric.Conv) 
    *C200230,1 Get the quantity ordered. [End]

  ELSE
    IF Trancd = '2'
      lnPoOtoS = lnPoOtoS - POFLN.nfabtotqty

      *C200230,1 Get the quantity ordered. [Begin]
      REPLACE &lcHeader..nQtyRec WITH &lcHeader..nQtyRec + (PoFLn.nfabtotqty * Fabric.Conv)
      *C200230,1 Get the quantity ordered. [End]

    ENDIF
  ENDIF
ENDSCAN

SELECT (lcAlias)
RETURN lnPoOtoS

*--End of lfOtoS.
*!*************************************************************
*! Name      : lfPrnDate
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function to cut the date into variables to print.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfPrnDate

lnDatePos = ASCAN(LAOGFXFLT,'ORDHDR.START')
IF lnDatePos > 0
  lnInd    = ASUBSCRIPT(LAOGFXFLT,ASCAN(LAOGFXFLT,'ORDHDR.START'),1)
  ldFrDate = ALLTRIM(SUBSTR(LAOGFXFLT[lnInd,6],1,10))
  ldToDate = ALLTRIM(SUBSTR(LAOGFXFLT[lnInd,6],12,20))
ENDIF

*--End of lfPrnDate.
*!**************************************************************************
*! Name      : lfClearRep
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 02/19/2001
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : The program
*!**************************************************************************
*! Calls     : None()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
FUNCTION lfClearRep

IF FILE(gcWorkDir + lcCutPick + '.CDX')
  SELECT CUTPICK
  CLOSE INDEX
  ERASE (gcWorkDir + lcCutPick + '.CDX')
ENDIF

SELECT CUTPICK
SET ORDER TO &lcOrderPik

IF USED('MAPOALO')
  USE IN MAPOALO
ENDIF

*--End of lfClearRep.



*:**************************************************************************
*:* Name        : lfLayout
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2002
*:* Purpose     : a function to adjust layout printing
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfLayout()
*:* Refer to    : B605855,4
*:***************************************************************************
FUNCTION lfLayout
SELECT (lcHeader)
llLine = .F.
SCAN
  DO CASE
    CASE &lcHeader..Type = 'H'
      llLine = .T.
    CASE &lcHeader..Type = 'L' AND llLine = .T.
      REPLACE lLPrint WITH .T.
      llLine = .F.
  ENDCASE
ENDSCAN
*-- end of lfLayout.