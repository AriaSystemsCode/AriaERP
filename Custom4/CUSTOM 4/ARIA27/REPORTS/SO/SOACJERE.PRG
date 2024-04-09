*:***************************************************************************
*: Program file  : SOACJERE.PRG
*: Program desc. : Unallocated Report For NORTH BAY APPAREL.
*: For Report    : SOACJERE.FRX
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : BASSEM RAAFAT ERNEST(BWA)
*: Date          : 11/02/2004
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOACJERE
*:***************************************************************************
*: This Report Program is due to CP124444 ...
*:***************************************************************************
*:Modifications :
*:B126351,1 NNA 02/16/2005 Make some modifications as follow :
*:B126351,1 NNA            1 - Change (Order completion date) option from Between to Less or Equal
*:B126351,1 NNA            2 - Print Sizes in the first of every page and if scale changed
*:B126351,1 NNA            3 - print a separate page for every Division/Seles Rep.
*:B126351,1 NNA            4 - Print only Orders that have Reasons (Problem) 
*:***************************************************************************

*--Initialize the variables.
STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl , lnNetStk
=lfChkStrct()
lcTime     =  gfGetTime()
ldCompDate = laOgFxFlt[ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.COMPLETE'),1),6]

*--Reoprn the Ordline file with another name to scan in it to get the Net stock.
IF !USED(lcOrdlnet)
  =gfOpenFile(gcDataDir+"ORDLINE","ORDLINES",'SH', @lcOrdlnet, .T.)
ENDIF

=lfCreatTmp()  && Create work cursor.
=lfRelatons()  && Create the used relation.
=lfCollData()  && Collect the data.

SELECT ORDLINE
SET RELATION OFF INTO SCALE

SELECT (lcOrdlins)

*B126351,1 NNA 02/16/2005 (Begin) Set order to Division+Sales Rep.+Account + Order +style
IF lcRpSortBy ='D'
  SET ORDER TO TAG DIVSREP
ENDIF
IF lcRpSortBy ='S'
  SET ORDER TO TAG SREPDIV
ENDIF
IF lcRpSortBy ='O'
  SET ORDER TO TAG AccOrd
ENDIF

*--Delete records that doesn't have Reason
DELETE ALL FOR ResonFld=SPACE(4) OR 'EMPTY' $ STYLFLD
PACK
IF RECCOUNT(lcOrdlins) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ELSE
  LOCATE
  =lfPrntSiz()
ENDIF
SET RELATION TO
=lfResons()
SET RELATION TO SRP1COD INTO Salesrep ADDITIVE
*B126351,1 NNA (End)

SET RELATION TO "S" + SclFld INTO SCALE ADDITIVE
LOCATE
DO gfDispRe WITH EVAL('lcRpForm')
WAIT CLEAR

SELECT(lcTmpOHdr)
SET RELATION OFF INTO ORDHDR

SELECT (lcOrdlins)
SET RELATION OFF INTO SCALE

                 *--Section for the functions of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Create Temp. file that hold the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : Report code
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp

PRIVATE lcAlias

lcAlias = ALIAS()
*B126351,1 NNA 02/16/2005 (Begin) Add new Field (llPrntSiz) to use it in printing Sizes or not
*:B126351,1 NNA           and add New Index to the temp file
*CREATE CURSOR (lcOrdlins) (StylFld C(lnStyLnGl) , ClrFld C(lnClrLnGl) , SclFld C(lnScaLnGl) , Enterdat D ,;
                           StartDat D     , CmpltDat D   , OrderNo C(6) , CustName C(18) , CustCod C(6)   , StoreNo C(8)   ,;
                           CustPo C(15)   , Srp1Cod C(3) , TermCod C(6) ,  TotQty N(7)   , PricFld N(6,2) , SizCde C(1)    ,;
                           Qty1 N(6)      , Qty2 N(6)    , Qty3 N(6)    , Qty4 N(6)      , Qty5 N(6)      , Qty6 N(6)      ,;
                           Qty7 N(6)      , Qty8 N(6)    , EndDate D    , ResonFld C(4)  , SesonCod C(6)  , DvsonCod C(6)  ,;
                           SesonFld C(30) , DvsonFld C(30))
*INDEX ON CustCod + OrderNo + DTOS(StartDat) TAG AccOrd of (gcWorkDir + lcOrdlins)
CREATE TABLE (gcWorkDir + lcOrdlins) (StylFld C(lnStyLnGl) , ClrFld C(lnClrLnGl) , SclFld C(lnScaLnGl) , Enterdat D ,;
                           StartDat D     , CmpltDat D   , OrderNo C(6) , CustName C(18) , CustCod C(6)   , StoreNo C(8)   ,;
                           CustPo C(15)   , Srp1Cod C(3) , TermCod C(6) ,  TotQty N(7)   , PricFld N(6,2) , SizCde C(1)    ,;
                           Qty1 N(6)      , Qty2 N(6)    , Qty3 N(6)    , Qty4 N(6)      , Qty5 N(6)      , Qty6 N(6)      ,;
                           Qty7 N(6)      , Qty8 N(6)    , EndDate D    , ResonFld C(8)  , SesonCod C(6)  , DvsonCod C(6)  ,;
                           SesonFld C(30) , DvsonFld C(30),llPrntSiz L(1))

INDEX ON Srp1Cod + DvsonCod + CustCod + OrderNo + StylFld TAG SREPDIV of (lcOrdlins)
INDEX ON CustCod + OrderNo + DTOS(StartDat) TAG AccOrd of (lcOrdlins)
INDEX ON DvsonCod + Srp1Cod + CustCod + OrderNo + StylFld TAG DIVSREP of (lcOrdlins)
SET ORDER TO TAG ACCORD
*B126351,1 NNA (End)

*--Create the Temp. OrdHdr file to be able to index it with the Entered Date.
IF !USED(lcTmpOHdr)
  =gfOpenFile(gcDataDir+"ORDHDR","ORDHDR",'SH', @lcTmpOHdr, .T.)
ENDIF

SELECT(lcTmpOHdr)
INDEX ON DTOS(ENTERED) + CORDTYPE + ORDER TAG EnterDate OF (gcWorkDir + lcTmpOHdr + '.CDX')


SELECT(lcAlias)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfRelatons
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Function to create the used relation.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfRelatons()
*!*************************************************************
FUNCTION lfRelatons

SELECT POSLN
SET RELATION TO POSLN.CSTYTYPE + POSLN.PO INTO POSHDR ADDITIVE

SELECT(lcTmpOHdr)
*B126351,1 NNA 02/16/2005 (Begin) Empty the Relation if found
SET RELATION TO
*B126351,1 NNA (End)
SET RELATION TO "O" + EVAL(lcTmpOHdr+'.ORDER') INTO ORDHDR ADDITIVE

SELECT ORDHDR
*B126351,1 NNA 02/16/2005 (Begin) Empty the Relation if found
SET RELATION TO
*B126351,1 NNA (End)
SET RELATION TO "O" + ORDHDR.ORDER INTO ORDLINE ADDITIVE

SELECT ORDLINE
*B126351,1 NNA 02/16/2005 (Begin) Empty the Relation if found
SET RELATION TO 
*B126351,1 NNA (End)
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO "S" + SCALE INTO SCALE ADDITIVE
SET RELATION TO "B" + ORDER INTO NOTEPAD ADDITIVE
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                        'S' + Account + Store) INTO CUSTOMER ADDITIVE

*--End of lfRelatons.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Function to collect the data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfCollData()
*!*************************************************************
FUNCTION lfCollData

PRIVATE lcAlias , lcRsnNmbrs , lcDivision , lcSeason , lcOrdChng , ldEndDate
PRIVATE llAddInfo , llEntrSacn , llRemov3 , AddRcrds

DECLARE laStylInfo[1,2]
lcAlias = ALIAS()
lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)

lcOrdChng = SPACE(0)
STORE .F. TO llAddInfo , llEntrSacn , llRemov3
STORE .T. TO AddRcrds
ldEndDate = {//}
lnCounter = 0
SELECT(lcTmpOHdr)
SCAN FOR &lcRpExp

  SELECT ORDLINE
  SCAN REST WHILE cordtype+order+STR(lineno,6) = "O" + ORDHDR.ORDER
    
    IF (ORDLINE.TOTQTY - ORDLINE.TOTPIK) > 0
      WAIT WINDOW 'Selecting Records For The Report ...Order(s):' + ORDLINE.ORDER NOWAIT
      IF lcOrdChng # ORDLINE.ORDER
        lcRsnNmbrs = lfRsnNmbr()
        lcDivision = gfCodDes(ORDHDR.CDIVISION,'CDIVISION')
        lcSeason   = gfCodDes(ORDHDR.SEASON,'SEASON')
        lcOrdChng  = ORDLINE.ORDER
        lnCounter  = 0
      ENDIF
      IF ASCAN(laStylInfo,ORDLINE.STYLE) > 0
        lnPos = ASUBSCRIPT(laStylInfo,ASCAN(laStylInfo,ORDLINE.STYLE),1)
        laStylInfo[lnPos,2] = laStylInfo[lnPos,2] - (ORDLINE.TOTQTY - ORDLINE.TOTPIK)
        lnCounter = lnPos
      ELSE
        lnCounter = lnCounter + 1
        DIMENSION laStylInfo[lnCounter , 2 ]
        laStylInfo[lnCounter,1] = ORDLINE.STYLE
        lnNetStk =lfNetStock()
        laStylInfo[lnCounter,2] = lnNetStk - (ORDLINE.TOTQTY - ORDLINE.TOTPIK)
        lnPos = lnCounter
      ENDIF

      IF laStylInfo[lnPos,2] < 0
        IF SEEK(ORDLINE.STYLE , 'POSLN')
          SELECT POSLN
          IF (POSHDR.STATUS $ "OHA")

            ldEndDate = POSHDR.COMPLETE
            DO CASE
              CASE (laStylInfo[lnCounter,2] + POSLN.TOTQTY > 0 AND ORDLINE.COMPLETE > ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY = 0 AND ORDLINE.COMPLETE > ldEndDate) 
                   ldEndDate = {//}
                =lfInsRecrd()
                llEntrSacn = .F.

              CASE (laStylInfo[lnCounter,2] + POSLN.TOTQTY > 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY > 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY = 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY = 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY < 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY < 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] + POSLN.TOTQTY < 0 AND ORDLINE.COMPLETE > ldEndDate)

                IF (laStylInfo[lnCounter,2] + POSLN.TOTQTY < 0 AND ORDLINE.COMPLETE > ldEndDate)
                  lcManValue = lcRsnNmbrs
                  lcRsnNmbrs = lcRsnNmbrs + "3"    &&*--Case there is no enough stock and open WIP with Date < Order date

                  *--Sort the value insert in "lcRsnNmbrs"
                  lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)

                  llRemov3 = .T.
                ENDIF

                *--Update the Qauntity.
                laStylInfo[lnCounter,2] = laStylInfo[lnCounter,2] + POSLN.TOTQTY

                *--Print the record of the order line.
                =lfInsRecrd()

                llEntrSacn = .T.
                SKIP

                *--Update the flag to not add the record of the order line after the scan.
                AddRcrds = .F.

                IF llRemov3
                  lcRsnNmbrs = lcManValue         &&*--Return the original value.
                  llRemov3 = .F.
 
                  *--Sort the value of "lcRsnNmbrs"
                  lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
                ENDIF
            ENDCASE
          ELSE
            ldEndDate = {//}
            =lfInsRecrd()
            llEntrSacn = .T.
            SKIP
          ENDIF

          IF llEntrSacn
            SCAN REST WHILE STYLE + CSTYTYPE + PO + STR(LINENO,6) + TRANCD = ORDLINE.STYLE
              ldEndDate = POSHDR.COMPLETE
              IF (POSHDR.STATUS $ "OHA")

                *--Update the Qauntity.
                laStylInfo[lnCounter,2] = laStylInfo[lnCounter,2] + POSLN.TOTQTY

                IF (laStylInfo[lnCounter,2] > 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] > 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] = 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] = 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] < 0 AND ORDLINE.COMPLETE = ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] < 0 AND ORDLINE.COMPLETE < ldEndDate) OR ;
                   (laStylInfo[lnCounter,2] < 0 AND ORDLINE.COMPLETE > ldEndDate)

                  IF (laStylInfo[lnCounter,2] < 0 AND ORDLINE.COMPLETE > ldEndDate)
                    lcManValue = lcRsnNmbrs
                    lcRsnNmbrs = lcRsnNmbrs + "3"    &&*--Case there is no enough stock and open WIP with Date < Order date

                    *--Sort the value insert in "lcRsnNmbrs"
                    lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
  
                    llRemov3 = .T.
                  ENDIF

                  llAddInfo = .T.
                  =lfInsRecrd()
                  llAddInfo = .F.

                  *--Update the flag to not add the record of the order line after the scan.
                  AddRcrds = .F.
                ELSE
                  ldEndDate = {//}
                ENDIF

                IF llRemov3
                  lcRsnNmbrs = lcManValue         &&*--Return the original value.
                  llRemov3 = .F.

                  *--Sort the value insert in "lcRsnNmbrs"
                  lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
                ENDIF
              ENDIF
            ENDSCAN

            IF AddRcrds
	          IF laStylInfo[lnCounter,2] < 0
                lcManValue = lcRsnNmbrs
                lcRsnNmbrs = lcRsnNmbrs + "3"    &&*--Case there is no enough stock and no open WIP
                *--Sort the value insert in "lcRsnNmbrs"
                lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
		      ENDIF

              llAddInfo = .T.
              =lfInsRecrd()
              llAddInfo = .F.
           
              IF laStylInfo[lnCounter,2] < 0
                lcRsnNmbrs = lcManValue         &&*--Return the original value.
                *--Sort the value of "lcRsnNmbrs"
                lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
	          ENDIF
            ENDIF
          ENDIF
        ELSE
          lcManValue = lcRsnNmbrs
          lcRsnNmbrs = lcRsnNmbrs + "3"    &&*--Case there is no enough stock and no open WIP

          *--Sort the value insert in "lcRsnNmbrs"
          lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)

          ldEndDate = {//}
          =lfInsRecrd()
          lcRsnNmbrs = lcManValue         &&*--Return the original value.

          *--Sort the value of "lcRsnNmbrs"
          lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)
        ENDIF
      ELSE
        ldEndDate = {//}
        =lfInsRecrd()
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN
SELECT (lcAlias)

*--End of lfCollData.
*!**************************************************************************
*! Name      : lfNetStock
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Get the net stock for the style.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfNetStock()
*!**************************************************************************
FUNCTION lfNetStock
PRIVATE lcAlias , lnRecNo
lcAlias = ALIAS()
STORE 0 TO lnNetStk
SELECT (lcOrdlnet)
lnRecNo = IIF(EOF(lcOrdlnet) , 0 , RECNO(lcOrdlnet))
lnNetStk = STYLE.TOTSTK
IF SEEK(ORDLINE.STYLE , lcOrdlnet )
  SCAN REST WHILE STYLE + DTOS(COMPLETE) + CORDTYPE + ORDER + STORE + STR(LINENO,6) = ORDLINE.STYLE
    lnNetStk = lnNetStk - EVAL(lcOrdlnet+'.TOTPIK')
  ENDSCAN
ENDIF
IF lnRecNo <> 0 
    GOTO lnRecNo IN (lcOrdlnet)
ENDIF
SELECT(lcAlias)
RETURN lnNetStk
*--End of lfNetStock.
*!*************************************************************
*! Name      : lfInsRecrd
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Save the data records.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : lfCollData()
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfInsRecrd()
*!*************************************************************
FUNCTION lfInsRecrd

PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcOrdlins)

IF llAddInfo
  *--Case if the End Date is empty.
  IF EVAL(lcOrdlins +'.EndDate') = {//}
    REPLACE EndDate  WITH ldEndDate  ,;
            ResonFld WITH lcRsnNmbrs
    SELECT (lcAlias)
    RETURN
  ELSE
    APPEND BLANK
  ENDIF

  *-- The field "StylFld" in this case has hard coded string because I couldn't leave the other 
  *-- fields empty due to they used in the index and I check for this value in the FRX 
  *-- and replace it with empty space.

  REPLACE CustCod  WITH ORDHDR.ACCOUNT ,;
          OrderNo  WITH ORDHDR.ORDER   ,;
          StartDat WITH ORDHDR.START   ,;
          TotQty   WITH POSLN.TOTQTY   ,;
          StylFld  WITH "EMPTYLINE"    ,;
          ResonFld WITH lcRsnNmbrs     ,;
          EndDate  WITH ldEndDate

  SELECT (lcAlias)
  RETURN
ENDIF

APPEND BLANK
REPLACE StylFld  WITH SUBSTR(ORDLINE.STYLE , lnStyPosGl , lnStyLnGl) ,;
        ClrFld   WITH SUBSTR(ORDLINE.STYLE , lnClrPosGl , lnClrLnGl) ,;
        SclFld   WITH SUBSTR(ORDLINE.STYLE , lnScaPosGl , lnScaLnGl) ,;
        Enterdat WITH ORDHDR.ENTERED                                 ,;
        StartDat WITH ORDHDR.START                                   ,;
        CmpltDat WITH ORDHDR.COMPLETE                                ,;
        OrderNo  WITH ORDHDR.ORDER                                   ,;
        CustName WITH LEFT(CUSTOMER.STNAME,18)

REPLACE CustCod  WITH ORDHDR.ACCOUNT      ,;
        StoreNo  WITH ORDLINE.STORE       ,;
        CustPo   WITH ORDHDR.CUSTPO       ,;
        Srp1Cod  WITH ORDHDR.REP1         ,;
        TermCod  WITH ORDHDR.CTERMCODE    ,;
        SizCde   WITH LEFT(SCALE.SCALE,1) ,;
        ResonFld WITH lcRsnNmbrs          ,;
        SesonCod WITH ORDHDR.SEASON       ,;
        DvsonCod WITH ORDHDR.CDIVISION    ,;
        SesonFld WITH lcSeason            ,;
        DvsonFld WITH lcDivision          ,;
        EndDate  WITH ldEndDate

REPLACE Qty1    WITH ORDLINE.Qty1   - ORDLINE.Pik1   ,;
        Qty2    WITH ORDLINE.Qty2   - ORDLINE.Pik2   ,;
        Qty3    WITH ORDLINE.Qty3   - ORDLINE.Pik3   ,;
        Qty4    WITH ORDLINE.Qty4   - ORDLINE.Pik4   ,;
        Qty5    WITH ORDLINE.Qty5   - ORDLINE.Pik5   ,;
        Qty6    WITH ORDLINE.Qty6   - ORDLINE.Pik6   ,;
        Qty7    WITH ORDLINE.Qty7   - ORDLINE.Pik7   ,;
        Qty8    WITH ORDLINE.Qty8   - ORDLINE.Pik8   ,;
        TotQty  WITH ORDLINE.TotQty - ORDLINE.TotPik ,;
        PricFld WITH ORDLINE.PRICE

SELECT (lcAlias)

*--End of lfInsRecrd.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : SOACJERE.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*!**************************************************************************
*! Name      : lfSeTSRep
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Go top in the SALESREP IN RANGE
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSeTSRep()
*!**************************************************************************
FUNCTION lfSeTSRep
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SELECT SALESREP
   SET ORDER TO TAG  SALESREP
   GO TOP

  CASE OpGrdParm = 'R'
    SELECT SALESREP
    SET ORDER TO
ENDCASE

*--End of lfSeTSRep.
*!*************************************************************
*! Name      : lfRsnNmbr
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Return the Reason numbers displayed in report.
*!*************************************************************
*! Called from : SOACJERE.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfRsnNmbr()
*!*************************************************************
FUNCTION lfRsnNmbr

lcRsnNmbrs = SPACE(0)
lnNotLine = 1

DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
  lcRsnNmbrs = lcRsnNmbrs + IIF("NO CONFIRMATION" $ MLINE(NOTEPAD.MNOTES,lnNotLine) , IIF("1" $ lcRsnNmbrs , "" , "1") , "")
  lcRsnNmbrs = lcRsnNmbrs + IIF("NO CREDIT" $ MLINE(NOTEPAD.MNOTES,lnNotLine) , IIF("2" $ lcRsnNmbrs , "" , "2") , "")
  lcRsnNmbrs = lcRsnNmbrs + IIF("SHIP COMPLETE" $ MLINE(NOTEPAD.MNOTES,lnNotLine) , IIF("4" $ lcRsnNmbrs , "" , "4") , "")
  lnNotLine  = lnNotLine + 1
ENDDO

*--Sort the value insert in "lcRsnNmbrs"
lcRsnNmbrs = lfSortRsn(lcRsnNmbrs)

RETURN lcRsnNmbrs
*--End of lfRsnNmbr.
*!*************************************************************
*! Name      : lfSortRsn
*! Developer : BASSEM RAAFAT ERNEST(BWA)
*! Date      : 02/11/2004
*! Purpose   : Return the Reason numbers Sorted.
*!*************************************************************
*! Called from : SOACJERE.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSortRsn()
*!*************************************************************
FUNCTION lfSortRsn
PARAMETERS lcTobeSort
PRIVATE laTobeSort , lcToReturn

DECLARE laTobeSort[1]
STORE SPACE(0) TO laTobeSort , lcToReturn

IF LEN(lcTobeSort) > 1
  DIMENSION laTobeSort[LEN(lcTobeSort)]
  FOR lnI = 1 TO LEN(lcTobeSort)
    laTobeSort[lnI] = SUBSTR(lcTobeSort,lnI,1)
  ENDFOR

  *--Sort the array.
  =ASORT(laTobeSort)
  
  FOR lnI = 1 TO LEN(lcTobeSort)
    lcToReturn = lcToReturn + laTobeSort[lnI]
  ENDFOR

ELSE
  lcToReturn = lcRsnNmbrs
ENDIF

RETURN lcToReturn

*--End of lfSortRsn
*!*************************************************************
*! Name      : lfPrntSiz
*! Developer : NADER NABIL (NNA)
*! Date      : 16/02/2005
*! Purpose   : Determine when the Sizes Print
*!*************************************************************
*! Called from : SOACJERE.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrntSiz()
*!*************************************************************
FUNCTION lfPrntSiz
PRIVATE lcDivision , lcSalRep , lcScale , llPrint
STORE '' TO lcDivision , lcSalRep , lcScale
STORE .F. TO llPrint
SELECT (lcOrdlins)
SCAN
  IF !(lcDivision == ALLTRIM(DvsonCod))
    lcDivision = ALLTRIM(DvsonCod)
    llPrint = .T.
  ELSE
    llPrint = .F.
  ENDIF
  IF !(lcSalRep == ALLTRIM(Srp1Cod))
    lcSalRep = ALLTRIM(Srp1Cod)
    llPrint = .T.
  ELSE
    llPrint = .F.
  ENDIF
  IF !(lcScale == ALLTRIM(SclFld))
    lcScale = ALLTRIM(SclFld)
    llPrint = .T.
  ELSE
    llPrint = .F.
  ENDIF
  REPLACE llPrntSiz WITH llPrint
ENDSCAN
*--End of lfPrntSiz.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : NADER NABIL (NNA)
*! Date      : 16/02/2005
*! Purpose   : put system date as default date in the Copletition 
*!           : date option
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
PRIVATE lnPos
IF lnOgSeting = 1
  lnPos = ASCAN(laOgFxFlt,'ORDHDR.COMPLETE')
  IF lnPos > 0
    lnPos = ASUBSCRIPT( laOgFxFlt , lnPos , 1 )
    laOgFxFlt[lnPos,6] = gdSysDate
    = lfOGShowGet('laOGFxFlt[' + ALLTRIM(STR(lnPos)) + ',6]')  
  ENDIF
ENDIF
*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfResons
*! Developer : NADER NABIL (NNA)
*! Date      : 16/02/2005
*! Purpose   : Add a comma or slash or space between the Resons numbers 
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ...
*!*************************************************************
*! Example   : = lfResons()
*!*************************************************************
FUNCTION lfResons
STORE '' TO lcNewRsons
SELECT (lcOrdlins)
SCAN FOR LEN(ALLTRIM(RESONFLD))>1
  FOR I = 1 TO LEN(ALLTRIM(RESONFLD))
    lcNewRsons = lcNewRsons + IIF(I<>1 ,'/','') +SUBSTR(ALLTRIM(RESONFLD),I,1)
  ENDFOR
  REPLACE RESONFLD WITH lcNewRsons
  STORE '' TO lcNewRsons
ENDSCAN
*--End of lfResons.
