*:****************************************************************************************
*: Program file  : PSCLSRGS.PRG
*: Program desc. : Registre Close Out Report.
*: System        : Aria Apparel System (A27).
*: Module        : Point Of Sale.  (PS)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102185,1
*: Date          : 01/24/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvSafe   , lfvDateRng  , lfvPbOk   , lfAdrShift
*:               :            : lfCretTemp, lfCollect   , lfwRepWhen, lfwVOldVal
*:               :            : lfvWareCode.
*:               ---------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#102185,1.
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*:
*-- Declaration variables.
*-- laCompAdd  :- Array to hold the Company address.
*-- lcCompPhon :- Variable to hold the Company Phone.
*-- lcTime     :- Variable to hold the Time
DECLARE laCompAdd[5,1] 
STORE '' To laCompAdd , lcCompPhon ,lcPhonPict ,lcCompName
lcTime     = TIME() 
*-- END Declaration variables.

*-- if user change filter criteria then you must collect data again [Begin]
IF llOGFltCh
  *-- Function to create the temp file.
  = lfCretTemp ()

  *-- Function to Collect the Data in  the temp transaction file.
  = lfCollect ()

  *-- Add the Recored again to rePrint the diff. bettween the tander types.
  SELECT (lcTmpTran)
  SCAN FOR cDummy = 'A'
    SCATTER MEMVAR MEMO
    m.cDummy = 'Z'
    INSERT INTO (lcTmpTran) FROM MEMVAR
  ENDSCAN
  *-- Order file to help us in printing.
  SET ORDER TO TAG (lcTmpTrndx)
ENDIF
*-- Select the Base File.
SELECT (lcSafeTemp)
SET RELATION TO cbnkcode+cWareCode INTO &lcTmpTran ADDITIVE
SET SKIP TO &lcTmpTran
GOTO TOP
DO gfDispRe WITH EVAL('lcRpForm')
*-- release the relation & skip

SELECT (lcSafeTemp)
SET RELATION TO

RETURN


*-- End OF Report Code.
*-- Function Section.
*:*************************************************************
*: Name      : lfvSafe
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Valid the safe.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvSafe()
*:*************************************************************
*
FUNCTION lfvSafe
PRIVATE lnAlias
STORE 0 TO lnAlias

lnAlias = SELECT (0)
IF !EMPTY(lcRpChRgst ) .AND. !SEEK(lcRpChRgst,'APBANKS')
  SELECT APBANKS
  GOTO TOP
  DECLARE laData[1]
  laData = ''
  lcOld_ttl   = lcFile_ttl
  lcPBrowTitl = "Registers"            && Browse title.
  lcTmpFld    = lcBrFields             && Save browse fields.
  lcBrFields = [cbnkcode:H="Register Code",cbnklndes:H="Long Description",cbnkshdes:H="Short Description",]+;
               [caddress1:H="Ship To Address1",caddress2:H="Ship To Address2",caddress3:H="Ship To Address3",]+;
               [caddress4:H="Ship To Address4",caddress5:H="Ship To Address5",caddress6:H="Ship To Address6",]+;
               [cphoneno:H="Phone",cfaxno:H="Fax"]
             
  llSele = ARIABROW('FOR cbnktype= "S"',lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,;
                       "","","cbnkcode","laData")
  
  *-- Restore browse fields.
  lcBrFields = lcTmpFld
  lcFile_ttl = lcOld_ttl
  llBrowse = .F.
  IF llSele
    lcRpChRgst = laData[1]
  ELSE
   lcRpChRgst = ''
  ENDIF
ENDIF
SELECT(lnAlias) 

*-- End OF lfvSafe.
*:*************************************************************
*: Name      : lfvDateRng
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Validation function for the user id.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvDateRng()
*:*************************************************************
*
FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = LDATE
LDTO   = HDATE
lcTitle = 'Date range'
DO DateRng.Spx

*-- End OF lfvDateRng
*:*************************************************************
*: Name      : lfvPbOk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Validate date range screen's OK button
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfvPbOk()
*:*************************************************************

FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  LDate = ldFrom
  HDate = ldTo
  llOGFltCh = .T.
  CLEAR READ
ENDIF
*-- End OF lfvPbOk
*:*************************************************************
*: Name      : lfAdrShift
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function to Shift the Address array if there is any
*:             empty lines in the address
*:*************************************************************
*: Called from :Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : Address Array name
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfAdrShift()
*:*************************************************************
*
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *-- IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- End Of lfAdrShift.
*:*************************************************************
*: Name      : lfCretTemp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function create the temp file.
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCretTemp()
*:*************************************************************
*
FUNCTION lfCretTemp

SELECT APBANKS
*-- Temp file hold safes.
=AFIELDS(laSafeTemp)
lnNewFld = ALEN(laSafeTemp,1)  
DIMENSION laSafeTemp[lnNewFld + 8,4]

laSafeTemp[lnNewFld + 1,1] = 'nTotSales'
laSafeTemp[lnNewFld + 1,2] = 'N'
laSafeTemp[lnNewFld + 1,3] = 10
laSafeTemp[lnNewFld + 1,4] = 2


laSafeTemp[lnNewFld + 2,1] = 'nTotRetrn'
laSafeTemp[lnNewFld + 2,2] = 'N'
laSafeTemp[lnNewFld + 2,3] = 10
laSafeTemp[lnNewFld + 2,4] = 0

laSafeTemp[lnNewFld + 3,1] = 'nMerDisnt'
laSafeTemp[lnNewFld + 3,2] = 'N'
laSafeTemp[lnNewFld + 3,3] = 10
laSafeTemp[lnNewFld + 3,4] = 2

laSafeTemp[lnNewFld + 4,1] = 'nTotSlsItm'
laSafeTemp[lnNewFld + 4,2] = 'N'
laSafeTemp[lnNewFld + 4,3] = 10
laSafeTemp[lnNewFld + 4,4] = 0

laSafeTemp[lnNewFld + 5,1] = 'nTotAdjst'
laSafeTemp[lnNewFld + 5,2] = 'N'
laSafeTemp[lnNewFld + 5,3] = 10
laSafeTemp[lnNewFld + 5,4] = 2

laSafeTemp[lnNewFld + 6,1] = 'nNonMer'
laSafeTemp[lnNewFld + 6,2] = 'N'
laSafeTemp[lnNewFld + 6,3] = 10
laSafeTemp[lnNewFld + 6,4] = 2


laSafeTemp[lnNewFld + 7,1] = 'nNumTrIn'
laSafeTemp[lnNewFld + 7,2] = 'N'
laSafeTemp[lnNewFld + 7,3] = 4
laSafeTemp[lnNewFld + 7,4] = 0

laSafeTemp[lnNewFld + 8,1] = 'cWareCode'
laSafeTemp[lnNewFld + 8,2] = 'C'
laSafeTemp[lnNewFld + 8,3] = 6
laSafeTemp[lnNewFld + 8,4] = 0

CREATE DBF (gcWorkDir+lcSafeTemp) FROM ARRAY laSafeTemp
INDEX ON cbnkcode+CwareCode TAG (lcSafeTemp) OF (gcWorkDir+lcSafeTemp)

*-- Temp File Hold the transaction on the safe.
DIMENSION laTmpTran[10,4]

laTmpTran[1,1] = 'cbnkcode'
laTmpTran[1,2] = 'C'
laTmpTran[1,3] = 8
laTmpTran[1,4] = 0

laTmpTran[2,1] = 'cTranCode'
laTmpTran[2,2] = 'C'
laTmpTran[2,3] = 6
laTmpTran[2,4] = 0

laTmpTran[3,1] = 'cWareCode'
laTmpTran[3,2] = 'C'
laTmpTran[3,3] = 6
laTmpTran[3,4] = 0

laTmpTran[4,1] = 'cTranDesc'
laTmpTran[4,2] = 'C'
laTmpTran[4,3] = 30
laTmpTran[4,4] = 0

laTmpTran[5,1] = 'nTotalIn'
laTmpTran[5,2] = 'N'
laTmpTran[5,3] = 8
laTmpTran[5,4] = 2

laTmpTran[6,1] = 'nTotalOut'
laTmpTran[6,2] = 'N'
laTmpTran[6,3] = 8
laTmpTran[6,4] = 2

laTmpTran[7,1] = 'nNetTot'
laTmpTran[7,2] = 'N'
laTmpTran[7,3] = 8
laTmpTran[7,4] = 2

laTmpTran[8,1] = 'nNetTotIn'
laTmpTran[8,2] = 'N'
laTmpTran[8,3] = 8
laTmpTran[8,4] = 0

laTmpTran[9,1] = 'nNetTotOut'
laTmpTran[9,2] = 'N'
laTmpTran[9,3] = 8
laTmpTran[9,4] = 0

laTmpTran[10,1] = 'cDummy'
laTmpTran[10,2] = 'C'
laTmpTran[10,3] = 1
laTmpTran[10,4] = 0


CREATE DBF (gcWorkDir+lcTmpTran) FROM ARRAY laTmpTran
*-- INDEX on the temp trabnsaction on the safe.
INDEX ON cbnkcode+cWareCode+cDummy+cTranCode TAG (lcTmpTrndx) OF (gcWorkDir+lcTmpTran)
*-- Index to help us in Printing
INDEX ON cbnkcode+cTranCode+cWareCode TAG (lcTmpTran) OF (gcWorkDir+lcTmpTran)
*-- End OF lfCretTemp
*:*************************************************************
*: Name      : lfCollect
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function collect data in transcation file.
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfCollect()
*:*************************************************************
*
FUNCTION lfCollect
PRIVATE lnAlias , lnTotSales ,lnTotRetrn , lnNumTrIn ,;
                  lnNumTrOut ,lnTotSlsIt , lnNonMer ,lcWareCode ,lcTranNum
STORE 0 To lnAlias , lnTotSales ,lnTotRetrn ,lnNumTrIn ,;
           lnNumTrOut , lnMerDisnt ,lnTotSlsIt,lnTotAdjst ,;
           lnNonMer
STORE .F. To llFoundTrn ,llFodTrnAdj
STORE '' To lcWareCode ,lcTranNum
lnAlias = SELECT (0)

*-- Set relation to style file to get the Group Code G.C. (Gift Certificate)
SELECT POSTRAN
SET RELATION TO Style INTO STYLE ADDITIVE


SELECT APBANKS

*-- To Optmize the scan Exp.
IF !EMPTY(lcRpChRgst)
  = SEEK(lcRpChRgst)
  lcScanExp = "REST FOR cbnkcode = lcRpChRgst "
ELSE
  GOTO TOP
  lcScanExp = "WHILE !EOF()  "
ENDIF

*-- To Optmize the scan Exp On POSTRAN FILE. 
DO CASE
  CASE !EMPTY(LDATE) .AND. !EMPTY(HDATE)
    lcScanTrEp = " FOR cBnkCode = APBANKS.cBnkCode .AND. BETWEEN(DTOC(Trandate),DTOC(LDATE),DTOC(HDATE))"
  CASE EMPTY(LDATE) .AND. !EMPTY(HDATE)
    lcScanTrEp = " FOR cBnkCode = APBANKS.cBnkCode .AND. DTOC(Trandate) <= DTOC(HDATE)"
  CASE !EMPTY(LDATE) .AND. EMPTY(HDATE)
    lcScanTrEp = " FOR cBnkCode = APBANKS.cBnkCode .AND. DTOC(Trandate) >= DTOC(LDATE)"
  OTHERWISE
    lcScanTrEp = " FOR cBnkCode = APBANKS.cBnkCode"  
ENDCASE

*-- Remove the TranDate from Exp and replace with Dpaydate and scan on the appymnt
*-- file , after scan return the old expr.
lcOldScan = lcScanTrEp
lcApymtExp = IIF(!EMPTY(HDATE) .OR. !EMPTY(LDATE),STRTRAN(lcScanTrEp,'Trandate','DpayDate'),lcScanTrEp)
lcScanTrEp = lcOldScan

*-- Add The warehouse to the exp. to get it when scan on the file.
lcScanTrEp = lcScanTrEp + IIF(EMPTY(lcRpWareH)," .AND. .T."," .AND. cWareCode = lcRpWareH")

*-- Scan On the Safes file.
SCAN &lcScanExp
  WAIT WINDOW 'Please wait .. collecting data.' Nowait 
  *-- Get the total adjustment from the Appaymnt
  IF SEEK(cBnkCode,'Appaymnt')
    SELECT APPAYMNT
     SCAN &lcApymtExp  .AND. cPayMeth $ 'ABN'
      IF cPayMeth = 'B' .OR. (cPayMeth $ 'NA' .AND. !EMPTY(Batch))
        llFodTrnAdj = .T.
        lnTotAdjst = lnTotAdjst + nPayAmnt
      ENDIF  
    ENDSCAN
    SELECT APBANKS
  ENDIF

  *-- Get the sales,return,number of sales ....etc from the postran file
  IF SEEK(cBnkCode,'POSTRAN')
    SELECT POSTRAN  
    *-- save the selected warehouse to get data for only selected  in the safe file.
    LOCATE &lcScanTrEp
    lcWareCode = IIF(EMPTY(lcRpWareH),POSTRAN.cWareCode,lcRpWareH)

    SCAN &lcScanTrEp
      *-- If found any transaction Append the sales & return & Discount 
      *-- In the Safes temp file.
      IF  POSTRAN.cWareCode <> lcWareCode .AND. llFoundTrn
        IF !SEEK(APBANKS.cBnkCode+lcWareCode,lcSafeTemp) 
          SELECT APBANKS
          SCATTER MEMVAR MEMO
          SELECT (lcSafeTemp)
          APPEND BLANK
          GATHER MEMVAR MEMO      
        ENDIF
        SELECT (lcSafeTemp)
        REPLACE nTotSales  WITH nTotSales  + lnTotSales ,;
                nTotRetrn  WITH nTotRetrn  + lnTotSales ,;
                nMerDisnt  WITH nMerDisnt  + lnMerDisnt ,;
                nTotSlsItm WITH nTotSlsItm + lnTotSlsIt ,;
                nTotAdjst  WITH nTotAdjst  + lnTotAdjst ,;
                nNonMer    WITH nNonMer    + lnNonMer   ,;
                nNumTrIn   WITH nNumTrIn   + lnNumTrIn  ,;
                cWareCode  WITH lcWareCode
      
        STORE 0 To lnTotSales ,lnTotRetrn ,lnNumTrIn ,;
                   lnNumTrOut , lnMerDisnt ,lnTotSlsIt ,lnNonMer
        STORE '' To lcTranNum
        STORE .F. To llFoundTrn ,llFodTrnAdj        
        lcWareCode = POSTRAN.cWareCode
        SELECT POSTRAN
      ENDIF
      DO CASE
          *-- Select POSTRAN File and get The total Freight amount 
          *-- during the day from  POSTRAN  file.[Hold type 'F'].
        CASE POSTRAN.TranType = 'F'
          lnNonMer = lnNonMer + nAmount
          llFoundTrn = .T.          
          *-- Select POSTRAN File and get the total sales amount 
          *-- during the day from  POSTRAN  file.[Hold type '1'].
        CASE POSTRAN.TranType = '1'
          lnMerDisnt =  lnMerDisnt + disc_amt + discount
          lnTotSlsIt = lnTotSlsIt  + TotQty
          IF lcTranNum <> POSTRAN.Tran 
            lnNumTrIn  = lnNumTrIn   + 1
            lcTranNum = POSTRAN.Tran
          ENDIF  
          *-- Get the Non Merchandise sales
          IF ALLTRIM(Style.cstygroup) = "G.C."
            lnNonMer   = lnNonMer + nAmount
           ELSE
             lnTotSales = lnTotSales  + nAmount 
          ENDIF
          llFoundTrn = .T.
          
          *-- Select POSTRAN File and get the total returns amount 
          *-- during the day from  POSTRAN  file.[Hold type '0'].
        CASE POSTRAN.TranType = '0'
          lnMerDisnt =  lnMerDisnt + disc_amt + discount
          IF ALLTRIM(Style.cstygroup) = "G.C."
            lnNonMer   = lnNonMer + nAmount
          ELSE
            lnTotRetrn = lnTotRetrn + nAmount*-1
          ENDIF
          llFoundTrn = .T.

        *-- Type Of Payment in POSTRAN [Hold Type '4']
        CASE POSTRAN.TranType = '4' .AND. !EMPTY(POSTRAN.TranCode)
          SELECT (lcTmpTran)
          IF !SEEK (POSTRAN.cBnkCode+POSTRAN.TranCode+POSTRAN.cWareCode)
            APPEND BLANK
            REPLACE cBnkCode  WITH POSTRAN.cBnkCode ,;
                    cTranCode WITH POSTRAN.TranCode ,;
                    cWareCode WITH POSTRAN.cWareCode,;
                    cTranDesc WITH gfCodDes(POSTRAN.trancode,'CTERMCODE'),;
                    nTotalIn  WITH 0                ,;
                    nTotalOut WITH 0                ,;
                    nNetTot   WITH 0                ,;
                    cDummy    WITH 'A'
          ENDIF
          IF POSTRAN.nAmount < 0 
            REPLACE nTotalIn  WITH nTotalIn  + ABS(POSTRAN.nAmount),; 
                    nNetTot   WITH nNetTot   + ABS(POSTRAN.nAmount),;
                    nNetTotIn WITH nNetTotIn + 1
          
          ELSE
              REPLACE nTotalOut  WITH nTotalOut + ABS(POSTRAN.nAmount),;
                      nNetTot    WITH nNetTot   - ABS(POSTRAN.nAmount),;
                      nNetTotOut WITH nNetTotOut + 1
          ENDIF
          llFoundTrn = .T.
          SELECT POSTRAN
      ENDCASE
    ENDSCAN
    *-- If found any transaction Append the sales & return & Discount 
    *-- In the Safes temp file.

    IF llFoundTrn .OR. llFodTrnAdj 
      IF !SEEK(APBANKS.cBnkCode+lcWareCode,lcSafeTemp)
        SELECT APBANKS
        SCATTER MEMVAR MEMO
        SELECT (lcSafeTemp)    
        APPEND BLANK
        GATHER MEMVAR MEMO      
      ENDIF
      SELECT (lcSafeTemp)
      REPLACE nTotSales  WITH nTotSales  + lnTotSales ,;
              nTotRetrn  WITH nTotRetrn  + lnTotSales ,;
              nMerDisnt  WITH nMerDisnt  + lnMerDisnt ,;
              nTotSlsItm WITH nTotSlsItm + lnTotSlsIt ,;
              nTotAdjst  WITH nTotAdjst  + lnTotAdjst ,;
              nNonMer    WITH nNonMer    + lnNonMer   ,;
              nNumTrIn   WITH nNumTrIn   + lnNumTrIn  ,;
              cWareCode  WITH lcWareCode
        STORE '' To lcTranNum 
        STORE 0 To lnTotAdjst, lnTotSales ,lnTotRetrn ,lnNumTrIn ,;
                   lnNumTrOut , lnMerDisnt ,lnTotSlsIt ,lnNonMer
        STORE .F. To llFoundTrn ,llFodTrnAdj
              
    ENDIF
  ENDIF
  SELECT APBANKS
ENDSCAN

SELECT(lnAlias)
*-- End OF lfCollect.
*:*************************************************************
*: Name      : lfwRepWhen
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Optional Grid When Function.
*:*************************************************************
*: Called from : Program.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen

*-- Made new Temp index on the Postran file to help us when 
*-- collecting date for selected safe.
SELECT POSTRAN
INDEX ON cBnkCode+DTOC(Trandate)+cWareCode TAG (lcPostRan) OF (gcWorkDir+lcPostRan)


*-- Made new Temp index on the Appaymnt file to help us when 
*-- collecting date for selected safe.
SELECT APPAYMNT
INDEX ON cBnkCode+DTOC(DpayDate) TAG (lcApayMent) OF (gcWorkDir+lcApayMent)

*-- End OF lfwRepWhen
*:****************************************************************
*: Name      : lfwVOldVal
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : When function to get the Old value
*:****************************************************************
*: Called from : Some of the Option Grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfwOldVal()
*:****************************************************************
*
FUNCTION lfwVOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value

*-- End of lfwVOldVal.
*:*************************************************************
*: Name      : lfvWareCode
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function to WareHouse Code.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfvWareCode()
*:*************************************************************
FUNCTION lfvWareCode

*-- Varible to hold  the name of the memory variable used to create the current GET field .
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field .
lcObjVal = EVALUATE(SYS(18))

*-- IF The user want to Browse or if the Warehouse he entered is not in the
*-- file.
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'WAREHOUS'))
  lcObjVal = gfBrowWare(.T.)
  lcObjVal = IIF(EMPTY(lcObjVal) , laOldVal , lcObjVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*-- End OF lfvWareCode.
*:****************************************************************
*: Name      : lfvPrintBy
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function to get the form that will print.
*:****************************************************************
*: Called from : Some of the Option Grid fields
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfvPrintBy()
*:****************************************************************
*
FUNCTION lfvPrintBy

*-- Print report on receipt Printer or laser printer

IF lcRpPrntBy = 'L'
  IF lcRpForm <> 'PSCLSRGL'
    lcRpForm = 'PSCLSRGL'
    = gfCrtFrm(lcRpForm ,lcOGFormArr,llOGRefForm)
    = lfRepPltFr(lcRpForm )
  ENDIF
ELSE
  IF lcRpForm <> 'PSCLSRGR'
    lcRpForm = 'PSCLSRGR'
    = gfCrtFrm(lcRpForm ,lcOGFormArr,llOGRefForm)
    = lfRepPltFr(lcRpForm )
  ENDIF  
ENDIF

*-- End OF lfvPrintBy.
*:****************************************************************
*: Name      : lfPrntWhos
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/24/2001
*: Purpose   : Function to get the Address for the warhouse.
*:****************************************************************
*: Called from : Frx.
*:****************************************************************
*: Calls       : None
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfPrntWhos()
*:****************************************************************
*
FUNCTION lfPrntWhos
PRIVATE lnALIAS
lnALIAS = 0

*-- Get Warehouse Address [begin].
lnALIAS = SELECT (0)
=SEEK (&lcSafeTemp..CWARECODE,'WAREHOUS')
lcCompName = WAREHOUS.cdesc   && Variable to hold the WAREHOUSE Name desc.
lcCompPhon = WAREHOUS.cphone  && Variable to hold the WAREHOUSE Phone
lcPhonPict = gfPhoneTem()      && Variable to hold the WAREHOUSE Phone Format
*-- Load Company address.
= gfGetAdr('WAREHOUS' , '' , '' , '' , @laCompAdd)
= lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
*-- Get company Address [end].

SELECT(lnALIAS)

*-- End OF lfPrntWhos
*:****************************************************************
