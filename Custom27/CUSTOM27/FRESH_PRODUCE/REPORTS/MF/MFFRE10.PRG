*:**************************************************************************
*: Program file  : MFFRE10
*: Program desc. : Custom Adornment Quantities Report for FRESH PRODUCE
*: System        : Aria Advantage Series.
*: Module        : Manufacturing (MF)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 01/26/2000
*: Reference     : C101752
*:**************************************************************************
*: Calls : 
*:    Procedures : lpCreatFil, lpShowObj
*:                   
*:    Functions  : gfModalGen, lfwRepWhen, lfvFormat, lfvOutput, lfvSort,  
*:               : lfSRDsgClr, lfSRDsgCod, lfSRAcount, lfSRRepCod, lfItmPos, 
*:               : lfvStates, lfGetReplc, lfvOrdDate, lfVarPos, lfPrevRun, 
*:               : lfReset, lfPrintTot, lfClearRep.
*:**************************************************************************
*: Example : DO MFFRE10
*:
*:**************************************************************************
*: Modifications
*:B#603517,1 SSH 29/03/00 Fix the following Bugs : - 
*:B#603517,1 SSH 1) The report is not using OrdHdr.Rep1.
*:B#603517,1 SSH 2) Report FRX files for both Format A & Format B.
*:B#603517,1 SSH    These forms must print out to LANDSCAPE 8 ½ x 14
*:B#603517,1 SSH 3) Option Title not print out when one is entered 
*:B#603517,1 SSH 4) Seasons Grid choice appear to not work on report.
*C200137,1 TAK 10/10/2000
*:**************************************************************************
*laTempExpr --> Array to hold all laOGFxFlt array
*lcSlRepExp --> Variable to hold only Sales Rep Expression
*lcBomVrExp --> Variable to hold only Design Code & Color Expression
*lcAfterExp --> Variable to detect "AND" operand after lcBomVrExp
*lcBeforeEx --> Variable to detect "AND" operand before lcBomVrExp
*lnGrTotOrd --> Variable to hold the Grand Total Ordered in Format "A"
*ldFrPeriod --> Variable to hold the From Period in Report Page Header
*ldToPeriod --> Variable to hold the To Period in Report Page Header
*lcIndexKey --> Variable to hold the active index value 
*llPrintTot --> Variable used in Report Format 'B' to detect whether to
*               SHOW ONLY total quantities for pieces allocated if all 
*               Orders have met acceptable allocation , this variable is 
*               declared in Syrepuvr 

lcStTime = TIME()    && Save Current time in a variable
PRIVATE ldFrstOfMo , ldDateVal , ldFrPeriod , dToPeriod ,;
        lcSetCent , lcSetDelet , lcDateType

STORE {} TO ldFrstOfMo , ldDateVal , ldFrPeriod , ldToPeriod
lcSetDelet = SET('DELETE')
lcSetCent = SET('CENTURY')
SET CENTURY ON

*Checking for Empty Date whether it's Start , Complete , Entered
*If Date is Empty or Null
IF LEN(ALLTRIM(SUBSTR(laOGFxFlt[lnDatePost,6],1,10))) < 10
  ldDateVal = gdSysDate
ELSE
  ldDateVal = CTOD(SUBSTR(laOGFxFlt[lnDatePost,6],1,10))
ENDIF

*get the first day of the next month
*Save date Type in a variable (American , British .. etc)
lcDateType = SET('DATE')
SET DATE TO BRITISH
ldFrstOfMo = CTOD('01/' + ALLTRIM(STR(MONTH(ldDateVal)+1) + '/' + ;
             ALLTRIM(STR(YEAR(gdSysDate)))))
SET DATE TO &lcDateType     && Restore old Date Type             

ldFrPeriod = ldDateVal
ldToPeriod = GOMONTH(ldDateVal,6)
*EndIf Date is Empty or Null

*-- If filter is changed collect data again [Begin]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF

  PRIVATE lcBomVrExp,lcExpToCut
  lcBomVrExp = ".T."
  lcExpToCut = ""
  
  IF "ORDHDR.REP1" $ lcRpExp OR "BOMVAR.CDSGNCODE" $ lcRpExp ;
      OR "ICDESIGN.CDSGNCLR" $ lcRpExp
    
    PRIVATE lcBrowPos
    
    *-- Copy all laOGFxFlt to another array    
    DIMENSION laTempExpr[1]
    =ACOPY(laOGFxFlt,laTempExpr)
    
    *we need to remove BOMVAR.CDSGNCODE & ICDESIGN.CDSGNCLR in 
    *a separate filter 
    IF "BOMVAR.CDSGNCODE" $ lcRpExp OR "ICDESIGN.CDSGNCLR" $ lcRpExp
      PRIVATE lcBomVrExp , lcAfterExp , lcBeforeEx
      DIMENSION laOGFxFlt[2,7]
      DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]      

      *Copy Browse Array to another Temp array
      =ACOPY(laBrFldFlt,laBrTmpFlt)

      *Change the position of both Design Code & Color in the Second
      *Dimensional array Column no 5 --> laOGFxFlr:4 , laOGFxFlr:5
      DIMENSION laBrFldFlt[2,5]
      
      *Convert numeric position to string
      lcBrowPos = ALLTRIM(STR(lnDsgCdPos))  

      FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
        IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
          =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),10)
        ENDIF
      ENDFOR
      laBrFldFlt[1,5] = "laOGFxFlt:1"
      laBrFldFlt[2,5] = "laOGFxFlt:2"

      =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnDsgCdPos,1),14)
      *Generate InList expression for Both Design Code & Color
      lcBomVrExp = gfGenFlt('laOGFxFlt',.T.,.T.)  

      *Restore Browse Array from the Temp array
      DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]      
      =ACOPY(laBrTmpFlt,laBrFldFlt)
      
      *-- Checking for the AND Operator before the lcBomVrExp
      lcBeforeEx = SUBSTR(lcRpExp,AT(lcBomVrExp,lcRpExp)-5,5)      
      
      lcExpToCut = lcBomVrExp
      IF "AND" $ lcBeforeEx
	    lcExpToCut = lcBeforeEx + lcExpToCut
	  ELSE
	    *-- Checking for the AND Operator after the lcBomVrExp      
        lcAfterExp = SUBSTR(lcRpExp,AT(lcBomVrExp,lcRpExp)+LEN(lcBomVrExp),5)
        IF "AND" $ lcAfterExp
	      lcExpToCut = lcExpToCut + lcAfterExp
        ENDIF	    	
      ENDIF
      
      *-- Remove the 2 conditions
      lcRpExp = ALLTRIM(STRTRAN(lcRpExp,lcExpToCut,''))
      lcRpExp = IIF(EMPTY(lcRpExp),".T.",lcRpExp)
    ENDIF

    *we need to convert (OrdHdr.Rep1) in filter Expression if found
    *to be (OrdHdr.Rep1 OR OrdHdr.Rep2)  
    IF "ORDHDR.REP1" $ lcRpExp            
      DIMENSION laOGFxFlt[1,7]
      DIMENSION laBrTmpFlt[ALEN(laBrFldFlt,1),ALEN(laBrFldFlt,2)]

      *Copy Browse Array to another Temp array
      =ACOPY(laBrFldFlt,laBrTmpFlt)

      *Change the position of both Design Code & Color in the Second
      *Dimensional array Column no 5 --> laOGFxFlr:4 , laOGFxFlr:5
      DIMENSION laBrFldFlt[1,5]
      
      *Convert numeric position to string
      lcBrowPos = ALLTRIM(STR(lnSlRepPos))

      FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
        IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
          =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),5)
        ENDIF
      ENDFOR
      laBrFldFlt[1,5] = "laOGFxFlt:1"

      =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnSlRepPos,1),7)
          
      PRIVATE lcSlRepExp
      *Generate InList expression for SalesRep
      lcSlRepExp = gfGenFlt('laOGFxFlt',.T.,.T.)
            
      lcRpExp = STRTRAN(lcRpExp,lcSlRepExp,"(" + lcSlRepExp + [ .OR. ] + ;
                STRTRAN(lcSlRepExp,".REP1",".REP2") + ")")
      
      *Restore Browse Array from the Temp array
      DIMENSION laBrFldFlt[ALEN(laBrTmpFlt,1),ALEN(laBrTmpFlt,2)]      
      =ACOPY(laBrTmpFlt,laBrFldFlt)
    ENDIF

    DIMENSION laOGFxFlt[1]
    =ACOPY(laTempExpr,laOGFxFlt)    && restore Original array
    RELEASE laTempExpr
  ENDIF
  
  *-- Make all relations in order to collect data [Begin]
  SELECT BomVar
  SET RELATION TO cdsgncode INTO Icdesign ADDITIVE
  
  SELECT OrdLine
  SET RELATION TO "SO" + Order INTO BomVar,;
              cOrdType + Order INTO Ordhdr,;
              'M'+ Ordline.account INTO Customer ADDITIVE
  *-- Make all relations in order to collect data [End]

  *-- collecting the data into Temp. file [Begin.]
  lcReplExpr = lfGetReplc()  && Get Sort by value to update cGroupKey with it.
  *Avoid Replacing Key field with proper sort by values in If...Endif block.
  llChSortBy = .F.  
  
  SELECT OrdLine
  WAIT WINDOW 'Selecting Orders, Please wait' NOWAIT
  *-- If Report format is "A" we have to collect its appropriate Data
  IF lcRpFormat = "A"
    DIMENSION lnGrTotMon[6]
    PRIVATE lnGrTotInv 
    STORE 0 TO lnGrTotInv , lnGrTotMon , lnGrTotOrd

    *-- Scan Around the Ordline file
    SCAN FOR &lcRpExp
      SELECT BomVar
      *If OrdLine Order no is found in BomVar
      IF SEEK ('SO'+OrdLine.Order+STR(OrdLine.LineNo,6))
        
        *-- Scan Around the BomVar file
        SCAN REST WHILE cidtype+cCost_Id+STR(lineno,6) = 'SO'+OrdLine.Order+STR(OrdLine.LineNo,6) ;
                    FOR &lcBomVrExp
          WAIT WINDOW 'Selecting Order# ' + ALLTRIM(OrdLine.Order)  + ;
                      ' For Design : ' + ALLTRIM(cDsgnCode) + ;
                      IIF(EMPTY(IcDesign.cDsgnClr),'',' , Color : ' + ;
                      ALLTRIM(IcDesign.cDsgnClr)) NOWAIT
          
          *-- Initializing memory variables used in Format 'A'                      
          STORE '' TO  m.Design_Cod, m.Desg_Color, m.Desgn_Size, m.Desgn_Name,;
                       m.Season, m.cAdorContr, m.cGroupKey 
          STORE 0  TO  m.Tot_Order, m.Tot_Invoic, m.nMonth1, m.nMonth2, ;
                       m.nMonth3, m.nMonth4, m.nMonth5, m.nMonth6
                       
          *Collecting 1st 5 records in Format "A"
          m.Design_Cod  = IcDesign.cDsgnCode
          m.Desg_Color  = IcDesign.cDsgnClr
          m.Desgn_Size  = IcDesign.cDsgnSize
          m.Desgn_Name  = IcDesign.cDsgnName
          m.Season      = IcDesign.Season

          *Adornment Contractor is not used in Frx but it's used only in Sorting
          m.cAdorContr = IcDesign.cVendCode
          
          *Total Invoiced
          *If Ordline Order no is found in InvLine
          IF SEEK(OrdLine.Order+STR(Ordline.LineNo,6),'InvLine')
            SELECT InvLine 
            SUM REST TotQty WHILE Order + STR(LineNo,6) + Invoice = ;
                     OrdLine.Order+STR(Ordline.LineNo,6) TO m.Tot_Invoic
          ELSE
            m.Tot_Invoic = 0
          ENDIF
          *EndIf of Ordline Order no is found in InvLine

          *lnGrTotInv to support Calculating of grand Total Invoiced
          lnGrTotInv = lnGrTotInv + m.Tot_Invoic 

          *Total Ordered --> m.Tot_Order
          m.Tot_Order   = OrdLine.TotQty + m.Tot_Invoic

          *lnGrTotOrd to support Calculating (% of OverAll Adornment Sales)
          lnGrTotOrd   = lnGrTotOrd + m.Tot_Order
                      
          DO CASE
            CASE BETWEEN(&lcDate,ldDateVal,ldFrstOfMo-1)
              m.nMonth1 = OrdLine.TotQty
              lnGrTotMon[1] = lnGrTotMon[1] + m.nMonth1

            CASE BETWEEN(&lcDate,ldFrstOfMo,GOMONTH(ldFrstOfMo,1)-1)
              m.nMonth2 = OrdLine.TotQty
              lnGrTotMon[2] = lnGrTotMon[2] + m.nMonth2
                            
            CASE BETWEEN(&lcDate,GOMONTH(ldFrstOfMo,1),GOMONTH(ldFrstOfMo,2)-1)
              m.nMonth3 = OrdLine.TotQty
              lnGrTotMon[3] = lnGrTotMon[3] + m.nMonth3
              
            CASE BETWEEN(&lcDate,GOMONTH(ldFrstOfMo,2),GOMONTH(ldFrstOfMo,3)-1)
              m.nMonth4 = OrdLine.TotQty
              lnGrTotMon[4] = lnGrTotMon[4] + m.nMonth4
              
            CASE BETWEEN(&lcDate,GOMONTH(ldFrstOfMo,3),GOMONTH(ldFrstOfMo,4)-1)
              m.nMonth5 = OrdLine.TotQty
              lnGrTotMon[5] = lnGrTotMon[5] + m.nMonth5
               
            CASE BETWEEN(&lcDate,GOMONTH(ldFrstOfMo,4),GOMONTH(ldFrstOfMo,5)-1)
              m.nMonth6 = OrdLine.TotQty
              lnGrTotMon[6] = lnGrTotMon[6] + m.nMonth6
          ENDCASE

          m.cGroupKey = EVALUATE(lcReplExpr)
          IF SEEK(PADR(m.cGroupKey,24) + m.Design_Cod + m.Desg_Color,lcWorkFile)
            SELECT (lcWorkFile)
            REPLACE Tot_Order  WITH Tot_Order + m.Tot_Order
            REPLACE Tot_Invoic WITH Tot_Invoic + m.Tot_Invoic

            FOR lnX = 1 TO 6
              lcX = ALLTRIM(STR(lnX))
              REPLACE nMonth&lcX WITH nMonth&lcX + m.nMonth&lcX
            ENDFOR
          ELSE
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF  
                      
	    ENDSCAN
	    *-- EndScan Around the BomVar file
	    
      ENDIF
      *EndIf OrdLine Order no is found in BomVar
      
    ENDSCAN
    *-- EndScan Around the Ordline file

    IF RECCOUNT(lcWorkFile) <> 0      
      SELECT (lcWorkFile)      
      APPEND BLANK
      REPLACE cGroupKey  WITH REPLICATE('z',24)
      REPLACE Season     WITH 'Totals'
      REPLACE Tot_Order  WITH lnGrTotOrd
      REPLACE Tot_Invoic WITH lnGrTotInv

      FOR lnX = 1 TO 6
        lcX = ALLTRIM(STR(lnX))
        REPLACE nMonth&lcX WITH lnGrTotMon[lnX]
      ENDFOR

      DELETE
    ENDIF
  
  ELSE   && Report format is "B"
    PRIVATE lnTotQty , lnTotPik , lnTotValue , lnSelcFile
    STORE '' TO lcDesign
    STORE 0 TO lnTotQty , lnTotPik , lnTotValue

    *-- Scan Around the Ordline file
    SCAN FOR &lcRpExp
      
      SELECT BomVar
      *If OrdLine Order no is found in BomVar
      IF SEEK ('SO'+OrdLine.Order+STR(OrdLine.LineNo,6))
        *-- Scan Around the BomVar file
        SCAN REST WHILE cidtype+cCost_Id+STR(lineno,6) = 'SO'+OrdLine.Order+STR(OrdLine.LineNo,6) ;
                    FOR &lcBomVrExp
          WAIT WINDOW 'Selecting Order# ' + ALLTRIM(OrdLine.Order)  + ;
                      ' For Design : ' + ALLTRIM(cDsgnCode) +  ;
                      ' , ' + ' Account ' + ALLTRIM(OrdLine.Account) NOWAIT

          *-- Initializing memory variables used in Format 'B'
          **C200137,1 
          STORE '' TO  m.Design_Cod, m.Account, m.Acct_Name, m.Sales_Rep, ;
                       m.Order, m.cGroupKey,m.Status
          STORE 0  TO  m.Total_Qty, m.Pcs_Alloct, m.Met_Alloct,                ;
                       m.Value, m.Tot_Invoic , m.nMonth1
          STORE {} TO  m.Start, m.Complete

          *Collecting records in Format "A" [Begin]
          m.Design_Cod = IcDesign.cDsgnCode
          m.Account    = OrdLine.Account
          m.Acct_Name  = Customer.BtName
          *:B#603517,1 SSH 1) Use OrdHdr.Rep1.  *** Begin ***
          *m.Sales_Rep  = IIF(EMPTY(OrdHdr.Rep1),OrdHdr.Rep1,OrdHdr.Rep2)
          m.Sales_Rep  = IIF(!EMPTY(OrdHdr.Rep1),OrdHdr.Rep1,OrdHdr.Rep2)
          *:B#603517,1 SSH 1) Use OrdHdr.Rep1. *** End ***
          m.Order      = OrdLine.Order
          m.Met_Alloct = OrdHdr.nMinAloPer
          m.Start      = OrdLine.Start
          m.Complete   = OrdLine.Complete
       
          m.Total_Qty  = OrdLine.TotQty
          m.Pcs_Alloct = OrdLine.TotPik
          **C200137,1 
          m.Status = ORDHDR.Status
            
          lnSelcFile   = SELECT()
          SELECT OrdHdr
          m.Value      = OrdLine.TotQty * IIF(cCurrCode=gcBaseCurr,;
                         OrdLine.Price,gfAmntDisp(OrdLine.Price,"O",Entered))
		  SELECT (lnSelcFile)
          *Collecting records in Format "A" [End]
                    
          m.nMonth1 = IIF(m.Total_Qty>0,IIF((m.Pcs_Alloct/m.Total_Qty)*100>;
                      m.Met_Alloct,0,1),1)

          SET DELETE OFF    
          m.cGroupKey = EVALUATE(lcReplExpr)
          IF SEEK(m.Design_Cod,lcWorkFile)
            lnTotQty   = lnTotQty   + m.Total_Qty
            lnTotPik   = lnTotPik   + m.Pcs_Alloct
            lnTotValue = lnTotValue + m.Value
          ELSE
            STORE 0 TO lnTotQty , lnTotPik , lnTotValue
            SELECT (lcWorkFile)
			APPEND BLANK
			REPLACE cGroupKey  WITH PADR(m.Design_Cod,6)+REPLICATE('z',18)
			REPLACE Desgn_size WITH REPLICATE('z',6)
			DELETE
          ENDIF
          
          IF SEEK(m.cGroupKey,(lcWorkFile))
            SELECT (lcWorkFile)
            REPLACE Total_Qty  WITH Total_Qty  + m.Total_Qty
            REPLACE Pcs_Alloct WITH Pcs_Alloct + m.Pcs_Alloct
            REPLACE Value      WITH Value      + m.Value            
          ELSE
            INSERT INTO (lcWorkFile) FROM MEMVAR
          ENDIF

          IF SEEK(PADR(m.Design_Cod,6)+REPLICATE('z',18),(lcWorkFile))
            SELECT (lcWorkFile)
            REPLACE Total_Qty  WITH Total_Qty  + m.Total_Qty
            REPLACE Pcs_Alloct WITH Pcs_Alloct + m.Pcs_Alloct            
            REPLACE Value      WITH Value      + m.Value
            REPLACE nMonth1    WITH nMonth1    + m.nMonth1
            IF nMonth1 > 0
              REPLACE Pcs_Alloct WITH 0
            ENDIF
            DELETE
          ENDIF

          SET DELETE ON
	    ENDSCAN
	    *-- EndScan Around the BomVar file
	    
      ENDIF
      *EndIf OrdLine Order no is found in BomVar
      
    ENDSCAN
    *-- EndScan Around the Ordline file

  ENDIF
  *-- Endif of Report Format
  
  *-- Break all relations [Begin]
  SELECT OrdLine
  SET RELATION OFF INTO BomVar
  SET RELATION OFF INTO Customer
  SET RELATION OFF INTO OrdHdr
  
  SELECT BomVar
  SET RELATION OFF INTO IcDesign  
  *-- Break all relations [End]
  
ENDIF
*-- If filter is changed collect data again [End]

WAIT CLEAR

*-- If no records in temp file (empty)
SELECT (lcWorkFile)
*-- If Seek is successful (There's Records)
IF RECCOUNT(lcWorkFile) <> 0
  
  *This idea does not work in this report
  *-- Reindexing file if user change sort by [Begin.]
  *IF llChSortBy
  *  llChSortBy = .F.  && Avoid Replacing Key field with proper sort by values again.
  *  lcReplExpr = lfGetReplc()
  *  REPLACE ALL cGroupKey WITH EVALUATE(lcReplExpr) && Replace key field with new values.
  *ENDIF
  *-- Reindexing file if user change sort by [End.]

  *-- If OutPut to Excel is Yes  
  IF llRpOutPut
    *If User Didn't Press Cancel Push Button
    IF !EMPTY(lcRpExcel)      
      SET DELETE OFF

      *-- If Report Format is "A"
      IF lcRpFormat = 'A'
        SELECT season , design_cod , desgn_size , desg_color , desgn_name ,;
               tot_order, ((tot_order/lnGrTotOrd)*100) AS Adorn_Sale , ;
               tot_invoic , nmonth1 AS (PROPER(CMONTH(ldFrstOfMo-1))) , ;
               nmonth2 AS (CMONTH(ldFrstOfMo)), ;
               nmonth3 AS (CMONTH(GOMONTH(ldFrstOfMo,1))) , ;
               nmonth4 AS (CMONTH(GOMONTH(ldFrstOfMo,2))) , ;
               nmonth5 AS (CMONTH(GOMONTH(ldFrstOfMo,3))) , ;
               nmonth6 AS (CMONTH(GOMONTH(ldFrstOfMo,4)));
        FROM (lcWorkFile);
        INTO TABLE (gcWorkDir+lcQueryFil)
              
        EXPORT TO (lcRpExcel) XLS   

      ELSE   && Else Report Format is "B"

        IF lcRpPktQty = "E"
**C200137,1
          SELECT Design_Cod, Account, Acct_Name, Sales_Rep, Order, Status AS St , Total_Qty,;
                 IIF(Desgn_Size="zzzzzz","   ",IIF(Total_Qty>0,;
                 IIF((Pcs_Alloct/Total_Qty)*100>Met_Alloct,"Yes","No"),"No"));
                 AS Met_Alloct,Start , Complete , Value ;
          FROM (lcWorkFile);
          INTO TABLE (gcWorkDir+lcQueryFil);
          ORDER BY cGroupKey
        ELSE
          SELECT Design_Cod, Account, Acct_Name, Sales_Rep, Order, Status AS St ,Total_Qty,;
                 Pcs_Alloct,IIF(Desgn_Size="zzzzzz","   ",IIF(Total_Qty>0,;
                 IIF((Pcs_Alloct/Total_Qty)*100>Met_Alloct,"Yes","No"),"No"));
                 AS Met_Alloct, Start , Complete , Value;
          FROM (lcWorkFile);
          INTO TABLE (gcWorkDir+lcQueryFil);
          ORDER BY cGroupKey
        ENDIF                 
               
        EXPORT TO (lcRpExcel) XLS
      ENDIF
      
      =gfModalGen("TRM000000B00000","DIALOG",'','','Export to Excel has been completed successfully.')  
    ENDIF      
    *EndIf of User Didn't Press Cancel Push Button

  ELSE    && OutPut to Excel is No (Display Report )
    SET DELETE ON
    DO gfDispRe WITH EVALUATE('lcRpForm')
  ENDIF  
ELSE     && there is no records in Temp file
  *-- No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
SET CENTURY &lcSetCent
SET DELETE &lcSetDelet
*-- EndIf of no records in temp file (empty)

*!***************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : OG when function
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : = lfwRepWhen()
*!***************************************************************************
*
FUNCTION lfwRepWhen
lnPktQtyPo = lfVarPos('lcRpPktQty')           && Picked Quantity Var Position

lnSlRepPos = lfItmPos('ORDHDR.REP1')          && Sales Rep Position
lnDsgCdPos = lfItmPos('BOMVAR.CDSGNCODE')     && Design Code Position 
lnDsgClrPo = lfItmPos('ICDESIGN.CDSGNCLR')    && Design Color Position 

DO CASE
  CASE lcRpDate = 'E'
    lnEnDatPos = lfItmPos('ORDHDR.ENTERED')   && Entered Date Position
    lnDatePost = lnEnDatPos
    lcDate = [ORDHDR.ENTERED]
    lcFrPeriod = SUBSTR(laOGFxFlt[lnEnDatPos,6],1,10)
    lcToPeriod = SUBSTR(laOGFxFlt[lnEnDatPos,6],11,20)
  CASE lcRpDate = 'S'
    lnStDatPos = lfItmPos('ORDLINE.START')    && Start Date Position
    lnDatePost = lnStDatPos
    lcDate = [ORDLINE.START]
    lcFrPeriod = SUBSTR(laOGFxFlt[lnStDatPos,6],1,10)
    lcToPeriod = SUBSTR(laOGFxFlt[lnStDatPos,6],11,20)
  CASE lcRpDate = 'C'
    lnCmDatPos = lfItmPos('ORDLINE.COMPLETE') && Complete Rep Position
    lnDatePost = lnCmDatPos
    lcDate = [ORDLINE.COMPLETE]
    lcFrPeriod = SUBSTR(laOGFxFlt[lnCmDatPos,6],1,10)
    lcToPeriod = SUBSTR(laOGFxFlt[lnCmDatPos,6],11,20)
ENDCASE

lcRpExcel = IIF(lcRpFormat = "A","Format_A","Format_B")
*=lfvOutPut()
DO lpShowObj
DO lpCreatFil   && Create the Work Temp file
*-- End of lfwRepWhen.

*!***************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Create work File.
*!***************************************************************************
*! Example   : DO lpCreatFil
*!***************************************************************************
*
PROCEDURE lpCreatFil
*Design_Cod  --> Design Code
*Desg_Color  --> Design Color
*Desgn_Size  --> Design Size
*Desgn_Name  --> Design Name
*Total_Qty   --> Total Quantity (Opened)
*Tot_Order   --> Total Booked (Opened + Shipped)
*Pcs_Alloct  --> Total Picked (Allocated)
*Met_Alloct  --> Percentage in Order Header to Allow Allocation % on Stock
*Start       --> Ordline Start Date 
*Complete    --> Ordline Complete Date 
*nMonth1..6  --> Opened Quantity Distributed on 6 Preceding Months

CREATE TABLE (gcWorkDir+lcWorkFile) ;
  (Design_Cod C(6), Desg_Color C(15), Desgn_Size C(6), Desgn_Name C(30), ;
   Season C(6), Account C(5), Acct_Name C(30), Sales_Rep C(3),    ;
   Order C(6), Total_Qty N(15,0), Tot_Order N(15,0), Pcs_Alloct N(15,0), ;
   Met_Alloct N(6,2), Value N(15,2), Start D(8), Complete D(8),  ;
   cAdorContr C(8),nMonth1 N(15,0),nMonth2 N(15,0),nMonth3 N(15,0), ;
   nMonth4 N(15,0),nMonth5 N(15,0),nMonth6 N(15,0),Tot_Invoic N(15,0), ;
   cGroupKey C(24),Status C(1) )
**C200137,1   

SELECT (lcWorkFile)
INDEX ON cGroupKey + Design_Cod + Desg_Color TAG (lcWorkFile)
*-- End of lpCreatFil.

*!***************************************************************************
*! Name      : lfvFormat
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Change report format (Format A/Format B)
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : =lfvFormat()
*!***************************************************************************
*
FUNCTION lfvFormat
*Remember in case of removing this function we have to put 
*the above line in gfDispRe
lcRpForm = IIF(lcRpFormat = 'A','MFFRE10A','MFFRE10B')
CLEAR READ
*-- End of lfvFormat.

*!**************************************************************************
*! Name      : lfvSort
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Change index flag to reindex temp Table.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfvSort()
*!**************************************************************************
FUNCTION lfvSort
llChSortBy = .T.
IF lcRpSort1 = lcRpSort2
  DO CASE
    CASE lcRpSort2 = "C"
      lcRpSort2 = "S"
    CASE lcRpSort2 = "S"
      lcRpSort2 = "V"
    CASE lcRpSort2 = "V"
      lcRpSort2 = "C" 
  ENDCASE
  CLEAR READ
ENDIF
*-- End of lfvSort.

*!**************************************************************************
*! Name      : lfvOrdDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Change Order Date Type (Entered,Start,complete Date)
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : =lfvOrdDate()
*!**************************************************************************
*
FUNCTION lfvOrdDate
CLEAR READ
*-- End of lfvOrdDate.

*!***************************************************************************
*! Name      : lfvOutput
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Change Output to Excel file (Yes/No)
*!***************************************************************************
*! Called from : OG
*!***************************************************************************
*! Example   : =lfvOutput()
*!***************************************************************************
*
FUNCTION lfvOutput
IF llRpOutput
  SHOW GET pbOGPrevie DISABLED
  lcRpExcel = IIF(lcRpFormat = "A","Format_A","Format_B")
  lcRpExcel =PUTFILE('Excel File:',lcRpExcel, 'XLS')
ELSE
  SHOW GET pbOGPrevie ENABLED
ENDIF  
*-- End of lfvOutput.

*!**************************************************************************
*! Name      : lfGetReplc
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Get Replaced expression.
*!**************************************************************************
*! Called from : Report code section.
*!**************************************************************************
*! Returns     : lcExpr ---> which means (Sort by expression) 
*!**************************************************************************
*! Example   : =lfGetReplc()
*!**************************************************************************
*
FUNCTION lfGetReplc
PRIVATE lcExpr
*-- if Report Format is "A"
IF lcRpFormat = 'A'
  lcExpr = IIF(lcRpSort1="C",[Desg_Color + ],IIF(lcRpSort1="S",[Desgn_Size + ],;
           [cAdorContr + ])) + IIF(lcRpSort2="C",[Desg_Color],IIF(lcRpSort2 = ;
           "S",[Desgn_Size],[cAdorContr]))
ELSE        && Report Format is "B"
  lcExpr = [Design_Cod + Account + Order]

ENDIF  && Endif of Report Format
RETURN lcExpr
*-- End of lfGetReplc

*!***************************************************************************
*! Name      : lpShowObj
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Enable/Disable some options in OG 
*!***************************************************************************
*! Example   : DO lpShowObj
*!***************************************************************************
*
PROCEDURE lpShowObj

*-- If report format is 'A' 
IF lcRpFormat = 'A'
  laOGObjCnt[lnPktQtyPo] = .F.      && Disable Pict Ticket Quantity
  lcRpPktQty = "E"                  && Turn Pict Ticket Quantity to "Excluded" option
ELSE   && Else report format is 'B'
  laOGObjCnt[lnPktQtyPo] = .T.      && Enable Pict Ticket Quantity
ENDIF
*-- Endif of report format  

=lfOGShowGet('lcRpPktQty') && Show get Object .
*-- End of lpShowObj.

*!***************************************************************************
*! Name      : lfvStates
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Valid for the states selection
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Calls       : ....
*!***************************************************************************
*! Example     : = lfvStates()
*!***************************************************************************
*
FUNCTION lfvStates
PRIVATE lcFile_Ttl, lcBrfields, lcStateObj , lcStateVal , llRetVal

lcStateObj = SYS(18)                    && Varible to hold  the name of the memory variable used to create the current GET field
lcStateVal = PADR(EVALUATE(SYS(18)),6)  && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the state code he entered is not in the file.
IF '?' $ lcStateVal .OR. (!EMPTY(lcStateVal) .AND. !SEEK('N'+PADR('STATE',10) + ALLTRIM(lcStateVal),'CODES'))
   lnCurAlias = SELECT(0)
   *-- browse all country codes [begin]
   SELECT CODES
   DECLARE laCodeFld[2]
   lcFile_Ttl = 'State Codes'
   lcBrfields = 'cCode_No :H= "State Code" ,cDiscrep :H="Description" :30'
   
   llRetVal = gfBrows('FOR cDefCode + cfld_name+ccode_no+cdiscrep = "N" ;
              + "STATE" AND cRltField="N"','cCode_No,cDiscrep','laCodeFld')
   lcStateVal = IIF(llRetVal,laCodeFld[1],'')
   &lcStateObj = lcStateVal

   *-- browse all country codes [end]
   SELECT (lnCurAlias)
ENDIF
*-- End of lfvStates.

*!***************************************************************************
*! Name      : lfSRDsgClr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Control the InList for IcDesign file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRDsgClr()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRDsgClr
PARAMETERS lcParm

SELECT IcDesign
IF !FILE(gcWorkDir+lcDsgnIndx+'.CDX')
  INDEX ON cDsgnClr TAG (lcDsgnIndx) OF (gcWorkDir+lcDsgnIndx+'.CDX')
ENDIF

DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO TAG (lcDsgnIndx)      && restore old Index
    GO TOP
  CASE lcParm = 'R'  && Reset code
    SET ORDER TO TAG Icdesign
ENDCASE
*-- End of lfSRDsgClr.

*!***************************************************************************
*! Name      : lfSRDsgCod
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Control the InList for IcDesign file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRDsgCod()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRDsgCod
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT IcDesign
  GO TOP
ENDIF    
*-- End of lfSRDsgCod.

*!***************************************************************************
*! Name      : lfSRAcount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Control the InList for Account file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRAcount()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRAcount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF    
*-- End of lfSRAcount.

*!***************************************************************************
*! Name      : lfSRRepCod
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Control the InList for Sales Rep file
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRRepCod()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRRepCod
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT SalesRep
  GO TOP
ENDIF    
*-- End of lfSRRepCod.

*!***************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : to get the position of the fixed filter in OG
*!***************************************************************************
*! Called from : OG When Function 
*!***************************************************************************
*! Example   : = lfItmPos()
*!***************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!***************************************************************************
*! Name      : lfVarPos
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : get position of some variable in OG
*!***************************************************************************
*! Called from : OG When Function
*!***************************************************************************
*! Example   : = lfVarPos()
*!***************************************************************************
*
FUNCTION lfVarPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos
lnItmPos = ASCAN(laOGObjType,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGObjType,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfVarPos.

*!**************************************************************************
*! Name      : lfPrevRun
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call in place of the Functions called when
*!             Pressing <PREVIEW> or <RUN>
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfPrevRun()
*!**************************************************************************
*
FUNCTION lfPrevRun
PRIVATE llContinue
llContinue = .T.
llOGEscPrsd = .F.
lnOGHD=IIF(EMPTY(laOGHdFlt[1,1]),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(laOGFxFlt[1,1]),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(laOGVrFlt[1,1]),0,ALEN(laOGVrFlt,1))
lcOGSelect=SELECT()
=lfInitValue('laOGVrFlt','T')
=lfInitValue('laOGFXFlt','T')
  
lcOGHDFlt=gfGenFlt('laOGHDFlt' , llOGFilter , .T.)
lcOGFxFlt=gfGenFlt('laOGFxFlt' , llOGFilter , .T.)
lcOGVrFlt=gfGenFlt('laOGVrFlt' , llOGFilter , .T.)
  
lnOGHD=IIF(EMPTY(lcOGHDFlt),0,ALEN(laOGHdFlt,1))
lnOGFX=IIF(EMPTY(lcOGFxFlt),0,ALEN(laOGFxFlt,1))
lnOGvr=IIF(EMPTY(lcOGVrFlt),0,ALEN(laOGVrFlt,1))  
=lfInitValue('laOGVrFlt','C')
=lfInitValue('laOGFXFlt','C')

IF VARREAD() = "PBRUN"  
  lcRpExp=lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
        lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
        IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')
  IF !llRpOutPut
    llContinue = pSetup(.T.,llOGBatchOk)
    IF llContinue
      IF gcDevice = 'BATCH'
        SELECT SYUBSRVJ
        IF EMPTY(CREP_ID)
          lcRpRepID = lcOGRepID 
          SAVE  TO MEMO MUSRDVAR ALL LIKE L?RP*
          IF !EMPTY(laOGHDFLT[1,1])
            SAVE TO MEMO mRepHdFlt ALL LIKE laOGHDFLT
          ENDIF
          IF !EMPTY(laOGFxFLT[1,1])  
            =lfInitValue('laOGFxFlt','T')    
            SAVE TO MEMO mRepFxFlt ALL LIKE laOGFxFlt
            =lfInitValue('laOGFxFlt','C')      
          ENDIF  
          IF !EMPTY(laOGVrFLT[1,1])  
            =lfInitValue('laOGVrFlt','T')
            SAVE TO MEMO mRepVrFlt ALL LIKE laOGVrFlt  
            =lfInitValue('laOGVrFlt','C') 
          ENDIF  
          REPLACE CREP_ID WITH lcOGManRep,;
                  cstatus WITH 'O'      
        ENDIF
      ENDIF
    ENDIF  
  ENDIF  
ELSE
  IF llContinue
    lcOGTmpExp=IIF(!EMPTY(lcOGHDFlt),lcOGHDFlt+'|Ÿ',lcOGHDFlt)+IIF(!EMPTY(lcOGFXFlt) AND !EMPTY(lcOGVRFlt),lcOGFXFlt+'|Ÿ',lcOGFxFlt)+;
               lcOGVrFlt+IIF(!EMPTY(lcOGHDFlt),'Ÿ','')+IIF(!EMPTY(lcOGFXFlt)AND !EMPTY(lcOGVRFlt),'Ÿ','')
           
    IF EMPTY(lcOGFxFlt) AND EMPTY(lcOGVrFlt)
      lcRpExp=lcOgHdFlt
    ELSE
      lcRpExp=STRTRAN(STRTRAN(lcOGTmpExp,IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'|Ÿ','|Ÿ|Ÿ');
                ,' AND('),IIF(OCCURS('|Ÿ|Ÿ',lcOGTmpExp)=0,'Ÿ','ŸŸ'),')')
    ENDIF          

    lcOGDevice=gcDevice
    lcOGOutFile=gcOutFile
    gcDevice="SCREEN"    && set printer to temprory file name
    gcOutFile=gcWorkDir+gfTempName()+".TXT"      
    llOGHeader = glHeader
    glHeader = .T.
  ENDIF
ENDIF

IF llContinue
  lcRpExp=IIF(EMPTY(lcRpExp),'.T.',lcRpExp)
  lcogExc=SET('EXACT')        
  SET EXACT ON
  llOGFltCh = IIF(llOGFltCh,llOGFltCh,lcogfltr<>lcrpexp)        

  SET EXACT &lcOGExc
  IF llOGFltCh
    lcOGFltr=lcRpExp
  ENDIF
  SET CONSOLE OFF

  IF !EMPTY(lcOGFormV)
    IF !(lcOGLastForm==&lcOGFormV)
      
      lcOGFormArr='laRpObj'+SUBSTR(lcOGPlatForm,1,3)
      
      lcOGLastForm=&lcOGFormV
      llOGStyleCh=.T.
    ENDIF  

    IF llOGRefForm .OR. lcRepAvlM = 'B' .OR. llOGStyleCh
      =gfCrtFrm(EVAL(lcOGFormV),lcOGFormArr,llOGRefForm)
    ENDIF

  ENDIF  

  DO (gcRepHome+gcAct_Appl+'REPORT.APP') WITH lcOGPrgName, .T.

  SET CONSOLE ON

  IF !VARREAD() == "PBRUN")
    gcDevice=lcOGDevice
    gcOutFile=lcOGOutFile
    glHeader = llOGHeader
  ENDIF  

  llOGFltCh = .F.
ENDIF  
RETURN .F.
*-- End of lfPrevRun.

*!**************************************************************************
*! Name      : lfPrintTot
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call every group to store .F. to llPrintTot
*!             when Minimum allocation percentage equal Yes 
*!**************************************************************************
*! Called from : Report FRX Format B
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfPrintTot
llPrintTot = .F.
RETURN "No"
*-- End of lfPrintTot

*!**************************************************************************
*! Name      : lfReset
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call every group to reset llPrintTot Flag
*!**************************************************************************
*! Called from : Report FRX Format B
*!**************************************************************************
*! Example     : = lfReset()
*!**************************************************************************
*
FUNCTION lfReset
llPrintTot = .T.
RETURN ""
*-- End of lfReset

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/26/2000
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
SELECT IcDesign
CLOSE INDEXES

IF FILE(gcWorkDir+lcDsgnIndx+'.CDX')
  ERASE (gcWorkDir+lcDsgnIndx+'.CDX')
ENDIF

*-- Close temp. opended files, if it used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
  ERASE (gcWorkDir+lcWorkFile+'.DBF')
  ERASE (gcWorkDir+lcWorkFile+'.CDX')
ENDIF

IF USED(lcQueryFil)
  USE IN (lcQueryFil)
  ERASE (gcWorkDir+lcQueryFil+'.DBF')
  ERASE (gcWorkDir+lcQueryFil+'.CDX')
ENDIF
*-- End of lfClearRep.
