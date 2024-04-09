*:***************************************************************************
*: Program file  : POSTYSGM
*: Program desc. : STYLE SUMMARY REPORT FOR GMA.
*: System        : Aria Advantage Series.
*: Module        : PO
*: Developer     : MOHAMED SHOKRY (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POSTYSGM          C#200422
*:***************************************************************************
*: Modifications  : 
*B606954,6 03/19/2003 Do the following
*B606954,6            1) Add an option (OG) to include/exclude temp orders (Y/N). 
*B606954,6            2) For PO’s: make sure all PO’s are included in the calculations except cancelled PO’s
*B606954,6            3) TOTSTK QTY (Style sheet): Even If a style doesn’t have SO/PO for the specified 
*B606954,6               criteria, and it has Tot Invnt > 0 (from cut and sold, style screen) then this 
*B606954,6               style should show in the grid. 
*B607463,1 SSE 08/03/2003 Fix bug of taking long time in exporting to Excel Sheet.
*B119853,1 SSE 11/04/2003 Fix bug of placing extra columns in CSV file due to commas in Style Description.
*:***************************************************************************
*

*B606954,1 TMI [Start] Create the temp file names
lcTmpDtlSt = gfTempName()
lcTmpDtlSo = gfTempName()
lcTmpDtlPo = gfTempName()
*B606954,1 TMI [End  ] 

*--Check Path And File Name
IF lfChkPth()
  RETURN
ENDIF

*--create temp file
=lfPrepTemp()

*--Set Relations with Style File
=lfPrepRel()

*B606954,1 TMI [Start] Remove ship date and po date from the lcRpExp
PRIVATE lcInvCrit,lcPoCrit,lcOrdCrit
STORE ' ' TO lcInvCrit,lcPoCrit,lcOrdCrit
=lfRmvShpPo()
*B606954,1 TMI [End  ] 

*--Collect  needed Data
WAIT WINDOW 'Collecting data. Please wait.....' NOWAIT
=lfCollData()

*-- check if there is any record u selected
SELECT (lcTmpDtlSt)
LOCATE
IF EOF()
  SELECT (lcTmpDtlSo)
  LOCATE
  IF EOF()
    SELECT (lcTmpDtlPo)
    LOCATE 
    IF EOF()
      *---Text : 'No Record Selected for the report..!'
      =gfModalGen('TRM00052B00000','DIALOG')
      SET DEVICE TO SCREEN
      RETURN
    ENDIF
  ENDIF
ENDIF

*--Print to Excel Sheet
*B607463,1 Print to Excel Sheet if user selected Excel Format. [Begin]
*=lfPrint()
IF lcRpExpTyp = 'E'
  =lfPrint()
ELSE
  =gfModalGen("TRM000000B00000","DIALOG",'','','Finished Exporting To Text Files of Type CSV Format.')
ENDIF
*B607463,1 Print to Excel Sheet if user selected Excel Format. [End]


****************************************************************************
*! PROG      : lfPrint.PRG   (C#102584)
*! DESC      : Function to Print To EXCEl Sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrint

lcRpDtDir  = gcDataDir  && Save The Data Directory.
lcRpWrDir  = gcWorkDir  && Save The Work Directory.
lcRpSysDir = gcSysHome  && Save The system  Directory.
lcRpRpDir  = gcRepHome  && Save The report Directory.
lcRpComp   = gcAct_Comp && Save The Active Company.
lcRpDefPth = gcDef_Path && Save The DefPath.
lcRpTmpFl1 = lcTmpDtlSo && Save The TempName for  Sales.
lcRpTmpFl2 = lcTmpDtlSt && Save The TempName for Style.
lcRpTmpFl3 = lcTmpDtlPo && Save The TempName for Po's.

*--copy file POSTYSGM to our new file 
lcRpPath = ALLTRIM(lcRpPath)
IF RIGHT(lcRpPath,1)<>"\"
  lcRpPath = lcRpPath +"\"
ENDIF

*--to be sure that it is EXCEL Name
lcRpFilNam = ALLTRIM(lcRpFilNam)
IF RIGHT(lcRpFilNam,4)<>".XLS"
  lcRpFilNam= lcRpFilNam+".XLS"
ENDIF

lcRpDefPth = ALLTRIM(lcRpDefPth)
lcSorCop = ALLTRIM(lcRpDefPth) +'POSTYSUM.XLS'
lcTarCop = lcRpPath + lcRpFilNam
*B606954,1 TMI [Start] When user press preview twice with no time interval between the two presses 
*B606954,1             an error occures while creating the target Excel file,this loop to resolve this situation
PRIVATE lcErrStr,llError
lcErrStr = ON('ERROR')
llError = .T.
ON ERROR llError = .T.
DO WHILE llError
  llError = .F.
  *B606954,1 TMI [End  ]   
  COPY FILE &lcSorCop TO  &lcTarCop  
  *B606954,1 TMI [Start] if error occurs while copying , ask to continue or exit loop
  IF llError
    WAIT WINDOW 'Error while creating the Excel file, Press ESC to exit or press any other key to retry.'
    IF LASTKEY() = 27
      RETURN
    ENDIF
  ENDIF
ENDDO  
ON ERROR &lcErrStr
*B606954,1 TMI [End  ] 

lnLen = LEN(lcRpFilNam)
lcRpFilNam =LEFT(lcRpFilNam,lnLen-4)

*--count Po's
*B606954,1 TMI [Start] ignore using the variable "llOGFltCh"
*IF llOGFltCh
*B606954,1 TMI [End  ] 
  SELECT (lcTmpDtlSo)  
  USE
  SELECT (lcTmpDtlSt)
  USE
  SELECT (lcTmpDtlPo)
  USE
*B606954,1 TMI [Start] 
*ENDIF
*B606954,1 TMI [End  ] 
SAVE TO (gcWorkDir+lcTempMemo+'.MEM') ALL LIKE l?RP* 

lcCommLine = (gcWorkDir+lcTempMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"

IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
  ",gcRepHome+'PO\'+[POSTYVFP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF

RETURN

****************************************************************************
*! PROG      : lfCollData.PRG   (C#102584)
*! DESC      : Function to Collect Data
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfCollData
*--our Algrothem 
*-- select master file to collect data from STYLE

*B607463,1 Make sure it is 7 + one Char. for report type. [Begin]
IF lcRpExpTyp = 'T'
  PRIVATE lnHandle , lcTitle
  lcRpPath = ALLTRIM(lcRpPath)
  IF RIGHT(lcRpPath,1)<>"\"
    lcRpPath = lcRpPath +"\"
  ENDIF  
  lcOutPut = lcRpPath + ALLTRIM(SUBSTR(lcRpFilNam,1,7))
ENDIF  
*B607463,1 Make sure it is 7 + one Char. for report type. [End]

*-- Loop for Selected Options
*--For select to print Style Summ
IF "T" $ lcRpType
  *B607463,1 If user selected Text Format. [Begin]
  IF lcRpExpTyp = 'T'
    lnHandle = FCREATE(lcOutput+'T.CSV')
    IF lnHandle < 0  
      *-- Message : < Cannot open output file. >
      *-- Buttons : <           OK             >
      =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
    ENDIF
    =FWRITE(lnHandle,"Style Report:,,,,,,,,,,,,,,,,,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,",,,")
    =FWRITE(lnHandle,lcRpOptTtl)
    =FWRITE(lnHandle,",,,,,,,,,,,,,,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,"DEPARTMENT,STYLE #,Sty DESC,GROUP,Category,Class,Sub Class,Sub Class,Sub Class,")
    =FWRITE(lnHandle,"SHIPPED QTY,SHIPPED AMT,OPEN ORDER QTY,OPEN ORDER AMT,TOT PO Order QTY,")
    =FWRITE(lnHandle,"TOT PO Order AMT,TOT PO Recd QTY,TOT PO Recd AMT,TOT PO Intransit QTY,")
    =FWRITE(lnHandle,"TOT PO Intransit AMT,TOT PO Open QTY,TOT PO Open AMT,TOTSTK QTY,")
    =FWRITE(lnHandle,"TOTSTK AMT,OPEN TO SELL QTY ( - ),OPEN TO SELL QTY (+),")
    =FWRITE(lnHandle,"OPEN TO SELL AMT (-),OPEN TO SELL AMT(+)")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
  ENDIF  
  *B607463,1 If user selected Text Format. [End]

  =lfUpDtSty()
  
  *B607463,1 Add the line to the text file. [Begin]
  IF lcRpExpTyp = 'T'
    SELECT (lcTmpDtlSt)
    SCAN
      WAIT WINDOW 'Copying Style : ' + ALLTRIM(Style) + ' to text file.' NOWAIT

      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [Begin]
      *=FWRITE(lnHandle,cStyDept+","+Style+","+cStyDesc+","+cStyGroup+",")
      =FWRITE(lnHandle,cStyDept+","+Style+","+STRTRAN(cStyDesc,[,],[;])+","+STRTRAN(cStyGroup,[,],[;])+",")
      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [End]

      =FWRITE(lnHandle,CStyCat+","+CstyClss+","+CstyCls1+","+CstyCls2+","+CstyCls3+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NShpQty,7))+","+ALLTRIM(STR(NShpAmnt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NOpnQty,7))+","+ALLTRIM(STR(NOpnAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPoOrdQty,7))+","+ALLTRIM(STR(NPoOrdAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPoRcdQty,7))+","+ALLTRIM(STR(NpoRcdAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPoTrnQty,7))+","+ALLTRIM(STR(NpoTrnAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NpoOpnQty,7))+","+ALLTRIM(STR(NpoOpnAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NtotStkQty,7))+","+ALLTRIM(STR(NtotStkAmt,14,2))+",")    
      =FWRITE(lnHandle,ALLTRIM(STR(NNgOTSQty,7))+","+ALLTRIM(STR(NNgOTSAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPsOTSQty,7))+","+ALLTRIM(STR(NPsOTSAmt,14,2))+",")
      =FWRITE(lnHandle,CHR(13)+CHR(10))
      WAIT CLEAR
    ENDSCAN 
    =FCLOSE(lnHandle)
  ENDIF  
  *B607463,1 Add the line to the text file. [End]
ENDIF  
*--For select to print Sales  
IF  "S" $ lcRpType
  *B607463,1 If user selected Text Format. [Begin]
  IF lcRpExpTyp = 'T'
    lnHandle = FCREATE(lcOutput+'S.CSV')
    IF lnHandle < 0  
      *-- Message : < Cannot open output file. >
      *-- Buttons : <           OK             >
      =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
    ENDIF
    =FWRITE(lnHandle,"Sales Report:,,,,,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,",,,")
    =FWRITE(lnHandle,lcRpOptTtl)
    =FWRITE(lnHandle,",,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))                     
    =FWRITE(lnHandle,"DEPARTMENT,STYLE #,Sty DESC,GROUP,Category,Class,Sub Class,Sub Class,Sub Class,")
    =FWRITE(lnHandle,"Cust Acct Code,Cust Acct Name,SHIPPED QTY,SHIPPED AMT,OPEN ORDER QTY,OPEN ORDER AMT")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
  ENDIF
  *B607463,1 If user selected Text Format. [End]

  =lfUpDtSo('S')

  *B607463,1 Add the line to the text file. [Begin]
  IF lcRpExpTyp = 'T'
    SELECT (lcTmpDtlSo)
    SCAN
      WAIT WINDOW 'Copying Account + Style : ' + ALLTRIM(cAccount) + ',' + ALLTRIM(Style) + ' to text file.' NOWAIT

      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [Begin]
      *=FWRITE(lnHandle,cStyDept+","+Style+","+cStyDesc+","+cStyGroup+",")
      =FWRITE(lnHandle,cStyDept+","+Style+","+STRTRAN(cStyDesc,[,],[;])+","+STRTRAN(cStyGroup,[,],[;])+",")
      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [End]

      =FWRITE(lnHandle,CStyCat+","+CstyClss+","+CstyCls1+","+CstyCls2+","+CstyCls3+",")
      =FWRITE(lnHandle,cAccount+","+cCustNam+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NShpQty,7))+","+ALLTRIM(STR(NShpAmnt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NOpnQty,7))+","+ALLTRIM(STR(NOpnAmt,14,2))+",")
      =FWRITE(lnHandle,CHR(13)+CHR(10))
      WAIT CLEAR
    ENDSCAN 
    =FCLOSE(lnHandle)
  ENDIF
  *B607463,1 Add the line to the text file. [End]
ENDIF
    
*--For select to print Sales Po
IF "P" $ lcRpType 
  *B607463,1 If user selected Text Format. [Begin]
  IF lcRpExpTyp = 'T'
    lnHandle = FCREATE(lcOutput+'P.CSV')
    IF lnHandle < 0  
      *-- Message : < Cannot open output file. >
      *-- Buttons : <           OK             >
      =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open output file.')
    ENDIF
    =FWRITE(lnHandle,"Purchase Report: ,,,,,,,,,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,",,,")
    =FWRITE(lnHandle,lcRpOptTtl)
    =FWRITE(lnHandle,",,,,,,,,,,,,,,,")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,CHR(13)+CHR(10))
    =FWRITE(lnHandle,"DEPARTMENT,STYLE #,Sty DESC,GROUP,Category,Class,Sub Class,Sub Class,Sub Class,")
    =FWRITE(lnHandle,"Vendor Code,Vendor Name,TOT PO Order QTY,TOT PO Order AMT,TOT PO Recd QTY,")
    =FWRITE(lnHandle,"TOT PO Recd AMT,TOT PO Intransit QTY,TOT PO Intransit AMT,TOT PO Open QTY,TOT PO Open AMT")
    =FWRITE(lnHandle,CHR(13)+CHR(10))
  ENDIF
  *B607463,1 If user selected Text Format. [End]

  =lfUpDtPo('P')

  *B607463,1 Add the line to the text file. [Begin]
  IF lcRpExpTyp = 'T'
    SELECT (lcTmpDtlPo)
    SCAN
      WAIT WINDOW 'Copying Vendor + Style : ' + ALLTRIM(cVenCode) + ',' + ALLTRIM(Style) + ' to text file.' NOWAIT

      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [Begin]
      *=FWRITE(lnHandle,cStyDept+","+Style+","+cStyDesc+","+cStyGroup+",")
      =FWRITE(lnHandle,cStyDept+","+Style+","+STRTRAN(cStyDesc,[,],[;])+","+STRTRAN(cStyGroup,[,],[;])+",")
      *B119853,1 SSE 11/04/2003 Replace all Commas in Style Desc. field with Semi Commas. [End]

      =FWRITE(lnHandle,CStyCat+","+CstyClss+","+CstyCls1+","+CstyCls2+","+CstyCls3+",")

      *B119853,1 SSE 11/04/2003 Replace all Commas in Vendor Name. field with Semi Commas. [Begin]
      *=FWRITE(lnHandle,cVenCode+","+cVennam+",")
      =FWRITE(lnHandle,cVenCode+","+STRTRAN(cVennam,[,],[;])+",")
      *B119853,1 SSE 11/04/2003 Replace all Commas in Vendor Name. field with Semi Commas. [End]

      =FWRITE(lnHandle,ALLTRIM(STR(NPoOrdQty,7))+","+ALLTRIM(STR(NPoOrdAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPorcdQty,7))+","+ALLTRIM(STR(NPorcdAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPoTrnQty,7))+","+ALLTRIM(STR(NPoTrnAmt,14,2))+",")
      =FWRITE(lnHandle,ALLTRIM(STR(NPoOpnQty,7))+","+ALLTRIM(STR(NPoOpnAmt,14,2))+",")
      =FWRITE(lnHandle,CHR(13)+CHR(10))
      WAIT CLEAR
    ENDSCAN 
    =FCLOSE(lnHandle)
  ENDIF
  *B607463,1 Add the line to the text file. [End]
ENDIF

****************************************************************************
*! PROG      : lfUpDtSty.PRG   (C#102584)
*! DESC      : Function Update Style Temp File
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfUpDtSty
PRIVATE lcInvOrd
lcInvOrd = ORDER('INVLINE')
*B606954,4 TMI [Start] 
*SET ORDER TO INVLINEO IN INVLINE
*B606954,4 TMI [End  ]

SELECT STYLE
*B606954,6 TMI [Start] Enhance by removing macro substitution with "Eval" funtion 
*SCAN FOR &lcRpExp
SCAN FOR EVAL(lcRpExp)
  *B606954,6 TMI [End  ] 
  *B606954,1 TMI [Start] Add here the code for Shipping and PO's of the style
  *B606954,1              Depending on the new option "lnRpShip" a style is not added for example 
  *B606954,1             if it has open orders and ship qty is 0
  
  PRIVATE lnShipQty,lnShipAmt,lnOpnQty,lnOpnAmt
  STORE 0 TO lnShipQty,lnShipAmt,lnOpnQty,lnOpnAmt

  *-- Open qtys
  SELECT ORDLINE  
  *B606954,6 TMI [Start] Enhance by useing the seek in an IF statment
  *=SEEK(STYLE.STYLE,'ORDLINE')
  IF SEEK(STYLE.STYLE,'ORDLINE')
    *B606954,6 TMI [End  ] 
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = STYLE.STYLE ;
               FOR ORDLINE.TOTQTY > 0 AND ORDHDR.STATUS $ "OH" 
      *B606954,6 TMI [Start] Exclude Temp orders Depending on the new option "llRpIncTmp"
      IF !llRpIncTmp AND ORDLINE.CORDTYPE = 'T'
        LOOP
      ENDIF
      *B606954,6 TMI [End  ] 
      lnOpnQty = ORDLINE.TOTQTY +lnOpnQty
      lnOpnAmt = ORDLINE.TOTQTY * ORDLINE.PRICE +lnOpnAmt
      *B606954,4 TMI [Start] 
      *- IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE') 
      *-   SELECT INVLINE
      *-   SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) ;
      *-             FOR   &lcInvCrit 
      *-     lnShipQty = lnShipQty + INVLINE.TOTQTY
      *-     lnShipAmt = lnShipAmt + INVLINE.TOTQTY * INVLINE.PRICE
      *-   ENDSCAN
      *- ENDIF
      *B606954,4 TMI [End  ] 
    ENDSCAN
    *B606954,6 TMI [Start] Close the if seek statment    
  ENDIF
  *B606954,6 TMI [End  ] 
  
  *B606954,4 TMI [Start]   
  IF SEEK(STYLE.STYLE,'INVLINE') 
    SELECT INVLINE
    SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = STYLE.STYLE ;
              FOR   INVHDR.STATUS = 'C' AND &lcInvCrit 
      lnShipQty = lnShipQty + INVLINE.TOTQTY
      lnShipAmt = lnShipAmt + INVLINE.TOTQTY * INVLINE.PRICE
    ENDSCAN
  ENDIF
  *B606954,4 TMI [End  ] 

  *-- Po's
  PRIVATE lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
  PRIVATE lnpoOpnQty,lnpoOpnAmt

  STORE 0 TO lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
  STORE 0 TO lnpoOpnQty,lnpoOpnAmt
  

  SELECT POSLN
  =SEEK(STYLE.STYLE)
  *B606954,6 TMI [Start] Include all PO's except Canceled ones
  *SCAN REST WHILE STYLE = STYLE.STYLE ;
            FOR POSHDR.STATUS $ "OHA" AND &lcPoCrit
  SCAN REST WHILE STYLE = STYLE.STYLE ;
            FOR POSHDR.STATUS # "X" AND EVAL(lcPoCrit)
    *B606954,6 TMI [End  ] 
    IF POSLN.TRANCD = '1'
      lnPoOrdQty  = POSLN.TOTQTY + lnPoOrdQty 
      lnPoOrdAmt  = POSLN.TOTQTY * POSLN.NCOST1 +lnPoOrdAmt
    ENDIF
     
    IF POSLN.TRANCD = '2'
      lnPoRcdQty  = POSLN.TOTQTY + lnPoRcdQty 
      lnpoRcdAmt  = POSLN.TOTQTY * POSLN.NCOST1 +lnpoRcdAmt 
    ENDIF

    IF POSLN.TRANCD = '3'
      lnPoTrnQty = POSLN.TOTQTY + lnPoTrnQty
      lnpoTrnAmt = POSLN.TOTQTY * POSLN.NCOST1 +lnpoTrnAmt
    ENDIF
    
    lnpoOpnQty = lnPoOrdQty  -lnPoRcdQty  
    lnpoOpnAmt = lnPoOrdAmt  -lnpoRcdAmt  
  ENDSCAN

  *-- Examine ship status for styles
  DO CASE
    CASE lcRPShip = "B"
      *-- If no open PO's , no open Sales orders , or no shipments ignore this style
      *B606954,6 TMI [Start] Include styles with TOTSTK>0 event they have no SO's or PO's      
      *IF lnpoOpnQty + lnOpnQty + lnShipQty = 0
      IF lnpoOpnQty + lnOpnQty + lnShipQty = 0 .AND. STYLE.TOTSTK = 0
        *B606954,6 TMI [End  ] 
        LOOP
      ENDIF

    CASE lcRPShip = "S"
      *-- if a style has open sales order qty here we need only styles with shipped qty>0
      *B606954,6 TMI [Start] Include styles with TOTSTK>0 event they have no SO's or PO's      
      *IF lnShipQty = 0
      IF lnShipQty = 0 .AND. STYLE.TOTSTK = 0
        *B606954,6 TMI [End  ] 
        LOOP
      ENDIF 
    
    CASE lcRPShip = "N"
      *-- if a style has open sales order qty here we need completly unshipped styles
      *B606954,6 TMI [Start] Include styles with TOTSTK>0 event they have no SO's or PO's      
      *IF lnShipQty > 0
      IF lnShipQty > 0 .AND. STYLE.TOTSTK = 0
        *B606954,6 TMI [End  ] 
        LOOP
      ENDIF       
  ENDCASE    
  *B606954,1 TMI [End  ] 

  SELECT (lcTmpDtlSt)
  APPEND BLANK
  *B606954,1 TMI [Start] Comment this line
  *-* REPLACE Style       WITH Style.Style,;
  *-*         cStyDept    WITH Style.Dept,;
  *-*         cStyDesc    WITH Style.Desc,;
  *-*         cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
  *-*         CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
  *-*         CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
  *-*         CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
  *-*         CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
  *-*         CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
  *-*         NtotStkQty  WITH Style.TotStk,;
  *-*         NtotStkAmt  WITH Style.TotStk*Style.Ave_Cost,;
  *-*         NNgOTSQty   WITH -1*(Style.TotWIP+Style.TotStk-Style.TotOrd),;
  *-*         NNgOTSAmt   WITH -1*(Style.TotWIP+Style.TotStk-Style.TotOrd)*Style.PriceA,;
  *-*         NPsOTSQty   WITH (Style.TotWIP+Style.TotStk-Style.TotOrd),;
  *-*         NPsOTSAmt   WITH (Style.TotWIP+Style.TotStk-Style.TotOrd)*Style.PriceA
  *--Comment these function with parameter 'T'
  *-* *--For select to print Sales Order 
  *-* =lfUpDtSo('T')
  *-* 
  *-* *--For select to print Po's Order   
  *-* =lfUpDtPo('T')
  PRIVATE lnQty,lnAmt
  *--Update  shipp qtys
  SELECT (lcTmpDtlSt)
  lnQty = Style.TotWIP+Style.TotStk-Style.TotOrd
  lnAmt = (Style.TotWIP+Style.TotStk-Style.TotOrd)*Style.PriceA
  
  *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
  *REPLACE Style       WITH Style.Style,;
  *        cStyDept    WITH Style.Dept,;
  *        cStyDesc    WITH Style.Desc,;
  *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
  *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
  *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
  *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
  *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
  *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
  *        NtotStkQty  WITH Style.TotStk,;
  *        NtotStkAmt  WITH Style.TotStk*Style.Ave_Cost,;
  *        NNgOTSQty   WITH IIF( lnQty < 0 , lnQty , 0 ),;
  *        NNgOTSAmt   WITH IIF( lnAmt < 0 , lnAmt , 0 ),;
  *        NPsOTSQty   WITH IIF( lnQty > 0 , lnQty , 0 ),;
  *        NPsOTSAmt   WITH IIF( lnAmt > 0 , lnAmt , 0 ),;
  *        NShpQty     WITH lnShipQty ,;
  *        NShpAmnt    WITH lnShipAmt ,;
  *        NOpnQty     WITH lnOpnQty ,;
  *        NOpnAmt     WITH lnOpnAmt ,;
  *        NPoOrdQty   WITH lnPoOrdQty,;
  *        NPoOrdAmt   WITH lnPoOrdAmt,;
  *        NPoRcdQty   WITH lnPoRcdQty,;
  *        NpoRcdAmt   WITH lnpoRcdAmt,;
  *        NPoTrnQty   WITH lnPoTrnQty,;
  *        NpoTrnAmt   WITH lnpoTrnAmt,;
  *        NpoOpnQty   WITH lnpoOpnQty,;
  *        NpoOpnAmt   WITH lnpoOpnAmt
  REPLACE Style       WITH Style.Style,;
          cStyDept    WITH Style.Dept,;
          cStyDesc    WITH Style.Desc1,;
          cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
          CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
          CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
          CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
          CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
          CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
          NtotStkQty  WITH Style.TotStk,;
          NtotStkAmt  WITH Style.TotStk*Style.Ave_Cost,;
          NNgOTSQty   WITH IIF( lnQty < 0 , lnQty , 0 ),;
          NNgOTSAmt   WITH IIF( lnAmt < 0 , lnAmt , 0 ),;
          NPsOTSQty   WITH IIF( lnQty > 0 , lnQty , 0 ),;
          NPsOTSAmt   WITH IIF( lnAmt > 0 , lnAmt , 0 ),;
          NShpQty     WITH lnShipQty ,;
          NShpAmnt    WITH lnShipAmt ,;
          NOpnQty     WITH lnOpnQty ,;
          NOpnAmt     WITH lnOpnAmt ,;
          NPoOrdQty   WITH lnPoOrdQty,;
          NPoOrdAmt   WITH lnPoOrdAmt,;
          NPoRcdQty   WITH lnPoRcdQty,;
          NpoRcdAmt   WITH lnpoRcdAmt,;
          NPoTrnQty   WITH lnPoTrnQty,;
          NpoTrnAmt   WITH lnpoTrnAmt,;
          NpoOpnQty   WITH lnpoOpnQty,;
          NpoOpnAmt   WITH lnpoOpnAmt
  *B607463,1 Replace Style Desc. with Style Long Desc. [End]

  *B606954,1 TMI [End  ] 
ENDSCAN

****************************************************************************
*! PROG      : lfUpDtSo.PRG   (C#102584)
*! DESC      : Function Update So(ordeline and invline in TemFile
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*B606954,1 TMI [Start] reWrite this function 
*FUNCTION lfUpDtSo
FUNCTION x_lfUpDtSo
*B606954,1 TMI [End  ] 
PARA lcType
PRIVATE lnShipQty,lnShipAmt,lnOpnQty,lnOpnAmt
STORE 0 TO lnShipQty,lnShipAmt,lnOpnQty,lnOpnAmt

*--mhm2003
*--Case get the value of the Start date.
lnStrtPos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'INVHDR.SHIPDATE'),1)
IF lnStrtPos > 0
  *--mhm C#200481 get Invoice date [Start]
  IF !EMPTY(SUBSTR(laOGFxFlt[2,6],1,10)) AND !EMPTY(SUBSTR(laOGFxFlt[2,6],12,10))
    ldLow =  CTOD(SUBSTR(laOGFxFlt[2,6],1,10))
    ldHigh = CTOD(SUBSTR(laOGFxFlt[2,6],12,10))
  ENDIF  
ENDIF
*----mhm2003

*--Update Style Summ
*--if called from Style case
IF lcType = 'T'
  *--mhm2000
  IF lcRPShip $"SB"
  *--mhm2000
    SELECT INVLINE
    SCAN REST WHILE STYLE = STYLE.STYLE
      *--MHM2003
      IF !EMPTY(ldLow) AND !EMPTY(ldHigh)
        IF !BETWEEN(INVHDR.SHIPDATE,ldLow,ldHigh)
          LOOP
        ENDIF
      ENDIF  
      *--MHM2003
      lnShipQty = INVLINE.TOTQTY +lnShipQty
      lnShipAmt = INVLINE.TOTQTY * INVLINE.PRICE +lnShipAmt
    ENDSCAN
  *--mhm2000
  ENDIF  
  *--mhm2000

  *--mhm2000
  IF lcRPShip $ "NB"
    SELECT INVLINE
    SET ORDER TO Invlineo
    *--mhm2000
    SELECT ORDLINE
    =SEEK(STYLE.STYLE)
    SCAN REST WHILE STYLE = STYLE.STYLE
      IF SEEK("O"+Ordline.Order,'ORDHDR') AND (ORDHDR.STATUS $ "OH") 
        *--MHM2003
        IF !EMPTY(ldLow) AND !EMPTY(ldHigh)
          =SEEK(Ordline.order+STR(Ordline.lineno,6),'INVLINE')
          =SEEK(INVLINE.INVOICE,'INVHDR')
          IF !BETWEEN(INVHDR.SHIPDATE,ldLow,ldHigh)
            LOOP
          ENDIF
        ENDIF  
        *--MHM2003
        lnOpnQty = ORDLINE.TOTQTY +lnOpnQty
        lnOpnAmt = ORDLINE.TOTQTY * ORDLINE.PRICE +lnOpnAmt
      ENDIF  
    ENDSCAN
  *--mhm2000
    SELECT INVLINE
    SET ORDER TO Invlineo
  ENDIF  
  *--mhm2000
  SELECT (lcTmpDtlSt)
  REPLACE NShpQty    WITH lnShipQty ,;
          NShpAmnt   WITH lnShipAmt ,;
          NOpnQty    WITH lnOpnQty ,;
          NOpnAmt    WITH lnOpnAmt
  
ELSE  && Update So Report
  *--get Related Data from other Tables (which has not relation with Style File) 
  IF ("T" $ lcRpType )
    SELECT STYLE 
    SET RELATION OFF INTO Ordline    
    SELECT ORDLINE
    SET RELATION TO Style INTO STYLE
  ENDIF 
  
  SELECT ORDLINE
  SCAN FOR &lcRpExp
      
    IF SEEK("O"+Ordline.Order,'ORDHDR') AND (ORDHDR.STATUS $ "OH")
      IF !SEEK(Account+Style,lcTmpDtlSo)
        =SEEK('M'+Ordline.Account,'CUSTOMER')
        SELECT (lcTmpDtlSo)
        APPEND BLANK
        *--mhm2000
        *REPLACE Style       WITH Style.Style,;
                cStyDept    WITH Style.Dept,;
                cStyDesc    WITH Style.Desc,;
                cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
                CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
                CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
                CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
                CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
                CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
                cAccount    WITH Ordline.Account,;
                CCustNam    WITH Customer.BTNAME,;
                NOpnQty    WITH ORDLINE.TOTQTY,;
                NOpnAmt    WITH ORDLINE.TOTQTY * ORDLINE.PRICE
        
        *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
        *REPLACE Style       WITH Style.Style,;
        *        cStyDept    WITH Style.Dept,;
        *        cStyDesc    WITH Style.Desc,;
        *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
        *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
        *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
        *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
        *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
        *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
        *        cAccount    WITH Ordline.Account,;
        *        CCustNam    WITH Customer.BTNAME
        REPLACE Style       WITH Style.Style,;
                cStyDept    WITH Style.Dept,;
                cStyDesc    WITH Style.Desc1,;
                cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
                CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
                CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
                CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
                CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
                CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
                cAccount    WITH Ordline.Account,;
                CCustNam    WITH Customer.BTNAME
        *B607463,1 Replace Style Desc. with Style Long Desc. [End]
        *--mhm2000

        *--we will get data from detail band         
        *--mhm2000
        IF lcRPShip $"SB"
        *--mhm2000
          SELECT INVLINE
          SCAN REST WHILE STYLE = STYLE.STYLE
            *--MHM2003
            IF !EMPTY(ldLow) AND !EMPTY(ldHigh)
              IF !BETWEEN(INVHDR.SHIPDATE,ldLow,ldHigh)
                LOOP
              ENDIF
            ENDIF  
            *--MHM2003
            lnShipQty = INVLINE.TOTQTY +lnShipQty
            lnShipAmt = INVLINE.TOTQTY * INVLINE.PRICE +lnShipAmt
          ENDSCAN
        *--mhm2000
        ENDIF
        *--mhm2000

        *--mhm2000
        IF lcRPShip $"NB"
          SELECT INVLINE
          SET ORDER TO Invlineo
        *--mhm2000
          SELECT ORDLINE
          =SEEK(STYLE.STYLE)
          SCAN REST WHILE STYLE = STYLE.STYLE
            IF SEEK("O"+Ordline.Order,'ORDHDR') AND (ORDHDR.STATUS $ "OH") 
              *--MHM2003
              IF !EMPTY(ldLow) AND !EMPTY(ldHigh)
                =SEEK(Ordline.order+STR(Ordline.lineno,6),'INVLINE')
                =SEEK(INVLINE.INVOICE,'INVHDR')
                IF !BETWEEN(INVHDR.SHIPDATE,ldLow,ldHigh)
                  LOOP
                ENDIF
              ENDIF  
              *--MHM2003
              lnOpnQty = ORDLINE.TOTQTY +lnOpnQty
              lnOpnAmt = ORDLINE.TOTQTY * ORDLINE.PRICE +lnOpnAmt
            ENDIF  
          ENDSCAN
        *--mhm2000
          SELECT INVLINE
          SET ORDER TO Invlineo
        ENDIF  
        *--mhm2000

        SELECT (lcTmpDtlSo)
        REPLACE NShpQty    WITH lnShipQty ,;
                NShpAmnt   WITH lnShipAmt 
      ELSE
        *--mhm2000
        IF lcRPShip $"NB"
        *--mhm2000
          SELECT (lcTmpDtlSo)
          REPLACE NOpnQty    WITH NOpnQty + ORDLINE.TOTQTY,;
                  NOpnAmt    WITH NOpnAmt + ORDLINE.TOTQTY * ORDLINE.PRICE
        *--mhm2000
        ENDIF          
        *--mhm2000
    
      ENDIF
    ENDIF
  ENDSCAN
  
ENDIF
****************************************************************************
*! PROG      : lfUpDtPo.PRG   (C#102584)
*! DESC      : Function Update PO(POSLN) in TemFile
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*B606954,1 TMI [Start] reWrite this function
*FUNCTION lfUpDtPo
FUNCTION x_lfUpDtPo
*B606954,1 TMI [End  ] 
PARA lcType

PRIVATE lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
PRIVATE lnpoOpnQty,lnpoOpnAmt

STORE 0 TO lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
STORE 0 TO lnpoOpnQty,lnpoOpnAmt

*--Update Style Summ
IF lcType = 'T'

  SELECT POSLN
  =SEEK(STYLE.STYLE)
  SCAN REST WHILE STYLE = STYLE.STYLE 
    =SEEK('P'+POSLN.PO,'POSHDR')  
    IF POSHDR.STATUS $ "OHA"

      IF POSLN.TRANCD = '1'
        lnPoOrdQty  = POSLN.TOTQTY +lnPoOrdQty 
        lnPoOrdAmt  = POSLN.TOTQTY * POSLN.NCOST1 +lnPoOrdAmt
      ENDIF
     
      IF POSLN.TRANCD = '2'
        lnPoRcdQty  = POSLN.TOTQTY +lnPoRcdQty 
        lnpoRcdAmt  = POSLN.TOTQTY * POSLN.NCOST1 +lnpoRcdAmt 
      ENDIF

      IF POSLN.TRANCD = '3'
        lnPoTrnQty = POSLN.TOTQTY +lnPoTrnQty
        lnpoTrnAmt = POSLN.TOTQTY * POSLN.NCOST1 +lnpoTrnAmt
      ENDIF
    
      lnpoOpnQty = lnPoOrdQty  -lnPoRcdQty  
      lnpoOpnAmt = lnPoOrdAmt  -lnpoRcdAmt  
    ENDIF  
  ENDSCAN

  SELECT (lcTmpDtlSt)
  REPLACE NPoOrdQty WITH lnPoOrdQty,;
          NPoOrdAmt WITH lnPoOrdAmt,;
          NPoRcdQty WITH lnPoRcdQty,;
          NpoRcdAmt WITH lnpoRcdAmt,;
          NPoTrnQty WITH lnPoTrnQty,;
          NpoTrnAmt WITH lnpoTrnAmt,;
          NpoOpnQty WITH lnpoOpnQty,;
          NpoOpnAmt WITH lnpoOpnAmt

ELSE  && Update So Report
  *--get Related Data from other Tables (which has not relation with Style File) 

  IF ("T" $ lcRpType )
    SELECT STYLE 
    SET RELATION OFF INTO POSLN
    SELECT POSLN
    SET RELATION TO Style INTO STYLE
  ENDIF 
  SELECT POSLN
  SCAN FOR &lcRpExp
    =SEEK('P'+POSLN.PO,'POSHDR')  
    IF POSHDR.STATUS $ "OHA"
  
      IF !SEEK(Vendor+Style,lcTmpDtlPo)
        =SEEK(PoslN.vendor,'APVENDOR')
        SELECT (lcTmpDtlPo)
        APPEND BLANK
        
        *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
        *REPLACE Style       WITH Style.Style,;
        *        cStyDept    WITH Style.Dept,;
        *        cStyDesc    WITH Style.Desc,;
        *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
        *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
        *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
        *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
        *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),; 
        *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
        *        CVenCode    WITH POSLN.Vendor,;
        *        CVenNam     WITH APVENDOR.cvencomp
        REPLACE Style       WITH Style.Style,;
                cStyDept    WITH Style.Dept,;
                cStyDesc    WITH Style.Desc1,;
                cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
                CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
                CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
                CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
                CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),; 
                CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
                CVenCode    WITH POSLN.Vendor,;
                CVenNam     WITH APVENDOR.cvencomp
        *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]

        IF POSLN.TRANCD = '1'
          lnPoOrdQty  = POSLN.TOTQTY
          lnPoOrdAmt  = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
    
        IF POSLN.TRANCD = '2'
          lnPoRcdQty  = POSLN.TOTQTY
          lnpoRcdAmt  = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
 
        IF POSLN.TRANCD = '3'
          lnPoTrnQty = POSLN.TOTQTY
          lnpoTrnAmt = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
    
        lnpoOpnQty = lnPoOrdQty  -lnPoRcdQty  
        lnpoOpnAmt = lnPoOrdAmt  -lnpoRcdAmt  

        SELECT (lcTmpDtlPo)
        REPLACE NPoOrdQty WITH lnPoOrdQty,;
                NPoOrdAmt WITH lnPoOrdAmt,;
                NPoRcdQty WITH lnPoRcdQty,;
                NpoRcdAmt WITH lnpoRcdAmt,;
                NPoTrnQty WITH lnPoTrnQty,;
                NpoTrnAmt WITH lnpoTrnAmt,;
                NpoOpnQty WITH lnpoOpnQty,;
                NpoOpnAmt WITH lnpoOpnAmt
      ELSE
        IF POSLN.TRANCD = '1'
          lnPoOrdQty  = POSLN.TOTQTY
          lnPoOrdAmt  = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
    
        IF POSLN.TRANCD = '2'
          lnPoRcdQty  = POSLN.TOTQTY
          lnpoRcdAmt  = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
 
        IF POSLN.TRANCD = '3'
          lnPoTrnQty = POSLN.TOTQTY
          lnpoTrnAmt = POSLN.TOTQTY * POSLN.NCOST1
        ENDIF
    
        lnpoOpnQty = lnPoOrdQty  -lnPoRcdQty  
        lnpoOpnAmt = lnPoOrdAmt  -lnpoRcdAmt  

        SELECT (lcTmpDtlPo)
        REPLACE NPoOrdQty WITH lnPoOrdQty+NPoOrdQty,;
                NPoOrdAmt WITH lnPoOrdAmt+NPoOrdAmt,;
                NPoRcdQty WITH lnPoRcdQty+NPoRcdQty,;
                NpoRcdAmt WITH lnpoRcdAmt+NpoRcdAmt,;
                NPoTrnQty WITH lnPoTrnQty+NPoTrnQty,;
                NpoTrnAmt WITH lnpoTrnAmt+NpoTrnAmt,;
                NpoOpnQty WITH lnpoOpnQty+NpoOpnQty,;
                NpoOpnAmt WITH lnpoOpnAmt+NpoOpnAmt

      ENDIF          
    ENDIF
  ENDSCAN
ENDIF

****************************************************************************
*! PROG      : lfPrepRel.PRG   (C#102584)
*! DESC      : Function to Prepar Relation With Style File
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
*B606954,1 TMI [Start] rewrite this function 
*FUNCTION lfPrepRel
FUNCTION x_lfPrepRel
*B606954,1 TMI [End  ] 
IF "T" $ lcRpType 
  SELECT STYLE 
  SET RELATION TO Style INTO POSLN ADDI
  SET RELATION TO Style INTO ORDLINE ADDI
  SET RELATION TO Style INTO INVLINE ADDI

  SELECT INVLINE
  SET RELATION TO invoice INTO INVHDR ADDI

  SELECT POSLN
  SET RELATION TO cStyType+Po INTO POSHDR ADDI
  
ENDIF

*-relation of sales 
IF "S" $ lcRpType 
  IF !("T" $ lcRpType )
    SELECT ORDLINE
    SET RELATION TO Style INTO STYLE
    SELECT INVLINE
    SET RELATION TO invoice INTO INVHDR ADDI
  ENDIF 

  SELECT STYLE 
  SET RELATION TO Style INTO INVLINE

ENDIF

*--relation of Po
IF "P" $ lcRpType 
  
  IF !("T" $ lcRpType )
    SELECT POSLN
    SET RELATION TO Style INTO STYLE
    SET RELATION TO cStyType+Po INTO POSHDR ADDI
  ENDIF  
  
ENDIF

*:*************************************************************
*: Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
*: Purpose   : When function of OG
*:*************************************************************
*: Called from : OG read cycle
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfwRepWhen()
*:*************************************************************
*
FUNCTION lfwRepWhen
*--DECLARE array to store order status and make mover
DECLARE laRpSource[3],laRpTarget[1]
STORE 'Purchase'     TO laRpSource[1]
STORE 'Sales'        TO laRpSource[2]
STORE 'Style'        TO laRpSource[3]

****************************************************************************
*! PROG      : lfPrepTemp.PRG   (C#102584)
*! DESC      : Function to Prepar temp file to print detail to Excel sheet
*! Developer : Mohamed Shokry (MHM)
*! Date      : 02/10/2002
****************************************************************************
*! Called from : 
****************************************************************************
*! Calls       : 
*!              Procedures : ....
*!              Functions  : 
****************************************************************************
*! Passed Parameters : None
****************************************************************************
*! Return      : ...
****************************************************************************
FUNCTION lfPrepTemp

DIMENSION laTempStru[1,4]
laTempStru = ''

DIMENSION laTempStru[ALEN(laTempStru,1) , 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyDept'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 5
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'Style'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 19
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyDesc'
laTempStru[ALEN(laTempStru,1),2] = 'C'

*B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
*laTempStru[ALEN(laTempStru,1),3] = 20
laTempStru[ALEN(laTempStru,1),3] = 60
*B607463,1 Replace Style Desc. with Style Long Desc. [End]

laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyGroup'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCat'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyClss'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls1'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls2'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls3'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NShpQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NShpAmnt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NOpnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NOpnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoOrdQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoOrdAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoRcdQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoRcdAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoTrnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoTrnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoOpnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoOpnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NtotStkQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NtotStkAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NNgOTSQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NNgOTSAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPsOTSQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPsOTSAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

CREATE DBF (gcWorkDir+lcTmpDtlSt)FROM ARRAY laTempStru
INDEX ON  Style TAG cStyIndx ADDITIVE

*-- For Po
DIMENSION laTempStru[1,4]
laTempStru = ''

DIMENSION laTempStru[ALEN(laTempStru,1) , 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyDept'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 5
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'Style'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 19
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyDesc'
laTempStru[ALEN(laTempStru,1),2] = 'C'

*B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
*laTempStru[ALEN(laTempStru,1),3] = 20
laTempStru[ALEN(laTempStru,1),3] = 60
*B607463,1 Replace Style Desc. with Style Long Desc. [End]

laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyGroup'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCat'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1)+1 , 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyClss'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls1'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls2'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls3'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CVenCode'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 8
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CVenNam'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoOrdQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoOrdAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoRcdQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoRcdAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NPoTrnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoTrnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoOpnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NpoOpnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2


CREATE DBF (gcWorkDir+lcTmpDtlPo)FROM ARRAY laTempStru
INDEX ON  cVenCode+ Style TAG cVnStyIndx ADDITIVE

*- for So
DIMENSION laTempStru[1,4]
laTempStru = ''

DIMENSION laTempStru[ALEN(laTempStru,1) , 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyDept'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 5
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'Style'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 19
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyDesc'
laTempStru[ALEN(laTempStru,1),2] = 'C'

*B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
*laTempStru[ALEN(laTempStru,1),3] = 20
laTempStru[ALEN(laTempStru,1),3] = 60
*B607463,1 Replace Style Desc. with Style Long Desc. [End]

laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CStyGroup'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCat'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1)+1 , 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyClss'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls1'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls2'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CstyCls3'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CAccount'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 5
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'CCustNam'
laTempStru[ALEN(laTempStru,1),2] = 'C'
laTempStru[ALEN(laTempStru,1),3] = 30
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NShpQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NShpAmnt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2

DIMENSION laTempStru[ALEN(laTempStru,1) + 1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NOpnQty'
laTempStru[ALEN(laTempStru,1),2] = 'N'
*B606954,1 TMI [Start] Change the field width from 7 to 11
*laTempStru[ALEN(laTempStru,1),3] = 7
laTempStru[ALEN(laTempStru,1),3] = 11
*B606954,1 TMI [End  ] 
laTempStru[ALEN(laTempStru,1),4] = 0

DIMENSION laTempStru[ALEN(laTempStru,1) +1, 4]
laTempStru[ALEN(laTempStru,1),1] = 'NOpnAmt'
laTempStru[ALEN(laTempStru,1),2] = 'N'
laTempStru[ALEN(laTempStru,1),3] = 14
laTempStru[ALEN(laTempStru,1),4] = 2


CREATE DBF (gcWorkDir+lcTmpDtlSo)FROM ARRAY laTempStru
INDEX ON  cAccount+ Style TAG cAcStyIndx ADDITIVE


*:*************************************************************
*: Name      : lfMajGet
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
*: Purpose   : To get the title and picture of style major segement 
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : = lfMajGet()
*:*************************************************************

FUNCTION lfMajGet

lcMajPic  = gfItemMask("PM")
lnMajPic  = LEN(lcMajPic)
lcMajPic  = "@! " + lcMajPic
lcMajTtl  = gfItemMask("HM")
lnColorWid= LEN(gfItemMask('PN'))

*-- End OF lfMajGet
*:*************************************************************
*: Name      : lfItmPos
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/04/2002
*: Purpose   : Evaluate fixed filter position within array.
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Calls       : .....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : Position.
*:*************************************************************
*: Example     : = lfItmPos()
*:*************************************************************
*:
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*!*************************************************************
*! Name      : lfsrvSty
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/12/2002
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*!*************************************************************
*! Name      : lfvOType
*: Developer : MOHAMED SHOKRY (MHM)
*! Date      : 15/06/2000
*! Purpose   : - Evaluate Type expression.
*!           : - Rise change Type flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : =lfvOType()
*!*************************************************************
*!
FUNCTION lfvOType

*B606954,1 TMI [Start] No need for these two variables
*PRIVATE lcOldStat,lcCurrChr
*lcOldStat = lcRpType  && Save old Type value.
*B606954,1 TMI [End  ] 

*B606954,6 TMI [Start] Redefine values of target array
laRpTarget = ''
DIME laRpTarget[IIF(EMPTY(lcRpType),1,LEN(ALLT(lcRpType)))]
IF AT('P',lcRpType)>0
  laRpTarget[AT('P',lcRpType)] = 'Purchase'
ENDIF
IF AT('S',lcRpType)>0
  laRpTarget[AT('S',lcRpType)] = 'Sales'
ENDIF
IF AT('T',lcRpType)>0
  laRpTarget[AT('T',lcRpType)] = 'Style'
ENDIF
*B606954,6 TMI [End  ] 

= gfMover(@laRpSource,@laRpTarget,'Select Report Type',.T.,'')  && call mover function.

lcRpType = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpType = lcRpType +IIF(laRpTarget[lnI] = 'Purchase','P',;
                              IIF(laRpTarget[lnI] = 'Sales','S',;
                              IIF(laRpTarget[lnI] = 'Style','T','')))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpType = IIF(EMPTY(lcRpType),'PST',ALLTRIM(lcRpType))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 

*B606954,1 TMI [Start] No need for this code since we ignored useing the variable  "llOGFltCh"
*-* IF LEN(lcOldStat) != LEN(lcRpType) 
*-*   llOGFltCh = .T.
*-* 
*-* ELSE  && else if length of current selected status equal previous length
*-*   *-- loop to check if it's the same selected status or not.
*-*   FOR lnJ = 1 TO LEN(lcOldStat)
*-*     lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
*-*     IF !(lcCurrChr $ lcRpType)
*-*       llOGFltCh = .T.
*-*       EXIT
*-*     ENDIF
*-*   ENDFOR  && end loop to check if it's the same selected status or not.
*-* ENDIF
*-* *-- Compare current selected Type with old value  [end]
*-* CLEA READ
*B606954,1 TMI [End  ] 

*B606954,6 TMI [Start] Reinclude "Clear read"
CLEA READ
*B606954,6 TMI [End  ] 
*-- end of lfvOStatus.
*!**************************************************************************
*! Name      : lfStDepart
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfStDepart()
*!**************************************************************************
*
FUNCTION lfStDepart
PARAMETERS OpGrdParm
*B606954,1 TMI [Start] Save the current alias
PRIVATE lnSlct
lnSlct = SELECT()
*B606954,1 TMI [End  ] 
SELECT ICDEPTHD   
DO CASE
  CASE  OpGrdParm='S'           
    lcTmpIndex =gfTempName()
    SELECT  DISTINCT DEPT,cDeptDesc FROM ICDEPTHD INTO DBF (gcWorkDir + lcTmpIndex)
    USE IN ICDEPTHD
    use in (lcTmpIndex)
    USE (gcWorkDir + lcTmpIndex) IN 0 ALIAS ICDEPTHD
    SELECT ICDEPTHD
    INDEX ON DEPT TAG Depthd
    GO TOP
  CASE  OpGrdParm='R'
    USE IN ICDEPTHD
    =gfOpenFile(gcDataDir+'ICDEPTHD',gcDataDir+'DEPTHD ','SH')
  
ENDCASE

*B606954,1 TMI [Start] restore the previous alias
SELECT (lnSlct)
*B606954,1 TMI [End  ] 

*-- End of lfStDepart.
*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Go top in the icdepthd file when browse.
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfvPath()
*!**************************************************************************
*
FUNCTION lfvPath
IF ALLTRIM(lcRpPath)  = "?"
 lcRpPath = GETDIR()

ENDIF
*!**************************************************************************
*! Name      : lfChkPth
*! Developer : Mohamed Shokry (MHM)
*! Date      : 10/09/2002
*! Purpose   : Check Path if Empty or not valid 
*!**************************************************************************
*! Calls     : 
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfChkPth()
*!**************************************************************************
*
FUNCTION lfChkPth
*--if Path Empty or not valid return message 
IF EMPTY(lcRpPath)
  WAIT WINDOW "Please enter valid path"
  RETURN .T.
ENDIF  

IF EMPTY(lcRpFilNam)
  WAIT WINDOW "Please enter valid file name"
  RETURN .T.
ENDIF  
IF !gfValdPath(lcRpPath)
  WAIT WINDOW "Please enter valid path"
  RETURN .T.
ENDIF
IF EMPTY(lcRpType)
  lcRpType = 'PST'   && P --Purchase , S ----- Sales , T ----Style
ENDIF  

RETURN .F.



*:**************************************************************************
*:* Name        : lfRmvShpPo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2003
*:* Purpose     : Remove ship date and po date from the lcRpExp
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRmvShpPo()
*:***************************************************************************
*B606954,1 
FUNCTION lfRmvShpPo
PRIVATE laLoop,lnI,lnPos,lnLen
DIMENSION laLoop[2]
laLoop[1] = 'BETWEEN(DTOS(POSHDR.COMPLETE)'
laLoop[2] = 'BETWEEN(DTOS(INVHDR.SHIPDATE)'
FOR lnI = 1 TO 2
  lnPos = AT(laLoop[lnI],lcRpExp)
  IF lnPos > 0
    lnLen = AT('})))',SUBSTR(lcRpExp,lnPos))+3
    lcRpExp = STUFF(lcRpExp,lnPos,lnLen,'.T.')
  ENDIF
ENDFOR
*B606954,6 TMI [Start] Remove ".T.  AND" from "lcRpExp" if any  
lcRpExp = ALLT(STRTRAN(lcRpExp,"AND  .T."))
*B606954,6 TMI [End  ] 

*-- Shipment date criteria
lnPos = ASUBSCRIPT(laOgFxFlt, ASCAN(laOgFxFlt,'INVHDR.SHIPDATE') , 1)
lcInvCrit = IIF(EMPTY(CHRTRAN(laOgFxFlt[lnPos,6],'/|','')),'.T.',;
              'BETWEEN(INVHDR.SHIPDATE,{'+LEFT(laOgFxFlt[lnPos,6],10)+'},{'+RIGHT(laOgFxFlt[lnPos,6],10)+'})' )

*-- Open order date Critria
lcOrdCrit = STRTRAN(lcInvCrit,'INVHDR.SHIPDATE','ORDHDR.COMPLETE')

*-- Paurches orders date critera
lnPos = ASUBSCRIPT(laOgFxFlt, ASCAN(laOgFxFlt,'POSHDR.COMPLETE') , 1)
lcPoCrit = IIF(EMPTY(CHRTRAN(laOgFxFlt[lnPos,6],'/|','')),'.T.',;
             'BETWEEN(POSHDR.COMPLETE,{'+LEFT(laOgFxFlt[lnPos,6],10)+'},{'+RIGHT(laOgFxFlt[lnPos,6],10)+'})' )

*B606954,4 TMI [Start] Also replace the expression of "ICDEPTHD.DEPT" with "STYLE.DEPT" to improve preformance
lnPos = AT('ICDEPTHD.DEPT+ICDEPTHD.CSTYGROUP',lcRpExp)
IF lnPos > 0
  lcRpExp = STUFF(lcRpExp,lnPos,LEN('ICDEPTHD.DEPT+ICDEPTHD.CSTYGROUP'),'STYLE.DEPT')
ENDIF
*B606954,4 TMI [End  ] 
*-- end of lfRmvShpPo.



*:**************************************************************************
*:* Name        : lfUpDtSo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2003
*:* Purpose     : Calculate open sales order and shipped qtys
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpDtSo()
*:***************************************************************************
*B606954,1 
FUNCTION lfUpDtSo
PARAMETERS lcParam

PRIVATE lnShipQty,lnShipAmt,lcLastSty,lcLastAcc
STORE 0 TO lnShipQty,lnShipAmt
lcInvOrd = ORDER('INVLINE')
*B606954,4 TMI [Start] 
*SET ORDER TO INVLINEO IN INVLINE
PRIVATE lnOpnQty
*B606954,4 TMI [End  ] 

lcLastSty = SPACE(19)
lcLastAcc = SPACE(6)

SELECT ORDLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE
*B606954,5 TMI [Start] 
SELECT INVLINE
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE
*B606954,5 TMI [End  ] 

*B606954,4 TMI [Start] Add the check of ordline.totqty>0
*SCAN FOR &lcRpExp AND ;
         ORDHDR.STATUS $ "OH"
SELECT ORDLINE
*B606954,6 TMI [Start] Enhance by apply Eval Fn. and Exclude Temp Orders depending on the new opetion "llRpIncTmp" , 
*SCAN FOR &lcRpExp ;
         AND ORDLINE.TOTQTY > 0 AND ORDHDR.STATUS $ "OH" 
SCAN FOR EVAL(lcRpExp) ;
         AND ORDLINE.TOTQTY > 0 AND ORDHDR.STATUS $ "OH"
  IF !llRpIncTmp AND ORDLINE.CORDTYPE = 'T'
    LOOP
  ENDIF     
  *B606954,6 TMI [End  ] 
  *B606954,4 TMI [End  ]          
  
  IF !SEEK(Account+Style,lcTmpDtlSo)
    =SEEK('M'+Ordline.Account,'CUSTOMER')
    
    SELECT (lcTmpDtlSo)      
    APPEND BLANK
    
    *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
    *REPLACE Style       WITH Style.Style,;
    *        cStyDept    WITH Style.Dept,;
    *        cStyDesc    WITH Style.Desc,;
    *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP  , 'CSTYGROUP'),;
    *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG   , 'CSTYCATG'),;
    *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS  , 'CSTYCLASS'),;
    *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
    *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
    *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
    *        cAccount    WITH Ordline.Account,;
    *        CCustNam    WITH Customer.BTNAME,;    
    *        NOpnQty     WITH ORDLINE.TOTQTY,;
    *        NOpnAmt     WITH ORDLINE.TOTQTY * ORDLINE.PRICE
    REPLACE Style       WITH Style.Style,;
            cStyDept    WITH Style.Dept,;
            cStyDesc    WITH Style.Desc1,;
            cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP  , 'CSTYGROUP'),;
            CStyCat     WITH gfCodDes(STYLE.CSTYCATG   , 'CSTYCATG'),;
            CstyClss    WITH gfCodDes(STYLE.CSTYCLASS  , 'CSTYCLASS'),;
            CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
            CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
            CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
            cAccount    WITH Ordline.Account,;
            CCustNam    WITH Customer.BTNAME,;    
            NOpnQty     WITH ORDLINE.TOTQTY,;
            NOpnAmt     WITH ORDLINE.TOTQTY * ORDLINE.PRICE
    *B607463,1 Replace Style Desc. with Style Long Desc. [End]        

    *--* *B606954,4 TMI [Start] 
    *--* STORE 0 TO lnShipQty,lnShipAmt
    *--* SELECT INVLINE
    *--* IF SEEK(ORDLINE.STYLE,'INVLINE')
    *--*   SCAN REST WHILE STYLE+INVOICE+STR(LINENO,6) = ORDLINE.STYLE ;
    *--*             FOR   ORDLINE.ACCOUNT = INVLINE.ACCOUNT AND &lcInvCrit
    *--*     lnShipQty = lnShipQty + INVLINE.TOTQTY
    *--*     lnShipAmt = lnShipAmt + INVLINE.TOTQTY * INVLINE.PRICE
    *--*   ENDSCAN
    *--*   SELECT (lcTmpDtlSo)      
    *--*   REPLACE NShpQty  WITH lnShipQty,;
    *--*           NShpAmnt WITH lnShipAmt
    *--* ENDIF
    
  ELSE
  
    SELECT (lcTmpDtlSo)      
    REPLACE NOpnQty    WITH NOpnQty + ORDLINE.TOTQTY,;
            NOpnAmt    WITH NOpnAmt + ORDLINE.TOTQTY * ORDLINE.PRICE
  ENDIF
  
  *B606954,5 TMI [Start] 
  *-* *B606954,4 TMI [Start] 
  *-* lnOpnQty = lnOpnQty + NOpnQty
  *-* *B606954,4 TMI [End  ] 
  *B606954,5 TMI [End  ] 
  
  *-- Update ship qty , if any
  *B606954,4 TMI [Start] 
  *- IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE') 
  *-   STORE 0 TO lnShipQty,lnShipAmt
  *-   SELECT INVLINE
  *-   SCAN REST WHILE ORDER+STR(LINENO,6) = ORDLINE.ORDER+STR(ORDLINE.LINENO,6) ;
  *-             FOR   &lcInvCrit
  *-     lnShipQty = lnShipQty + INVLINE.TOTQTY
  *-     lnShipAmt = lnShipAmt + INVLINE.TOTQTY * INVLINE.PRICE
  *-   ENDSCAN
  *-   SELECT (lcTmpDtlSo)      
  *-   REPLACE NShpQty  WITH NShpQty  + lnShipQty,;
  *-           NShpAmnt WITH NShpAmnt + lnShipAmt
  *- ENDIF
  *B606954,4 TMI [End  ] 
ENDSCAN

*B606954,5 TMI [Start] since there is no relation between open qty and ship qty , let we collect shp qty as separate
STORE 0 TO lnShipQty,lnShipAmt
SELECT INVLINE
LOCATE
SCAN FOR &lcRpExp ;
       AND INVHDR.STATUS = 'C' ;
       AND &lcInvCrit 
  IF !SEEK(Account+Style,lcTmpDtlSo)
    =SEEK('M'+Invline.Account,'CUSTOMER')
    
    SELECT (lcTmpDtlSo)      
    APPEND BLANK
    
    *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
    *REPLACE Style       WITH Style.Style,;
    *        cStyDept    WITH Style.Dept,;
    *        cStyDesc    WITH Style.Desc,;
    *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP  , 'CSTYGROUP'),;
    *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG   , 'CSTYCATG'),;
    *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS  , 'CSTYCLASS'),;
    *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
    *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
    *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
    *        cAccount    WITH Invline.Account,;
    *        CCustNam    WITH Customer.BTNAME,;    
    *        NShpQty     WITH INVLINE.TOTQTY,;
    *        NShpAmnt    WITH INVLINE.TOTQTY * INVLINE.PRICE
    REPLACE Style       WITH Style.Style,;
            cStyDept    WITH Style.Dept,;
            cStyDesc    WITH Style.Desc1,;
            cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP  , 'CSTYGROUP'),;
            CStyCat     WITH gfCodDes(STYLE.CSTYCATG   , 'CSTYCATG'),;
            CstyClss    WITH gfCodDes(STYLE.CSTYCLASS  , 'CSTYCLASS'),;
            CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
            CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),;
            CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
            cAccount    WITH Invline.Account,;
            CCustNam    WITH Customer.BTNAME,;    
            NShpQty     WITH INVLINE.TOTQTY,;
            NShpAmnt    WITH INVLINE.TOTQTY * INVLINE.PRICE
    *B607463,1 Replace Style Desc. with Style Long Desc. [End]        

  ELSE

    SELECT (lcTmpDtlSo)      
    REPLACE NShpQty  WITH NShpQty  + INVLINE.TOTQTY ,;
            NShpAmnt WITH NShpAmnt + INVLINE.TOTQTY * INVLINE.PRICE
  ENDIF

ENDSCAN
*B606954,5 TMI [End  ] 

SELECT (lcTmpDtlSo)
GO TOP
DO CASE
  CASE lcRpShip $ "S" 
    DELETE FOR NShpQty = 0

  CASE lcRpShip $ "N" 
    DELETE FOR NShpQty > 0
ENDCASE    

SET ORDER TO &lcInvOrd IN INVLINE
*-- end of lfUpDtSo.


*:**************************************************************************
*:* Name        : lfUpDtPo
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2003
*:* Purpose     : Get open Paurchas order qtys for a style based on a vendor
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpDtPo()
*:***************************************************************************
*B606954,1 
FUNCTION lfUpDtPo
PARAMETERS lcParam

PRIVATE lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
PRIVATE lnpoOpnQty,lnpoOpnAmt

STORE 0 TO lnPoOrdQty,lnPoOrdAmt  ,lnPoRcdQty,lnpoRcdAmt  ,lnPoTrnQty,lnpoTrnAmt
STORE 0 TO lnpoOpnQty,lnpoOpnAmt

SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE
LOCATE

*B606954,6 TMI [Start] Include all PO's except Canceled ones,Also enahnce by useing Eval Fn. instead of macro substitution
*SCAN FOR &lcRpExp AND ;
         POSHDR.STATUS $ "OHA" AND ;
         &lcPoCrit
SCAN FOR EVAL(lcPoCrit) AND EVAL(lcRpExp) AND POSHDR.STATUS # "X" 
  *B606954,6 TMI [End  ]          
  IF !SEEK(Vendor+Style,lcTmpDtlPo)
    =SEEK(PoslN.vendor,'APVENDOR')
    SELECT (lcTmpDtlPo)
    APPEND BLANK
    
    *B607463,1 Replace Style Desc. with Style Long Desc. [Begin]
    *REPLACE Style       WITH Style.Style,;
    *        cStyDept    WITH Style.Dept,;
    *        cStyDesc    WITH Style.Desc,;
    *        cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
    *        CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
    *        CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
    *        CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
    *        CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),; 
    *        CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
    *        CVenCode    WITH POSLN.Vendor,;
    *        CVenNam     WITH APVENDOR.cvencomp,;
    *        NPoOrdQty WITH IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY , 0 ),;
    *        NPoOrdAmt WITH IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
    *        NPoRcdQty WITH IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY , 0 ),;
    *        NpoRcdAmt WITH IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
    *        NPoTrnQty WITH IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY , 0 ),;
    *        NpoTrnAmt WITH IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
    *        NpoOpnQty WITH NPoOrdQty - NPoRcdQty,;
    *        NpoOpnAmt WITH NPoOrdAmt - NPoRcdAmt
    REPLACE Style       WITH Style.Style,;
            cStyDept    WITH Style.Dept,;
            cStyDesc    WITH Style.Desc1,;
            cStyGroup   WITH gfCodDes(STYLE.CSTYGROUP , 'CSTYGROUP'),;
            CStyCat     WITH gfCodDes(STYLE.CSTYCATG  , 'CSTYCATG'),;
            CstyClss    WITH gfCodDes(STYLE.CSTYCLASS , 'CSTYCLASS'),;
            CstyCls1    WITH gfCodDes(STYLE.CSTYSBCLAS , 'CSTYSBCLAS'),;
            CstyCls2    WITH gfCodDes(STYLE.CSTYSBCLS2 , 'CSTYSBCLS2'),; 
            CstyCls3    WITH gfCodDes(STYLE.CSTYSBCLS3 , 'CSTYSBCLS3'),;
            CVenCode    WITH POSLN.Vendor,;
            CVenNam     WITH APVENDOR.cvencomp,;
            NPoOrdQty WITH IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY , 0 ),;
            NPoOrdAmt WITH IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
            NPoRcdQty WITH IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY , 0 ),;
            NpoRcdAmt WITH IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
            NPoTrnQty WITH IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY , 0 ),;
            NpoTrnAmt WITH IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY * POSLN.NCOST1 , 0 ),;
            NpoOpnQty WITH NPoOrdQty - NPoRcdQty,;
            NpoOpnAmt WITH NPoOrdAmt - NPoRcdAmt
    *B607463,1 Replace Style Desc. with Style Long Desc. [End]
  
  ELSE
  
    SELECT (lcTmpDtlPo)
    REPLACE NPoOrdQty WITH NPoOrdQty + IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY , 0 ),;
            NPoOrdAmt WITH NPoOrdAmt + IIF(POSLN.TRANCD = '1' , POSLN.TOTQTY * POSLN.NCOST1, 0 ),;
            NPoRcdQty WITH NPoRcdQty + IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY , 0 ),;
            NpoRcdAmt WITH NpoRcdAmt + IIF(POSLN.TRANCD = '2' , POSLN.TOTQTY * POSLN.NCOST1, 0 ),;
            NPoTrnQty WITH NPoTrnQty + IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY , 0 ),;
            NpoTrnAmt WITH NpoTrnAmt + IIF(POSLN.TRANCD = '3' , POSLN.TOTQTY * POSLN.NCOST1, 0 ),;
            NpoOpnQty WITH NPoOrdQty - NPoRcdQty ,;
            NpoOpnAmt WITH NPoOrdAmt - NPoRcdAmt 
  ENDIF          
ENDSCAN

*-- end of lfUpDtPo.

*:**************************************************************************
*:* Name        : lfPrepRel
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/10/2003
*:* Purpose     : Set needed relations
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPrepRel()
*:***************************************************************************
*B606954,1 
FUNCTION lfPrepRel
SELECT ORDLINE
SET RELATION TO CORDTYPE+ORDER INTO ORDHDR 
LOCATE

SELECT INVLINE
SET RELATION TO INVOICE INTO INVHDR 
LOCATE

SELECT POSLN
SET RELATION TO 'P'+PO INTO POSHDR
LOCATE

*-- end of lfPrepRel.
