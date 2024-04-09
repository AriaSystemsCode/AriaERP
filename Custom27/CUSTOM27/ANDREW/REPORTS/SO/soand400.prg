*****************************************************************************
*: Program file  : SOAND400.PRG      (Cust. Prog. # 101528)
*: Program desc. : Order header report itemized by Prepaid,Declined,Resubmitted
*:                 for AND100. Convert AND400 from 2.6 to 2.7.
*:         System: Aria Apparel System (A27)
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************

R_TITLE  = 'ORDER HEADER REPORT'
XREPORT  = 'SOAND400'
XTITLE  = lcRpTitle
llPrintNote = IIF(lcRpONote ='Y',.T.,.F.)
SORTFIELD = 'Approval+Decl_Code+Account+Order'
BREAK = 'Approval+Account+Decl_Code'
IF llOgFltCh
  IF FILE(gcWorkDir+lcRpTmp+".DBF")
    IF USED(lcRpTmp)
      USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
      ERASE &gcWorkDir.&lcRpTmp+'.DBF'
      ERASE &gcWorkDir.&lcRpTmp+'.CDX'
    ENDIF
  ENDIF
  DIMENSION XSEASON(6), XDIVISION(6)
  STORE SPACE(5)  TO LAccount,HAccount
  STORE SPACE(6)  TO LORDER,HORDER
  STORE SPACE(3)  TO LREP,HREP
  STORE SPACE(10) TO XCUSTPO
  STORE CTOD('  /  /    ') TO LCOMPLETE,HCOMPLETE,LSTART,HSTART,LENTERED,HENTERED
  STORE SPACE(4)  TO XSTATUS
  STORE SPACE(2)  TO TSEA,TDIV
  STORE SPACE(1)  TO XPRIORITY
  STORE SPACE(2)  TO XTERMS,XSPCINST,XSHIPVIA,XREGION
  =lfUpdFltVar()
  SELECT OrdHdr
  SET ORDER TO 0
  SET RELATION TO
  SET FILTER   TO
  GOTO TOP
  xFilter = ' '
  xFilter1= ' .AND. .T.'
  xFilter = xFilter+".AND.STATUS='H'"
  ** * VALIDATE THE ACCOUNT #
  IF LAccount<>' '.OR.HAccount<>' '
    xFilter = xFilter +".AND.BETWEEN(Account,LAccount,HAccount)"
  ENDIF
  ** VALIDATE THE ORDERS
  IF LORDER<>' '.OR.HORDER<>' '
    xFilter = xFilter +".AND.BETWEEN(ORDER,LORDER,HORDER)"
  ENDIF
  *** VALIDATE THE SALESREP
  IF !EMPTY(LREP) OR !EMPTY(HREP)
    xFilter = xFilter + ".AND. (BETWEEN(REP1,LREP,HREP);
                                .OR. BETWEEN(REP2,LREP,HREP))"
  ENDIF
  *** VALIDATE THE CUSTPO #
  IF !EMPTY(XCUSTPO)
    xFilter1 = xFilter1 + ".AND.CUSTPO=XCUSTPO"
  ENDIF
  IF DTOC(LENTERED)<>' '.OR.DTOC(HENTERED)<>' '
    xFilter1 = xFilter1+'.AND. BETWEEN(ENTERED,LENTERED,HENTERED)'
  ENDIF
  IF DTOC(LCOMPLETE)<>' '.OR.DTOC(HCOMPLETE)<>' '
    xFilter = xFilter +'.AND.BETWEEN(COMPLETE,LCOMPLETE,HCOMPLETE)'
  ENDIF
  IF DTOC(LSTART)<>' '.OR.DTOC(HSTART)<>' '
    xFilter = xFilter+'.AND.BETWEEN(START,LSTART,HSTART)'
  ENDIF
  IF !EMPTY(XPRIORITY)
    xFilter = xFilter+'.AND.PRIORITY$XPRIORITY'
  ENDIF
  *** VALIDATE THE STATUS
  IF !EMPTY( TSEA )
    xFilter = xFilter + ".AND. ALLTRIM(SEASON) $ TSEA "
  ENDIF
  ***************************
  *** CHECK THE DIVISIONS
  ***************************
  IF !EMPTY( TDIV )
    xFilter = xFilter + ".AND. ALLTRIM(cDIVISION) $ TDIV "
  ENDIF
  IF lcRpBulk = 'Y'
    xFilter1 = xFilter1+".AND. BULK='Y'"
  ENDIF
  IF lcRpMStor = 'Y'
    xFilter1 = xFilter1+".AND.MULTI='Y'"
  ENDIF
  IF lcRpReOrd = 'Y'
    xFilter1 = xFilter1+".AND.cREORDER='Y'"
  ENDIF
  IF !EMPTY(XSPCINST)
    xFilter1 = xFilter1+'.AND.SPCINST=XSPCINST'
  ENDIF
  IF !EMPTY(XTERMS)
    xFilter1 = xFilter1+'.AND.ALLTRIM(CTERMCODE)=XTERMS'
  ENDIF
  IF !EMPTY(XSHIPVIA)
    xFilter1 = xFilter1+'.AND.SHIPVIA=XSHIPVIA'
  ENDIF
  IF !EMPTY(XREGION)
    xFilter1 = xFilter1+'.AND.CUSTOMER.REGION=XREGION'
  ENDIF
  IF LEN(xFilter)>1
    xFilter=SUBSTR(xFilter,7)
  ENDIF
  IF LEN(xFilter1)>1
    xFilter1=SUBSTR(xFilter1,7)
  ENDIF
  *-------------------------------------------
  * SELECT SORT SEQUENCE
  *-------------------------------------------
  COPY STRU TO &gcWorkDir.&lcRpTmp
  =gfOpenFile(gcWorkDir+lcRpTmp,'','EX')
  SELECT OrdHdr
  SET RELATION TO IIF(STORE=SPACE(8),'M'+Account,'S'+Account+STORE) INTO CUSTOMER
  GOTO TOP
  SCAN FOR &xFilter  AND &Xfilter1
    llCopy = .F.
    IF EMPTY(xStatus)
      llCopy = .T.
    ELSE
      DO CASE
        CASE EMPTY(Approval) AND 'C' $ xStatus
          llCopy = .T.
        CASE Approval = "PREPAID" AND 'P' $ xStatus
          llCopy = .T.
        CASE Approval = "DECLINE" AND 'D' $ xStatus
          llCopy = .T.
        CASE Approval = "RESUB." AND 'R' $ xStatus
          llCopy = .T.
        OTHERWISE
          llCopy = .F.
      ENDCASE
    ENDIF
    IF llCopy
      SCAT MEMVAR MEMO
      WAIT WINDOW  'Selecting records for Order # ' +m.Order  NOWAIT
      INSERT INTO &lcRpTmp FROM MEMVAR
    ENDIF
  ENDSCAN
  SELECT OrdHdr
  SET RELATION TO
  SET FILTER TO
  SELECT &lcRpTmp
  SET RELATION TO IIF(STORE=SPACE(8),'M'+Account,'S'+Account+STORE) INTO CUSTOMER
  GOTO TOP
*  IF !EMPTY(XFILTER1)
*    DELETE ALL FOR !&XFILTER1
*    PACK
*  ENDIF 
  SELECT &lcRpTmp
  GOTO TOP
  IF EOF()
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  *---------------------------------------------------------
  * [3] SELECT REPORT FILE & INITIALIZE MEMORY
  *---------------------------------------------------------
  * SORT TO lcRpTmp INDEX
  IF SORTFIELD <>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW  'SORTING &Z RECORDS FOR ORDER SUMMARY REPORT ...' NOWAIT
    INDEX ON &SORTFIELD TAG &lcRpTmp
    SET ORDER TO TAG &lcRpTmp
  ENDIF
ENDIF
SELECT OrdHdr
DO WHILE .T.
  SET DEVICE TO SCREEN
  lnGrTotal  = 0.00
  lnSubTotal = 0.00
  lnTypSub   = 0.00
  PAGENO = 0
  ROW    = 99

  SELECT &lcRpTmp
  GOTO TOP
  IF LEN(TRIM(BREAK)) <>0
    HBREAK = &BREAK
  ENDIF
  SELECT &lcRpTmp
  lcAppr = Approval
  lcDecl = Decl_Code
  *---------------------------------------------------------
  * [REPORT] LOOP
  *---------------------------------------------------------
  SET DEVICE TO SCREEN
  WAIT WINDOW  'Report printing - <SPACE BAR> to abort' NOWAIT
  DO WHILE INKEY() <> 32
    SET DEVICE TO PRINT
    IF ROW >=53 
      IF BREAK <> ' '
        XTITLE1 = ALLTRIM(XTITLE)+' TOTAL: ' + IIF(EMPTY(Approval),'HOLD',Approval)
      ELSE
        XTITLE1 = XTITLE
      ENDIF
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,XTITLE1,R_WIDTH
      @ 05,000 SAY 'ACCOUNT# SHIP-TO NAME.................  '
      @ 06,000 SAY 'ORDER#   DIVISION ORDER PO# ENTER DATE START DATE COMPLETE DATE TERMS REP1 REP2       AMOUNT$  REASON NOTES'
      ROW = 7
    ENDIF

    DO WHILE LEN(TRIM(BREAK)) <> 0 .OR. lcAppr <> Approval .OR. IIF(ALLTRIM(lcAppr)='DECLINE',lcDecl<>Decl_Code,.T.)
   
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      @ ROW,000 SAY '* SUB TOTAL * '+ ALLTRIM(SUBSTR(HBREAK,11,5)) + '  ' + SUBSTR(HBREAK,16,2)
      @ ROW,083 SAY lnSubTotal PICTURE '9999999.99'

      ROW = ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      lnGrTotal  = lnGrTotal + lnSubTotal
      lnSubTotal = 0.00
      IF lcAppr <> Approval
        @ ROW,00 SAY REPLICATE('=',132)
        ROW = ROW+1
        @ ROW,000 SAY '* TOTAL FOR * '+ IIF(EMPTY(lcAppr),'HOLD',ALLTRIM(lcAppr)) + ' Orders'
        @ ROW,083 SAY lnTypSub PICTURE '9999999.99'
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('=',132)
        ROW = ROW+1
        lnTypSub = 0
        lcAppr = Approval
        ROW    = IIF(EOF(),ROW,99)
      ENDIF
      IF lcDecl <> Decl_Code
        lcDecl = Decl_Code
        ROW    = IIF(EOF(),ROW,99)
      ENDIF
      HBREAK = &BREAK
      EXIT
    ENDDO
    *--------------------- END SUBTOTALS ----------------------------
    IF EOF()
      EXIT
    ENDIF

    IF ROW >=53
      ROW = 99
      LOOP
    ENDIF

    SELECT &lcRpTmp

    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW+1
    @ ROW,000 SAY Account
    @ ROW,009 SAY Customer.StName
    ROW = ROW+1

    @ ROW,000 SAY Order
    @ ROW,009 SAY SUBSTR(cDivision,1,2)
    @ ROW,016 SAY IIF(MULTIPO,'*M-PO#*',SUBSTR(CUSTPO,1,10))
    @ ROW,028 SAY Entered
    @ ROW,039 SAY Start
    @ ROW,050 SAY Complete
    @ ROW,064 SAY SUBSTR(CTERMCODE,1,2)
    @ ROW,070 SAY Rep1
    @ ROW,075 SAY Rep2
    @ ROW,083 SAY OpenAmt
    @ ROW,095 SAY Decl_Code
    @ ROW,102 SAY Note1
    ROW = ROW+1
    lnSubTotal = lnSubTotal + OpenAmt
    lnTypSub   = lnTypSub   + OpenAmt

    ***************************************************
    IF llPrintNote
      DO PRT_NOTE WITH 'B',Order
    ENDIF
    SELECT &lcRpTmp
    SKIP
  ENDDO
   *------------------ END MAIN REPORT LOOP --------------------

  @ ROW,00 SAY REPLICATE('*',132)
  ROW = ROW+1
  @ ROW,000 SAY '** GRAND TOTAL ** '
  @ ROW,083 SAY lnGrTotal  PICTURE '9999999.99'
  ROW = ROW+1
  @ ROW,00 SAY REPLICATE('*',132)
  EXIT
ENDDO
DO ENDREPORT   && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREE
RETURN
*---------------------------
*   END AND400.PRG
*---------------------------


*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvcustom
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfVAcc

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF


*!*************************************************************
*! Name        : lfwRepWhen
*! Developer   : Ahmed Salah Shalaby -(SSH)
*! Date        : 05/10/99
*! Purpose     : Option grid when function
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

lnDatePos = ASCAN(laOGFxFlt,"INVHDR.INVDATE")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  IF EMPTY(laOGFxFlt[lnDatePos,6])
    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
  ENDIF  
ENDIF
*--- STANDARD REPORT IS 'WIDE'
R_WIDTH = 'W'
*!*************************************************************
*! Name      : lfvOrder
*! Developer : Ahmed Salah Shalaby
*! Date      : 03/27/1999
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('O'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ORDER     :R :H= 'ORDER#' , "    +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "Terms=gfCodDes(CTERMCODE , 'CTERMCODE') :R :H= 'Terms' ,"  +;
               "ShipV=gfCodDes(ShipVia , 'SHIPVIA')   :R :H= 'ShipVia' ,"  +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X" AND OPEN >= 1 ]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfvSalesr
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Validate Primary Sales Rep. in SALESREP file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfBrows
*!*************************************************************
*! Called from : Option Grid [Sales representative Object]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSalesr()
*!*************************************************************
FUNCTION lfvSalesr
PRIVATE lcVar , lcObj , laTemp

lcSaleF = VARREAD()
lcSales = EVALUATE(lcSaleF) 

lnSelFile =  SELECT(0)
SELECT SALESREP
lcCusTag  = ORDER('SALESREP')
SET ORDER TO TAG SALESREP IN SALESREP


IF !EMPTY(lcSales) .AND. ('?' $ lcSales OR !SEEK(lcSales , 'SALESREP'))

  DIMENSION laTemp[1]
  laTemp = ''    
  lcBrFields = "REPCODE   :R :H= 'Code' , "   +;
               "NAME      :R :H= 'Name' ,"    +;
               "cAddress6 :R :H= 'Country' ," +;
               "PHONE     :R :H= 'Phone' ,"   +;
               "BALANCE   :R :H= 'Balance' "
  
  lcFile_Ttl = "Sales Representative ..."
  = gfBrows('','REPCODE','laTemp')
    
  IF !EMPTY(laTemp[1])
    lcSales   = laTemp[1]
    lcRepName = SalesRep.Name
  ELSE   
    lcSales   = laOldVal
    lcRepName = ""
  ENDIF    
ENDIF    

&lcSaleF = lcSales 
SET ORDER TO lcCusTag
SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 05/10/99
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

XCUSTPO = lcRpCustPo
FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      LENTERED = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HENTERED = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      LCOMPLETE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HCOMPLETE = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.START' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      LSTART = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      HSTART = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ORDER' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Order Range
      LORDER = ALLTRIM(laOgFxFlt[lnInd,6])
      HORDER = ALLTRIM(laOgFxFlt[lnInd,6])
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Account Range
      LAccount = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,5))
      HAccount = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],7,5))
 
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SALESREP.REPCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Sales Rep.
      LREP = SUBSTR(laOgFxFlt[lnInd,6],1,3)
      HREP = SUBSTR(laOgFxFlt[lnInd,6],5,3)

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SPCINST' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Spe. Ins.
      XSPCINST = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.PRIORITY' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Priority
      XPRIORITY = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.STATUS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Status
      XSTATUS = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Season
      TSEA = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Division
      TDIV = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CTERMCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Term Code
      XTERMS = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SHIPVIA' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Ship Via
      XSHIPVIA = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.REGION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- region
      XREGION = ALLTRIM(laOgFxFlt[lnInd,6])

  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'