*:***************************************************************************
*: Program file  : SORDHDEJ
*: Program desc. : Order Header Report For ERIC
*: For Report    : SORDHDEL.FRX,SORDHDES.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar [MMT]
*: Date          : 04/30/2007
*: Reference     : C200783 ,1
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : gfDispRe,gfModalGen,gfPhoneTem,gfTempName,gfMover,gfGetAdr.
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOORDHD
*!***************************************************************************
*! Modification:
*----------------------- Report Code Begin -----------------------------

#INCLUDE R:\Aria4xp\reports\so\SORDHDEJ.h
lcStTime   = TIME()                     && Variable to hold the start Time.
lcLastRep1 = ''
lnCntRep   = 0
lnGrandAcc = 0

*Use Full Multi currency format capability [Begin]
STORE "" TO lcHedTitle,lcLineCurr
*HMA,uncomment the Commented code becaue we use these variables in report [Begin]
*!*	llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
*!*	                          ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
*!*	                            (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))

*!*	llCurInGrp = !llPrintTot
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
                          ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
                            (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot
*HMA,uncomment the Commented code becaue we use these variables in report [End]

=lfGetRepVr() && Get Report Variables...

*Use Full Multi currency format capability [End  ]


*-- If user change report critria, Collect report data.
IF loOGScroll.llOGFltCh     && Variable to detect OG filter changes

  *-B127856, HFK 05/10/2005 Add summary format to report
  *!*    llFilCodes = (lcRpFormat = "S") 
  llFilCodes = (lcRpFormat = "S") .OR. (lcRpFormat = "H")  
  *-B127856, HFK 05/10/2005 Add summary format to report  
  *-- Unrise all Critria variables.
  llChSortBy = .F.  && We Fill index field with its corresponding data in data collection.

  *-- Create scan expression from selected filter. [begin]
  lcMultiExp = IIF(llRpMulti,[MULTI = 'Y'],'')
  lcReOrdExp = IIF(llRpReOrd,[CREORDER = 'Y'],'')
  lcBulkExp  = IIF(llRpBulk ,[BULK = 'Y'],'')


  *! B128322,1 SMM 06/02/2005 When saving settings the status seems not to be saved [START]
  LOCAL lnCount 	
  IF !EMPTY(laRpTarget[1])
    FOR lnCount = 1 TO ALEN(laRpTarget,1)
      lcRpStatus = lcRpStatus + IIF(laRpTarget[lnCount] = LANG_Soordhd_Bid,'B',;
                                IIF(laRpTarget[lnCount] = LANG_Soordhd_Open,'O',;
                                IIF(laRpTarget[lnCount] = LANG_Soordhd_Hold,'H',;
                                IIF(laRpTarget[lnCount] = LANG_Soordhd_Completed,'C',;
                                IIF(laRpTarget[lnCount] = LANG_Soordhd_Canceled,'X','')))))
    ENDFOR
  ENDIF
  lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
  *! B128322,1 SMM 06/02/2005 When saving settings the status seems not to be saved [END]

  lcStatExp  = IIF(EMPTY(lcRpStatus),'',[STATUS $ lcRpStatus])

  lcHiddExp  = IIF(EMPTY(lcMultiExp),'',[ AND ] + lcMultiExp) +;
               IIF(EMPTY(lcReOrdExp),'',[ AND ] + lcReOrdExp) +;
               IIF(EMPTY(lcBulkExp),'',[ AND ] + lcBulkExp)   +;
               IIF(EMPTY(lcStatExp),'',[ AND ] + lcStatExp)

  lcRpExp    =  IIF('.T.' $ lcRpExp,[(CORDTYPE+ORDER = 'O' OR CORDTYPE+ORDER = 'T')],;
  				   [(CORDTYPE+ORDER ='O' OR CORDTYPE+ORDER ='T')  AND ] + ;
  				   lcRpExp) + IIF(EMPTY(lcHiddExp),'', lcHiddExp)                              

  lcRpExp    = STRTRAN(lcRpExp,'ORDHDR.','')

  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [begin]
  IF !EMPTY(loOGScroll.laOGFxFlt[lnRepPos,6])
    lcRepFile  = loOGScroll.laOGFxFlt[lnRepPos,6]
    lnRepStPos = ATC('SEEK(REP1',lcRpExp)
    lnRepLen   = ATC(lcRepFile,lcRpExp) + 10 - lnRepStPos
    lcRepCond1 = SUBSTR(lcRpExp,lnRepStPos,lnRepLen)
    lcRepCond2 = STRTRAN(lcRepCond1,'REP1','REP2')
    lcRepCond  = '(' + lcRepCond1 + ' OR ' + lcRepCond2 + ')'
    lcRpExp    = STRTRAN(lcRpExp,lcRepCond1,lcRepCond)
  ENDIF
  *-- Check for REP1 in filter then change filter to be in Both REP1 or REP2 [end]

  *-- Create scan expression from selected filter. [end]

  *-- if you have previous data clear WorkFile then recreate it. [begin]
  IF !USED(WorkFile) OR RECCOUNT(WorkFile) > 0
    IF USED(WorkFile)
      USE IN (WorkFile)
    ENDIF
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear WorkFile then recreate it. [end]
  *-- Fill temporary file with report data [begin]
  SELECT ORDHDR
  SET ORDER TO
  SET RELATION TO 'M' + Account INTO CUSTOMER

 
  *Add Field LEDIORDER to the filter expression [Begin.]
  *- E128374,1 HFK 06/05/2005 [Start]
  lcRpEdiFlt = ""
  IF 'EB' $ oAriaApplication.CompanyInstalledModules 
    DO CASE
      CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
        lcRpEdiFlt = [!OrdHdr.lEdiOrder]
      CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
        lcRpEdiFlt = [(OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T')]
      CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
        lcRpEdiFlt = [(OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O')]
      CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
        lcRpEdiFlt = [(!OrdHdr.lEdiOrder  .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T'))]
      CASE ("T" $ lcRpType .AND. "R" $ lcRpType .AND. !("O" $ lcRpType))
        lcRpEdiFlt = [OrdHdr.lEdiOrder]
      CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType )
        lcRpEdiFlt = [(!OrdHdr.lEdiOrder .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O'))]
    ENDCASE
  ENDIF
  *- E128374,1 HFK 06/05/2005 [End]  
  IF !EMPTY(lcRpEdiFlt)
    IF !EMPTY(lcRpExp)
      lcRpExp = lcRpExp + [ AND ]
    ENDIF
    lcRpExp = lcRpExp + lcRpEdiFlt
  ENDIF
  *Add Field LEDIORDER to the filter expression [End.]

  SCAN FOR &lcRpExp
    SCATTER MEMVAR MEMO
    =lfFillData()
  ENDSCAN
  SET ORDER TO ORDHDR
  SET RELATION OFF INTO CUSTOMER
  *-- Fill temporary file with report data [end ]
ENDIF  && end If user change report critria, Collect report data.
 
IF RECCOUNT(WorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok >
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

lcShpTName = ''
lcPhonePic = gfPhoneTem()

SELECT (WorkFile)
*-- if not first run for the same filter and start run is short format then fill codes. [Begin]
IF !loOGScroll.llOGFltCh AND llFilCodes AND (lcRpFormat = "L")
  llFilCodes = .F.
  SET ORDER TO
  SCAN
    =lfFillData(.T.)
    IF llChSortBy
      REPLACE cTempKey WITH EVALUATE(lcIndexTg)
    ENDIF
  ENDSCAN
  llChSortBy = .F.
  SET ORDER TO (WorkFile)
ENDIF
*-- if not first run for the same filter and start run is short format then fill codes. [End  ]

IF llChSortBy
  REPLACE ALL cTempKey WITH EVALUATE(lcIndexTg)
  llChSortBy = .F.
ENDIF

*-- Temp File used to Calculate No. of accounts [begin]
*-- in the current sales rep. and in grand totals.
CustFile = gfTempName()
CREATE CURSOR (CustFile) (ACCOUNT C(5) , SALESREP C(3), cCurrCode C(3) ,;
                          cShip1 C(30),cShip2 C(30),cShip3 C(30),cShip4 C(30),cShip5 C(30) ,;
                          STORE C(8))
SELECT (CustFile)
ZAP
INDEX ON ACCOUNT+STORE TAG (CustFile) OF (oAriaApplication.WorkDir+CustFile+'.CDX')

IF lcRpSortBy = 'S'        && WHEN SORTING BY SALESREP
  CUSTFL1 = gfTempName()
  IF llMultCurr
    INDEX ON SALESREP+CCURRCODE+ACCOUNT TAG (CUSTFL1) OF (oAriaApplication.WorkDir+CustFile+'.CDX')
  ELSE
    INDEX ON SALESREP+ACCOUNT TAG (CUSTFL1) OF (oAriaApplication.WorkDir+CustFile+'.CDX')
  ENDIF
ENDIF
SET ORDER TO TAG (CustFile)
*-- Temp File used to Calculate No. of accounts [end]
*---------
 
llSameCurr = .T.
SELECT (WorkFile)
LOCATE 
lcFirstCurr = CCURRCODE
IF RECCOUNT() > 1
   SCAN
    IF lcFirstCurr <> CCURRCODE
      llSameCurr = .F.
      EXIT 
    ENDIF 
  ENDSCAN
ENDIF   
*---------
SELECT (WorkFile)
GO TOP

*-- Set relation between temp. file and customer file.
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account,;
                                   'S' + Account + Store) INTO CUSTOMER  && To customer file.
SET RELATION TO 'B'+Order INTO NOTEPAD ADDITIVE

lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW LANG_Soordhd_SelectMsg +' '+ ALLTRIM(STR(RECCOUNT(WorkFile))) + LANG_Soordhd_RecInMsg + ALLTRIM(STR(lnInterval,6,2)) + LANG_Soordhd_SecondMsg  NOWAIT

*! B129280,1 ASM 10/09/2005 Open amount wraps around to next line [Start]
loogscroll.nfontsize = 7
*! B129280,1 ASM 10/09/2005 Open amount wraps around to next line [End]

*! N000535,1 MMT 19/09/2006 convert to graphics [Start]
loOgScroll.cCRorientation = 'P'
*! N000535,1 MMT 19/09/2006 convert to graphics [End]

DO gfDispRe WITH EVAL('lcRpForm')
*--B99999,1,[MMT],01/27/2005,Fixing problem of error after exporting to HTML
IF USED(CustFile)
  USE IN (CustFile)
ENDIF   
*--B99999,1,[MMT],01/27/2005,Fixing problem of error after exporting to HTML
RETURN
*-- end of report code.

*------------------ Functions Section ---------------------
*----------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : When function of OG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*
FUNCTION lfwRepWhen

lnRepPos  = lfItmPos('ORDHDR.REP1')
lnOrdPos  = lfItmPos('ORDHDR.ORDER')
IF llMultCurr
  lnCurrPos  = lfItmPos('ORDHDR.CCURRCODE')
ENDIF

IF EMPTY(laRpSource)
  *Add Feature to allow printing the Bid status.
  DECLARE laRpSource[5],laRpTarget[5]  && Redeclare the source and target arrays.
  *Redeclare the source and arrays with new status Bid.[Begin]
  STORE LANG_Soordhd_Bid       TO laRpSource[1],laRpTarget[1]
  STORE LANG_Soordhd_Open      TO laRpSource[2],laRpTarget[2]
  STORE LANG_Soordhd_Hold      TO laRpSource[3],laRpTarget[3]
  STORE LANG_Soordhd_Canceled  TO laRpSource[4],laRpTarget[4]
  STORE LANG_Soordhd_Completed TO laRpSource[5],laRpTarget[5]
  *Redeclare the source and arrays with new status Bid. [End]
  
ENDIF
*- E128374,1 HFK 06/05/2005 [Start]
IF EMPTY(laRpTypSrc)
  DECLARE laRpTypSrc[3],laRpTypDst[3]  && Redeclare the source and target arrays.
  STORE "Orders"               TO laRpTypSrc[1],laRpTypDst[1]
  STORE "EDI Temporary Orders" TO laRpTypSrc[2],laRpTypDst[2]
  STORE "EDI Received Orders"  TO laRpTypSrc[3],laRpTypDst[3]
ENDIF
*- E128374,1 HFK 06/05/2005 [End]

IF !USED(WorkFile)

  DECLARE  laShipTo[5,1],laCodDesc[7,3]
  STORE ''         TO lcRpStatus,laShipTo,laCodDesc
  laCodDesc[1,2] = LANG_Soordhd_Season           
  laCodDesc[2,2] = LANG_Soordhd_Cdivision 
  laCodDesc[3,2] = LANG_Soordhd_CTermCode 
  laCodDesc[4,2] = LANG_Soordhd_Shipvia   
  laCodDesc[5,2] = LANG_Soordhd_SpecialInst
  laCodDesc[6,2] = LANG_Soordhd_Region   
  laCodDesc[7,2] = LANG_Soordhd_Decl_Code   
  DIMENSION laTempStru[1,18]
  laTempStru = '' 
  SELECT ORDHDR
  lnFildLen = AFIELD(laTempStru)

  *-- Add Code fields.
  DIMENSION laTempStru[lnFildLen + 10, 18]
  laTempStru[lnFildLen + 1, 1] = 'cSeaDesc'
  laTempStru[lnFildLen + 1, 2] = 'C'
  laTempStru[lnFildLen + 1, 3] = 37
  laTempStru[lnFildLen + 1, 4] = 0

  laTempStru[lnFildLen + 2, 1] = 'cDivDesc'
  laTempStru[lnFildLen + 2, 2] = 'C'
  laTempStru[lnFildLen + 2, 3] = 37
  laTempStru[lnFildLen + 2, 4] = 0

  laTempStru[lnFildLen + 3, 1] = 'cTrmDesc'
  laTempStru[lnFildLen + 3, 2] = 'C'
  laTempStru[lnFildLen + 3, 3] = 37
  laTempStru[lnFildLen + 3, 4] = 0

  laTempStru[lnFildLen + 4, 1] = 'cViaDesc'
  laTempStru[lnFildLen + 4, 2] = 'C'
  laTempStru[lnFildLen + 4, 3] = 37
  laTempStru[lnFildLen + 4, 4] = 0

  laTempStru[lnFildLen + 5, 1] = 'cSpcDesc'
  laTempStru[lnFildLen + 5, 2] = 'C'
  laTempStru[lnFildLen + 5, 3] = 37
  laTempStru[lnFildLen + 5, 4] = 0

  laTempStru[lnFildLen + 6, 1] = 'cRegDesc'
  laTempStru[lnFildLen + 6, 2] = 'C'
  laTempStru[lnFildLen + 6, 3] = 37
  laTempStru[lnFildLen + 6, 4] = 0

  laTempStru[lnFildLen + 7, 1] = 'cCanDesc'
  laTempStru[lnFildLen + 7, 2] = 'C'
  laTempStru[lnFildLen + 7, 3] = 37
  laTempStru[lnFildLen + 7, 4] = 0

  laTempStru[lnFildLen + 8, 1] = 'cCurDesc'
  laTempStru[lnFildLen + 8, 2] = 'C'
  laTempStru[lnFildLen + 8, 3] = 37
  laTempStru[lnFildLen + 8, 4] = 0
  
  laTempStru[lnFildLen + 9, 1] = 'cDecl_code'
  laTempStru[lnFildLen + 9, 2] = 'C'
  laTempStru[lnFildLen + 9, 3] = 37
  laTempStru[lnFildLen + 9, 4] = 0
 
  *-- cTempKey :  field used in all sort by cases as the master key ,
  laTempStru[lnFildLen + 10, 1] = 'cTempKey'
  laTempStru[lnFildLen + 10, 2] = 'C'
  laTempStru[lnFildLen + 10, 3] = 26
  laTempStru[lnFildLen + 10, 4] = 0

  FOR lnInc=7 TO 16
    FOR lnRaw=1 TO 10
      STORE SPACE(1) TO laTempStru[lnFildLen + lnRaw,lnInc]
    ENDFOR 
  ENDFOR 
  FOR lnRaw=1 TO 10
    STORE 0  TO laTempStru[lnFildLen + lnRaw,17], laTempStru[lnFildLen + lnRaw,18]
  ENDFOR 
  
  SET ORDER TO ORDHDR   IN ORDHDR
  SET ORDER TO CUSTOMER IN CUSTOMER
  SET ORDER TO SALESREP IN SALESREP

  =lfWorkFile()
  
  =lfvSortBy()   &&Initially Rise Currency flags 

ENDIF

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag.
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!***************************************************************************
*! Modification:
*! B123663,1 SMM 07/13/2004 Change gfMover to lfOGMover.
*!***************************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
* B123663,1 SMM Change gfMover to lfOGMover
*= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.
= lfOGMover(@laRpSource,@laRpTarget,LANG_Soordhd_SelectOrdStat,.T.,'')  && call mover function.
* B123663,1 SMM End

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = LANG_Soordhd_Bid,'B',;
                              IIF(laRpTarget[lnI] = LANG_Soordhd_Open,'O',;
                              IIF(laRpTarget[lnI] = LANG_Soordhd_Hold,'H',;
                              IIF(laRpTarget[lnI] = LANG_Soordhd_Completed,'C',;
                              IIF(laRpTarget[lnI] = LANG_Soordhd_Canceled,'X','')))))
  ENDFOR
ENDIF
lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*!*	*-- if length of current selected status differ from previous length
*!*	IF LEN(lcOldStat) != LEN(lcRpStatus)
*!*	  llOGFltCh  = .T.
*!*	  llClearOrd = .T.
*!*	ELSE  && else if length of current selected status equal previous length
*!*	  *-- loop to check if it's the same selected status or not.
*!*	  FOR lnJ = 1 TO LEN(lcOldStat)
*!*	    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
*!*	    IF !(lcCurrChr $ lcRpStatus)
*!*	      llOGFltCh  = .T.
*!*	      llClearOrd = .T.
*!*	      EXIT
*!*	    ENDIF
*!*	  ENDFOR  && end loop to check if it's the same selected status or not.
*!*	ENDIF
*!*	*-- Compare current selected status with old value  [end]

*!*	*-- Activate suppress expression [begin]
*!*	llCancNow = ('X' $ lcRpStatus) OR EMPTY(lcRpStatus)
*!*	llCancPrv = ('X' $ lcOldStat)  OR EMPTY(lcOldStat)
*!*	IF (llCancNow AND !llCancPrv) OR (!llCancNow AND llCancPrv)
*!*	  ClearRead()
*!*	ENDIF
*!*	*-- Activate suppress expression [end]

*HMA 05/16/2005 delete the selected records from Order # Line 
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrdPos > 0 
  lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
  IF USED(lcOrderSel)
    SELECT(lcOrderSel)
    ZAP 
  ENDIF 
ENDIF  

llClearOrd = .T.
*HMA 05/16/2005 delete the selected records from Order # Line 
*in the Option grid if the order status or order type has changed. [End]

*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    *!*	    DO CASE
    IF !EMPTY(lcRpStatus) .AND. LEN(lcRpStatus) <> 5
        SELECT ORDHDR
        *- E128374,1 HFK 06/05/2005 [Start]
        *- SET FILTER TO (ORDHDR.STATUS$lcRpStatus) ,;        
        *- AND IIF(lcRpEdiPrn ='O',!ORDHDR.lEdiOrder,IIF(lcRpEdiPrn ='E',ORDHDR.lEdiOrder,.T.))
        DO CASE
          CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. "R" $ lcRpType ) .OR. EMPTY(lcRpType)
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus)
          CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. !lEdiOrder
          CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .And. cOrdType = 'T')
          CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .And. cOrdType = 'O')
          CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (!lEdiOrder .OR. (lEdiOrder .And. cOrdType = 'T'))
          CASE ("T" $ lcRpType .AND. "R" $ lcRpType .AND. !("O" $ lcRpType))
            SET FILTER TO lEdiOrder .AND. (ORDHDR.STATUS$lcRpStatus)
          CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND."R" $ lcRpType )
            SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (!lEdiOrder .OR. (lEdiOrder .And. cOrdType = 'O')) 
        ENDCASE
        *- E128374,1 HFK 06/05/2005 [End]
        LOCATE 
     ELSE
        SELECT ORDHDR
        *- E128374,1 HFK 06/05/2005 [Start]
        *-SET FILTER TO IIF(lcRpEdiPrn  ='O',!ORDHDR.lEdiOrder,IIF(lcRpEdiPrn  ='E',ORDHDR.lEdiOrder,.T.))
        DO CASE
          CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND. !("R" $ lcRpType))
            SET FILTER TO !lEdiOrder
          CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
            SET FILTER TO (lEdiOrder .And. cOrdType = 'T')
          CASE (!("O" $ lcRpType) .AND. !("T" $ lcRpType) .AND. "R" $ lcRpType)
            SET FILTER TO (lEdiOrder .And. cOrdType = 'O')
          CASE ("O" $ lcRpType .AND. "T" $ lcRpType .AND. !("R" $ lcRpType))
            SET FILTER TO (!lEdiOrder .OR. (lEdiOrder .And. cOrdType = 'T'))
          CASE (!("O" $ lcRpType) .AND. "T" $ lcRpType .AND."R" $ lcRpType)
            SET FILTER TO lEdiOrder 
          CASE ("O" $ lcRpType .AND. !("T" $ lcRpType) .AND."R" $ lcRpType )
            SET FILTER TO (!lEdiOrder .OR. (lEdiOrder .And. cOrdType = 'O')) 
        ENDCASE
        *- E128374,1 HFK 06/05/2005 [End]
        LOCATE 
     ENDIF   
     *--B99999,1 mmt fix bug of not ordering data in browser[Start]
     SET ORDER TO ORDHDR   && CORDTYPE+ORDER 
     *--B99999,1 mmt fix bug of not ordering data in browser[End]
*        LOCATE FOR CORDTYPE+ORDER = "O"

*!*	      CASE lcRpEdiPrn = "C"
*!*	        SET FILTER TO (CORDTYPE + ORDER = "C") AND (ORDHDR.STATUS$lcRpStatus) ,;
*!*	        AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
*!*	        IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
*!*	        LOCATE FOR CORDTYPE+ORDER = "C"

*!*	      CASE lcRpEdiPrn = "T"
*!*	        SET FILTER TO (CORDTYPE + ORDER = "T") AND (ORDHDR.STATUS$lcRpStatus)  ,;
*!*	        AND IIF(ORDHDR.cOrdType='T' .AND. ;
*!*	        ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ;
*!*	        IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
*!*	        LOCATE FOR CORDTYPE+ORDER = "T"

*!*	    ENDCASE
    
  CASE lcParm = 'R'
    
      SELECT ORDHDR
      SET FILTER TO
ENDCASE
*-- End of lfsChOrder.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change sales rep. flag, in range browse screen.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE

*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Create work cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfWorkFile
CREATE CURSOR (WorkFile) ;
   FROM ARRAY laTempStru

SELECT (WorkFile)
*-- Fix Cursor bug [Begin]
ZAP
*-- Fix Cursor bug [End  ]
INDEX ON cTempKey TAG (WorkFile) OF (oAriaApplication.WorkDir+WorkFile+'.CDX')


*!*************************************************************
*! Name      : lfClearRep
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Function that we call when Close the option grid.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfClearRep
loOGScroll.llOGFltCh = .T.

*-- Close temp. opended files, if it used.
*-- Delete temporary work file.
IF USED(WorkFile)
  USE IN (WorkFile)
ENDIF

IF USED('SYCCURR')
  USE IN SYCCURR
ENDIF

IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

*Restore old currency setting before exit.
IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF USED("SYCINT")
    USE IN SYCINT
  ENDIF

ENDIF

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Rise change index flag to reindex temp cursor.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvSortBy
llChSortBy = .T.
llPrintTot = !llMultCurr OR (lcRpCurr <> "F") OR ;
                          ((!EMPTY(loOGScroll.laOGFxFlt[lnCurrPos,6]) AND ATC("|",loOGScroll.laOGFxFlt[lnCurrPos,6])=0) OR ;
                            (!EMPTY(loOGScroll.laOGFxFlt[lnOrdPos ,6]) AND USED(loOGScroll.laOGFxFlt[lnOrdPos,6]) AND RECCOUNT(loOGScroll.laOGFxFlt[lnOrdPos,6])=1))

llCurInGrp = !llPrintTot

*!*************************************************************
*! Name      : lfShipToAd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate ship to address then calculate No. of accounts in range.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfShipToAd
PARAMETERS llShort
PRIVATE lnSeekedRc

*-- if short format.
IF llShort AND llMultCurr
  =lfChCurSm()
ENDIF

*-- Calculate No. Of accounts [Begin]
SET ORDER TO TAG (CustFile) IN (CustFile)
lnSeekedRc = 0
* If printing "Long", ask about the store in custfile.[Begin]
IF SEEK(ACCOUNT+IIF(llShort,'',STORE) , CustFile)
* If printing "Long", ask about the store in custfile.[End]
  IF Alt_ShpTo
    lcShpTName  = ALLTRIM(Customer.STName)
  ELSE
    lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  ENDIF

ELSE

  lnGrandAcc  = lnGrandAcc + 1
  lnCntRep    = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
  lcLastRep1  = REP1+IIF(llMultCurr,CCURRCODE,'')
  m.Account   = ACCOUNT
  m.Store     = STORE
  m.SalesRep  = REP1
  m.cCurrCode = cCurrCode
  INSERT INTO (CustFile) FROM MEMVAR
  =lfUpdAdd()

ENDIF
lnSeekedRc = RECNO(CustFile)
*-- if sort by sales rep.
IF lcRpSortBy = 'S'
  SET ORDER TO TAG (CUSTFL1) IN (CustFile)
  lcSeekVal = IIF(llMultCurr,REP1+cCurrCode+ACCOUNT,REP1+ACCOUNT)
  IF !SEEK(lcSeekVal,CustFile)
    lnCntRep   = IIF(lcLastRep1=REP1+IIF(llMultCurr,CCURRCODE,''),lnCntRep + 1,1)
    lcLastRep1 = REP1+IIF(llMultCurr,CCURRCODE,'')
    m.Account  = ACCOUNT
    m.Store     = STORE
    m.SalesRep = REP1
    m.cCurrCode = cCurrCode
    INSERT INTO (CustFile) FROM MEMVAR
  ENDIF
ENDIF

IF lnSeekedRc <> 0
  GO lnSeekedRc IN (CustFile)
ENDIF

*-- Calculate No. Of accounts [End]
RETURN ''



*!*************************************************************
*! Name      : lfUpdAdd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Update Customer addresses.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfUpdAdd
PRIVATE lnCurrAls

lnCurrAls = SELECT(0)
laShipTo = ''
*-- IF alternate ship to address
IF Alt_ShpTo
  lcShpTName  = ALLTRIM(Customer.STName)
  IF !llShort
    laShipTo[1] = ALLTRIM(cAddress1)
    laShipTo[2] = ALLTRIM(cAddress2)
    laShipTo[3] = ALLTRIM(cAddress3)
    laShipTo[4] = ALLTRIM(cAddress4)
    laShipTo[5] = ALLTRIM(cAddress5)
  ENDIF
ELSE    && Else
  lcShpTName  = IIF(EMPTY(Customer.DBA) , ALLTRIM(Customer.STName) , ALLTRIM(Customer.DBA))
  IF !llShort
    * Adjust address function (gfGetAdr) to fasten report.
    =gfGetAdr('CUSTOMER' , '' , '' , '' , @laShipTo)

  ENDIF
ENDIF    && End of IF

IF !llShort
  =lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
  *Remove charachters more than 21 to appear completly in report.[Begin]
  FOR lnCnt = 1 TO ALEN(laShipTo)
    laShipTo[lnCnt] = SUBSTR(laShipTo[lnCnt],1,21)
  ENDFOR
  *Remove charachters more than 21 to appear completly in report.[End]
  SELECT (CustFile)
  GATHER FROM laShipTo FIELDS cShip1,cShip2,cShip3,cShip4,cShip5
ENDIF
SELECT (lnCurrAls)


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return       : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
                                  EMPTY(&lcArrayNam.[lnCount])
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    
ENDFOR    

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    
ENDFOR    

*!*************************************************************
*! Name      : lfvFormat
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Change report format (Long/Short)
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvFormat

*-B127856, HFK 05/10/2005 Add summary format to report
*!*  lcRpForm = IIF(lcRpFormat = 'L','SOORDHDL','SOORDHDS','SOORDHDS')
lcRpForm = IIF(lcRpFormat = 'L','SORDHDEL','SORDHDES')
*-B127856, HFK 05/10/2005 Add summary format to report
= lfRepPltFr(lcRpForm)

*!*************************************************************
*! Name      : lfGetRepVr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Get Report Variables
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*Use full Multi currency format capability.
FUNCTION lfGetRepVr
*-- ReIndex work file if user change sort By [Begin]
IF llChSortBy
  DO CASE
    CASE lcRpSortBy = 'O'		&& Sort by Order Case
      lcSortTitl= LANG_Soordhd_Order
      IF llCurInGrp
        lcBreak   = 'CCURRCODE'
        lcGrpExpr = [CCURRCODE + LANG_Soordhd_Total  +':']
        lcIndexTg = [CCURRCODE+ORDER]
      ELSE
        lcBreak   = ''
        lcGrpExpr = ''
        lcIndexTg = [ORDER]
      ENDIF

       
    CASE lcRpSortBy = 'A'		&& Sort by Account Case
      IF llCurInGrp
        lcBreak = 'ACCOUNT+CCURRCODE'
        lcGrpExpr = [ACCOUNT + "/" + CCURRCODE + LANG_Soordhd_Total +':']
        lcIndexTg = [ACCOUNT+CCURRCODE+ORDER]
      ELSE
        lcBreak = 'ACCOUNT'
        lcGrpExpr = [ACCOUNT + LANG_Soordhd_Total +':']
        lcIndexTg = [ACCOUNT+ORDER]
      ENDIF
      lcSortTitl= LANG_Soordhd_Account

    CASE lcRpSortBy = 'C'		&& Sort by CustPo Case
      IF llCurInGrp
        lcBreak = 'ACCOUNT+CUSTPO+CCURRCODE'
        lcGrpExpr = [ACCOUNT + '/' + CUSTPO + '/' + CCURRCODE + LANG_Soordhd_Total +':']
        lcIndexTg = [ACCOUNT+CUSTPO+CCURRCODE+ORDER]
      ELSE
        lcBreak = 'ACCOUNT+CUSTPO'
        lcGrpExpr = [ACCOUNT + '/' + CUSTPO + LANG_Soordhd_Total +':']
        lcIndexTg = [ACCOUNT+CUSTPO+ORDER]
      ENDIF

      lcSortTitl= LANG_Soordhd_CustPo

    CASE lcRpSortBy = 'P'		&& Sort by Priority Case
      IF llCurInGrp
        *B037288,1 HMA 08/18/2004 Change the group expression in case of sort by periority.[BEGIN]

        *lcBreak = 'DTOS(COMPLETE)+PRIORITY+CCURRCODE'
        *lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + '/' + CCURRCODE + LANG_Soordhd_Total +':']
        *lcIndexTg = [DTOS(COMPLETE)+PRIORITY+CCURRCODE+ACCOUNT+ORDER]
      *ELSE
        *lcBreak = 'DTOS(COMPLETE)+PRIORITY'
        *lcGrpExpr = [DTOC(COMPLETE) + '/' + PRIORITY + LANG_Soordhd_Total +':']
        *lcIndexTg = [DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER]

        lcBreak = 'PRIORITY+DTOS(COMPLETE)+CCURRCODE'
        lcGrpExpr = ['Periority'+'('+ALLTRIM(PRIORITY)+')  '+DTOC(COMPLETE)+CCURRCODE +'    '+LANG_Soordhd_Total +':']
        lcIndexTg = [PRIORITY+DTOS(COMPLETE)+CCURRCODE+ACCOUNT+ORDER]
      ELSE 
        lcBreak = 'PRIORITY+DTOS(COMPLETE)'
        lcGrpExpr = ['Periority'+'('+ALLTRIM(PRIORITY)+')  '+DTOC(COMPLETE)+'    '+ LANG_Soordhd_Total +':']
        *-- B999999,1 fix prblem of wrong sort with priority[Start]
        lcIndexTg = [PRIORITY+DTOS(COMPLETE)+ACCOUNT+ORDER]
        *--lcIndexTg = [DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER]
        *-- B999999,1 fix prblem of wrong sort with priority[End]
        *B037288,1 HMA 08/18/2004 Change the group expression in case of sort by periority.[END]
      ENDIF

      lcSortTitl= LANG_Soordhd_Priority


    CASE lcRpSortBy = 'S'		&& Sort by Primary sales rep. Case
      IF llCurInGrp
        lcBreak   = 'REP1+CCURRCODE'
        lcGrpExpr = [REP1 + '/' + CCURRCODE + LANG_Soordhd_Total +':']
        lcIndexTg = [REP1+CCURRCODE+ORDER]
      ELSE
        lcBreak   = 'REP1'
        lcGrpExpr = [REP1 + LANG_Soordhd_Total +':']
        lcIndexTg = [REP1+ORDER]
      ENDIF
      lcSortTitl= LANG_Soordhd_PSalesRep

    CASE lcRpSortBy = 'U'		&& Sort by Currency Case
      lcBreak   = 'CCURRCODE'
      lcGrpExpr = [lfCurrDesc() + LANG_Soordhd_Total +':']
      lcIndexTg = [CCURRCODE+ORDER]
      lcSortTitl= LANG_Soordhd_Currency

  ENDCASE

  IF llCurInGrp AND (lcRpSortBy <> 'U')

    IF lcRpSortBy="O"
      lcSortTitl = LANG_Soordhd_Currency+"/" + lcSortTitl
    ELSE
      lcSortTitl = lcSortTitl+"/"+LANG_Soordhd_Currency
    ENDIF
    
  ENDIF

ENDIF

IF llMultCurr
  lcLineCurr = lfCurrPrnt()
  lcHedTitle = lcSortTitl + ", "+LANG_Soordhd_Print+" : "
  DO CASE
    CASE lcRpCurr = "F"  && Foreign Currency.
      lcHedTitle = lcHedTitle + LANG_Soordhd_ForeignCurr
    CASE lcRpCurr = "O"  && Original Rates
      lcHedTitle = lcHedTitle + LANG_Soordhd_OriginalTrans
    CASE lcRpCurr = "D"  && Rates on date
      lcHedTitle = lcHedTitle + LANG_Soordhd_RateDefinedOn+ DTOC(ldRpExDate) + "."
    OTHERWISE            && Rates defined now.
      lcHedTitle = lcHedTitle + LANG_Soordhd_RateDefinedNow 
  ENDCASE
ENDIF

*-- ReIndex work file if user change sort By [End  ]

*!*************************************************************
*! Name      : lfAsignVar
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Assign all Report Variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
*Use full Multi currency format capability.
FUNCTION lfAsignVar

*Call Function to open closed files in lfClearRep Function. [Begin]
PARAMETERS llFromPrg
*Call Function to open closed files in lfClearRep Function. [End  ]

*Define array if called from OG Only. [Begin]
IF !llFromPrg
  *-- Make Sort by Arrays and fill it. [Begin]
  DIMENSION laSortDesc[5,1],laSortVal[5,1]

  laSortDesc[1,1] = LANG_Soordhd_Order
  laSortDesc[2,1] = LANG_Soordhd_Account
  laSortDesc[3,1] = LANG_Soordhd_CustPo
  laSortDesc[4,1] = LANG_Soordhd_Priority
  laSortDesc[5,1] = LANG_Soordhd_PrimSalesRep 

  laSortVal[1,1]  = "O"
  laSortVal[2,1]  = "A"
  laSortVal[3,1]  = "C"
  laSortVal[4,1]  = "P"
  laSortVal[5,1]  = "S"
  *-- Make Sort by Arrays and fill it. [Begin]

  llMultCurr = gfGetMemVar('llMulCurr')
ENDIF
*Define array if called from OG Only. [End  ]

IF llMultCurr
 
  IF !USED("SYCINT")
    lcSelectCommand=[SELECT * FROM SYCINT]
    lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
  ENDIF

  *-- Add Currency item to sort by array. [Begin]
  DIMENSION laSortDesc[ALEN(laSortDesc,1) + 1 , 1] , laSortVal[ALEN(laSortVal,1) + 1 , 1]
  laSortDesc[ALEN(laSortDesc,1),1] = LANG_Soordhd_Currency
  laSortVal[ALEN(laSortDesc,1),1]  = "U"
  *-- Add Currency item to sort by array. [End  ]

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
    IF !USED('SYCCURR')
    = gfOpenFile(oAriaApplication.SysPath +'SYCCURR',oAriaApplication.SysPath +'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF

  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]

  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]

ENDIF


IF !USED('SYCCOMP')
  lcSelectCommand=[SELECT * FROM SYCCOMP ]
  lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
ENDIF

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Activate currency display screen to get user
*!           : selection for currencies.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfCurrPrnt
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfCurrPrnt
PRIVATE lcCurrCode
*-- Not Multi Currency Or it is and any Equavelent method.
IF lcRpCurr <> "F"
  lcCurrCode = [oAriaApplication.BaseCurrency]
ELSE && Multi Currency and Print forign currency.
  lcCurrCode = [cCurrCode]
ENDIF
RETURN lcCurrCode


*!*************************************************************
*! Name      : lfChCurSm
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Compute Currency symbol to print.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfChCurSm
PRIVATE lcCurrCurr,lnCurrAls,lcSelectCommand,lnResult 

lnCurrAls = SELECT(0)
lcCurrCurr = ALLTRIM(EVALUATE(lcLineCurr))

*--Call Function to open closed files in lfClearRep Function.
IF !USED("SYCINT")
  =lfAsignVar(.T.)
ENDIF


lcSelectCommand=[SELECT cCurrCode,cCurrency,cCurrencyI FROM SYCINT WHERE cCurrCode=']+lcCurrCurr+[']
lnResult = oAriaApplication.remotesystemdata.execute(lcSelectCommand,"","SYCINT","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
IF lnResult >= 1 AND !EOF()
  lcCurrRtL = ALLTRIM(cCurrency)
  lcCurrSet = ALLTRIM(cCurrencyI)
  SET CURRENCY TO lcCurrSet
  SET CURRENCY &lcCurrRtL
ENDIF
SELECT (lnCurrAls)
RETURN ''

*!*************************************************************
*! Name      : lfCurrDesc
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Currency description if sort by currency.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Currency description.
*!*************************************************************
*! Modification : ....
*!*************************************************************

FUNCTION lfCurrDesc
PRIVATE lcCurrVal , lcCurDesc

lcCurDesc = ''
lcCurrVal  = ALLTRIM(cCurrCode)
lnCurVlPos = ASCAN(laCurrVal,lcCurrVal)
IF lnCurVlPos > 0
  lcCurDesc  = laCurrDesc[lnCurVlPos,1]
ENDIF
RETURN PADR(ALLTRIM(lcCurDesc),20)

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate fixed filter position within array.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : Position
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos


*!************************************************************
*! Name      : lfFillData
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Evaluate all memvars and insert new line.
*!************************************************************
*! Passed Parameters  : ...
*!************************************************************
*! Returns            : Position
*!************************************************************
*! Modification : ....
*! B039069,1 MMT 02/22/2005 report peformance Improvement
*!************************************************************
*TO Fasten the report long form.
FUNCTION lfFillData
PARAMETERS llEditOnly
*-- if it is long format
IF lcRpFormat = "L"
  PRIVATE lcCanReson,lcCurrDesc
  STORE "" TO lcCanReson,lcCurrDesc

  IF !EMPTY(ccancreson)
    lcCanReson = gfCodDes(CCANCRESON, 'CCANCRESON')
  ENDIF

  IF llMultCurr
    lcCurrDesc = lfCurrDesc()
  ENDIF

  m.cSeaDesc =  PADR(SEASON,6) + '-' + gfCodDes(SEASON, 'SEASON')
  m.cDivDesc =  PADR(cDIVISION,6) + '-' + gfCodDes(cDIVISION, 'CDIVISION')
  m.cTrmDesc =  gfCodDes(CTERMCODE, 'CTERMCODE')
  m.cViaDesc =  gfCodDes(SHIPVIA, 'SHIPVIA')
  m.cSpcDesc =  gfCodDes(SPCINST, 'SPCINST')
  m.cRegDesc =  gfCodDes(CUSTOMER.REGION, 'REGION')
  m.cDecl_code= gfCodDes(DECL_CODE, 'DECL_CODE')
  m.cCanDesc = lcCanReson
  m.cCurDesc = lcCurrDesc
ENDIF

IF llEditOnly
  REPLACE cSeaDesc WITH PADR(SEASON,6) + '-' + gfCodDes(SEASON, 'SEASON') ,;
          cDivDesc WITH PADR(cDIVISION,6) + '-' + gfCodDes(cDIVISION, 'CDIVISION') ,;
          cTrmDesc WITH gfCodDes(CTERMCODE, 'CTERMCODE') ,;
          cViaDesc WITH gfCodDes(SHIPVIA, 'SHIPVIA') ,;
          cSpcDesc WITH gfCodDes(SPCINST, 'SPCINST') ,;
          cRegDesc WITH gfCodDes(CUSTOMER.REGION, 'REGION'),;
          cDecl_code WITH gfCodDes(DECL_CODE, 'DECL_CODE') ,;
          cCanDesc WITH lcCanReson     ,;
          cCurDesc WITH lcCurrDesc
ELSE

  m.cTempKey = EVALUATE(lcIndexTg)
  INSERT INTO (WorkFile) FROM MEMVAR

ENDIF
*!**************************************************************************
*! Name      : lfvEdiOrd
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : to validate (Print Orders/Edi Orders) popup in OG
*!**************************************************************************
*! Passed Parameters  : ...
*!**************************************************************************
*! Returns   : None
*!**************************************************************************
*! Modification : ....
*!**************************************************************************
*-Due to Enhancement 128374, this finction is no longer in use
*!*  FUNCTION lfvEdiOrd
*!*  lcRpEdiFlt = ""
*!*  *- E128374,1 HFK 06/05/2005 [Start]
*!*  *!*  IF 'EB' $ oAriaApplication.CompanyInstalledModules AND lcRpEdiPrn <> "B"
*!*  *!*    lcRpEdiFlt = IIF(lcRpEdiPrn="O",[!OrdHdr.lEdiOrder],[OrdHdr.lEdiOrder])
*!*  *!*  ENDIF
*!*  IF 'EB' $ oAriaApplication.CompanyInstalledModules 
*!*    DO CASE
*!*      CASE ("A" $ lcRpType .AND. !("B" $ lcRpType) .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder]
*!*      CASE (!("A" $ lcRpType) .AND. "B" $ lcRpType .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T']
*!*      CASE (!("A" $ lcRpType) .AND. !("B" $ lcRpType) .AND. "C" $ lcRpType)
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O']
*!*        SET FILTER TO (ORDHDR.STATUS$lcRpStatus) .AND. (lEdiOrder .And. cOrdType = 'O')
*!*      CASE ("A" $ lcRpType .AND. "B" $ lcRpType .AND. !("C" $ lcRpType))
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder  .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'T')]
*!*      CASE ("B" $ lcRpType .AND. "C" $ lcRpType .AND. !("A" $ lcRpType))
*!*        lcRpEdiFlt = [OrdHdr.lEdiOrder]
*!*      CASE ("A" $ lcRpType .AND. !("B" $ lcRpType) .AND."C" $ lcRpType )
*!*        lcRpEdiFlt = [!OrdHdr.lEdiOrder .OR. (OrdHdr.lEdiOrder .AND. OrdHdr.cOrdType = 'O')]
*!*    ENDCASE
*!*  ENDIF
*!*  *- E128374,1 HFK 06/05/2005 [End]
*!*  lnOrdPos = ASCAN(loOGScroll.laOGFxFlt,'ORDHDR.ORDER')
*!*  *-- Check if The user entered a date and get the date period to be printed.

*!*  IF lnOrdPos > 0
*!*    lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
*!*    lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
*!*    IF USED(lcOrderSel)
*!*      SELECT(lcOrderSel)
*!*      ZAP 
*!*    ENDIF 
*!*  ENDIF 

*!*  llClearOrd = .T.

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 12/04/2003
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************


FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTarget)
    FOR lnTarget = 1 TO ALEN(laRpTarget,1)
      lcStatusStr = lcStatusStr + ", " + laRpTarget[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*-- end of RefreshStatus.
*!*************************************************************
*! Name      : lfGrpExp
*: Developer : Mariam Mazhar [MMT]
*: Date      : 01/02/05
*! Purpose   : function to put a group title in layout 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************

FUNCTION lfGrpExp
lcRetValue = IIF(lcRpSortBy='O','Order: '+ ORDER,IIF(lcRpSortBy='A','Account:'+ACCOUNT+' - '+CUSTOMER.BTNAME,iif(lcRpSortBy='C','Cust PO.:'+account+'/'+CUSTPO,iif(lcRpSortBy='P','Priority:'+'('+alltrim(PRIORITy)+') '+dtoc(Complete),''))))
lcRetValue = iif(lcRpSortBy='S','Primary Sales Rep.:'+REP1,iif(lcRpSortBy='U','Currency:'+lfCurrdesc(),lcRetValue))
RETURN lcRetValue




*!*************************************************************
*! Name      : lfvTypes
*! Developer : Heba Fathi (HFK)
*! Date      : 06/05/2005
*! Purpose   : - Evaluate order types expression.
*!*************************************************************
*- E128374,1 HFK 06/05/2005 [Start]
FUNCTION lfvTypes
PRIVATE lcOldTypes,lcCurrChr

lcOldTypes = lcRpType  && Save old status value.
= lfOGMover(@laRpTypSrc,@laRpTypDst,"Select Order Type",.T.,'')  && call mover function.

lcRpType = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTypDst[1])
  FOR lnI = 1 TO ALEN(laRpTypDst,1)
    lcRpType = lcRpType + IIF(laRpTypDst[lnI] = "Orders",'O',;
                          IIF(laRpTypDst[lnI] = "EDI Temporary Orders",'T',;
                          IIF(laRpTypDst[lnI] = "EDI Received Orders",'R','')))
  ENDFOR
ENDIF
lcRpType = IIF(EMPTY(lcRpType),'ABC',ALLTRIM(lcRpType))
lnOrdPos = ASCAN(loOGScroll.laOGFxFlt,'ORDHDR.ORDER')

*-- Check if The user entered a date and get the date period to be printed.
IF lnOrdPos > 0
  lnOrdPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnOrdPos,1)
  lcOrderSel =IIF(!EMPTY(laOgFxFlt[lnOrdPos,6]),laOgFxFlt[lnOrdPos,6],'')
  IF USED(lcOrderSel)
    SELECT(lcOrderSel)
    ZAP 
  ENDIF 
ENDIF 
llClearOrd = .T.
*- E128374,1 HFK 06/05/2005 [End]
*!*************************************************************
*! Name      : RefreshTypes
*! Developer : Heba Fathi (HFK)
*! Date      : 06/05/2005
*! Purpose   : Return the selected types in the ReadBox
*!*************************************************************
*- E128374,1 HFK 06/05/2005 [Start]
FUNCTION RefreshTypes
LOCAL lcTypesStr, lnTarget
lcTypesStr = ""
  IF !EMPTY(laRpTypDst)
    FOR lnTarget = 1 TO ALEN(laRpTypDst,1)
      lcTypesStr = lcTypesStr+ "," + laRpTypDst[lnTarget]
    ENDFOR 
    lcTypesStr = SUBSTR(lcTypesStr,2)
  ENDIF   
  RETURN lcTypesStr
ENDFUNC 
*- E128374,1 HFK 06/05/2005 [End]
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/12/98
*! Purpose   : - Evaluate Status expression.
*!           : - Raise change status flag. 
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
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.
=gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.

IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Cancelled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length

  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF

DO lpChkStat
*-- end of lfvOStatus.
