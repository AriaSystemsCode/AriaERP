*:***************************************************************************
*: Program file  : ALPAKLG.PRG
*: Program desc. : Packing List Log Report
*: For Report    : ALPAKLG.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order Allocation (AL)
*: Developer     : Mohamed Shokry (MHM)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: This Program is due to E500338 ...
*:***************************************************************************
*: Modifactions :
*: E301439,1 HBG 08/08/2000 Add an option to allow different currencies
*: E119890,1 ABD 10/13/2003 Add the Store/DC to the Packing List Log report. 
*: E119890,1 ABD [Modfification at the FRX only]
*:***************************************************************************

*-- Variables Declaration
lcSPTime = TIME()         && Variable to hold the Start print Time
*E301439,1 HBG Variable to hold The Amount [Begin]
lnCurrAmt = 0
*E301439,1 HBG [End]
*-- if Filter was changed

IF llOGFltCh
  DO lpCreaTemp  && Create Temp Cursor
  DO lpColect    && Collect data
ENDIF

*-- Asking if no records (Display message) otherwise print report [Begin.]
IF RECCOUNT(lcPackTmp) = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

*-- llLastPage : Detect we are to print last page (Used within .FRX)
*-- llNotRep : Detect we are to print Not Repeated Pk#,Order,... (Used within .FRX)
PRIVATE llLastPage
llLastPage = .F.
llNotRep = .F.
*To get the date in a variables (Date Sides is between)
lnSHdate = ASCAN(laOgFxFlt,'PACK_HDR.SHIP_DATE')    &&Vriable to get ship_date
IF lnSHdate  = 0
  STORE {} TO ldLDate,ldUDate
ELSE
  lnSHdate  = ASUBSCRIPT(laOgFxFlt,lnSHdate ,1)
  ldLDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnSHdate ,6],1,10)))   && Variable to hold the Lower Date
  ldUDate = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnSHdate ,6],12,20)))  && Variable to hold the Upper Date
ENDIF 

*-- Relation Section
SELECT PACK_HDR
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER



SELECT (lcPackTmp)
SET ORDER TO TAG &lcPackTmp
SET RELATION TO pack_no INTO Pack_hdr ,;
                "O" + PACK_HDR.ORDER +  STR(nordlineno,6) INTO Ordline,;
                "O" + PACK_HDR.ORDER INTO OrdHdr ,;
                Pack_No INTO (lcRevTemp) ADDITIVE

*!--End of Relations Section
*!-- Printing Section
DO gfDispRe WITH EVALUATE('lcRpName')
*!-- End of Printing Section

*--To Avoid Engaged of Relation
SET RELATION TO  
SELECT PACK_HDR
SET RELATION TO  

*-- end report prg

*!**************************************************************************
*! Name      : lpCreaTemp
*! Developer : Mohamed Shokry
*! Date      : 16/05/2000
*! Purpose   : Procedure to create Temp. File 
*!**************************************************************************
*! Example   : DO lpCreaTemp
*!**************************************************************************
*!
PROCEDURE lpCreaTemp
*-- check If File is created or not
IF USED(lcPackTmp) AND RECCOUNT(lcPackTmp) > 0
  USE IN (lcPackTmp)
  USE IN (lcRevTemp)
ENDIF
*-- Create File
IF !USED(lcPackTmp)
  IF TYPE("laTempStru[1,1]") $ "UL" 
    DIMENSION laTempStru[1,4]
    SELECT PACK_LIN
    =AFIELDS(laTempStru)

    *E301439,1 HBG 08/08/2000 Add currency code to the array that hold the structer of the file [Begin]
    =lfAddField("laTempStru","cCurrCode","C",3,0)
    *E301439,1 [End]
    
  ENDIF
  CREATE CURSOR (lcPackTmp) FROM ARRAY laTempStru          
  ZAP

  *E301439,1 HBG 08/08/2000 Add currency code to the index [Begin]
  IF llMultCurr AND (lcRpCurr = "F") 
    *--Index on Pack Number On reverse Temp.  To get Last Reccount  
    INDEX ON Pack_No TAG (lcRevTemp) OF (gcWorkDir + lcPackTmp + ".CDX") DESCENDING
    *-- Index On Temp File  
    INDEX ON cCurrCode + Pack_No TAG (lcPackTmp) OF (gcWorkDir + lcPackTmp + ".CDX")
  ELSE
    *--Index on Pack Number On reverse Temp.  To get Last Reccount  
    INDEX ON  Pack_No TAG (lcRevTemp) OF (gcWorkDir + lcPackTmp + ".CDX") DESCENDING
    *-- Index On Temp File  
    INDEX ON  pack_no TAG (lcPackTmp) OF (gcWorkDir + lcPackTmp + ".CDX")
  ENDIF
  *E301439,1 [End]

  
  DO lpRevOpen
ENDIF


*-- End of lpCreaTemp.

*!**************************************************************************
*! Name      : lpRevOpen
*! Developer : Mohamed Shokry
*! Date      : 16/05/2000
*! Purpose   : Function  To Get Reverse Of File In Another Alias
*!**************************************************************************
*! Example   : =lpRevOpen
*!**************************************************************************
*!
FUNCTION lpRevOpen
PRIVATE lcFullPath , lcRevTable , lnActAlias

*-- Save Setting
lnActAlias = SELECT(0)
lcFullPath = SET("FULLPATH")

*-- Get Cursor path [Begin]
SET FULLPATH ON
lcRevTable = FULLPATH(DBF(lcPackTmp))
*-- Get Cursor path [End  ]

*-- Use the table again in reverse order [Begin]
USE (lcRevTable) IN 0 AGAIN ALIAS (lcRevTemp) ORDER (lcRevTemp)
*-- Use the table again in reverse order [End  ]

*-- Restore Setting
SET FULLPATH &lcFullPath
SELECT (lnActAlias)
*-- end of lpRevOpen.

*!**************************************************************************
*! Name      : lpColect
*! Developer : Mohamed Shokry
*! Date      : 16/05/2000
*! Purpose   : Procedure TO Colecte Data
*!**************************************************************************
*! Example   : Do lpColect()
*!**************************************************************************
*!
PROCEDURE lpColect
lcRpExp = STRTRAN(lcRpExp,"PACK_HDR.","")
SELECT Pack_Hdr
GO TOP
*E301439,1 HBG 08/08/2000 Set relation with ordhdr [Begin]
SET RELATION TO  "O" + ORDER INTO OrdHdr
*E301439,1 [End]

SCAN FOR &lcRpExp
  IF SEEK(pack_no,"PACK_LIN")
    SELECT PACK_LIN
    SCAN REST WHILE pack_no = PACK_HDR.pack_no
      SCATTER MEMVAR MEMO
      *E301439,1 HBG 08/08/2000 Get the value of currency code [Begin]
      m.cCurrCode = ORDHDR.cCurrCode
      *E301439,1 [End]
      INSERT INTO (lcPackTmp) FROM MEMVAR
    ENDSCAN
  ENDIF
ENDSCAN
SET RELATION TO  
*-- end of lpColect.

*!**************************************************************************
*! Name      : lfLastPage
*! Developer : Mohamed Shokry
*! Date      : 25/05/2000
*! Purpose   : Fuction to Get Last Page (Called from .FRX summary band)
*!**************************************************************************
*! Example   : =lfLastPage
*!**************************************************************************
*!
FUNCTION lfLastPage
llLastPage = .T.
RETURN ""
*-- end of lfLastPage.

*!*************************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Shokry (MHM)
*! Date      : 16/05/2000
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Called from : Option Grid
*!*************************************************************************
*! Example   : =lfClearRep()
*!*************************************************************************
*
FUNCTION lfClearRep
llOGFltCh = .T.
*-- Delete temporary  REP. file.
IF USED(lcPackTmp)
 USE IN (lcPackTmp)
 USE IN (lcRevTemp)
ENDIF
=lfSpcClear()

*E301439,1 HBG Report support multi currency format close needed files.[begin]
IF llOpenComp AND USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF

IF llMultCurr
  SET CURRENCY TO lcCurrSymb
  SET CURRENCY &lcCurAlign

  IF llOpenInt AND USED("SYCINT")
    USE IN SYCINT 
  ENDIF

  IF llOpenCurr AND USED("SYCCURR")
    USE IN SYCCURR
  ENDIF

  IF llOpenExch AND USED("SYCEXCH")
    USE IN SYCEXCH
  ENDIF  
ENDIF
*E301439,1 HBG [End]
*-- end of lfClearRep.
*!**************************************************************************
*! Name      : lfNotRep
*! Developer : Mohamed Shokry
*! Date      : 25/05/2000
*! Purpose   : Fuction to Get Not repeated pk# ,order,...
*!**************************************************************************
*! Example   : =lfNotRep
*!**************************************************************************
*!
FUNCTION lfNotRep
llNotRep = .T.
RETURN ""
*-- end of lfNotRep.
*!**************************************************************************
*! Name      : lfNNotRep
*! Developer : Mohamed Shokry
*! Date      : 25/05/2000
*! Purpose   : Fuction to Get Next Not repeated pk# ,order,...
*!**************************************************************************
*! Example   : =lfNotRep
*!**************************************************************************
*!
FUNCTION lfNNotRep
llNotRep = .F.
RETURN ""
*-- end of lfNotRep.

*!*************************************************************
*! Name      : lfFillVars
*: Developer : ABDOU ELGENDI - (ABD)
*! Date      : 05/31/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFillVars()
*!*************************************************************
*E301439,1
FUNCTION lfFillVars

*E301424,1 ABD- Report support multi currency format open needed files.[begin]
IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF

  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  

  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
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

*-- End Of lfFillVars.



*!**************************************************************************
*! Name      : lfAddField
*! Developer : Hend Ghanem (HBG)
*! Date      : 16/05/2000
*! Purpose   : Add fields to the the array of file structer
*!**************************************************************************
*! Example   : =lfAddField()
*!**************************************************************************
*!
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + 1
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec
*-- end of lfAddField.


*!*************************************************************
*! Name      : lfGtCurDes
*! Developer : Hend Ghanem (HBG)
*! Date      : 087/17/2000
*! Purpose   : Function Get the Description ofthe Currency
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGtCurDes()
*!*************************************************************
*!*E301439,1
FUNCTION lfGtCurDes
IF .F.
IF SEEK(cCurrCode,'SYCCURR')
  RETURN SYCCURR.cCurrDesc
ENDIF

RETURN ""
ENDIF
*-- end of lfGtCurDes.

FUNCTION lfvFormat
IF (llMultCurr AND (lcRpCurr = 'F') AND lcRpFormat <> 'S')
  lcRepGrp = [OrdHdr.cCurrCode]
ELSE
  lcRepGrp = [""]
ENDIF
*-- end of lfvFormat.


*!*************************************************************
*! Name      : lfvRepCurr
*! Developer : Hend Ghanem (HBG)
*! Date      : 087/17/2000
*! Purpose   : 
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRepCurr()
*!*************************************************************
*!*E301439,1
FUNCTION lfvRepCurr

llOGFltCh = .T.
=gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/02/2000
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm

SELECT Customer
SET ORDER TO Customer
LOCATE
*-- End of lfsrAcc.