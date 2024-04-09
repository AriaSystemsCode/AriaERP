*****************************************************************************
*: Program file  : ICMUL10.PRG  (C# 101624)
*: Program desc. : Importing physical inventory from Excel files.
*:               : Convert MUL1000.PRG & MUL100.SCX From A2.6 To A2.7
*:               : For Customer MulBerry.
*: Module        : IC (Inventory Locking)
*: Screens       : ICMUL10.SPX
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Notes         : This program use Excel file (*.CSV) to importing phisical
*:               : inventory instead of enter phisical manualy.
*:               : And use customized file (MULLOCK.DBF).
*:               :
*:     IMPORTANT : Excel file Structure : 
*:               : In the first cell there is a heading.
*:               : In cells A2 (Date) and A3 (warehouse),,,,,,
*:               : 1/4/99,,,,,,,,
*:               : WAREH1,,,,,,,,
*:               : STYLE,CW,DESC,STK1,STK2,STK3,,,
*:               : The Style And Stk's Start from the 5th cell.
*****************************************************************************
*: Calls : FUNCTIONS  : gfSetup,gfOpenFile,lfvProceed,lfvPath,lfvLock,
*:                      lfvFile,lfUpdInv,lfUpdTGl,lfvStyles,lfPrnRep,
*:                      lfUnLock,lfUpdRes,lfFiilDiv,lfvDiv,lfFiilGrp,lfvGrp.
*****************************************************************************
*: Passed Parameters  : None.
*****************************************************************************
*: Modifications :
*: B603831 08/22/2000 MHM Subscript out of bounds
*:                       To solve these bug do the following
*:                       Increase the length of the array laOldStk to 11 
*:                       Replace laOldStk[11] with the transaction date .
*: B603873 09/04/2000 AMH Incorrect Updating in StyInvJl
*: B607127,1 KHM 04/10/2003 Getting the correct cost according to the costing method
*: B607127,1                and sum the styinvjl regardless of the session check.
*****************************************************************************

*-- Initializing the necessary variables.
lnanswer = 3
DIMENSION laTFile[1], laTWare[1], laDiv[1,1],laDivT[1,1],laDivS[1,1], laGrpS[1],laGrp[1]
STORE '' TO laDiv, laGrp , laDivS, laGrpS,laDivT,laGrpT
lcPathName = SPACE(40)
lcPathNam  = SPACE(40)
lcLinkCode = ''
lcGLFYear  = SPACE(04)  && Fiscal Year
lcGLPeriod = SPACE(02)  && Period
lcGLDTemp  = ''

*B607127,1 KHM 04/10/2003 (Begin) Getting the style costing & multi ware house settings
lcCostMthd = gfGetMemVar('M_Cost_Meth')
lcMullWH   = gfGetMemVar('M_WAREHOUSE')
*B607127,1 KHM 04/10/2003 (End)

IF !gfSetup()
  RETURN
ENDIF
*-- To get the picture of the major style.
lcStyPic  = gfItemMask("HI")
lcMajPict = gfItemMask("PM")
lnMajLen  = LEN(lcMajPict)
lcSepar   = SUBSTR(lcstypic,lnMAjlen+1,1)
llLinkGL  = IIF(gfGetMemVar('M_Link_GL',gcAct_Comp)='Y',.T.,.F.)
=gfOpenFile(gcdatadir+'CODES','CODES','SH')
=gfOpenFile(gcdatadir+'MULLOCK','MULLOCK','EX')
STORE '' TO lcStrDiv,lcStrGrp
=lfFiilGrp()
=lfFiilDiv()
DO gcScrDir+'IC\'+("ICMUL10.SPR")
*****************************************************************************
*: Program file  : lfvProceed.PRG  (C# 101624)
*: Program desc. : Valid Function For Update Button.
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  lfvProceed()
*****************************************************************************
*:
FUNCTION lfvProceed

IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Are you sure you want to proceed ?')=2
  _CUROBJ = OBJNUM(pbCancel)
  RETURN
ENDIF
=gfOpenFile(gcdatadir+'STYLE','STYLE','SH')
=gfOpenFile(gcdatadir+'STYDYE','STYDYE','SH')
=gfOpenFile(gcdatadir+'WAREHOUS','WAREHOUS','SH')
=gfOpenFile(gcdatadir+'STYINVJL','STYINVJL','SH')
=gfOpenFile(gcdatadir+'INVTADJ','INVTADJ','SH')
IF llLinkGL  
  =gfOpenFile(gcdatadir+'GLDIST','GLDISTAC','SH')
  *AMH Creating lcGLDTemp file [Start]
  SELECT GLDIST
  =AFIELDS(laGlData)
  lcGLDTemp  = gfTempName()
  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
  *AMH Creating lcGLDTemp file [End  ]
ENDIF  
lcRejSty = gfTempName()
*: B603831 08/22/2000 MHM [Begin]
*DIMENSION laOldStk(10) , laNewStk(9)
DIMENSION laOldStk(11) , laNewStk(9)
*: B603831 08/22/2000 MHM [End]

CREATE TABLE (gcWorkDir+lcRejSty) (WARECODE C(6), STYLE C(19), REASON C(60))
INDEX ON WARECODE+STYLE TAG &lcRejSty

CREATE CURSOR TmpInv (STYLE C(12),COLOR C(06),DESC C(20), STK1 N(6), STK2 N(6),;
                      STK3 N(6), STK4 N(6), STK5 N(6), STK6 N(6), STK7 N(6), STK8 N(6),;
                      TOTSTK N(7))


SET RELATION TO STYLE+lcSepar+COLOR INTO STYLE ADDI
lnLenth = ALEN(laTWare,1) 
STORE 0 TO lnAdj
FOR lncnt = 1 TO ALEN(laTFile)
  SELECT TmpInv
  ZAP
  APPEND FROM (ALLT(lcpathname)+IIF(RIGHT(lcPathName,1) <> "\","\","")+;
               laTFile(lnCnt)) DELIMITED
  GO 2
  ldDate = CTOD(STYLE)
  SKIP
  lcWare = PADR(ALLT(STYLE),6)
  IF !lfVFile()
    LOOP
  ENDIF
  IF !CHECKPRD(ldDate,'lcGLFYear','lcGLPeriod','IA')
    RETURN
  ENDIF
  *AMH zap lcGLDTemp file [Start]
  *IF llLinkGL
  *  SELECT GLDIST
  *  lcGLDTemp  = gfTempName()
  *  = AFIELDS (laGlData)
  *  CREATE TABLE gcWorkDir+(lcGLDTemp) FROM ARRAY laGlData
  *ENDIF
  IF llLinkGL
    SELECT (lcGLDTemp)
    ZAP
  ENDIF
  *AMH zap lcGLDTemp file [End  ]
  SELECT TmpInv
  SCAN FOR RECNO() >=5 AND !EMPTY(STYLE)
    WAIT WINDOW "Uploading file " + laTFile(lnCnt)+":"+  lcStyPic  +":"+ ALLT(STYLE+lcSepar+COLOR) NOWAIT
    IF !lfvStyles()
      LOOP
    ENDIF
    REPLACE TOTSTK WITH STK1+STK2+STK3+STK4+STK5+STK6+STK7+STK8
    =lfUpdInv(STYLE+lcSepar+COLOR,lcWare)
    IF llLinkGL 
      =lfUpdTGl()
    ENDIF
  ENDSCAN
  STORE 0 TO lnElm,lnDelElm
  lnElm = ASCAN(laTWare,ALLT(lcWare))
  lnDelElm = IIF(lnElm <> 0, ADEL(laTWare,lnElm),lnDelElm)
  lnAdj = lnAdj + IIF(lnDelElm <> 0,1,0)
ENDFOR
IF lnLenth - lnAdj = 0
  DIMENSION laTWare(1)
  STORE "" TO laTWare
ELSE
  DIMENSION laTWare(lnLenth - lnAdj)
ENDIF
=lfUnlock()
WAIT CLEAR
SELECT &lcRejSty
GO TOP
IF !EOF()
   lnAnswer = gfModalGen("QRM00000B42017","DIALOG",.F.,.F.,'One or more '+ lcStyPic +' got rejected.')
   =lfPrnRep()
ENDIF
SHOW GET pbProceed DISABLE
*: B603831 08/22/2000 MHM [Begin]
*SELECT (lcGLDTemp)
*USE
*ERASE (gcWorkDir+lcGLDTemp+'.DBF')
*: B603831 08/22/2000 MHM [End]

*****************************************************************************
*: Program file  : lfvPath.PRG  (C# 101624)
*: Program desc. : Valid Function For Path Button.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvPath()
*****************************************************************************
*:
FUNCTION lfvPath

DIMENSION laSFile[1,1]
STORE ' ' TO laSFile

lcOldPath = FULLPATH("")
lcPathName=GETDIR('','Select Source Directory')
IF EMPTY(lcPathName)
  SHOW GET pbProceed DISABLE
  STORE SPACE(40) TO lcPathName
  =lfRefresh()
  SET DEFAUL TO (lcOldPath)
  RETURN
ELSE
  =lfRefresh()
ENDIF
SET DEFA TO (lcPathName)
IF ADIR(laFiles,"*.CSV") = 0 
  SET DEFAULT TO (lcOldPath)
  *--- "No files to be uploaded in the specified path."
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'No files to be uploaded in the specified path.')
  
  lcPathName = SPACE(40)
  SHOW GET pbProceed DISABLE
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
SET DEFAUL TO (lcOldPath)
DIMENSION laSFile[ALEN(laFiles,1),1]
FOR lnCount = 1 TO ALEN(laFiles,1)
  laSFile[lnCount,1] = laFiles[lnCount,1]
ENDFOR
=gfMover(@laSFile,@laTFile,"Select files to upload",.T.)
IF EMPTY(laTFile)
  SHOW GET pbProceed DISABLE
  RETURN
ENDIF
SHOW GET pbProceed ENABLE
_CUROBJ = OBJNUM(pbProceed)

*****************************************************************************
*: Program file  : lfvLock.PRG  (C# 101624)
*: Program desc. : Valid Function For Lock Button.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvLock()
*****************************************************************************
*:
FUNCTION lfvLock

=gfOpenFile(gcdatadir+'STYINVJL','','SH')
=gfOpenFile(gcdatadir+'WAREHOUS','WAREHOUS','SH')

DIMENSION laSWare(1)
STORE ' ' TO laSWare
STORE 0 TO lcSesNo
SELECT WAREHOUS
SELECT cWareCode FROM WAREHOUS INTO ARRAY laSWare
=gfMover(@laSWare,@laTWare,"Select warehouses",.T.)
IF EMPTY(laTWare)
  RETURN 
ENDIF
*---"Are you sure you want to proceed with the locking? "
IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Are you sure you want to proceed with the locking?') = 2

  RETURN
ENDIF
IF EMPTY(lcStrDiv)
  SELECT DISTINCT cCode_No FROM CODES ;
         WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
             = "N"+'CDIVISION ' AND CrltField = 'N' INTO ARRAY laDiv

ENDIF

IF EMPTY(lcStrGrp)
  SELECT DISTINCT cCode_No FROM CODES ;
         WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
              ="N" + 'CSTYGROUP ' AND CrltField = 'N' INTO ARRAY laGrp
ENDIF

SELECT STYINVJL
GO BOTT
lcSesNo = cSession
SELECT MULLOCK
FOR lnWhCnt = 1 TO ALEN(laTWare)
  FOR lnDivCnt = 1 TO ALEN(laDiv)
    IF EMPTY(laDiv(lnDivCnt))
      LOOP
    ENDIF
    FOR lnGrpCnt = 1 TO ALEN(laGrp)
      IF EMPTY(laGrp(lnGrpCnt))
        LOOP
      ENDIF
      WAIT WINDOW "Locking warehouse : " + ALLT(laTWare(lnWhCnt)) + "/Division : "  +;
                  ALLT(laDiv(lnDivCnt))+ "/Group " + ALLT(laGrp(lnGrpCnt))   NOWAIT
      IF SEEK(PADR(laTWare(lnWhCnt),6)+laDiv(lnDivCnt)+laGrp(lnGrpCnt),"MULLOCK")
        *---"Warehouse " + laTWare(lnWhCnt) + "/Division : " laDiv(lnDivCnt)+ "/Group " + laGrp(lnGrpCnt)+ " has already been locked. "
      IF gfModalGen("QRM00000B42002","DIALOG",.F.,.F.,'Warehouse/Division/Group : '+  laTWare(lnWhCnt)+'/'+laDiv(lnDivCnt)+'/'+laGrp(lnGrpCnt) +' has already been locked. Lock again ? ' ) = 2
          LOOP
        ENDIF
        REPLACE cSession WITH lcSesNo 
      ELSE
        APPEND BLANK
        REPLACE cWareCode WITH laTWare(lnWhCnt),;
                cdivision WITH ALLT(laDiv(lnDivCnt)),;
                cStyGroup WITH ALLT(laGrp(lnGrpCnt)),;
                cSession WITH lcSesNo
      ENDIF
    ENDFOR
  ENDFOR
ENDFOR
*: B603831 08/22/2000 MHM [Begin]
*B603873 AMH [Start] Commented the following lines and call lfFlagLock funtion
*B603873     in order to lock the correct record for transaction in StyInvJl 
*SELECT MULLOCK
*SCAN 
*  SELECT STYINVJL
*  GO TOP
*  SCAN FOR  MULLOCK.cSession = cSession AND MULLOCK.cWareCode = cWareCode
*  REPLACE llockflg WITH  .T.
*  ENDSCAN
*ENDSCAN
=lfFlagLock(.T.)
* B603873 AMH [End]
*: B603831 08/22/2000 MHM [End]

IF !EMPTY(lcPathName)
  SHOW GET pbProceed ENABLE
ENDIF
WAIT CLEA

*****************************************************************************
*: Program file  : lfvFile.PRG  (C# 101624)
*: Program desc. : Valid Function To check the selected *.CSV File.
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvFile()
*****************************************************************************
*:
FUNCTION lfvFile

IF EMPTY(ldDate) 
  *---"Cannot upload file " +laTFile(lnCnt)+"No Posting Date is specified in cell A2." )
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ No Posting Date is specified in cell A2.])
  RETURN .F.
ENDIF
IF ldDate > gdSysdate
  *---('I',"Cannot upload file " + laTFile(lnCnt)+ ", " +;
                "the posting date is greater than the system date." )  
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ the posting date is greater than the system date.])
  RETURN .F.
ENDIF

IF !SEEK(lcWare,"WAREHOUS")		
  *---('I',"Cannot upload file " + laTFile(lnCnt)+;
                " Warehouse code " +lcWare +" is not found in the warehouse file.")
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ Warehouse code ]+lcWare+" is not found in the warehouse file.") 
  RETURN .F.
ENDIF
IF !SEEK(lcWare,"MULLOCK")
  *---('I',"Cannot upload file " + laTFile(lnCnt)+ ". " +;
                "No locking is done for warehouse " + lcWare)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Cannot upload file '+laTFile(lnCnt)+[ No locking is done for warehouse ]+ lcWare) 
  RETURN .F.
ENDIF

*****************************************************************************
*: Program file  : lfUpdInv.PRG  (C# 101624)
*: Program desc. : Function To Update inventory.
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : Style,Warcode
*****************************************************************************
*: Example       :  =lfUpdInv()
*****************************************************************************
*:
FUNCTION lfUpdInv
PARAMETERS lcSty,lcWar

STORE 0 TO laOldStk

**Calculate the old stock as of the locking date

SELECT STYINVJL
=SEEK(lcSty+lcWar)

*B607127,1 KHM 04/10/2003 (Begin) Removing the checking of session.
*SUM REST NSTK1,NSTK2,NSTK3,NSTK4,NSTK5,NSTK6,NSTK7,NSTK8,NTOTSTK;
         TO ARRAY laOldStk ;
         WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE = lcSty+lcWar;
         FOR Csession <= MULLOCK.cSession
SUM REST NSTK1,NSTK2,NSTK3,NSTK4,NSTK5,NSTK6,NSTK7,NSTK8,NTOTSTK;
         TO ARRAY laOldStk ;
         WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE = lcSty+lcWar
*B607127,1 KHM 04/10/2003 (End)

*B607127,1 KHM 04/10/2003 (Begin) Getting the correct cost according to the costing method
lnCost =  IIF(lcCostMthd='S',Style.TotCost,IIF(lcMullWH = 'Y',StyDye.Ave_Cost,Style.Ave_Cost))
*laOldStk[10] = laOldStk[9]*STYLE.TotCost
laOldStk[10] = laOldStk[9] * lnCost
*B607127,1 KHM 04/10/2003 (End)

*: B603831 08/22/2000 MHM [Begin]
laOldStk[11] = dTrDate
*: B603831 08/22/2000 MHM [End]

SELECT InvtAdj
APPEND BLANK

*B607127,1 KHM 04/10/2003 (Begin) Using the correct cost according to the costing method
*REPLACE Style     WITH lcSty,;
        cReason   WITH "Physical Count",; 
        Date      WITH ldDate,;
        Type      WITH 'P',;
        cFromWare WITH lcWar ,; 
        Unt_Cost  WITH STYLE.TotCost,;
        Old_Cost  WITH STYLE.TotCost

*REPLACE Style     WITH lcSty           ,;
        cReason   WITH "Physical Count" ,; 
        Date      WITH ldDate           ,;
        Type      WITH 'P'              ,;
        cFromWare WITH lcWar            ,; 
        Unt_Cost  WITH lnCost           ,;
        Old_Cost  WITH lnCost
*B607127,1 KHM 04/10/2003 (End)

FOR lnCount=1 TO 8
  lcCount = STR(lnCount,1)
  REPLACE Adj&lcCount    WITH TmpInv.STK&lcCount,;
          OldQty&lcCount WITH laoldStk(lnCount)
ENDFOR

REPLACE TotAdj     WITH TmpInv.TotStk,;
        TotOld     WITH laOldStk(9) &&,;
        cAdd_Date  WITH gdSysdate,;
        cAdd_Time  WITH TIME(),;
        cAdd_User  WITH "ICMUL10"

IF llLinkGL
  lcLinkCode=IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF')
  REPLACE LINK_CODE WITH lcLinkCode,;
          GLFYEAR   WITH lcGLFYear,;
          GLPERIOD  WITH lcGLPeriod
ENDIF

lcSession = gfSequence('GLSESSION')
*--G/L Array difinition and initialization.
IF llLinkGL
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldDate     TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = ''
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

DECLARE laAdjust[9]
SCATTER FIELDS TMPINV.Stk1,TMPINV.Stk2,TMPINV.Stk3,TMPINV.Stk4,;
               TMPINV.Stk5,TMPINV.Stk6,TMPINV.Stk7,TMPINV.Stk8,TMPINV.TOTSTK TO laAdjust
*---19
*B607127,1 KHM 04/10/2003 (Begin) Using the correct cost according to costing method.
*lnRet=gfStyCrl('9',lcSty,lcWar,'',ldDate,'',@laAdjust,STYLE.TOTCOST,;
               '',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
lnRet=gfStyCrl('9',lcSty,lcWar,'',ldDate,'',@laAdjust,lnCost,;
               '',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
*B607127,1 KHM 04/10/2003 (End)

*****************************************************************************
*: Program file  : lfUpdTGl.PRG  (C# 101624)
*: Program desc. : Function To Update GLDIST file.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfUpdTGl()
*****************************************************************************
*:
FUNCTION lfUpdTGl
*: B603831 08/22/2000 MHM [Begin]
*IF llLinkGL
*  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
*  lcLinkCode=IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDFE')
*  SELECT (lcGLDTemp)
*  *-- Generate a unique session number.
*  lcGlSess = gfSequence('GLSESSION')
*  REPLACE ALL GLSESSION WITH lcGlSess
*  USE
*  SELECT GLDIST
*  APPEND FROM &gcWorkDir.&lcGLDTemp
*  ERASE (gcWorkDir+lcGLDTemp+'.DBF')
*ENDIF
WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
lcLinkCode=IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDFE')
*-- Generate a unique session number.
lcGlSess = gfSequence('GLSESSION')
SELECT (lcGLDTemp)
REPLACE ALL GLSESSION WITH lcGlSess
SELECT GLDIST
APPEND FROM &gcWorkDir.&lcGLDTemp
SELECT (lcGLDTemp)
ZAP
*: B603831 08/22/2000 MHM [End]

*****************************************************************************
*: Program file  : lfvStyles.PRG  (C# 101624)
*: Program desc. : Function To Valid the style.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfvStyles()
*****************************************************************************
*:
FUNCTION lfvStyles

IF EOF("STYLE") OR !SEEK(STYLE+lcSepar+COLOR+lcWare,"STYDYE") OR ;
   IIF(!EMPTY(lcStrDiv),!(STYLE.cdivision $ lcStrDiv),.F.) OR;
   IIF(!EMPTY(lcStrGrp),!(STYLE.cStyGroup $ lcStrGrp),.F.) OR;
   !SEEK(lcWare+STYLE.cdivision+STYLE.cStyGroup,"MULLOCK")
  SELECT &lcRejSty
  APPEND BLANK
  REPLACE Style    WITH TMPINV.STYLE+lcSepar+TMPINV.COLOR,;
          WareCode WITH lcWare,;
          Reason   WITH lfUpdRes()
  SELECT TMPINV
  RETURN .F.
ENDIF

*****************************************************************************
*: Program file  : lfPrnRep.PRG  (C# 101624)
*: Program desc. : Function To Print the rejected styles.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       :  =lfPrnRep()
*****************************************************************************
*:
FUNCTION lfPrnRep

R_WIDTH = 'N'
lcRpForm = 'ICMUL10'

DO CASE
  CASE  lnanswer =  1
    OGPlatForm = 'WINDOW'
    lcOGPlatForm = OGPlatForm
    gcDevice  = "SCREEN"
    gcOutFile = gcWorkDir+gfTempName()+".TXT"      
    SET PRINTER TO &gcOutFile     && set printer to temprory file name
    SET DEVICE TO PRINTER
  CASE  lnanswer =  2
    OGPlatForm = 'DOS'
    gcDevice   = "PRINTER"
    lcOGPlatForm = OGPlatForm
    IF !pSetup(.T.)
      llCancel = .T.
      SET DEVICE TO SCREEN
      RETURN
    ENDIF
    SET DEVICE TO SCREEN
    SET PRINTER TO &gcOutFile
    SET DEVICE TO PRINTER
  CASE  lnanswer =  3
    llCancel = .T. 
    SET DEVICE TO SCREEN
    RETURN
ENDCASE
lcDefa = FULLPATH('')
SET DEFA TO (gcrepHome)
SELECT (lcRejSty)
GOTO TOP
DO gfDispRe WITH ('ICMUL10.FRX')
SET DEFA TO (lcDefa)
SET DEVICE TO SCREEN

*****************************************************************************
*: Program file  : lfUnLock.PRG  (C# 101624)
*: Program desc. : Function To UnLock Styles.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfUnLock()
*****************************************************************************
*:
FUNCTION lfUnLock

IF EMPTY(lcStrDiv)
  SELECT DISTINCT cCode_No FROM CODES ;
         WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
             = "N"+'CDIVISION ' AND CrltField = 'N' INTO ARRAY laDiv
ENDIF

IF EMPTY(lcStrGrp)
  SELECT DISTINCT cCode_No FROM CODES ;
         WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
              ="N" + 'CSTYGROUP ' AND CrltField = 'N' INTO ARRAY laGrp
ENDIF
*: B603831 08/22/2000 MHM [Begin]
*B603873 AMH [Start] Commented the following lines and call lfFlagLock funtion
*B603873     in order to unlock the correct record for transaction in StyInvJl 
*SELECT MULLOCK
*SCAN 
*  SELECT STYINVJL
*  GO TOP
*  SCAN FOR cSession =MULLOCK.cSession AND cWareCode = MULLOCK.cWareCode
*  REPLACE llockflg WITH  .F.
*  ENDSCAN
*ENDSCAN
=lfFlagLock(.F.)
* B603873 AMH [End]
*: B603831 08/22/2000 MHM [End]
SELECT MULLOCK
FOR lnWhCnt = 1 TO ALEN(laTFile)
  FOR lnDivCnt = 1 TO ALEN(laDiv)
    IF EMPTY(laDiv(lnDivCnt))
      LOOP
    ENDIF
    FOR lnGrpCnt = 1 TO ALEN(laGrp)
      IF EMPTY(laGrp(lnGrpCnt))
        LOOP
      ENDIF
      IF SEEK(lcWare+laDiv(lnDivCnt)+laGrp(lnGrpCnt),"MULLOCK")
        DELETE
      ENDIF
    ENDFOR
  ENDFOR
ENDFOR
PACK

*****************************************************************************
*: Program file  : lfUpdRes.PRG  (C# 101624)
*: Program desc. : Function To Update reason of rejection.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfUpdRes()
*****************************************************************************
*:
FUNCTION lfUpdRes

DO CASE
  CASE EOF("STYLE")
    RETURN "The  "+lcStyPic+"  is not found in the style file"
  CASE !SEEK(STYLE+lcWare,"STYDYE")
    RETURN "The  "+lcStyPic+"  is not found in warehouse " + lcWare
  CASE IIF(!EMPTY(lcStrDiv),!(STYLE.cdivision $ lcStrDiv),.F.)
    RETURN "The  "+lcStyPic+"  doesn't match with the selection criteria"
  CASE IIF(!EMPTY(lcStrGrp),!(STYLE.cStyGroup $ lcStrGrp),.F.)
    RETURN "The  "+lcStyPic+"  doesn't match with the selection criteria"
  OTHERWISE
    RETURN "No locking has been done"
ENDCASE

*****************************************************************************
*: Program file  : lfFiilDiv.PRG  (C# 101624)
*: Program desc. : Function To Fill the division array.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfFiilDiv()
*****************************************************************************
*:
FUNCTION lfFiilDiv

DIME laDivS[1,1]
DIME laDivT[1,1]
STORE '' TO laDivT,laDiv , laDivS
SELECT DISTINCT cCode_No+' - '+cDiscRep FROM CODES ;
       WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
           = "N"+'CDIVISION ' AND CrltField = 'N' INTO ARRAY laDivS

*****************************************************************************
*: Program file  : lfvDiv.PRG  (C# 101624)
*: Program desc. : Valid Function for Division Check Box.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfvDiv()
*****************************************************************************
*:
FUNCTION lfvDiv

=gfMover(@laDivS,@laDivT,'Division Code',.T.,'')
FOR lnInd = 1 TO ALEN(laDivT)
  DIME laDiv[lnInd]
  laDiv[lnInd] = SUBSTR(laDivT[lnInd],1,6)
ENDFOR
IF EMPTY(laDiv[1])
  lnDiv = 0
ELSE
  FOR lnIndex = 1 TO ALEN(laDiv)
    lcStrDiv = laDiv[lnIndex]+' | ' + lcStrDiv
  ENDFOR
  lnDiv = 1
ENDIF
SHOW GET lnDiv


*****************************************************************************
*: Program file  : lfFiilGrp.PRG  (C# 101624)
*: Program desc. : Function To Fill the style Group Array.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfFiilGrp()
*****************************************************************************
*:
FUNCTION lfFiilGrp

DIME laGrpS[1,1]
DIME laGrpT[1,1]
STORE '' TO laGrp , laGrpT , laGrpS
SELECT DISTINCT cCode_No+' - '+cDiscRep FROM CODES ;
       WHERE cdefcode+cfld_name+cCode_No+cdiscrep+crltd_nam;
            ="N" + 'CSTYGROUP ' AND CrltField = 'N' INTO ARRAY laGrpS

*****************************************************************************
*: Program file  : lfvGrp.PRG  (C# 101624)
*: Program desc. : Valid Function For style Group.
*:         System: Aria Apparel System (A2.7)
*:      Developer: AHMED SALAH SHALABY - (SSH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : None.
*****************************************************************************
*: Example       : =lfvGrp()
*****************************************************************************
*:
FUNCTION lfvGrp

=gfMover(@laGrpS,@laGrpT,'Style Group',.T.,'')
FOR lnInd = 1 TO ALEN(laGrpT)
  DIME laGrp[lnInd]
  laGrp[lnInd] = SUBSTR(laGrpT[lnInd],1,6)
ENDFOR
IF EMPTY(laGrp[1])
  lnGrp = 0
ELSE
  FOR lnIndex = 1 TO ALEN(laGrp)
    lcStrGrp = laGrp[lnIndex]+' | ' + lcStrGrp
  ENDFOR
  lnGrp = 1
ENDIF
SHOW GET lnGrp

*****************************************************************************
*: Program file  : lfFlagLock  (B# 603873)
*: Program desc. : Function to fage up lo
*:        System : Aria Apparel System (A2.7)
*:     Developer : AHMED MAHER - (AMH)
*****************************************************************************
*: Calls         : None
*****************************************************************************
*: Passed Param. : llFlag
*****************************************************************************
*: Example       : lfFlagLock()
*****************************************************************************
*:
FUNCTION lfFlagLock
PARAMETER llFlag

CREATE CURSOR TmpInv (STYLE C(12),COLOR C(06),DESC C(20), STK1 N(6), STK2 N(6),;
                      STK3 N(6), STK4 N(6), STK5 N(6), STK6 N(6), STK7 N(6), STK8 N(6))

FOR lncnt = 1 TO ALEN(laTFile)
  SELECT TmpInv
  ZAP
  APPEND FROM (ALLT(lcpathname)+IIF(RIGHT(lcPathName,1) <> "\","\","")+;
               laTFile(lnCnt)) DELIMITED
  GO 2
  ldDate = CTOD(STYLE)
  SKIP
  lcWare = PADR(ALLT(STYLE),6)
  SCAN FOR RECNO() >=5 AND !EMPTY(STYLE)
    SELECT STYINVJL
    lcOldOrder = SET('ORDER')
    SET ORDER TO TAG STYINVJL
    SEEK (STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE)
    SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE = TmpInv.STYLE+lcSepar+TmpInv.COLOR+lcWare ;
         FOR Csession <= MULLOCK.cSession
      REPLACE llockflg WITH llFlag
    ENDSCAN
    SET ORDER TO &lcOldOrder
  ENDSCAN
ENDFOR