*!************************************************************************
*!
*!    Program : APREPORT.PRG
*!
*!
*!************************************************************************
*E300296,1 12/18/95 M.H Add the multi currency to the AP reports.
*B601013,1 RENEE 04/10/96 If the company is not linked to a G/L, get the
*B601013,1                exchange difference account from the company setup
*B601013,1                (SYCSETUP) instead of the APSETUP file.
*B800556,1 Reham On 04/11/96
*B800556,1 Remark this line "llApGlLink = .F." because we already get 
*B800556,1 the variable value from the APsetup file.
*E300406,1 Hesham El-Sheltawi 07/09/96
*E300406,1 change the functions that create the temprory frx to use and
*E300406,1 external frx beside any frx inside the project itself
*B601173,1 Hesham El-Sheltawi On 07/22/96
*B601173,1 in the vendor code validation function make the softseek work
*B601504,1 HS 12/22/1996 Change the Preod Range from [Preod-Year] to [Year-Preod
*B601504,1               [Year-Preod] to correct the selection criteria 
*B601526,1 HS 01/22/1997 Fix the function gfAmntDisp 
*B601555,1  HS  03/30/97  Use the long describtion of the currency in the
*B601555,1                amount printed in string instaed of the word dollar
*E300683,7 Hesham El-Sheltawi 06/15/97
*E300683,7 Make the report programs run from inside the module id dir. under
*E300683,7 reports direct
*E300693,1 Hesham El-Sheltawi 06/22/97
*E300693,1 Make all the module reports can be run directly from inside any
*E300693,1 program in the system through the toolbar
*B602079,1 MAN   09/13/98 Fix Running shared reports from their module directory
*B602486,1 AMM 01/25/99 Replace the word 'AND' by the sign '&' when saying 
*B602486,1 AMM          the number by word in lfSayNumber()
*E300789,4  AMM 03/07/99 Remove field cComp_id from files Accod, FisHD, FsPrd, FsHld, Codes
*E301077,108 HDM 03/14/1999 Close SydReprt After program exit
*E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
*E301214,1 MAB 05/05/99 Transfer gfAmntDisp from APREPORT to be a global function
*B602924,1 MAN 05/24/99 Fix the bug of leaving temporary files when close the reports
*B803135,1 SSE 03/23/2000 Fix the Bug of Displaying the Currency Sign Before 
*B803135,1                the Decimals e.g. SEVENTY TWO DOLLARS & 56/100 became
*B803135,1                SEVENTY TWO & 56/100 DOLLARS
*B606024,1 ALB 09/02/2002 Fix the text from lfsaynumber() appear in check frx for example
*B606024,1                'One hundred & US$' -->'One hundred US$' 
*E038033,1 HMA 04/03/2004 Select only records belongs to A27(to split 
*E038033,1                A27 records of SYDREPRT File from A4XP records).
*E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Task:T20081225.0020]
*!***********************************************************************************
*E300693,1 Hesham El-Sheltawi (Start)
*PARAMETERS lcRpPrg,llDoReport
PARAMETERS lcRpPrg,llDoReport,lcRepDir
*E300693,1 Hesham El-Sheltawi (End)

*--HDM E301077,108 private flag[Begin]
PRIVATE llRepFile
llRepFile = .F.
*--HDM E301077,108 [End]


llDoReport = IIF(PARAM() = 1,.F.,llDoReport)

llOpenBy   = .F.

STORE 0   TO lnCurr_yer,lnCurr_prd
STORE {}  TO ldFisBgDat,ldFisEnDat,ldPyBgDate,ldNyEnDate
*--MAN Added lcOpnByAPR
PRIVATE lcOpnByAPR
lcOpnByAPR = ""
IF !USED("SYCCOMP")
  lcOpnByAPR = lcOpnByAPR + PADR("SYCCOMP",8)
  SELECT 0
  USE (gcSyshome+"SYCCOMP")
ENDIF

*HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]

*IF !USED("SYCFISHD")
*  SELECT 0
*  USE (gcSyshome+"SYCFISHD")
*ENDIF
*
*SELECT SYCFISHD
IF !USED("FISHD")
  lcOpnByAPR = lcOpnByAPR + PADR("FISHD",8) 
  SELECT 0
  USE (gcDataDir + "FISHD")
ENDIF

SELECT FISHD

*HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]

SET ORDER TO COMPFYEAR

*** In case the user uninstall the G/L from the current company 
*** we have to conferm that the link to the G/L is disconnected
SELECT SYCCOMP
SET ORDER TO CCOMP_ID

llGlLink  = .F.

IF gcComp_Lvl = "C"
  IF SEEK(gcPrnt_Cmp,"SYCCOMP")
    llGlLink = IIF("GL" $ SYCCOMP.mComp_Mdl,.T.,.F.)
  ENDIF
ELSE
  llGlLink   = IIF("GL" $ gcComp_Mdl,.T.,.F.)
ENDIF

lcCurr_yer = IIF(SEEK(gcPrnt_Cmp,'sycComp'),sycComp.cCurr_Yer,'')
lnCurr_yer = INT(VAL(lcCurr_yer))

lcCurr_prd = sycComp.cCurr_prd
lnCurr_prd = INT(VAL(sycComp.cCurr_prd))

*HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [Begin]

*SELECT SYCFISHD
*
*ldFisBgDat = IIF(SEEK(gcPrnt_Cmp+lcCurr_yer,'sycFisHd'),sycFisHd.dFisBgDat,{})
*ldFisEnDat = sycFisHd.dFisEnDat
*SKIP -1
*ldPyBgDate = sycFisHd.dfisbgdat
*SKIP 2
*ldNyEnDate = sycFisHd.dFisEnDat
SELECT FISHD
*E300789,4  AMM Adjust to fit the new index
*ldFisBgDat = IIF(SEEK(gcPrnt_Cmp + lcCurr_yer , 'FISHD') ,;
                 FISHD.dFisBgDat , {})
ldFisBgDat = IIF(SEEK(lcCurr_yer , 'FISHD') ,;
                 FISHD.dFisBgDat , {})
*E300789,4  AMM end
ldFisEnDat = FISHD.dFisEnDat
SKIP -1
ldPyBgDate = FISHD.dfisbgdat
SKIP 2
ldNyEnDate = FISHD.dFisEnDat

*HAYTHAR Change these lines to Use FISHD instead of SYCFISHD [End]
*man
PRIVATE llASByApr
llASByApr = .F.
IF !USED("APSETUP")
  SELECT 0
  USE (gcDataDir+"APSETUP")
  llASByApr = .T.
ENDIF

SELECT APSETUP

*** This line is very important to refresh the station buffer with new 
*** information from the disk
GO TOP
IF !EOF()
  GO 1
  llApGlLink = (CAPSGLLINK = 'Y')
ENDIF  

*E300296,1 12/18/95 M.H Add the multi currency to the AP reports.
*E300296,1 Variable to hold if the company is using the multi currency or not.
llMultCurr = gfGetMemVar('LLMULCURR')
*E300296,1 End.
IF llDoReport
  lcRepAlias = SELECT()
  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT 

    *HDM E301077,108 [Begin]
    llRepFile = .T.
    *HDM E301077,108 [End]

  ELSE
    SELECT SYDREPRT 
  ENDIF
  SET ORDER TO TAG CREP_ID
  lcSetRpPrg = lcRpPrg
  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  = SEEK(PADR(lcRpPrg,8))
  LOCATE  WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))

  *E038033,1 HMA [END] 
  
  SELECT (lcRepAlias)
  
  *E300683,7 Hesham El-Sheltawi (Start)
  *E300683,7 check if the report is shared between modules or not
  *E300683,7 if shared then run it from the reports dir. else from
  *E300683,7 module dir under the reports dir
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *B602079,1 Check for the existance of the report in its module directory
  *B602079,1 if it does not exist in the active module's directory
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),gcRepHome+lcSetRpPrg,gcRepHome+gcAct_Appl+'\'+lcSetRpPrg)
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  IF (GCMULTIINST AND FILE(gcCRepHome+lcSetRpPrg+'.FXP')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP')) 

    lcSetRpPrg = IIF(FILE(gcCRepHome+lcSetRpPrg+'.FXP'),;
                   gcCRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcCRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))

  ELSE
  lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  
  ENDIF 

  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  *B602079,1 end
  SET FULL &lcFullSet
  *E300683,7 Hesham El-Sheltawi (End)
  DO &lcSetRpPrg
ELSE
*E300683,7 Hesham El-Sheltawi (Start)
*E300683,7 Make all the variables that is used by the modules main progs. private
  PRIVATE  lcBrFields,lcBaseFile,laField_H,laField_N,lcFile_Ttl,lcSetProcd,laScrMode
*E300683,7 Hesham El-Sheltawi (End)

  *B602924,1 MAN 05/24/99 Declare an array to hold the temporary files to delete them whem 
  *B602924,1 MAN 05/24/99 close the reports
  *MAN Added laPrgTemps
  DECLARE laPrgTemps[1]
  *B602924,1 MAN [END]
   
  DECLARE laField_H[1,1],laField_N[1,1],laOpenFile[10,3],laScrMode[5]
*E300683,7 Hesham El-Sheltawi (Start)  
  STORE .F. TO laScrMode
*E300683,7 Hesham El-Sheltawi (End)  
  STORE " " TO lcRpForm ,lcRpFiles,lcRpExpr,lcRpTargt,lcRpOrder,lcRpFields,lcRpExp
  STORE " " TO lcBrFields,lcBaseFile,laField_H,laField_N,lcFile_Ttl,lcSetProcd

  *B800556,1 Reham On 04/11/96    *** Begin ***
  *B800556,1 Remark this line because we already get the variable value
  *B800556,1 from the APsetup file.
  *llApGlLink = .F.
  *B800556,1 Reham On 04/11/96    *** End   ***
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  
  *IF USED('SYCACCOD')
  *  SELECT SYCACCOD
  *ELSE
  *  SELECT 0 
  *  USE (gcSysHome+"SYCACCOD")
  *ENDIF  
  IF USED('ACCOD')
    SELECT ACCOD
  ELSE
    lcOpnByAPR = lcOpnByAPR + PADR("ACCOD",8)   
    SELECT 0 
    USE (gcDataDir + "ACCOD")
  ENDIF  
  
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  *E300789,4  AMM Adjust to fit the new structure
  *SET ORDER TO TAG COMPID 
  *=SEEK(gcAct_Comp) 
  GO TOP
  *E300789,4  AMM end
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [Begin]
  *lcApsAcMas = SYCACCOD.cAcsMask
  lcApsAcMas = ACCOD.cAcsMask
  *HAYTHAR Change these lines to Use ACCOD instead of SYCACCOD [End]
  
  lcApsAcMas = STRTRAN(lcApsAcMas,'#',IIF(APSETUP.cApsgllink='Y','9','X'))
  lcApsAcMas = ALLTRIM("X"+SUBSTR(lcApsAcMas,2))
  lnApsAcLen = LEN(ALLTRIM(lcApsAcMas))

  llUsd_GLSTP = .F.
  IF llGlLink
    IF !USED("GLSETUP")
      llUsd_GLSTP = .T.
      SELECT 0
      USE (gcDataDir+"GLSETUP")
    ENDIF
    SELECT GLSETUP
    GO TOP
  ENDIF
  
  
  *B601013,1 If the company is not linked to a G/L, get the
  *B601013,1 exchange difference account from the company setup
  *B601013,1 (SYCSETUP) instead of the APSETUP file.
  *lcExDifAcc = IIF(llApGlLink,GLSETUP.CSETEXMJ,APSETUP.CEXDIFACC)
  *lcExDifAcc = ALLTRIM(lcExDifAcc)+STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"9","0")
  lcExDifAcc  = IIF(llApGlLink, ALLTRIM(GLSETUP.CSETEXMJ) + ;
                              STRTRAN(SUBSTR(lcApsAcMas,AT("-",lcApsAcMas)),"9","0"),;
                              gfGetMemVar('LNEXRATACC', gcAct_Comp))
  *B601013,1 end.
  
  *E301170,1 MAB 03/15/1999 llOGFltCh Defined in OG program.
  *llOGFltCh  = .T.

  llOpenChar = lfOpenCh()

  IF !USED('SYDREPRT')
    SELECT 0
    USE &gcSysHome.SYDREPRT

    *HDM E301077,108 [Begin]
    llRepFile = .T.
    *HDM E301077,108 [End]
    
  ELSE
    SELECT SYDREPRT 
  ENDIF  
  SET ORDER TO TAG CREP_ID  
  lcSetRpPrg = lcRpPrg
  *E038033,1 HMA 04/03/2004 Select only Records belong to ARIA27 [BEGIN]
  
  *IF SEEK(PADR(lcRpPrg,8))
  *  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  *ENDIF
  = SEEK(PADR(lcRpPrg,8))
  LOCATE WHILE cRep_id= PADR(lcRpPrg,8) FOR cVer<>"A40"
  lcSetRpPrg=ALLTRIM(IIF(!EMPTY(CMAINREPID),PADR(ALLTRIM(CMAINREPID),8),PADR(ALLTRIM(lcRpPrg),8)))
  
  *E038033,1 HMA [END] 

  
  *E300683,7 Hesham El-Sheltawi (Start)
  *E300683,7 check if the report is shared between modules or not
  *E300683,7 if shared then run it from the reports dir. else from
  *E300683,7 module dir under the reports dir  
  lcFullSet = SET('FULLPATH')
  SET FULLPATH ON
  *B602079,1 Check for the existance of the report in its module directory
  *B602079,1 if it does not exist in the active module's directory
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),gcRepHome+lcSetRpPrg,gcRepHome+gcAct_Appl+'\'+lcSetRpPrg)
  
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [Start]
  *lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  IF (GCMULTIINST AND FILE(gcCRepHome+lcSetRpPrg+'.FXP')) OR ;
     (GCMULTIINST AND FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP')) 

    lcSetRpPrg = IIF(FILE(gcCRepHome+lcSetRpPrg+'.FXP'),;
                   gcCRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcCRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcCRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))

  ELSE
  lcSetRpPrg = IIF(FILE(gcRepHome+lcSetRpPrg+'.FXP'),;
                   gcRepHome+lcSetRpPrg,;
  				   IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcSetRpPrg+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcSetRpPrg,;
  				            gcRepHome+LEFT(lcSetRpPrg,2)+'\'+lcSetRpPrg))
  
  ENDIF 
  *E302566,1 MMT 01/06/2009 Modify SysFiles Paths for SAAS [End]
  
  *B602079,1 end
  SET FULL &lcFullSet
  *E300683,7 Hesham El-Sheltawi (End)
  
  lcSetProcd=SET('PROCEDURE')
  lcOldErr=ON('ERROR')
  ON ERROR llDumErr = .T.
  SET PROCEDURE TO (ALLTRIM(lcSetRpPrg))
  ON ERROR &lcOldErr
  *E300693,1 Hesham El-Sheltawi (Start)
*  =gfOpGrid(lcRpPrg,.f.)  
  =gfOpGrid(lcRpPrg,.f.,lcRepDir)

  *HDM E301077,108 [Begin]
  IF llRepFile
    USE IN SYDREPRT
  ENDIF
  *HDM E301077,108 [End]

  *E300693,1 Hesham El-Sheltawi (End)
  SET PROCEDURE TO (lcSetProcd)

  IF llOpenChar .AND. USED('lcLinkChar')  
    USE IN ('lcLinkChar')
  ENDIF
  
  *B602924,1 MAN 05/24/99 Delete the temporary files when closing the reports 
  *==MAN Clean temp Files
  
  IF TYPE('laPrgTemps')#'U'
    FOR lnCount = 1 TO ALEN(laPrgTemps,1)
      IF !EMPTY(laPrgTemps[lnCount,1])
        IF USED(laPrgTemps[lnCount,1])
          USE IN (laPrgTemps[lnCount,1])
        ENDIF
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.DBF')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.FPT')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.CDX')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.TXT')
        ERASE (gcWorkDir+laPrgTemps[lnCount,1]+'.TMP')
      ENDIF
    ENDFOR
  ENDIF
  *B602924,1 MAN [END]

ENDIF
*--MAN Close the opened files by ApReport
FOR lnFlNo = 1 TO INT(LEN(lcOpnByAPR)/8)
  lcToClose = ALLTRIM(SUBSTR(lcOpnByAPR,((lnFlNo-1)*8)+1,8))
  IF USED(lcToClose)
    USE IN (lcToClose)
  ENDIF
ENDFOR    

IF llASByApr AND USED("APSETUP")
  USE IN APSETUP
ENDIF

IF llUsd_GLSTP .AND. USED("GLSETUP")
  USE IN GLSETUP
ENDIF


*!*********************************************************************************
*!
*!             FUNCTION : lfRpThermo
*!
*!*********************************************************************************
*
FUNCTION lfRpThermo
PARAMETERS lnRpTotal,lnRpCount,lcMessage
PRIVATE lcOldEsc
lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,'Collecting Data')
&lnRpCount = &lnRpCount + 1
lcOldEsc=SET('ESCAPE')
SET ESCAPE OFF
IF &lnRpCount<=lnRpTotal
  =gfThermo(lnRpTotal,&lnRpCount,lcMessage,' ')
ENDIF  
SET ESCAPE &lcOldEsc


*!*********************************************************************************
*!
*!             FUNCTION : lfClosThrm
*!
*!*********************************************************************************
*
FUNCTION lfClosThrm

PARAMETERS lnRpTotal,lnRpCount,lcMessage,lnStep

lnStep=IIF(TYPE('lnStep')='N',lnStep,1)
lnStep=IIF(lnStep<1,1,lnStep)
lcMessage=IIF(TYPE('lcMessage')='C',lcMessage,'Collecting Data')
FOR lnCounter  = lnRpCount TO lnRpTotal STEP lnStep
  =gfThermo(lnRpTotal,lnCounter,lcMessage,' ')     
ENDFOR
IF WEXIST("gwdThermo")
    RELEASE WINDOW gwdThermo
ENDIF


*!************************************************************************
*!
*!      FUNCTION lfWaitMsg
*!
*!************************************************************************
FUNCTION lfWaitMsg

WAIT "Collecting data... " WINDOW NOWAIT



*!**************************************************************************
*!
*!      Function: lfwOldVal
*!
*!**************************************************************************
* 
FUNCTION lfwOldVal

lcRpVar  = SYS(18)
lcOldVal = &lcRpVar


*!**************************************************************************
*!
*!      Function: lfClearRep
*!
*!**************************************************************************
* 
FUNCTION lfClearRep


*!**************************************************************************
*!
*!      Function: lfGetVend
*!
*!**************************************************************************
* 
FUNCTION lfGetVend

DECLARE laRpRetFld(1)
lcBrFields    = 'Cvendcode:H="Vendor code",Cvencomp:H="Company"'
laRpRetFld[1] = ''
lcRpCurFld    = SYS(18)
lcRpArea      = ALIAS()
*--MAN
llApVenUsd = .F.
IF !USED("APVENDOR")
  llApVenUsd = .T.
  SELECT 0
  USE (gcDataDir + "APVENDOR")
ENDIF
*--MAN
SELECT APVENDOR
SET ORDER TO TAG VENCODE
IF !EMPTY(&lcRpCurFld) .AND. !SEEK(&lcRpCurFld)
  *B601173,1 Hesham El-Sheltawi (Start)
  *B601173,1 if there is  any near matching records
  *B601173,1 then go to the nearest record
  IF !EMPTY(&lcRpCurFld) AND RECNO(0)>0
    GO RECNO(0)
  ENDIF  
  *B601173,1 Hesham El-Sheltawi (End)  
  =gfBrows(.F.,'Cvendcode',"laRpRetFld",'Vendors',.F.)
  &lcRpCurFld = laRpRetFld[1]
  SHOW GET (lcRpCurFld)
ENDIF
*--MAN
IF llApVenUsd AND USED("APVENDOR")
  USE IN  APVENDOR
ENDIF 
*--MAN 
SELECT (lcRpArea)

*!**************************************************************************
*!
*!      Function: lfvYearPrd
*!
*!**************************************************************************
* 
FUNCTION lfvYearPrd

lcRpArea  = ALIAS()
lcRpVar   = SYS(18)
&lcRpVar  = ALLTRIM(&lcRpVar)

*HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]

*IF !USED("SYCFSPRD")
*  SELECT 0
*  USE (gcSysHome+"SYCFSPRD")  
*ENDIF
*SELECT SYCFSPRD
*MAN Flag to determine if the FSPRD is opened by the function
PRIVATE llPrdIsUsed
llPrdIsUsed = USED("FSPRD")
IF !USED("FSPRD")
  SELECT 0
  USE (gcDataDir + "FSPRD")  
ENDIF
SELECT FSPRD

*HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]

*B601504,1 This line was added by HS for safety [Begin]
lcSavOrd = SET('ORDER')   && Varible to save the old Order
*B601504,1 This line was added by HS for safety [End]

SET ORDER TO TAG COMFYRPRDI 

*B601504,1 Change this line [Begin]
*IF !EMPTY(STRTRAN(&lcRpVar,'-')) AND !SEEK(gcPrnt_Cmp+RIGHT(&lcRpVar,4)+LEFT(&lcRpVar,2),'SYCFSPRD')
*IF Statment to check if we are going to browse

*HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [Begin]
*IF (AT('?' , &lcRpVar) <> 0) .OR. (!EMPTY(STRTRAN(&lcRpVar,'-')) AND !SEEK(gcPrnt_Cmp+LEFT(&lcRpVar,4)+RIGHT(&lcRpVar,2),'SYCFSPRD'))
*E300789,4  AMM Adjust to fit the new index
*IF (AT('?' , &lcRpVar) <> 0) .OR. (!EMPTY(STRTRAN(&lcRpVar , '-')) AND;
   !SEEK(gcPrnt_Cmp + LEFT(&lcRpVar,4) + RIGHT(&lcRpVar , 2) , 'FSPRD'))
IF (AT('?' , &lcRpVar) <> 0) .OR. (!EMPTY(STRTRAN(&lcRpVar , '-')) AND;
   !SEEK(LEFT(&lcRpVar,4) + RIGHT(&lcRpVar , 2) , 'FSPRD'))
*E300789,4  AMM end

*HAYTHAR Change these lines to Use FSPRD instead of SYCFSPRD [End]

*B601504,1 Change this line [End]

  DECLARE laRpRetFld(2)

  *B601504,1 This line was added by HS for safety [Begin]
  lcSavFilds = lcBrFields      && Varible to save the old browse fields
  *B601504,1 This line was added by HS for safety [End]

  *B601504,1 Change this line [Begin]
  *lcBrFields    = [FSPPRDID=CFSPPRDID+'-'+CFISFYEAR:H="Period-Year",DFSPPBGDT:H="Begining date",dfsppendt:H="Ending date"]
  lcBrFields    = [FSPPRDID=CFISFYEAR+'-'+CFSPPRDID:H="Year-Period",DFSPPBGDT:H="Begining date",dfsppendt:H="Ending date"]
  *B601504,1 Change this line [End]

  STORE '' TO laRpRetFld[2]
  *MAN
  *B601504,1 Change this line [Begin]
  *=gfBrows('"'+gcPrnt_Cmp+'"','CFSPPRDID,CFISFYEAR',"laRpRetFld",'Fiscal Periods',.F.)
  *=gfBrows('"'+gcPrnt_Cmp+'"','CFISFYEAR,CFSPPRDID',"laRpRetFld",'Fiscal Periods',.F.)
  =gfBrows("",'CFISFYEAR,CFSPPRDID',"laRpRetFld",'Fiscal Periods',.F.)
  *B601504,1 Change this line [End]
  
  IF !EMPTY(laRpRetFld[1])
    &lcRpVar = laRpRetFld[1]+'-'+laRpRetFld[2]
  ELSE  
    &lcRpVar=lcOldVal
  ENDIF  

  *B601504,1 This line was added by HS for safety [Begin]
  lcBrFields = lcSavFilds    && Restore the old browse fields 
  *B601504,1 This line was added by HS for safety [End]

ENDIF

IF EMPTY(STRTRAN(&lcRpVar,'-'))
  &lcRpVar=''
ENDIF

*B601504,1 This lines was added by HS for safety [Begin]

*HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [Begin]
*SELECT SYCFSPRD
SELECT FSPRD
*HAYTHAR Change this line to Use FSPRD instead of SYCFSPRD [End]
*MAN Close the file
IF !llPrdIsUsed 
  USE
ELSE
  SET ORDER TO &lcSavOrd
ENDIF  
*B601504,1 This lines was added by HS for safety [End]

SELECT (lcRpArea)

*!************************************************************************
*!
*!      Function lfSayNumber
*!
*!************************************************************************
*
FUNCTION lfSayNumber
PARAMETERS lnSendNum,lcOtherLine
DIMENSION laWords(15)
laWords = SPACE(15)
DIMENSION laDigit(10)
laDigit[1]  = ' '
laDigit[2]  = 'One '
laDigit[3]  = 'Two '
laDigit[4]  = 'Three '
laDigit[5]  = 'Four '
laDigit[6]  = 'Five '
laDigit[7]  = 'Six '
laDigit[8]  = 'Seven '
laDigit[9]  = 'Eight '
laDigit[10] = 'Nine '

IF lnSendNum = 0

  *B601555,1 Change this line to use the currency describtion [Begin]
  *RETURN '* Zero Dollars *'
  RETURN '* Zero ' + lcRpCurDes + ' *'
  *B601555,1 Change this line to use the currency describtion [End]

ENDIF

lcSendNum = STR(lnSendNum,ALEN(laWords,1),2)

IF lnSendNum < 1

  *B601555,1 Change this line to use the currency describtion [Begin]
  *RETURN  '* Zero Dollars and '+ SUBSTR(lcSendNum,ALEN(laWords,1)-1,2) + '/100 *'

  *B803135,1 SSE 03/23/2000 Remove Currency Description to the end of Amount [Begin]
  *RETURN  '* Zero ' + lcRpCurDes + ' and '+ SUBSTR(lcSendNum,ALEN(laWords,1)-1,2) + '/100 *'
  RETURN  '* Zero ' + ' and '+ SUBSTR(lcSendNum,ALEN(laWords,1)-1,2) + '/100 ' + lcRpCurDes + ' *'
  *B803135,1 SSE 03/23/2000 [End]
  
  *B601555,1 Change this line to use the currency describtion [End]

ENDIF

lcReturnWord = ''

FOR lnCount = 1 TO ALEN(laWords,1)
  DO CASE

    CASE SUBSTR(lcSendNum,lnCount,1) = '.'
      laWords[lnCount] = '~~~ '

    CASE SUBSTR(lcSendNum,lnCount,1) = ' '
      laWords[lnCount] = ' '

    OTHERWISE
      lnNumber = VAL(SUBSTR(lcSendNum,lnCount,1))
      laWords[lnCount] = laDigit(lnNumber + 1)

  ENDCASE
ENDFOR
lnCount = 1

DO WHILE .T.
  IF lnCount >= ALEN(laWords,1) - 2
    EXIT
  ENDIF

  IF laWords[lnCount] > ' '
    lcReturnWord = lcReturnWord + laDigit(VAL(SUBSTR(lcSendNum,lnCount,1))+1) + 'Hundred ~~~ '
  ENDIF
  lnCount = lnCount + 1

  IF laWords[lnCount] > ' '
    IF laWords[lnCount] = laDigit[2]
      lnCount = lnCount + 1
      =lfTeens()
      =lfThou()
      lnCount = lnCount + 1
      LOOP
    ELSE
      =lfTens()
    ENDIF
  ENDIF

  lnCount = lnCount + 1
  IF laWords[lnCount] > ' '
    lcReturnWord = lcReturnWord + laDigit(VAL(SUBSTR(lcSendNum,lnCount,1))+1) 
  ENDIF
  
  =lfThou()
  lnCount = lnCount + 1
ENDDO

*B601555,1 Change this line to use the currency describtion [Begin]
*lcReturnWord = lcReturnWord + 'Dollars' 

*B803135,1 SSE 03/23/2000 Commented out this line to be at the end of Function 
*B803135,1                after adding the decimals [Begin]
*lcReturnWord = lcReturnWord + lcRpCurDes 
*B803135,1 SSE 03/23/2000 [End]

*B601555,1 Change this line to use the currency describtion [End]

IF VAL(SUBSTR(lcSendNum,14,2)) > 0
  *B606024,1 ALB [Begin]
  *lcReturnWord = STRTRAN(lcReturnWord, '~~~ ', '')
  lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '')
  *lcReturnWord = lcReturnWord + '~~~ ' + SUBSTR(lcSendNum,14,2) + '/100'
  lcReturnWord = lcReturnWord + '&' + SUBSTR(lcSendNum,14,2) + '/100 '+ '~~~ '
  *B606024,1 ALB [End]
ELSE
  *B606024,1 ALB [Begin]
  *DO WHILE ATC('~~~ ',lcReturnWord,2) <> 0
  *  lcReturnWord = STRTRAN(lcReturnWord, '~~~ ', '',1,1)
  *ENDDO  
  DO WHILE ATC('!!! ',lcReturnWord,2) <> 0
    lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '',1,1)
  ENDDO  
  lcReturnWord = STRTRAN(lcReturnWord, '!!! ', '&')
  *B606024,1 ALB [End]
ENDIF

*B606024,1 ALB [Begin]  
*B602486,1 AMM Replace the word 'AND' by the sign '&'
*lcReturnWord = STRTRAN(lcReturnWord, '~~~', 'AND')  
*lcReturnWord = STRTRAN(lcReturnWord, '~~~', '&')  
*B602486,1 AMM end

*B601555,1 Change this line to use the currency describtion [Begin]
*lcReturnWord = STRTRAN(lcReturnWord, 'AND Dollars', 'Dollars')  
*B602486,1 AMM Replace the word 'AND' by the sign '&'
*lcReturnWord = STRTRAN(lcReturnWord, 'AND ' + lcRpCurDes , lcRpCurDes)  
*lcReturnWord = STRTRAN(lcReturnWord, '& ' + lcRpCurDes , lcRpCurDes)
lcReturnWord = STRTRAN(lcReturnWord, '~~~ ' ,'') 
*B602486,1 AMM end
*B601555,1 Change this line to use the currency describtion [End]

*B803135,1 SSE 03/23/2000 Add Currency Description at the end of amount
lcReturnWord = lcReturnWord + "" + lcRpCurDes
*B803135,1 SSE 03/23/2000 [End]

lcReturnWord='* '+UPPER(SUBSTR(lcReturnWord,1,1))+LOWER(SUBSTR(lcReturnWord,2))+' *'

IF LEN(lcReturnWord) > 72
  lcTempStr    = SUBSTR(lcReturnWord,1,71)
  lnEndOfLine  = RAT(' ',lcTempStr)
  lcTempStr    = SUBSTR(lcTempStr,1,lnEndOfLine) + '*'
  &lcOtherLine  = '* ' + SUBSTR(lcReturnWord,lnEndOfLine+1)
  lcReturnWord = lcTempStr 
*MAN Added else condition  
ELSE  
  &lcOtherLine  = ''
ENDIF

RETURN lcReturnWord

*!***************************************************************
*!
*!   FUNCTION lfThou
*!
*!***************************************************************
*
FUNCTION lfThou

DO CASE

  CASE lnCount = 3
    IF laWords[1] > ' ' OR laWords[2] > ' ' OR laWords[3] > ' '
      *B606024,1 ALB [Begin]
      *lcReturnWord = lcReturnWord + 'Billion ~~~ '
      lcReturnWord = lcReturnWord + 'Billion !!! '
      *B606024,1 ALB [End]
    ENDIF

  CASE lnCount = 6
    IF laWords[4] > ' ' OR laWords[5] > ' ' OR laWords[6] > ' '
      *B606024,1 ALB [Begin]
      *lcReturnWord = lcReturnWord + 'Million ~~~ '
      lcReturnWord = lcReturnWord + 'Million !!! '
      *B606024,1 ALB [End]
    ENDIF

  CASE lnCount = 9
    IF laWords[7] > ' ' .OR. laWords[8] > ' ' .OR. laWords[9] > ' '
      *B606024,1 ALB [Begin]
      *lcReturnWord = lcReturnWord + 'Thousand ~~~ '
      lcReturnWord = lcReturnWord + 'Thousand !!! '
      *B606024,1 ALB [End]
    ENDIF

ENDCASE


*!***************************************************************
*!
*!   FUNCTION lfTeens
*!
*!***************************************************************
*
FUNCTION lfTeens

DO CASE
  CASE laWords[lnCount] == laDigit[1]
    lcReturnWord = lcReturnWord + 'Ten '
    
  CASE laWords[lnCount] = laDigit[2]
    lcReturnWord = lcReturnWord + 'Eleven '
    
  CASE laWords[lnCount] = laDigit[3]
    lcReturnWord = lcReturnWord + 'Twelve '
    
  CASE laWords[lnCount] = laDigit[4]
    lcReturnWord = lcReturnWord + 'Thirteen '
    
  CASE laWords[lnCount] = laDigit[5]
    lcReturnWord = lcReturnWord + 'Fourteen '
    
  CASE laWords[lnCount] = laDigit[6]
    lcReturnWord = lcReturnWord + 'Fifteen '
    
  CASE laWords[lnCount] = laDigit[7]
    lcReturnWord = lcReturnWord + 'Sixteen '
    
  CASE laWords[lnCount] = laDigit[8]
    lcReturnWord = lcReturnWord + 'Seventeen '
    
  CASE laWords[lnCount] = laDigit[9]
    lcReturnWord = lcReturnWord + 'Eighteen '
    
  CASE laWords[lnCount] = laDigit[10]
    lcReturnWord = lcReturnWord + 'Nineteen '

ENDCASE


*!***************************************************************
*!
*!   FUNCTION lfTens
*!
*!***************************************************************
*
FUNCTION lfTens

DO CASE

  CASE laWords[lnCount] == laDigit[1]
    RETURN

  CASE laWords[lnCount] = laDigit[2]
    lcReturnWord = lcReturnWord + 'Ten '
    
  CASE laWords[lnCount] = laDigit[3]
    lcReturnWord = lcReturnWord + 'Twenty '
    
  CASE laWords[lnCount] = laDigit[4]
    lcReturnWord = lcReturnWord + 'Thirty '
    
  CASE laWords[lnCount] = laDigit[5]
    lcReturnWord = lcReturnWord + 'Forty '
    
  CASE laWords[lnCount] = laDigit[6]
    lcReturnWord = lcReturnWord + 'Fifty '
    
  CASE laWords[lnCount] = laDigit[7]
    lcReturnWord = lcReturnWord + 'Sixty '
    
  CASE laWords[lnCount] = laDigit[8]
    lcReturnWord = lcReturnWord + 'Seventy '
    
  CASE laWords[lnCount] = laDigit[9]
    lcReturnWord = lcReturnWord + 'Eighty '
    
  CASE laWords[lnCount] = laDigit[10]
    lcReturnWord = lcReturnWord + 'Ninety '

ENDCASE


