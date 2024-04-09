*:************************************************************************
*:
*: Procedure file: SMACCOD.PRG 
*:                 (Account code structure)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*B601803,1 RENEE 06/26/97 Show company name in popup if called from SMCMINF
*E300692,4 Hesham El_Sheltawi 01/07/1997
*E300692,4 change the account code stru. file from system file to data file
*E301098,1 HESHAM 12/16/98 Get company data path using gfGetDataDir(..)
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*T20060831.0036 TMI (B607794) 10/03/2006 fix a bug that when copy strucutre from one co. to another no company list appears
*:************************************************************************
PARAMETERS pcComp_ID

EXTERNAL ARRAY laData

DECLARE laObjStat[6]  && Array to hold the display status of each seg.
DECLARE laObjdisp[6]  && Array to control the display of seg. Des. in Child.
DECLARE laAcDes[6,3]  && Array to hold the description of each seg.
laAcDes    = ' '        && Default blank to seg description array

*** Default value of all variables holding the seg. width. ***
STORE 0 TO lnSeg1,lnSeg2,lnSeg3,lnSeg4,lnSeg5,lnSeg6

*** Default value of all variables holding the old seg. width. ***
STORE 0 TO lnOldSeg1,lnOldSeg2,lnOldSeg3,lnOldSeg4,lnOldSeg5,lnOldSeg6,lnOldSNo

lnBoxh     = 16         && Max size of box in Child screen is 16
lnSetCostC = 0

laObjStat  = "DISABLE"  && Default status of all seg. is DISABLE
lcOldVal   = ''         && Hold the old value of any object in the screen
lcGlSetup  = " "        && Variable hold the temp alias for "GLSETUP"

llAcsUsed  = .F.
llCopyFlg  = .F.        && Flag to know copy or not.
laObjdisp  = .T.        && Default Display all lines in child screen 
llCancel   = .F.        && Flag to indicate if there is a canslation in
                        && the child screen
llChange   = .F.        && Check if any change in the data
                        && the platform is DOS
ibCompany  = 1
puComp     = " "
puComp_ID  = " "
lcCompany  = " "
lcComp_Id  = " "
lcOldComp  = " "
lcDataDir  = " "         && Hold the company setup dir.
llCompPrnt = .T.


llNoContrl = .T.
lnOldSize = ''          && Old total size
lcOldMask = ''          && Old Account Mask
lcOldDes  = ''          && Old Account segments desciption
lcOldNoSeg= ''          && Old No of segments


lcAccExct  = SET("EXACT")
SET EXACT OFF


IF !gfSetup()    
  RETURN
ENDIF  

*rENEE
IF !WEXIST(gcBaseWind)
  =gfOpenFile(GCSYSHOME+'SYCCOMP','CCOMP_ID')
ENDIF

IF EMPTY(pcComp_ID) .AND. !WEXIST(lcBaseWind)
  *** See if there is any companies available in the system. ***
  *RENEE
  *=gfOpenFile(GCSYSHOME+'SYCCOMP','CCOMP_ID')
  SELECT SYCCOMP
  GO TOP
  IF EOF()
    *** No companies available. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00189B00000","DIALOG","companies")
    glQuitting = .T.
    RETURN
  ENDIF
ENDIF


lcGlSetup = gfTempName()

IF _WINDOWS
  puComp = " "
  DEFINE POPUP puComp_Id prompt field ;
  SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME scroll;
  FROM 3.917+1.583,4.125 TO 9,35.125;
  MESSAGE gfObj_msg()
  ON SELECTION POPUP puComp_Id DO lfvComp
ENDIF

*** To know if the program called from the menu ***
*** or called from another program
IF !EMPTY(pcComp_ID)
  lcComp_ID  = pcComp_ID
  lcCompany  = lcComp_ID+" "+LOOKUP(SYCCOMP.cCom_name,lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
  llComIdFlg = .T.
  *B601803,1 Initialize popup name from SYCCOMP
  puComp     = lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
  *B601803,1 end
  lcCompStat = "DISABLE"
  lcData_4   = 'ENABLE'
  *E300692,4 Hesham El_Sheltawi (Start)
  *E300692,4 call the valid function of the selection of the company popup
  *E300692,4 to simulate that the user has select the company from the popup
  =lfvComp()
  *E300692,4 Hesham El_Sheltawi (End)
ELSE
  lcComp_Id  = " "
  lcCompany  = " "
  llComIdFlg = .F.
  lcCompStat = "ENABLE"
  lcData_4   = 'DISABLE'
ENDIF  

*E300692,4 Hesham El_Sheltawi (Start)
*SELECT SYCACCOD 

*** Resize the laData 
*SCATTER FIELDS &lcScFields TO laData BLANK

*** Store old valus in variables
*lnOldSize = laData[1]          && Old total size
*lcOldMask = laData[2]          && Old Account Mask
*lcOldDes  = laData[3]          && Old Account segments desciption
*lcOldNoSeg= laData[4]          && Old No of segments

*** Collect the no. and size of segments from the old mask
*lnNext =0

*FOR lnCount  = 1 TO 6
*  lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
*  lnCrurnt   = lnNext
*  lnNext     = IIF(AT('-',laData[2],lnCount)=0,LEN(ALLTRIM(laData[2]))+1,;
*                 AT('-',laData[2],lnCount))
* lnwidth    = MAX(0,lnNext-lnCrurnt-1) 
*  &lcObjName = lnwidth
*ENDFOR
*E300692,4 Hesham El_Sheltawi (End)

*E300683,4 Call *.SPR from screens directory
* DO SMaccod.SPR 
DO (gcScrDir + gcWinAppl + '\SMaccod.SPR')
*E300683,4 end   
SET EXACT &lcAccExct 

*** Becouse the screen has no controle screen This flag has to be set 
*** On exiting the screen 
glQuitting  = .T.  
*E300692,4 Hesham El_Sheltawi (Start)
IF USED('SYCACCOD')
  USE IN SYCACCOD
ENDIF
*E300692,4 Hesham El_Sheltawi (End)

RELEASE POPUP puComp_Id

*!**************************************************************************
*!
*!      Function: lfvComp
*!
*!**************************************************************************
*
FUNCTION lfvComp

DO CASE
  CASE _WINDOWS
    *** If any change in the data
    IF llChange
      *** Do you want to save the change. ***
      *** < Save > - < NO > - < Cancel > ****
      lnOption = gfModalGen("QRM00153B00025","DIALOG")
      DO CASE
        CASE lnOption = 1
          *** If no segments description while saving
          IF !lfSaveData()
            lcComp_Id = lcOldComp
            puComp    = lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
            SHOW GET puComp
          ELSE
            puComp     = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
            lcComp_Id  = SYCCOMP.CCOMP_ID
            *E301098,1 Hesham (Start)
            *lcDataDir  = ALLT(SYCCOMP.cCom_DDir)
            lcDataDir  = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
            *E301098,1 Hesham (End)
            llCompPrnt = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
            SHOW GET puComp
            llChange   = .F.
          ENDIF
        CASE lnOption = 2
          puComp      = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
          lcComp_Id   = SYCCOMP.CCOMP_ID
          *E301098,1 Hesham (Start)
          *lcDataDir   = ALLT(SYCCOMP.cCom_DDir)
          lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
          *E301098,1 Hesham (End)
          llCompPrnt  = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
          SHOW GET puComp
          llChange    = .F.
        CASE lnOption = 2
          lcComp_Id   = lcOldComp
          puComp      = lcComp_Id+" "+LOOKUP(SYCCOMP.cCom_name,lcComp_ID,SYCCOMP.ccomp_id,'ccomp_id')
          SHOW GET puComp
      ENDCASE
    ELSE
      puComp      = SYCCOMP.CCOMP_ID+" "+SYCCOMP.CCOM_NAME
      lcComp_Id   = SYCCOMP.CCOMP_ID
      *E301098,1 Hesham (Start)
      *lcDataDir   = ALLT(SYCCOMP.cCom_DDir)
      lcDataDir   = gfGetDataDir(ALLT(SYCCOMP.cCom_DDir))
      *E301098,1 Hesham (End)
      llCompPrnt  = IIF(EMPTY(SYCCOMP.CCOMPPRNT),.T.,.F.)
      SHOW GET puComp
    ENDIF
ENDCASE

IF FILE("GLSETUP")
  SELECT 0
  USE (lcDataDir+'GLSETUP') AGAIN ALIAS &lcGlSetup
  SELECT (lcGlSetup)
  GO TOP
  lnSetCostC = IIF(EMPTY(&lcGlSetup..nSetcostc),0,&lcGlSetup..nSetcostc)
  USE IN &lcGlSetup.
ELSE
  lnSetCostC = 0
ENDIF
*E300692,4 Hesham El_Sheltawi (Start)
*SELECT SYCACCOD
*IF !EMPTY(lcOldComp) 
IF !EMPTY(lcOldComp) AND USED('SYCACCOD')
  SELECT SYCACCOD
*E300692,4 Hesham El_Sheltawi (End)  
  *E300789,1 Hesham (Start)
  *IF SEEK(lcOldComp)
  IF SEEK("")
  *E300789,1 Hesham (End)
    =gfObj_Lock(.F.)
  ENDIF
ENDIF
*E300692,4 Hesham El_Sheltawi (Start)
*E300692,4 open the account code stru. file from the 
*E300692,4 company data dir.
=lfOpenCmpData()
*E300692,4 Hesham El_Sheltawi (End)
*** Get information stored in the first record
*E999999,1 Hesham (Start)
*IF SEEK(lcComp_Id)
IF SEEK("")
*E999999,1 Hesham (End)
  IF gfObj_Lock(.T.)
    SCATTER FIELDS &lcScFields TO laData 
    llAcsUsed = lAcsUsed
    
    *** Get Short and long description for all segment in array
    FOR lnCount = 1 TO laData[4]
      laAcDes [lnCount,1] = gfSubStr(laData[3],lnCount,'-')
      *E999999,1 Hesham (Start)
      *IF SYCACCOD.CCOMP_ID = lcComp_Id AND !EOF()
      IF !EOF()      
      *E999999,1 Hesham (End)
        SKIP
        laAcDes [lnCount,2] = cAcsLgDes
      ENDIF  
    ENDFOR
    
    *** Store old valus in variables
    lnOldSize = laData[1]          && Old total size
    lcOldMask = laData[2]          && Old Account Mask
    lcOldDes  = laData[3]          && Old Account segments desciption
    lcOldNoSeg= laData[4]          && Old No of segments
    
    *** Collect the no. and size of segments from the old mask
    lnNext = 0
    FOR lnCount  = 1 TO 6
      lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
      lnCrurnt   = lnNext
      lnNext     = IIF(AT('-',laData[2],lnCount)=0,LEN(ALLTRIM(laData[2]))+1,;
                       AT('-',laData[2],lnCount))
      lnwidth    = MAX(0,lnNext-lnCrurnt-1) 
      &lcObjName = lnwidth
    ENDFOR
    
    *** Check if this file is empty or not.  If it's empty, ***
    *** the user can copy from another company ***
    IF llAcsUsed
      llCopyFlg = .F.
      lcData_4  = 'DISABLE'
    ELSE
      llCopyFlg = .T.
      lcData_4  = 'ENABLE'
    ENDIF
  ELSE
    SCATTER FIELDS &lcScFields TO laData BLANK
    lcData_4     = 'ENABLE'
    llCopyFlg    = .T. 
    llAcsUsed    = .F.
    *** Store old valus in variables
    lnOldSize    = laData[1]          && Old total size
    lcOldMask    = laData[2]          && Old Account Mask
    lcOldDes     = laData[3]          && Old Account segments desciption
    lcOldNoSeg   = laData[4]          && Old No of segments
    FOR lnCount  = 1 TO 6
      lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
      &lcObjName = 0
    ENDFOR
  ENDIF
ELSE
  SCATTER FIELDS &lcScFields TO laData BLANK
  lcData_4 = 'ENABLE'
  llCopyFlg = .T. 
  llAcsUsed = .F.
  *** Store old valus in variables
  lnOldSize = laData[1]          && Old total size
  lcOldMask = laData[2]          && Old Account Mask
  lcOldDes  = laData[3]          && Old Account segments desciption
  lcOldNoSeg= laData[4]          && Old No of segments
  FOR lnCount  = 1 TO 6
    lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
    &lcObjName = 0
  ENDFOR
ENDIF

=lfvData_4()
=lfComShow()

IF _WINDOWS
  DEACTIVATE POPUP puComp_Id
ENDIF

*!**************************************************************************
*!
*!      Function: lfComShow
*!
*!**************************************************************************
*
FUNCTION lfComShow

IF llCompPrnt .OR. llAcsUsed
  SHOW GET laData[4]
  SHOW GET laData[4] &lcData_4.

  IF llCopyFlg
    SHOW GET pbCopy ENABLE
  ELSE
    SHOW GET pbCopy  DISABLE
  ENDIF

  SHOW GET pbDiscrip ENABLE
  
  FOR lnCount  = 1 TO 6
    lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
    SHOW GET (lcObjName) &lcData_4.
  ENDFOR

  IF !llChange
    SHOW GET pbSave DISABLE
  ELSE
    SHOW GET pbSave ENABLE
  ENDIF
ELSE
  SHOW GETS DISABLE ONLY
  DO CASE
    CASE _DOS
      SHOW GET ibCompany ENABLE
    CASE _WINDOWS
      SHOW GET puComp    ENABLE
      SHOW GET puComp_Id ENABLE
  ENDCASE
  SHOW GET pbClose   ENABLE
  SHOW GET pbDiscrip ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfwData_4
*!
*!**************************************************************************
* Valid of all segments :Calculat the new mask and description and show 
* the objects enable or disable
*
FUNCTION lfwData_4

lnOldSNo = laData[4]

FOR lnCount = 1 TO 6
  lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(lnCount<=laData[4],IIF(EMPTY(&lcObjName),3,&lcObjName),0)
  lcObjStat  = "DISABLE"
  laObjdisp [lnCount] =IIF(lnCount<=laData[4],.T.,.F.)
  SHOW GET (lcObjName) &lcObjStat
ENDFOR
 
*!**************************************************************************
*!
*!      Function: lfvData_4
*!
*!**************************************************************************
* Valid of all segments :Calculat the new mask and description and show 
* the objects enable or disable
*
FUNCTION lfvData_4

IF llComIdFlg
  llCopyFlg    = .T. 
  llAcsUsed    = .F.
  SHOW GET pbDiscrip ENABLE
  SHOW GET pbCopy    ENABLE
  SHOW GET pbSave      ENABLE
ENDIF

IF !EMPTY(laData[4]) .AND. LASTKEY() = 13
  IF laData[4] < lnSetCostC .AND. lnSetCostC > 0
    *** You have been setup your cost ***
    *** center on segment no. ð.
    *** To decrease the no. of segment, ***
    *** you  have to adjust  your  cost ***
    *** center first. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00173B00000","DIALOG",ALLTRIM(STR(lnOldSNo)))
    laData[4] = lnOldSNo
    SHOW GET laData[4]
    RETURN
  ENDIF
ENDIF

FOR lnCount = 1 TO 6
  lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
  &lcObjName = IIF(lnCount<=laData[4],IIF(EMPTY(&lcObjName),3,&lcObjName),0)
  lcObjStat  = IIF(!llAcsUsed,IIF(lnCount<=laData[4],"ENABLE","DISABLE"),;
               IIF(lnCount>lcOldNoSeg .AND. lnCount<=laData[4] ,"ENABLE",;
               "DISABLE"))
  laObjdisp [lnCount] =IIF(lnCount<=laData[4],.T.,.F.)
  SHOW GET (lcObjName) &lcObjStat
ENDFOR

*** Redifine the variable used to control the display of the child screen
lnBoxh = (laData[4] * 2) + 5
=lfTotalSiz ()
 
*!**************************************************************************
*!
*!      Function: lfTotalSiz
*!
*!**************************************************************************
*
FUNCTION lfTotalSiz

laData[1]   = MAX(laData[4]-1,0)
laData[2]   = ''
laData[3]   = ''
FOR lnCount = 1 TO laData[4]
  lnVarName  = "lnSeg"+ALLTRIM(STR(lnCount))
  laData[1]  = laData[1] + &lnVarName 
  laData[2]  = laData[2] +IIF(EMPTY(laData[2]),'','-')+;
               REPLICATE('#',&lnVarName)
  laData[3]  = laData[3] +IIF(EMPTY(laData[3]),'','-')+;
               IIF(LEN(ALLTRIM(laAcDes[lnCount,1]))>&lnVarName,;
               LEFT(ALLTRIM(laAcDes[lnCount,1]),&lnVarName),;
               ALLTRIM(laAcDes[lnCount,1])+;
               SPACE(&lnVarName-LEN(ALLTRIM(laAcDes[lnCount,1]))))
ENDFOR 
IF WREAD(lcBaseWind)
  SHOW GETS
ENDIF

*!**************************************************************************
*!
*!      Function: lfvDiscrip
*!
*!**************************************************************************
* Branch to segments descriptions screen
*
FUNCTION lfvDiscrip

llCancel = .F.

*E300683,4 Call *.SPR from screens directory
* DO SMACDES.SPR 
DO (gcScrDir + gcWinAppl + '\SMACDES.SPR')
*E300683,4 end   
llChange = llChange OR !llCancel

IF llChange
  SHOW GET pbSave ENABLE
ENDIF

SHOW GETS

*!**************************************************************************
*!
*!      Function: lfvSave
*!
*!**************************************************************************
* On termination of the home screen save data to master file
*
FUNCTION lfvSave

IF llChange
 =lfSaveData()
ENDIF

*!**************************************************************************
*!
*!      Function: lfvClose
*!
*!**************************************************************************
* On termination of the home screen save data to master file
*
FUNCTION lfvClose

IF llChange
  lnOption = gfModalGen("QRM00153B00025","DIALOG")
  DO CASE
    CASE lnOption = 1
      IF lfSaveData()
        llChange = .F.
      ENDIF
    CASE lnOption = 2
      llChange = .F.
  ENDCASE
ENDIF
*E300692,4 Hesham El_Sheltawi (Start)
*IF SEEK(lcComp_Id,"SYCACCOD") .AND. !EMPTY(SYCACCOD.nacssegsz)
*  =gfObj_Lock(.F.)
*ENDIF
*E999999,1 Hesham (Start)
IF !EMPTY(lcComp_id) AND USED("SYCACCOD")
  *IF SEEK(lcComp_Id,"SYCACCOD") .AND. !EMPTY(SYCACCOD.nacssegsz)
  IF SEEK("","SYCACCOD") .AND. !EMPTY(SYCACCOD.nacssegsz)  
*E999999,1 Hesham (End)  
    SELECT SYCACCOD
    =gfObj_Lock(.F.)
  ENDIF
ENDIF  
*E300692,4 Hesham El_Sheltawi (End)
IF !llChange
  CLEAR READ
ENDIF

*!**************************************************************************
*!
*!      Function: lfSegDes
*!
*!**************************************************************************
*
* Function used in the picture of all short descriptions to define how manay
* Characters to be accepted in each field
*
FUNCTION lfSegDes
PARAMETERS lnWidth

RETURN REPLICATE("N",lnWidth)

*!**************************************************************************
*!
*!      Function: lfvCopyACD
*!
*!**************************************************************************
*
FUNCTION lfvCopyACD

_TALLY = 0
*E300692,4 Hesham El_Sheltawi (Start)
*SELECT cComp_ID,cCom_Name,cCom_DDir," " ;
       FROM (gcSysHome+"SYCCOMP") ;
       WHERE SYCCOMP.cComp_ID <> lcComp_Id .AND. ;
             IIF(EMPTY(CCOM_DDIR),.F.,lfCheckDir(cComp_ID,.F.)) .AND. ;
             IIF(EMPTY(CCOMPPRNT),.T.,.F.) ;
       ORDER BY cCom_Name ;
       INTO ARRAY laCopyCom

SELECT cComp_ID,cCom_Name,cCom_DDir," " ;
       FROM (gcSysHome+"SYCCOMP") ;
       WHERE SYCCOMP.cComp_ID <> lcComp_Id .AND. ;
             IIF(EMPTY(CCOM_DDIR),.F.,lfCheckDir(CCOM_DDIR,.F.)) .AND. ;
             IIF(EMPTY(CCOMPPRNT),.T.,.F.) ;
       ORDER BY cCom_Name ;
       INTO ARRAY laCopyCom
*E300692,4 Hesham El_Sheltawi (End)
IF _TALLY > 0
  *** Valid function of the push button ***
  *** <copy from another company> ...
  DECLARE laCompInfo [ALEN(laCopyCom,1),1]
  laCompInfo = " "
  SELECT SYCACCOD
  FOR lnCount = 1 TO ALEN(laCopyCom,1)
*E300692,4 Hesham El_Sheltawi (Start)  
    =lfCheckDir(laCopyCom[lncount,3],.T.,lnCount)
*    IF SEEK(laCopyCom[lnCount,1])
*      laCopyCom [lnCount,4] = CACSEGDES
*      laCompInfo[lnCount,1] = laCopyCom[lnCount,1] + "  " + ;
*                              laCopyCom[lnCount,2] + "  " + ;
*                              laCopyCom[lnCount,4]
*    ENDIF
*E300692,4 Hesham El_Sheltawi (End)
  ENDFOR

  *** Select company to copy its account code struc. ***
  *E300683,4 Call *.SPR from screens directory
  * DO SMSELCM.SPR 
  DO (gcScrDir + gcWinAppl + '\SMSELCM.SPR')
  *E300683,4 end   
  *** Refresh all the data has been collected from another company. ***
  =lfvData_4()
  IF llChange
    SHOW GET pbSave ENABLE
  ENDIF
  SHOW GETS
ELSE
  *** No company available. ***
  *** <  Ok  > ***
  =gfModalGen("TRM00189B00000","DIALOG","companies")
ENDIF

*!**************************************************************************
*!
*!      Function: lfCheckDir
*!
*!**************************************************************************
*
FUNCTION lfCheckDir
*E300692,4 Hesham El_Sheltawi (Start)
*PARAMETERS lcCompId,llExcloded
*SELECT SYCACCOD
*RETURN (SEEK(lcCompId) .OR. llExcloded)
PARAMETERS lcComp_path,llExcloded,lnArrRow
PRIVATE lcAlias,llFound
llFound = .F.
lcAlias = gfTempName()
lcComp_path = ALLT(lcComp_path)
lcSetfull = SET('FULL')
SET FULLPATH ON
IF FILE(lcComp_path+'ACCOD.DBF')
  SELECT 0
  USE (lcComp_path+'ACCOD.DBF') AGAIN ALIAS &lcAlias ORDER TAG Accsegno
  LOCATE
  llFound = Found()
  IF llExcloded
    *T20060831.0036 TMI [Start] select the correct value to seek with
    *IF SEEK(laCopyCom[lnArrRow,1])
    IF SEEK('0')
      *T20060831.0036 TMI [End  ] 
      laCopyCom [lnArrRow,4] = CACSEGDES
      laCompInfo[lnArrRow,1] = laCopyCom[lnArrRow,1] + "  " + ;
                              laCopyCom[lnArrRow,2] + "  " + ;
                              laCopyCom[lnArrRow,4]
    ENDIF
  
  ENDIF
  USE IN (lcAlias)
ENDIF  
SET FULL &lcSetfull
RETURN (llFound .OR. llExcloded)
*E300692,4 Hesham El_Sheltawi (End)

*!**************************************************************************
*!
*!      Function: lfvCompany
*!
*!**************************************************************************
*
FUNCTION lfvCompany
*** Valid function of the list & select push button. ***
*E300692,4 Hesham El_Sheltawi (Start)
*SELECT sycaccod

*** Get information stored in the first record
*=SEEK(SUBSTR(laCompInfo[lsCompany,1],1,2))

*SCATTER FIELDS &lcScFields TO laData 
*llAcsUsed = lAcsUsed

*** Get Short and long description for all segment in array
*FOR lnCount = 1 TO laData[4]
*  laAcDes [lnCount,1] = gfSubStr(laData[3],lnCount,'-')
*  IF &lcAlias..CCOMP_ID = SUBSTR(laCompInfo[lsCompany,1],1,2) AND !EOF()
*    SKIP
*    laAcDes [lnCount,2] = cAcsLgDes
*  ENDIF  
*ENDFOR

*** Collect the no and size of segments from the old mask
*lnNext =0
*FOR lnCount  = 1 TO 6
*  lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
*  lnCrurnt   = lnNext
*  lnNext     = IIF(AT('-',laData[2],lnCount)=0,LEN(ALLTRIM(laData[2]))+1,;
*                 AT('-',laData[2],lnCount))
*  lnwidth    = MAX(0,lnNext-lnCrurnt-1) 
*  &lcObjName = lnwidth
*ENDFOR

lcAccFields = STRTRAN(UPPER(lcScFields),'SYCACCOD.')
lcAlias = gfTempName()
SELECT 0
USE (ALLT(laCopyCom[lsCompany,3])+'ACCOD.DBF') AGAIN ALIAS &lcAlias ORDER TAG Accsegno

SELECT (lcAlias)

*** Get information stored in the first record
*E999999,1 Hesham (Start)
*=SEEK(SUBSTR(laCompInfo[lsCompany,1],1,2))
=SEEK("")
*E999999,1 Hesham (End)

SCATTER FIELDS &lcAccFields TO laData 
llAcsUsed = lAcsUsed

*** Get Short and long description for all segment in array
FOR lnCount = 1 TO laData[4]
  laAcDes [lnCount,1] = gfSubStr(laData[3],lnCount,'-')
  *E999999,1 Hesham (Start)
  *IF &lcAlias..CCOMP_ID = SUBSTR(laCompInfo[lsCompany,1],1,2) AND !EOF()
  IF !EOF()  
  *E999999,1 Hesham (End)
    SKIP
    laAcDes [lnCount,2] = cAcsLgDes
  ENDIF  
ENDFOR

*** Collect the no and size of segments from the old mask
lnNext =0
FOR lnCount  = 1 TO 6
  lcObjName  = "lnSeg"+ALLTRIM(STR(lnCount))
  lnCrurnt   = lnNext
  lnNext     = IIF(AT('-',laData[2],lnCount)=0,LEN(ALLTRIM(laData[2]))+1,;
                 AT('-',laData[2],lnCount))
  lnwidth    = MAX(0,lnNext-lnCrurnt-1) 
  &lcObjName = lnwidth
ENDFOR
USE IN (lcAlias)
SELECT SYCACCOD
*E300692,4 Hesham El_Sheltawi (End)
llChange = .T.

*!**************************************************************************
*!
*!      Function: lfSaveData
*!
*!**************************************************************************
*
FUNCTION lfSaveData

SELECT SYCACCOD

IF laData[4] > 0
  *** This validate the  segments short  descriptions if it's ***
  *** empty or not, If it's empty, we force the user to enter ***
  *** the descriptions or accept default...
  llClrRead = .T.
  FOR lnCount = 1 TO laData[4]
    IF EMPTY(laAcDes[lnCount,1]) .OR. EMPTY(laData[3])
      *** You have to add segment short  descriptions ***
      *** or the system will add default descriptions ***
      *** <Enter descriptions> - <Add defaults> ***
      lnOption = gfModalGen("TRM00174B00029","DIALOG")
      DO CASE
        CASE lnOption = 1
          llClrRead = .F.
          FOR lnCont = 1 TO ALEN(laAcDes,1)
            laAcDes [lnCont,1] = " "
            laAcDes [lnCont,2] = " "
          ENDFOR
          _CUROBJ = OBJNUM(laAcDes[1,1])
          EXIT
        CASE lnOptioN = 2
          llClrRead = .F.
          FOR lnCont = 1 TO ALEN(laAcDes,1)
            laAcDes [lnCont,1] = "Sg" + ALLTRIM(STR(lnCont))
            laAcDes [lnCont,2] = "Default"
          ENDFOR
          EXIT
      ENDCASE
    ENDIF
  ENDFOR

  IF !llClrRead 
    *E300683,4 Call *.SPR from screens directory
    * DO SMACDES.SPR 
    DO (gcScrDir + gcWinAppl + '\SMACDES.SPR')
    *E300683,4 end   
    IF llCancel 
      FOR lnCont = 1 TO ALEN(laAcDes,1)
        laAcDes [lnCont,1] = " "
        laAcDes [lnCont,2] = " "
      ENDFOR
      llCancel = .F.
    ENDIF
    SHOW GETS
    RETURN .F.
  ENDIF

  *** Save mask and no of segments to the first record
  *E999999,1 Hesham (Start)
  *REPLACE ALL cComp_Id WITH "  " FOR CComp_Id  = lcComp_Id
  DELETE ALL
  *E999999,1 Hesham (End)
  IF !SEEK("  ")
    APPEND BLANK
  ENDIF
  GATHER FROM laData FIELDS &lcScFields
  *E999999,1 Hesham (Start)
  *REPLACE  CComp_Id WITH lcComp_Id
  *E999999,1 Hesham (End)
  =gfAdd_Info()

  *** Save seg. size, short and long description to records as no of seg.
  FOR lnCount = 1 TO laData[4]
    lnVarName  = "lnSeg"+ALLTRIM(STR(lnCount))
    IF !SEEK("  ")
      APPEND BLANK
    ENDIF
    *E999999,1 Hesham (Start)
    *REPLACE  CComp_Id  WITH lcComp_Id  ;
             nAcsSegNo WITH lnCount    ;
             nAcsSize  WITH &lnVarName ;
             cAcsShDes WITH IIF(EMPTY(laAcDes[lnCount,1]),;
                                "Sg"+ALLTRIM(STR(lnCont)),;
                                laAcDes[lnCount,1]);
             cAcsLgDes WITH IIF(EMPTY(laAcDes[lnCount,2]),;
                                "Default",laAcDes[lnCount,2])

    REPLACE  nAcsSegNo WITH lnCount    ;
             nAcsSize  WITH &lnVarName ;
             cAcsShDes WITH IIF(EMPTY(laAcDes[lnCount,1]),;
                                "Sg"+ALLTRIM(STR(lnCont)),;
                                laAcDes[lnCount,1]);
             cAcsLgDes WITH IIF(EMPTY(laAcDes[lnCount,2]),;
                                "Default",laAcDes[lnCount,2])

    *E999999,1 Hesham (End)                                
    =gfAdd_Info()
  ENDFOR
  *E999999,1 Hesham (Start)
  *DELETE ALL FOR EMPTY(cComp_Id)
  *E999999,1 Hesham (End)

  *** If there is childs companies belong to this company. ***
  *** Update the account code structure of these child companies...
  *E999999,1 Hesham (Start)    
*  SELECT SYCCOMP
*  DECLARE laChldComp[1]
*  laChldComp = " "
*  _TALLY     = 0

*  SELECT cComp_Id ;
         FROM  (gcSysHome+"SYCCOMP") ;
         WHERE cCompprnt = lcComp_Id ;
         INTO  ARRAY laChldComp

*  SELECT SYCACCOD 
*  IF _TALLY > 0
*    FOR lnCont1 = 1 TO ALEN(laChldComp,1)
    
*      *** Prepare array hold the current company account code struc. ***
*      DECLARE laStruc[1]
*      laStruc = " "
*      *E999999,1 Hesham (Start)
*      SELECT laChldComp[lnCont1] AS CCOMP_ID,NACSSEGSZ,CACSMASK, ;
             CACSEGDES,NACSNOSEG,NACSSEGNO, ;
             NACSSIZE,CACSSHDES,CACSLGDES, ;
             LACSUSED,gcUser_Id AS CADD_USER, ;
             DATE() AS DADD_DATE,CADD_TIME ;
      FROM   (gcSysHome+"SYCACCOD") ;
      WHERE  cComp_Id = lcComp_Id   ;
      INTO   ARRAY laStruc

*      REPLACE ALL FOR CCOMP_ID = laChldComp[lnCont1] ;
                      CCOMP_ID WITH "  "
                
*      DELETE ALL FOR EMPTY(CComp_Id)
    
*      APPEND FROM ARRAY laStruc ;
      FIELDS CCOMP_ID,NACSSEGSZ,CACSMASK, ;
             CACSEGDES,NACSNOSEG,NACSSEGNO, ;
             NACSSIZE,CACSSHDES,CACSLGDES, ;
             LACSUSED,CADD_USER,DADD_DATE, ;
             CADD_TIME

*    ENDFOR
*  ENDIF
*  IF SEEK(lcComp_Id,"SYCACCOD") .AND. !EMPTY(SYCACCOD.nacssegsz)
*    =gfObj_Lock(.F.)
*  ENDIF
  SELECT SYCCOMP
  DECLARE laChldComp[1]
  laChldComp = " "
  _TALLY     = 0
  SELECT cComp_Id,gfGetDataDir(ALLT(CCOM_DDIR)) ;
         FROM  (gcSysHome+"SYCCOMP") ;
         WHERE cCompprnt = lcComp_Id ;
         INTO  ARRAY laChldComp

  SELECT SYCACCOD 
  IF _TALLY > 0
    FOR lnCont1 = 1 TO ALEN(laChldComp,1)
    
      *** Prepare array hold the current company account code struc. ***
      DECLARE laStruc[1]
      laStruc = " "
      *E999999,1 Hesham (Start)
      USE (laChldComp[lnCont1,2]+'ACCOD') IN 0 AGAIN ALIAS CHLDACCOD ORDER TAG Accsegno
      SELECT CHLDACCOD
      DELETE ALL 
    
      IF !SEEK("  ")
        APPEND BLANK
      ENDIF
      GATHER FROM laData FIELDS &lcScFields
      =gfAdd_Info()

      *** Save seg. size, short and long description to records as no of seg.
      FOR lnCount = 1 TO laData[4]
        lnVarName  = "lnSeg"+ALLTRIM(STR(lnCount))
        IF !SEEK("  ")
          APPEND BLANK
        ENDIF

        REPLACE  nAcsSegNo WITH lnCount    ;
                 nAcsSize  WITH &lnVarName ;
                 cAcsShDes WITH IIF(EMPTY(laAcDes[lnCount,1]),;
                                    "Sg"+ALLTRIM(STR(lnCont)),;
                                    laAcDes[lnCount,1]);
                 cAcsLgDes WITH IIF(EMPTY(laAcDes[lnCount,2]),;
                                    "Default",laAcDes[lnCount,2])

        =gfAdd_Info()
      ENDFOR
      USE IN CHLDACCOD
    ENDFOR
  ENDIF
  IF SEEK('',"SYCACCOD") 
    =gfObj_Lock(.F.)
  ENDIF

  *E999999,1 Hesham (End)  
  IF llComIdFlg
    llChange = .F.
    llSavAcc = .T.
  ENDIF
ENDIF

*** Store old valus in variables
lnOldSize = laData[1]          && Old total size
lcOldMask = laData[2]          && Old Account Mask
lcOldDes  = laData[3]          && Old Account segments desciption
lcOldNoSeg= laData[4]          && Old No of segments

llChange = .F.
SHOW GET pbSave DISABLE

SHOW GETS

*!**************************************************************************
*!
*!      Function: lfvNewVal
*!
*!**************************************************************************
*
FUNCTION lfvNewVal

IF lcOldVal <> EVAL(VARREAD())
  llChange = llChange OR .T.
  SHOW GET pbSave ENABLE
ENDIF

*!**************************************************************************
*!
*!      Function: lfwOldVal
*!
*!**************************************************************************
*
FUNCTION lfwOldVal

lcOldVal = EVAL(VARREAD())

*E300692,4 Hesham El_Sheltawi (Start)
*!**************************************************************************
*!
*!      Function: lfOpenCmpData
*!
*!**************************************************************************
* function to open the account code struc. file from the company data dir.
FUNCTION lfOpenCmpData
IF USED('SYCACCOD')
  USE IN SYCACCOD
ENDIF
IF !EMPTY(lcComp_Id)
  USE (lcDataDir+'ACCOD') IN 0 AGAIN ALIAS SYCACCOD ORDER TAG Accsegno
ENDIF
SELECT SYCACCOD 
*E300692,4 Hesham El_Sheltawi (End)