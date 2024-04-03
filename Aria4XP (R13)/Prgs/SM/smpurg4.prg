*:************************************************************************
*:  Program File: ARIA4XP\PRGS\SM\SMPURG4.PRG
*:  Module      : System Manager
*:  Desc.       : Purge Program
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 08/27/2012
*:  Reference   : *E303220,1
*:************************************************************************
*N000682,1 04/16/2013 HES Globlization changes[Globalization]
*B610715,1 TMI 04/20/2014 allow to open the SYUSTATC exclusively [T20140402.0008]
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008]
*B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006]
*B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011]
*B610942,2 MMT 02/11/2015 Error while purging Sales Rep. Module[T20141111.0011]
*B610942,3 MMT 02/18/2015 Error while purging Key off in the 2nd purge rEun[T20141111.0011]
*B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007]
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008]
*E303963,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL
*E304030,1 SAH 07/5/2018 CONVERT STYINVJL TO SQL 
*E304030,2 MMT 03/05/2019 Error while purging style inventory module[T20180508.0022]
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007]
*B611938,1 ES 11/25/2019 Purge Program takes long time to purge data [T20171010.0007]
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves
*B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly
*:************************************************************************

*N000682,1 04/16/2013 HES Globlization changes[Start]
#INCLUDE R:\ARIA4XP\PRGS\SM\SMREBAL.H
*N000682,1 04/16/2013 HES Globlization changes[End  ]

*E303220,1 TMI 08/29/2012 [Start]
lcCr = CHR(13)+CHR(10)
lcBakFldr = ''   && xx.bak folder for sql backup files

*- Define the variable of the 1st Option grid
LOCAL lcVar,lcVal
lcPrepPurg = "SMPRPRG4"
lnRemResult = oAriaApplication.RemoteSystemData.Execute;
   ("Select * from SYREPUVR WHERE CREP_ID= '&lcPrepPurg'",'',"SYREPUVR","",oAriaApplication.cAria4SysFiles,3,"",SET("Datasession"))
SCAN FOR CREP_ID+CEXPTYPE+STR(NVARPOS) = lcPrepPurg  AND NVARPOS>0
  lcVar = ALLTRIM(MFLD_NAME)
  lcVal = ALLTRIM(MDATA_DEF)
  lcVal = IIF(CDATA_TYP='L' AND EMPTY(lcVal),'.F.',lcVal)
  &lcVar = IIF(CDATA_TYP='L',EVALUATE(lcVal),lcVal)
ENDSCAN
lcExp=gfOpGrid(lcPrepPurg,.T.,,,.t.)   && Prepare for Purge
IF TYPE('lcExp')='C' AND EVALUATE(lcExp) = .F.
  RETURN
ENDIF
*E303220,1 TMI 08/29/2012 [End]

*---------------------------------------------------------------------------------------------*
*----------------------------------------Main Section-----------------------------------------*
*---------------------------------------------------------------------------------------------*
*
*-- Declear Arrays for valid entrys of company option.
*E303220,1 TMI 08/29/2012 [Start] Define some variables
PRIVATE lnSubsc, lcACmpDir, lcCmpStat, lcHCmpCod, lcHCmpDir
STORE '' TO lnSubsc, lcACmpDir, lcCmpStat, lcHCmpCod, lcHCmpDir
*E303220,1 TMI 08/29/2012 [End  ]

DECLARE laRpCmpCod[1,5] , laRpCmpDsc[1,1] , laRpCmpNo[1,1]
STORE SPACE(0) TO laRpCmpCod , laRpCmpDsc , laRpCmpNo

*-- Declear Transaction array that be used in Transaction mover
DECLARE laRpSorTrn[1] , laRpTarTrn[1]
STORE SPACE(0) TO laRpSorTrn , laRpTarTrn

*-- Inatial value for variables of company .
STORE oAriaApplication.ActiveCompanyID TO lcDefCmp , lcOldVAl , lcRpComp
STORE {  /  /    } TO ldDefPrDt , ldRpPrDt

lcFilePath = oAriaApplication.DataDir

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*-- Add New procedure to the purge call Purge Rolles in case
*DECLARE laTransact[16,9] && Array to hold the all transaction
STORE .F. TO llTrkRolls  && Variable to  keep track Rolles Y/N.
lnAddTrkRl = 0
DECLARE laTransact[17,9] && Array to hold the all transaction
*E301816,1 ABD

*-SAB ----- [Start]
*- Move the code that build the transaction array to new function
=lfBuildTrans()
*-SAB ----- [End]

lcPathStat = SET('FULLPATH ')       && Varible to save the SET FULLPATH status
SET FULLPATH ON

=lfIntials()                        && Get Intial Values.

*-SAB ----- [Start]
*- Define the variable of the 2nd Option grid
STORE .F. TO llRpIgnrStat, llRpPrgMstr
*-SAB ----- [End]

*E303220,1 TMI 08/28/2012 [Start]
*llExpr = gfOpGrid('SMPURGE' , .T.)  && Run selection grid.
lnRpTrans = 0

lcExp = gfOpGrid('SMPURGE4',.T.,,,.t.)   && Purge Option Grid
IF TYPE('lcExp')='C' AND EVALUATE(lcExp) = .F.
  *=lfReOpenStatc()
  RETURN
ENDIF
*E303220,1 TMI 08/28/2012 [End  ]

*E303220,1 TMI 08/29/2012 [Start]
*- put this after the 2nd option grid and before starting the purge itself
IF !lfLockSys()
  glQuitting = .T.
  SET FULLPATH &lcPathStat
  lfReOpenStatc()
  RETURN .F.
ENDIF

*- If the user has no access to create the backup folder then exist the program
*-SAB ----- [Start]
STORE '' TO lcBackup
*-SAB ----- [End]
IF llRpBackup
  lcBackup = lfBackup()
  IF EMPTY(lcBackup)
    RETURN .F.
  ENDIF

  *-SAB ----- [Start]
  *&& do the backup
  *=lfBackup(.T.)
  *-SAB ----- [End]
ENDIF

*E303220,1 TMI 08/29/2012 [End]

*B604743,1 MHM 08/15/2001 variable to check material costing methid [Start]
IF !EMPTY(lcRpComp)
  lcCstMth   = gfGetMemVar('M_MATCSTMT',lcRpComp)
ENDIF
*B604743,1 MHM 08/15/2001 [End]

*B606114,1 AMH Variable to check Style costing method [Start]
IF !EMPTY(lcRpComp)
  lcStyCostM = gfGetMemVar('M_COST_MET',lcRpComp)
ENDIF
*B606114,1 AMH [End]

*-SAB ----- Add log to purge program [Start]
*- Generate New Temp file
PRIVATE llHasLog, lcTmpLogFile
llHasLog     = .F.
lcTmpLogFile = oAriaApplication.WorkDir + gfTempName() + ".TXT"
*-SAB ----- Add log to purge program [End]
IF TYPE('llExpr')  <> 'L'
  *-- check if N/A Company.
  IF EMPTY(lcRpComp)
    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"No company selected, Cannot purge.")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Purge,.F.,LANG_No_Comp)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge,loFormSet.GetHeaderText("LANG_Purge",loFormSet.HeaderAlias)),.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Comp,loFormSet.GetHeaderText("LANG_No_Comp",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]
  ELSE
    IF EMPTY(laRpTarTrn)
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"No transactions selected to purge.")
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Purge,.F.,LANG_No_Trans)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge,loFormSet.GetHeaderText("LANG_Purge",loFormSet.HeaderAlias)),.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Trans,loFormSet.GetHeaderText("LANG_No_Trans",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
    ELSE
      *-SAB ----- [Start]
      && do the backup
      IF llRpBackup
        =lfBackup(.T.)  && Don't forgit to uncomment this LineXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  UNCOMMENT
      ENDIF
      *-SAB ----- [End]
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][Start]
ldTimeX=SECONDS()
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][End]
      *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
  	  CREATE CURSOR PurgProg (Trans C(40),DpDate D(8),nHeadCnt N(15),nPerc N(15),llSel L,dLowest D(8),Value N(5))
  	  FOR lnT = 1 TO  16 + lnAddTrkRl
    	  IF IIF(ASCAN(laRpTarTrn,laTransact[lnT ,1]) <> 0 , .T. , .F.)
    	    lddLowest = {}
    	    lnTotRec =  lfGetTransCntDate(laTransact[lnT,1], ldRpPrDt)
    	    INSERT INTO PurgProg (Trans,DpDate,nHeadCnt,nPerc ,llSel,dLowest) VALUES (laTransact[lnT,1],ldRpPrDt,lnTotRec,0,.T.,IIF(ISNULL(lddLowest),{},lddLowest))
    	  ENDIF  
    	ENDFOR  
    	IF RECCOUNT('PurgProg') > 0
    	  SELECT 'PurgProg'
    	  LOCATE 
    	  =gfCallForm('SMPRGPRO','SM',"ldRpPrDt")
    	  glQuitting = .T.  && Rise quit flag because it's modal screen.
    	  * Reset the FULLPATH setting.
    	  SET FULLPATH &lcPathStat
    	  *- call this at the end of the purge
    	  lfReOpenStatc()
    	ENDIF
      RETURN 
      *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
      DO lpPurge                        && Run purge program.
    ENDIF
  ENDIF
ENDIF

glQuitting = .T.  && Rise quit flag because it's modal screen.

* Reset the FULLPATH setting.
SET FULLPATH &lcPathStat


*- call this at the end of the purge
*SET STEP ON 
lfReOpenStatc()

*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	ldTimeY=SECONDS()
*!*	ldTime=ldTimeY-ldTimeX
*!*	STRTOFILE(CHR(13)+CHR(10) + "Time : "+ STR(ldTime)+ CHR(13)+CHR(10), 'D:\Shared\PurgedLog.txt' ,1)

*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][End]


*--- end of main program.

************************************************************
*! Name      : lfReOpenStatc
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/28/2012
*! Purpose   : Reopen SYUSTATC file
************************************************************
FUNCTION lfReOpenStatc
*- close SYUSTATC file,open it in the normal way, exit

IF USED('SYUSTATC')
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*  USE IN SYUSTATC
  =gfCloseTable('SYUSTATC')
 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
   
ENDIF
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*=gfOpenFile(oAriaApplication.cAria4SysPath+'SYUSTATC','CUSER_ID','SH')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
=gfOpenTable(oAriaApplication.cAria4SysPath+'SYUSTATC','CUSER_ID','SH')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
IF USED('A27_SYUSTATC')
  USE IN A27_SYUSTATC
ENDIF

ENDFUNC
*- End of lfReOpenStatc  .

************************************************************
*! Name      : lfLockSys
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/28/2012
*! Purpose   : Lock the system
************************************************************
FUNCTION lfLockSys

*!*	*- Lock the system a4
*!*	TRY
*!*	  SELECT SYUSTATC
*!*	  USE (oAriaApplication.cAria4SysPath+'SYUSTATC') EXCLUSIVE
*!*	CATCH
*!*	  *36057
*!*	  *ð is in use by another user.
*!*	  lcMsg = 'Cannot run purge program, the system'
*!*	  gfModalGen('INM36057B00000','DIALOG',lcMsg)
*!*	  *-SAB ----- [Start]
*!*	  *RETURN .F.
*!*	  llContinue = .F.
*!*	  *-SAB ----- [End]
*!*	ENDTRY

*!*	*- Lock the system a27
*!*	TRY
*!*	  *-SAB ----- [Start]
*!*	  *USE (oAriaApplication.SysPath+'SYUSTATC') EXCLUSIVE IN 0 ALIAS a27_SYUSTATC
*!*	  IF llContinue
*!*	    USE (oAriaApplication.SysPath+'SYUSTATC') EXCLUSIVE IN 0 ALIAS a27_SYUSTATC
*!*	  ENDIF
*!*	  *-SAB ----- [End]
*!*	CATCH
*!*	  *36057
*!*	  *ð is in use by another user.
*!*	  lcMsg = 'Cannot run purge program, the Aria27 system'
*!*	  gfModalGen('INM36057B00000','DIALOG',lcMsg)
*!*	  *-SAB ----- [Start]
*!*	  *RETURN .F.
*!*	  llContinue = .F.
*!*	  *-SAB ----- [End]
*!*	ENDTRY

*B610715,1 TMI 04/20/2014 10:35 [Start] list all the open dbfs, check them , close the one of SYUSTATC
LOCAL lnDbf
FOR lnDbf = 1 TO 255
  
  IF !EMPTY(ALIAS(lnDbf)) AND 'SYUSTATC'$DBF(lnDbf)
    SELECT (ALIAS(lnDbf))
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
 *       USE 
  =gfCloseTable('SYUSTATC')
   *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  ENDIF 
ENDFOR 
*B610715,1 TMI 04/20/2014 10:35 [End  ] 

LOCAL llContinue, lcOnErr
llContinue = .T.
lcOnErr = ON('Error')
ON ERROR llContinue = .F.
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][Start]
*SELECT SYUSTATC
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][End]

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*USE (oAriaApplication.cAria4SysPath+'SYUSTATC') EXCLUSIVE ORDER CUSER_ID   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
=gfOpenTable(oAriaApplication.cAria4SysPath+'SYUSTATC','CUSER_ID','EX')
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

IF !llContinue
  *ð is in use by another user.
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  lcMsg = 'Cannot run purge program, the system'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcMsg = LANG_cannot_Run
lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_cannot_Run,loFormSet.GetHeaderText("LANG_cannot_Run",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  gfModalGen('INM36057B00000','DIALOG',lcMsg)
ENDIF

*B610715,1 TMI 04/20/2014 11:39 [Start] comment the following as there is no longer a27 sysfiles
*!*	IF llContinue
*!*	  USE (oAriaApplication.SysPath+'SYUSTATC') EXCLUSIVE IN 0 ALIAS A27_SYUSTATC

*!*	  IF !llContinue
*!*	    *ð is in use by another user.
*!*	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    *!*	    lcMsg = 'Cannot run purge program, the Aria27 system'
*!*	    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*!*	    *lcMsg = LANG_cannot_Run_A27
*!*	    lcMsg = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_cannot_Run_A27,loFormSet.GetHeaderText("LANG_cannot_Run_A27",loFormSet.HeaderAlias))
*!*	    *N000682,1 11/20/2012 MMT Globlization changes[End]

*!*	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
*!*	    gfModalGen('INM36057B00000','DIALOG',lcMsg)
*!*	  ENDIF
*!*	ENDIF
*B610715,1 TMI 04/20/2014 11:39 [End  ] 

ON ERROR &lcOnErr.

RETURN llContinue
*- End of lfLockSys.

************************************************************
*! Name      : lfvNewHst
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2012
*! Purpose   : Create new History Company validation
************************************************************
FUNCTION lfvNewHst

*- End of lfvNewHst.
************************************************************
*! Name      : lfvBackup
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2012
*! Purpose   : Create backup files
************************************************************
FUNCTION lfvBackup

*- End of lfvBackup.
************************************************************
*! Name      : lfvAppTHst
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2012
*! Purpose   : Append to Existing History Company
************************************************************
FUNCTION lfvAppTHst

*- End of lfvAppTHst.
************************************************************
*! Name      : lfvPrvRun
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2012
*! Purpose   : check for Valid run
************************************************************
FUNCTION lfvPrvRun
IF llRpNewHst AND !llRpAppTHst
  *00113
  *Company files cannot be ð.
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  gfModalGen('INM00113B00000','DIALOG','Purged')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*gfModalGen('INM00113B00000','DIALOG',LANG_Purged)
gfModalGen('INM00113B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purged,loFormSet.GetHeaderText("LANG_Purged",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  RETURN .F.
ENDIF

*-SAB ----- [Start]
*IF llRpAppTHst AND EMPTY(gfGetMemvar('M_COMP_ID'))
*  gfModalGen('INM00274B00000','DIALOG','The active company does not have a history Company, can not run purge')
*  RETURN .F.
*ENDIF
*-SAB ----- [End]

IF !llRpBackup
  *40169
  *Are you sure you want to ð?
  *\!\<Proceed;\?\<Cancel
  && 2 : Cancel
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  IF gfModalGen('INB40169B00012','DIALOG','purge the data without taking a backup') = 2
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF gfModalGen('INB40169B00012','DIALOG',LANG_Purge_Data) = 2
IF gfModalGen('INB40169B00012','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge_Data,loFormSet.GetHeaderText("LANG_Purge_Data",loFormSet.HeaderAlias))) = 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
*- End of lfvPrvRun.


************************************************************
*! Name      : lfvProceed
*! Developer : Saber A Razek (SAB)
*! Date      : 08/30/2012
*! Purpose   : Validate the Option Grid
************************************************************
FUNCTION lfvProceed

IF EMPTY(ldRpPrDt)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  gfModalGen('INM00113B00000','DIALOG','Purged date')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*gfModalGen('INM00113B00000','DIALOG',LANG_Purged_date)
gfModalGen('INM00113B00000','DIALOG',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purged_date,loFormSet.GetHeaderText("LANG_Purged_date",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  RETURN .F.
ENDIF

RETURN .T.
*- End of lfvProceed


************************************************************
*! Name      : lfBackup
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/27/2012
*! Purpose   : Get Backup Folder, create it if not found
************************************************************
FUNCTION lfBackup
PARAMETERS llDoBackup

LOCAL lcRoot,llRet,lcInfo
llGetBackupInfo = !llDoBackup

IF llGetBackupInfo
  lcRoot = ADDBS(oAriaApplication.cAria4SysPath)
  lcExpireDate = DATE() + 30
  lcBackup = SUBSTR(lcRoot,1,RAT('\',lcRoot,2))+'BACKUP\'+;
             oAriaApplication.ActiveCompanyID+;
             '-EXPIRE_'+ DTOS(lcExpireDate)+'\'
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][Start]
  lcBackup = oAriaApplication.AriaEnviromentVariables.ResolveMappedDrive(lcBackup)
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][End]              
  lcInfo = 'Backed up by the purge program'+CHR(13)+CHR(10)+;
           'User:'+oAriaApplication.User_ID
  IF !DIRECTORY(lcBackup)
    TRY
      MD (lcBackup)
      STRTOFILE(lcInfo,lcBackup+'Info.txt',1)
    CATCH
      *00017
      *Access denied. Please check with your system administrator.
      =gfModalGen('INM00017B00000','DIALOG')
      lcBackup = ''
    ENDTRY
  ENDIF
  RETURN lcBackup

ELSE
  lcSafe = SET("Safety")
  SET SAFETY OFF

  LOCAL lnI
  lcCompDir = STRTRAN(oAriaApplication.DataDir, oAriaApplication.ActiveCompanyID, lcRpComp)
  ADIR(laDbfs,lcCompDir+'*.DBF')
  *- Backup Company DBFs files

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  WAIT WINDOW "Backup Company Tables Please Wait ..." NOCLEAR
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Backup_Comp NOCLEAR
*E303456,1 TMI 04/20/2014 13:03 [Start] add nowait
*WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_Comp,loFormSet.GetHeaderText("LANG_Backup_Comp",loFormSet.HeaderAlias)) NOCLEAR
WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_Comp,loFormSet.GetHeaderText("LANG_Backup_Comp",loFormSet.HeaderAlias)) NOCLEAR
*E303456,1 TMI 04/20/2014 13:03 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]

  FOR lnI = 1 TO ALEN(laDbfs,1)
    *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
    TRY
    *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]    
      COPY FILE (lcCompDir+laDbfs[lnI,1]) TO (lcBackup+laDbfs[lnI,1])
    *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
    CATCH
    ENDTRY  
    *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]        
    lcCdx = STRTRAN(laDbfs[lnI,1],'.DBF','.CDX')
    IF FILE(lcCompDir+lcCdx)
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
      TRY
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]
        COPY FILE (lcCompDir+lcCdx) TO (lcBackup+lcCdx)
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
      CATCH
      ENDTRY
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]      
    ENDIF

    lcFPT = STRTRAN(laDbfs[lnI,1],'.DBF','.FPT')
    IF FILE(lcCompDir+lcFPT)
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
      TRY
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]     
        COPY FILE (lcCompDir+lcFPT) TO (lcBackup+lcFPT)
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][Start]
      CATCH
      ENDTRY 
      *B610942,1 MMT 02/05/2015 Purge program: File is ue error while creating DBFs backup[T20141111.0011][End]
    ENDIF
  ENDFOR
  WAIT CLEAR

*SET STEP ON 
  *- Backup Company SQL Database
  IF USED('SYCCOMP')
    USE IN SYCCOMP
  ENDIF
  =gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')  && CCOMP_ID
  SELECT SYCCOMP
  =SEEK(ALLTRIM(UPPER(lcRpComp)))
  lcDBSrvr = ALLTRIM(SYCCOMP.cConServer)
  lcDBName = ALLTRIM(SYCCOMP.cConDBName)
  lcDBUser = ALLTRIM(SYCCOMP.cConUserID)
  lcDBPass = ALLTRIM(SYCCOMP.cConPaswrd)
  lcBackStat = "BACKUP DATABASE [" + lcDBName + "]"
  lcBackStat = lcBackStat + " TO  DISK = N'" + ADDBS(lcBackup) + lcDBName
  lcBackStat = lcBackStat + ".bak' WITH NOFORMAT, NOINIT,  NAME = N'[" + lcDBName +"]"
  lcBackStat = lcBackStat + "', SKIP, NOREWIND, NOUNLOAD,  STATS = 10"

  lcSQLConStr = oAriaApplication.mReadConStr(lcRpComp)
  lnHand   = SQLSTRINGCONNECT(lcSQLConStr)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  WAIT WINDOW "Backup SQL Company Tables Please Wait ..." NOCLEAR
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Backup_SQL NOCLEAR
*E303456,1 TMI 04/20/2014 13:03 [Start] add nowait
*WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_SQL,loFormSet.GetHeaderText("LANG_Backup_SQL",loFormSet.HeaderAlias)) NOCLEAR
WAIT WINDOW NOWAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_SQL,loFormSet.GetHeaderText("LANG_Backup_SQL",loFormSet.HeaderAlias)) NOCLEAR
*E303456,1 TMI 04/20/2014 13:03 [End  ] 
*N000682,1 11/20/2012 MMT Globlization changes[End]
*SET STEP ON 
  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  lnResult = SQLEXEC(lnHand, lcBackStat, "Result")
  WAIT CLEAR

  =SQLDISCONNECT(lnHand)
  
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
  *USE IN SYCCOMP
  IF USED('SYCCOMP')
    USE IN SYCCOMP
  ENDIF
 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
  *- restore setting
  SET SAFETY &lcSafe
*SET STEP ON 
  IF lnResult < 1

    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    WAIT WINDOW "Backup Failed!" TIMEOUT 2
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Backup_Failed TIMEOUT 2
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_Failed,loFormSet.GetHeaderText("LANG_Backup_Failed",loFormSet.HeaderAlias)) TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]

    RETURN .F.
  ELSE

    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    WAIT WINDOW "Backup Completed Successfully." TIMEOUT 2
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Backup_Completed TIMEOUT 2
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup_Completed,loFormSet.GetHeaderText("LANG_Backup_Completed",loFormSet.HeaderAlias)) TIMEOUT 2
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]

    RETURN .T.
  ENDIF


ENDIF

ENDFUNC
*- End of lfBackup.


*!*************************************************************
*! Name      : lfIntials
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Intial Values for Option Grid.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfGetComps, lfGetTrn, lfGetPrDt
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfIntials()
*!*************************************************************

FUNCTION lfIntials
*E303220,1 TMI 08/29/2012 [Start] open some tables
IF USED('SYDAPPL')
  USE IN SYDAPPL
ENDIF
IF USED('SYCCOMP')
  USE IN SYCCOMP
ENDIF
=gfOpenFile(oAriaApplication.SysPath+'SYDAPPL','CAPP_ID','SH')
=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID','SH')  && CCOMP_ID
*E303220,1 TMI 08/29/2012 [End  ]

*-- Fill Company Array.
=lfGetComps()
*-- check the current company
IF EMPTY(lcDefCmp)
  STORE SPACE(0) TO lcDefCmp , lcOldVal
ELSE
  llHist = GfGetMemVar('LLHIST',lcDefCmp)
  lcHisCmpID = GfGetMemVar('M_COMP_ID',lcDefCmp)
  *E303220,1 TMI 08/29/2012 [Start] check if not valid company to purge
  *IF !llHist .AND. EMPTY(lcHisCmpID)
  IF !llHist .AND. (EMPTY(lcHisCmpID) OR !SEEK(lcHisCmpID,'SYCCOMP'))
    *E303220,1 TMI 08/29/2012 [End  ]
    laRpCmpCod[ASCAN(laRpCmpNo,lcDefCmp)-1,5] = 'N'
    STORE SPACE(0) TO lcDefCmp , lcOldVal
  ELSE
    laRpCmpCod[ASCAN(laRpCmpNo,lcDefCmp)-1,5] = IIF(llHist,'H','A')
  ENDIF
ENDIF

IF !EMPTY(lcDefCmp)
  =lfGetTrn(lcDefCmp)              && get company transaction
  ldDefPrDt = lfGetPrDt(lcDefCmp)  && get purge date
ENDIF
*-- end of lfIntials.
************************************************************
*! Name      : lfwOGWhen
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 08/29/2012
*! Purpose   : When Function
************************************************************
FUNCTION lfwOGWhen
ldRpPrDt = ldDefPrDt
*- End of lfwOGWhen.

*!*************************************************************
*! Name      : lfGetComps
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get company array
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetComps()
*!*************************************************************

FUNCTION lfGetComps
PRIVATE lnComNo , lnI
*-- Collect all companies
SELECT ccomp_id+" - "+cCom_Name,cCom_dDir,mComp_mdl,mModlSet,"" ;
  FROM SYCCOMP                            ;
  INTO ARRAY laRpCmpCod                   ;
  ORDER BY 1

lnComNo = _TALLY

DECLARE laRpCmpDsc[lnComNo+1,1] , laRpCmpNo[lnComNo+1,1]
laRpCmpDsc[1,1] ='N/A'
laRpCmpNo[1,1] = SPACE(0)
IF lnComNo > 0
  lnI = 1
  FOR lnI = 1 TO lnComNo
    laRpCmpDsc[lnI+1,1] = laRpCmpCod[lnI,1]
    laRpCmpNo[lnI+1,1]  = LEFT(laRpCmpCod[lnI,1],2)
  ENDFOR
ENDIF
*--- end of lfGetComps.

*!*************************************************************
*! Name      : lfChVPrCmp
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Check Valid Purge Company
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : GfGetMemVar, gfModalGen
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to check)
*!*************************************************************
*! Returns            : lcCompCod (Company code after check)
*!*************************************************************
*! Example   : = lfChVPrCmp(X)
*!*************************************************************

FUNCTION lfChVPrCmp
PARAMETER lcCompCod
PRIVATE lnCmpNo

IF !EMPTY(lcCompCod)
  lnCmpNo = ASCAN(laRpCmpNo,lcCompCod)-1
  DO CASE
    CASE laRpCmpCod[lnCmpNo,5] = 'N'     && case not valid company
      *-- Message
      *** The Selected Company don't have a History Company. Select Another One.***
      *** <  Ok  > ***
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
*!*	                  "The selected company does not have a history company.")
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_companies,.F.,LANG_comp_Not_Hist)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_companies,loFormSet.GetHeaderText("LANG_companies",loFormSet.HeaderAlias)),.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_comp_Not_Hist,loFormSet.GetHeaderText("LANG_comp_Not_Hist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[Start]
      lcCompCod = lcOldVal
    CASE laRpCmpCod[lnCmpNo,5] =''
      llHist = GfGetMemVar('LLHIST',lcCompCod)
      lcHisCmpID = GfGetMemVar('M_COMP_ID',lcCompCod)
      IF !llHist .AND. EMPTY(lcHisCmpID)
        *-- Message
        *** The Selected Company don't have a History Company. Select Another One.***
        *** <  Ok  > ***

        *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	        =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
*!*	                    "The selected company does not have a history company.")
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_companies,.F.,LANG_comp_Not_Hist)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_companies,loFormSet.GetHeaderText("LANG_companies",loFormSet.HeaderAlias)),.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_comp_Not_Hist,loFormSet.GetHeaderText("LANG_comp_Not_Hist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 04/16/2013 HES Globlization changes[Start]
        laRpCmpCod[ASCAN(laRpCmpNo,lcCompCod)-1,5] = 'N'                 && not valid company
        lcCompCod = lcOldVal
      ELSE
        laRpCmpCod[ASCAN(laRpCmpNo,lcCompCod)-1,5] = IIF(llHist,'H','A') && H=History, A= Active
      ENDIF
  ENDCASE
ENDIF
RETURN lcCompCod
*--- end of lfChVPrCmp.

*!*************************************************************
*! Name      : lfGetTrn
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get source & target transaction array
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials, OG
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to get its
*!                      source & target transaction array)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetTrn(X)
*!*************************************************************

FUNCTION lfGetTrn
PARAMETER lcCompCod

* Copy Transaction array to source Transaction array.
*-SAB ----- [Start]
*lnCount = 0
lnItmCount = 0
*-SAB ----- [End]
lnCmpCodNo = ASCAN(laRpCmpNo,lcCompCod)-1

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*E301816,1 ABD - Check if selected company is Keep track lot.
*FOR lnI = 1  TO  16
STORE .F. TO llTrkRolls
llTrkRolls = IIF(gfGetMemVar('M_TrkRolls',lcRpComp)='Y',.T.,.F.)
lnAddTrkRl = IIF(llTrkRolls,1,0)
FOR lnI = 1  TO  16 + lnAddTrkRl
  *E301816,1 ABD - [End]

  IF laTransact[lnI,2]$laRpCmpCod[lnCmpCodNo,3]
    IF SEEK(laTransact[lnI,2],'SYDAPPL')
      IF !SYDAPPL.lSetreq .OR. laTransact[lnI,2]$laRpCmpCod[lnCmpCodNo,4]
        *-SAB ----- [Start]
        *lnCount = lnCount + 1
        *DECLARE laRpSorTrn[lnCount] , laRpTarTrn[lnCount]
        *laRpSorTrn[lnCount]  =  laTransact[lnI , 1]
        *laRpTarTrn[lnCount]  =  laTransact[lnI , 1]
        lnItmCount = lnItmCount + 1
        DECLARE laRpSorTrn[lnItmCount] , laRpTarTrn[lnItmCount]
        laRpSorTrn[lnItmCount]  =  laTransact[lnI , 1]
        laRpTarTrn[lnItmCount]  =  laTransact[lnI , 1]
        *-SAB ----- [End]
      ENDIF
    ENDIF
  ENDIF
ENDFOR
*--- end of lfGetTrn.

*!*************************************************************
*! Name      : lfGetPrDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Company Purge Date
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfIntials, Option Grid
*!*************************************************************
*! Passed Parameters  : lcCompCod (Company code to get its
*!                                 purge date)
*!*************************************************************
*! Returns            : ldGetDate (Purge date)
*!*************************************************************
*! Example   : =lfGetPrDt(X)
*!*************************************************************

FUNCTION lfGetPrDt
PARAMETER lcCompCod
PRIVATE ldGetDate

lnCmpCodNo = ASCAN(laRpCmpNo,lcCompCod)-1
=gfOpenFile(ALLTRIM(laRpCmpCod[lnCmpCodNo,2])+'FISHD','Compfyear','SH')

lnAlias = SELECT(0)
SELECT FISHD
LOCATE FOR CFISYSTAT ='P'
ldGetDate = DFISBGDAT-1
SELECT (lnAlias)

IF USED('FISHD')
  USE IN FISHD
ENDIF
RETURN ldGetDate
*--- end of lfGetPrDt.

*---------------------------------------------------------------------------------------------*
*-------------------------------------Option Grid Section-------------------------------------*
*---------------------------------------------------------------------------------------------*

*!*************************************************************
*! Name      : lfvCompany
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Valid Function for Company
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvCompany()
*!*************************************************************

FUNCTION lfvCompany

*-SAB ----- [Start]
ClearRead()
*-SAB ----- [End]
*-- Check if the selected company have a history company.
lcRpComp =lfChVPrCmp(lcRpComp)

*-- update transaction arrays and purge date.
IF !(lcRpComp == lcOldVal) .AND. !EMPTY(lcRpComp)
  =lfGetTrn(lcRpComp)
  ldRpPrDt = lfGetPrDt(lcRpComp)
ENDIF
*-SAB ----- [Start]
*CLEARREAD()
*-SAB ----- [End]

*-- update lcOldVal with current company.

lcOldVal = lcRpComp

*-- end of lfvCompany.

*!*************************************************************
*! Name      : lfvTransct
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : - Call Categories mover function
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Called from : OG
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvTransct()
*!*************************************************************

FUNCTION lfvTransct

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	= gfMover(@laRpSorTrn,@laRpTarTrn,'Select Module',.T.,'')  && call mover function.
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfMover(@laRpSorTrn,@laRpTarTrn,LANG_Select_Module,.T.,'')  && call mover function.
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*= gfMover(@laRpSorTrn,@laRpTarTrn,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Module,loFormSet.GetHeaderText("LANG_Select_Module",loFormSet.HeaderAlias)),.T.,'')  && call mover function.
= lfOgMover(@laRpSorTrn,@laRpTarTrn,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_Module,loFormSet.GetHeaderText("LANG_Select_Module",loFormSet.HeaderAlias)),.T.,'')  && call mover function.
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]

*-- end of lfvTransct.

*!*************************************************************
*! Name      : lfvPrDt
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : valid function of purge date
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfvPrDt()
*!*************************************************************
FUNCTION lfvPrDt

*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][Start]
lcControl = loOGScroll.FocusControl
lcCurVar  = loOGScroll.&lcControl.
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][End]
*-- Chech if Purge Date is Empty.
IF EMPTY(ldRpPrDt)
  *-- Message
  *** Date Can't be Empty ***
  *** <  Ok  > ***
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,"Date cannot be empty.")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_companies ,.F.,LANG_Date_Cannot_Empty)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_companies,loFormSet.GetHeaderText("LANG_companies",loFormSet.HeaderAlias)) ,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Date_Cannot_Empty,loFormSet.GetHeaderText("LANG_Date_Cannot_Empty",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  *B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][Start]
  *_CUROBJ = _CUROBJ
  ldRpPrDt = lcCurVar.ActiveControl.oldValue
  *B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][End]
  RETURN
ENDIF

*-- Check Purge date belonging in the Previous Year.
lnCmpCodNo = ASCAN(laRpCmpNo,lcRpComp)-1
=gfOpenFile(ALLTRIM(laRpCmpCod[lnCmpCodNo,2])+'FISHD','Compfyear','SH')

lnAlias = SELECT(0)
SELECT FISHD
LOCATE FOR CFISYSTAT ='P'
IF ldRpPrDt > DFISBGDAT-1
  *-- Message
  *** Date Can't be greater than or equal the Start Day of Previous Year ***
  *** <  Ok  > ***
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  =gfModalGen("TRM00000B00000","DIALOG","companies",.F.,;
*!*	              "Date cannot be greater than or equal the Start Date of the Previous Year.")
  =gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_companies,loFormSet.GetHeaderText("LANG_companies",loFormSet.HeaderAlias)) ,.F.,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Date_NotValid,loFormSet.GetHeaderText("LANG_Date_NotValid",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]
  *B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][Start]
  * _CUROBJ = _CUROBJ
  ldRpPrDt = lcCurVar.ActiveControl.oldValue
  *B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][End]
ENDIF
SELECT (lnAlias)

IF USED('FISHD')
  USE IN FISHD
ENDIF
*--- end of lfvPrDt.

*---------------------------------------------------------------------------------------------*
*----------------------------------------Purge Section----------------------------------------*
*---------------------------------------------------------------------------------------------*

*!*************************************************************
*! Name      : lpPurge
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Purge DataBase Procedure.
*!*************************************************************
*! Calls     :
*!             Procedures : lpPrTrn
*!             Functions  : gfGetMemVar, lfGetNdFls, lfAdGlDist,
*!                          lfAddMsFls, lfAddMsFls, lfOpenFile,
*!                          lfCpMsFls, lfPackFile
*!*************************************************************
*! Called from : Main Program
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPurge
*!*************************************************************

PROCEDURE lpPurge
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
PARAMETERS lcTransaction,ldPDate,loFormSet
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
PRIVATE lnCount, lnI, lnJ
PRIVATE lnFPosFl, lnLPosFl, lnFPosIn, lnLPosIn, lnPosSt, lcFileSt
PRIVATE lnFPosRF, lnLPosRF, lnFPosRD, lnLPosRD, lnFileNo, lcStySeq, lcMatSeq
*-- Fill Transaction array col 3
*SET STEP ON 
lnI = 1
*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16
FOR lnI = 1 TO 16 + lnAddTrkRl
  *E301816,1 ABD - [End]
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
  *laTransact[lnI,3]=IIF(ASCAN(laRpTarTrn,laTransact[lnI,1]) <> 0 , .T. , .F.)
  IF ALLTRIM(laTransact[lnI,1]) <> ALLTRIM(lcTransaction)
    laTransact[lnI,3]= .F.
  ELSE
    laTransact[lnI,3]=.T.
  ENDIF
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
ENDFOR
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
ldOldPDate = ldRpPrDt
ldRpPrDt = ldPDate
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-- Get Active and History compnay information
lnSubsc = ASCAN(laRpCmpNo,lcRpComp)-1
lcACmpDir = ALLTRIM(laRpCmpCod[lnSubsc,2])
lcCmpStat = laRpCmpCod[lnSubsc,5]
llLinkGL  = IIF(gfGetMemVar('M_Link_GL',lcRpComp)='Y',.T.,.F.)
IF lcCmpStat = 'A'
  lcHCmpCod = GfGetMemVar('M_COMP_ID',lcRpComp)
  lnSubsc = ASCAN(laRpCmpNo,lcHCmpCod)-1
  lcHCmpDir = ALLTRIM(laRpCmpCod[lnSubsc,2])
ENDIF

*-- Add GLDIST file to Transaction array.

= lfAdGlDist()

*-- Declare arrays to holed needed files to open for each transaction.
*-- Declare array laNeedFls to needed files with 9 cols.
*-- col 1 = Transaction name
*-- col 2 = File full path and name
*-- col 3 = file index
*-- col 4 = alias
*-- col 5 = company and file status
*-- col 6 = related file number
*-- col 7 = seek expration for the relationship
*-- col 8 = scan for expration
*-- col 9 = file purged (.T./.F.)

DECLARE laNeedFls[1,9]
lnCount = lfGetNdFls()

*-- Add Master files to needed files array.
= lfAddMsFls(lnCount)
*-- Copy Master Files.
*-- independent files
IF lcCmpStat = 'A'
  = lfCpMsFls()
ENDIF

*-- Purge Selected Transactions.
*-- dependent files
STORE '' TO lcStySeq , lcMatSeq
lnI = 1

*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16
FOR lnI = 1 TO  16 + lnAddTrkRl
  *E301816,1 ABD - [End]

  IF laTransact[lnI,3]
    DO lpPrTrn WITH laTransact[lnI,1]
  ENDIF
ENDFOR
*SET STEP ON
*-- Copy Style Inventory Journal.
*-SAB ----- [Start]
*SET STEP ON 
IF lcCmpStat = 'A'
*IF .F.
*-SAB ----- [End]
  *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
*!*	  IF FILE(lcACmpDir+'styinvjl.dbf') .AND. FILE(lcHCmpDir+'styinvjl.dbf');
*!*	     .AND. FILE(lcHCmpDir+'style.dbf')
  IF FILE(lcHCmpDir+'style.dbf')
  *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
    IF !EMPTY(lcStySeq)
      =lfPurgStJl(lcStySeq)
    ENDIF
  ENDIF
ENDIF
*SET STEP ON  
*-- Copy Fabric Inventory Journal.
*-SAB ----- [Start]
*IF lcCmpStat = 'A'
IF .F.
*-SAB ----- [End]
  IF FILE(lcACmpDir+'matinvjl.dbf') .AND. FILE(lcHCmpDir+'matinvjl.dbf');
     .AND. FILE(lcHCmpDir+'fabric.dbf')
    IF !EMPTY(lcMatSeq)
      =lfPurgMtJl(lcMatSeq)
    ENDIF
  ENDIF
ENDIF


*-SAB ----- [Start]
*- Call Style Master Purge Program
*SET STEP ON 
IF llRpPrgMstr
*SET STEP ON 

  =lfPurgStyMstr()
ENDIF
*-SAB ----- [End]

*-- Run the rebalance routine.
=lfRunRebal(lcRpComp)
IF lcCmpStat = 'A'
  =lfRunRebal(lcHCmpCod)
ENDIF
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
ldRpPrDt = ldOldPDate 
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*B607933,1 TMI [Start] be sure to close the thermometer
*E303220,1 TMI 08/29/2012 [Start]
*=gfThermo(0,0,'Closing .. ',' ')
*E303220,1 TMI 08/29/2012 [End  ]
*B607933,1 TMI [End  ]

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	=gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Purge is complete.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Purge ,.F.,LANG_Purge_Complete)
*XXX
*!*	=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge,loFormSet.GetHeaderText("LANG_Purge",loFormSet.HeaderAlias)) ,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge_Complete,loFormSet.GetHeaderText("LANG_Purge_Complete",loFormSet.HeaderAlias)))
*!*	*N000682,1 11/20/2012 MMT Globlization changes[End]

*!*	*N000682,1 04/16/2013 HES Globlization changes[End  ]

*!*	*-SAB ----- Add log to purge program [Start]
*!*	*- Call Log Screen
*!*	IF llhasLog
*!*	  lcErrorTxt = FILETOSTR(lcTmpLogFile)
*!*	  CREATE CURSOR TMPSTR (mStrRep M(10))
*!*	  SELECT TMPSTR
*!*	  APPEND BLANK
*!*	  REPLACE mStrRep WITH REPLICATE('*',55) + CHR(13) +;
*!*	                       "*                             Purge log                                                   *" + CHR(13) +;
*!*	                       REPLICATE('*',55) + CHR(13) + ' ' + CHR(13)

*!*	  REPLACE mStrRep WITH mStrRep+lcErrorTxt IN TMPSTR
*!*	  SELECT TMPSTR
*!*	  DO FORM (oAriaApplication.ScreenHome+'SMERRLOG') WITH 'Purge Log'
*!*	  USE IN TMPSTR
*!*	  llhasLog = .F.
*!*	ENDIF
*!*	ERASE (lcTmpLogFile)
*XXXX
*-SAB ----- Add log to purge program [End]

*--- end of Purge Program.

*!*************************************************************
*! Name      : lfAdGlDist
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Add GLDIST file to transaction array
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAdGlDist()
*!*************************************************************

FUNCTION lfAdGlDist
IF llLinkGL
  lnJ = 1
  FOR lnJ = 1 TO 16
    IF INLIST(lnJ,3,4,5,13,14,16)
      LOOP
    ENDIF
    *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
    *IF laTransact[lnJ,3]  
    IF laTransact[lnJ,3]  AND !'GLDIST,' $ laTransact[lnJ,4]
    *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
      laTransact[lnJ,4] = laTransact[lnJ,4] + 'GLDIST,'
      laTransact[lnJ,5] = laTransact[lnJ,5] + 'GLDISTNO,'
      laTransact[lnJ,6] = laTransact[lnJ,6] + 'B,'
      laTransact[lnJ,7] = laTransact[lnJ,7] + IIF(INLIST(lnJ,7,8,9,12),'1-1-,','1-,')
      laTransact[lnJ,9] = laTransact[lnJ,9] + '.T.,'
    ENDIF
  ENDFOR
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][Start]
*!*	  laTransact[01,8] = laTransact[01,8] + IIF(laTransact[01,3],'INVOICE+"IN"-,','')
*!*	  laTransact[02,8] = laTransact[02,8] + IIF(laTransact[02,3],'TRAN+"KO"-,','')
*!*	  laTransact[06,8] = laTransact[06,8] + IIF(laTransact[06,3],'CRMEMO+"RM"-,','')
*!*	  laTransact[07,8] = laTransact[07,8] + IIF(laTransact[07,3],'CTRCODE+"IA"-CTRCODE+"IP"-,','')
*!*	  laTransact[08,8] = laTransact[08,8] + IIF(laTransact[08,3],'CTRAN+"MA"-CTRAN+"MP"-,','')
*!*	  laTransact[09,8] = laTransact[09,8] + IIF(laTransact[09,3],'CUTTKT+"CT"-CUTTKT+"JP"-,','')
*!*	  laTransact[10,8] = laTransact[10,8] + IIF(laTransact[10,3],'PO+"PO"-,','')
*!*	  laTransact[11,8] = laTransact[11,8] + IIF(laTransact[11,3],'POMAT+"MO"-,','')
*!*	  laTransact[12,8] = laTransact[12,8] + IIF(laTransact[12,3],'CMFGORDNO+"MM"-CMFGORDNO+"MC"-,','')
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*!*	  laTransact[01,8] = laTransact[01,8] + IIF(laTransact[01,3],'INVOICE+"IN"-,','')
*!*	  laTransact[02,8] = laTransact[02,8] + IIF(laTransact[02,3],'TRAN+"KO"-,','')
*!*	  laTransact[06,8] = laTransact[06,8] + IIF(laTransact[06,3],'CRMEMO+"RM"-,','')
*!*	  laTransact[07,8] = laTransact[07,8] + IIF(laTransact[07,3],'CTRCODE+"IA"-CTRCODE+"IP"-,','')
*!*	  laTransact[08,8] = laTransact[08,8] + IIF(laTransact[08,3],'CTRCODE+"MA"-CTRCODE+"MP"-,','')
*!*	  
*!*	*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	  *laTransact[09,8] = laTransact[09,8] + IIF(laTransact[09,3],'CUTTKT+"CT"-CUTTKT+"JP"-,','')
*!*	   laTransact[09,8] = laTransact[09,8] + IIF(laTransact[09,3],'PO+"CT"-PO+"JP"-,','')
*!*	 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
*!*	  
*!*	  laTransact[10,8] = laTransact[10,8] + IIF(laTransact[10,3],'PO+"PO"-,','')
*!*	  laTransact[11,8] = laTransact[11,8] + IIF(laTransact[11,3],'PO+"MO"-,','')
*!*	 
*!*	 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	  *laTransact[12,8] = laTransact[12,8] + IIF(laTransact[12,3],'CMFGORDNO+"MM"-CMFGORDNO+"MC"-,','')
*!*	   laTransact[12,8] = laTransact[12,8] + IIF(laTransact[12,3],'PO+"MM"-PO+"MC"-,','')
  laTransact[01,8] = laTransact[01,8] + IIF(laTransact[01,3] AND !'INVOICE+"IN"-,' $ laTransact[01,8],'INVOICE+"IN"-,','')
  laTransact[02,8] = laTransact[02,8] + IIF(laTransact[02,3] AND !'TRAN+"KO"-,' $ laTransact[02,8],'TRAN+"KO"-,','')
  laTransact[06,8] = laTransact[06,8] + IIF(laTransact[06,3] AND !'CRMEMO+"RM"-,' $ laTransact[06,8],'CRMEMO+"RM"-,','')
  laTransact[07,8] = laTransact[07,8] + IIF(laTransact[07,3] AND !'CTRCODE+"IA"-CTRCODE+"IP"-,' $ laTransact[07,8],'CTRCODE+"IA"-CTRCODE+"IP"-,','')
  laTransact[08,8] = laTransact[08,8] + IIF(laTransact[08,3] AND !'CTRCODE+"MA"-CTRCODE+"MP"-,' $ laTransact[08,8],'CTRCODE+"MA"-CTRCODE+"MP"-,','')
  laTransact[09,8] = laTransact[09,8] + IIF(laTransact[09,3] AND !'PO+"CT"-PO+"JP"-,' $ laTransact[09,8],'PO+"CT"-PO+"JP"-,','')
  laTransact[10,8] = laTransact[10,8] + IIF(laTransact[10,3] AND !'PO+"PO"-,' $ laTransact[10,8],'PO+"PO"-,','')
  laTransact[11,8] = laTransact[11,8] + IIF(laTransact[11,3] AND !'PO+"MO"-,' $ laTransact[11,8],'PO+"MO"-,','')
  laTransact[12,8] = laTransact[12,8] + IIF(laTransact[12,3] AND !'PO+"MM"-PO+"MC"-,' $ laTransact[12,8] ,'PO+"MM"-PO+"MC"-,','')
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][End]
ENDIF
*--- end of lfAdGlDist.

*!*************************************************************
*! Name      : lfGetNdFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Needed Files.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : lfGetRFile
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : lnCount (number of needed files)
*!*************************************************************
*! Example   : =lfGetNdFls()
*!*************************************************************

FUNCTION lfGetNdFls
lnCount = 0        && Counter of needed files
lnI = 1
*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*FOR lnI = 1 TO 16  && loop for all transations

FOR lnI = 1 TO  16 + lnAddTrkRl && loop for all transations
  *E301816,1 ABD - [End]

  lnFileNo = 0     && counter of needed files for transation no lnI
  lnNFleNo = 0     && counter of not needed files for transation no lnI
  IF laTransact[lnI,3]      && if selected transactin
    *-- lnFPosFl = frist posation of file name in array
    *-- lnFPosIn = frist posation of index in array
    *-- lnFPosRF = frist posation of related file in array
    *-- lnFPosRD = frist posation of related field in array
    *-- lnFPosEx = frist posation of scan for expration in array
    *-- lnPosSt  = frist posation of file status in array
    *-- lnLPosFl = last posation of file name in array
    *-- lnLPosIn = last posation of index in array
    *-- lnLPosRF = last posation of related file in array
    *-- lnLPosRD = last posation of related field in array
    *-- lnLPosEx = last posation of scan for expration in array
    STORE 0 TO lnFPosFl , lnFPosIn , lnFPosRF , lnFPosRD , lnFPosEx
    lnPosSt = -1
    *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][Start]
    CREATE CURSOR 'IgnoreFile' (FileNO CHAR(3))
    SELECT 'IgnoreFile'
    INDEX on FILENO TAG 'IgnoreFile'
    *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][End]
    
    DO WHILE .T.
      lnLPosFl = AT(',',SUBSTR(laTransact[lnI,4],lnFPosFl+1))
      lnLPosIn = AT(',',SUBSTR(laTransact[lnI,5],lnFPosIn+1))
      lnPosSt = lnPosSt + 2
      lcFileSt = SUBSTR(laTransact[lnI,6],lnPosSt,1)
      lnLPosRF = AT(',',SUBSTR(laTransact[lnI,7],lnFPosRF+1))
      lnLPosRD = AT(',',SUBSTR(laTransact[lnI,8],lnFPosRD+1))
      lnLPosEx = AT(',',SUBSTR(laTransact[lnI,9],lnFPosEx+1))
      *-- lnMax determained the number of times we will need the file
      lnMax = IIF(lcCmpStat = 'A' .AND. lcFileSt $ 'AB', 2 , ;
              IIF(lcCmpStat = 'H' .AND. lcFileSt $ 'AD', 0 , 1))
      IF lnMax > 0
        lnCount = lnCount + 1
        lnFileNo = lnFileNo + 1
        =lfGetRFile(lnI,lnFPosRF,lnFPosRF+lnLPosRF-1,lnFPosRD,lnFPosRD+lnLPosRD-1,lnFileNo,lnCount,lnNFleNo)
      ELSE
        lnFileNo = lnFileNo + 1
        lnNFleNo = lnNFleNo + 1
        *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][Start]
        INSERT INTO 'IgnoreFile' values(STR(lnFileNo,3))
        *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][End]
      ENDIF
      lnJ = 1
      FOR lnJ = 1 TO lnMax
        DECLARE laNeedFls[lnCount,9]
        laNeedFls[lnCount,1] = laTransact[lnI,1]
        laNeedFls[lnCount,2] = IIF(lnJ=1,lcACmpDir,lcHCmpDir);
                             + SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)
        laNeedFls[lnCount,3] = SUBSTR(laTransact[lnI,5],lnFPosIn+1,lnLPosIn-1)
        laNeedFls[lnCount,4] = IIF(SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)='DEBIT','DEBIT',;
                               IIF(SUBSTR(laTransact[lnI,4],lnFPosFl+1,lnLPosFl-1)='CREDIT','CREDIT',;
                                   gfTempName()))
        laNeedFls[lnCount,5] = lcCmpStat + lcFileSt
        laNeedFls[lnCount,6] = ''
        laNeedFls[lnCount,7] = ''
        laNeedFls[lnCount,8] = STRTRAN(SUBSTR(laTransact[lnI,9],lnFPosEx+1,lnLPosEx-1),'~',',')
        *-SAB ----- [Start]
        *laNeedFls[lnCount,9] = IIF(laNeedFls[lnCount,4]$'DEBITCREDIT',.T.,;
                               IIF(!FILE(laNeedFls[lnCount,2]+'.dbf'),.T.,.F.))
        laNeedFls[lnCount,9] = IIF(laNeedFls[lnCount,4]$'DEBITCREDIT',.T.,.F.)
        *-SAB ----- [End]
        IF lnJ = 2
          IF laNeedFls[lnCount,9]
            laNeedFls[lnCount-1,9] = .T.
          ELSE
            IF laNeedFls[lnCount-1,9]
              laNeedFls[lnCount,9] = .T.
            ENDIF
          ENDIF
        ENDIF
        IF lnMax = 2 .AND. lnJ = 1
          lnCount = lnCount + 1
          lnFileNo = lnFileNo + 1
        ENDIF
      ENDFOR
      lnFPosFl = lnFPosFl + lnLPosFl
      lnFPosIn = lnFPosIn + lnLPosIn
      lnFPosRF = lnFPosRF + lnLPosRF
      lnFPosRD = lnFPosRD + lnLPosRD
      lnFPosEx = lnFPosEx + lnLPosEx
      IF !(lnFPosFl < LEN(laTransact[lnI,4]))  && if the last file of transaction
        EXIT
      ENDIF                                 && end if the last file of transaction
    ENDDO
  ENDIF                                     && end if selected transaction
ENDFOR                                      && end loop for all transactions
RETURN lnCount
*--- end of lfGetNdFls.

*!*************************************************************
*! Name      : lfGetRFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Get Related Files.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfGetNdFls
*!*************************************************************
*! Passed Parameters  : lnTrnId  (Transaction number)
*!                      lnStartR (frist posation of related file)
*!                      lnEndR   (last posation of related file)
*!                      lnStartD (frist posation of related filed)
*!                      lnEndD   (last posation of related file)
*!                      lnFleNo  (file number of this transaction)
*!                      lnLCount (file number of needed files)
*!                      lnNFleNo (number of not needed files)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfGetRFile()
*!*************************************************************

FUNCTION lfGetRFile
PARAMETER lnTrnId, lnStartR, lnEndR, lnStartD, lnEndD, lnFleNo, lnLCount, lnNFleNo
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD, lnFrsFlTrn

lnFPosR = lnStartR        && frist posation of related file
lnFPosD = lnStartD        && frist posation of related field
DO WHILE lnFPosR < lnEndR && loop for existing another file
  lnLPosR = AT('-',SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnEndR-lnStartR))
  lnLPosD = AT('-',SUBSTR(laTransact[lnTrnId,8],lnFPosD+1,lnEndD-lnStartD))
  
  *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][Start]
  IF  VAL(SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnLPosR-1))>0 AND  SEEK(STR(VAL(SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnLPosR-1)),3),'IgnoreFile')
    lnFPosR = lnFPosR + lnLPosR
    lnFPosD = lnFPosD + lnLPosD
    loop
  ENDIF
  *B611071,1 MMT 11/01/2015 Error while purging Invoice Detail module[T20151004.0007][End]
  
  lnRFleNo = VAL(SUBSTR(laTransact[lnTrnId,7],lnFPosR+1,lnLPosR-1))-lnNFleNo && file no in trans. array
  *-- file no in needed files array for this transaction
  lnFrsFlTrn = ASCAN(laneedfls,latransact[lnTrnId,1])
  lnRFleTrn = IIF(lnFrsFlTrn=0,lnLCount,ASUBSCRIPT(laNeedFls,lnFrsFlTrn,1))
  lnRCount = 1
  FOR lnK = 1 TO lnRFleNo-1
    lnRCount = lnRCount + IIF(laNeedFls[lnRFleTrn,5]$'AAB' , 2 , 1)
    lnRFleTrn = lnRFleTrn + IIF(laNeedFls[lnRFleTrn,5]$'AAB' , 2 , 1)
  ENDFOR
  IF lnFleNo-lnNFleNo > lnRCount  && if the file is a chaild to another one
    laNeedFls[lnRFleTrn,6] = laNeedFls[lnRFleTrn,6] + ALLTRIM(STR(lnLCount)) + ','
    laNeedFls[lnRFleTrn,7] = laNeedFls[lnRFleTrn,7] + ;
                            SUBSTR(laTransact[lnTrnId,8],lnFPosD+1,lnLPosD-1) + ','
  ENDIF
  lnFPosR = lnFPosR + lnLPosR
  lnFPosD = lnFPosD + lnLPosD
ENDDO
*--- end of lfGetRFile.

*!*************************************************************
*! Name      : lfAddMsFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Add Master Files to needed files array.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lnCount (number of needed files)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAddMsFls()
*!*************************************************************

FUNCTION lfAddMsFls
PARAMETER lnCount
IF lcCmpStat = 'A'
  *-- Add Codes file.
  IF FILE(lcACmpDir+'codes.dbf') .AND. FILE(lcHCmpDir+'codes.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'CODES'
      laNeedFls[lnCount+1+(lnJ),3] = 'CODES'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Scale file.
  IF FILE(lcACmpDir+'scale.dbf') .AND. FILE(lcHCmpDir+'scale.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'SCALE'
      laNeedFls[lnCount+1+(lnJ),3] = 'SCALE'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Apbanks file.
  IF FILE(lcACmpDir+'apbanks.dbf') .AND. FILE(lcHCmpDir+'apbanks.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'APBANKS'
      laNeedFls[lnCount+1+(lnJ),3] = 'BANKCODE'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Warehous file.
  IF FILE(lcACmpDir+'warehous.dbf') .AND. FILE(lcHCmpDir+'warehous.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'WAREHOUS'
      laNeedFls[lnCount+1+(lnJ),3] = 'WAREHOUS'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Whsloc file.
  IF FILE(lcACmpDir+'whsloc.dbf') .AND. FILE(lcHCmpDir+'whsloc.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'WHSLOC'
      laNeedFls[lnCount+1+(lnJ),3] = 'WHSLOC'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Dye_rel file.
  IF FILE(lcACmpDir+'dye_rel.dbf') .AND. FILE(lcHCmpDir+'dye_rel.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'DYE_REL'
      laNeedFls[lnCount+1+(lnJ),3] = 'DYE_REL'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Gl_link file.
  IF FILE(lcACmpDir+'gl_link.dbf') .AND. FILE(lcHCmpDir+'gl_link.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'GL_LINK'
      laNeedFls[lnCount+1+(lnJ),3] = 'GL_LINK'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Factor file.
  IF FILE(lcACmpDir+'factor.dbf') .AND. FILE(lcHCmpDir+'factor.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'FACTOR'
      laNeedFls[lnCount+1+(lnJ),3] = 'FACTOR'
    ENDFOR
    lnCount = lnCount + 2
  ENDIF

  *-- Add Glacchar file.
  IF FILE(lcACmpDir+'glacchar.dbf') .AND. FILE(lcHCmpDir+'glacchar.dbf')
    lnJ = 1
    DECLARE laNeedFls[lnCount+2,9]
    FOR lnJ = 1 TO 2
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      laNeedFls[lnCount+lnJ,1] = 'All'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laNeedFls[lnCount+lnJ,1] = LANG_ALL
laNeedFls[lnCount+lnJ,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      laNeedFls[lnCount+lnJ,4] = gfTempName()
      laNeedFls[lnCount+lnJ,5] = ''
      laNeedFls[lnCount+lnJ,6] = ''
      laNeedFls[lnCount+lnJ,7] = ''
      laNeedFls[lnCount+lnJ,8] = '.T.'
    ENDFOR

    lnJ = 0
    FOR lnJ = 0 TO 1
      laNeedFls[lnCount+1+(lnJ),2] = IIF(lnJ=0,lcACmpDir,lcHCmpDir) + 'GLACCHAR'
      laNeedFls[lnCount+1+(lnJ),3] = 'ACCTCODE'
    ENDFOR
  ENDIF
ENDIF
*--- end of lfAddMsFls.

*!*************************************************************
*! Name      : lfCpMsFls
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Copy Master Files to History Company.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfCpMsFls()
*!*************************************************************

FUNCTION lfCpMsFls
PRIVATE lnAlias, lnFrsFl, lnLstFl, lnI, lcLasKey, lcKey, llCopied
lnAlias = SELECT(0)
*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,'All'),1)  && the frist master file
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,LANG_ALL),1)  && the frist master file
lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))),1)  && the frist master file
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]
*-- Open master files.

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	IF !lfOpenFile('All',lnFrsFl)
*!*	  =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,"Copy master files failed.")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*IF !lfOpenFile(LANG_ALL,lnFrsFl)
IF !lfOpenFile(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias)),lnFrsFl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Purge,.F.,LANG_copy_Master_Failed)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge,loFormSet.GetHeaderText("LANG_Purge",loFormSet.HeaderAlias)),.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_copy_Master_Failed,loFormSet.GetHeaderText("LANG_copy_Master_Failed",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]

  RETURN
ENDIF
lnLstFl = ALEN(laNeedFls,1)  
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
LOCAL lnI
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
                             && the last master file
lnI = 0
FOR lnI = 0 TO (lnLstFl-lnFrsFl-1) STEP 2   && loop for master files of active company
  SELECT (laNeedFls[lnFrsFl+lnI,4])
  LOCATE
  *-- lcLasKey = key value of preivous record
  *-- lcKey = key value of current record
  *-- llCopied = flag to determined current record copied in history company or not
  lcLasKey = ''
  llCopied = .F.
  SCAN
    *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
    WAIT WINDOW "Copying Master Data...." NOWAIT
    *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
    lcKey = EVALUATE(KEY())
    IF !(lcKey == lcLasKey)  && if curr key value different to preivous key value
      llCopied = SEEK(lcKey,laNeedFls[lnFrsFl+lnI+1,4])
    ENDIF
    IF !llCopied  && if the current record not copied to history company
      SCATTER MEMO MEMVAR
      SELECT (laNeedFls[lnFrsFl+lnI+1,4])
      APPEND BLANK
      GATHER MEMO MEMVAR
      SELECT (laNeedFls[lnFrsFl+lnI,4])
    ENDIF
    lcLasKey = lcKey
  ENDSCAN
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
  SELECT (laNeedFls[lnFrsFl+lnI+1,4])
  =gfTableUpdate()
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
ENDFOR
SELECT (lnAlias)
*-- Close master files.

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	=lfClsFile('All',lnFrsFl)
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*=lfClsFile(LANG_ALL,lnFrsFl)
=lfClsFile(IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias)),lnFrsFl)
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	WAIT WINDOW 'Copy Master files Complete' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_copy_Master_Complete NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_copy_Master_Complete,loFormSet.GetHeaderText("LANG_copy_Master_Complete",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]
*--- end of lfCpMsFls.

*!*************************************************************
*! Name      : lpPrTrn
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Purge Transactions
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : DO lpPrTrn
*!*************************************************************

PROCEDURE lpPrTrn
PARAMETER lcTrn
PRIVATE lnFrsFl , lnI


lnFrsFl = ASUBSCRIPT(laNeedFls,ASCAN(laNeedFls,lcTrn),1) && frist file of transaction
*-- Open needed files.
IF !lfOpenFile(lcTrn,lnFrsFl)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  =gfModalGen("TRM00000B00000","DIALOG","Purge",.F.,'Transaction '+lcTrn+' failed.')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen("TRM00000B00000","DIALOG",LANG_Purge ,.F.,LANG_Transaction+lcTrn+LANG_failed)
=gfModalGen("TRM00000B00000","DIALOG",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge,loFormSet.GetHeaderText("LANG_Purge",loFormSet.HeaderAlias)) ,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transaction,loFormSet.GetHeaderText("LANG_Transaction",loFormSet.HeaderAlias))+lcTrn+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_failed,loFormSet.GetHeaderText("LANG_failed",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  RETURN
ENDIF

lnAlias = SELECT(0)

*B606114,1 AMH we start calculate STYINVJL after Purge in case of [Start]
*B606114,1     AVARGE or STANDERD :
*B606114,1     we purge and all remaining record we calculate to get STK's
*B606114,1     LIFO ,FIFO :
*B606114,1     we will delete only all records with balance of STK's
*B606114,1     equal zero that has the same session Number
*B606114,1     we comment this code to purge first and call lfApSyPhIv Function to calculate STK's
*IF lcTrn = 'Style Inventory Adjustments'
*  SELECT (laNeedFls[lnFrsFl,4])
*  *-- append style physical invtory record to reflects the total balance of the purged records.
*  =lfApStPhIv(lnFrsFl)
*ENDIF
*--check for choosing purge "Style Inventory Adjustments" or purge other

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*llStycheck = IIF(lcTrn = 'Style Inventory Adjustments',lcStyCostM $'AS',.T.)
llStycheck = IIF(lcTrn = 'Style Inventory Journal',lcStyCostM $'AS',.T.)
*B608475,1 MHM [End]

*B606114,1 AMH [End]

*B604743,1 MHM 08/15/2001 we start calculate MATINVJL after Purge in case of  [Start]
*B604743,1                AVARGE or STANDERD :
*B604743,1                we purge and all remaning record we calculate to get ON HAND
*B604743,1                LIFO ,FIFO,LOT :
*B604743,1                we will delete only all records with balance of ON HAND
*B604743,1                equal zero that has the same session Number
*B604743,1 we comment this code to purge first and call lfApFaPhIv Function to calculate ONHAND
*IF lcTrn = 'Fabric Inventory Adjustments'
*  SELECT (laNeedFls[lnFrsFl,4])
*  *-- append fabric physical invtory record to reflects the total balance of the purged records.
*  =lfApFaPhIv(lnFrsFl)
*ENDIF
*--check for choosing purge "Fabric Inventory Adjustments" or purge other

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*llcheck = IIF(lcTrn = 'Fabric Inventory Adjustments',lcCstMth $'AS',.T.)
llcheck = IIF(lcTrn = 'Fabric Inventory Journal',lcCstMth $'AS',.T.)
*B608475,1 MHM [END]


*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]

*SET STEP ON 
IF lcTrn = 'Purge Rolls'
  llcheck = .F.
  *-- scan file for purged records
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  WAIT WINDOW 'Purge Transaction '+lcTrn NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Purge_Transaction +lcTrn NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge_Transaction,loFormSet.GetHeaderText("LANG_Purge_Transaction",loFormSet.HeaderAlias)) +lcTrn NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  SELECT (laNeedFls[lnFrsFl,4])
  = lfPurgRols (lnFrsFl)
ENDIF
*E301816,1 ABD - [End]

*B040155,1  TMI [Start] Define a pre-purge functions to be run for each set of transactions befor purge process
*                       This includes
*                         1)  Add a summrization line for previouse transactions
*                         2)  for a specific file, create a temp table add in it records that can not be purged
*                             and befor purging any record for that table check from within lfScanFile that this record is
*                             purgable (i.e not found in this temp file)
DO lpPrePurge WITH lcTrn
*B040155,1  TMI [End  ]
*   SET STEP ON 
*B606114,1 AMH Don't purge style inventory adjustments with FIFO or LIFO costing methodes [Start]
*IF llcheck
IF llcheck .AND. llStycheck
*B606114,1 AMH [End]

*B604743,1 MHM 08/15/2001 [End]

  lnI = lnFrsFl
  DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
*   SET STEP ON 
    IF !laNeedFls[lnI,9]  && if the file not purged
      SELECT (laNeedFls[lnI,4])
      LOCATE
      *-- scan file for purged records
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	      WAIT WINDOW 'Purge Transaction '+lcTrn NOWAIT
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Purge_Transaction +lcTrn NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge_Transaction,loFormSet.GetHeaderText("LANG_Purge_Transaction",loFormSet.HeaderAlias)) +lcTrn NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]
*SET STEP ON 
      *N000682,1 04/16/2013 HES Globlization changes[End  ]
      = lfScanFile(lnI,.F.,'')
    ENDIF
    lnI = lnI + 1
    IF lnI > ALEN(laNeedFls,1)
      EXIT
    ENDIF
  ENDDO

*B604743,1 MHM 08/15/2001 we move lfApFaPhIv to calculate after purge program [Start]
ENDIF

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*IF lcTrn = 'Fabric Inventory Adjustments'
*-SAB ----- [Start]
*IF lcTrn = 'Fabric Inventory Journal'
IF .F.
*-SAB ----- [End]
*B608475,1 MHM [End]

  SELECT (laNeedFls[lnFrsFl,4])
  *-- append fabric physical invtory record to reflects the total balance of the purged records.
  =lfApFaPhIv(lnFrsFl)
ENDIF
*B604743,1 MHM 08/15/2001 [End]

*B606114,1 AMH we move lfApSyPhIv to calculate after purge program [Start]

*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*IF lcTrn = 'Style Inventory Adjustments'
*-SAB ----- [Start]
IF lcTrn = 'Style Inventory Journal'
*IF .F.
*-SAB ----- [End]
*B608475,1 MHM [End]

  SELECT (laNeedFls[lnFrsFl,4])
  *-- append style adjustment inventory record to reflects the total balance of the purged records.
  =lfApStPhIv(lnFrsFl)
ENDIF
*B606114,1 AMH [End]


*-SAB ----- [Start]
*- Call the gfTableUpdate()
*SET STEP ON
=lfUpdateFiles(lcTrn,lnFrsFl)
*-SAB ----- [End]
SELECT (lnAlias)
*SET STEP ON 
*-- Pack needed files.
=lfPackFile(lcTrn,lnFrsFl)

*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	WAIT WINDOW 'Transaction '+lcTrn+' Complete' NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Transaction +lcTrn+LANG_Complete NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Transaction,loFormSet.GetHeaderText("LANG_Transaction",loFormSet.HeaderAlias)) +lcTrn+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Complete,loFormSet.GetHeaderText("LANG_Complete",loFormSet.HeaderAlias)) NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[End  ]

*-- end of lpPrTrn.

*!*************************************************************
*! Name      : lfOpenFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Open Needed Files of transaction
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : .T. (all files opend)/.F. (error occur)
*!*************************************************************
*! Example   : =lfOpenFile()
*!*************************************************************

FUNCTION lfOpenFile
PARAMETER lcTrn, lnFrsFl
LOCAL lnFleCount, lnI, lnJ
lnFleCount = ALEN(laNeedFls,1)
lcAllFiles = ''
lnI = lnFrsFl
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1
  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos)
  lcFileStat = IIF(lcFileName$lcAllFiles,'SH','EX')
  lcAllFiles = lcAllFiles + IIF(lcFileStat='EX',lcFileName+'\','')
  lcAlias = laNeedFls[lnI,4]
  *-SAB ----- [Start]
  *IF FILE(laNeedFls[lnI,2]+'.dbf')
  IF !EMPTY(laNeedFls[lnI,2])
  *-SAB ----- [End]
    *E303220,1 TMI 08/29/2012 [Start]
    *IF !gfOpenFile(laNeedFls[lnI,2], laNeedFls[lnI,3], lcFileStat , @lcAlias , .T.)
    *-SAB ----- [Start]
    *IF !gfOpenTable(laNeedFls[lnI,2], laNeedFls[lnI,3], lcFileStat , @lcAlias , .T.)
    IF !gfOpenTable(laNeedFls[lnI,2], laNeedFls[lnI,3], lcFileStat , @lcAlias , .T., lfGetComp(laNeedFls[lnI,2]))
    *-SAB ----- [End]
      *E303220,1 TMI 08/29/2012 [End  ]
      lnJ = 1
      FOR lnJ = lnFrsFl TO lnI - 1
        =gfCloseTable(laNeedFls[lnI,4])
      ENDFOR
      RETURN .F.
    ENDIF
    *-SAB ----- [Start]
    IF !lfIsNative(lcFileName)
      SELECT (lcAlias)
      =gfSeek('')
    ENDIF
    *-SAB ----- [End]
  ENDIF
  *B127944,1  TMI [Start] set filter on "NOTEPAD" file for types not one of the following
  *-  {Account Number,Style Number,Item Number,Vendor Number,Salesrep Code,User Name,Template}
  *-  to deny purging notes for these types since it is a master files data not transactions.
  IF 'NOTEPAD'$DBF()
    SET FILTER TO !(TYPE$'AFGHJUT')
    GO TOP
  ENDIF
  *B127944,1  TMI [End  ]
  laNeedFls[lnI,4] = lcAlias
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
RETURN .T.
*--- end of lfOpenFile.

*!*************************************************************
*! Name      : lfClsFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Close Needed Files of transaction
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfClsFile()
*!*************************************************************

FUNCTION lfClsFile
PARAMETER lcTrn, lnFrsFl
PRIVATE lnFleCount, lnI
lnFleCount = ALEN(laNeedFls,1)
lnI = lnFrsFl
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  =gfCloseTable(laNeedFls[lnI,4])
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
*--- end of lfClsFile.

*!*************************************************************
*! Name      : lfScanFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Scan File for Purge records
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnPos      (file number)
*!                      llRelated  (file is chaild)
*!                      lcKeyValue (key value of previous rec.)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfScanFile()
*!*************************************************************

FUNCTION lfScanFile

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*PARAMETER lnPos , llRelated , lcKeyValue
lPARAMETER lnPos , llRelated , lcKeyValue
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

PRIVATE lnPos, llRelated, lcKeyValue
PRIVATE lcLasKey, lcKey, llCopied
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD
SET STEP ON 
*-- lcLasKey = key value of previous record
*-- lcKey = key value of current record
*-- llCopied = current record copied to history company
lcLasKey = lcKeyValue
*-- if the file status not (append or both in active company) , llCopied = .F.
*B126198,1 ASH 02/09/2005 (Begin) Fix the bug 'Alias not found' if MF is not installed.
*llCopied = IIF(laNeedFls[lnPos,5]$'AAB',SEEK(lcLasKey,laNeedFls[lnPos+1,4]),.F.)
llCopied = IIF(laNeedFls[lnPos,5]$'AAB' AND !EMPTY(lcLasKey),SEEK(lcLasKey,laNeedFls[lnPos+1,4]),.F.)
*B126198,1 ASH 02/09/2005 (End)


IF RIGHT(laNeedFls[lnPos,2],7) = 'CUTPICK'
*SET STEP ON  
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
IF left(laNeedfls[lnPos,5],1)  ='H'
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  IF laNeedFls[lnPos,3] = 'CUTPICK'
    lnOrdPos = lnPos + IIF(laNeedFls[lnPos,5]='AB',3,2)
    llHasOrd = SEEK("O"+ORDER,laNeedFls[lnOrdPos,4])
    laNeedFls[lnPos,5] = IIF(llHasOrd,IIF(laNeedFls[lnPos,5]='AB','AA','HA'),;
                         IIF(llCopied,'AD',laNeedFls[lnPos,5]))
  ELSE
    lnCktkPos = lnPos + IIF(laNeedFls[lnPos,5]='AB',4,IIF(trancd='1',2,3))
    *B126198,1 ASH 02/09/2005 (Begin) Fix the bug 'Alias not found' if MF is not installed.
    IF !('MF' $ oAriaApplication.CompanyInstalledModules)
      lnCktkPos = lnCktkPos + 2
    ENDIF
    *B126198,1 ASH 02/09/2005 (End)
   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
    *llHasCktk = SEEK(IIF(trancd='1','','P')+CTKTNO,laNeedFls[lnCktkPos,4])
     llHasCktk = SEEK(IIF(trancd='1','PU','PP')+CTKTNO,laNeedFls[lnCktkPos,4])
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
    laNeedFls[lnPos,5] = IIF(llHasCktk,IIF(laNeedFls[lnPos,5]='AB','AA','HA'),;
                         IIF(llCopied,'AD',laNeedFls[lnPos,5]))
  ENDIF
  llCopied = .F.
ENDIF
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
ENDIF 
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][Start]
lnReccount=0
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][End]
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
LOCAL lcScanExpFull,lcScanFileName ,lcOrgFileName 
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]

*B611938,1 ES 11/25/2019 Purge Program takes long time to purge data [Start]
*SCAN REST FOR EVALUATE(laNeedFls[lnPos,8])
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
LOCAL lcTmpFile
lcTmpFile = gfTempName()
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
lcScanFileName = ALIAS()
lcOrgFileName = ALIAS()
lcScanExpFull = ""
IF EMPTY(lcKeyValue)
  lcKeyExpFields = KEY()
  lcSortFieldList = STRTRAN(lcKeyExpFields ,'+',',')
  lcCondiTionExp = laNeedFls[lnPos,8]
  IF ')' $ lcKeyExpFields 
    SELECT * FROM (lcOrgFileName) WHERE &lcCondiTionExp. INTO CURSOR (lcTmpFile) READWRITE
    SELECT (lcTmpFile)
    INDEX ON (lcKeyExpFields) TAG (lcTmpFile)
    LOCATE
  ELSE
    SELECT * FROM (lcOrgFileName) WHERE &lcCondiTionExp. INTO CURSOR (lcTmpFile) ORDER BY &lcSortFieldList.
  ENDIF 
  *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
  SELECT (lcOrgFileName)
  SET FILTER TO &lcCondiTionExp. 
  LOCATE 
  *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]
  SELECT (lcTmpFile)
  SET RELATION TO &lcKeyExpFields. INTO (lcOrgFileName) ADDITIVE 
  LOCATE
  lcScanFileName = lcTmpFile
ELSE
  lcKeyExpFields = KEY()
  lcScanExpFull = "REST While "+lcKeyExpFields + " ='"+lcKeyValue+"' FOR EVALUATE(laNeedFls[lnPos,8])"  
  lcScanFileName = ALIAS()
ENDIF
SELECT (lcScanFileName)
SCAN &lcScanExpFull.
  
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
  IF !llRelated 
    REPLACE nPerc WITH nPerc+1 IN PurgProg 
    REPLACE Value WITH IIF(CEILING((nPerc/nHeadCnt) * 100)>100,100,CEILING((nPerc/nHeadCnt) * 100)) IN PurgProg 
  ENDIF
  *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
  
  SELECT (lcOrgFileName)
*B611938,1 ES 11/25/2019 Purge Program takes long time to purge data [End]

  lcKey = EVALUATE(KEY())
*SET STEP ON 
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  WAIT WINDOW 'Purge file ' + laNeedFls[lnPos,2] + ' rec. ' + lcKey NOWAIT
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Purge_file + laNeedFls[lnPos,2] + LANG_record + lcKey NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Purge_file,loFormSet.GetHeaderText("LANG_Purge_file",loFormSet.HeaderAlias)) + laNeedFls[lnPos,2] + IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_record,loFormSet.GetHeaderText("LANG_record",loFormSet.HeaderAlias)) + lcKey NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]

  *-- if key value of current rec. different key value of previous rec.
 
 IF !(lcKey == lcLasKey) .AND. IIF(llRelated , !(lcKey = lcLasKey) , .T.)
    IF llRelated
      RETURN
    ENDIF
    llCopied = IIF(laNeedFls[lnPos,5]$'AAB',SEEK(lcKey,laNeedFls[lnPos+1,4]),;
               IIF(laNeedFls[lnPos,5]='HB',.F.,.T.))
  ENDIF
  *-SAB ----- [Start]
  *IF !llCopied   && if record not copied to history company
*SET STEP ON 
  IF !llCopied OR (llRpAppTHst AND llCopied)
  *-SAB ----- [End]
    IF laNeedFls[lnPos,5]$'AAB'  && if file status is append or both in active company
      SCATTER MEMO MEMVAR
      SELECT (laNeedFls[lnPos+1,4])
      *-SAB ----- [Start]
      *APPEND BLANK
      *GATHER MEMO MEMVAR
      IF !llCopied
        =gfAppend('', .T.)
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][Start]
    lnReccount=lnReccount+1
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][End]
      ELSE
        GATHER MEMO MEMVAR
        =gfReplace('')
      ENDIF
      *-SAB ----- [End]

      *-SAB ----- Add log to purge program [Start]
      *laNeedFls[lnPos,8]

      IF llRpIgnrStat
        llRpIgnrStat = .F.
        IF !EVALUATE(laNeedFls[lnPos,8])
          STRTOFILE('File ' + laNeedFls[lnPos,2] + ' rec. ' + lcKey + CHR(13)+CHR(10), lcTmpLogFile,1)

          llhasLog = .T.
        ENDIF
        llRpIgnrStat = .T.
      ENDIF
      *-SAB ----- Add log to purge program [End]

      *-- Put Zero value for all numeric fields of STYLE , STYDYE , FABRIC , FABDYE
      =lfZeroUpdt(lnPos)
      SELECT (laNeedFls[lnPos,4])
    ENDIF  && end if file status is append or both in active company
    IF laNeedFls[lnPos,5]$'AUHU' && if file status is update
      lnUPos = RAT('\',laNeedFls[lnPos,2]) + 1    && get file name posation
      lcUFile = SUBSTR(laNeedFls[lnPos,2],lnUPos) && get file name
      *-- lcFieldU get fields to update
      lcFieldU = IIF(laNeedFls[lnPos,1] ='Sales Order Details','ORD',;
                 IIF(laNeedFls[lnPos,1] ='Picking Tickets','PIK','CUT'))
      *-- update tot field
      REPLACE TOT&lcFieldU. WITH TOT&lcFieldU. - laUpdate[9]
      *-- update 8 size fields for line files only (CUTTKTL,POSLN,ORDLINE)
      IF lcUFile = 'CUTTKTL' .OR. lcUFile = 'POSLN' .OR. lcUFile = 'ORDLINE'
        FOR lnZ = 1 TO 8
          lcZ = ALLTRIM(STR(lnZ))
          lcFieldUp = lcFieldU + lcZ
          REPLACE &lcFieldUp. WITH &lcFieldUp. - laUpdate[lnZ]
        ENDFOR
      ENDIF
    ENDIF  && end if file status is update
    IF !EMPTY(laNeedFls[lnPos,6]) && if the file has related files
      *-- lnFPosR = frist posatoin of related file number
      *-- lnFPosD = frist posation of seek expration of related file
      *-- lnLPosR = last posation of related file number
      *-- lnLPosD = last Posation of seek expration of related file
      *-- lnRPos = the number or related file
      *-- lcField = seek expration of related file
      *-- laUpdate array to hold the updated qtys
      STORE 0 TO lnFPosR , lnFPosD
      DO WHILE lnFPosR < LEN(laNeedFls[lnPos,6])  && loop for existing file
        lnLPosR = AT(',',SUBSTR(laNeedFls[lnPos,6],lnFPosR+1))
        lnRPos = VAL(SUBSTR(laNeedFls[lnPos,6],lnFPosR+1,lnLPosR-1))
        lnLPosD = AT(',',SUBSTR(laNeedFls[lnPos,7],lnFPosD+1))
        IF laNeedFls[lnRPos,5] <> 'HN'
          *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
          SELECT (laNeedFls[lnPos,4])
          *E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
          lcField = EVALUATE(SUBSTR(laNeedFls[lnPos,7],lnFPosD+1,lnLPosD-1))
          IF SEEK(lcField,laNeedFls[lnRPos,4]) && get the related rec. in related file
            IF laNeedFls[lnRPos,5]$'AUHU'  && if file status is update
              DECLARE laUpdate[9]
              SCATTER FIELDS qty1,qty2,qty3,qty4,qty5,qty6,qty7,qty8,Totqty TO laUpdate
            ENDIF  && end if file status is update
            IF !laNeedFls[lnRpos,9]
              SELECT (laNeedFls[lnRPos,4])
              = lfScanFile(lnRPos,.T.,lcField) && scan related file
            ENDIF
            SELECT (laNeedFls[lnPos,4])
          ENDIF  && end get the related rec. in related file
        ENDIF
        lnFPosR = lnFPosR + lnLPosR
        lnFPosD = lnFPosD + lnLPosD
      ENDDO
    ENDIF  && end if the file has related files
    *-- if the file status is delete or both in active company or both in history company
    
    IF laNeedFls[lnPos,5]$'ADABHB'
      *-SAB ----- [Start]
      *DELETE
      =gfDelete()
      *-SAB ----- [End]
    ENDIF
    *-- if the file status is N in active company
    IF laNeedFls[lnPos,5]$'AN'
      IF laNeedFls[lnPos,1]='Material Purchase Order' .OR. ;
         laNeedFls[lnPos,1]='Material Manufacturing Order'
        
         *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
         *lcMatSeq = MAX(ctrn_seq,lcMatSeq)
          lcMatSeq = MAX(CSESSION,lcMatSeq)
         *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
      ELSE
        lcStySeq = MAX(csession,lcStySeq)
      ENDIF
    ENDIF
  ENDIF  && end if record not copied to history company
  lcLasKey = IIF(llRelated,lcLasKey,lcKey)
  

*B611938,1 ES 11/25/2019 Purge Program takes long time to purge data [Start]
  SELECT (lcScanFileName)
*B611938,1 ES 11/25/2019 Purge Program takes long time to purge data [End]
  
ENDSCAN


  *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
  SELECT (lcOrgFileName)
  SET FILTER TO 
  LOCATE 
  *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]

*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	  STRTOFILE(CHR(13)+CHR(10)+ "Table : "+laNeedFls[lnPos,2] + " rec#: " + STR(lnReccount) + CHR(13)+CHR(10),'D:\Shared\PurgedLog.txt' ,1)
*B611797,1 ES 09/25/2019 Performance Issue / Data purge [T20171010.0007][End]

IF !llRelated
  laNeedFls[lnPos,9] = .T.  && file is purged
  IF laNeedFls[lnPos,5]$'AAB'  && if file status is append or both in active company
    laNeedFls[lnPos+1,9] = .T.  && the file of history company is purged
  ENDIF
  =lfSkipFile(lnPos)
ENDIF
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
lfClearMemory()
SYS(1104)
DIMENSION laPrgTemps[1]
laPrgTemps = ''
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*--- end of lfScanFile.

*!*************************************************************
*! Name      : lfApStPhIv
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Append Style Physical Inventory reflects the
*!             total balance of the purged records.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnCurFile (file to append physical rec)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfApStPhIv()
*!*************************************************************

FUNCTION lfApStPhIv
PARAMETER lnCurFile
set step on
*B606114,1 AMH Comment lfApSyPhIv FUNCTION because we change the  way we purge [Start]
*lcGLFYear  = SPACE(04)   && Fiscal Year
*lcGLPeriod = SPACE(02)   && Period
*lcTmpFile = gfTempName() && temp name for dbf file hold adjust qtys
*IF laNeedFls[lnCurFile,5]$'AAB'
*  SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,;
*         cIRType,cRSession,cISession,cAdjreason,;
*         SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
*         SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
*         SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*         SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal',;
*         SEEK(style+cwarecode+csession;
*         +DTOS(dtrdate)+ctrcode+STR(lineno,6),laNeedFls[lnCurFile+1,4]) AS 'lCopied' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRDATE <= ldRpPrDt .AND. CTRTYPE$"12";
*  GROUP BY &laNeedFls[lnCurFile,4]..Style,&laNeedFls[lnCurFile,4]..cWareCode,;
*           &laNeedFls[lnCurFile,4]..cDyelot ;
*  HAVING !lCopied ;
*  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
*ELSE
*  SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,;
*         cIRType,cRSession,cISession,cAdjreason,;
*         SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',;
*         SUM(nStk4) AS 'nStk4',SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',;
*         SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*         SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRDATE <= ldRpPrDt .AND. CTRTYPE$"12";
*  GROUP BY &laNeedFls[lnCurFile,4]..Style,&laNeedFls[lnCurFile,4]..cWareCode,;
*           &laNeedFls[lnCurFile,4]..cDyelot ;
*  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
*ENDIF

*lnDirLn = RAT('\',laNeedFls[lnCurFile,2])  && get the length of path of company
*lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)  && get the path of company
**-- open needed files
*=gfOpenFile(lcDataDir+'STYLE','STYLE','SH')
*=gfOpenFile(lcDataDir+'STYDYE','STYDYE','SH')
*IF llLinkGL && if linked with GL
*  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
*  SELECT GLDIST
*  =AFIELDS(laGlData)
*  lcGLDTemp  = gfTempName()
*  CREATE TABLE oAriaApplication.WorkDir+(lcGLDTemp) FROM ARRAY laGlData
*ENDIF
*lcCurCmp = oAriaApplication.ActiveCompanyID  && save the current company code
*lcCurDir = oAriaApplication.DataDir   && save the current data dir
*oAriaApplication.ActiveCompanyID = lcRpComp
*oAriaApplication.DataDir = lcDataDir
*=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','IA',.T.)
*oAriaApplication.DataDir = lcCurDir  && restore the current company code
*oAriaApplication.ActiveCompanyID = lcCurCmp && restore the current company code
*SELECT (lcTmpFile)
*LOCATE
*DECLARE laAdjust[9]
*DIMENSION laOldStk[11]
*STORE 0 TO laOldStk
*laOldStk[11] = ldRpPrDt+1
*lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1) && get invtadj file number
*SCAN
*  *-- append physical balance in invtadj
*  SELECT (laNeedFls[lnNextFile,4])
*  APPEND BLANK
*  REPLACE Style     WITH &lcTmpFile..Style,;
*          cReason   WITH "Physical balance of Purge",;
*          Date      WITH ldRpPrDt+1,;
*          DPOSTDate WITH ldRpPrDt+1,;
*          Type      WITH 'P',;
*          cFromWare WITH &lcTmpFile..cWareCode ,;
*          Unt_Cost  WITH &lcTmpFile..nStkVal/IIF(&lcTmpFile..nTotStk=0,1,&lcTmpFile..nTotStk),;
*          Old_Cost  WITH 0
*
*  FOR lnZ=1 TO 8
*    lcZ = STR(lnZ,1)
*    REPLACE Adj&lcZ    WITH &lcTmpFile..nStk&lcZ.,;
*            OldQty&lcZ WITH laoldStk(lnZ)
*  ENDFOR
*
*  IF llLinkGL
*    lcLinkCode=IIF(SEEK(&lcTmpFile..Style+&lcTmpFile..cWareCode+&lcTmpFile..cDyelot,'STYDYE'),;
*               IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF'),'DEFDEF')
*    REPLACE LINK_CODE WITH lcLinkCode,;
*            GLFYEAR   WITH lcGLFYear,;
*            GLPERIOD  WITH lcGLPeriod
*  ENDIF
*
*  REPLACE TotAdj     WITH &lcTmpFile..nTotStk,;
*          TotOld     WITH laOldStk(9) &&,;
*          dAdd_Date  WITH oAriaApplication.SystemDate,;
*          cAdd_Time  WITH TIME(),;
*          cAdd_User  WITH "SMPURGE"
*  *--G/L Array difinition and initialization.
*  IF llLinkGL
*    DECLARE laGLDistAr[2,13]
*    laGLDistAr[1,1] = lcLinkCode
*    laGLDistAr[2,1] = lcLinkCode
*    laGLDistAr[1,2] = '006'
*    laGLDistAr[2,2] = '007'
*    laGLDistAr[1,3] = 1
*    laGLDistAr[2,3] = -1
*    STORE 'IA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
*    STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
*    STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
*    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
*    STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
*    STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
*    laGLDistAr[2,10] = ''
*  ELSE
*    DIME laGLDistAr[1,1]
*    laGLDistAr = ''
*  ENDIF
*  SELECT (lcTmpFile)
*  SCATTER FIELDS &lcTmpFile..nStk1,&lcTmpFile..nStk2,&lcTmpFile..nStk3,;
*                 &lcTmpFile..nStk4,&lcTmpFile..nStk5,&lcTmpFile..nStk6,;
*                 &lcTmpFile..nStk7,&lcTmpFile..nStk8,&lcTmpFile..nTotStk TO laAdjust
*  lnI = 1
*  FOR lnI = 1 TO 9
*    laAdjust[lnI] = laAdjust[lnI] * 2
*  ENDFOR
*  lcCurCmp = oAriaApplication.ActiveCompanyID  && save the current company code
*  lcCurDir = oAriaApplication.DataDir   && save the current data dir
*  oAriaApplication.ActiveCompanyID = lcRpComp
*  oAriaApplication.DataDir = lcDataDir
*  =gfStyCrl('2',Style,cWareCode,cDyelot,ldRpPrDt+1,'',@laAdjust,;
*            nStkVal/IIF(nTotStk=0,1,nTotStk),'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
*  oAriaApplication.DataDir = lcCurDir  && restore the current company code
*  oAriaApplication.ActiveCompanyID = lcCurCmp && restore the current company code
*  SELECT (laNeedFls[lnCurFile,4])
*  lcOrder = SET('ORDER')  && save the order setting of styinvjl
*  SET ORDER TO            && remove order
*  GO BOTTOM               && get the physical last rec.
*  REPLACE cAdjreason WITH &lcTmpFile..cAdjreason
*  lcSession = cSession    && get session number
*  SELECT (laNeedFls[lnNextFile,4])
*  REPLACE cSession WITH lcSession  && save session number in invtadj
*  SELECT (laNeedFls[lnCurFile,4])
*  SET ORDER TO &lcOrder.  && restore the order setting
*ENDSCAN
*
**-- close files
*IF USED(lcTmpFile)
*  USE IN (lcTmpFile)
*ENDIF
*IF llLinkGl .AND. USED(lcGLDTemp)
*  USE IN (lcGLDTemp)
*ENDIF
*IF USED('GLDIST')
*  USE IN GLDIST
*ENDIF
*IF USED('STYDYE')
*  USE IN STYDYE
*ENDIF
*IF USED('STYLE')
*  USE IN STYLE
*ENDIF
**--Erase the temp. file.
*ERASE (oAriaApplication.WorkDir+lcTmpFile+'.DBF')
*IF llLinkGl
*  ERASE (oAriaApplication.WorkDir+lcGLDTemp+'.DBF')
*ENDIF

*--we start calculate STYINVJL after Purge in case of
*--AVARGE or STANDERD :
*--                   we purge all remaning record we calculate to get STK's
*--LIFO ,FIFO :
*--            we will delete only all records with balance of STK's
*--            equal zero that has the same session Number

*--save link check variable
llStorLink = llLinkGL
llLinkGL = .F.
lcGLFYear  = SPACE(04)  && Fiscal Year
lcGLPeriod = SPACE(02)  && Period
lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys
llWareHous = gfGetMemVar('M_WareHouse',lcRpComp)= 'Y'
lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
*E303220,1 TMI 08/29/2012 [Start]
*=gfOpenFile(lcDataDir+'STYLE','STYLE','SH')
*=gfOpenFile(lcDataDir+'STYDYE','STYDYE','SH')
*B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
*!*	=gfOpenTable(lcDataDir+'STYLE','STYLE','SH')
*!*	=gfOpenTable(lcDataDir+'STYDYE','STYDYE','SH')
=gfOpenTable('STYLE','STYLE','SH','STYLE',.T.,lcRpComp)
=gfOpenTable('STYDYE','STYDYE','SH','STYDYE',.T.,lcRpComp)
*B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]
*E303220,1 TMI 08/29/2012 [End  ]
IF llLinkGL
  *E303220,1 TMI 08/29/2012 [Start]
  *=gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
   *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
   *=gfOpenTable(lcDataDir+'GLDIST','GLDISTAC','SH')
   =gfOpenTable('GLDIST','GLDISTAC','SH','GLDIST',.T.,lcRpComp)
   *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]
  *E303220,1 TMI 08/29/2012 [End  ]
  SELECT GLDIST
  =AFIELDS(laGlData)
  lcGLDTemp  = gfTempName()
  CREATE TABLE oAriaApplication.WorkDir+(lcGLDTemp) FROM ARRAY laGlData
ENDIF
lcCurCmp = oAriaApplication.ActiveCompanyID
lcCurDir = oAriaApplication.DataDir
oAriaApplication.ActiveCompanyID = lcRpComp
oAriaApplication.DataDir = lcDataDir
=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','IA',.T.)
oAriaApplication.DataDir = lcCurDir
oAriaApplication.ActiveCompanyID = lcCurCmp

*B040155,1  TMI [Start] Avoid a bug in data that is some lines in stydye have empty warehouse code.
SELECT STYDYE
DELETE FOR EMPTY(CWARECODE)
GO TOP
*B040155,1  TMI [End  ]

*-in case of Costing AVARAGE OR SATANDERD
IF lcStyCostM $'AS'
  STORE '' TO lcAdReason
  SELECT STYLE
  LOCATE
  lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)
  DECLARE laAdjust[9]
  DIMENSION laOldStk[11]

  SCAN
    
    *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
    REPLACE nPerc WITH nPerc+1 IN PurgProg 
    REPLACE Value WITH IIF(CEILING((nPerc/nHeadCnt) * 100)>100,100,CEILING((nPerc/nHeadCnt) * 100)) IN PurgProg 
    *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]
     
    STORE 0 TO laOldStk
    laOldStk[11] = ldRpPrDt+1
    =SEEK(STYLE,'STYDYE')

    *B607311,1 AMH Scan only the purged styles [Start]
    lcSetDel = SET('DELETE')
    *B040155,3  TMI [Start] loop in stydye file
    *SET DELETE OFF
    llFound = .F.
    SELECT STYDYE
    SCAN REST WHILE STYLE+CWARECODE+DYELOT = STYLE.STYLE
      SET DELETE OFF
      *B040155,3  TMI [End  ]
      IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])

        *B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
        *LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
        *                  STYDYE.STYLE+STYDYE.CWARECODE;
        *            FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt .AND. CTRTYPE$"12"

        LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                          STYDYE.STYLE+STYDYE.CWARECODE;
                    FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt
       *B608475,1 MHM [END]

        IF !FOUND()
          SET DELETE &lcSetDel.
          LOOP
          *B040155,3  TMI [Start] if found exit the loop
        ELSE
          llFound = .T.
          EXIT
        *B040155,3  TMI [End  ]
        ENDIF
        *B040155,3  TMI [Start] move the following line below
      *ELSE
        *SET DELETE &lcSetDel.
        *LOOP
        *B040155,3  TMI [End  ]
      ENDIF
      SET DELETE &lcSetDel.
      *B607311,1 AMH [End]

      *B040155,3  TMI [Start] if not found skip this style
    ENDSCAN
    IF !llFound
      LOOP
    ENDIF
    SET DELETE &lcSetDel.
    *B040155,3  TMI [End  ]

    *B607311,1 AMH Move this part of code to update the INVTADJ file in case update the
    *B607311,1 AMH STYINVJL file [Start]
    *--append in INVTADJ FILE
    *SELECT (laNeedFls[lnNextFile,4])
    *APPEND BLANK
    *IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
    *  SELECT (laNeedFls[lnCurFile,4])
    *  LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
    *                    STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
    *  IF FOUND()
    *    lnUnitCost = NCOST
    *  ELSE
    *    lnUnitCost = 0
    *  ENDIF
    *  SELECT (laNeedFls[lnNextFile,4])
    *ELSE
    *  lnUnitCost = 0
    *ENDIF
    *REPLACE Style     WITH STYLE.Style,;
    *        cReason   WITH "Physical Count",;
    *        Date      WITH ldRpPrDt+1,;
    *        DPOSTDate WITH ldRpPrDt+1,;
    *        Type      WITH 'A',;
    *        cFromWare WITH STYDYE.cWareCode,;
    *        Unt_Cost  WITH lnUnitCost,;
    *        Old_Cost  WITH 0
    *
    *IF llLinkGL
    *  lcLinkCode = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF')
    *  REPLACE LINK_CODE WITH lcLinkCode,;
    *          GLFYEAR   WITH lcGLFYear,;
    *          GLPERIOD  WITH lcGLPeriod
    *ENDIF
    *
    *REPLACE dAdd_Date  WITH oAriaApplication.SystemDate,;
    *        cAdd_Time  WITH TIME(),;
    *        cAdd_User  WITH "SMPURGE"
    *B607311,1 AMH [End]

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
      STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = ''
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT STYLE

    lcCurCmp = oAriaApplication.ActiveCompanyID
    lcCurDir = oAriaApplication.DataDir
    oAriaApplication.ActiveCompanyID = lcRpComp
    oAriaApplication.DataDir = lcDataDir

    IF SEEK(STYLE,'STYDYE')
      SELECT STYDYE
      SCAN REST WHILE Style+cWareCode+Dyelot = STYLE.STYLE
        IF STYLE.cdye_flg = 'N' .OR. EMPTY(Dyelot)
          lnStyCost = IIF(llWareHous,STYDYE.AVE_COST,STYLE.AVE_COST)

          *B608475,1 MHM 03/09/2008 Recalc[Start]
          IF lcStyCostM $'A'
            lnStyCost = lfClcAvcst()
          ENDIF
          *B608475,1 MHM 03/09/2008 [End]

        ENDIF
        IF STYLE.cdye_flg = 'Y' AND EMPTY(Dyelot)
          LOOP
        ENDIF
        lnRecNo = RECNO()
        *--seek in MatInvJl to get total STK's for the same style color
        SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laAdjust
        SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldStk
        IF SEEK(Style+cWareCode,laNeedFls[lnCurFile,4])
          SELECT (laNeedFls[lnCurFile,4])
          LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                            STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
          IF FOUND()
            *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	            lcAdReason = "Physical Count"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcAdReason = LANG_Physical_Count
lcAdReason = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Physical_Count,loFormSet.GetHeaderText("LANG_Physical_Count",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 04/16/2013 HES Globlization changes[End  ]
            SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                            STYLE.STYLE+STYDYE.cWareCode FOR CDYELOT = STYDYE.DYELOT
              lnI = 1
              FOR lnI = 1 TO 8
                lcI = STR(lnI,1)
                laAdjust[lnI] = laAdjust[lnI] - EVALUATE('NSTK'+lcI)
              ENDFOR
              laAdjust[9] = laAdjust[9] - NTOTSTK
            ENDSCAN
          ENDIF
        ENDIF
        *--initialize stydye
        IF laAdjust[9] <> 0
          REPLACE STYDYE.STK1    WITH 0,;
                  STYDYE.STK2    WITH 0,;
                  STYDYE.STK3    WITH 0,;
                  STYDYE.STK4    WITH 0,;
                  STYDYE.STK5    WITH 0,;
                  STYDYE.STK6    WITH 0,;
                  STYDYE.STK7    WITH 0,;
                  STYDYE.STK8    WITH 0,;
                  STYDYE.TOTSTK  WITH 0,;
                  STYDYE.NSTKVAL WITH 0
          *--ADD a new  StyInvJl record with new STK's balance

          *B607311,1 AMH Call the gfStyCrl only one times [Start]
          *DECLARE laIssAdj[9],laRcvAdj[9]
          *STORE 0 TO laIssAdj,laRcvAdj
          *FOR lnI = 1 TO 8
          *  IF laAdjust[lnI] < 0
          *    laIssAdj[lnI] = laAdjust[lnI]
          *    laIssAdj[9] = laIssAdj[9] + laIssAdj[lnI]
          *  ELSE
          *    laRcvAdj[lnI] = laAdjust[lnI]
          *    laRcvAdj[9] = laRcvAdj[9] + laRcvAdj[lnI]
          *  ENDIF
          *ENDFOR
          *IF laIssAdj[9] <> 0
          *  =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laIssAdj,;
          *            lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          *ENDIF
          *IF laRcvAdj[9] <> 0
          *  =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laRcvAdj,;
          *            lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          *ENDIF
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
          =gfSeek('',laNeedFls[lnCurFile,4],'STYINVJL')
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]          
          lnRecCnt = RECCOUNT(laNeedFls[lnCurFile,4])
          IF laAdjust[9] <> 0
            =gfStyCrl('1',STYLE.STYLE,STYDYE.CWARECODE,STYDYE.DYELOT,ldRpPrDt+1,'',@laAdjust,;
                      lnStyCost,'',.T.,'',0,'','',@laGLDistAr,0,'','',@laOldStk)
          ENDIF

          *--append in INVTADJ FILE
          SELECT (laNeedFls[lnNextFile,4])
          APPEND BLANK
          IF SEEK(STYDYE.STYLE+STYDYE.CWARECODE,laNeedFls[lnCurFile,4])
            SELECT (laNeedFls[lnCurFile,4])
            LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                              STYDYE.STYLE+STYDYE.CWARECODE FOR CDYELOT = STYDYE.DYELOT
            IF FOUND()
              lnUnitCost = NCOST
            ELSE
              lnUnitCost = 0
            ENDIF
            SELECT (laNeedFls[lnNextFile,4])
          ELSE
            lnUnitCost = 0
          ENDIF
          *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	          REPLACE Style     WITH STYLE.Style,;
*!*	                  cReason   WITH "Physical Count",;
*!*	                  Date      WITH ldRpPrDt+1,;
*!*	                  DPOSTDate WITH ldRpPrDt+1,;
*!*	                  Type      WITH 'A',;
*!*	                  cFromWare WITH STYDYE.cWareCode,;
*!*	                  Unt_Cost  WITH lnUnitCost,;
*!*	                  Old_Cost  WITH 0

          REPLACE Style     WITH STYLE.Style,;
                  cReason   WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Physical_Count,loFormSet.GetHeaderText("LANG_Physical_Count",loFormSet.HeaderAlias)),;
                  Date      WITH ldRpPrDt+1,;
                  DPOSTDate WITH ldRpPrDt+1,;
                  Type      WITH 'A',;
                  cFromWare WITH STYDYE.cWareCode,;
                  Unt_Cost  WITH lnUnitCost,;
                  Old_Cost  WITH 0
          *N000682,1 04/16/2013 HES Globlization changes[End  ]

          IF llLinkGL
            lcLinkCode = IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,'DEFDEF')
            REPLACE LINK_CODE WITH lcLinkCode,;
                    GLFYEAR   WITH lcGLFYear,;
                    GLPERIOD  WITH lcGLPeriod
          ENDIF

          REPLACE dAdd_Date  WITH oAriaApplication.SystemDate,;
                  cAdd_Time  WITH TIME(),;
                  cAdd_User  WITH "SMPURGE"

          *--update in StyInvJl with Adjustment reason
          SELECT (laNeedFls[lnCurFile,4])
          *E304030,2 MMT 03/05/2019 Error while purging style inventory module[T20180508.0022][Start]
          *lcOrder = SET('ORDER')          
          lcOrder = ORDER()
          *E304030,2 MMT 03/05/2019 Error while purging style inventory module[T20180508.0022][End]
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
          =gfSeek('',laNeedFls[lnCurFile,4],'STYINVJL')
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
          SET ORDER TO
          GO BOTTOM
          FOR lnQ = 1 TO RECCOUNT(laNeedFls[lnCurFile,4]) - lnRecCnt
            IF lnQ = 2
              SKIP -1
            ENDIF
            *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
            =gfREPLACE("REFERENCE WITH '"+lcAdReason+"'")
            *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
            lcSession = cSession
            SELECT (laNeedFls[lnNextFile,4])
            IF lnQ = 2
              SCATTER MEMVAR
              APPEND BLANK
              GATHER MEMVAR
            ENDIF
            REPLACE cSession WITH lcSession

            FOR lnZ=1 TO 8
              lcZ = STR(lnZ,1)
              REPLACE Adj&lcZ    WITH EVALUATE(laNeedFls[lnCurFile,4]+'.NSTK'+lcZ),;
                      OldQty&lcZ WITH laoldStk[lnZ]
              laoldStk[lnZ] = laoldStk[lnZ] + EVALUATE('ADJ'+lcZ)
            ENDFOR
            REPLACE TotAdj     WITH EVALUATE(laNeedFls[lnCurFile,4]+'.NTOTSTK'),;
                    TotOld     WITH laOldStk[9]

            *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
            =gfReplace('')
            *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
                    
                    
            laoldStk[9] = laoldStk[9] + TOTADJ
            SELECT (laNeedFls[lnCurFile,4])
          ENDFOR
          SET ORDER TO &lcOrder.
          *B607311,1 AMH [End]
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
          SELECT (laNeedFls[lnNextFile,4])
          =gfTableUpdate()
          *E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]

        ENDIF
        SELECT STYDYE
        GOTO lnRecNo
      ENDSCAN
    ENDIF
    SELECT STYLE
    oAriaApplication.DataDir = lcCurDir
    oAriaApplication.ActiveCompanyID = lcCurCmp

    *B607311,1 AMH Move this part of code to the correct position [Start]
    *--update in StyInvJl with Adjustment reason
    *SELECT (laNeedFls[lnCurFile,4])
    *lcOrder = SET('ORDER')
    *SET ORDER TO
    *GO BOTTOM
    *REPLACE cAdjreason WITH lcAdReason
    *lcSession = cSession
    *SELECT (laNeedFls[lnNextFile,4])
    *REPLACE cSession WITH lcSession
    *FOR lnZ=1 TO 8
    *  lcZ = STR(lnZ,1)
    *  REPLACE Adj&lcZ    WITH laAdjust[lnZ],;
    *          OldQty&lcZ WITH laoldStk(lnZ)
    *ENDFOR
    *REPLACE TotAdj     WITH laAdjust[9],;
    *        TotOld     WITH laOldStk(9)
    *SELECT (laNeedFls[lnCurFile,4])
    *SET ORDER TO &lcOrder.
    *B607311,1 AMH [End]

  ENDSCAN

  IF llLinkGl .AND. USED(lcGLDTemp)
    USE IN (lcGLDTemp)
  ENDIF
  IF USED('GLDIST')
  *E303963,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	    USE IN GLDIST
    =gfCloseTable('GLDIST')
    *E303963,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
  ENDIF
  IF USED('STYDYE')
    *B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
    *USE IN STYDYE
    =gfCloseTable('STYDYE')
    *B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][End]
  ENDIF
  IF USED('STYLE')
    *B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
    *USE IN STYLE
    =gfCloseTable('STYLE')
    *B611123,1 MMT 03/13/2016 Error while purging Rolls and  Style Inventory Journal[T20151016.0008][End]
  ENDIF
  IF USED('STYINVJL')
  
  *E304030,1 SAH  07/5/2018 CONVERT STYINVJL TO SQL [BEGIN]
  * USE IN STYINVJL
  =gfClosetable('STYINVJL')
  *E304030,1 SAH  07/5/2018 CONVERT STYINVJL TO SQL [END]
  ENDIF
  *--Erase the temp. file.
  IF llLinkGl
    ERASE (oAriaApplication.WorkDir+lcGLDTemp+'.DBF')
  ENDIF

*-- in case of costing method LIFO , FIFO
ELSE
  *--get all styles with balance equal Zero in a temp. file
  *-SAB ----- [Start]
  LOCAL lcEngBhv
  lcEngBhv = SET("EngineBehavior")
  SET ENGINEBEHAVIOR 70
  *-SAB ----- [End]
  SELECT cSession,Style,cWareCode,cDyelot,cRSession,cISession,;
         cTrCode,cTrType,dTrDate,nCost,SUM(nStk1) AS 'nBal1',SUM(nStk2) AS 'nBal2',;
         SUM(nStk3) AS 'nBal3',SUM(nStk4) AS 'nBal4',SUM(nStk5) AS 'nBal5',;
         SUM(nStk6) AS 'nBal6',SUM(nStk7) AS 'nBal7',SUM(nStk8) AS 'nBal8';
  FROM   (laNeedFls[lnCurFile,4]);
  WHERE    DTRDATE <= ldRpPrDt;
  GROUP BY &laNeedFls[lnCurFile,4]..Style,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  HAVING nBal1 = 0 .AND. nBal2 = 0 .AND. nBal3 = 0 .AND. nBal4 = 0 .AND. nBal5 = 0 .AND.;
         nBal6 = 0 .AND. nBal7 = 0 .AND. nBal8 = 0;
  ORDER BY &laNeedFls[lnCurFile,4]..Style,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
  *-SAB ----- [Start]
  SET ENGINEBEHAVIOR (lcEngBhv)
  *-SAB ----- [End]

  SELECT (lcTmpFile)
  *--we calculate Stk's if sum equal zero for the same seasion number
  *-- purge those records
  SCAN

    IF LEFT(laNeedFls[lnCurFile,5],1) = 'A'  && if purge the active company
      *--Note the Following
      *--We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to
      *-- alias , we open every table Of our Purge and the equivelant in the history company
      *--for ex. STYINVJL Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
      *--INVTADJ  Source laNeedFls[lnCurFile +2,4]  Target laNeedFls[lnCurFile + 3,4]
      *--UNCMSESS Source laNeedFls[lnCurFile +4,4]  Target laNeedFls[lnCurFile + 5,4]
      *--STYLE    Source laNeedFls[lnCurFile +6,4]  Target laNeedFls[lnCurFile + 7,4]
      *--STYDYE   Source laNeedFls[lnCurFile +8,4]  Target laNeedFls[lnCurFile + 9,4]
      *--NOTEPAD  Source laNeedFls[lnCurFile +10,4] Target laNeedFls[lnCurFile + 11,4]
      *--BOM      Source laNeedFls[lnCurFile +12,4] Target laNeedFls[lnCurFile + 13,4]

      IF SEEK(STYLE,laNeedFls[lnCurFile+6,4])
        SELECT (laNeedFls[lnCurFile+6,4])
        IF !SEEK(STYLE,laNeedFls[lnCurFile +7,4])
          SCATTER MEMVAR MEMO
          SELECT(laNeedFls[lnCurFile+7,4])
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDIF
        IF SEEK("F"+STYLE,laNeedFls[lnCurFile+10,4])
          SELECT (laNeedFls[lnCurFile+10,4])
          IF !SEEK(STYLE,laNeedFls[lnCurFile +11,4])
            SCATTER MEMVAR MEMO
            SELECT(laNeedFls[lnCurFile+11,4])
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDIF
          SELECT (laNeedFls[lnCurFile+6,4])
        ENDIF
        IF SEEK(CSTYMAJOR,laNeedFls[lnCurFile+12,4])
          SELECT (laNeedFls[lnCurFile+12,4])
          SCAN REST WHILE cItmMajor+Typ+cItmMask+MfgCode+Item+Iclr =;
                          EVALUATE(laNeedFls[lnCurFile+6,4]+'.CSTYMAJOR')
            IF !SEEK(STYLE,laNeedFls[lnCurFile +13,4])
              SCATTER MEMVAR MEMO
              SELECT(laNeedFls[lnCurFile+13,4])
              APPEND BLANK
              GATHER MEMVAR MEMO
            ENDIF
          ENDSCAN
          SELECT (laNeedFls[lnCurFile+6,4])
        ENDIF
        SELECT (lcTmpFile)
      ENDIF

      IF SEEK(STYLE+CWARECODE+CDYELOT,laNeedFls[lnCurFile+8,4])
        SELECT laNeedFls[lnCurFile+8,4]
        IF !SEEK(STYLE+cWareCode+Dyelot,laNeedFls[lnCurFile+9,4])
          SCATTER MEMVAR MEMO
          SELECT(laNeedFls[lnCurFile+9,4])
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDIF
        SELECT (lcTmpFile)
      ENDIF

      IF SEEK(STYLE+CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])
        SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                      &lcTmpFile..STYLE+&lcTmpFile..CWARECODE;
                    FOR CDYELOT = &lcTmpFile..CDYELOT .AND. (Crsession =&lcTmpFile..Crsession;
                        .OR. Cisession=&lcTmpFile..Cisession)
          IF !SEEK(STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6),laNeedFls[lnCurFile+1,4])
            SCATTER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+1,4])
            APPEND BLANK
            GATHER MEMO MEMVAR
          ENDIF
          SELECT (laNeedFls[lnCurFile,4])
          DELETE
        ENDSCAN
      ENDIF
    ELSE && if purge the history company
      IF SEEK(STYLE+CWARECODE,laNeedFls[lnCurFile,4])
        SELECT (laNeedFls[lnCurFile,4])
        DELETE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                      &lcTmpFile..STYLE+&lcTmpFile..CWARECODE;
                    FOR CDYELOT = &lcTmpFile..CDYELOT .AND. (Crsession =&lcTmpFile..Crsession;
                        .OR. Cisession=&lcTmpFile..Cisession)
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (laNeedFls[lnCurFile,4])

  *--Erase temp file
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (oAriaApplication.WorkDir+lcTmpFile+'.DBF')
ENDIF

*--close opend files
IF USED('STYDYE')
  USE IN STYDYE
ENDIF
IF USED('STYLE')
  USE IN STYLE
ENDIF
*--Restore Link check variable
llLinkGL = llStorLink

*B606114,1 AMH [End]
*--- end of lfApStPhIv

*!*************************************************************
*! Name      : lfApFaPhIv
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Append Fabric Physical Inventory reflects the
*!             total balance of the purged records.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPrTrn
*!*************************************************************
*! Passed Parameters  : lnCurFile (file to append physical rec)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfApFaPhIv()
*!*************************************************************

FUNCTION lfApFaPhIv
PARAMETER lnCurFile

*B604743,1 MHM 08/15/2001 Comment lfApFaPhIv FUNCTION becase we change the  way we purge [Start]
*lcGLFYear  = SPACE(04)  && Fiscal Year
*lcGLPeriod = SPACE(02)  && Period
*lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys
*IF laNeedFls[lnCurFile,5]$'AAB'
*  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,dTranDate,cTranType,cTran,;
*         cRSession,cISession,cAdjreason,;
*         SUM(nReceived) AS 'nReceived',SUM(nIssued) AS 'nIssued',SUM(nUnitCost) AS 'nUnitCost',;
*         SEEK(cfabric+ccolor+cwarecode+cdyelot;
*         +crsession+cisession,laNeedFls[lnCurFile+1,4]) AS 'lCopied' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRANDATE <= ldRpPrDt .AND. CTRANTYPE$"23";
*  GROUP BY &laNeedFls[lnCurFile,4]..CFABRIC,&laNeedFls[lnCurFile,4]..CCOLOR,;
*           &laNeedFls[lnCurFile,4]..cWareCode,&laNeedFls[lnCurFile,4]..cDyelot ;
*  HAVING !lCopied ;
*  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
*ELSE
*  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,dTranDate,cTranType,cTran,;
*         cRSession,cISession,cAdjreason,;
*         SUM(nReceived) AS 'nReceived',SUM(nIssued) AS 'nIssued',SUM(nUnitCost) AS 'nUnitCost' ;
*  FROM  (laNeedFls[lnCurFile,4]) ;
*  WHERE DTRANDATE <= ldRpPrDt .AND. CTRANTYPE$"23";
*  GROUP BY &laNeedFls[lnCurFile,4]..CFABRIC,&laNeedFls[lnCurFile,4]..CCOLOR,;
*           &laNeedFls[lnCurFile,4]..cWareCode,&laNeedFls[lnCurFile,4]..cDyelot ;
*  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
*ENDIF

*lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
*lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
*=gfOpenFile(lcDataDir+'FABRIC','FABRIC','SH')
*=gfOpenFile(lcDataDir+'FABDYE','FABDYE','SH')
*IF llLinkGL
*  =gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
*  SELECT GLDIST
*  =AFIELDS(laGlData)
*  lcGLDTemp  = gfTempName()
*  CREATE TABLE oAriaApplication.WorkDir+(lcGLDTemp) FROM ARRAY laGlData
*ENDIF
*lcCurCmp = oAriaApplication.ActiveCompanyID
*lcCurDir = oAriaApplication.DataDir
*oAriaApplication.ActiveCompanyID = lcRpComp
*oAriaApplication.DataDir = lcDataDir
*=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','MA',.T.)
*oAriaApplication.DataDir = lcCurDir
*oAriaApplication.ActiveCompanyID = lcCurCmp
*SELECT (lcTmpFile)
*LOCATE
*lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)
*SCAN
*  SELECT (laNeedFls[lnNextFile,4])
*  APPEND BLANK
*  REPLACE Fabric     WITH &lcTmpFile..cFabric,;
*          Color      WITH &lcTmpFile..cColor,;
*          cReason    WITH "Physical Count",;
*          Date       WITH ldRpPrDt+1,;
*          Type       WITH 'P',;
*          cFromWare  WITH &lcTmpFile..cWareCode ,;
*          nfUnitCost WITH &lcTmpFile..nUnitCost,;
*          nUntCstbuy WITH &lcTmpFile..nUnitCost

*  IF llLinkGL
*    lcLinkCode=IIF(SEEK(&lcTmpFile..cFabric+&lcTmpFile..cColor+&lcTmpFile..cWareCode+;
*                   &lcTmpFile..cDyelot,'FABDYE'),;
*               IIF(!EMPTY(FABDYE.GL_Link),FABDYE.GL_Link,'DEFDEF'),'DEFDEF')
*    REPLACE LINK_CODE WITH lcLinkCode,;
*            GLFYEAR   WITH lcGLFYear,;
*            GLPERIOD  WITH lcGLPeriod
*  ENDIF

*  REPLACE nmTotAdj   WITH &lcTmpFile..nReceived-&lcTmpFile..nIssued,;
*          Oldqty     WITH 0 ,;
*          nOldtoqty  WITH 0 ,;
*          Used_qty   WITH 0 ,;
*          dAdd_Date  WITH oAriaApplication.SystemDate,;
*          cAdd_Time  WITH TIME(),;
*          cAdd_User  WITH "SMPURGE"
*  *--G/L Array difinition and initialization.
*  IF llLinkGL
*    DECLARE laGLDistAr[2,13]
*    laGLDistAr[1,1] = lcLinkCode
*    laGLDistAr[2,1] = lcLinkCode
*    laGLDistAr[1,2] = '015'
*    laGLDistAr[2,2] = '016'
*    laGLDistAr[1,3] = 1
*    laGLDistAr[2,3] = -1
*    STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
*    STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
*    STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
*    STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
*    STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
*    STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
*    laGLDistAr[2,10] = ''
*  ELSE
*    DIME laGLDistAr[1,1]
*    laGLDistAr = ''
*  ENDIF
*  SELECT (lcTmpFile)
*  lcCurCmp = oAriaApplication.ActiveCompanyID
*  lcCurDir = oAriaApplication.DataDir
*  oAriaApplication.ActiveCompanyID = lcRpComp
*  oAriaApplication.DataDir = lcDataDir
*  = gfMatCrl('3',cFabric,cColor,cWareCode,cDyelot,ldRpPrDt+1,ldRpPrDt+1,SPACE(6),;
*                (nReceived-nIssued)*2,nUnitCost,'','',0,'','',@laGLDistAr)
*  oAriaApplication.DataDir = lcCurDir
*  oAriaApplication.ActiveCompanyID = lcCurCmp
*  SELECT (laNeedFls[lnCurFile,4])
*  lcOrder = SET('ORDER')
*  SET ORDER TO
*  GO BOTTOM
*  REPLACE cAdjreason WITH &lcTmpFile..cAdjreason
*  lcSession = cTrn_Seq
*  SELECT (laNeedFls[lnNextFile,4])
*  REPLACE cTrn_Seq WITH lcSession
*  SELECT (laNeedFls[lnCurFile,4])
*  SET ORDER TO &lcOrder.
*ENDSCAN

*IF USED(lcTmpFile)
*  USE IN (lcTmpFile)
*ENDIF
*IF llLinkGl .AND. USED(lcGLDTemp)
*  USE IN (lcGLDTemp)
*ENDIF
*IF USED('GLDIST')
*  USE IN GLDIST
*ENDIF
*IF USED('FABDYE')
*  USE IN FABDYE
*ENDIF
*IF USED('FABRIC')
*  USE IN FABRIC
*ENDIF
*IF USED('MATINVJL')
*  USE IN MATINVJL
*ENDIF
*--Erase the temp. file.
*ERASE (oAriaApplication.WorkDir+lcTmpFile+'.DBF')
*IF llLinkGl
*  ERASE (oAriaApplication.WorkDir+lcGLDTemp+'.DBF')
*ENDIF

*--we start calculate MATINVJL after Purge in case of
*--AVARGE or STANDERD :
*--                   we purge all remaning record we calculate to get ON HAND
*--LIFO ,FIFO,LOT :
*--                we will delete only all records with balance of ON HAND
*--                equal zero that has the same session Number

*--save link check variable
llStorLink = llLinkGL
llLinkGL = .F.
lcGLFYear  = SPACE(04)  && Fiscal Year
lcGLPeriod = SPACE(02)  && Period
lcTmpFile = gfTempName() && temp name of dbf file to hold adjust qtys

*B604743,4 MHM 10/09/2001 Define variable to get warehous[Start]
llWareHous = gfGetMemVar('M_WareHouse',lcRpComp)= 'Y'
*B604743,4 MHM 10/09/2001 [End]

lnDirLn = RAT('\',laNeedFls[lnCurFile,2])
lcDataDir = SUBSTR(laNeedFls[lnCurFile,2],1,lnDirLn)
*E303220,1 TMI 08/29/2012 [Start]
*=gfOpenFile(lcDataDir+'FABRIC','FABRIC','SH')
*=gfOpenFile(lcDataDir+'FABDYE','FABDYE','SH')

=gfOpenTable(lcDataDir+'FABRIC','FABRIC','SH')
=gfOpenTable(lcDataDir+'FABDYE','FABDYE','SH')


*E303220,1 TMI 08/29/2012 [End  ]
IF llLinkGL
  *E303220,1 TMI 08/29/2012 [Start]
  *=gfOpenFile(lcDataDir+'GLDIST','GLDISTAC','SH')
  =gfOpenTable(lcDataDir+'GLDIST','GLDISTAC','SH')
  *E303220,1 TMI 08/29/2012 [End  ]
  SELECT GLDIST
  =AFIELDS(laGlData)
  lcGLDTemp  = gfTempName()
  CREATE TABLE oAriaApplication.WorkDir+(lcGLDTemp) FROM ARRAY laGlData
ENDIF
lcCurCmp = oAriaApplication.ActiveCompanyID
lcCurDir = oAriaApplication.DataDir
oAriaApplication.ActiveCompanyID = lcRpComp
oAriaApplication.DataDir = lcDataDir
=CHECKPRD(ldRpPrDt+1,'lcGLFYear','lcGLPeriod','MA',.T.)
oAriaApplication.DataDir = lcCurDir
oAriaApplication.ActiveCompanyID = lcCurCmp

*-in case of Costing AVARAGE OR SATANDERD
IF lcCstMth $'AS'
  STORE '' TO lcAdReason
  SELECT FABRIC
  LOCATE
  lnNextFile = lnCurFile + IIF(laNeedFls[lnCurFile,5]$'AAB' , 2 , 1)

  SCAN
    STORE 0 TO lnFabCost,lnOnHand
    =SEEK(Fabric+Color,'FABDYE')

    *--append in FINVADJ FILE
*    SET STEP ON 
    SELECT (laNeedFls[lnNextFile,4])
    APPEND BLANK
    IF SEEK(FABDYE.Fabric+FABDYE.Color+FABDYE.cWareCode+FABDYE.Dyelot,laNeedFls[lnCurFile,4])
      lnUnitCost = &laNeedFls[lnCurFile,4]..nUnitCost
    ELSE
      lnUnitCost = 0
    ENDIF
    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    REPLACE Fabric     WITH FABRIC.Fabric,;
*!*	            Color      WITH FABRIC.Color,;
*!*	            cReason    WITH "Physical Count",;
*!*	            Date       WITH ldRpPrDt+1,;
*!*	            Type       WITH 'P',;
*!*	            cFromWare  WITH FABDYE.cWareCode ,;
*!*	            nfUnitCost WITH lnUnitCost,;
*!*	            nUntCstbuy WITH lnUnitCost
    REPLACE Fabric     WITH FABRIC.Fabric,;
            Color      WITH FABRIC.Color,;
            cReason    WITH IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Physical_Count,loFormSet.GetHeaderText("LANG_Physical_Count",loFormSet.HeaderAlias)),;
            Date       WITH ldRpPrDt+1,;
            Type       WITH 'P',;
            cFromWare  WITH FABDYE.cWareCode ,;
            nfUnitCost WITH lnUnitCost,;
            nUntCstbuy WITH lnUnitCost
    *N000682,1 04/16/2013 HES Globlization changes[Start]
    IF llLinkGL
      lcLinkCode=IIF(SEEK(FABRIC.Fabric+FABRIC.Color+FABRIC.cWareCode+;
                     FABRIC.Dyelot,'FABDYE'),;
                 IIF(!EMPTY(FABDYE.GL_Link),FABDYE.GL_Link,'DEFDEF'),'DEFDEF')

     REPLACE LINK_CODE WITH lcLinkCode,;
              GLFYEAR   WITH lcGLFYear,;
              GLPERIOD  WITH lcGLPeriod
    ENDIF

    REPLACE Oldqty     WITH 0 ,;
            nOldtoqty  WITH 0 ,;
            Used_qty   WITH 0 ,;
            dAdd_Date  WITH oAriaApplication.SystemDate,;
            cAdd_Time  WITH TIME(),;
            cAdd_User  WITH "SMPURGE"
    *--G/L Array difinition and initialization.
    IF llLinkGL
      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = lcLinkCode
      laGLDistAr[2,1] = lcLinkCode
      laGLDistAr[1,2] = '015'
      laGLDistAr[2,2] = '016'
      laGLDistAr[1,3] = 1
      laGLDistAr[2,3] = -1
      STORE 'MA'       TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE ''         TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE ldRpPrDt+1 TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear  TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGLPeriod TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGLDTemp  TO laGLDistAr[1,9],laGLDistAr[2,9]
      laGLDistAr[2,10] = ''
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    SELECT FABRIC

    lcCurCmp = oAriaApplication.ActiveCompanyID
    lcCurDir = oAriaApplication.DataDir
    oAriaApplication.ActiveCompanyID = lcRpComp
    oAriaApplication.DataDir = lcDataDir

    IF SEEK(Fabric+Color,'FABDYE')
      SELECT FABDYE

      *B604743,4 MHM 10/09/2001 we have to consider dyelot too [Start]
      *SCAN REST WHILE Fabric+Color+cWareCode+Dyelot = FABRIC.FABRIC+FABRIC.COLOR FOR EMPTY(Dyelot)
      SCAN REST WHILE Fabric+Color+cWareCode+Dyelot = FABRIC.FABRIC+FABRIC.COLOR
        IF FABRIC.cdye_flg = 'Y' AND EMPTY(Dyelot)
          lnFabCost = IIF(llWareHous,FabDye.nAveCstBuy,Fabric.NAveCstBuy)/Fabric.Conv
          LOOP
        ENDIF
      *B604743,1 MHM 10/09/2001 [END]

        lnRecNo = RECNO()
        *--seek in MatInvJl to get total ON HAND for the same fabric color

        *B604743,5 MHM 10/09/2001 get onhand [Start]
        lnOnHand  = FABDYE.ONHAND
        *B604743,5 MHM 10/09/2001 [End]

        IF SEEK(Fabric+Color+cWareCode+Dyelot,laNeedFls[lnCurFile,4])
          SELECT (laNeedFls[lnCurFile,4])

          *--lcAdReason  --------> to get adjast. reason
          lcAdReason = cAdjreason

          *B604743,4 MHM 10/09/2001 GET correct cost [Start]
          *lnFabCost = FABDYE.NFAVE_COST
          IF FABRIC.cdye_flg = 'N'
            lnFabCost = IIF(llWareHous,FabDye.nAveCstBuy,Fabric.NAveCstBuy)/Fabric.Conv
          ENDIF
          *B604743,4 MHM 10/09/2001 [Start]

          SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                          FABDYE.FABRIC+FABDYE.COLOR+FABDYE.cWareCode+;
                          FABDYE.DYELOT FOR DTRANDATE > ldRpPrDt
            lnOnHand = lnOnHand - NRECEIVED + NISSUED
          ENDSCAN
        ENDIF
        *--initialize fabdye
        *B604743,5 MHM 10/09/2001  [Start]
        IF lnOnHand<> 0
        *B604743,5 MHM [End]
          REPLACE FABDYE.ONHAND WITH 0
          *--ADD a new  MatInvJl record with new ONHAND balance
          = gfMatCrl('2',FABDYE.Fabric,FABDYE.Color,FABDYE.cWareCode,FABDYE.Dyelot,ldRpPrDt+1,ldRpPrDt+1,SPACE(6),;
                     lnOnHand,lnFabCost,'','',0,'','',@laGLDistAr)
        *B604743,5 MHM 10/09/2001  [Start]
        ENDIF
        *B604743,5 MHM [End]
        SELECT FABDYE
        GOTO lnRecNo
      ENDSCAN
    ENDIF
    SELECT FABRIC
    oAriaApplication.DataDir = lcCurDir
    oAriaApplication.ActiveCompanyID = lcCurCmp
    *--update in MatInvJl with Adjustement reason
    SELECT (laNeedFls[lnCurFile,4])
    lcOrder = SET('ORDER')
    SET ORDER TO
    GO BOTTOM
    REPLACE cAdjreason WITH lcAdReason
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
   *lcSession = cTrn_Seq
    lcSession = CSESSION
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
    SELECT (laNeedFls[lnNextFile,4])
    
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	    REPLACE cTrn_Seq WITH lcSession,;
*!*	            nmTotAdj WITH lnOnHand
    REPLACE CSESSION WITH lcSession,;
            ADJ1 WITH lnOnHand
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
    
    SELECT (laNeedFls[lnCurFile,4])
    SET ORDER TO &lcOrder.
  ENDSCAN

  IF llLinkGl .AND. USED(lcGLDTemp)
    USE IN (lcGLDTemp)
  ENDIF
  IF USED('GLDIST')
  *E303963,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[Start]
*!*	    USE IN GLDIST
  =gfCloseTable('GLDIST') 
  *E303963,1 AHH 1/04/2018 use the GLDIST table remotely not native, because of conversion to SQL[End]
 
  ENDIF
  IF USED('FABDYE')
    USE IN FABDYE
  ENDIF
  IF USED('FABRIC')
    USE IN FABRIC
  ENDIF
  IF USED('MATINVJL')
    USE IN MATINVJL
  ENDIF
  *--Erase the temp. file.
  IF llLinkGl
    ERASE (oAriaApplication.WorkDir+lcGLDTemp+'.DBF')
  ENDIF

*-- in case of costing method LIFO , FIFO and LOT
ELSE

  *B040155,3  TMI [Start] fix a bug that if a physical inventory is done for material just entered with 0 stock then both
  *                       CRSESSION ,CISESSION is empty
  SELECT (laNeedFls[lnCurFile,4])
  GO TOP
  
 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	  REPLACE CISESSION WITH CTRN_SEQ  ;
*!*	          REFERENCE WITH 'issue sess. updated by Purge' ;
*!*	    FOR CTRANTYPE = '3' AND EMPTY(CRSESSION) AND EMPTY(CISESSION)
  REPLACE CISESSION WITH CSESSION;
          REFERENCE WITH 'issue sess. updated by Purge' ;
    FOR CTRANTYPE = '3' AND EMPTY(CRSESSION) AND EMPTY(CISESSION)
   *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
   
  *B040155,3  TMI [End  ]

  *--get all fabrics with balance equal Zero in a temp. file
  *B040155,3  TMI [Start] check that both NRECEIVED+NISSUED > 0
  *-* SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession ,;
  *-*        cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy      ,;
  *-*        SUM(nReceived-nIssued) AS 'nBalance';
  *-* FROM   (laNeedFls[lnCurFile,4]);
  *-* WHERE    DTRANDATE <= ldRpPrDt;
  *-* GROUP BY &laNeedFls[lnCurFile,4]..cFabric,;
  *-*          &laNeedFls[lnCurFile,4]..cColor,;
  *-*          &laNeedFls[lnCurFile,4]..cWareCode,;
  *-*          &laNeedFls[lnCurFile,4]..cDyelot,;
  *-*          &laNeedFls[lnCurFile,4]..cRSession;
  *-* HAVING nBalance = 0;
  *-* ORDER BY &laNeedFls[lnCurFile,4]..cFabric,;
  *-*          &laNeedFls[lnCurFile,4]..cColor ,;
  *-*          &laNeedFls[lnCurFile,4]..cWareCode,;
  *-*          &laNeedFls[lnCurFile,4]..cDyelot,;
  *-*          &laNeedFls[lnCurFile,4]..cRSession;
  *-* INTO DBF (oAriaApplication.WorkDir+lcTmpFile)

  *-SAB ----- [Start]
  LOCAL lcEngBhv
*  SET STEP ON 
  lcEngBhv = SET("EngineBehavior")
  SET ENGINEBEHAVIOR 70
  *-SAB ----- [End]
  SELECT cTrn_Seq,cFabric,cColor,cWareCode,cDyelot,cRSession,cISession ,;
         cTran,cTranType,dTranDate,dPostDate,nUnitCost,nUntCstBuy      ,;
         SUM(nReceived-nIssued) AS 'nBalance';
  FROM   (laNeedFls[lnCurFile,4]);
  WHERE    DTRANDATE <= ldRpPrDt AND ;
           NRECEIVED+NISSUED > 0 ;
  GROUP BY &laNeedFls[lnCurFile,4]..cFabric,;
           &laNeedFls[lnCurFile,4]..cColor,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  HAVING nBalance = 0;
  ORDER BY &laNeedFls[lnCurFile,4]..cFabric,;
           &laNeedFls[lnCurFile,4]..cColor ,;
           &laNeedFls[lnCurFile,4]..cWareCode,;
           &laNeedFls[lnCurFile,4]..cDyelot,;
           &laNeedFls[lnCurFile,4]..cRSession;
  INTO DBF (oAriaApplication.WorkDir+lcTmpFile)
  *-SAB ----- [Start]
  SET ENGINEBEHAVIOR (lcEngBhv)
  *-SAB ----- [End]

  *B040155,3  TMI [End  ]

  SELECT (lcTmpFile)
  *--we calculate on hand if sum equal zero for the same seasion number
  *-- purge those records
  SCAN

    *B604743,4 MHM 10/09/2001 Get Fabric ,Fabdye , Notepad From its file If Found [Start]
    *--Note the Following
    *--We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to
    *-- alias , we open every table Of our Purge and the equivelant in the history company
    *--for ex. MAtinvjl Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
    *--FinvAdj  Source laNeedFls[lnCurFile +2,4] Target laNeedFls[lnCurFile + 3,4]
    *--Fabric   Source laNeedFls[lnCurFile +4,4] Target laNeedFls[lnCurFile + 5,4]
    *--FabDye   Source laNeedFls[lnCurFile +6,4] Target laNeedFls[lnCurFile + 7,4]
    *--NotePad  Source laNeedFls[lnCurFile +8,4] Target laNeedFls[lnCurFile + 9,4]
    *--Rolls    Source laNeedFls[lnCurFile +10,4] Target laNeedFls[lnCurFile + 11,4]

    =SEEK(&lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR,laNeedFls[lnCurFile+4,4])
    SELECT (laNeedFls[lnCurFile+4,4])
    IF !SEEK(Fabric+Color,laNeedFls[lnCurFile +5,4])
      SCATTER MEMVAR MEMO
      SELECT(laNeedFls[lnCurFile+5,4])
      APPEND BLANK
      GATHER MEMVAR MEMO
      IF !SEEK(Fabric+Color,laNeedFls[lnCurFile+7,4])
        IF SEEK(Fabric+Color,laNeedFls[lnCurFile+6,4])
          SELECT laNeedFls[lnCurFile+6,4]
          SCAN REST WHILE Fabric+Color+cWareCode+DyeLot = &laNeedFls[lnCurFile +4,4]..fabric + &laNeedFls[lnCurFile +4,4]..color
            SCATTER MEMVAR MEMO
            SELECT(laNeedFls[lnCurFile+7,4])
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDSCAN
        ENDIF
      ENDIF
    ENDIF

    *--get from NotePad For Material
    SELECT laNeedFls[lnCurFile+4,4]
    IF SEEK('G'+Fabric,laNeedFls[lnCurFile+8,4])
      SELECT(laNeedFls[lnCurFile + 8,4])
      IF !SEEK('G'+Fabric,laNeedFls[lnCurFile + 9,4])
        SCATTER MEMVAR MEMO
        SELECT(laNeedFls[lnCurFile + 9,4])
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDIF
    ENDIF
    SELECT (laNeedFls[lnCurFile,4])
    *B604743,4 MHM  [End]

    IF SEEK(&lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR+&lcTmpFile..CWARECODE)
      SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
                    &lcTmpFile..CFABRIC+&lcTmpFile..CCOLOR+&lcTmpFile..CWARECODE+;
                    &lcTmpFile..CDYELOT FOR Crsession =&lcTmpFile..Crsession ;
                    .OR. Cisession=&lcTmpFile..Cisession
        *--we handle ROLLS file here
        *--if costing method is LOT then move all ROLLS record for the same cTrn_Seq in the MatInvJl
        *-- and sume of them is zero means that we issue all this roll
        SELECT (laNeedFls[lnCurFile+10,4])
        IF lcCstMth = 'L' .AND. SEEK(&laNeedFls[lnCurFile,4]..cTrn_Seq)
          SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot+crollid = &laNeedFls[lnCurFile,4]..cTrn_Seq
            SCATTER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+11,4])
            APPEND BLANK
            GATHER MEMO MEMVAR
            SELECT (laNeedFls[lnCurFile+10,4])
            DELE
          ENDSCAN
        ENDIF
        SELECT (laNeedFls[lnCurFile,4])
        SCATTER MEMO MEMVAR
        SELECT (laNeedFls[lnCurFile+1,4])
        APPEND BLANK
        GATHER MEMO MEMVAR

        SELECT (laNeedFls[lnCurFile,4])
        DELE
      ENDSCAN
    ENDIF
  ENDSCAN
  SELECT (laNeedFls[lnCurFile,4])

  *--Erase temp file
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (oAriaApplication.WorkDir+lcTmpFile+'.DBF')
ENDIF

*--close opend files
IF USED('FABDYE')
  USE IN FABDYE
ENDIF
IF USED('FABRIC')
  USE IN FABRIC
ENDIF
*--Restore Link check variable
llLinkGL = llStorLink

*B604743,1 MHM 08/15/2001 [end]
*--- end of lfApFaPhIv

*!*************************************************************
*! Name      : lfPackFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Pack Needed Files.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : lcTrn (transaction name)
*!                      lnFrsFl (frist file in transactoin)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPackFile()
*!*************************************************************

FUNCTION lfPackFile
PARAMETER lcTrn, lnFrsFl
*-SAB ----- [Start]
*PRIVATE lnFleCount, lnI, lnPack, lnJ
PRIVATE lnFleCount, lnPack, lnJ
LOCAL lnI
*SET STEP ON 
*-SAB ----- [End]
lnFleCount = ALEN(laNeedFls,1)
lcAllFiles = ''  && get all files name
lnI = lnFrsFl
lnPack = 0  && number of file need to pack
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1       && get file name posation
  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos) && get file name
  *-SAB ----- [Start]
  *IF FILE(laNeedFls[lnI,2]+'.dbf')
  IF lfIsNative(lcFileName) AND FILE(laNeedFls[lnI,2]+'.dbf')
  *-SAB ----- [End]
   IF !(lcFileName$lcAllFiles)
      lnPack = lnPack +1
      DECLARE laPackFile[lnPack] && array of file need to pack
      laPackFile[lnPack] = laNeedFls[lnI,4]
    ELSE
      =gfCloseTable(laNeedFls[lnI,4])
    ENDIF
  ENDIF
  lcAllFiles = lcAllFiles + IIF(lcFileName$lcAllFiles,'',lcFileName+'\')
  lnI = lnI + 1
  IF lnI > lnFleCount
    EXIT
  ENDIF
ENDDO
lnJ = 1

FOR lnJ = 1 TO lnPack
  SELECT (laPackFile[lnJ])
  *E303220,1 TMI 08/29/2012 [Start]
  IF lfIsNATIVE(JUSTSTEM(DBF()))
    lcTbl = DBF(laPackFile[lnJ])
    gfCloseTable(laPackFile[lnJ])
    gfOpenFile(lcTbl,'','EX',laPackFile[lnJ])
    SELECT (laPackFile[lnJ])
    *E303220,1 TMI 08/29/2012 [End  ]
    PACK
    *-SAB ----- [Start]
    USE IN (laPackFile[lnJ])
    *-SAB ----- [End]
  ENDIF
  =gfCloseTable(laPackFile[lnJ])
ENDFOR
*--- end of lfPackFile.

*!*************************************************************
*! Name      : lfRunRebal
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Run Rebalance.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lPPurge
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfRunRebal()
*!*************************************************************

FUNCTION lfRunRebal
PARAMETER lcRebCmp
lcNeedRebal = ''
lnI = 1
FOR lnI = 1 TO 16
  IF laTransact[lnI,3]
    DO CASE
      CASE laTransact[lnI,2] = 'AR'
        lcNeedRebal = IIF('SH,ST'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',SH,ST' + '')
      CASE laTransact[lnI,2] = 'SO' .OR. laTransact[lnI,2] = 'AL'
        lcNeedRebal = IIF('OR,AL'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',OR,AL' + '')
      CASE laTransact[lnI,2] = 'RM'
        lcNeedRebal = IIF('RE,RA'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',RE,RA' + '')
      CASE laTransact[lnI,2] = 'IC'
        lcNeedRebal = IIF('ST'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',ST' + '')
      CASE laTransact[lnI,2] = 'MA'
        lcNeedRebal = IIF('ONO,MH'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',ONO,MH' + '')
      CASE laTransact[lnI,2] = 'MF' .OR. laTransact[lnI,2] = 'PO'
        lcNeedRebal = IIF('WI,WO'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',WI,WO' + '')
      CASE laTransact[lnI,2] = 'PO'
        lcNeedRebal = IIF('IN'$lcNeedRebal,lcNeedRebal,lcNeedRebal + ',IN' + '')
    ENDCASE
  ENDIF
ENDFOR
IF AT(',',lcNeedRebal) = 1
  lcNeedRebal = SUBSTR(lcNeedRebal,2)
ENDIF
IF !EMPTY(lcNeedRebal)
  *E303220,1 TMI 08/29/2012 [Start] comment for now until finishing the main purge program
  *DO (oAriaApplication.ApplicationHome+"SM\SMREBAL.FXP") WITH lcNeedRebal, lcRebCmp, .T. &&*x*
  *E303220,1 TMI 08/29/2012 [End  ]
ENDIF
*--- end of lfRunRebal.

*!*************************************************************
*! Name      : lfSkipFile
*! Developer : AHMED MAHER (AMH)
*! Date      : 10/02/2000
*! Purpose   : Flage the related files as purged.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lfScanFile
*!*************************************************************
*! Passed Parameters  : lnPos (file number)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfSkipFile()
*!*************************************************************

FUNCTION lfSkipFile
PARAMETER lnPos
PRIVATE lnFPosR, lnLPosR, lnFPosD, lnLPosD, lnRPos
IF !EMPTY(laNeedFls[lnPos,6]) && if the file has related files
  *-- lnFPosR = frist posatoin of related file number
  *-- lnLPosR = last posation of related file number
  *-- lnRPos = the number or related file
  STORE 0 TO lnFPosR , lnFPosD
  DO WHILE lnFPosR < LEN(laNeedFls[lnPos,6])  && loop for existing file
    lnLPosR = AT(',',SUBSTR(laNeedFls[lnPos,6],lnFPosR+1))
    lnRPos = VAL(SUBSTR(laNeedFls[lnPos,6],lnFPosR+1,lnLPosR-1))
    laNeedFls[lnRPos,9] = .T.
    IF laNeedFls[lnRPos,5]$'AAB'
      laNeedFls[lnRPos+1,9] = .T.
    ENDIF
    =lfSkipFile(lnRPos)
    lnFPosR = lnFPosR + lnLPosR
  ENDDO
ENDIF  && end if the file has related files
*--- end of lfSkipFile.

*!*************************************************************
*! Name      : lfZeroUpdt
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/13/2000
*! Purpose   : update style , stydye , fabric , fabdye for
*!             numeric fields by zeros
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lnUpPos (the row no. of laNeedFls array)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfZeroUpdt(X)
*!*************************************************************

FUNCTION lfZeroUpdt
PARAMETER lnUpPos
PRIVATE lnUpPos

IF RIGHT(laNeedFls[lnUpPos,2],5) = 'STYLE' .OR. RIGHT(laNeedFls[lnUpPos,2],6) = 'STYDYE'
  REPLACE ORD1 WITH 0 , ORD2 WITH 0 , ORD3 WITH 0 , ORD4 WITH 0 ,;
          ORD5 WITH 0 , ORD6 WITH 0 , ORD7 WITH 0 , ORD8 WITH 0 ,;
          WIP1 WITH 0 , WIP2 WITH 0 , WIP3 WITH 0 , WIP4 WITH 0 ,;
          WIP5 WITH 0 , WIP6 WITH 0 , WIP7 WITH 0 , WIP8 WITH 0 ,;
          STK1 WITH 0 , STK2 WITH 0 , STK3 WITH 0 , STK4 WITH 0 ,;
          STK5 WITH 0 , STK6 WITH 0 , STK7 WITH 0 , STK8 WITH 0 ,;
          ALO1 WITH 0 , ALO2 WITH 0 , ALO3 WITH 0 , ALO4 WITH 0 ,;
          ALO5 WITH 0 , ALO6 WITH 0 , ALO7 WITH 0 , ALO8 WITH 0 ,;
          SHP1 WITH 0 , SHP2 WITH 0 , SHP3 WITH 0 , SHP4 WITH 0 ,;
          SHP5 WITH 0 , SHP6 WITH 0 , SHP7 WITH 0 , SHP8 WITH 0 ,;
          RET1 WITH 0 , RET2 WITH 0 , RET3 WITH 0 , RET4 WITH 0 ,;
          RET5 WITH 0 , RET6 WITH 0 , RET7 WITH 0 , RET8 WITH 0 ,;
          RA1 WITH 0 , RA2 WITH 0 , RA3 WITH 0 , RA4 WITH 0 ,;
          RA5 WITH 0 , RA6 WITH 0 , RA7 WITH 0 , RA8 WITH 0 ,;
          INTRANS1 WITH 0 , INTRANS2 WITH 0 , INTRANS3 WITH 0 , INTRANS4 WITH 0 ,;
          INTRANS5 WITH 0 , INTRANS6 WITH 0 , INTRANS7 WITH 0 , INTRANS8 WITH 0 ,;
          NWO1 WITH 0 , NWO2 WITH 0 , NWO3 WITH 0 , NWO4 WITH 0 ,;
          NWO5 WITH 0 , NWO6 WITH 0 , NWO7 WITH 0 , NWO8 WITH 0 ,;
          TOTORD WITH 0 , TOTWIP WITH 0 , TOTSTK WITH 0 , TOTALO WITH 0 ,;
          TOTSHP WITH 0 , TOTRET WITH 0 , TOTRA WITH 0 , TOTINTRN WITH 0 , NTOTWO WITH 0
ENDIF
IF RIGHT(laNeedFls[lnUpPos,2],6) = 'FABRIC' .OR. RIGHT(laNeedFls[lnUpPos,2],6) = 'FABDYE'
  REPLACE ONHAND WITH 0 , ONORDER WITH 0 , ONRET WITH 0 , USAGE WITH 0 , NMATWIP WITH 0
ENDIF
*--- end of lfZeroUpdt.

*!*************************************************************
*! Name      : lfPurgStJl
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/21/2000
*! Purpose   : Copy Style Invtory Jornal.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lcStySeq (the last Session Purged)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPurgStJl(X)
*!*************************************************************
*
FUNCTION lfPurgStJl
PARAMETER lcStySeq
PRIVATE lcStySeq, lcActStJl, lcHstStJl, lcLstKey, lcKey, llCopied
lcActStJl = gfTempName()
lcHstStJl = gfTempName()
lcHstStyl = gfTempName()

*E303220,1 TMI 08/29/2012 [Start]
*Old:=gfOpenFile(lcACmpDir+'STYINVJL','','SH', @lcActStJl , .T.)
*Old:=gfOpenFile(lcHCmpDir+'STYINVJL','STYINVJL','SH', @lcHstStJl , .T.)
*Old:=gfOpenFile(lcHCmpDir+'STYLE','STYLE','SH', @lcHstStyl , .T.)
=gfOpenTable(lcACmpDir+'STYINVJL','','SH', @lcActStJl , .T.)
=gfOpenTable(lcHCmpDir+'STYINVJL','STYINVJL','SH', @lcHstStJl , .T.)
=gfOpenTable(lcHCmpDir+'STYLE','STYLE','SH', @lcHstStyl , .T.)
*E303220,1 TMI 08/29/2012 [End  ]
SELECT (lcActStJl)
LOCATE
lcLstKey = ''
llCopied = SEEK(style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6),lcHstStJl);
           .OR. !SEEK(style,lcHstStyl)
SCAN REST WHILE csession <= lcStySeq
  lcKey = style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)
  IF lcKey <> lcLstKey
    llCopied = SEEK(style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6),lcHstStJl);
               .OR. !SEEK(style,lcHstStyl)
  ENDIF

  *B804119,1 AMH Fix Illegal value [Start]
  *IF VAL(lcStySeq) > 0
   * =gfThermo(VAL(lcStySeq),VAL(csession),"Copy Style Inventory Journal :","Session No. : "+csession)
  *ELSE
    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    WAIT WINDOW "Copy Style Inventory Journal, Session No. : "+csession NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Copy_Style+csession NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Style,loFormSet.GetHeaderText("LANG_Copy_Style",loFormSet.HeaderAlias))+csession NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]
  *ENDIF
  *B804119,1 AMH [End]
  IF !llCopied
    SCATTER MEMO MEMVAR
    SELECT (lcHstStJl)
    APPEND BLANK
    GATHER MEMO MEMVAR
    SELECT (lcActStJl)
  ENDIF
  lcLstKey = lcKey
ENDSCAN
WAIT CLEAR

=gfCloseTable(lcHstStyl)
=gfCloseTable(lcHstStJl)
=gfCloseTable(lcActStJl)
*--- end of lfPurgStJl.

*!*************************************************************
*! Name      : lfPurgMtJl
*! Developer : AHMED MAHER (AMH)
*! Date      : 11/22/2000
*! Purpose   : Copy Fabric Invtory Jornal.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ----
*!*************************************************************
*! Called from : lpPurge
*!*************************************************************
*! Passed Parameters  : lcMatSeq (the last Session Purged)
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfPurgMtJl(X)
*!*************************************************************
*
FUNCTION lfPurgMtJl
PARAMETER lcMatSeq
PRIVATE lcMatSeq, lcActStJl, lcHstStJl, lcLstKey, lcKey, llCopied

lcActMtJl = gfTempName()
lcHstMtJl = gfTempName()
lcHstFabr = gfTempName()

*E303220,1 TMI 08/29/2012 [Start]
*Old:=gfOpenFile(lcACmpDir+'MATINVJL','','SH', @lcActMtJl , .T.)
*Old:=gfOpenFile(lcHCmpDir+'MATINVJL','MATINVJL','SH', @lcHstMtJl , .T.)
*Old:=gfOpenFile(lcHCmpDir+'FABRIC','FABRIC','SH', @lcHstFabr , .T.)
=gfOpenTable(lcACmpDir+'MATINVJL','','SH', @lcActMtJl , .T.)
=gfOpenTable(lcHCmpDir+'MATINVJL','MATINVJL','SH', @lcHstMtJl , .T.)
=gfOpenTable(lcHCmpDir+'FABRIC','FABRIC','SH', @lcHstFabr , .T.)
*E303220,1 TMI 08/29/2012 [End  ]
SELECT (lcActMtJl)
LOCATE
lcLstKey = ''
llCopied = SEEK(cfabric+ccolor+cwarecode+cdyelot+crsession+cisession,lcHstMtJl);
           .OR. !SEEK(cfabric+ccolor,lcHstFabr)
SCAN REST WHILE ctrn_seq <= lcMatSeq
  lcKey = cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
  IF lcKey <> lcLstKey
    llCopied = SEEK(cfabric+ccolor+cwarecode+cdyelot+crsession+cisession,lcHstMtJl);
               .OR. !SEEK(cfabric+ccolor,lcHstFabr)
  ENDIF
  *B804119,1 AMH Fix Illegal value [Start]
  *IF VAL(lcMatSeq) > 0
    *=gfThermo(VAL(lcMatSeq),VAL(ctrn_seq),"Copy Fabric Inventory Journal :","Session No. : "+ctrn_seq)
  *ELSE
    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	    WAIT WINDOW "Copy Fabric Inventory Journal, Session No. : "+ctrn_seq NOWAIT
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT WINDOW LANG_Copy_Fabric +ctrn_seq NOWAIT
WAIT WINDOW IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Copy_Fabric,loFormSet.GetHeaderText("LANG_Copy_Fabric",loFormSet.HeaderAlias)) +ctrn_seq NOWAIT
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 04/16/2013 HES Globlization changes[End  ]
  *ENDIF
  *B804119,1 AMH [End]
  IF !llCopied
    SCATTER MEMO MEMVAR
    SELECT (lcHstMtJl)
    APPEND BLANK
    GATHER MEMO MEMVAR
    SELECT (lcActMtJl)
  ENDIF
  lcLstKey = lcKey
ENDSCAN
WAIT CLEAR

=gfCloseTable(lcHstFabr)
=gfCloseTable(lcHstMtJl)
=gfCloseTable(lcActMtJl)
*--- end of lfPurgMtJl.
*:*************************************************************
*: Name      : lfPurgRols
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 02/13/2002
*: Purpose   : Purge the rolls file to history comapny.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : File Name.
*:*************************************************************
*: Return      : None.
*:*************************************************************
*: Example     : = lfPurgRols ()
*:*************************************************************
*E301816,1 ABD - Add the ability to delete rolls that have zero qty.
FUNCTION lfPurgRols
PARAMETER lnCurFile
*SET STEP ON 
*-- Note the Following
*-- We use laNeedFls[lnCurFile,4] And Increase lnCurFile to open each file we need accourding to
*-- Alias , we open every table Of our Purge and the equivelant in the history company
*-- for ex. MAtinvjl Source laNeedFls[lnCurFile,4] Target laNeedFls[lnCurFile +1,4]
*-- Fabric   Source laNeedFls[lnCurFile +2,4] Target laNeedFls[lnCurFile + 3,4]
*-- FabDye   Source laNeedFls[lnCurFile +4,4] Target laNeedFls[lnCurFile + 5,4]
*-- Rolls    Source laNeedFls[lnCurFile +6,4] Target laNeedFls[lnCurFile + 7,4]

*-- If we will Purge Master company to a onther company
*-- Else We will purge the purge company.

*- Check if the cost methoed is lot or not , if lot complete the process
*-- else return
IF lcCstMth = 'L'
  *-- Check if we Purge the history company or company has a purge company.

  IF left(laNeedfls[lnCurFile,5],1) = 'H'
    *-- Create relation bettween MAtinvjl Source File and Rolls source File.
    SELECT (laNeedFls[lnCurFile +3,4])
    SET RELATION TO csession INTO (laNeedFls[lnCurFile,4]) ADDITIVE

    =AFIELDS(laRolls)

    lcTmpFile = gfTempName() && temp name of dbf file to hold Rolls line.
    CREATE TABLE oAriaApplication.WorkDir+(lcTmpFile) FROM ARRAY laRolls

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	    INDEX ON cRollItem + Color + cWareCode+ Dyelot + cRSession TAG (lcTmpFile) ;
*!*	      OF (oAriaApplication.WorkDir+lcTmpFile+'.CDX')
    INDEX ON Style + cWareCode+ cRSession TAG (lcTmpFile) ;
      OF (oAriaApplication.WorkDir+lcTmpFile+'.CDX')
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
    
    *-- Get all Rolls with balance equal Zero in a temp. file
    
    SELECT (laNeedFls[lnCurFile+3,4])

    SCAN FOR nqtybal = 0 .AND. &laNeedFls[lnCurFile,4]..DTRANDATE <= ldRpPrDt
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    SELECT (laNeedFls[lnCurFile +3,4])
    SET RELATION TO
    SET ORDER TO Rolapl


    SELECT (laNeedFls[lnCurFile,4])
    SET ORDER TO Matinvjl

    SELECT (lcTmpFile)
    *-- We calculate Rolls if sum equal zero for the same seasion number
    *--  & delete from the rolles file.
    SCAN
    
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	      SELECT (laNeedFls[lnCurFile +3,4])
*!*	      IF SEEK(&lcTmpFile..crsession,laNeedFls[lnCurFile +3,4]) && Seek in the Master rolls file.
*!*	        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
*!*	          crollitem+color+cwarecode+dyelot+crollid = &lcTmpFile..crollitem ;
*!*	          +&lcTmpFile..color +&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
      SELECT (laNeedFls[lnCurFile +6,4])
      IF SEEK(&lcTmpFile..crsession,laNeedFls[lnCurFile +6,4]) && Seek in the Master rolls file.
        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
          Style+cwarecode+dyelot+crollid = &lcTmpFile..Style+&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]


          *--1) Delete Recored from the Material Inv. Journal First.

          SELECT (laNeedFls[lnCurFile,4])
          *-- Seek and scan in the material Inv Journal for the same session.
          *-- Index On cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
          
          *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	          IF SEEK(&laNeedFls[lnCurFile+3,4]..cRollItem+&laNeedFls[lnCurFile+3,4]..Color;
*!*	              +&laNeedFls[lnCurFile+3,4]..cWareCode+&laNeedFls[lnCurFile+3,4]..Dyelot+;
*!*	              &laNeedFls[lnCurFile+3,4]..Crsession+&laNeedFls[lnCurFile+3,4]..Cisession)
*!*	            SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
*!*	                      &laNeedFls[lnCurFile+3,4]..cRollItem+&laNeedFls[lnCurFile+3,4]..Color+;
*!*	                      &laNeedFls[lnCurFile+3,4]..cWareCode+&laNeedFls[lnCurFile+3,4]..Dyelot+;
*!*	                      &laNeedFls[lnCurFile+3,4]..Crsession+&laNeedFls[lnCurFile+3,4]..Cisession
*CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)  ='0002'
          IF SEEK('0002'+&laNeedFls[lnCurFile+3,4]..Style+&laNeedFls[lnCurFile+3,4]..cWareCode+&laNeedFls[lnCurFile+3,4]..Csession)
            SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)  ='0002'+;
                      &laNeedFls[lnCurFile+3,4]..Style+&laNeedFls[lnCurFile+3,4]..cWareCode+;
                      &laNeedFls[lnCurFile+3,4]..Csession FOR Cisession = &laNeedFls[lnCurFile+3,4]..Cisession AND Crsession = &laNeedFls[lnCurFile+3,4]..Crsession 
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

              SELECT (laNeedFls[lnCurFile,4])
              =RLOCK()
              *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

              *DELETE
              =gfDelete()
              *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

              UNLOCK
            ENDSCAN
          ENDIF

          *--2) Delete recored from the Rolls File.
          
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
       *  SELECT (laNeedFls[lnCurFile +3,4])
          SELECT (laNeedFls[lnCurFile +6,4])
        *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

          =RLOCK()
          *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

          *DELETE
          =gfDelete()
         *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

          UNLOCK
        ENDSCAN
      ENDIF
      SELECT (lcTmpFile)
    ENDSCAN
  ELSE
    *-- Create relation bettween MAtinvjl Source File and Rolls source File.
    SELECT (laNeedFls[lnCurFile +6,4])
    
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
   * SET RELATION TO csession INTO (laNeedFls[lnCurFile,4]) ADDITIVE
     SET RELATION TO '0002'+Style+CwareCode+csession INTO (laNeedFls[lnCurFile,4]) ADDITIVE
 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
    =AFIELDS(laRolls)
    lcTmpFile = gfTempName() && temp name of dbf file to hold Rolls line.
    CREATE TABLE oAriaApplication.WorkDir+(lcTmpFile) FROM ARRAY laRolls
    INDEX ON cRollItem + Color + cWareCode+ Dyelot + cRSession TAG (lcTmpFile) ;
      OF (oAriaApplication.WorkDir+lcTmpFile+'.CDX')
    *-- Get all Rolls with balance equal Zero in a temp. file
    
 
    SELECT (laNeedFls[lnCurFile +6,4])
    
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	SCAN FOR nqtybal = 0 .AND. &laNeedFls[lnCurFile,4]..DTRANDATE <= ldRpPrDt
    SCAN FOR nqtybal = 0 .AND. &laNeedFls[lnCurFile,4]..DTRDATE <= ldRpPrDt
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpFile) FROM MEMVAR
    ENDSCAN
    SELECT (laNeedFls[lnCurFile +6,4])
    SET RELATION TO
    
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
    =gfSetOrder('Rolapl')
   *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

    SET ORDER TO Rolapl

    SELECT (laNeedFls[lnCurFile,4])
    
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
    *SET ORDER TO Matinvjl
    =gfSetOrder('STYINVJL')
  *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

    SELECT (laNeedFls[lnCurFile+1,4])
    
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
    *SET ORDER TO Matinvjl
   =gfSetOrder('STYINVJL')
   *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

    SELECT (lcTmpFile)
    *-- We calculate Rolls if sum equal zero for the same seasion number
    *-- Purge those records and delete from the rolles file.
    SCAN
      *-- Update the purge company with the fabric color.
     
    *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
      *=SEEK(&lcTmpFile..cRollItem+&lcTmpFile..Color,laNeedFls[lnCurFile+2,4])
      =SEEK('0002'+&lcTmpFile..Style,laNeedFls[lnCurFile+2,4])
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
      
      SELECT (laNeedFls[lnCurFile+2,4])
      
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
      *IF !SEEK(Fabric+Color,laNeedFls[lnCurFile +3,4])
      IF !SEEK('0002'+Style,laNeedFls[lnCurFile +3,4])
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
      
        SCATTER MEMVAR MEMO
        SELECT(laNeedFls[lnCurFile+3,4])
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

        *APPEND BLANK
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

        m.StkVal  = 0
        
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
       *m.OnHand  = 0
       *m.OnOrder = 0
		m.TOTSTK = 0
		m.Stk1 = 0
		m.TOTWIP = 0
		m.Wip1 = 0
		m.nstkval = 0
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
       
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

       *GATHER MEMVAR MEMO
       =gfAppend('',.T.)
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]


       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
        *        IF !SEEK(Fabric+Color,laNeedFls[lnCurFile+5,4])
        *          IF SEEK(Fabric+Color,laNeedFls[lnCurFile+4,4])
*!*	                    SELECT laNeedFls[lnCurFile+4,4]
*!*	            SCAN REST WHILE Fabric+Color+cWareCode+DyeLot = ;
*!*	              &laNeedFls[lnCurFile + 2,4]..Fabric + &laNeedFls[lnCurFile + 2,4]..Color
        IF !SEEK('0002'+Style,laNeedFls[lnCurFile+5,4])
          IF SEEK('0002'+Style,laNeedFls[lnCurFile+4,4])
            SELECT laNeedFls[lnCurFile+4,4]
            SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+DYELOT   = '0002'+&laNeedFls[lnCurFile + 2,4]..Style
        *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
                     
              SCATTER MEMVAR MEMO
              SELECT(laNeedFls[lnCurFile+5,4])
              *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

              *APPEND BLANK
           *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

              m.StkVal  = 0
              
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
 *!*	m.OnHand  = 0
 *!*	m.OnOrder = 0
 *!*	m.usage     = 0
		m.TOTSTK = 0
		m.Stk1 = 0
		m.TOTWIP = 0
		m.Wip1 = 0
		m.nstkval = 0
		m.NHUSAGE1 = 0
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
             
        *  GATHER MEMVAR MEMO
         =gfAppend('', .T.)
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]


            ENDSCAN
          ENDIF
        ENDIF
      ENDIF


      *-- We handle ROLLS file here
      *-- If costing method is LOT then move all ROLLS record for the same cTrn_Seq in the MatInvJl
      *-- Scan in the Master Rolls File

      SELECT (laNeedFls[lnCurFile +6,4])
      IF SEEK(&lcTmpFile..crsession,laNeedFls[lnCurFile +6,4]) && Seek in the Master rolls file.
     
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
*!*	          crollitem+color+cwarecode+dyelot+crollid = &lcTmpFile..crollitem ;
*!*	          +&lcTmpFile..color +&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
        SCAN REST WHILE crsession = &lcTmpFile..crsession FOR ;
          Style+cwarecode+dyelot+crollid = &lcTmpFile..Style +&lcTmpFile..cwarecode +&lcTmpFile..dyelot +&lcTmpFile..crollid
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
          
          SCATTER MEMO MEMVAR
          *-- Select the History Rolls file and add the Rolls record.
          SELECT (laNeedFls[lnCurFile+7,4])
        *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

*!*	          APPEND BLANK
*!*	          *-- This variable will update the material inv. Adj.
*!*	          GATHER MEMO MEMVAR
	        =gfAppend('', .T.)
          *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]


          *-- Select the master trolls file and delete the Rolls record.
          SELECT (laNeedFls[lnCurFile+6,4])
          =RLOCK()
          *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

*          DELETE
          =gfDelete()
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

          UNLOCK
          SELECT (laNeedFls[lnCurFile +6,4])

          *-- Update the Mt Inv Journal.
          SELECT (laNeedFls[lnCurFile,4])
          *-- Seek and scan in the material Inv Journal for the same session.
          *-- Index On cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
        
        *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
          **CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)                                                   
*!*	          IF SEEK(&laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color;
*!*	              +&laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
*!*	              &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession)
*!*	            SCAN REST WHILE Cfabric+Ccolor+Cwarecode+Cdyelot+Crsession+Cisession=;
*!*	                      &laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color+;
*!*	                      &laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
*!*	                      &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession
          IF SEEK('0002'+&laNeedFls[lnCurFile+7,4]..Style+&laNeedFls[lnCurFile+7,4]..cWareCode)
            SCAN REST WHILE CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)  ='0002'+&laNeedFls[lnCurFile+7,4]..Style+&laNeedFls[lnCurFile+7,4]..cWareCode FOR ;
                      Crsession = &laNeedFls[lnCurFile+7,4]..Crsession AND  Cisession = &laNeedFls[lnCurFile+7,4]..Cisession
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

              SELECT (laNeedFls[lnCurFile,4])
              SCATTER MEMO MEMVAR

              *-- Check if we append this recored before.
              *-- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
              SELECT (laNeedFls[lnCurFile+1,4])
           
             *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	              *IF SEEK(&laNeedFls[lnCurFile+7,4]..cRollItem+&laNeedFls[lnCurFile+7,4]..Color +;
*!*	                      &laNeedFls[lnCurFile+7,4]..cWareCode+&laNeedFls[lnCurFile+7,4]..Dyelot+;
*!*	                      &laNeedFls[lnCurFile+7,4]..Crsession+&laNeedFls[lnCurFile+7,4]..Cisession);
*!*	                      .AND. EMPTY(&laNeedFls[lnCurFile+1,4]..cisession)
*!*	                  REPLACE nreceived WITH nreceived + &lcTmpFile..nQty ,;
                        NstkVal   WITH nreceived * nUnitCost    
              =SEEK('0002'+&laNeedFls[lnCurFile+7,4]..Style+&laNeedFls[lnCurFile+7,4]..cWareCode)
              LOCATE REST WHILE  CINVTYPE+STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)  ='0002'+&laNeedFls[lnCurFile+7,4]..Style+&laNeedFls[lnCurFile+7,4]..cWareCode FOR ;
                      Crsession = &laNeedFls[lnCurFile+7,4]..Crsession AND  Cisession = &laNeedFls[lnCurFile+7,4]..Cisession AND EMPTY(&laNeedFls[lnCurFile+1,4]..cisession)
              IF FOUND()
                REPLACE NSTK1 WITH NSTK1+ &lcTmpFile..nQty ,;
                        NTOTSTK WITH NTOTSTK + &lcTmpFile..nQty ,;
                        NstkVal   WITH NTOTSTK * ncost 
              *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007] [End]   
                   *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

        =gfReplace('')
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

                                 
              ELSE
            *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

                *APPEND BLANK
                *GATHER MEMO MEMVAR
                =gfAppend('', .T.)
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

                *-- In case add Item with type 1 [Reciving session],
                *-- I will update withe the current value only.
                IF EMPTY(&laNeedFls[lnCurFile+1,4]..cisession)
                  
                 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*                  REPLACE nreceived WITH &lcTmpFile..nQty     ,;
                           NstkVal   WITH nreceived* nUnitCost
               REPLACE NSTK1 WITH  &lcTmpFile..nQty ,;
                        NTOTSTK WITH &lcTmpFile..nQty ,;
                        NstkVal   WITH NTOTSTK * ncost                          
              *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]      
                 
                ENDIF
           *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]

        =gfReplace('')
       *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

              ENDIF
              SELECT (laNeedFls[lnCurFile,4])
        *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007] [Start]

              =gfDelete()
         *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

            ENDSCAN
          ENDIF
          SELECT (laNeedFls[lnCurFile +6,4])
        ENDSCAN
      ENDIF
      SELECT (lcTmpFile)
    ENDSCAN
  ENDIF

  *--Erase temp file
  IF USED(lcTmpFile)
    USE IN (lcTmpFile)
  ENDIF
  ERASE (oAriaApplication.WorkDir+lcTmpFile+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTmpFile+'.CDX')

ENDIF



RETURN
*-- End Of lfPurgRols.
*E301816,1 ABD - Add the ability to delete rolls that have zero qty.
*:*************************************************************


*:**************************************************************************
*:* Name        : lpPrePurge
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 05/02/2006
*:* Purpose     : Define a pre-purge functions to be run for each set of transactions befor purge process
*:***************************************************************************
*:* Called from : lpPrTrn
*:***************************************************************************
*:* Parameters : lcTrn
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
FUNCTION lpPrePurge
PARAMETERS lcTrn

*B607933,1 TMI [Start] Convert the file names to upper case to can be sought correctly in pre preparing functions
FOR lnI = 1 TO ALEN(laNeedFls,1)
  laNeedFls[lnI,2] = UPPER(laNeedFls[lnI,2])
ENDFOR
*B607933,1 TMI [End  ]

DO CASE
CASE lcTrn = 'Keyed Off Transactions'
  =lfPrepAR()
CASE lcTrn = 'SalesRep Commission'
  =lfPrepSR()
ENDCASE

*-- end of lpPrePurge.

*:**************************************************************************
*:* Name        : lfPrepAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/13/2006
*:* Purpose     : Prepare AR module by setting the non-purgable records
*:***************************************************************************
*B040155,1
FUNCTION lfPrepAR
PRIVATE lnSlct,lnPos,lcArHst,lcKey,lcDenyPurg
lnSlct = SELECT()

lcArHst = gfTempName()

*-SAB ----- [Start]
*lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , oAriaApplication.DataDir+'ARHIST' ) , 1 )
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][Start]
*lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , STRTRAN(oAriaApplication.DataDir, oAriaApplication.ActiveCompanyID, lcRpComp)+'ARHIST' ) , 1 )
lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , UPPER(STRTRAN(oAriaApplication.DataDir, oAriaApplication.ActiveCompanyID, lcRpComp))+'ARHIST' ) , 1 )
*B610845,1 MMT 09/10/2014 Fix bugs in Aria4XP purge program [T20140402.0008][End]
*-SAB ----- [End]

*- Open the arhist file again
*E303220,1 TMI 08/29/2012 [Start] open ARHIST with order ARHIST, do not use INDEX ON
*Old:USE (laNeedFls[lnPos,2]) AGAIN IN 0 ALIAS &lcArHst
*Old:SELECT &lcArHst
*Old:INDEX ON ACCOUNT+HISTORY TO (oAriaApplication.WorkDir+lcArHst)
USE (laNeedFls[lnPos,2]) AGAIN IN 0 ALIAS &lcArHst ORDER TAG ARHIST
*E303220,1 TMI 08/29/2012 [End  ]
STORE gfTempName() TO lcDenyPurg
CREATE TABLE (oAriaApplication.WorkDir+lcDenyPurg) (ACCOUNT C(5),HISTORY C(6))
INDEX ON ACCOUNT+HISTORY TAG &lcDenyPurg
SELECT (laNeedFls[lnPos,4])
lcSvOrd = ORDER()
SET ORDER TO ARHIST
GO TOP

SCAN FOR TRANDATE<=ldRpPrDt
  lcKey = ACCOUNT+HISTORY
  =SEEK(lcKey,lcArHst)
  SELECT (lcArHst)
  SCAN REST WHILE ACCOUNT+HISTORY = lcKey
    IF TRANDATE > ldRpPrDt
      INSERT INTO &lcDenyPurg VALUES (&lcArHst..ACCOUNT,&lcArHst..HISTORY)
    ENDIF
  ENDSCAN
ENDSCAN

SELECT (laNeedFls[lnPos,4])
SET ORDER TO &lcSvOrd

IF RECCOUNT(lcDenyPurg)>0
  laNeedFls[lnPos,8] = laNeedFls[lnPos,8] + ".AND. !SEEK(ACCOUNT+HISTORY,'"+lcDenyPurg+"')"
ENDIF

USE IN &lcArHst

SELECT (lnSlct)
*-- end of lfPrepAR.


*:**************************************************************************
*:* Name        : lfPrepSR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/13/2006
*:* Purpose     : Prepare SR module befor purge process by summarizing data on repcomm file
*:***************************************************************************
*B040155,1
FUNCTION lfPrepSR
PRIVATE lnSlct,lnPos,lcRepSumms,lcKey
lnSlct = SELECT()

IF !USED('SYCCURR')
  =gfOpenFile(oAriaApplication.SysPath+'SYCCURR','CCURRCODE','SH')
ENDIF
=SEEK(oAriaApplication.BaseCurrency,'SYCCURR')
lnCurrUnit = SYCCURR.NCURRUNIT

lcRepSumms = gfTempName()

*-SAB ----- [Start]
*lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , oAriaApplication.DataDir+'REPCOMM' ) , 1 )
*B610942,2 MMT 02/11/2015 Error while purging Sales Rep. Module[T20141111.0011][Start]
*lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , STRTRAN(oAriaApplication.DataDir, oAriaApplication.ActiveCompanyID, lcRpComp)+'REPCOMM' ) , 1 )
lnPos = ASUBSCRIPT( laNeedFls , ASCAN( laNeedFls , UPPER(STRTRAN(oAriaApplication.DataDir, oAriaApplication.ActiveCompanyID, lcRpComp)+'REPCOMM')) , 1 )
*B610942,2 MMT 02/11/2015 Error while purging Sales Rep. Module[T20141111.0011][End]
*-SAB ----- [End]

SELECT (laNeedFls[lnPos,4])
GO TOP
TOTAL ON REPCODE FOR DATE <= ldRpPrDt TO (oAriaApplication.WorkDir+lcRepSumms)
SELECT 0
USE (oAriaApplication.WorkDir+lcRepSumms)
SCAN FOR AMOUNT<>0
  SCATTER MEMVAR
  SELECT (laNeedFls[lnPos,4])
  APPEND BLANK
  REPLACE REPCODE   WITH M.REPCODE  ;
          AMOUNT    WITH M.AMOUNT   ;
          BALANCE   WITH M.BALANCE  ;
          DATE      WITH ldRpPrDt+1 ;
          DESC      WITH STR(MONTH(ldRpPrDt),2) +'/'+ STR(YEAR(ldRpPrDt),2) + ' purge summary' ;
          TRANTYPE  WITH IIF(M.AMOUNT<0,'3','4')  ;
          STATUS    WITH 'O' ;
          CCURRCODE WITH oAriaApplication.BaseCurrency ;
          NCURRUNIT WITH lnCurrUnit ;
          NEXRATE   WITH 1 ;
          NFORAMNT  WITH M.AMOUNT ;
          CADD_USER WITH oAriaApplication.User_ID ;
          CADD_TIME WITH TIME()    ;
          DADD_DATE WITH oAriaApplication.SystemDate
ENDSCAN

USE IN &lcRepSumms
ERASE (oAriaApplication.WorkDir+lcRepSumms+'.DBF')

SELECT (lnSlct)
*-- end of lfPrepSR.

*:**************************************************************************
*:* Name        : lfPrepIC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/18/2006
*:* Purpose     : Prepare styinvjl file for purge
*               1)  do not purge lines with related ones in POSHDR,RETHDR,INVHDR
*-              2)  summarize purged lines with new added line with reference "Purge Count"
*               3)  Delete purged lines from styinvjl
*:***************************************************************************
FUNCTION lfPrepIC

*-- end of lfPrepIC.


*:**************************************************************************
*:* Name        : lfClcAvcst
*:* Developer   : MHM - Mohamed Shokry
*:* Date        : 07/06/2008
*:* Purpose     : Recalcualte Style Cost
*--B608475
*:***************************************************************************
FUNCTION lfClcAvcst

PRIVATE lnlcCost , lnAlias
*B608475,3 TMI 09/01/2008 [start] define the variable lnlcCost
PRIVATE lcSvDelSt
lcSvDelSt = SET('DELETED')
lnlcCost = 0
lnTotStk = 0
lnAvCost = 0
*B608475,3 TMI 09/01/2008 [end  ]

lnAlias = SELECT()
SELECT (laNeedFls[lnCurFile,4])

LOCATE
*B608475,3 TMI 09/01/2008 [start] locate the file STYINVJLS
SET DELETED OFF
*E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [Start]
*=SEEK(STYDYE.STYLE+STYDYE.CWARECODE)
=gfSEEK(STYDYE.STYLE+STYDYE.CWARECODE)
*E304030,1 MMT 07/24/2018 CONVERT STYINVJL TO SQL [End]
*B608475,3 TMI 09/01/2008 [END  ]
SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)=;
                          STYDYE.STYLE+STYDYE.CWARECODE;
                    FOR CDYELOT = STYDYE.DYELOT .AND. DTRDATE<=ldRpPrDt

  *B608475,3 TMI 09/02/2008 [START]
  *lnlcCost = ncost
  lnlcCost = lnlcCost + NSTKVAL
  lnTotStk = lnTotStk + NTOTSTK

  lnAvCost = IIF(lnTotStk=0,nCost,lnlcCost/lnTotStk)
  *B608475,3 TMI 09/02/2008 [END  ]

ENDSCAN
*B608475,3 TMI 09/03/2008 [Start]
SET DELETED &lcSvDelSt
*B608475,3 TMI 09/03/2008 [End  ]

SELECT (lnAlias)
RETURN lnAvCost




*!*************************************************************
*! Name      : lfIsNative
*! Developer : Tarek Mohamed Ibrahim
*! Date      : 07/21/2011
*! Purpose   : check if table is fox
*!*************************************************************
*E302678,3 TMI 07/21/2011
FUNCTION LFISNATIVE
PARAMETERS LCALIAS
LCALIAS = IIF(EMPTY(LCALIAS),ALIAS(),LCALIAS)
LCALIAS = UPPER(LCALIAS)
LOCAL LLNATIVE,LCTEMPCURS,LNSLCT
LNSLCT = SELECT(0)
LCTEMPCURS = GFTEMPNAME()
LLNATIVE = .T.
*<Write here the code that checks if this table is native>
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
*LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFILES WHERE CFILE_NAM = '&lcAlias'",'',"&lcTempCurs","",OARIAAPPLICATION.CARIA4SYSFILES,3,"",SET("Datasession"))
LNREMRESULT = OARIAAPPLICATION.REMOTESYSTEMDATA.EXECUTE("Select * from SYDFILES WHERE CFILE_NAM = '&lcAlias' AND cVer = 'A40'",'',"&lcTempCurs","",OARIAAPPLICATION.CARIA4SYSFILES,3,"",SET("Datasession"))
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][End]
SELECT (LCTEMPCURS)
LOCATE
LLNATIVE = !FOUND()
USE IN (LCTEMPCURS)

SELECT (LNSLCT)
RETURN LLNATIVE


************************************************************
*! Name      : MSG
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 09/05/2012
*! Purpose   : MSG, can run only on my PC
************************************************************
FUNCTION MSG
PARAMETERS llDoNotUseStep
IF SYS(0)='DEV4 # tarek'
  ON ERROR
  _SCREEN.Visible=.T.
  IF !llDoNotUseStep
  ENDIF
ENDIF
RETURN
*- End of MSG.


************************************************************
*! Name      : lfGetComp
*! Developer : Saber A Razek (SAB)
*! Date      : 09/03/2012
*! Purpose   : Get Comp ID from DBF Path
************************************************************
FUNCTION lfGetComp
LPARAMETERS lcFilePath

LOCAL lcCompID
lcCompID = ''
IF !EMPTY(lcFilePath)
ENDIF
  x = RAT('\', ADDBS(lcFilePath), 3) + 1
  y = RAT('\', ADDBS(lcFilePath), 2)
  lcCompID = SUBSTR(ADDBS(lcFilePath), x, y - x)
RETURN lcCompID

ENDFUNC
*- End of lfGetComp


************************************************************
*! Name      : lfGetComp
*! Developer : Saber A Razek (SAB)
*! Date      : 09/03/2012
*! Purpose   : Get Comp ID from DBF Path
************************************************************
FUNCTION lfGetCompConStr
LPARAMETERS lcCompanyID

LOCAL lcCompConStr
lcCompConStr = oAriaApplication.ActiveCompanyConStr
lcCompConStr = lcCompConStr


RETURN lcCompConStr

ENDFUNC
*- End of lfGetComp


************************************************************
*! Name      : lfPurgStyMstr
*! Developer : Saber A Razek (SAB)
*! Date      : 09/03/2012
*! Purpose   : Purge Style Master
************************************************************
FUNCTION lfPurgStyMstr

LOCAL lcActStyle, lcHstStyle
lcActStyle = gfTempName()
lcHstStyle = gfTempName()
*SET STEP ON 

IF USED('STYLE')
  USE IN STYLE
ENDIF

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007] [Start]
*!*	=gfOpenTable(lcACmpDir+'STYLE','STYLE','EX',lcActStyle)   && STYLE
*!*	=gfOpenTable(lcHCmpDir+'STYLE','STYLE','EX',lcHstStyle)   && STYLE
*!*	=gfOpenTable(lcACmpDir+'STYDYE','STYDYE','SH')   && STYLE+CWARECODE+DYELOT
*!*	=gfOpenTable(lcACmpDir+'STYINVJL','STYDATE','SH')   && STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE
*!*	=gfOpenTable(lcACmpDir+'ORDLINE','ORDLINES','SH')   && STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)
*!*	=gfOpenTable(lcACmpDir+'ORDHDR','ORDHDR','SH')   && CORDTYPE+ORDER
*!*	=gfOpenTable(lcACmpDir+'POSLN','POSLNS','SH')   &&CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD
oAriaApplication.Ref4.ActiveCompanyID = lcRpComp
oAriaApplication.GetCompanyInformation(lcRpComp)
=gfOpenTable(lcACmpDir+'STYLE','STYLE','EX',lcActStyle)   && STYLE
=gfOpenTable(lcACmpDir+'STYDYE','STYDYE','SH')   && STYLE+CWARECODE+DYELOT
=gfOpenTable(lcACmpDir+'STYINVJL','STYDATE','SH')   && STYLE+CWARECODE+DTOS(DTRDATE)+CSESSION+CIRTYPE
=gfOpenTable(lcACmpDir+'ORDLINE','ORDLINES','SH')   && STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6)
=gfOpenTable(lcACmpDir+'ORDHDR','ORDHDR','SH')   && CORDTYPE+ORDER
=gfOpenTable(lcACmpDir+'POSLN','POSLNS','SH')   &&CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD

oAriaApplication.Ref4.ActiveCompanyID = lcHCmpCod
oAriaApplication.GetCompanyInformation(lcHCmpCod)
=gfOpenTable(lcHCmpDir+'STYLE','STYLE','EX',lcHstStyle)   && STYLE

oAriaApplication.Ref4.ActiveCompanyID = lcRpComp
oAriaApplication.GetCompanyInformation(lcRpComp)

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]

SELECT (lcActStyle)
SCAN
  llHasTrns = .F.
  lcSeekKey = &lcActStyle..STYLE
  *- Check if the Style has Transaction in Style Inventory Journal after the purge date
  
  *E304030,1 SAH 07/5/2018 CONVERT STYINVJL TO SQL [BEGIN]
  *=SEEK(lcSeekKey,'STYINVJL')
  =GFSEEK(lcSeekKey,'STYINVJL')
  *E304030,1 SAH 07/5/2018 CONVERT STYINVJL TO SQL [END]
  
  SELECT STYINVJL
  LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = lcSeekKey ;
              FOR CTRTYPE $ '345678' AND dTrDate > ldRpPrDt
  llHasTrns = FOUND()


  IF !llHasTrns
    *- Check if the Style has Orders or Contracts after the purge date
    =SEEK(lcSeekKey,'ORDLINE')
    SELECT ORDLINE
    LOCATE REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcSeekKey ;
              FOR CORDTYPE $ 'OC' AND SEEK(CORDTYPE+ORDER, 'ORDHDR') AND COMPLETE > ldRpPrDt
    llHasTrns = FOUND()
  ENDIF

  IF !llHasTrns
    *- Check if the Style has Orders or Contracts after the purge date
    =SEEK(lcSeekKey,'POSLN')
    SELECT POSLN
    LOCATE REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = "0001"+lcSeekKey ;
              FOR CBUSDOCU+CSTYTYPE $ 'PPU' AND dTrDate > ldRpPrDt
    llHasTrns = FOUND()
  ENDIF

  IF !llHasTrns
    SELECT (lcActStyle)
    SCATTER MEMO MEMVAR
    SELECT (lcHstStyle)
    =SEEK(&lcActStyle..STYLE)
    IF !FOUND()
      =gfAppend('', .T.)
    ELSE
      GATHER MEMO MEMVAR
      =gfReplace('')
    ENDIF
    SELECT (lcActStyle)
    =gfDelete()
  ENDIF
ENDSCAN

SELECT (lcActStyle)
=gfTableUpdate()
SELECT (lcHstStyle)
=gfTableUpdate()

SELECT (lcActStyle)
lcTbl = DBF(lcActStyle)
gfCloseTable(lcActStyle)
gfCloseTable(lcHstStyle)
gfCloseTable('STYDYE')
gfCloseTable('STYINVJL')
gfCloseTable('ORDLINE')
gfCloseTable('ORDHDR')
gfCloseTable('POSLN')
gfOpenFile(lcTbl,'','EX',lcActStyle)
SELECT (lcActStyle)
PACK
USE IN (lcActStyle)
=gfCloseTable(lcActStyle)


ENDFUNC
*- End of lfPurgStyMstr


*!*************************************************************
*! Name      : lfUpdateFiles
*! Developer : Saber A Razek (SAB)
*! Date      : 09/03/2012
*! Purpose   : Call gfTableUpdate() for the purged tables
*!*************************************************************
FUNCTION lfUpdateFiles
PARAMETER lcTrn, lnFrsFl

*-SAB ----- [Start]
LOCAL lnI, llRetValue, lcActCompany
lnI = 1
llRetValue = .F.
lcActCompany = oAriaApplication.ActiveCompanyID
*B610942,3 MMT 02/18/2015 Error while purging Key off in the 2nd purge run[T20141111.0011][Start]
lcOldDataDir = oAriaApplication.DataDir
*B610942,3 MMT 02/18/2015 Error while purging Key off in the 2nd purge run[T20141111.0011][End]
DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1       && get file name posation
  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos) && get file name
  IF USED(laNeedFls[lnI,4])
    IF '\'+UPPER(ALLTRIM(lcRpComp))+'\' $ UPPER(laNeedFls[lnI,2])
    
      *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*            oAriaApplication.ActiveCompanyID = lcRpComp
      oAriaApplication.Ref4.ActiveCompanyID = lcRpComp
     *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
      
      oAriaApplication.GetCompanyInformation(lcRpComp)
      SELECT (laNeedFls[lnI,4])
      llRetValue = gfTableUpdate()
    ELSE
      oAriaApplication.Ref4.ActiveCompanyID = lcHCmpCod
      oAriaApplication.GetCompanyInformation(lcHCmpCod)
      SELECT (laNeedFls[lnI,4])
      llRetValue = gfTableUpdate()
    ENDIF
  ENDIF
  lnI = lnI + 1
  IF !llRetValue
    EXIT
  ENDIF
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][Start]
  IF lnI > ALEN(laNeedFls,1)
    EXIT
  ENDIF
  *B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][End]
ENDDO

*SET STEP ON 


*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*oAriaApplication.ActiveCompanyID = lcActCompany
*B610942,3 MMT 02/18/2015 Error while purging Key off in the 2nd purge run[T20141111.0011][Start]
*oAriaApplication.DataDir = lcOldDataDir 
*B610942,3 MMT 02/18/2015 Error while purging Key off in the 2nd purge run[T20141111.0011][End]
 oAriaApplication.Ref4.ActiveCompanyID = lcActCompany
     
      oAriaApplication.GetCompanyInformation(lcActCompany)
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]




*!*	LOCAL lnI, lcTranCode, llRetValue, lcActCompany
*!*	lnI = 1
*!*	lcActCompany = oAriaApplication.ActiveCompanyID
*!*	llRetValue = .F.

*!*	oAriaApplication.ActiveCompanyID = lcRpComp
*!*	oAriaApplication.GetCompanyInformation(lcRpComp)
*!*	lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
*!*	oAriaApplication.ActiveCompanyID = lcHCmpCod
*!*	oAriaApplication.GetCompanyInformation(lcHCmpCod)
*!*	lcHTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')

*!*	lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran("Driver={SQL Server};server=(local)\ARIASQL;DATABASE=SHA13_LDBCG;uid=demo;pwd=demo",3,'')
*!*	lcHTranCode = oAriaApplication.RemoteCompanyData.BeginTran("Driver={SQL Server};server=(local)\ARIASQL;DATABASE=HIST_LDB99;uid=demo;pwd=demo",3,'')

*!*	DO WHILE laNeedFls[lnI,1] = lcTrn  && loop for files of transaction
*!*	  lnFilePos = RAT('\',laNeedFls[lnI,2]) + 1       && get file name posation
*!*	  lcFileName = SUBSTR(laNeedFls[lnI,2],lnFilePos) && get file name
*!*	  IF USED(laNeedFls[lnI,4])
*!*	    IF '\'+ALLTRIM(UPPER(lcHCmpCod))+'\' $ UPPER(laNeedFls[lnI,2])
*!*	      oAriaApplication.ActiveCompanyID = lcRpComp
*!*	      oAriaApplication.GetCompanyInformation(lcRpComp)
*!*	      SELECT (laNeedFls[lnI,4])
*!*	      llRetValue = gfTableUpdate(lcTranCode)
*!*	    ELSE
*!*	      oAriaApplication.ActiveCompanyID = lcHCmpCod
*!*	      oAriaApplication.GetCompanyInformation(lcHCmpCod)
*!*	      SELECT (laNeedFls[lnI,4])
*!*	      llRetValue = gfTableUpdate(lcHTranCode)
*!*	    ENDIF
*!*	  ENDIF
*!*	  lnI = lnI + 1
*!*	  IF !llRetValue
*!*	    EXIT
*!*	  ENDIF
*!*	ENDDO
*!*	IF llRetValue
*!*	  oAriaApplication.ActiveCompanyID = lcRpComp
*!*	  oAriaApplication.GetCompanyInformation(lcRpComp)
*!*	  llRetValue = (oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)=1)  AND llRetValue
*!*	  oAriaApplication.ActiveCompanyID = lcHCmpCod
*!*	  oAriaApplication.GetCompanyInformation(lcHCmpCod)
*!*	  llRetValue = (oAriaApplication.RemoteCompanyData.CommitTran(lcHTranCode)=1)  AND llRetValue
*!*	ELSE
*!*	  oAriaApplication.ActiveCompanyID = lcRpComp
*!*	  oAriaApplication.GetCompanyInformation(lcRpComp)
*!*	  llRetValue = (oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)=1)
*!*	  oAriaApplication.ActiveCompanyID = lcHCmpCod
*!*	  oAriaApplication.GetCompanyInformation(lcHCmpCod)
*!*	  llRetValue = (oAriaApplication.RemoteCompanyData.RollBackTran(lcHTranCode)=1)
*!*	ENDIF

*!*	oAriaApplication.ActiveCompanyID = lcActCompany
*-SAB ----- [End]

ENDFUNC
*--- end of lfUpdateFiles.


************************************************************
*! Name      : lfBuildTrans
*! Developer : Saber A Razek (SAB)
*! Date      : 09/03/2012
*! Purpose   : Build laTransact Array
************************************************************
FUNCTION lfBuildTrans

*!*	*laTransact[*,1] = Transaction Name
*!*	laTransact[1,1]  = 'Invoice Details'
*!*	laTransact[2,1]  = 'Keyed Off Transactions'
*!*	laTransact[3,1]  = 'Sales Order Details'
*!*	laTransact[4,1]  = 'SalesRep Commission'
*!*	laTransact[5,1]  = 'Return Authorization'
*!*	laTransact[6,1]  = 'Return Details'

*!*	*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*!*	*laTransact[7,1]  = 'Style Inventory Adjustments'
*!*	laTransact[7,1]  = 'Style Inventory Journal'
*!*	*laTransact[8,1]  = 'Fabric Inventory Adjustments'
*!*	laTransact[8,1]  = 'Fabric Inventory Journal'

*!*	*B608475,1 MHM [End]

*!*	laTransact[9,1]  = 'Cutting Tickets Details'
*!*	laTransact[10,1] = 'Style Purchase Order'
*!*	laTransact[11,1] = 'Material Purchase Order'
*!*	laTransact[12,1] = 'Material Manufacturing Order'
*!*	laTransact[13,1] = 'A/P Invoice'
*!*	laTransact[14,1] = 'Picking Tickets'
*!*	laTransact[15,1] = 'Point of Sale'
*!*	laTransact[16,1] = 'Bill of Lading'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	*-- Add New procedure to the purge call Purge Rolles.
*!*	laTransact[17,1] = 'Purge Rolls'
*!*	*E301816,1 ABD - [End]

*!*	*laTransact[*,2] = Needed Module
*!*	laTransact[1,2]  = 'AR'
*!*	laTransact[2,2]  = 'AR'
*!*	laTransact[3,2]  = 'SO'
*!*	laTransact[4,2]  = 'SR'
*!*	laTransact[5,2]  = 'RM'
*!*	laTransact[6,2]  = 'RM'
*!*	laTransact[7,2]  = 'IC'
*!*	laTransact[8,2]  = 'MA'
*!*	laTransact[9,2]  = 'MF'
*!*	laTransact[10,2] = 'PO'
*!*	laTransact[11,2] = 'MA'
*!*	laTransact[12,2] = 'MA'
*!*	laTransact[13,2] = 'AP'
*!*	laTransact[14,2] = 'AL'
*!*	laTransact[15,2] = 'PS'
*!*	laTransact[16,2] = 'EB'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	laTransact[17,2] = 'MA'
*!*	*E301816,1 ABD - [End]


*!*	*laTransact[*,3]  = Selected (.T./.F.)

*!*	*laTransact[*,4] = Needed Files for Transaction
*!*	*Each file name end with ',' and the last file also must end with ','
*!*	*Some files repated for seek it with more than one seek expration
*!*	*the sorting of files is very important to determained the related files of any file
*!*	*the parant file most be before its chaild
*!*	laTransact[1,4]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DEBIT,UNCMSESS,NOTEPAD,NOTEPAD,'+;
*!*	                   'NOTEPAD,BOM,SALESREP,ARCUSHST,STYINVJL,'
*!*	laTransact[2,4]  = 'ARHIST,UNCMSESS,CUSTOMER,SALESREP,ARCUSHST,'
*!*	laTransact[3,4]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTPICK,CUTPICK,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,UNCMSESS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
*!*	                   'BOM,SALESREP,ARCUSHST,'
*!*	laTransact[4,4]  = 'REPCOMM,SALESREP,UNCMSESS,'
*!*	laTransact[5,4]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ARCUSHST,'
*!*	laTransact[6,4]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CREDIT,UNCMSESS,BOM,'+;
*!*	                   'SALESREP,ARCUSHST,STYINVJL,'
*!*	laTransact[7,4]  = 'STYINVJL,INVTADJ,UNCMSESS,STYLE,STYDYE,NOTEPAD,BOM,'
*!*	laTransact[8,4]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,ROLLS,'
*!*	laTransact[9,4]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMCOST,'+;
*!*	                   'STYLE,STYDYE,UNCMSESS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,STYINVJL,'
*!*	laTransact[10,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,'+;
*!*	                   'STYLE,STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,STYINVJL,'+;
*!*	                   'MFGOPRHD,MFGOPRDT,'
*!*	laTransact[11,4] = 'POFHDR,POFLN,FABRIC,FABDYE,APVENDOR,NOTEPAD,NOTEPAD,ROLLS,MATINVJL,'
*!*	laTransact[12,4] = 'MMFGORDH,MMFGORDD,CTKTBOM,BOMLINE,BOMCOST,MFGOPRHD,MFGOPRDT,'+;
*!*	                   'FABRIC,FABDYE,UNCMSESS,ROLLS,MATINVJL,'
*!*	laTransact[13,4] = 'APINVHDR,APVINVDT,APINVTKT,APDIST,APPAYMNT,APVENDOR,'
*!*	laTransact[14,4] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,UNCMSESS,BOM,'
*!*	laTransact[15,4] = 'POSTRAN,UNCMSESS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
*!*	                   'ARHIST,CREDIT,DEBIT,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
*!*	                   'BOM,SALESREP,ARCUSHST,STYINVJL,'
*!*	laTransact[16,4] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty , Open
*!*	*E301816,1 ABD - Needed files. [Begin]
*!*	laTransact[17,4] = 'MATINVJL,FABRIC,FABDYE,ROLLS,'
*!*	*E301816,1 ABD - [End]


*!*	*laTransact[*,5] = Indexes of Needed Files
*!*	*index of each file in laTransact[*,4]
*!*	*-SAB ----- [Start]
*!*	*laTransact[1,5]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DRTRAN,TRANS,NOTEPAD,NOTEPAD,'+;
*!*	                   'NOTEPAD,BOM,SALESREP,ACTHST,MFGOPR,'
*!*	laTransact[1,5]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DRTRAN,TRANS,NOTEPAD,NOTEPAD,'+;
*!*	                   'NOTEPAD,MULTIBOM,SALESREP,ACTHST,MFGOPR,'
*!*	*-SAB ----- [End]
*!*	laTransact[2,5]  = 'ARHISTHT,TRANS,CUSTOMER,SALESREP,ACTHST,'
*!*	laTransact[3,5]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTORD,CUTORD,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,TRANS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
*!*	                   'BOM,SALESREP,ACTHST,'
*!*	laTransact[4,5]  = 'REPCOMM,SALESREP,TRANS,'
*!*	laTransact[5,5]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ACTHST,'
*!*	laTransact[6,5]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CRTRAN,TRANS,BOM,'+;
*!*	                   'SALESREP,ACTHST,MFGOPR,'
*!*	laTransact[7,5]  = 'STYINVJL,INVTADJ,TRANS,STYLE,STYDYE,NOTEPAD,BOM,'

*!*	*B604743,1 MHM 08/15/2001 change index of ROLLS file [Start]
*!*	*laTransact[8,5]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,ROLLITEM,'
*!*	laTransact[8,5]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,SESSION,'
*!*	*B604743,1 MHM 08/15/2001 [End]

*!*	laTransact[9,5]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,POBOMCLS,'+;
*!*	                   'STYLE,STYDYE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,MFGOPR,'
*!*	laTransact[10,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
*!*	                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
*!*	laTransact[11,5] = 'POFHDR,POFLN,FABRIC,FABDYE,VENCODE,NOTEPAD,NOTEPAD,ROLLITEM,MFGOPR,'
*!*	laTransact[12,5] = 'MMFGORDH,MMFGORDD,CTKTBOM,MFGOPR,POBOMCLS,MFGOPRHD,MFGOPRDT,'+;
*!*	                   'FABRIC,FABDYE,TRANS,ROLLITEM,MFGOPR,'
*!*	laTransact[13,5] = 'VENDINV,ORGVINV,LNCONT,INVVEND,TYPMETHDOC,VENCODE,'
*!*	laTransact[14,5] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,TRANS,BOM,'
*!*	laTransact[15,5] = 'POSTRAN,TRANS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
*!*	                   'ARHISTT,CRTRAN,DRTRAN,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
*!*	                   'BOM,SALESREP,ACTHST,MFGOPR,'
*!*	laTransact[16,5] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty , open files
*!*	*E301816,1 ABD - With the Following Index. [Begin]
*!*	laTransact[17,5] = 'MTINVSEQ,FABRIC,FABDYE,SESSION,'
*!*	*E301816,1 ABD - [End]

*!*	*laTransact[*,6] = Status of Needed Files
*!*	*Status of needed files determined the opration will done with each file
*!*	*A=Append , D=Delete , B=Append and Delete , U=Update , N=Open only
*!*	laTransact[1,6]  = 'B,B,B,B,B,B,B,A,A,A,A,N,D,B,B,B,A,A,A,N,'
*!*	laTransact[2,6]  = 'B,D,A,A,A,'
*!*	laTransact[3,6]  = 'B,B,B,B,B,U,U,U,U,A,A,A,A,B,D,B,B,B,A,A,A,'
*!*	laTransact[4,6]  = 'B,A,D,'
*!*	laTransact[5,6]  = 'B,B,A,A,A,A,A,A,A,'
*!*	laTransact[6,6]  = 'B,B,A,A,A,A,N,D,A,A,A,N,'
*!*	laTransact[7,6]  = 'B,B,D,A,A,A,A,'
*!*	laTransact[8,6]  = 'B,B,A,A,A,A,'
*!*	laTransact[9,6]  = 'B,B,B,U,U,B,B,B,A,A,D,B,A,A,B,B,N,'
*!*	laTransact[10,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
*!*	laTransact[11,6] = 'B,B,A,A,A,B,A,A,N,'
*!*	laTransact[12,6] = 'B,B,B,B,B,B,B,A,A,D,A,N,'
*!*	laTransact[13,6] = 'B,B,B,B,B,A,'
*!*	laTransact[14,6] = 'B,B,U,A,A,A,A,D,A,'
*!*	laTransact[15,6] = 'B,D,B,B,B,B,B,B,B,B,N,N,A,A,A,A,A,A,A,A,N,'
*!*	laTransact[16,6] = 'B,B,B,B,B,'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	laTransact[17,6] = 'B,B,B,B,'
*!*	*E301816,1 ABD - [End]

*!*	*laTransact[*,7] = Related Files
*!*	*for each file in laTransact[*,4] determaind the number of related file by its sort in
*!*	*laTransact[*,4] , the end of each number determined by '-' and the end of each file
*!*	*determined by ','
*!*	*for each chaild file we determained its parant file number
*!*	*if the file is not chaild of any file we tack his number as related file
*!*	*ex1. (the file number 1 not chaild of any file and file number 2 is chaild of 1 = '1-,1-,')
*!*	*ex2. (the file number 1 not chaild of any file and file number 2 also = '1-,2-,')
*!*	*ex3. (the file number 1 not chaild of any file and file number 2 is chaild of 1
*!*	*      and file number 3 is chaild of 1 and 2 = '1-,1-,1-2-,')
*!*	laTransact[1,7]  = '1-,1-,3-,3-,1-,1-,1-,2-4-,2-4-,1-2-3-4-,1-2-3-4-,12-,13-,1-,8-,10-11-,8-,'+;
*!*	                   '10-10-,10-,1-3-,'
*!*	laTransact[2,7]  = '1-,2-,1-1-,3-3-,3-,'
*!*	laTransact[3,7]  = '1-,1-,1-,1-,1-,4-,4-,5-,5-,2-,2-,1-,1-,1-,15-,1-,10-,12-13-,10-,1-1-12-12-,'+;
*!*	                   '12-,'
*!*	laTransact[4,7]  = '1-,1-,3-,'
*!*	laTransact[5,7]  = '1-,1-,2-,2-,1-,1-,3-,5-5-,5-,'
*!*	laTransact[6,7]  = '1-,1-,2-,2-,1-,1-,7-,8-,3-,5-5-,5-,1-,'
*!*	laTransact[7,7]  = '1-,2-,3-,1-,1-,4-,4-,'
*!*	laTransact[8,7]  = '1-,2-,1-,1-,3-,3-,'
*!*	laTransact[9,7]  = '1-,1-,1-,3-,3-,1-,1-,1-,2-,2-,11-,1-,9-,9-,1-,1-,1-,'
*!*	laTransact[10,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
*!*	laTransact[11,7] = '1-,1-,2-,2-,1-,1-,3-,3-,1-,'
*!*	laTransact[12,7] = '1-,1-,1-,1-,1-,1-,1-,2-,2-,10-,8-,1-,'
*!*	laTransact[13,7] = '1-,1-,1-,1-,5-,1-,'
*!*	laTransact[14,7] = '1-,1-,2-,1-,2-,2-,2-,8-,6-,'
*!*	laTransact[15,7] = '1-,2-,1-,3-,3-,3-,6-,1-,8-,1-,11-,12-,1-,1-,1-,13-,15-,13-,15-,15-,3-8-,'
*!*	laTransact[16,7] = '1-,1-,3-,3-,5-,'


*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	laTransact[17,7] = '1-,1-,1-,2-,'
*!*	*E301816,1 ABD - [End]

*!*	*laTransact[*,8] = Seek Expration for relation
*!*	*for each chaild file determain the seek expration for relation with parant
*!*	*as in laTransact[*,7] if the file is not chaild of any file we tack its number
*!*	laTransact[1,8]  = '1-,INVOICE-,3-,INVOICE-,INVOICE-,INVOICE-,INVOICE-,STYLE-STYLE-,STYLE-STYLE-,'+;
*!*	                   '"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-,'+;
*!*	                   '"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-,12-,13-,'+;
*!*	                   '"C"+INVOICE-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'+;
*!*	                   'INVOICE-INVOICE-,'
*!*	laTransact[2,8]  = '1-,2-,"M"+ACCOUNT-"S"+ACCOUNT-,SALESREP-REP2-,ACCOUNT-,'
*!*	laTransact[3,8]  = '1-,CORDTYPE+ORDER-,CORDTYPE+ORDER-,"1"+ORDER-,"2"+ORDER-,CTKTNO-,CTKTNO+STYLE-,'+;
*!*	                   '"P"+CTKTNO-,"P"+CTKTNO+STYLE-,STYLE-STYLE-,STYLE-STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'+;
*!*	                   '"SO"+ORDER-,15-,"B"+ORDER-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,CSTYMAJOR-,'+;
*!*	                   'REP1-REP2-SALESREP-REP2-,ACCOUNT-,'
*!*	laTransact[4,8]  = '1-,REPCODE-,3-,'
*!*	laTransact[5,8]  = '1-,RANO-,STYLE-,STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'
*!*	laTransact[6,8]  = '1-,CRMEMO-,STYLE-,STYLE-,"M"+ACCOUNT-,'+;
*!*	                   '"S"+ACCOUNT+STORE-,7-,8-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,CRMEMO-,'
*!*	laTransact[7,8]  = '1-,2-,3-,STYLE-,STYLE-,"F"+STYLE-,CSTYMAJOR-,'
*!*	laTransact[8,8]  = '1-,2-,CFABRIC+CCOLOR-,CFABRIC+CCOLOR-,"G"+FABRIC-,FABRIC-,'
*!*	laTransact[9,8]  = '1-,CUTTKT-,"1"+CUTTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"M"+CUTTKT-,"M"+CUTTKT-,"M"+CUTTKT-,'+;
*!*	                   'STYLE-,STYLE-,11-,"I"+CUTTKT-,"F"+STYLE-,CSTYMAJOR-,"M"+CUTTKT-,"M"+CUTTKT-,CUTTKT-,'
*!*	laTransact[10,8] = '1-,CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
*!*	                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
*!*	laTransact[11,8] = '1-,CMATTYPE+POMAT-,FABRIC+COLOR-,FABRIC+COLOR-,VENDOR-,"M"+POMAT-,"G"+FABRIC-,FABRIC-,POMAT-,'
*!*	laTransact[12,8] = '1-,CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,'+;
*!*	                   'CFABRIC+COLOR-,CFABRIC+COLOR-,10-,FABRIC-,CMFGORDNO-,'
*!*	*B608086,1 TMI [Start] when seeking the Ap invoice in APINVHDR seek in APDIST with the key CINVNO+CVENDCODE
*!*	*laTransact[13,8] = '1-,CVENDCODE+CINVNO-,CVENDCODE+CINVNO-,CINVNO-,5-,CVENDCODE-,'
*!*	laTransact[13,8] = '1-,CVENDCODE+CINVNO-,CVENDCODE+CINVNO-,CINVNO+CVENDCODE-,5-,CVENDCODE-,'
*!*	*B608086,1 TMI [End  ]

*!*	laTransact[14,8] = '1-,PIKTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"O"+ORDER+STYLE-,STYLE-,STYLE-,8-,CSTYMAJOR-,'
*!*	laTransact[15,8] = '1-,2-,TRAN-,INVOICE-,INVOICE-,"O"+ORDER-,CORDTYPE+ORDER-,TRAN-,CRMEMO-,ACCOUNT+TRAN-,'+;
*!*	                   '11-,12-,STYLE-,STYLE-,"M"+ACCOUNT-,"F"+STYLE-,"A"+ACCOUNT-,'+;
*!*	                   'CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,INVOICE-CRMEMO-,'
*!*	laTransact[16,8] = '1-,BOL_NO-,3-,PACK_NO-,5-,'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	laTransact[17,8] = '1-,FABRIC+COLOR-,FABRIC+COLOR-,4-,'
*!*	*E301816,1 ABD - [End]

*!*	*laTransact[*,9] = Scan For Expration
*!*	*determain the scan for expration for each file
*!*	laTransact[1,9]  = 'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,'+;
*!*	                   'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "IINVOICE" .OR. CUTRANTYPE = "DINVOICE"),'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,'

*!*	*B608062,1 NNA 04/29/2007 (Begin) Check the HistDate instead of TranDate with the Keyed off Transaction
*!*	*laTransact[2,9]  = 'TRANDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "KEYOFF",.T.,.T.,.T.,'
*!*	laTransact[2,9]  = 'HISTDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "KEYOFF",.T.,.T.,.T.,'
*!*	*B608062,1 NNA (END)

*!*	laTransact[3,9]  = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SOORD" .OR. CUTRANTYPE = "SMMASCA"),.T.,.T.,.T.,'+;
*!*	                   '.T.,.T.,.T.,'
*!*	laTransact[4,9]  = 'DATE<=ldRpPrDt,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SR_PAY" .OR. CUTRANTYPE = "SR_DR" .OR. CUTRANTYPE = "SR_CR"),'
*!*	laTransact[5,9]  = 'RADATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	laTransact[6,9]  = 'CRDATE<=ldRpPrDt .AND. !SEEK("0"+CRMEMO~"CREDIT"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "CRDT_MEMO",.T.,.T.,.T.,.T.,'

*!*	*B608475,1 MHM 03/09/2008 Modify Purge program to purge style inventory for all types[Start]
*!*	*laTransact[7,9]  = 'DTRDATE<=ldRpPrDt .AND. CTRTYPE$"12",DATE<=ldRpPrDt,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "INVLOCK_M" .OR. CUTRANTYPE = "INVLOCK_S"),.T.,.T.,.T.,.T.,'
*!*	laTransact[7,9]  = 'DTRDATE<=ldRpPrDt ,DATE<=ldRpPrDt,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "INVLOCK_M" .OR. CUTRANTYPE = "INVLOCK_S"),.T.,.T.,.T.,.T.,'
*!*	*B608475,1 MHM [End]

*!*	*B604664 MHM 08/15/2001 include all trantype in purge program[START]
*!*	*laTransact[8,9]  = 'DTRANDATE<=ldRpPrDt .AND. CTRANTYPE$"23",DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
*!*	laTransact[8,9]  = 'DTRANDATE<=ldRpPrDt ,DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
*!*	*B604743,1 MHM 08/15/2001 [END]

*!*	*T20070514.0034 TMI [Start] *- purge only cuttickets with colsed date before the purge date
*!*	*laTransact[9,9]  = 'COMPLETE<=ldRpPrDt .AND. STATUS$"SX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	laTransact[9,9]  = '(STATUS="S" .AND. DEDIT_DATE<=ldRpPrDt).OR. (STATUS="X" .AND. COMPLETE<=ldRpPrDt),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	*T20070514.0034 TMI [End  ]

*!*	*B127368,1 EIH 06/26/2005 Include status (S) closed po's in criteria [Begin].
*!*	*laTransact[10,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	laTransact[10,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXS",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	*B127368,1 EIH [End].

*!*	
*!*	*B608059,1 TMI [Start] purge the Closed Material Paurchase order
*!*	*laTransact[11,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	laTransact[11,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXL",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	*B608059,1 TMI [End  ]
*!*	laTransact[12,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"SX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "MAMFORD",.T.,.T.,'
*!*	*B607933,1 TMI [Start] add a condition to be applied to the apdist file
*!*	*laTransact[13,9] = 'DINVDATE<=ldRpPrDt .AND. NINVAMNT=NINVPAID+NINVDISTK+NINVADJ,.T.,.T.,.T.,DPAYDATE<=ldRpPrDt,.T.,'
*!*	laTransact[13,9] = 'DINVDATE<=ldRpPrDt .AND. NINVAMNT=NINVPAID+NINVDISTK+NINVADJ,.T.,.T.,DAPDTRDAT<=ldRpPrDt,DPAYDATE<=ldRpPrDt,.T.,'
*!*	*B607933,1 TMI [End  ]
*!*	laTransact[14,9] = 'DATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,'+;
*!*	                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"ALORDAL\ALAUTAL\ALRELPI\PACKLIST",.T.,'
*!*	laTransact[15,9] = 'TRANDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "PNTOSALE",'+;
*!*	                   '!SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,!SEEK("0"+CRMEMO~"CREDIT"),'+;
*!*	                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*!*	laTransact[16,9] = 'SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,CANCELLED<=ldRpPrDt,'

*!*	*E301816,1 ABD - Add the ability to delete rolls that have zero qty. [Begin]
*!*	laTransact[17,9] = '.T.,.T.,DTRANDATE<=ldRpPrDt,.T.,'
*!*	*E301816,1 ABD - [End]

*laTransact[*,1] = Transaction Name
*laTransact[*,2] = Needed Module
*laTransact[*,3] = Selected (.T./.F.)
*laTransact[*,4] = Needed Files for Transaction
*          Each file name end with ',' and the last file also must end with ','
*          Some files repated for seek it with more than one seek expration
*          the sorting of files is very important to determained the related files of any file
*          the parant file most be before its chaild
*laTransact[*,5] = Indexes of Needed Files
*          index of each file in laTransact[*,4]
*laTransact[*,6] = Status of Needed Files
*          Status of needed files determined the opration will done with each file
*          A=Append , D=Delete , B=Append and Delete , U=Update , N=Open only
*laTransact[*,7] = Related Files
*          for each file in laTransact[*,4] determaind the number of related file by its sort in
*          laTransact[*,4] , the end of each number determined by '-' and the end of each file determined by ','
*          for each chaild file we determained its parant file number
*          if the file is not chaild of any file we tack his number as related file
*          ex1. (the file number 1 not chaild of any file and file number 2 is chaild of 1 = '1-,1-,')
*          ex2. (the file number 1 not chaild of any file and file number 2 also = '1-,2-,')
*          ex3. (the file number 1 not chaild of any file and file number 2 is chaild of 1
*                and file number 3 is chaild of 1 and 2 = '1-,1-,1-2-,')
*laTransact[*,8] = Seek Expration for relation
*          for each chaild file determain the seek expration for relation with parant
*          as in laTransact[*,7] if the file is not chaild of any file we tack its number
*laTransact[*,9] = Scan For Expration
*          determain the scan for expration for each file
****************************************************** Invoice Details **********************************************
*- All Tables are Fox Except (BOM)
laTransact[1,1]  = 'Invoice Details'
laTransact[1,2]  = 'AR'
laTransact[1,4]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DEBIT,UNCMSESS,NOTEPAD,NOTEPAD,'+;
                   'NOTEPAD,BOM,SALESREP,ARCUSHST,STYINVJL,'
*-SAB ----- [Start]
*laTransact[1,5]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DRTRAN,TRANS,NOTEPAD,NOTEPAD,'+;
                   'NOTEPAD,BOM,SALESREP,ACTHST,MFGOPR,'
laTransact[1,5]  = 'INVHDR,INVLINE,CONSINVH,CONSINVL,ARINSTMH,ARINSTMD,INVCHRG,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,DRTRAN,TRANS,NOTEPAD,NOTEPAD,'+;
                   'NOTEPAD,MULTIBOM,SALESREP,ACTHST,MFGOPR,'
*-SAB ----- [End]
laTransact[1,6]  = 'B,B,B,B,B,B,B,A,A,A,A,N,D,B,B,B,A,A,A,N,'
laTransact[1,7]  = '1-,1-,3-,3-,1-,1-,1-,2-4-,2-4-,1-2-3-4-,1-2-3-4-,12-,13-,1-,8-,10-11-,8-,10-10-,10-,1-3-,'
*-SAB ----- [Start]
*laTransact[1,8]  = '1-,INVOICE-,3-,INVOICE-,INVOICE-,INVOICE-,INVOICE-,STYLE-STYLE-,STYLE-STYLE-,'+;
                   '"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-,12-,13-,'+;
                   '"C"+INVOICE-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'+;
                   'INVOICE-INVOICE-,'
laTransact[1,8]  = '1-,INVOICE-,3-,INVOICE-,INVOICE-,INVOICE-,INVOICE-,STYLE-STYLE-,STYLE-STYLE-,'+;
                   '"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-"S"+ACCOUNT+STORE-,12-,13-,'+;
                   '"C"+INVOICE-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,"0001"+CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'+;
                   'INVOICE-INVOICE-,'
*-SAB ----- [End]
laTransact[1,9]  = 'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,'+;
                   'INVDATE<=ldRpPrDt .AND. !SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "IINVOICE" .OR. CUTRANTYPE = "DINVOICE"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,'

****************************************************** Keyed Off Transactions ***************************************
*- All Tables are Fox
laTransact[2,1]  = 'Keyed Off Transactions'
laTransact[2,2]  = 'AR'
laTransact[2,4]  = 'ARHIST,UNCMSESS,CUSTOMER,SALESREP,ARCUSHST,'
laTransact[2,5]  = 'ARHISTHT,TRANS,CUSTOMER,SALESREP,ACTHST,'
laTransact[2,6]  = 'B,D,A,A,A,'
laTransact[2,7]  = '1-,2-,1-1-,3-3-,3-,'
laTransact[2,8]  = '1-,2-,"M"+ACCOUNT-"S"+ACCOUNT-,SALESREP-REP2-,ACCOUNT-,'
laTransact[2,9]  = 'HISTDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "KEYOFF",.T.,.T.,.T.,'

****************************************************** Sales Order Details ******************************************
*- All Tables are Fox Except (BOM - POSHDR - POSLN - CUTPICK)
laTransact[3,1]  = 'Sales Order Details'
laTransact[3,2]  = 'SO'

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*laTransact[3,4]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTPICK,CUTPICK,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,UNCMSESS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ARCUSHST,'
laTransact[3,4]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTPICK,CUTPICK,POSHDR,POSLN,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,UNCMSESS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ARCUSHST,'                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]      
          
*-SAB ----- [Start]
*laTransact[3,5]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTORD,CUTORD,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,TRANS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ACTHST,'
                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]                  
*!*	laTransact[3,5]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTORD,CUTORD,CUTTKTH,CUTTKTL,POSHDR,POSLN,'+;
*!*	                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,TRANS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
*!*	                   'MULTIBOM,SALESREP,ACTHST,'
laTransact[3,5]  = 'ORDHDR,ORDLINE,ORDCANLN,CUTORD,CUTORD,POSHDR,POSLN,POSHDR,POSLN,'+;
                   'STYLE,STYDYE,CUSTOMER,CUSTOMER,BOMVAR,TRANS,NOTEPAD,NOTEPAD,NOTEPAD,'+;
                   'MULTIBOM,SALESREP,ACTHST,'
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End] 
               
*-SAB ----- [End]
laTransact[3,6]  = 'B,B,B,B,B,U,U,U,U,A,A,A,A,B,D,B,B,B,A,A,A,'
laTransact[3,7]  = '1-,1-,1-,1-,1-,4-,4-,5-,5-,2-,2-,1-,1-,1-,15-,1-,10-,12-13-,10-,1-1-12-12-,12-,'
*-SAB ----- [Start]
*laTransact[3,8]  = '1-,CORDTYPE+ORDER-,CORDTYPE+ORDER-,"1"+ORDER-,"2"+ORDER-,CTKTNO-,CTKTNO+STYLE-,'+;
                   '"P"+CTKTNO-,"P"+CTKTNO+STYLE-,STYLE-STYLE-,STYLE-STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'+;
                   '"SO"+ORDER-,15-,"B"+ORDER-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,"0001"+CSTYMAJOR-,'+;
                   'REP1-REP2-SALESREP-REP2-,ACCOUNT-,'
                   
 *B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*laTransact[3,8]  = '1-,CORDTYPE+ORDER-,CORDTYPE+ORDER-,"1"+ORDER-,"2"+ORDER-,CTKTNO-,CTKTNO+STYLE-,'+;
                   '"P"+"P"+CTKTNO-,"P"+"P"+CTKTNO+STYLE-,STYLE-STYLE-,STYLE-STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'+;
                   '"SO"+ORDER-,15-,"B"+ORDER-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,"0001"+CSTYMAJOR-,'+;
                   'REP1-REP2-SALESREP-REP2-,ACCOUNT-,'
laTransact[3,8]  = '1-,CORDTYPE+ORDER-,CORDTYPE+ORDER-,"1"+ORDER-,"2"+ORDER-,"P"+"U"+CTKTNO-,"P"+"U"+CTKTNO+STYLE-,'+;
                   '"P"+"P"+CTKTNO-,"P"+"P"+CTKTNO+STYLE-,STYLE-STYLE-,STYLE-STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'+;
                   '"SO"+ORDER-,15-,"B"+ORDER-,"F"+STYLE-,"A"+ACCOUNT-"A"+ACCOUNT-,"0001"+CSTYMAJOR-,'+;
                   'REP1-REP2-SALESREP-REP2-,ACCOUNT-,'
                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[3,9]  = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SOORD" .OR. CUTRANTYPE = "SMMASCA"),.T.,.T.,.T.,'+;
                   '.T.,.T.,.T.,'
laTransact[3,9]  = 'COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~ STATUS$"CX"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SOORD" .OR. CUTRANTYPE = "SMMASCA"),.T.,.T.,.T.,'+;
                   '.T.,.T.,.T.,'
*-SAB ----- [End]

****************************************************** SalesRep Commission ******************************************
*- All Tables are Fox
laTransact[4,1]  = 'SalesRep Commission'
laTransact[4,2]  = 'SR'
laTransact[4,4]  = 'REPCOMM,SALESREP,UNCMSESS,'
laTransact[4,5]  = 'REPCOMM,SALESREP,TRANS,'
laTransact[4,6]  = 'B,A,D,'
laTransact[4,7]  = '1-,1-,3-,'
laTransact[4,8]  = '1-,REPCODE-,3-,'
laTransact[4,9]  = 'DATE<=ldRpPrDt,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "SR_PAY" .OR. CUTRANTYPE = "SR_DR" .OR. CUTRANTYPE = "SR_CR"),'

****************************************************** Return Authorization *****************************************
*- All Tables are Fox Except (RETAUTH - RALINE - BOM)
laTransact[5,1]  = 'Return Authorization'
laTransact[5,2]  = 'RM'
laTransact[5,4]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ARCUSHST,'
*-SAB ----- [Start]
*laTransact[5,5]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,BOM,SALESREP,ACTHST,'
laTransact[5,5]  = 'RETAUTH,RALINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,MULTIBOM,SALESREP,ACTHST,'
*-SAB ----- [End]
laTransact[5,6]  = 'B,B,A,A,A,A,A,A,A,'
laTransact[5,7]  = '1-,1-,2-,2-,1-,1-,3-,5-5-,5-,'
*-SAB ----- [Start]
*laTransact[5,8]  = '1-,RANO-,STYLE-,STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'
laTransact[5,8]  = '1-,RANO-,STYLE-,STYLE-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,"0001"+CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[5,9]  = 'RADATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[5,9]  = 'RADATE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CX"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'
*-SAB ----- [End]

****************************************************** Return Details ***********************************************
*- All Tables are Fox Except (RETAUTH - RALINE - BOM)
laTransact[6,1]  = 'Return Details'
laTransact[6,2]  = 'RM'
laTransact[6,4]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CREDIT,UNCMSESS,BOM,'+;
                   'SALESREP,ARCUSHST,STYINVJL,'
*-SAB ----- [Start]
*laTransact[6,5]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CRTRAN,TRANS,BOM,'+;
                   'SALESREP,ACTHST,MFGOPR,'
laTransact[6,5]  = 'RETHDR,RETLINE,STYLE,STYDYE,CUSTOMER,CUSTOMER,CRTRAN,TRANS,MULTIBOM,'+;
                   'SALESREP,ACTHST,MFGOPR,'
*-SAB ----- [End]
laTransact[6,6]  = 'B,B,A,A,A,A,N,D,A,A,A,N,'
laTransact[6,7]  = '1-,1-,2-,2-,1-,1-,7-,8-,3-,5-5-,5-,1-,'
*-SAB ----- [Start]
*laTransact[6,8]  = '1-,CRMEMO-,STYLE-,STYLE-,"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-,7-,8-,CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,CRMEMO-,'
laTransact[6,8]  = '1-,CRMEMO-,STYLE-,STYLE-,"M"+ACCOUNT-,'+;
                   '"S"+ACCOUNT+STORE-,7-,8-,"0001"+CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,CRMEMO-,'
*-SAB ----- [End]

*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]

*laTransact[6,9]  = 'CRDATE<=ldRpPrDt .AND. !SEEK("0"+CRMEMO~"CREDIT"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "CRDT_MEMO",.T.,.T.,.T.,.T.,'
 laTransact[6,9]  = 'CRDATE<=ldRpPrDt .AND. !SEEK("0"+CRMEMO~"CREDIT"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
                  'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "CRDT_MEMO",.T.,.T.,.T.,CTRTYPE $ "78",'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]

****************************************************** Style Inventory Journal **************************************
*- All Tables are Fox Except (BOM)
laTransact[7,1]  = 'Style Inventory Journal'
laTransact[7,2]  = 'IC'
laTransact[7,4]  = 'STYINVJL,INVTADJ,UNCMSESS,STYLE,STYDYE,NOTEPAD,BOM,'
*-SAB ----- [Start]
*laTransact[7,5]  = 'STYINVJL,INVTADJ,TRANS,STYLE,STYDYE,NOTEPAD,BOM,'
laTransact[7,5]  = 'STYINVJL,INVTADJ,TRANS,STYLE,STYDYE,NOTEPAD,MULTIBOM,'
*-SAB ----- [End]
laTransact[7,6]  = 'B,B,D,A,A,A,A,'
laTransact[7,7]  = '1-,2-,3-,1-,1-,4-,4-,'
*-SAB ----- [Start]
*laTransact[7,8]  = '1-,2-,3-,STYLE-,STYLE-,"F"+STYLE-,CSTYMAJOR-,'
laTransact[7,8]  = '1-,2-,3-,STYLE-,STYLE-,"F"+STYLE-,"0001"+CSTYMAJOR-,'
*-SAB ----- [End]
laTransact[7,9]  = 'DTRDATE<=ldRpPrDt ,DATE<=ldRpPrDt,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "INVLOCK_M" .OR. CUTRANTYPE = "INVLOCK_S"),.T.,.T.,.T.,.T.,'

****************************************************** Fabric Inventory Journal *************************************
laTransact[8,1]  = 'Fabric Inventory Journal'
laTransact[8,2]  = 'MA'
*-SAB ----- [Start]
*laTransact[8,4]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,ROLLS,'
laTransact[8,4]  = 'ITEMJRNL,ITEMADJ,ITEM,ITEMLOC,NOTEPAD,ROLLS,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[8,5]  = 'MATINVJL,FINVTADJ,FABRIC,FABDYE,NOTEPAD,SESSION,'
laTransact[8,5]  = 'STYINVJL,INVTADJ,STYLE,STYDYE,NOTEPAD,ROLLITEM,'
*-SAB ----- [End]
laTransact[8,6]  = 'B,B,A,A,A,A,'
laTransact[8,7]  = '1-,2-,1-,1-,3-,3-,'
*-SAB ----- [Start]
*laTransact[8,8]  = '1-,2-,CFABRIC+CCOLOR-,CFABRIC+CCOLOR-,"G"+FABRIC-,FABRIC-,'
laTransact[8,8]  = 'CINVTYPE = "0002"-,CINVTYPE = "0002"-,CINVTYPE+STYLE-,CINVTYPE+STYLE-,"G"+STYLE-,STYLE-,'
*-SAB ----- [End]
*B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][Start]
*laTransact[8,9]  = 'DTRANDATE<=ldRpPrDt ,DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
laTransact[8,9]  = 'DTRDATE<=ldRpPrDt ,DATE<=ldRpPrDt,.T.,.T.,.T.,.T.,'
*B610930,1 MMT 01/18/2015 Cannot Purge Material Modules[T20141223.0006][End]

****************************************************** Cutting Tickets Details **************************************
*- All Tables are Fox Except (CUTPICK - CTKTBOM - BOMLINE - BOMCOST - BOM - MFGOPRHD - MFGOPRDT)
laTransact[9,1]  = 'Cutting Tickets Details'
laTransact[9,2]  = 'MF'
*-SAB ----- [Start]
*laTransact[9,4]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMCOST,'+;
                   'STYLE,STYDYE,UNCMSESS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,STYINVJL,'
laTransact[9,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,STYLE,'+;
                   'STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,STYINVJL,MFGOPRHD,MFGOPRDT,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[9,5]  = 'CUTTKTH,CUTTKTL,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,POBOMCLS,'+;
                   'STYLE,STYDYE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPRHD,MFGOPRDT,MFGOPR,'
laTransact[9,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,MULTIBOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[9,6]  = 'B,B,B,U,U,B,B,B,A,A,D,B,A,A,B,B,N,'
laTransact[9,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[9,7]  = '1-,1-,1-,3-,3-,1-,1-,1-,2-,2-,11-,1-,9-,9-,1-,1-,1-,'
laTransact[9,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
*-SAB ----- [End]

*-SAB ----- [Start]
*laTransact[9,8]  = '1-,CUTTKT-,"1"+CUTTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"M"+CUTTKT-,"M"+CUTTKT-,"M"+CUTTKT-,'+;
                   'STYLE-,STYLE-,11-,"I"+CUTTKT-,"F"+STYLE-,CSTYMAJOR-,"M"+CUTTKT-,"M"+CUTTKT-,CUTTKT-,'

*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]                 
*!*	laTransact[9,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
*!*	                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'

*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]

*laTransact[9,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"1"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"M"+PO-,"M"+PO-,"AD"+PO-,"M"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"I"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"M"+PO-,"M"+PO-,'
 laTransact[9,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"1"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"M"+PO-,"M"+PO-,"AD"+PO-,"M"+PO-,'+;
                  'STYLE-,STYLE-,VENDOR-,13-,"M"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"M"+PO-,"M"+PO-,'                  
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]
                
                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End] 

               
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[9,9]  = '(STATUS="S" .AND. DEDIT_DATE<=ldRpPrDt).OR. (STATUS="X" .AND. COMPLETE<=ldRpPrDt),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]
*laTransact[9,9] = 'CSTYTYPE+CBUSDOCU = "UP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
 laTransact[9,9] = 'CSTYTYPE+CBUSDOCU = "UP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                  'DTRANDATE<=ldRpPrDt .AND. (CUTRANTYPE = "CUTTKT" .OR. CUTRANTYPE = "CTFROMORD" .OR. CUTRANTYPE = "CTFROMPL"),'+;
                  '.T.,.T.,.T.,CTRTYPE $ "5I",.T.,.T.,'                  
 *B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]
                 
*-SAB ----- [End]

****************************************************** Style Purchase Order *****************************************
*- All Tables are Fox Except (POSHDR - POSLN - CUTPICK - CTKTBOM - BOMLINE - BOMCOST - BOM - MFGOPRHD - MFGOPRDT)
laTransact[10,1] = 'Style Purchase Order'
laTransact[10,2] = 'PO'
laTransact[10,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,'+;
                   'STYLE,STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,STYINVJL,'+;
                   'MFGOPRHD,MFGOPRDT,'
*-SAB ----- [Start]
*laTransact[10,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,BOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
laTransact[10,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,MULTIBOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
*-SAB ----- [End]
laTransact[10,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
laTransact[10,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
*-SAB ----- [Start]
*laTransact[10,8] = '1-,CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
laTransact[10,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[10,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXS",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
                   
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]              
*laTransact[10,9] = 'CSTYTYPE+CBUSDOCU = "PP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
laTransact[10,9] = 'CSTYTYPE+CBUSDOCU = "PP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                  'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"STYLE_PO\RECIVING\LC\ADJCFRCV\POFROMORD",'+;
                  '.T.,.T.,.T.,CTRTYPE $ "I6",.T.,.T.,'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]
             
*-SAB ----- [End]

****************************************************** Material Purchase Order **************************************
*- All Tables are Fox Except (ROLLS)
laTransact[11,1] = 'Material Purchase Order'
laTransact[11,2] = 'MA'
*-SAB ----- [Start]
*laTransact[11,4] = 'POFHDR,POFLN,FABRIC,FABDYE,APVENDOR,NOTEPAD,NOTEPAD,ROLLS,MATINVJL,'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]
*laTransact[11,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,STYLE,'+;
                   'STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,ITEMJRNL,MFGOPRHD,MFGOPRDT,'
 laTransact[11,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,ITEM,'+;
                  'ITEMLOC,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,ITEMJRNL,MFGOPRHD,MFGOPRDT,'                  
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]
                 
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[11,5] = 'POFHDR,POFLN,FABRIC,FABDYE,VENCODE,NOTEPAD,NOTEPAD,ROLLITEM,MFGOPR,'
laTransact[11,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,MULTIBOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[11,6] = 'B,B,A,A,A,B,A,A,N,'
laTransact[11,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[11,7] = '1-,1-,2-,2-,1-,1-,3-,3-,1-,'

laTransact[11,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[11,8] = '1-,CMATTYPE+POMAT-,FABRIC+COLOR-,FABRIC+COLOR-,VENDOR-,"M"+POMAT-,"G"+FABRIC-,FABRIC-,POMAT-,'

*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start] 
*laTransact[11,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
laTransact[11,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
                  '"0002"+STYLE-,"0002"+STYLE-,VENDOR-,13-,"MP"+PO-,"F"+STYLE-,"0002"+CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End] 
                   
                   
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[11,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"CXL",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'

laTransact[11,9] = 'CSTYTYPE+CBUSDOCU = "MP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"MAT_POP\MAT_POR\MAT_PORECP",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*-SAB ----- [End]

****************************************************** Material Manufacturing Order *********************************
*- All Tables are Fox Except (MFGOPRHD - MFGOPRDT - CTKTBOM - BOMLINE - BOMCOST - ROLLS)
laTransact[12,1] = 'Material Manufacturing Order'
laTransact[12,2] = 'MA'
*-SAB ----- [Start]


*laTransact[12,4] = 'MMFGORDH,MMFGORDD,CTKTBOM,BOMLINE,BOMCOST,MFGOPRHD,MFGOPRDT,'+;
                   'FABRIC,FABDYE,UNCMSESS,ROLLS,MATINVJL,'
*B611797,1 ES 10/08/2019 Performance Issue / Data purge [Start]

*laTransact[12,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,STYLE,'+;
                   'STYDYE,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,ITEMJRNL,MFGOPRHD,MFGOPRDT,'
              
laTransact[12,4] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,BOMLINE,BOMVAR,BOMCOST,ITEM,'+;
                   'ITEMLOC,APVENDOR,UNCMSESS,NOTEPAD,NOTEPAD,BOM,ITEMJRNL,MFGOPRHD,MFGOPRDT,'         
*B611797,1 ES 10/08/2019 Performance Issue / Data purge [End]

*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[12,5] = 'MMFGORDH,MMFGORDD,CTKTBOM,MFGOPR,POBOMCLS,MFGOPRHD,MFGOPRDT,'+;
                   'FABRIC,FABDYE,TRANS,ROLLITEM,MFGOPR,'
       
laTransact[12,5] = 'POSHDR,POSLN,CUTPICK,ORDLINE,ORDHDR,CTKTBOM,MFGOPR,BOMVAR,POBOMCLS,STYLE,'+;
                   'STYDYE,VENCODE,TRANS,NOTEPAD,NOTEPAD,MULTIBOM,MFGOPR,MFGOPRHD,MFGOPRDT,'
 
                   
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[12,6] = 'B,B,B,B,B,B,B,A,A,D,A,N,'
laTransact[12,6] = 'B,B,B,U,U,B,B,B,B,A,A,A,D,B,A,A,N,B,B,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[12,7] = '1-,1-,1-,1-,1-,1-,1-,2-,2-,10-,8-,1-,'
laTransact[12,7] = '1-,1-,1-,3-,3-,1-,1-,1-,1-,2-,2-,1-,13-,1-,10-,10-,1-,1-,1-,'
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[12,8] = '1-,CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,"T"+CMFGORDNO-,'+;
                   'CFABRIC+COLOR-,CFABRIC+COLOR-,10-,FABRIC-,CMFGORDNO-,'
                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][Start]
*!*	laTransact[12,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"I"+PO-,"I"+PO-,"AD"+PO-,"I"+PO-,'+;
*!*	                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"I"+PO-,"I"+PO-,'
*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][Start]

*laTransact[12,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"T"+PO-,"T"+PO-,"AD"+PO-,"T"+PO-,'+;
                   'STYLE-,STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0001"+CSTYMAJOR-,PO-,"T"+PO-,"T"+PO-,'

*B611797,1 ES 10/08/2019 Performance Issue / Data purge [Start]

*laTransact[12,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"T"+PO-,"T"+PO-,"AD"+PO-,"T"+PO-,'+;
                  '"0002"+CSTYMAJOR-,"0002"+CSTYMAJOR-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0002"+CSTYMAJOR-,PO-,"T"+PO-,"T"+PO-,'                   
laTransact[12,8] = '1-,CBUSDOCU+CSTYTYPE+PO-,"2"+PO-,"O"+ORDER+STYLE-,"O"+ORDER-,"T"+PO-,"T"+PO-,"AD"+PO-,"T"+PO-,'+;
                  '"0002"+STYLE-,"0002"+STYLE-,VENDOR-,13-,"P"+PO-,"F"+STYLE-,"0002"+CSTYMAJOR-,PO-,"T"+PO-,"T"+PO-,'                             

*B611797,1 ES 10/08/2019 Performance Issue / Data purge [End]

*B611797,1 ES 09/24/2019 Performance Issue / Data purge [T20171010.0007][End]
                   
*B611797,1 ES 09/01/2019 Performance Issue / Data purge [T20171010.0007][End]
  
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[12,9] = 'COMPLETE<=ldRpPrDt .AND. STATUS$"SX",.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "MAMFORD",.T.,.T.,'
laTransact[12,9] = 'CSTYTYPE+CBUSDOCU = "FP" .AND. COMPLETE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CXS"),.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "MAMFORD",'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,'
*-SAB ----- [End]

****************************************************** A/P Invoice **************************************************
*- All Tables are Fox
laTransact[13,1] = 'A/P Invoice'
laTransact[13,2] = 'AP'
laTransact[13,4] = 'APINVHDR,APVINVDT,APINVTKT,APDIST,APPAYMNT,APVENDOR,'
laTransact[13,5] = 'VENDINV,ORGVINV,LNCONT,INVVEND,TYPMETHDOC,VENCODE,'
laTransact[13,6] = 'B,B,B,B,B,A,'
laTransact[13,7] = '1-,1-,1-,1-,5-,1-,'
laTransact[13,8] = '1-,CVENDCODE+CINVNO-,CVENDCODE+CINVNO-,CINVNO+CVENDCODE-,5-,CVENDCODE-,'
laTransact[13,9] = 'DINVDATE<=ldRpPrDt .AND. NINVAMNT=NINVPAID+NINVDISTK+NINVADJ,.T.,.T.,DAPDTRDAT<=ldRpPrDt,DPAYDATE<=ldRpPrDt,.T.,'

****************************************************** Picking Tickets **********************************************
*- All Tables are Fox Except (BOM)
laTransact[14,1] = 'Picking Tickets'
laTransact[14,2] = 'AL'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*laTransact[14,4] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,UNCMSESS,BOM,'
laTransact[14,4] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,UNCMSESS,BOM,CUSTOMER,CUSTOMER,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-SAB ----- [Start]
*laTransact[14,5] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,TRANS,BOM,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*laTransact[14,5] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,TRANS,MULTIBOM,'
laTransact[14,5] = 'PIKTKT,PIKLINE,ORDLINE,ORDHDR,ORDLINE,STYLE,STYDYE,TRANS,MULTIBOM,CUSTOMER,CUSTOMER,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-SAB ----- [End]
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*!*  laTransact[14,6] = 'B,B,U,A,A,A,A,D,A,'
*!*  laTransact[14,7] = '1-,1-,2-,1-,2-,2-,2-,8-,6-,'
laTransact[14,6] = 'B,B,U,A,A,A,A,D,A,A,A,'
laTransact[14,7] = '1-,1-,2-,1-,2-,2-,2-,8-,6-,1-,1-'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-SAB ----- [Start]
*laTransact[14,8] = '1-,PIKTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"O"+ORDER+STYLE-,STYLE-,STYLE-,8-,CSTYMAJOR-,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*laTransact[14,8] = '1-,PIKTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"O"+ORDER+STYLE-,STYLE-,STYLE-,8-,"0001"+CSTYMAJOR-,'
laTransact[14,8] = '1-,PIKTKT-,"O"+ORDER+STYLE-,"O"+ORDER-,"O"+ORDER+STYLE-,STYLE-,STYLE-,8-,"0001"+CSTYMAJOR-,"M"+ACCOUNT-,"S"+ACCOUNT+STORE-,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-SAB ----- [End]
*-SAB ----- [Start]
*laTransact[14,9] = 'DATE<=ldRpPrDt .AND. STATUS$"CX",.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"ALORDAL\ALAUTAL\ALRELPI\PACKLIST",.T.,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*laTransact[14,9] = 'DATE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CX"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"ALORDAL\ALAUTAL\ALRELPI\PACKLIST",.T.,'
laTransact[14,9] = 'DATE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS$"CX"),.T.,.T.,.T.,.T.,.T.,.T.,'+;
                   'DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE$"ALORDAL\ALAUTAL\ALRELPI\PACKLIST",.T.,.T.,.T.,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*-SAB ----- [End]

****************************************************** Point of Sale ************************************************
*- All Tables are Fox Except (BOM - RETHDR - RETLINE)
laTransact[15,1] = 'Point of Sale'
laTransact[15,2] = 'PS'
laTransact[15,4] = 'POSTRAN,UNCMSESS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
                   'ARHIST,CREDIT,DEBIT,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ARCUSHST,STYINVJL,'
*-SAB ----- [Start]
*laTransact[15,5] = 'POSTRAN,TRANS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
                   'ARHISTT,CRTRAN,DRTRAN,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
                   'BOM,SALESREP,ACTHST,MFGOPR,'
laTransact[15,5] = 'POSTRAN,TRANS,INVHDR,INVLINE,INVCHRG,ORDHDR,ORDLINE,RETHDR,RETLINE,'+;
                   'ARHISTT,CRTRAN,DRTRAN,STYLE,STYDYE,CUSTOMER,NOTEPAD,NOTEPAD,'+;
                   'MULTIBOM,SALESREP,ACTHST,MFGOPR,'
*-SAB ----- [End]
laTransact[15,6] = 'B,D,B,B,B,B,B,B,B,B,N,N,A,A,A,A,A,A,A,A,N,'
laTransact[15,7] = '1-,2-,1-,3-,3-,3-,6-,1-,8-,1-,11-,12-,1-,1-,1-,13-,15-,13-,15-,15-,3-8-,'
*-SAB ----- [Start]
*laTransact[15,8] = '1-,2-,TRAN-,INVOICE-,INVOICE-,"O"+ORDER-,CORDTYPE+ORDER-,TRAN-,CRMEMO-,ACCOUNT+TRAN-,'+;
                   '11-,12-,STYLE-,STYLE-,"M"+ACCOUNT-,"F"+STYLE-,"A"+ACCOUNT-,'+;
                   'CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,INVOICE-CRMEMO-,'
laTransact[15,8] = '1-,2-,TRAN-,INVOICE-,INVOICE-,"O"+ORDER-,CORDTYPE+ORDER-,TRAN-,CRMEMO-,ACCOUNT+TRAN-,'+;
                   '11-,12-,STYLE-,STYLE-,"M"+ACCOUNT-,"F"+STYLE-,"A"+ACCOUNT-,'+;
                   '"0001"+CSTYMAJOR-,SALESREP-REP2-,ACCOUNT-,INVOICE-CRMEMO-,'
*-SAB ----- [End]
laTransact[15,9] = 'TRANDATE<=ldRpPrDt,DTRANDATE<=ldRpPrDt .AND. CUTRANTYPE = "PNTOSALE",'+;
                   '!SEEK("1"+INVOICE~"DEBIT"),.T.,.T.,.T.,.T.,.T.,!SEEK("0"+CRMEMO~"CREDIT"),'+;
                   '.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,.T.,'

****************************************************** Bill of Lading ***********************************************
*- All Tables are Fox
laTransact[16,1] = 'Bill of Lading'
laTransact[16,2] = 'EB'
laTransact[16,4] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'
laTransact[16,5] = 'BOL_HDR,BOL_LIN,PACK_HDR,PACK_LIN,ASN_SHIP,'
laTransact[16,6] = 'B,B,B,B,B,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
*!*  laTransact[16,7] = '1-,1-,3-,3-,5-,'
*!*  laTransact[16,8] = '1-,BOL_NO-,3-,PACK_NO-,5-,'
laTransact[16,7] = '1-,1-,3-,3-,1-,'
laTransact[16,8] = '1-,BOL_NO-,3-,PACK_NO-,BOL_NO-,'
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[END]
*-SAB ----- [Start]
*laTransact[16,9] = 'SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,SHIP_DATE<=ldRpPrDt .AND. STATUS = "C",.T.,CANCELLED<=ldRpPrDt,'
laTransact[16,9] = 'SHIP_DATE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS = "C"),.T.,'+;
                   'SHIP_DATE<=ldRpPrDt .AND. IIF(llRpIgnrStat~.T.~STATUS = "C"),.T.,CANCELLED<=ldRpPrDt,'
*-SAB ----- [End]
****************************************************** Purge Rolls **************************************************
*- All Tables are Fox Except (ROLLS)
laTransact[17,1] = 'Purge Rolls'
laTransact[17,2] = 'MA'
*-SAB ----- [Start]
*laTransact[17,4] = 'MATINVJL,FABRIC,FABDYE,ROLLS,'
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
*laTransact[17,4] = 'ITEMJRNL,FABRIC,FABDYE,ROLLS,'
laTransact[17,4] = 'ITEMJRNL,ITEM,ITEMLOC,ROLLS,'
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][End]
*-SAB ----- [End]
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
*laTransact[17,5] = 'MTINVSEQ,FABRIC,FABDYE,SESSION,'
laTransact[17,5] = 'STYINVJL,STYLE,STYDYE,SESSION,'
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][End]
laTransact[17,6] = 'B,B,B,B,'
laTransact[17,7] = '1-,1-,1-,2-,'
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][Start]
*laTransact[17,8] = '1-,FABRIC+COLOR-,FABRIC+COLOR-,4-,'
laTransact[17,8] = '1-,"0002"+STYLE-,"0002"+STYLE-,4-,'
*B611123,1 MMT 03/13/2016 Error while purging Rolls and Style Inventory Journal[T20151016.0008][End]
laTransact[17,9] = '.T.,.T.,DTRANDATE<=ldRpPrDt,.T.,'

ENDFUNC

*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[Start]
************************************************************
*! Name      : lfGetTransCntDate
*! Developer : Mariam Mazhar[MMT]
*! Date      : 04/20/2021
*! Purpose   : Get the Number of records will be purged per module
************************************************************
FUNCTION lfGetTransCntDate
PARAMETERS lcTransaction, ldPurgDate
lnArrayPos = 0
lcDatExp = ''
FOR lnTran = 1 TO ALEN(laTransact,1)
  IF ALLTRIM(laTransact[lnTran ,1])  = ALLTRIM(lcTransaction)
    lnArrayPos = lnTran 
    EXIT 
  ENDIF
ENDFOR
IF lnArrayPos = 0
  RETURN 0
ELSE
  lnTrnCnt = 0
  DIMENSION laTables[1]
  laTables[1] = ''
  =gfSubStr(laTransact[lnArrayPos  ,4],@laTables,",") 
  DIMENSION laIndex[1]
  laIndex[1] = ''
  =gfSubStr(laTransact[lnArrayPos  ,5],@laIndex,",") 
  DIMENSION laConditions[1]
  laConditions[1] = ''
  =gfSubStr(laTransact[lnArrayPos  ,9],@laConditions,",") 
  IF UPPER('<=ldRpPrDt') $ UPPER(laConditions[1])
    lnENDPos = ATC(UPPER('<=ldRpPrDt'),UPPER(laConditions[1]))
    lcDatExp = ALLTRIM(SUBSTR(UPPER(laConditions[1]),1,lnENDPos-1 ))
    IF '.AND.' $ lcDatExp
      lnDtPos = ATC('.AND.',lcDatExp) 
      lcDatExp = ALLTRIM(SUBSTR(lcDatExp,lnDtPos+5))
    ENDIF
  ENDIF
  *laConditions[1] = STRTRAN(laConditions[1],'ldRpPrDt','ldPurgDate')
  lnTabPosArr = 0
  IF "SEEK" $ UPPER(laConditions[1])
    lcSeekExp = SUBSTR(laConditions[1],ATC('SEEK',laConditions[1])+4)
    lnStTabPos = ATC('~',lcSeekExp )+1
    lcTableName =  SUBSTR(lcSeekExp ,lnStTabPos)
    lcTableName =  STRTRAN(lcTableName ,'")','')
    lcTableName =  STRTRAN(lcTableName ,'"','')
    
    IF !EMPTY(lcTableName)
     
      FOR lnA=2 TO ALEN(laTables,1)
        IF UPPER(laTables[lnA]) ==UPPER(lcTableName)
          lnTabPosArr = lnA
          EXIT 
        ENDIF
      ENDFOR
      IF lnTabPosArr > 0
        =gfOpenTable(laTables[lnTabPosArr],laIndex[lnTabPosArr],'SH',laTables[lnTabPosArr],.T.,lcRpComp)   
        *gfOpenTable(laNeedFls[lnI,2], laNeedFls[lnI,3], lcFileStat , @lcAlias , .T., lfGetComp(laNeedFls[lnI,2]))   
      ENDIF
    ENDIF
  ENDIF
  =gfOpenTable(laTables[1],laIndex[1],'SH',laTables[1],.T.,lcRpComp)
  IF !GFGETREMOTEPROP('LLNATIVE',laTables[1]) AND lcTransaction <> 'Return Details'
   * laConditions[1] = STRTRAN(laConditions[1],'ldRpPrDt',ldPurgDate)
    laConditions[1] = STRTRAN(laConditions[1],'ldRpPrDt',"'"+DTOC(ldPurgDate)+"'")
  ELSE
    laConditions[1] = STRTRAN(laConditions[1],'ldRpPrDt',"CTOD('"+DTOC(ldPurgDate)+"')")
  ENDIF
  IF  ".AND. IIF(llRpIgnrStat~.T.~" $ laConditions[1]
    IF !llRpIgnrStat AND !GFGETREMOTEPROP('LLNATIVE',laTables[1]) AND lcTransaction <> 'Return Details'
      lnDollSign = ATC('$',laConditions[1])
      IF lnDollSign > 0
        laConditions[1] = STRTRAN(laConditions[1], ".AND. IIF(llRpIgnrStat~.T.~",'')
        laConditions[1] = STRTRAN(UPPER(laConditions[1]), "STATUS",' AND ')
        laConditions[1] = STRTRAN(UPPER(laConditions[1]), '")',"')<>0")
        laConditions[1] = STRTRAN(laConditions[1], "$",' CHARINDEX( status,')
        laConditions[1] = STRTRAN(laConditions[1], '"',"'")
      ENDIF  
    ELSE
      lnStrSign = ATC(".AND. IIF(llRpIgnrStat~.T.~",laConditions[1])
      lnEndSign = ATC('")',laConditions[1])+2
      laConditions[1] = STRTRAN(laConditions[1], SUBSTR(laConditions[1],lnStrSign ,lnEndSign ),'')
    ENDIF    
  ENDIF
  laConditions[1] = STRTRAN(laConditions[1],'~',',')
  laConditions[1] = STRTRAN(laConditions[1],'.AND.','AND')
  laConditions[1] = STRTRAN(laConditions[1],'"',"'")
  IF ALLTRIM(laConditions[1]) == '.T.'
    laConditions[1] = STRTRAN(laConditions[1],'.T.','')
  ENDIF
  IF USED(laTables[1  ,1])
    IF !GFGETREMOTEPROP('LLNATIVE',laTables[1])
      IF EMPTY(laConditions[1] )
        laConditions[1] = "1=1"
      ENDIF
      IF lcTransaction <> 'Return Details'
        IF gfSqlRun("Select COUNT(*) as 'Cnt' from "+laTables[1]+IIF(lnTabPosArr > 0,","+laTables[lnTabPosArr],"")+" Where "+laConditions[1],laTables[1],.t.,'CntTable')
          lnTrnCnt = CntTable.Cnt
          USE IN 'CntTable'
          IF !EMPTY(lcDatExp)
            IF gfSqlRun("Select MIN("+lcDatExp+") as 'ldnt' from "+laTables[1]+;
                IIF(lnTabPosArr > 0,","+laTables[lnTabPosArr],"")+" Where "+laConditions[1]+" AND "+lcDatExp+" !=''",laTables[1],.t.,'lDatTable')
              IF USED('lDatTable')
                SELECT 'lDatTable'
                LOCATE 
                lddLowest = lDatTable.ldnt
                USE IN 'lDatTable'
              ENDIF
            ENDIF
          ENDIF
          =gfCloseTable(laTables[1])
          IF  lnTabPosArr > 0
            =gfCloseTable(laTables[lnTabPosArr])
          ENDIF
        ELSE
          =gfCloseTable(laTables[1])
          IF  lnTabPosArr > 0
            =gfCloseTable(laTables[lnTabPosArr])
          ENDIF
          RETURN 0  
        ENDIF
      ELSE
        SELECT(laTables[1])
        =gfSeek('')
        COUNT TO lnCntx FOR EVALUATE(laConditions[1]) AND !DELETED()
        IF lnCntx > 0 
          lnTrnCnt = lnCntx 
          SELECT MIN(&lcDatExp.) as 'lLowDate' FROM laTables[1] WHERE EVALUATE(laConditions[1]) AND !EMPTY(&lcDatExp.)  AND !DELETED() INTO CURSOR 'DtLwDat'
          IF USED('DtLwDat')
                SELECT 'DtLwDat'
                LOCATE 
                lddLowest = DtLwDat.lLowDate
                USE IN 'DtLwDat'
              ENDIF
          =gfCloseTable(laTables[1])
          IF  lnTabPosArr > 0
            =gfCloseTable(laTables[lnTabPosArr])
          ENDIF
        ELSE
          =gfCloseTable(laTables[1])
          IF  lnTabPosArr > 0
            =gfCloseTable(laTables[lnTabPosArr])
          ENDIF
          RETURN 0  
        ENDIF 
        
      ENDIF  
      *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
      IF lcTransaction =  'Style Inventory Journal'
        =gfOpenTable('INVTADJ','INVTADJ','SH','INVTADJ',.T.,lcRpComp)
        SELECT INVTADJ
        =gfSeek('')
        laConditions[1] = STRTRAN(laConditions[1],"'"+DTOC(ldPurgDate)+"'","CTOD('"+DTOC(ldPurgDate)+"')")
        laConditions[1] = STRTRAN(UPPER(laConditions[1]),'DTRDATE','DATE')
        lcDatExp = STRTRAN(UPPER(lcDatExp),'DTRDATE','DATE')
        COUNT TO lnCntx FOR EVALUATE(laConditions[1]) AND !DELETED()
        IF lnCntx > 0 
          lnTrnCnt = lnTrnCnt +  lnCntx 
          SELECT MIN(&lcDatExp.) as 'lLowDate' FROM INVTADJ WHERE EVALUATE(laConditions[1]) AND !EMPTY(&lcDatExp.)  AND !DELETED() INTO CURSOR 'DtLwDat'
          IF USED('DtLwDat')
            SELECT 'DtLwDat'
            LOCATE 
            lddLowest = MIN(lddLowest ,DtLwDat.lLowDate)
            USE IN 'DtLwDat'
          ENDIF
          =gfCloseTable('INVTADJ')
        ELSE
          =gfCloseTable('INVTADJ')
        ENDIF 
        IF lnTrnCnt > 0
        lcStyCostM = gfGetMemVar('M_COST_MET',lcRpComp)
        IF lcStyCostM  $ 'AS'
         =gfOpenTable('STYLE','STYLE','SH','STYLE',.T.,lcRpComp)
         SELECT STYLE
         =gfSeek('')
         lnStyCntr = 0
         COUNT FOR !DELETED() TO lnStyCntr
         lnTrnCnt = lnTrnCnt +  lnStyCntr
         =gfCloseTable('STYLE')
        ENDIF 
        ENDIF
      ENDIF
      *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End] 
    ELSE
      *lcSqlStat ="Select COUNT(*) as 'Cnt' from "+laTables[1]+IIF(lnTabPosArr > 0,","+laTables[lnTabPosArr],"")+" Where "+laConditions[1]+" Into cursor "+'CntTable'
      *&lcSqlStat.
      SELECT (laTables[1])
      COUNT TO lnCntx FOR EVALUATE(laConditions[1])
      IF lnCntx > 0 &&USED('CntTable')
        lnTrnCnt = lnCntx &&CntTable.Cnt
                  SELECT MIN(&lcDatExp.) as 'lLowDate' FROM laTables[1] WHERE EVALUATE(laConditions[1]) AND !EMPTY(&lcDatExp.) AND !DELETED() INTO CURSOR 'DtLwDat'
          IF USED('DtLwDat')
                SELECT 'DtLwDat'
                LOCATE 
                lddLowest = DtLwDat.lLowDate
                USE IN 'DtLwDat'
              ENDIF

        *USE IN 'CntTable'
        =gfCloseTable(laTables[1])
        IF  lnTabPosArr > 0
          =gfCloseTable(laTables[lnTabPosArr])
        ENDIF
      ELSE
        =gfCloseTable(laTables[1])
        IF  lnTabPosArr > 0
          =gfCloseTable(laTables[lnTabPosArr])
        ENDIF
        RETURN 0  
      ENDIF  
        *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[Start]
        IF lcTransaction =  'Bill of Lading'
          =gfOpenTable('PACK_HDR','PACK_HDR','SH','PACK_HDR',.T.,lcRpComp)
          SELECT PACK_HDR
          =gfSeek('')
          COUNT TO lnCntx FOR EVALUATE(laConditions[1]) AND !DELETED()
          IF lnCntx > 0 
            lnTrnCnt = lnTrnCnt +  lnCntx 
            SELECT MIN(&lcDatExp.) as 'lLowDate' FROM PACK_HDR WHERE EVALUATE(laConditions[1]) AND !EMPTY(&lcDatExp.)  AND !DELETED() INTO CURSOR 'DtLwDat'
            IF USED('DtLwDat')
              SELECT 'DtLwDat'
              LOCATE 
              lddLowest = MIN(lddLowest ,DtLwDat.lLowDate)
              USE IN 'DtLwDat'
            ENDIF
            =gfCloseTable('PACK_HDR')
          ELSE
            =gfCloseTable('PACK_HDR')
          ENDIF 
        ENDIF
          *B612410,1 MMT 06/08/2021 Purging Style Inventory Journal module does not update the progress bar correctly[End]
         
    ENDIF  
  ENDIF  
ENDIF
  
RETURN lnTrnCnt
*E612380,1 MMT 04/20/2021 Enhance the purge program so that the clients can easily purge by themselves[End]
*!*************************************************************
*! Name      : lfClearMemory
*! Developer : Mariam Mazhar
*! Date      : 05/09/2021
*! Purpose   : Clear Memory
*!*************************************************************
Function lfClearMemory
Declare Integer SetProcessWorkingSetSize In kernel32 As SetProcessWorkingSetSize  ;
  Integer hProcess , ;
  Integer dwMinimumWorkingSetSize , ;
  Integer dwMaximumWorkingSetSize
Declare Integer GetCurrentProcess In kernel32 As GetCurrentProcess
nProc = GetCurrentProcess()
bb = SetProcessWorkingSetSize(nProc,-1,-1)
