*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\SMMTDIC.prg
*:  Module      : System Manager
*: Program desc. : Maintain Dictionary
*:  System      : Aria 4XP
*:  Developer   : TMI - Tarek Mohamed Ibrahim
*:  Date        : 04/02/2013
*:  Reference   : *E303375,1
*:************************************************************************
*B610333,1 fix media R13 issues TMI 05/22/13
*B610447,1 TMI 07/25/2013 fix problems in maintain dictionary screen
*B610532,1 TMI 09/27/2013 be sure that the array data copied to is the same length as the copy from    [T20130926.0011 TASK] 
*B610540,1 MMT 10/03/2013 fix Error if file records is not found in sydflfld
*B610540,2 MMT 10/07/2013 fix Error if SQ Table has no index
*B610591,1 TMI 11/18/2013 comment out opening SYUUSER as it is not used in this program [T20130916.0017] 
*B611103,1 MMT 01/18/2016 Error after closing Maintain dictioanry screen[T20150914.0017]
*B611756,1 HMS 04/01/2019 - Aria 5 - errors -update file indices [T20190301.0001]
*:************************************************************************

PARAMETER llExtCall
#Include  R:\Aria4xp\PRGS\SM\SMMTDIC.H
lcRunScx = lfGetScx("SM\SMMTDIC.scx")
DO FORM (lcRunScx)

************************************************************
*! Name      : lfGetScx
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : Get the scx path to run in SaaS environemt
************************************************************
FUNCTION lfGetScx
PARAMETERS lcScx
LOCAL lcRunScx
IF oAriaApplication.Multiinst AND FILE(oAriaApplication.clientscreenhome+lcScx)
  lcRunScx = oAriaApplication.clientscreenhome+lcScx
ELSE
  lcRunScx = oAriaApplication.screenhome+lcScx
ENDIF
RETURN lcRunScx
 *- End of lfGetScx.

*!*************************************************************
*! Name      : lfFormInit
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/05/2012
*! Purpose   : called from the Screen init Method
*!*************************************************************
FUNCTION lfFormInit
PARAMETERS loFormSet

loFormSet.AddProperty('lcProgName','SMMTDIC')

*!*	*- Set functions to the APMAIN.FXP
*!*	lcPath = oAriaapplication.ApplicationHome
*!*	SET PROCEDURE TO (lcPath+'AP\APMAIN.FXP') ADDITIVE
*!*	SET PROCEDURE TO (lcPath+'GL\GL.FXP') ADDITIVE

*!*	*- Open tables
*!*	=lfOpenPRGFILES(loFormSet.lcProgName)

=gfOpenFile(oAriaApplication.SysPath+'SYUSTATC','CUSER_ID')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
*B610447,3 TMI 08/01/2013 [Start] 
*=gfOpenFile(oAriaApplication.SysPath+'SYCINST','')   && COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION
*SELECT sycinst
*llLock = FLOCK()
*IF !llLock 
*  gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not open the screen')
*  RETURN .F.
*ENDIF 
*B610447,3 TMI 08/01/2013 [End  ] 




IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
* RETURN
ENDIF

*B610333,1 fix media R13 issues TMI 05/22/13 [Start] add a waiting window 
WAIT WINDOW NOWAIT "Collecting tables' definitions"
*B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 

IF !lfDefineVars(loFormSet)
  RETURN .F.
ENDIF

*B610333,1 fix media R13 issues TMI 05/22/13 [Start] remove the wait window
WAIT clear 
*B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 

*** Load program base file
*=lfAddProp(loFormSet,'lcBaseFile',ALLTRIM(sydObjct.cBaseFile))

*- set control source
WITH loFormSet.Ariaform1
  .laApp_fl.RowSource = 'Thisformset.laApp_fl'
  *B610333,1 fix media R13 issues TMI 05/22/13 [Start] 
  *.laApp_fl.FontName = 'FoxFont'
  *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 

  .laApp_nam.RowSource = 'Thisformset.laApp_nam'
  .laCompany.RowSource = 'Thisformset.laCompany'
  .laCompany.Refresh()
  .laCompany.Value = oAriaApplication.ActiveCompanyID
  .laCompany.Valid()
ENDWITH
*B611756,1 HMS , 04/01/2019 - Aria 5 - errors -update file indices [T20190301.0001][Begin]
lnCurUsers = gfUserList(.T.)
IF lnCurUsers-1 > 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Error_SYSTEM_IN_USE,loFormSet.GetHeaderText("LANG_Error_SYSTEM_IN_USE",loFormSet.HeaderAlias)))
  RETURN .F.
ENDIF
IF SEEK('WIN','SYUSTATC')
  SELECT SYUSTATC
  LOCATE REST WHILE COBJ_TYP+ALLTRIM(COBJ_NAME)+CUSER_ID+CSTATION = 'WIN' FOR ALLTRIM(COBJ_NAME)<>'AADSMMTDIC' AND ;
         UPPER(ALLTRIM(CUSER_ID))  == UPPER(ALLTRIM(oAriaApplication.User_ID)) .AND. ;
           UPPER(ALLTRIM(CSTATION )) == UPPER(ALLTRIM(oAriaApplication.Station)) and;
           UPPER(ALLTRIM(cComp_id )) == UPPER(ALLTRIM(oAriaApplication.ActiveCompanyID)) AND !DELETED()
  IF FOUND()
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ERROR_EXCL,loFormSet.GetHeaderText("LANG_ERROR_EXCL",loFormSet.HeaderAlias)))
    RETURN .F.
  ENDIF           
ENDIF

USE (ADDBS(oAriaApplication.SysPath)+'Sycinst.DBF') SHARED IN 0
SELECT SYCINST
LOCATE
IF sycinst.lLockSys AND !RLOCK("SYCINST")
   =gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Error_SYSTEM_LOCKED,loFormSet.GetHeaderText("LANG_Error_SYSTEM_LOCKED",loFormSet.HeaderAlias)))
   RETURN .F.
ELSE
  =RLOCK("SYCINST")
  REPLACE Sycinst.lLockSys WITH .T. IN SYCINST
  LOCATE
ENDIF
*B611756,1 HMS , 04/01/2019 - Aria 5 - errors -update file indices [T20190301.0001][End]
IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
  * go ahead
ELSE
  DO lpExtCall
  RETURN .F.
ENDIF


*- End of lfFormInit
************************************************************
*! Name      : lfFormDestroy
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2013
*! Purpose   : lfFormDestroy
************************************************************
FUNCTION lfFormDestroy
PARAMETERS loFormSet

*=lfThermo(100 ,100,'','')
*SET EXCL &lcSaveExc

IF !USED('syuStatc')
  SELECT 0
  USE (oAriaApplication.SysPath+"syuStatc") ORDER 1
  =SEEK ("INI"+"OLDVARS"+oAriaApplication.User_Id+oAriaApplication.Station)
  =RLOCK()
ENDIF

IF !(TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall)
  IF !USED('SYDSUPRG')
    SELECT 0
    USE (oAriaApplication.SysPath+"SYDSUPRG") ORDER TAG PRGCOMP
  ENDIF
ENDIF

*B610591,1 TMI 11/18/2013 18:59 [Start] comment out opening SYUUSER as it is not used in this program
*IF !USED('SYUUSER')
*  SELECT 0
*  USE (oAriaApplication.SysPath+"SYUUSER") ORDER TAG CUSER_ID SHARED
*ENDIF
*B610591,1 TMI 11/18/2013 19:00 [End  ] 

*DO gpChngData
*B611756,1 HMS , 04/01/2019 - Aria 5 - errors -update file indices [T20190301.0001][Begin]
*!*	IF glLockSys
*!*	  glLockSys = .F.
*!*	  =gfLockSys()
*!*	ENDIF

IF !USED('SYCINST')
  USE (ADDBS(oAriaApplication.SysPath)+'Sycinst.DBF') SHARED IN 0
ENDIF 
 REPLACE Sycinst.lLockSys WITH .F. IN Sycinst
  UNLOCK IN SYCINST
*B611756,1 HMS , 04/01/2019 - Aria 5 - errors -update file indices [T20190301.0001][End]
CLOSE DATA

*- End of lfFormDestroy.

*/************************ original code *********************************/*
************************************************************
*! Name      : lfDefineVars
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 03/10/2013
*! Purpose   :
************************************************************
FUNCTION lfDefineVars
PARAMETERS loFormSet

*** Check first if all the users are loged out becouse this program
*** is going to open all the files excluseve

SELECT SYUSTATC
LOCATE FOR cObj_name ='OLDVARS' .AND. syuStatc.cStation <> oAriaApplication.Station

*** If at least one user have a record in the static files
*** keep locating for all users
*B610447,3 TMI 08/02/2013 [Start] add this until you be able to resolve the problem of opening other things along with this screen , this is a temp sloution
IF .F.
*B610447,3 TMI 08/02/2013 [End  ] 
DO WHILE FOUND()
  lcUser    = syuStatc.cUser_ID
  lcStion   = syuStatc.cStation
  IF lcUser = oAriaApplication.User_Id .AND. lcStion = oAriaApplication.Station
    CONTINUE
  ELSE
    lcOldRep = SET('REPROCESS')
    SET REPROCESS TO 1
    IF RLOCK('SYUSTATC')
      UNLOCK IN ALIAS('SYUSTATC')
      SET REPROCESS TO lcOldRep
      CONTINUE
    ELSE
      SET REPROCESS TO lcOldRep
      =gfModalGen ('INM00025B00000','Alert')
      glQuitting = .T.
      RETURN .F.
    ENDIF
  ENDIF
ENDDO

*B610447,3 TMI 08/02/2013 [Start] 
ENDIF

*- I intend to check if there is a record with syustatc.COBJ_NAME = 'AADSMMTDIC ', if so then do not open the screen, 
*- also put in the ariaformset class a check in init, if there is a record with syustatc.COBJ_NAME = 'AADSMMTDIC ' then do not open the screen
*IF syustatc.COBJ_NAME = 'AADSMMTDIC '
*ENDIF 
*B610447,3 TMI 08/02/2013 [End  ] 

*- Define needed variables to be used
loFormSet.AddProperty('laApp_nam[1,1]',' ')
loFormSet.AddProperty('laApp_fl[1,1]',' ')
loFormSet.AddProperty('laSelected [1,2]',' ')
loFormSet.AddProperty('laCompany[1,1]',' ')
loFormSet.AddProperty('laComPath[1,1]',' ')
loFormSet.AddProperty('laComMdls[1,1]',' ')

loFormSet.AddProperty('lnBarNo'     , 1)
loFormSet.AddProperty('lnSelected'  , 0)
loFormSet.AddProperty('lcModules'   , " ")
loFormSet.AddProperty('lcMod_ID'    , " ")
loFormSet.AddProperty('lcOldMdl'    , " ")
loFormSet.AddProperty('lcCompany'   , " ")
loFormSet.AddProperty('lcComp_ID'   , " ")
loFormSet.AddProperty('lcOldComp'   , " ")
loFormSet.AddProperty('lcComMdls'   , " ")
loFormSet.AddProperty('lcSubsVar'   , ' ')
loFormSet.AddProperty('lcTargDir'   , ' ')
loFormSet.AddProperty('lcFilePath'  , ' ')
loFormSet.AddProperty('lcTUnAble'   , ' ')
*loFormSet.AddProperty('lcSaveExc'   , SET('EXCL'))
loFormSet.AddProperty('llAllComp'   , .F.)
loFormSet.AddProperty('llTaged'     , .F.)
loFormSet.AddProperty('llNoMdls'    , .F.)
loFormSet.AddProperty('llYes2All'   , .F.)
*E303375,1 TMI 04/03/2013 [Start]
loFormSet.AddProperty('llOpenRep'   , .F.)
loFormSet.AddProperty('lcTempNam'   ,' ')
loFormSet.AddProperty('lcFileNam'   ,' ')
loFormSet.AddProperty('lnRepLine'   ,' ')


loFormSet.AddProperty('ActiveCompanyID' , oAriaApplication.ActiveCompanyID)
*E303375,1 TMI 04/03/2013 [End  ]

lcSaveExc = SET('EXCL')

loFormSet.AddProperty('lcWinTitl' , "Files structure discrepancies report")

CLOSE DATA
SET EXCL ON

SELECT 0
USE (oAriaApplication.caria4syspath+"syustatc") ORDER 1 SHARED
SELECT 0
*B610591,1 TMI 11/18/2013 17:59 [Start] comment out
*USE (oAriaApplication.SysPath+"syuusrpr") ORDER 1 SHARED
*B610591,1 TMI 11/18/2013 17:59 [End  ] 

=gfOpenFile(oAriaApplication.SysPath+'SYCCOMP','CCOMP_ID')
=gfOpenFile(oAriaApplication.SysPath+'SYDAPPL','CAPP_ID')
=gfOpenFile(oAriaApplication.SysPath+'SYDFILES')
=gfOpenFile(oAriaApplication.SysPath+'SYDFLFLD')
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD')
=gfOpenFile(oAriaApplication.SysPath+'SYDINDEX')

*** Collect all companies
SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),;
       ALLT(syccomp.mcomp_mdl),lrunfroma4,ccomp_id;
  FROM (oAriaApplication.SysPath+"syccomp") ;
  INTO ARRAY loFormSet.laCompany ;
  ORDER BY 1

*- check the connection is valid to the selected companies
LOCAL lnI,laValid,lnDim,lnColCnt
lnColCnt = ALEN(loFormSet.laCompany,2)
DIMENSION laValid[ALEN(loFormSet.laCompany,1),2] && valid companies
FOR lnI = 1 TO ALEN(loFormSet.laCompany,1)
  lcConn = ''
  laValid[lnI,1] = lfCheckConnValid(loFormSet.laCompany[lnI,5],@lcConn)
  laValid[lnI,2] = lcConn
ENDFOR
*- Keep only valid companies
lnDim = 0
DIMENSION laVldComp[1]
FOR lnI = 1 TO ALEN(loFormSet.laCompany,1)
  IF laValid[lnI,1]
    lnDim = lnDim + 1
    DIMENSION laVldComp[lnDim,lnColCnt+1]
    FOR lnJ = 1 TO lnColCnt
      laVldComp[lnDim,lnJ] = loFormSet.laCompany[lnI,lnJ]
    ENDFOR
    laVldComp[lnDim,lnColCnt+1] = laValid[lnI,2]
  ENDIF
ENDFOR
DIMENSION loFormSet.laCompany[ALEN(laVldComp,1),ALEN(laVldComp,2)]
=ACOPY(laVldComp,loFormSet.laCompany)

IF EMPTY(loFormSet.laCompany[1])
*N000682,1 04/16/2013 THB Globlization changes[Start]
*  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No valid connection string for all companies')
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=gfModalGen('INM00000B00000',.F.,.F.,.F.,LANG_No_valid_connection_string_for_all_companies)
=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_valid_connection_string_for_all_companies,loFormSet.GetHeaderText("LANG_No_valid_connection_string_for_all_companies",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 THB Globlization changes[END]
  RETURN .F.
ENDIF

lnColCnt = ALEN(loFormSet.laCompany,2)
lnCoNo = ALEN(loFormSet.laCompany,1)
IF lnCoNo > 0
  DECLARE loFormSet.laCompany [lnCoNo+1,lnColCnt]

  =AINS(loFormSet.laCompany,1)  && 1

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.laCompany [1,1] = "     All Companies"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laCompany [1,1] = LANG_All_Companies
loFormSet.laCompany [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Companies,loFormSet.GetHeaderText("LANG_All_Companies",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  loFormSet.laCompany [1,2] = " "
  loFormSet.laCompany [1,3] = " "
  loFormSet.laCompany [1,5] = " "
  loFormSet.lcComp_ID       = SUBSTR(loFormSet.laCompany[2,1],1,2)
  loFormSet.lcCompany       = loFormSet.laCompany[2,1]
  loFormSet.Ariaform1.laCompany.ListIndex       = 2
  loFormSet.lcFilePath      = ALLTRIM(LOWER(loFormSet.laCompany[2,2]))
  loFormSet.lcComMdls       = ALLTRIM(loFormSet.laCompany[2,3])
  DIMENSION laComMdls[ALEN(loFormSet.laComMdls)]
  ACOPY(loFormSet.laComMdls,laComMdls)
  =gfSubStr(loFormSet.lcComMdls,@laComMdls,"|")
  ACOPY(laComMdls,loFormSet.laComMdls)
ENDIF

*** Collect all modules
SELECT cApp_name,cApp_ID ;
  FROM (oAriaApplication.SysPath+"sydappl");
  INTO ARRAY loFormSet.laApp_nam ;
  ORDER BY 2;
  WHERE cApp_ID <> "SY" .AND. cApp_ID <> "SM" .AND.;
        cApp_ID $ loFormSet.lcComMdls

IF _TALLY > 0
  *** Add more bar to select all modules
  DECLARE loFormSet.laApp_nam [_TALLY+2,2]
  =AINS(loFormSet.laApp_nam,1)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.laApp_nam [1,1] = "Company Settings "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_Company_Settings
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  loFormSet.laApp_nam [1,2] = "__"
  loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
  loFormSet.lcMod_ID        =  '  '

  =AINS(loFormSet.laApp_nam,1)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.laApp_nam [1,1] = "     All Modules"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_All_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Modules,loFormSet.GetHeaderText("LANG_All_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  loFormSet.laApp_nam [1,2] = "  "
  loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
  loFormSet.lcMod_ID        = loFormSet.laApp_nam [1,2]

  *puApp_nam       = 1
  loFormSet.Ariaform1.laApp_nam.ListIndex = 1
  loFormSet.llNoMdls        = .F.
ELSE
  *** Add more bar to select all modules
  DECLARE loFormSet.laApp_nam [_TALLY+2,2]
  =AINS(loFormSet.laApp_nam,1)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.laApp_nam [1,1] = "Company Settings "
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_Company_Settings
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  loFormSet.laApp_nam [1,2] = "__"
  loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
  loFormSet.lcMod_ID        = '  '

  =AINS(loFormSet.laApp_nam,1)

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.laApp_nam [1,1] = "No modules are installed for this company"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_No_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Modules,loFormSet.GetHeaderText("LANG_No_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]

  loFormSet.laApp_nam [1,2] = "  "
  loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
  loFormSet.lcMod_ID        = loFormSet.laApp_nam [1,2]
  loFormSet.llNoMdls        = .T.
ENDIF

*** Collect Files for selected module
IF !EMPTY(loFormSet.lcComMdls)
   SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl,cFile_nam  ;
      FROM (oAriaApplication.SysPath+"sydfiles") ;
      INTO ARRAY loFormSet.laApp_fl ;
      WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
      ORDER BY 1
ENDIF

*!*	IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
*!*	  lcOldError = ON('ERROR')
*!*	  ON ERROR DO lpErrHand WITH LINENO()
*!*	  DO (oAriaApplication.ScreenHome + 'SM' + '\SMMtDic.SCX')
*!*	  ON ERROR &lcOldError
*!*	ELSE
*!*	  DO lpExtCall
*!*	ENDIF
*!*	=lfThermo(100 ,100,'','')
*!*	CLOSE DATA

*!*	SET EXCL &lcSaveExc

*!*	IF !USED('syuStatc')
*!*	  SELECT 0
*!*	  USE (oAriaApplication.SysPath+"syuStatc") ORDER 1
*!*	  =SEEK ("INI"+"OLDVARS"+oAriaApplication.User_Id+oAriaApplication.Station)
*!*	  =RLOCK()
*!*	ENDIF

*!*	IF !(TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall)
*!*	  IF !USED('SYDSUPRG')
*!*	    SELECT 0
*!*	    USE (oAriaApplication.SysPath+"SYDSUPRG") ORDER TAG PRGCOMP
*!*	  ENDIF
*!*	ENDIF

*!*	IF !USED('SYUUSER')
*!*	  SELECT 0
*!*	  USE (oAriaApplication.SysPath+"SYUUSER") ORDER TAG CUSER_ID
*!*	ENDIF

*!*	DO gpChngData

*!*	IF glLockSys
*!*	  glLockSys = .F.
*!*	  =gfLockSys()
*!*	ENDIF

*!*	glQuitting = .T.
*!*	CLEAR READ

RETURN
*- End of lfDefineVars.

*!*******************************************************************
*!
*!      Function: lfvSysOrMd
*!
*!*******************************************************************
* Collect files of ether the system files or the selected module
* or all modules
FUNCTION lfvSysOrMd
PARAMETERS loFormSet,loFld

DECLARE loFormSet.laApp_fl [1,1]
loFormSet.laApp_fl  =" "

*** Select only system files
IF loFormSet.Ariaform1.cbSysOrMd.Value = 1
*  SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
    FROM (oAriaApplication.SysPath+"sydFiles") ;
    INTO ARRAY loFormSet.laApp_fl ;
    WHERE "SY" $ mFile_app .OR. "SM" $ mFile_app ;
    ORDER BY 1

  SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
    FROM (oAriaApplication.SysPath+"sydFiles") ;
    INTO ARRAY loFormSet.laApp_fl ;
    WHERE LEFT(cFile_nam,2) = 'SY' AND ;
         ("SY" $ mFile_app .OR. "SM" $ mFile_app) ;
    ORDER BY 1

  *SHOW GET ibModules DISABLE 1
  *SHOW GET ibCompany DISABLE
  loFormSet.Ariaform1.laApp_nam.Enabled = .F.
  loFormSet.Ariaform1.laCompany.Enabled = .F.
*** Select module(s) files
ELSE
  *** Files of one module
  IF !EMPTY(loFormSet.lcMod_ID)
     IF loFormSet.lcMod_ID = '__'
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE 'SY' $ mFile_app ;
        AND LEFT(cFile_nam,2) <>'SY';
        ORDER BY 1

     ELSE
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE loFormSet.lcMod_ID $ mFile_app ;
        ORDER BY 1
      ENDIF
  *** All files of all modules
  ELSE
    *** If all companies was selected select all files of all modules
    IF EMPTY(loFormSet.lcComp_ID)
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
        FROM (oAriaApplication.SysPath+"sydfiles") ;
        INTO ARRAY loFormSet.laApp_fl ;
       WHERE LEFT(cFile_nam,2) <> "SY" ;
       ORDER BY 1

    *** If company was selected select all files of all installed modules
    *** to the selected cmompany
    ELSE
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
    ENDIF
  ENDIF

  *SHOW GET ibModules ENABLE
  *SHOW GET ibCompany ENABLE
  loFormSet.Ariaform1.laApp_nam.Enabled = .T.
  loFormSet.Ariaform1.laCompany.Enabled = .T.
ENDIF

*SHOW GET lsApp_fl

loFormSet.llTaged     = .F.                        && Tag None
*N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	loFormSet.Ariaform1.pbTag.Caption = "Tag \<All"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.pbTag.Caption = LANG_Tag  +"\<"+LANG_ALL
loFormSet.Ariaform1.pbTag.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tag,loFormSet.GetHeaderText("LANG_Tag",loFormSet.HeaderAlias))  +"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/16/2013 HES Globlization changes[ENd  ]
*WALID
IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
  *_CUROBJ = OBJNUM(lsApp_fl)
  *loFormset.AriaForm1.laApp_fl.SetFocus()
ENDIF
loFormset.AriaForm1.Refresh()
*!*******************************************************************
*!
*!      Function: lfvModules
*!
*!*******************************************************************
FUNCTION lfvModules
PARAMETERS loFormSet,loFld

loFormSet.lcMod_ID = loFormSet.laApp_nam[loFormSet.Ariaform1.laApp_nam.ListIndex,2]

*** If the user select new module
*IF loFormSet.lcMod_ID  <> loFormSet.lcOldMdl
IF loFld.Value <> loFld.OldValue
  DIMENSION loFormSet.laApp_fl[1,1]
  loFormSet.laApp_fl = " "

  *** If the user select the first option (All modules)
  IF !EMPTY(loFormSet.lcMod_ID)
     IF loFormSet.lcMod_ID = '__'
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE 'SY' $ mFile_app ;
        AND LEFT(cFile_nam,2) <>'SY';
        ORDER BY 1

     ELSE
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE loFormSet.lcMod_ID $ mFile_app ;
        ORDER BY 1
      ENDIF
  *** If the user select on module
  ELSE
    *** If the user was selecting all companies option
    *** get all the files
    IF EMPTY(loFormSet.lcComp_ID)
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" ;
        ORDER BY 1
    *** If one company was selected get all the files of the installed
    *** modules
    ELSE
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (oAriaApplication.SysPath+"sydfiles") ;
         INTO ARRAY loFormSet.laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
    ENDIF
  ENDIF

  *SHOW GET lsApp_fl
  loFormSet.llTaged     = .F.                        && Tag None

  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  loFormSet.Ariaform1.pbTag.Caption = "Tag \<All"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.pbTag.Caption = LANG_Tag  +"\<"+LANG_ALL
loFormSet.Ariaform1.pbTag.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tag,loFormSet.GetHeaderText("LANG_Tag",loFormSet.HeaderAlias))  +"\<"+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]

  loFormSet.Ariaform1.laApp_fl.ListIndex = 0
  loFormSet.Ariaform1.laApp_fl.RowSource = 'Thisformset.laApp_fl'
  loFormSet.Ariaform1.laApp_fl.Refresh()
ENDIF

*_CUROBJ = OBJNUM(lsApp_fl)
*loFormset.AriaForm1.laApp_fl.SetFocus()

*!*******************************************************************
*!
*!      Function: lfCompMdl
*!
*!*******************************************************************
* This function is beeing called from the select SQL to determin if
* one file from the file dectionary belong to a module installed to
* a spacific company to be colleted or not
FUNCTION lfCompMdl
PARAMETERS lcFileMdl

llRetFlag = .F.

FOR lnCount = 1 TO ALEN(loFormSet.laComMdls,1)
  IF (loFormSet.laComMdls[lnCount] $ lcFileMdl) OR ('SY' $ lcFileMdl)
    llRetFlag = .T.
    EXIT
  ENDIF
ENDFOR

RETURN llRetFlag

*!*******************************************************************
*!
*!      Function: lfvCompany
*!
*!*******************************************************************
FUNCTION lfvCompany
PARAMETERS loFormSet,loFld

*E303375,1 TMI 04/07/2013 [Start]
=lfSqlDisconn(loFormSet)
*E303375,1 TMI 04/07/2013 [End  ]

*** Get the company ID from the selected bar of the popup
loFormSet.lcFilePath = loFormSet.laCompany[loFormSet.Ariaform1.laCompany.ListIndex,2]
*loFormSet.lcComp_ID  = SUBSTR(loFormSet.laCompany[loFormSet.Ariaform1.laCompany.ListIndex,1],1,2)
loFormSet.lcComp_ID  = loFormSet.Ariaform1.laCompany.Value
lnAryElem  = loFormSet.Ariaform1.laCompany.ListIndex

*** If the user select new company
IF loFormSet.lcComp_ID  <> loFormSet.lcOldComp

  *** If the user select all companies option
  IF EMPTY(loFormSet.lcComp_ID)
    loFormSet.llAllComp  =.T.
    loFormSet.lcComMdls  = " "
    loFormSet.laComMdls  = " "

    loFormSet.llNoMdls        = .T.

    SELECT cApp_name,cApp_ID ;
      FROM (oAriaApplication.SysPath+"sydappl");
      INTO ARRAY loFormSet.laApp_nam ;
     WHERE cApp_ID <> "SY" .AND. cApp_ID <> "SM"

    *** Add new bar " all modules "
    IF _TALLY > 0
       *** Add more bar to select all modules
       DECLARE loFormSet.laApp_nam [_TALLY+2,2]
       =AINS(loFormSet.laApp_nam,1)

       *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	       loFormSet.laApp_nam [1,1] = "Company Settings "
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_Company_Settings
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

       *N000682,1 04/16/2013 HES Globlization changes[Start]
       loFormSet.laApp_nam [1,2] = "__"
       loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
       loFormSet.lcMod_ID        = '  '

       =AINS(loFormSet.laApp_nam,1)
       *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	       loFormSet.laApp_nam [1,1] = "     All Modules"
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_All_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Modules,loFormSet.GetHeaderText("LANG_All_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

       *N000682,1 04/16/2013 HES Globlization changes[Start]
       loFormSet.laApp_nam [1,2] = " "
       IF loFormSet.llNoMdls
         loFormSet.lcModules     = loFormSet.laApp_nam [1,1]
         loFormSet.lcMod_ID      = loFormSet.laApp_nam [1,2]
         loFormSet.Ariaform1.laApp_nam.ListIndex     = 1
       ENDIF
       loFormSet.llNoMdls        = .F.
    ELSE
       *** Add more bar to select all modules
       DECLARE loFormSet.laApp_nam [_TALLY+2,2]
       =AINS(loFormSet.laApp_nam,1)
       *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	       loFormSet.laApp_nam [1,1] = "Company Settings "
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] =  LANG_Company_Settings
loFormSet.laApp_nam [1,1] =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

       *N000682,1 04/16/2013 HES Globlization changes[End  ]
       loFormSet.laApp_nam [1,2] = "__"
       loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
       loFormSet.lcMod_ID        = '  '

       =AINS(loFormSet.laApp_nam,1)
       *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	       loFormSet.laApp_nam [1,1] = "No modules are installed for this company"
       *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_No_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Modules,loFormSet.GetHeaderText("LANG_No_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

       *N000682,1 04/17/2013 HES Globalization changes[End  ]
       loFormSet.laApp_nam [1,2] = " "
       loFormSet.llNoMdls        = .T.
    ENDIF

    SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
       FROM (oAriaApplication.SysPath+"sydfiles") ;
       INTO ARRAY loFormSet.laApp_fl ;
       WHERE LEFT(cFile_nam,2) <> "SY" ;
       ORDER BY 1


  ***********************************
  *** If the user select one company
  ELSE
    loFormSet.llAllComp   =.F.

    loFormSet.llNoMdls        = .T.

    loFormSet.lcComMdls   = ALLTRIM(loFormSet.laCompany[lnAryElem,3])
    DIMENSION laComMdls[ALEN(loFormSet.laComMdls)]
    ACOPY(loFormSet.laComMdls,laComMdls)
    =gfSubStr(loFormSet.lcComMdls,@laComMdls,"|")
    *B610532,1 TMI 09/27/2013 [Start] be sure that the array data copied to is the same length as the copy from    
    DIMENSION loFormSet.laComMdls[ALEN(laComMdls,1)]
    *B610532,1 TMI 09/27/2013 [End  ] 
    ACOPY(laComMdls,loFormSet.laComMdls)

    SELECT cApp_name,cApp_ID ;
      FROM  (oAriaApplication.SysPath+"sydappl"),(oAriaApplication.SysPath+"SYCCOMP");
      INTO  ARRAY loFormSet.laApp_nam ;
      WHERE syccomp.ccomp_id=loFormSet.lcComp_ID .AND. ;
            sydappl.capp_id $ ALLTRIM(syccomp.mcomp_mdl) .AND. ;
            cApp_ID <> "SY" .AND. cApp_ID <> "SM"

     *** Add new bar " All Modules"
    IF _TALLY > 0
      *** Add more bar to select all modules
      DECLARE loFormSet.laApp_nam [_TALLY+2,2]
      =AINS(loFormSet.laApp_nam,1)
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.laApp_nam [1,1] = "Company Settings "
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_Company_Settings
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
      loFormSet.laApp_nam [1,2] = "__"
      loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
      loFormSet.lcMod_ID        = '  '

      =AINS(loFormSet.laApp_nam,1)
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.laApp_nam [1,1] = "     All Modules"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_All_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_All_Modules,loFormSet.GetHeaderText("LANG_All_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
      loFormSet.laApp_nam [1,2] = " "

      IF loFormSet.llNoMdls
        loFormSet.lcModules     = loFormSet.laApp_nam [1,1]
        loFormSet.lcMod_ID      = loFormSet.laApp_nam [1,2]
        loFormSet.Ariaform1.laApp_nam.ListIndex     = 1
      ENDIF

      loFormSet.llNoMdls        = .F.
    ELSE
      *** Add more bar to select all modules
      DECLARE loFormSet.laApp_nam [_TALLY+2,2]
      =AINS(loFormSet.laApp_nam,1)
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.laApp_nam [1,1] = "Company Settings "
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_Company_Settings
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Company_Settings,loFormSet.GetHeaderText("LANG_Company_Settings",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
      loFormSet.laApp_nam [1,2] = "__"
      loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
      loFormSet.lcMod_ID        = '  '

      =AINS(loFormSet.laApp_nam,1)
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.laApp_nam [1,1] = "No modules are installed for this company"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.laApp_nam [1,1] = LANG_No_Modules
loFormSet.laApp_nam [1,1] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_No_Modules,loFormSet.GetHeaderText("LANG_No_Modules",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
      loFormSet.laApp_nam [1,2] = " "
      loFormSet.lcModules       = loFormSet.laApp_nam [1,1]
      loFormSet.lcMod_ID        = loFormSet.laApp_nam [1,2]
      loFormSet.Ariaform1.laApp_nam.ListIndex       = 1
      loFormSet.llNoMdls        = .T.
      DIMENSION loFormSet.laApp_fl[1,1]
      loFormSet.laApp_fl = " "
    ENDIF

    *** If the selected module is not installed to the new selected
    *** company select the first one and collect it's files
    IF !loFormSet.llNoMdls
      IF !EMPTY(loFormSet.lcMod_ID)
        IF ASCAN(loFormSet.laApp_nam,loFormSet.lcMod_ID) = 0
          loFormSet.lcModules       = loFormSet.laApp_nam [2,1]
          loFormSet.lcMod_ID        = loFormSet.laApp_nam [2,2]

          DIMENSION loFormSet.laApp_fl[1,1]
          loFormSet.laApp_fl = " "
       IF loFormSet.lcMod_ID = '__'
         SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
           FROM (oAriaApplication.SysPath+"sydfiles") ;
           INTO ARRAY loFormSet.laApp_fl ;
          WHERE 'SY' $ mFile_app ;
          AND LEFT(cFile_nam,2) <>'SY';
          ORDER BY 1
       ELSE
          SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
            FROM (oAriaApplication.SysPath+"sydfiles") ;
            INTO ARRAY loFormSet.laApp_fl ;
           WHERE loFormSet.lcMod_ID $ mFile_app ;
           ORDER BY 1
        ENDIF
        ENDIF
      ELSE
        SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
           FROM (oAriaApplication.SysPath+"sydfiles") ;
           INTO ARRAY loFormSet.laApp_fl ;
           WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
           ORDER BY 1
      ENDIF
    ENDIF
  ENDIF

  *SHOW GET puApp_nam

  loFormSet.llTaged     = .F.                        && Tag None
  *SHOW GET lsApp_fl
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	  loFormSet.Ariaform1.pbTag.Caption = "Tag \<All"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.pbTag.Caption = LANG_Tag  +"\<"+ LANG_ALL
loFormSet.Ariaform1.pbTag.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tag,loFormSet.GetHeaderText("LANG_Tag",loFormSet.HeaderAlias))  +"\<"+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
ENDIF

*!*******************************************************************
*!
*!      Function: lfvApp_fl
*!
*!*******************************************************************
*
FUNCTION lfvApp_fl
PARAMETERS loFormSet,loFld
lsApp_fl = loFld.ListIndex
IF LEFT(loFormSet.laApp_fl[lsApp_fl,1],1) = ""
   loFormSet.laApp_fl[lsApp_fl,1] =" "+SUBSTR(loFormSet.laApp_fl[lsApp_fl,1],2)
ELSE
   loFormSet.laApp_fl[lsApp_fl,1] =""+SUBSTR(loFormSet.laApp_fl[lsApp_fl,1],2)
ENDIF
loFormSet.Ariaform1.laApp_fl.ListIndex = lsApp_fl
loFormSet.Ariaform1.laApp_fl.RowSource = 'Thisformset.laApp_fl'
loFormSet.Ariaform1.laApp_fl.Refresh()

*SHOW GET lsApp_fl

*!*******************************************************************
*!
*!      Function: lfvTag
*!
*!*******************************************************************
*
FUNCTION lfvTag
PARAMETERS loFormSet

IF !EMPTY(loFormSet.laApp_fl)
  DO CASE
    CASE !loFormSet.llTaged                              && Tag All Files
      loFormSet.llTaged     = .T.
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.Ariaform1.pbTag.Caption = "Tag \<None"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.pbTag.Caption = LANG_Tag  +"\<"+  LANG_None
loFormSet.Ariaform1.pbTag.Caption = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tag,loFormSet.GetHeaderText("LANG_Tag",loFormSet.HeaderAlias))  +"\<"+  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_None,loFormSet.GetHeaderText("LANG_None",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]

      FOR lcFlNo = 1 TO ALEN(loFormSet.laApp_fl,1)
        IF LEFT(loFormSet.laApp_fl[lcFlNo,1],1)<> ""
          loFormSet.laApp_fl[lcFlNo,1] =""+SUBSTR(loFormSet.laApp_fl[lcFlNo,1],2)
        ENDIF
      ENDFOR

    CASE loFormSet.llTaged
      loFormSet.llTaged     = .F.                        && Tag None
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	      loFormSet.Ariaform1.pbTag.Caption = "Tag \<All"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*loFormSet.Ariaform1.pbTag.Caption =  LANG_Tag  +"\<"+  LANG_ALL
loFormSet.Ariaform1.pbTag.Caption =  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Tag,loFormSet.GetHeaderText("LANG_Tag",loFormSet.HeaderAlias))  +"\<"+  IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_ALL,loFormSet.GetHeaderText("LANG_ALL",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
      FOR lcFlNo = 1 TO ALEN(loFormSet.laApp_fl,1)
        IF LEFT(loFormSet.laApp_fl[lcFlNo,1],1) = ""
          loFormSet.laApp_fl[lcFlNo,1] =" "+SUBSTR(loFormSet.laApp_fl[lcFlNo,1],2)
        ENDIF
      ENDFOR
  ENDCASE
  *SHOW GET lsApp_fl

  *loFormSet.Ariaform1.laApp_fl.ListIndex = 0
  loFormSet.Ariaform1.laApp_fl.RowSource = 'Thisformset.laApp_fl'
  loFormSet.Ariaform1.laApp_fl.Refresh()
ENDIF

*!*******************************************************************
*!
*!      Function: lfDoAction
*!
*!*******************************************************************
*
FUNCTION lfDoAction
PARAMETERS loFormSet,lcAction

DECLARE lax[1]
lax=" "

IF lcAction # "CHECK"
  =lfvAselect(loFormSet)                 && Collect Selected files
ENDIF

oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')

IF !EMPTY(loFormSet.laSelected [1,1])   && Exit if None

  DECLARE laFileStrc[1,1],laFileAStr[1,1]

  loFormSet.llOpenRep  = .F.               && Open Report flage
  lcFilHandl = ''
  loFormSet.lcTempNam  = ''


  lcFixMess  = ''

  loFormSet.lnRepLine  = 0
  loFormSet.llYes2All  = .F.
  llUpdated = .f.
  DO CASE
    CASE lcAction = "VERIFY"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_Verifying_file_structure
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Verifying_file_structure,loFormSet.GetHeaderText("LANG_Verifying_file_structure",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcAction = "UPDATE"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_Update_file_structure
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Update_file_structure,loFormSet.GetHeaderText("LANG_Update_file_structure",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcAction = "INDEX"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_INDEX
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_INDEX,loFormSet.GetHeaderText("LANG_INDEX",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcAction = "REORGANIZ"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_REORGANIZ
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_REORGANIZ,loFormSet.GetHeaderText("LANG_REORGANIZ",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcAction = "CLEARLOCKS"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_Clear_locks
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Clear_locks,loFormSet.GetHeaderText("LANG_Clear_locks",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    CASE lcAction = "CHECK"
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcTherMesg  = LANG_Check_for_corruption
lcTherMesg  = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Check_for_corruption,loFormSet.GetHeaderText("LANG_Check_for_corruption",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  ENDCASE

  *** Verify files for all companies
  IF loFormSet.llAllComp .AND. loFormSet.Ariaform1.cbSysOrMd.Value = 2
    *** Check all selected files
    lnLastComp = ALEN(loFormSet.laCompany,1)

    *** Loop all or one company
    FOR lnCompNo = 2 TO lnLastComp
      loFormSet.lcFilePath = ALLTRIM(loFormSet.laCompany[lnCompNo,2])
      loFormSet.lnSelected = ALEN(loFormSet.laSelected,1)

      *** Check if there is one module selecet if this module is not installed to
      *** the company in the loop skip this company
      IF !EMPTY(loFormSet.lcMod_ID) .AND. !(loFormSet.lcMod_ID $ ALLTRIM(loFormSet.laCompany[lnCompNo,3])) AND loFormSet.lcMod_ID<>'__'
        LOOP
      ENDIF

      lcCompID = loFormSet.laCompany[lnCompNo,5]

      *** Loop all selected files
      LOCAL lnCount
      FOR lnCount = 1 TO loFormSet.lnSelected
        lcFileNam = UPPER(ALLTRIM(loFormSet.laSelected[lnCount,1]))

        loFormSet.lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2)) = 'SY', ALLTRIM(oAriaApplication.SysPath) , ALLTRIM(loFormSet.lcFilePath)))

        IF !USED('sydfiles')
          =gfOpenFile(oAriaApplication.SysPath+'sydfiles','Cfile_nam')
        ELSE
          SELECT sydfiles
          SET ORDER TO TAG Cfile_nam
        ENDIF
        *B610447,1  TMI 07/25/2013 [Start] supply file width
        *=SEEK(lcFileNam,'sydfiles')
        =SEEK(padr(lcFileNam,oAriaApplication.FileW),'sydfiles')
        *B610447,1  TMI 07/25/2013 [End  ] 

        =gfSubStr(sydfiles.Mfile_app,@laX,',')

        PRIVATE llFound
        llFound=.F.
        FOR lnI=1 TO ALEN(laX,1)
          IF laX[lnI] $ ALLTRIM(loFormSet.laCompany[lnCompNo,3])
            llFound=.T.
            EXIT
          ENDIF
        ENDFOR
        IF llFound

          llSqlTblFound = lfCheckSqlFile(lcFileNam)

          *** If the to be cheked file belong to one of the installed modules
          *** for this company, then verify it
          loFormSet.lcSubsVar = ALLTRIM(loFormSet.laSelected[lnCount,2])
          =lfThermo(loFormSet.lnSelected ,lnCount-.01,lcTherMesg,loFormSet.lcSubsVar)
          DO CASE
            CASE lcAction = "VERIFY"
              =lfVer_Upd(lcFileNam)
            CASE lcAction = "UPDATE"
              llUpdated = lfVer_Upd(lcFileNam,.T.)
              IF !llUpdated
                EXIT
              ENDIF
            CASE lcAction = "INDEX"
              =lfIndex(lcFileNam)
            CASE lcAction = "REORGANIZ"
              =lfReOrg(lcFileNam)
            CASE lcAction ="CLEARLOCKS"
              =lfClrLock(lcFileNam)

            CASE lcAction = "CHECK"
              laFixInfo[1]=loFormSet.lcTargDir+lcFileNam
              *=lfFixDbf()
              =IIF(sydfiles.cVer = 'A27', lfFixDbf() , '' )

          ENDCASE
          =lfThermo(loFormSet.lnSelected ,lnCount,lcTherMesg,loFormSet.lcSubsVar)
        ELSE
          *** If not skip this files but keep the thermometer running
          loFormSet.lcSubsVar = " "
          =lfThermo(loFormSet.lnSelected ,lnCount,lcTherMesg,loFormSet.lcSubsVar)
        ENDIF
      ENDFOR
    ENDFOR
  *** Verify files of one company
  ELSE

    *E303375,1 TMI 04/07/2013 [Start] close sql connections that holds the fox system files
    =lfSqlDisconn(loFormSet)
    *E303375,1 TMI 04/07/2013 [End  ]

    loFormSet.lnSelected = ALEN(loFormSet.laSelected,1)
    lcCompID = loFormSet.laCompany[loFormSet.Ariaform1.laCompany.ListIndex,5]

    *** Loop all selected files
    LOCAL lnCount
    FOR lnCount = 1 TO loFormSet.lnSelected
      lcFileNam = UPPER(ALLTRIM(loFormSet.laSelected[lnCount,1]))

      *E303375,1 TMI 04/07/2013 [Start] exclude the dictionary tables from the updates, this leads to a cyclic relation
      *B611103,1 MMT 01/18/2016 Error after closing Maintain dictioanry screen[T20150914.0017][Start]
      *IF lcFileNam $ 'SYDFILES,SYDFIELD,SYDFLFLD,SYDINDEX'
      IF lcFileNam $ 'SYDFILES,SYDFIELD,SYDFLFLD,SYDINDEX,SYUSTATC'
      *B611103,1 MMT 01/18/2016 Error after closing Maintain dictioanry screen[T20150914.0017][End]
        LOOP
      ENDIF
      *E303375,1 TMI 04/07/2013 [End  ]

      *E303375,1 TMI 04/07/2013 [Start] close the file in all sessions
      IF loFormSet.Ariaform1.cbSysOrMd.Value = 1
        lcCurrSess = SET("Datasession")
        DIMENSION laSess[1]
        ASESSIONS(laSess)
        FOR lnSessCnt = 1 TO ALEN(laSess)
          SET DATASESSION TO laSess[lnSessCnt]
          FOR lnDbf = 1 TO 255
            IF lcFileNam $ UPPER(DBF(lnDbf))
              SELECT (ALIAS(lnDbf))
              USE
            ENDIF
          ENDFOR
        ENDFOR
        SET DATASESSION TO lcCurrSess
      ENDIF
      *E303375,1 TMI 04/07/2013 [End  ]

      loFormSet.lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2)) = 'SY', ALLTRIM(oAriaApplication.SysPath) , ALLTRIM(loFormSet.lcFilePath)))

      *E303375,1 TMI 04/03/2013 [Start]
      IF !USED('sydfiles')
        =gfOpenFile(oAriaApplication.SysPath+'sydfiles','Cfile_nam')
      ELSE
        SELECT sydfiles
        SET ORDER TO TAG Cfile_nam
      ENDIF
      *B610447,3 TMI 08/02/2013 [Start] seek the correct file
      *=SEEK(lcFileNam,'sydfiles')
      =SEEK(PADR(lcFileNam,oAriaApplication.FileW),'sydfiles')
      *B610447,3 TMI 08/02/2013 [End  ] 

      *B610447,3 TMI 08/02/2013 [Start] exclude System files with cver = 'A40'
      IF LEFT(lcFileNam,2)='SY' AND SYDFILES.CVER = 'A40'
        LOOP
      ENDIF
      *B610447,3 TMI 08/02/2013 [End  ]       

      llSqlTblFound = lfCheckSqlFile(lcFileNam)
      *E303375,1 TMI 04/03/2013 [End  ]

      loFormSet.lcSubsVar = ALLTRIM(loFormSet.laSelected[lnCount,2])
      =lfThermo(loFormSet.lnSelected ,lnCount-.01,lcTherMesg,loFormSet.lcSubsVar)
      DO CASE
        CASE lcAction = "VERIFY"
          =lfVer_Upd(lcFileNam)
        CASE lcAction = "UPDATE"
          llUpdated = lfVer_Upd(lcFileNam,.T.)
          IF !llUpdated
            EXIT
          ENDIF
        CASE lcAction = "INDEX"
          =lfIndex(lcFileNam)
        CASE lcAction = "REORGANIZ"
          =lfReOrg(lcFileNam)
        CASE lcAction ="CLEARLOCKS"
          =lfClrLock(lcFileNam)
        CASE lcAction = "CHECK"
          laFixInfo[1]=loFormSet.lcTargDir+lcFileNam
          *=lfFixDbf()
          =IIF(sydfiles.cVer = 'A27', lfFixDbf() , '' )

      ENDCASE
      =lfThermo(loFormSet.lnSelected ,lnCount,lcTherMesg,loFormSet.lcSubsVar)
    ENDFOR
  ENDIF

  *** If there is a report created dispaly it to the user
  IF loFormSet.llOpenRep
    =FFLUSH(lcFilHandl)

    DO WHILE !FCLOSE(lcFilHandl)

    ENDDO
    CREATE CURSOR TMPSTR (mStrRep M(10))
    APPEND BLANK
    APPEND MEMO mStrRep FROM (oAriaApplication.WorkDir+"structur.txt")
    IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
      DO FORM (oAriaApplication.ScreenHome + 'SM' + '\SMSTRREP.SCX')
    ELSE
      DO FORM (oAriaApplication.ScreenHome + 'SM\SMSTRREP.SCX')
    ENDIF
    USE IN TMPSTR
  ELSE
    *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
    =lfShowHideProgressBar()
    *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
    DO CASE
      CASE lcAction = "VERIFY"
        =gfModalGen ('INM00005B00000','Dialog')
      CASE lcAction = "UPDATE" and llUpdated
        =gfModalGen ('INM00279B00000','Dialog')
      CASE lcAction = "INDEX"
        =gfModalGen ('INM00280B00000','Dialog')
      CASE lcAction = "REORGANIZ"
        =gfModalGen ('INM00281B00000','Dialog')
      CASE lcAction ="CLEARLOCKS"
        =gfModalGen ('INM00282B00000','Dialog')
      CASE lcAction = "CHECK"
        *-- No corrupted file(s) found.
        =gfModalGen ('INM00363B00000','Dialog')
    ENDCASE
  ENDIF
ELSE

  *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
  =lfShowHideProgressBar()
  *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
  =gfModalGen ('INM00018B00000','Dialog')
ENDIF

*B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
=lfShowHideProgressBar(.T.)
*B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 

*!*	IF lcAction ="CLEARLOCKS" and loFormSet.Ariaform1.cbSysOrMd.Value = 2
*!*	  *** Verify files for all companies
*!*	  IF loFormSet.llAllComp
*!*	   * lnLastComp = ALEN(loFormSet.laCompany,1)
*!*	   ** FOR lnCompNo = 2 TO lnLastComp
*!*	   **   *IF loFormSet.laCompany[lnCompNo,4]
*!*	   **   *  *IF gfModalGen('QRM00442B00006','Dialog') = 1
*!*	   **   *  *  =lfClrLkSQL()
*!*	   **   *  *ENDIF
*!*	   **   *  EXIT
*!*	   **   *ENDIF
*!*	   ** ENDFOR
*!*	  ELSE
*!*	    *IF loFormSet.laCompany[loFormSet.Ariaform1.laCompany.ListIndex,4] .AND. gfModalGen('QRM00442B00006','Dialog') = 1
*!*	    *  =lfClrLkSQL()
*!*	    *ENDIF
*!*	  ENDIF
*!*	ENDIF

************************************************************
*! Name      : lfSqlDisconn
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/07/2013
*! Purpose   : Sql Disconnect
************************************************************
FUNCTION lfSqlDisconn
PARAMETERS loFormSet
LOCAL lcOnError,llError,lnCount
IF loFormSet.Ariaform1.cbSysOrMd.Value = 1
  lcOnError = ON('ERROR')
  ON ERROR llError = .T.
  FOR lnCount = 1 TO 100
    =SQLDISCONNECT(lnCount)
    llError = .F.
  ENDFOR
  ON ERROR &lcOnError
ENDIF
*- End of lfSqlDisconn.

************************************************************
*! Name      : lfCheckSqlFile
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/04/2013
*! Purpose   : *- if the file is sql then check if it is already there in the database
************************************************************
FUNCTION lfCheckSqlFile
PARAMETERS lcFileNam
LOCAL lnResult
lnResult = 0

IF sydfiles.CVER = 'A40'
  lnCompNo = IIF(TYPE('lnCompNo')='N',lnCompNo, loFormSet.Ariaform1.laCompany.ListIndex )
  lcConn = loFormSet.laCompany[lnCompNo,6]
  lcSqlStatment = 'SELECT TOP 0 * FROM '+lcFileNam
  lnResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcFileNam, '',;
                                  lcConn , 3, 'SAVE', SET("Datasession"))
ENDIF
RETURN lnResult > 0
*- End of lfCheckSqlFile.


*!*******************************************************************
*!
*!      Function: lfVer_Upd
*!
*!*******************************************************************
*
* Function to verify or update data and system files
*
FUNCTION lfVer_Upd
PARAMETERS lcFileNam,llUpdate

LOCAL lnI
DECLARE laFileStrc[1,1],laFileAStr[1,1]

llUpdate   = IIF(TYPE('llUpdate')='U',.F.,llUpdate)
lcTmpFNm   = ''
llFileCorr = .F.               && Flage If file Corrupted
llRetFlag  = .T.

*E303375,1 TMI 04/03/2013 [Start] set a filter condition to get the exact field based on the platform, either a27 or A40
m.CVER = sydfiles.CVER
*E303375,1 TMI 04/03/2013 [End  ]

SET ENGINEBEHAVIOR 70
SELECT ALLT(sydflfld.cfld_name),sydfield.cdata_typ,;
       IIF(sydfield.cdata_typ=='L' ,001,sydfield.nfld_wdth),;
       sydfield.nfld_dec,sydflfld.nfld_pos;
       FROM  (oAriaApplication.SysPath+"sydflfld"),(oAriaApplication.SysPath+"sydfield");
       ORDER BY sydflfld.nfld_pos;
       GROUP BY sydField.cFld_Name;
       WHERE UPPER(sydflfld.cfile_nam) = UPPER(PADR(lcFileNam,FSIZE('cfile_nam','sydflfld')));
       AND sydfield.cfld_name = sydflfld.cfld_name AND (EMPTY(SYDFIELD.CVER) OR SYDFIELD.CVER = m.CVER) ;
       INTO ARRAY laFileStrc
SET ENGINEBEHAVIOR 90

*B610540,1 MMT 10/03/2013 fix Error if file records is not found in sydflfld[Start]
if !empty(laFileStrc)
*B610540,1 MMT 10/03/2013 fix Error if file records is not found in sydflfld[END]
=gfAdel(@laFileStrc,5,2)       && An additional colum holding the position
                               && of each field has to be removed from the
                               && Array befor verification or building.
*B610540,1 MMT 10/03/2013 fix Error if file records is not found in sydflfld[Start]
ENDIF
*B610540,1 MMT 10/03/2013 fix Error if file records is not found in sydflfld[End]
LOCAL lnCount
*B610447,1  TMI 07/24/2013 [Start] check laFileStrc not empty
if !empty(laFileStrc)
  *B610447,1  TMI 07/24/2013 [End  ] 
  FOR lnCount = 1 TO ALEN(laFileStrc,1)
    laFileStrc[lnCount,1] = ALLTRIM(laFileStrc[lnCount,1])
  ENDFOR
  *B610447,1  TMI 07/24/2013 [Start] 
endif
*B610447,1  TMI 07/24/2013 [End  ] 

*- Disconnect sql
=lfSqlDisconn(loFormSet)

*** Check if file exist or build new one
IF (sydfiles.CVER = 'A27' AND FILE(loFormSet.lcTargDir+lcFileNam+".DBF")) .OR. (sydfiles.CVER = 'A40' AND llSqlTblFound)
  IF sydfiles.CVER = 'A27'
    *** File was found in target directory
    IF USED(lcFileNam)
      llOpened = .T.
      SELECT(lcFileNam)
    ELSE
      llOpened = .F.
      SELECT 0
      USE (loFormSet.lcTargDir+lcFileNam)
    ENDIF
  ELSE
    *B610540,2 MMT 10/07/2013 fix Error if SQ Table has no index[Start]
    *llOpened = gfOpenTable(lcFileNam,ALLTRIM(sydfiles.CFILE_TAG),'SH',lcFileNam,.T.,loFormSet.Ariaform1.laCompany.Value)
    llOpened = gfOpenTable(lcFileNam,"",'SH',lcFileNam,.T.,loFormSet.Ariaform1.laCompany.Value)
    *B610540,2 MMT 10/07/2013 fix Error if SQ Table has no index[End]
  ENDIF

  *** Actual file structure
  =AFIELDS(laTmpArr)

  *E303375,1 TMI 04/07/2013 [Start] check for memo fields, change its width from 10 to 4
  FOR lnI = 1 TO ALEN(laTmpArr,1)
    IF laTmpArr[lnI,2] = 'M'
      laTmpArr[lnI,3] = 4
    ENDIF
  ENDFOR
  *E303375,1 TMI 04/07/2013 [End  ]

  *- in sql case check if the REC_NO field exists
  * also sort the created array by field name
  * change the type T to D
  IF sydfiles.cVer = 'A40'
    lnFound_REC_NO = ASCAN(laFileStrc,'REC_NO ')
    IF lnFound_REC_NO = 0
      lnRec_no = ASCAN(laTmpArr,'REC_NO')
      IF lnRec_no > 0
        =ADEL(laTmpArr,ASUBSCRIPT(laTmpArr,lnRec_no,1))
        DIMENSION laTmpArr[ALEN(laTmpArr,1)-1,ALEN(laTmpArr,2)]
      ENDIF
    ENDIF
    FOR lnI = 1 TO ALEN(laTmpArr,1)
      IF laTmpArr[lnI,2]='T'
        laTmpArr[lnI,2]='D'
      ENDIF
    ENDFOR
    =ASORT(laTmpArr)
    =ASORT(laFileStrc)
  ENDIF

  *- get an array with only 4 columns as in vfp9 the created array using afields function has more columns
  DIMENSION laFileAStr[ALEN(laTmpArr,1),ALEN(laFileStrc,2)]
  FOR i = 1 TO ALEN(laFileAStr,1)
    FOR j=1 TO ALEN(laFileAStr,2)
      laFileAStr[i,j] = laTmpArr[i,j]
    ENDFOR
  ENDFOR

  IF !EMPTY(laFileStrc[1])            && Found data for this file
    *** Check all fields information
    FOR lnFieldNo = 1 TO ALEN(laFileStrc)     &&All Fields
      IF TYPE ('laFileAStr[lnFieldNo]') <> 'U'

        IF !(UPPER(PADR(laFileStrc[lnFieldNo],10))==UPPER(PADR(laFileAStr[lnFieldNo],10)))

          IF llUpdate
            llFileCorr = .T.
            EXIT
          ELSE
            *** Report corrupted field
            =lfReport(1)              && Wrong field
          ENDIF
        ENDIF
      ELSE
        IF llUpdate
          llFileCorr = .T.
          EXIT
        ELSE
          *** Report not existing field
          =lfReport(1)
        ENDIF
      ENDIF
    ENDFOR
    IF ALEN(laFileStrc)<>ALEN(laFileAStr) AND !llFileCorr
      llFileCorr = .T.
      =lfReport(4)
    ENDIF
    IF llFileCorr AND llUpdate
      IF !loFormSet.llYes2All
        *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
        =lfShowHideProgressBar()
        *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
        lnOption = gfModalGen('QRM00004B00004','Dialog',loFormSet.lcSubsVar)
        *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
        =lfShowHideProgressBar(.T.)
        *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
        DO CASE
          CASE lnOption = 1
            *** Fix file structure
            =lfUpdate(lcFileNam)
          CASE lnOption = 3
            loFormSet.llYes2All  = .T.
            =lfUpdate(lcFileNam)
          CASE lnOption = 4
            llRetFlag  = .F.
            =lfThermo(100 ,100,'','')
        ENDCASE
      ELSE
        *** Fix file structure
        =lfUpdate(lcFileNam)
      ENDIF

    ENDIF
  ELSE
    *** File infornation is not stored in dictionary
    =lfReport(2)
  ENDIF

  IF !llOpened
     USE
  ENDIF

*** File was not found in target directory
ELSE
  IF llUpdate
    *** Build new file
    =lfUpdate(lcFileNam,.T.)
  ENDIF
  =lfReport(3)
ENDIF

RETURN llRetFlag


************************************************************
*! Name      : lfCheckConnValid
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/04/2013
*! Purpose   : Check the connection to a company and try to open a table from
*************************************************************x
FUNCTION lfCheckConnValid
PARAMETERS lcCompanyID,lcConn
LOCAL lnSlct,lnDataSession
lnSlct = SELECT(0)

lcSycComp = gfTempName()
lnDataSession = SET("Datasession")
SET DATASESSION TO 1

lnConnHandle = 0
lnRemResult = oAriaApplication.remotesystemdata.execute("Select * from syccomp where cComp_ID='"+lcCompanyID+"'",'',lcSycComp,"",oAriaApplication.SystemConnectionString,3,"",1)
IF lnRemResult>=1
  SELECT (lcSycComp)
  lcConStr = oAriaApplication.mGenConstr()
  lcConn = lcConStr
  lnConnHandle = oAriaApplication.remotecompanydata.oConnectionsClass.Open(3,lcConStr,'SAVE')
ENDIF

SET DATASESSION TO lnDataSession
SELECT (lnSlct)
RETURN lnConnHandle>0
*- End of lfCheckConnValid.

************************************************************
*! Name      : updatetablestructure
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/03/2013
*! Purpose   : updatetablestructure
************************************************************
FUNCTION updatetablestructure
PARAMETERS loFormSet,llChkStruc

objremotedataaccess = CREATEOBJECT("remotedataaccess")
ndatasessionid = SET("Datasession")

lcCompid = loFormSet.Ariaform1.laCompany.Value
lnCompNo = IIF(TYPE('lnCompNo')='N',lnCompNo, loFormSet.Ariaform1.laCompany.ListIndex )
lcConnStr = loFormSet.laCompany[lnCompNo,6]
LOCAL lnhandle 
lnhandle = SQLSTRINGCONNECT(lcConnStr)

LOCAL lcSvOrdFlfld,lcSvOrdIndx,lcSvOrdField
lcSvOrdFlfld = ORDER('SYDFLFLD')
lcSvOrdIndx = ORDER('SYDINDEX')
lcSvOrdField = ORDER('sydfield')

SET ORDER TO CFILE_NAM IN SYDFLFLD
SET ORDER TO CFILE IN SYDINDEX
SET ORDER TO CFLD_NAME IN sydfield

SELECT sydfiles
lctable = ALLTRIM(sydfiles.cfile_nam)
lcfile_ttl = cfile_ttl
IF llChkStruc
  SET KEY TO PADR(ALLTRIM(lctable),oAriaApplication.FileW) IN sydflfld
  objremotedataaccess.mcreatetable(lccompid, lcTable, lcfile_ttl, 'sydflfld', 'sydfield', 'sydindex', ndatasessionid, lnhandle, .F., lcconnstr)
ENDIF
SET KEY TO PADR(ALLTRIM(lctable),oAriaApplication.FileW) IN sydindex
objremotedataaccess.mcreateindex(lccompid, lctable, lcfile_ttl, 'sydindex', ndatasessionid, lnhandle, lcconnstr)
SELECT sydindex
SET KEY TO
SET ORDER TO &lcSvOrdIndx
SELECT sydflfld
SET KEY TO
SET ORDER TO &lcSvOrdFlfld
SELECT sydfield
SET ORDER TO &lcSvOrdField

RELEASE objremotedataaccess

*- End of updatetablestructure.


************************************************************
*! Name      : lfThermo
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 04/02/2013
*! Purpose   : show thermometer
************************************************************
FUNCTION lfThermo
PARAMETERS lnTotRecs, lnThermRec, lcPstTB,lcTranNo

WAIT CLEAR
IF lnTotRecs = lnThermRec
  oProgress = .NULL.
  RELEASE oProgress
  RETURN
ENDIF

lcPstTB = IIF(EMPTY(lcPstTB),'',lcPstTB)
lcTranNo = IIF(EMPTY(lcTranNo),'',lcTranNo)

WITH oProgress
  .TotalProgress = lnTotRecs
  .lblFirstLabel.CAPTION = lcPstTB
  .lblSecondLabel.CAPTION = lcTranNo
  .CurrentProgress(lnThermRec)
  .SHOW()
ENDWITH
*- End of lfThermo.

*!*******************************************************************
*!
*!      Function: lfUpdate
*!
*!*******************************************************************
*
* This function will build a new data files from the dectionary
* and their indexes
FUNCTION lfUpdate
PARAMETERS lcFileNam,llBldFile

DO CASE
CASE SYDFILES.cVer = 'A27'

  llBldFile = IIF(TYPE('llBldFile')='U',.F.,llBldFile)
  loFormSet.lcTempNam = gfTempName()

  IF !EMPTY(laFileStrc[1])
    IF llBldFile
      CREATE DBF  (loFormSet.lcTargDir+lcFileNam) FROM ARRAY laFileStrc
    ELSE
      CREATE DBF  (loFormSet.lcTargDir+loFormSet.lcTempNam) FROM ARRAY laFileStrc
      APPEND FROM (loFormSet.lcTargDir+lcFileNam)
      SELECT (loFormSet.lcTempNam)
      USE

      IF USED(lcFileNam)
        USE IN (lcFileNam)
      ENDIF

      ERASE  (loFormSet.lcTargDir+lcFileNam+".DBF")
      ERASE  (loFormSet.lcTargDir+lcFileNam+".FPT")
      ERASE  (loFormSet.lcTargDir+lcFileNam+".CDX")

      RENAME (loFormSet.lcTargDir+loFormSet.lcTempNam+".DBF") TO (loFormSet.lcTargDir+lcFileNam+".DBF")

      IF FILE(loFormSet.lcTargDir+loFormSet.lcTempNam+'.FPT')	
        RENAME (loFormSet.lcTargDir+loFormSet.lcTempNam+".FPT") TO (loFormSet.lcTargDir+lcFileNam+".FPT")
      ENDIF

      SELECT 0
      USE (loFormSet.lcTargDir+lcFileNam)
    ENDIF

    =lfIndex(lcFileNam)

  ELSE
    WAIT loFormSet.lcTUnAble+lcFileNam WINDOW
  ENDIF

CASE SYDFILES.cVer = 'A40'

  =UpdateTableStructure(loFormSet,.T.)

ENDCASE

*!*******************************************************************
*!
*!      Function: lfIndex
*!
*!*******************************************************************
* This function will creat or fix the index for one file
FUNCTION lfIndex
PARAMETERS lcFileNam
DECLARE laFileCDX[1,1]

lcSavAlias = SELECT(0)
SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
       sydindex.lunique;
       FROM (oAriaApplication.SysPath+"sydindex");
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,oAriaApplication.FileW);
       INTO ARRAY laFileCDX

IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF

*- delete CDX file before reindexing so in case of having a fatel error in the harddisk
*- it will create a  New CDX file in a new cluster
IF sydfiles.cVer = 'A27'
  IF USED(lcFileNam)
    USE IN (lcFileNam)
  ENDIF
ELSE
  =gfCloseTable(lcFileNam)
ENDIF

IF sydfiles.cVer = 'A27'
  IF FILE(ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDX')
    IF FILE(ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDX')
       ERASE (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
    ENDIF
    RENAME ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDX' TO ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDB'
  ENDIF

  PRIVATE lcOnError,llError
  lcOnError = ON('ERROR')
  llError = .F.

  ON ERROR llError = .T.

  IF FILE(ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.DBF')
    USE (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)) EXCL
    *- If there was error while opening file then open it again so the header of the file
    *- will be modified that there is no indexes
    IF llError
      USE (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)) EXCL

    ENDIF
    IF !EMPTY(laFileCDX[1])
        FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
          lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
          lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
          INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2];
                ADDITIVE &lcAscend &lcUnique
        ENDFOR
    ENDIF
    USE
  ELSE
    =lfReport(2)
  ENDIF

  ON ERROR &lcOnError

  IF FILE(ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDX')
     ERASE (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
  ELSE
    IF FILE(ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDB')
      RENAME (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDB') TO (ALLTRIM(loFormSet.lcTargDir)+ALLTRIM(lcFileNam)+'.CDX)
    ENDIF
  ENDIF

ELSE

  =UpdateTableStructure(loFormSet,.F.)

ENDIF
SELECT (lcSavAlias)

*!*******************************************************************
*!
*!      Function: lfReOrg
*!
*!*******************************************************************
*!
FUNCTION lfReOrg
PARAMETERS lcFileNam
IF !USED('SYDFILES')
  SELECT 0
  USE (oAriaApplication.SysPath+'SYDFILES') EXCL
ELSE
  SELECT SYDFILES
ENDIF
SET ORDER TO TAG CFILE_NAM

IF UPPER(lcFileNam) == 'CODES'
  lcMastTag = 'CODES'
ELSE
  lcMastTag = LOOKUP(cfile_tag,lcFileNam,CFILE_NAM,'CFILE_NAM')
ENDIF

IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF

*-- use gfopenfile to be able to see our errorhandler so if any
*-- error in file openning error handler will fix it

IF sydfiles.cVer = 'A27'

  USE (loFormSet.lcTargDir+lcFileNam) EXCL

  IF !EMPTY(lcMastTag)
    SET ORDER TO TAG &lcMastTag
  ENDIF

  IF TYPE('lcExt') # "C"
    lcTempName = gfTempName()
    COPY TO (loFormSet.lcTargDir+lcTempName+".DBF") CDX FOR .T.
    USE

    ERASE (loFormSet.lcTargDir+lcFileNam+".DBF")
    ERASE (loFormSet.lcTargDir+lcFileNam+".CDX")
    ERASE (loFormSet.lcTargDir+lcFileNam+".FPT")

    IF FILE (loFormSet.lcTargDir+lcTempName+".DBF")
      RENAME (loFormSet.lcTargDir+lcTempName+".DBF") TO (loFormSet.lcTargDir+lcFileNam+".DBF")
    ENDIF

    IF FILE (loFormSet.lcTargDir+lcTempName+".CDX")
      RENAME (loFormSet.lcTargDir+lcTempName+".CDX") TO (loFormSet.lcTargDir+lcFileNam+".CDX")
    ENDIF

    IF FILE (loFormSet.lcTargDir+lcTempName+".FPT")
      RENAME (loFormSet.lcTargDir+lcTempName+".FPT") TO (loFormSet.lcTargDir+lcFileNam+".FPT")
    ENDIF
  ENDIF

ENDIF


*!*******************************************************************
*!
*!      Function: lfClrLock
*!
*!*******************************************************************
*
FUNCTION lfClrLock
PARAMETERS lcFileNam

IF sydfiles.cVer = 'A27'

  IF FILE(loFormSet.lcTargDir+lcFileNam+".DBF")
    IF !USED(lcFileNam)
      SELECT 0
    ELSE
      SELECT (lcFileNam)
    ENDIF
    USE (loFormSet.lcTargDir+lcFileNam) EXCL
   *- added condition to chek wether the locking fields
   *- exist or not before replacing because some files does not
   *- have the locking fields "ex. Resource file"
   IF TYPE('llok_stat')='L' AND TYPE('dlok_date')='D';
      AND TYPE('clok_user')='C' AND TYPE('clok_time')='C'
      REPLACE ALL llok_stat WITH .F. ;
                  dlok_date WITH {} ;
                  clok_user WITH "" ;
                  clok_time WITH "" ;
              FOR llok_stat
    ENDIF
    SELECT (lcFileNam)
    USE
  ENDIF

ELSE
  IF llSqlTblFound
    LOCAL lcConn,lcSqlStatment,lnConn,lnResult
    lnCompNo = IIF(TYPE('lnCompNo')='N',lnCompNo, loFormSet.Ariaform1.laCompany.ListIndex )
    lcConn = loFormSet.laCompany[lnCompNo,6]
    lcSqlStatment = "update &lcFileNam  "+;
                    "set "+;
                    "dlok_date = '',"+;
                    "lok_stat  = 0 ,"+;
                    "clok_user = '',"+;
                    "clok_time = '' "+;
                    "Where llok_stat>0"
    *lnResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStatment, lcFileNam, '', lcConn , 3, 'SAVE', SET("Datasession"))
    lnConn = SQLSTRINGCONNECT(lcConn)
    lnResult = SQLEXEC(lnConn,lcSqlStatment)
    =SQLDISCONNECT(lnConn)

  ENDIF

ENDIF

*!*******************************************************************
*!
*!      Function: lfReport
*!
*!*******************************************************************
*
FUNCTION lfReport
PARAMETERS lnAction
DECLARE laFieldTyp[4,1]
laFieldTyp[1]='name  '
laFieldTyp[2]='type  '
laFieldTyp[3]='width '
laFieldTyp[4]='dec.  '

IF !loFormSet.llOpenRep
  loFormSet.llOpenRep = .T.
  lcFilHandl = FCREAT(oAriaApplication.WorkDir+'structur.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,"*                  FILE VERIFICATION STATUS REPORT                  *")
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
ENDIF

IF loFormSet.lcTempNam <> lcFileNam
   loFormSet.lcTempNam =  lcFileNam
   IF loFormSet.lnRepLine > 0
     *=FPUTS(lcFilHandl,REPLICATE('Ä',68))
     =FPUTS(lcFilHandl,REPLICATE('-',68))
   ENDIF
   loFormSet.lnRepLine   = loFormSet.lnRepLine + 1
   lcTargDir = loFormSet.lcTargDir
  =FPUTS(lcFilHandl,STR(loFormSet.lnRepLine,2)+". File &lcTargDir&lcFileNam :")
ENDIF

DO CASE
  CASE lnAction = 1
    IF TYPE ('laFileAStr[lnFieldNo]') <> 'U'
      lcCurrFld=laFileAStr[ASUBSCRIPT('laFileAStr',lnFieldNo,1),1]
      IF lcTmpFNm <> lcCurrFld
        lcTmpFNm = lcCurrFld
        =FPUTS(lcFilHandl,;
        "   * Field &lcCurrFld"+SPACE(10-LEN(lcCurrFld))+":")
      ENDIF

      =FPUTS(lcFilHandl,;
        "       - Wrong "+laFieldTyp[ASUBSCRIPT('laFileStrc',lnFieldNo,2)]+;
        IIF(TYPE('laFileAStr[lnFieldNo]')='N',;
        STR(laFileAStr[lnFieldNo],2),ALLTRIM(laFileAStr[lnFieldNo]))+;
        " it should be "+;
        IIF(TYPE('laFileStrc[lnFieldNo]')='N',;
        STR(laFileStrc[lnFieldNo],2),ALLTRIM(laFileStrc[lnFieldNo])))
      ELSE
        lcCurrFld=laFileStrc[ASUBSCRIPT('laFileStrc',lnFieldNo,1),1]
        IF lcTmpFNm <> lcCurrFld
          lcTmpFNm = lcCurrFld
          *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	          =FPUTS(lcFilHandl,;
*!*	          "   * Field &lcCurrFld"+SPACE(10-LEN(lcCurrFld))+":")
*!*	          =FPUTS(lcFilHandl,"       - Field dose not Exist.")
          =FPUTS(lcFilHandl,;
IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Field,loFormSet.GetHeaderText("LANG_Field",loFormSet.HeaderAlias)) +"&lcCurrFld"+SPACE(10-LEN(lcCurrFld))+":")
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=FPUTS(lcFilHandl,LANG_Fld_Dosnt_Exist)
=FPUTS(lcFilHandl,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Fld_Dosnt_Exist,loFormSet.GetHeaderText("LANG_Fld_Dosnt_Exist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

          *N000682,1 04/17/2013 HES Globalization changes[End  ]
        ENDIF

      ENDIF

  CASE lnAction = 2
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	    =FPUTS(lcFilHandl,"   * File's information is not stored in the dictionary.")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=FPUTS(lcFilHandl,LANG_FD_notfound)
=FPUTS(lcFilHandl,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FD_notfound,loFormSet.GetHeaderText("LANG_FD_notfound",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
  CASE lnAction = 3
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	    =FPUTS(lcFilHandl,"   * File dose not Exist.")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=FPUTS(lcFilHandl,LANG_File_Does_Not_Exist)
=FPUTS(lcFilHandl,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_Does_Not_Exist,loFormSet.GetHeaderText("LANG_File_Does_Not_Exist",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
  CASE lnAction = 4
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	    =FPUTS(lcFilHandl,"   * File Mismatch structure. or have removed fields")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*=FPUTS(lcFilHandl,LANG_File_Mismatch)
=FPUTS(lcFilHandl,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_Mismatch,loFormSet.GetHeaderText("LANG_File_Mismatch",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
  CASE lnAction = 5
    *=FPUTS(lcFilHandl,"   * Proplems Found . or file access denied")
    =FPUTS(lcFilHandl,&lcFixMess)
ENDCASE


*!*******************************************************************
*!
*!      Function: lfvAselect
*!
*!*******************************************************************
*
* Fill array with marked options from multyselect popup
*
FUNCTION lfvAselect
PARAMETERS loFormSet

DECLARE loFormSet.laSelected [1,3]
loFormSet.laSelected = ''
lnAryLen   = 0

FOR lnCount = 1 TO ALEN(loFormSet.laApp_fl,1)
  IF LEFT(loFormSet.laApp_fl[lnCount,1],1)=""
    lnAryLen = lnAryLen + 1
    DECLARE loFormSet.laSelected [lnAryLen,3]
    *B610532,1 TMI 09/27/2013 [Start] deal with file width oAriaApplication.FileW property
    *loFormSet.laSelected [lnAryLen,1] = SUBSTR(loFormSet.laApp_fl[lnCount,1],2,9)
    *loFormSet.laSelected [lnAryLen,2] = SUBSTR(loFormSet.laApp_fl[lnCount,1],13 )
    *loFormSet.laSelected [lnAryLen,3] = loFormSet.laApp_fl[lnCount,3]
    loFormSet.laSelected [lnAryLen,1] = SUBSTR(loFormSet.laApp_fl[lnCount,1],2,oAriaApplication.FileW+1)
    loFormSet.laSelected [lnAryLen,2] = SUBSTR(loFormSet.laApp_fl[lnCount,1],oAriaApplication.FileW+5  )
    loFormSet.laSelected [lnAryLen,3] = loFormSet.laApp_fl[lnCount,3]
    *B610532,1 TMI 09/27/2013 [End  ] 
  ENDIF
ENDFOR

*!*******************************************************************
*!
*!      Function: lfImport
*!
*!*******************************************************************
*
FUNCTION lfImport
PARAMETERS loFormSet
PRIVATE ALL LIKE l*

*** Let the user point to any file to import it's structure to the
*** dectionary or to reupdate it's information if it is olredy
*** exist in the dectionary
*N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	lcPathFile= getfile("DBF","Select file to import it's structure ...")
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcPathFile= getfile("DBF",LANG_Select_File)
lcPathFile= getfile("DBF",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select_File,loFormSet.GetHeaderText("LANG_Select_File",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 HES Globalization changes[End  ]

*** If no selection return without doing anyting
IF EMPTY(lcPathFile)
  RETURN
ELSE
  *** Cutt off the path and extention
  lcFileName = SUBSTR(SUBSTR(lcPathFile,RAT('\',lcPathFile)+1),1,RAT('.',SUBSTR(lcPathFile,RAT('\',lcPathFile)+1))-1)
ENDIF

=gfOpenFile(oAriaApplication.SysPath+'SYDFILES','CFILE_NAM')
=gfOpenFile(oAriaApplication.SysPath+'SYDFLFLD','CFILE_NAM')
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD','CFLD_NAME')
=gfOpenFile(oAriaApplication.SysPath+'SYDINDEX','CFILE_NAM')

*** Open the file to check it's structure
IF !USED(lcFileName)
  SELECT 0
  USE(lcPathFile)
ELSE
  SELECT (lcFileName)
ENDIF

*** Get the file structre
DECLARE laFileStrc[1,4]
DECLARE laTagesExp[1,2]
laFileStrc = " "
laTagesExp = " "
=AFIELDS('laFileStrc')

*** Collect all tages in an array
lnTagNo = 1
DO WHILE !EMPTY(TAG(lnTagNo))
  IF !EMPTY(laTagesExp[1,1])
    DECLARE laTagesExp[ALEN('laTagesExp',1)+1,2]
  ENDIF
  laTagesExp[lnTagNo,1] = TAG(lnTagNo)
  laTagesExp[lnTagNo,2] = SYS(14,lnTagNo)
  lnTagNo = lnTagNo + 1
ENDDO


*** Check if the file exist or not and update it's data
SELECT SYDFILES
*** If the file is not in the dectionary add new record
IF !SEEK(lcFileName)
  APPEND BLANK
ENDIF

REPLACE cfile_nam  WITH  lcFileName                              ;
        nfld_no    WITH  ALEN('laFileStrc',1)                    ;
        nindx_no   WITH lnTagNo-1                                ;
        cfile_tag  WITH IIF(EMPTY(cfile_tag),laTagesExp[1,1],cfile_tag)   ;
        cfile_ttl  WITH IIF(EMPTY(cfile_ttl),lcFileName,cfile_ttl);
        cupgrdlvl  WITH IIF(EMPTY(cupgrdlvl),"A",cupgrdlvl)      ;
        cadd_user  WITH oAriaApplication.User_Id                                ;
        cadd_time  WITH gfGetTime()                              ;
        dadd_date  WITH DATE()                                   ;
        llok_stat  WITH .F.

*** Update file field data and field dectionary data
SELECT SYDFLFLD

IF SEEK(lcFileName)
   REPLACE ALL cfile_nam WITH " " FOR cfile_nam = lcFileName
   DELETE  ALL FOR EMPTY(cfile_nam)
ENDIF

SET DELETE OFF
FOR lnFldNo = 1 TO ALEN('laFileStrc',1)

  IF SEEK(" ")  .AND. DELETED()
    RECALL
  ELSE
    APPEND BLANK
  ENDIF

  *** Update the file field data
  REPLACE cfile_nam WITH lcFileName                           ;
          cfld_name WITH laFileStrc[lnFldNo,1]                ;
          nfld_pos  WITH lnFldNo                              ;
          cupgrdlvl WITH IIF(LEFT(lcFileName,2)="SY","S","A") ;
          cadd_user WITH oAriaApplication.User_Id                            ;
          cadd_time WITH gfGetTime()                          ;
          dadd_date WITH DATE()                               ;
          llok_stat WITH .F.
ENDFOR
SET DELETE ON

*** Update the field dictionaty with new information about each field
SELECT SYDFIELD
FOR lnFldNo = 1 TO ALEN('laFileStrc',1)
  IF !SEEK(laFileStrc[lnFldNo,1])
    APPEND BLANK
  ENDIF

  REPLACE cfld_name WITH  laFileStrc[lnFldNo,1]   ;
          cdata_typ WITH  laFileStrc[lnFldNo,2]   ;
          nfld_wdth WITH  laFileStrc[lnFldNo,3]   ;
          nfld_dec  WITH  laFileStrc[lnFldNo,4]   ;
          cupgrdlvl WITH  IIF(EMPTY(cupgrdlvl),IIF(LEFT(lcFileName,2)="SY","S","A"),cupgrdlvl);
          cadd_user WITH  oAriaApplication.User_Id               ;
          cadd_time WITH  gfGetTime()             ;
          dadd_date WITH  DATE()                  ;
          llok_stat WITH  .F.

ENDFOR


*** Update index name data
SELECT (lcFileName)
lcCdxName = FULLPATH(CDX(1))
lcCdxName = SUBSTR(lcCdxName,RAT('\',lcCdxName)+1)

SELECT SYDINDEX

FOR lnTagNo = 1 TO ALEN(laTagesExp,1)
 IF !SEEK(lcFileName+laTagesExp[lnTagNo,1])
   APPEND BLANK
 ENDIF

 REPLACE cindx_nam WITH  lcCdxName             ;
         cfile_nam WITH  lcFileName            ;
         cfile_tag WITH  laTagesExp[lnTagNo,1] ;
         cindx_exp WITH  laTagesExp[lnTagNo,2] ;
         cupgrdlvl WITH   IIF(EMPTY(cupgrdlvl),IIF(LEFT(lcFileName,2)="SY","S","A"),cupgrdlvl);
         cadd_user WITH  oAriaApplication.User_Id             ;
         cadd_time WITH  gfGetTime()           ;
         dadd_date WITH  DATE()                ;
         llok_stat WITH  .F.
ENDFOR

*         lascend   WITH
*         lunique   WITH


USE IN (lcFileName)
USE IN SYDFILES
USE IN SYDFIELD
USE IN SYDINDEX
USE IN SYDFLFLD
*!**************************************************************************
*! Name      : lfvFix
*! Developer : Walid Abou El-Magd
*! Date      : 10/01/1999
*! Purpose   : fix
*!**************************************************************************
FUNCTION lfvFix
PARAMETERS loFormSet

=lfvAselect(loFormSet)                 && Collect Selected files
IF !EMPTY(loFormSet.laSelected [1,1])   && Exit if None
  *-- Save trapping setting .
  *lcHldTab = ON('KEY','TAB')
  *lcHldBtb = ON('KEY','BACKTAB')
  *lcHldEnt = ON('KEY','ENTER')
  *ON KEY LABEL TAB
  *ON KEY LABEL BACKTAB
  *ON KEY LABEL ENTER
  *-- Call  screen.
  lnFix = 1
  lnBackup = 1
  lnLastBackup = 1
  lnLog = 1
  IF TYPE('loFormSet.llExtCall')='U' OR !loFormSet.llExtCall
    DO FORM (oAriaApplication.ScreenHome+'SM'+"\SMFIX.SCX") WITH loFormSet
  ELSE
    DO FORM (oAriaApplication.ScreenHome+"SM\SMFIX.SCX") WITH loFormSet
  ENDIF
  *-- Restore trapping setting .
  *ON KEY LABEL TAB     &lcHldTab
  *ON KEY LABEL BACKTAB &lcHldBtb
  *ON KEY LABEL ENTER   &lcHldEnt
ELSE
  *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
  =lfShowHideProgressBar()
  *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
  =gfModalGen ('INM00018B00000','Dialog')
  *B610333,1 fix media R13 issues TMI 05/22/13 [Start] hide the progress bar before the messagebox, show it after
  =lfShowHideProgressBar(.T.)
  *B610333,1 fix media R13 issues TMI 05/22/13 [End  ] 
ENDIF
*!**************************************************************************
*! Name      : lfvProceed
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   :
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************
*! Returns   :
*!**************************************************************************
*! Example   :
*!**************************************************************************
*! Notes     :
*!**************************************************************************
FUNCTION lfvProceed
PARAMETERS loFormSet
*-- Start define the parameters from the screen
loFormSet.Ariaform1.pbClose.Enabled = .F.
DIMENSION laFixInfo[10]
laFixInfo=" "
laFixInfo[1]=" "

*-- Set the what to do parameter according to its control variable lnFix
DO CASE
  CASE lnFix=1
    laFixInfo[2]="A" &&-- Ask
  CASE lnFix=2
    laFixInfo[2]="D" &&-- Don't Fix
  CASE lnFix=3
    laFixInfo[2]="F" &&-- AutoFix
ENDCASE

*-- Set the detail parameter according to its control variable lnLog
DO CASE
  CASE lnLog=1
    laFixInfo[3]="T" &&-- Detail
  CASE lnLog=2
    laFixInfo[3]="F" &&-- Summary
ENDCASE

*-- Set the backup parameter according to its control variable lnBackup
DO CASE
  CASE lnBackup=1
    laFixInfo[4]="A" &&-- Ask
  CASE lnBackup=2
    laFixInfo[4]="T" &&-- Automatically make backup
  CASE lnBackup=3
    laFixInfo[4]="F" &&-- Don't make backup
ENDCASE

*-- Set the last backup parameter according to its control variable lnLastBackup
DO CASE
  CASE lnLastBackup=1
    laFixInfo[5]="A" &&-- Ask
  CASE lnLastBackup=2
    laFixInfo[5]="T" &&-- Auto-Overwrite
  CASE lnLastBackup=3
    laFixInfo[5]="F" &&-- Make new
ENDCASE

=lfDoAction(loFormSet,"CHECK")
loFormSet.Ariaform1.pbClose.Enabled = .T.

*!**************************************************************************
*! Name      : lfFixDbf
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Call and analysis the return values from gfFixDbf()
*!**************************************************************************
FUNCTION lfFixDbf
=gfFixDbf(@laFixInfo)
lcFixMess=SPACE(0)
DO CASE
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  CASE UPPER(laFixInfo[6])="NO PROBLEMS FOUND"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE UPPER(laFixInfo[6])=LANG_NO_PRO
CASE UPPER(laFixInfo[6])=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_PRO,loFormSet.GetHeaderText("LANG_NO_PRO",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]
    RETURN
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  CASE UPPER(laFixInfo[6])="FILE ACCESS DENIED"
*!*	    lcFixMess = "'   * File Access Denied'"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE UPPER(laFixInfo[6])=LANG_FILE_ACCESS_DENIED
CASE UPPER(laFixInfo[6])=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FILE_ACCESS_DENIED,loFormSet.GetHeaderText("LANG_FILE_ACCESS_DENIED",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFixMess = '"'+LANG_FILE_ACCESS_DENIED_1+'"'
lcFixMess = '"'+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FILE_ACCESS_DENIED_1,loFormSet.GetHeaderText("LANG_FILE_ACCESS_DENIED_1",loFormSet.HeaderAlias))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]
    =lfReport(5)
    RETURN
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  CASE UPPER(laFixInfo[6])="FILE NOT EXIST"
*!*	    lcFixMess = "'* File Not Exist'"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE UPPER(laFixInfo[6])=LANG_File_not_Exist
CASE UPPER(laFixInfo[6])=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_not_Exist,loFormSet.GetHeaderText("LANG_File_not_Exist",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFixMess = "'* "+LANG_File_not_Exist+'"'
lcFixMess = "'* "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_not_Exist,loFormSet.GetHeaderText("LANG_File_not_Exist",loFormSet.HeaderAlias))+'"'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]
    =lfReport(5)
    RETURN
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  CASE UPPER(laFixInfo[6])="PROBLEMS FOUND"
*!*	    lcFixMess = "   * Problems Found"+'|'
*!*	    lcFixMess =lcFixMess+IIF(!EMPTY(laFixInfo[8]),'   * Backup :  '+laFixInfo[8]+'|',SPACE(0))+IIF(laFixInfo[3]="T",laFixInfo[9],SPACE(0))+"   * Current Status :"+laFixInfo[10]+'|'
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*CASE UPPER(laFixInfo[6])=LANG_PROBLEMS_FOUNDs
CASE UPPER(laFixInfo[6])=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PROBLEMS_FOUNDs,loFormSet.GetHeaderText("LANG_PROBLEMS_FOUNDs",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFixMess = "   * "+LANG_PROBLEMS_FOUNDs+'|'
lcFixMess = "   * "+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PROBLEMS_FOUNDs,loFormSet.GetHeaderText("LANG_PROBLEMS_FOUNDs",loFormSet.HeaderAlias))+'|'
*N000682,1 11/20/2012 MMT Globlization changes[End]

    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lcFixMess =lcFixMess+IIF(!EMPTY(laFixInfo[8]),LANG_Backup+laFixInfo[8]+'|',SPACE(0))+IIF(laFixInfo[3]="T",laFixInfo[9],SPACE(0))+LANG_Current_Status+laFixInfo[10]+'|'
lcFixMess =lcFixMess+IIF(!EMPTY(laFixInfo[8]),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Backup,loFormSet.GetHeaderText("LANG_Backup",loFormSet.HeaderAlias))+laFixInfo[8]+'|',SPACE(0))+IIF(laFixInfo[3]="T",laFixInfo[9],SPACE(0))+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Current_Status,loFormSet.GetHeaderText("LANG_Current_Status",loFormSet.HeaderAlias))+laFixInfo[10]+'|'
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[Start]

    lcFixMess = PADR(lcFixMess,LEN(lcFixMess)-1)
    lcFixMess = STRTRAN(lcFixMess,'|','"+CHR(13)+"' )
    lcFixMess = '"'+lcFixMess+'"'
    =lfReport(5)
ENDCASE

PROCEDURE lpExtCall
loFormSet.Ariaform1.laCompany.ListIndex = 1
loFormSet.Ariaform1.cbSysOrMd.Value = 2
loFormSet.lcOldComp="ABCDEFG"
=lfvCompany()
=lfvTag(loFormSet)
=gfModalGen ('INM00364B00000','Alert')
=lfvFix()
loFormSet.Ariaform1.cbSysOrMd.Value=1
=lfvSysOrMd(loFormSet,loFormSet.AriaForm1.cbSysOrMd)
=lfvTag(loFormSet)
=gfModalGen ('INM00365B00000','Alert')
=lfvFix()
return

*!*************************************************************
*! Name      : lfPrnt
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 27/02/2001
*! Purpose   : Print the Report structure discreptions.
*!*************************************************************
*! Passed Parameters      : None.
*!*************************************************************
*! Returns                : ....
*!*************************************************************
*! Example   : = lfPrnt()
*!*************************************************************
FUNCTION lfVPrnt

IF pSetup(.T.)
  gcOutFile = oAriaApplication.WorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF

*--End of lfVPrnt.

*:*************************************************************
*: Name      : lpErrHand
*: Developer : Abdou Elgendy (ABD)
*: Date      : 08/06/2002
*: Purpose   : Function add recored into error File.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None()
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : DO lpErrHand WITH LINENO()
*:*************************************************************
PROCEDURE lpErrHand
PARAMETERS lnLine
PRIVATE lcErrNum , lcLine , lcMessage
ON ERROR

lcErrNum  = ALLTRIM(STR(ERROR()))
lcLine    = ALLTRIM(STR(lnLine))
lcMessage = MESSAGE()


*N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	lcFixMess = ['Error Number  : ' + lcErrNum  + CHR(13) +;
*!*	             'Line Number   : ' + lcLine    + CHR(13) +;
*!*	             'Error Message : ' + lcMessage + CHR(13)]
lcFixMess =  [']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Error_Num,loFormSet.GetHeaderText("LANG_Error_Num",loFormSet.HeaderAlias))     +['] + lcErrNum  + CHR(13) +;
             [']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Line_Number,loFormSet.GetHeaderText("LANG_Line_Number",loFormSet.HeaderAlias)) +['] + lcLine    + CHR(13) +;
             [']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Error_Msg,loFormSet.GetHeaderText("LANG_Error_Msg",loFormSet.HeaderAlias))     +['] + lcMessage + CHR(13)+[']
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 HES Globalization changes[Start]

= lfReport(5)

ON ERROR DO lpErrHand WITH LINENO()
*-- End OF lpErrHand.

*:*************************************************************
*: Name      : lfClrLkSQL
*: Developer : AHMED MAHER (AMH)
*: Date      : 07/31/2005
*: Purpose   : Function to clear locks for SQL tables of Aria 4 XP.
*:*************************************************************
*: Called from : Prog.
*:*************************************************************
*: Calls       : None()
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : lfClrLkSQL()
*:*************************************************************
FUNCTION lfClrLkSQL

lcTmpMemo  = gfTempName()
lcXComp_ID = loFormSet.lcComp_ID
lcXSysHome = oAriaApplication.SysPath

SAVE TO (oAriaApplication.WorkDir+lcTmpMemo+'.MEM') ALL LIKE lcX*
lcCommLine = (oAriaApplication.WorkDir+lcTmpMemo+'.MEM')
lcLib=oAriaApplication.defaultpath+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (oAriaApplication.defaultpath+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcAppHome+'SM\'+[SMCLRLCK.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   RELEASE LIBRARY (oAriaApplication.defaultpath+"FOXTOOLS.FLL")
ELSE

  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*WAIT LANG_LIBRARY_NOT_FOUND WINDOW
WAIT IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_LIBRARY_NOT_FOUND,loFormSet.GetHeaderText("LANG_LIBRARY_NOT_FOUND",loFormSet.HeaderAlias)) WINDOW
*N000682,1 11/20/2012 MMT Globlization changes[End]

  RETURN .F.
ENDIF
*--end of lfClrLkSQL.



*!***************************************************************************
*! Name      : gfFixDbf
*! Developer : Walid Abou El-Magd  (WAM)
*! Date      : 09/24/1999
*! Purpose   : Function to automaticlly fix any corrupted DBF in ARIA27 system.
*! Job ID    : E# 301316,1
*!***************************************************************************
*! Calls     : None
*!***************************************************************************
*! Return    : None
*!***************************************************************************
*! Passed Parameters  : laFixInfo  : Pointer to the fix information array.
*! Notes  : This charachter array consistes of 10 cells
*!        : The programmer fill the first 5 , And the function return the rest.
*!        : laFixInfo[1] = The full path of the file to be fixed...
*!
*!        : laFixInfo[2] = What do you want the function to do
*!                         "F"-->Auto Fix & "A"-->Ask befor fixing & "D" -> Don't fix
*!
*!                         [if not passed "A" will be the default]
*!
*!        : laFixInfo[3] = Do you want to get detail information about the problems found in this file?
*!                         "T"=Yes & "F"=No [if not passed "F" will be the default]
*!
*!        : laFixInfo[4] = Do you want to make a backup for this file befor fixing?
*!                         "T"=Yes & "F"=No & "A"=Ask befor backup
*!                         [if not passed "A" will be the default]
*!
*!        : laFixInfo[5] = Do you want to overwrite the last backup?
*!                         "T"=Yes & "F"=No & "A"=Ask[if not passed "A" will be the default]
*!
*!        : This function return its information in the next elementes of array
*!
*!        laFixInfo[6]  -> File Check State one of the following strings
*!                         "FILE NOT EXIST"    , "FILE ACCESS DENIED"
*!                         "NO PROBLEMS FOUND" , "PROBLEMS FOUND"
*!        laFixInfo[7]  -> Disk Space report
*!                         " ","Insufficient Disk Space"
*!        laFixInfo[8]  -> Backup report
*!                         " ","Canceled By User","<File name of backup>"
*!        laFixInfo[9]  -> Problems report [all problems found in the file]
*!        laFixInfo[10] -> File report [Fixed] or [Not Fixed]
*!***************************************************************************
*! Example   : DIMENSION laFixInfo[10]
*!             laFixInfo=" "
*!             laFixInfo[1]=lcDataDir+lcFileName
*!             laFixInfo[2]="F"        &&--Auto Fix
*!             laFixInfo[3]="T"        &&--Details
*!             laFixInfo[4]="A"        &&--Ask befor backup
*!             laFixInfo[5]="A"        &&--Ask if overwrite the last backup?
*!             = gfFixDbf(@laFixInfo)
*!***************************************************************************
FUNCTION gfFixDbf
PARAMETERS laFixInfo
PRIVATE llAutoFix,llDontFix,llGetInfo,llBackup,llBackAsk,llBackOver,llOverAsk,lnChkVal
STORE .F. TO llAutoFix,llDontFix,llGetInfo,llBackup,llBackAsk,llBackOver,llOverAsk
STORE  0  TO lnChkVal
*--
IF TYPE("laFixInfo")# "C" OR EMPTY(laFixInfo[1]) OR !(UPPER(LEFT(ALLTRIM(laFixInfo[2]),1))$"FAD")
  RETURN
ENDIF
*-- Set the required libraries
PRIVATE lcSetLibr
lcSetLibr = SET("LIBRARY")
IF !(oAriaApplication.defaultpath+"FOXTOOLS.FLL" $ lcSetLibr)
  SET LIBRARY TO (oAriaApplication.defaultpath+"FOXTOOLS.FLL") ADDITIVE
ENDIF
IF !(oAriaApplication.defaultpath+"FOXTOOLS.FLL" $ lcSetLibr)
  SET LIBRARY TO (oAriaApplication.defaultpath+"FFIX4_25.FLL") ADDITIVE
ENDIF

FOR lnX= 1 TO 10
  IF lnX <=5
    STORE UPPER(ALLTRIM(laFixInfo[lnX])) TO laFixInfo[lnX]
  ELSE
    STORE SPACE(0) TO laFixInfo[lnX]
  ENDIF
ENDFOR

*-- Add the extension (.DBF) to the full file name .
*-- It is not important If this file name passed with or without extension.
laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")

*-- Check if the file is phyiscally exist on the disk
IF !FILE(laFixInfo[1])
*N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	  laFixInfo[6]="FILE NOT EXIST"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[6]=LANG_File_not_Exist
laFixInfo[6]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_File_not_Exist,loFormSet.GetHeaderText("LANG_File_not_Exist",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 HES Globalization changes[End  ]
  = LfRelLibr()
  RETURN
ENDIF

*-- Set the control variable llAutoFix if not passed default it to Ask
IF !LEFT(laFixInfo[2],1)$"FAD"
  laFixInfo[2]="A"
  llAutoFix=.F.

ELSE
  llAutoFix=(LEFT(laFixInfo[2],1)="F")
  llDontFix=(LEFT(laFixInfo[2],1)="D")
ENDIF


*-- If detail parameter is not passed default it to "F"=NO.
IF !LEFT(laFixInfo[3],1)$"TF"
  laFixInfo[3]="F"
ELSE
  llGetInfo=(LEFT(laFixInfo[3],1) = "T" )
ENDIF

*-- If backup parameter is not passed default it to "A"=Ask befor backup

IF !LEFT(laFixInfo[4],1)$"TFA"
  laFixInfo[4]= "A"
  STORE .T. TO llBackAsk
ELSE
  llBackup    = LEFT(laFixInfo[4],1) = "T"
  llBackAsk   = LEFT(laFixInfo[4],1) = "A"
ENDIF

*-- If what about last backup parameter is not passed default it to "A"=Ask.
IF !LEFT(laFixInfo[5],1)$"TFA" OR LEFT(laFixInfo[5],1)$"A"
  *-- In this case the variable llBackOver will be set or reset by the user
  laFixInfo[5]="A"
  llOverAsk   =.T.
ELSE
  llBackOver=LEFT(laFixInfo[5],1) = "T"
ENDIF

*-- Now it is the time to check
lnChkVal=FIXDBF(laFixInfo[1],0)

DO CASE
  CASE lnChkVal <= 0
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	    laFixInfo[6]=IIF(lnChkVal=0,"NO PROBLEMS FOUND","FILE ACCESS DENIED")
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[6]=IIF(lnChkVal=0,LANG_NO_PRO,LANG_FILE_ACCESS_DENIED)
laFixInfo[6]=IIF(lnChkVal=0,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_NO_PRO,loFormSet.GetHeaderText("LANG_NO_PRO",loFormSet.HeaderAlias)),IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_FILE_ACCESS_DENIED,loFormSet.GetHeaderText("LANG_FILE_ACCESS_DENIED",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
    = LfRelLibr()
    RETURN
  CASE lnChkVal > 0
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	    laFixInfo[6]="PROBLEMS FOUND"
    *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[6]=LANG_PROBLEMS_FOUNDs
laFixInfo[6]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_PROBLEMS_FOUNDs,loFormSet.GetHeaderText("LANG_PROBLEMS_FOUNDs",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
ENDCASE

*-- if detail information required
IF llGetInfo
  DO WHILE lnChkVal > 0
    DO CASE
	  CASE lnChkVal >= 64
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * Memo Non-Standard Block size |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info1   + ' |'
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info1,loFormSet.GetHeaderText("LANG_Info1",loFormSet.HeaderAlias))   + ' |'
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 64
	  CASE lnChkVal >= 32
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * Memo Next Available Block incorrect |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info2+ ' |'
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info2,loFormSet.GetHeaderText("LANG_Info2",loFormSet.HeaderAlias))+ ' |'
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
        lnChkVal = lnChkVal - 32
	  CASE lnChkVal >= 16
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * CDX flag incorrect |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info3  +' |'	
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info3,loFormSet.GetHeaderText("LANG_Info3",loFormSet.HeaderAlias))  +' |'	
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 16
      CASE lnChkVal >= 8
      *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * DBF type ID incorrect |'
      *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[9]=laFixInfo[9]+LANG_Info4+' |'
laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info4,loFormSet.GetHeaderText("LANG_Info4",loFormSet.HeaderAlias))+' |'
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 8
	  CASE lnChkVal >= 4
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * Record count incorrect |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info5  +' |'
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info5,loFormSet.GetHeaderText("LANG_Info5",loFormSet.HeaderAlias))  +' |'
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 4
	  CASE lnChkVal >= 2
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * Record length incorrect |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info5  +' |'
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info5,loFormSet.GetHeaderText("LANG_Info5",loFormSet.HeaderAlias))  +' |'
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 2
	  CASE lnChkVal >= 1
	    *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*		    laFixInfo[9]=laFixInfo[9]+'   * Header length incorrect |'
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	    laFixInfo[9]=laFixInfo[9]+LANG_Info5  +' |'	
	    laFixInfo[9]=laFixInfo[9]+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Info5,loFormSet.GetHeaderText("LANG_Info5",loFormSet.HeaderAlias))  +' |'	
*N000682,1 11/20/2012 MMT Globlization changes[End]

	    *N000682,1 04/16/2013 HES Globlization changes[End  ]
	    lnChkVal = lnChkVal - 1
    ENDCASE
  ENDDO		
ENDIF
*-- If must ask fix or not....
*-- Do you want to fix the file (XXX)?
IF llDontFix OR (!llAutoFix AND gfModalGen('QRM00357B00006','Dialog',laFixInfo[1])=2)
  *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	  laFixInfo[10] = "Not Fixed"
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10] = LANG_Not_Fixed
laFixInfo[10] = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Not_Fixed,loFormSet.GetHeaderText("LANG_Not_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/16/2013 HES Globlization changes[End  ]
  = LfRelLibr()
  RETURN
ENDIF
*--the next code must be in case of fix,Either forced or user-choice .


IF llBackAsk
  *-- All the next calculations must be done before the message since we
  *-- Need the size in this message.
  PRIVATE lnFileSize
  lnFileSize=0
  =ADIR(laFileSize,laFixInfo[1])    &&-- size of .DBF
  lnFileSize=laFileSize[1,2]
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF
  *-- Restore the original extension.
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")
  *-- Make a backup for XXX ?
  *-- Total Size nnnnnnn Byets
  *-- <Yes> <No>
  llBackup = IIF(gfModalGen('QRM00358B00006','Dialog',laFixInfo[1]+'|'+STR(lnFileSize))=2,.F.,.T.)
  *-- Report the backup information...
  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	  laFixInfo[8]=IIF(llBackup,"","Canceled By User")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[8]=IIF(llBackup,"",LANG_Cancelled_ByUser)
laFixInfo[8]=IIF(llBackup,"",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancelled_ByUser,loFormSet.GetHeaderText("LANG_Cancelled_ByUser",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
ENDIF

*-- If backup required
IF llBackup

  PRIVATE lcBakupExt,lcLastBakup,lcExt,lnOption,lnFileSize,;
          laFileSize,lnDrivSize,laBackSize,lnBackSize,;
          lnSecPClus,lnBytPSect,lnFreeClus,lnTotClus,lcFreeSpac,lcConsSet

  STORE SPACE(0) TO lcBakupExt,lcLastBakup,lcFreeSpac,lcConsSet

  STORE  0       TO lnFileSize,lnBackSize,lnDrivSize,lnSecPClus,lnBytPSect,lnFreeClus,lnTotClus
  STORE  1       TO lnOption

  *-- Get the file size...[Start]

  *-- Note that: we can't use wildcard characters (* and ?) to get all needed
  *-- Information in only one call to ADIR() to avoid selection old backups
  *-- For this file .
  =ADIR(laFileSize,laFixInfo[1])    &&-- size of .DBF
  lnFileSize=laFileSize[1,2]
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .CDX
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
  IF FILE(laFixInfo[1])
    =ADIR(laFileSize,laFixInfo[1])    &&-- size of .FPT
    lnFileSize=lnFileSize+laFileSize[1,2]
  ENDIF
  *-- Restore the original extension.
  laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")

  *-- Get the file size...[End..]

  *-- Get the disk free space [Start]
  *-- Fox Help Said That :
  *-- On some networks, the value returned by the fox function DISKSPACE()
  *-- may not be accurate for large network drives.
  *-- So I will use the windows API to get the free disk space.

  lcConsSet = SET('CONS')
  SET CONS OFF
  DIR LIKE *.XXX TO FILE (oAriaApplication.WorkDir+'Tmpspace.TXT')
  SET CONS &lcConsSet
  CREATE CURSOR TMPSPCE (mTxt M)
  APPEND BLANK
  APPEND MEMO TMPSPCE.mTxt FROM (oAriaApplication.WorkDir+'Tmpspace.TXT')
  lcFreeSpac = SUBSTR(MLINE(TMPSPCE.mTxt,;
  ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))),1,;
  ATC("BYTES REMAINING",MLINE(TMPSPCE.mTxt,ATCLINE("BYTES REMAINING",UPPER(TMPSPCE.mTxt))))-1)
  lnDrivSize= ROUND((VAL(lcFreeSpac)/1024/1024),1)


  *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	  laFixInfo[7]=IIF(lnDrivSize > lnFileSize," ","Insufficient Disk Space")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[7]=IIF(lnDrivSize > lnFileSize," ",LANG_Insuf_DS)
laFixInfo[7]=IIF(lnDrivSize > lnFileSize," ",IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Insuf_DS,loFormSet.GetHeaderText("LANG_Insuf_DS",loFormSet.HeaderAlias)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  *N000682,1 04/17/2013 HES Globalization changes[End  ]
  *-- Get the disk free space [End..]

  *-- Get a valid new backup extention [Start]

  FOR lnX = 1 to 99
    IF !FILE(FORCEEXT(laFixInfo[1],"D"+IIF(lnX < 10 ,'0','')+ALLTRIM(STR(lnX))))
      lcBakupExt="D"+IIF(lnX < 10 ,'0','')+ALLTRIM(STR(lnX))
      EXIT
    ENDIF
  ENDFOR

  DO CASE
    *-- Case Maximum number of backups encountered
    CASE EMPTY(lcBakupExt)
      *-- If llBackOver it's ok , if not I will inform the user that I can't
      *-- Make a new backup <Overwrite>,<Fix without backup>,<Cancel>
      IF llOverAsk OR !llBackOver
        PRIVATE lnDummy
        *-- Get information about the ".?99"
        =ADIR(laBackSize,FORCEEXT(laFixInfo[1],'?99'))
        *-- Max number of backups encounterd
        *-- Name :XXX.D99
        *-- Date :XX\XX\XX
        *-- <Overwrite>,<Skip backup>,<Cancel> NEW B

        *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	        lnDummy = gfModalGen('QRM00361B00036','Dialog','Maximum number of backups encountered'+'|'+FORCEEXT(laFixInfo[1],'D99')+'|'+DTOC(laBackSize[1,3]))
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*lnDummy = gfModalGen('QRM00361B00036','Dialog',LANG_Max+'|'+FORCEEXT(laFixInfo[1],'D99')+'|'+DTOC(laBackSize[1,3]))
lnDummy = gfModalGen('QRM00361B00036','Dialog',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Max,loFormSet.GetHeaderText("LANG_Max",loFormSet.HeaderAlias))+'|'+FORCEEXT(laFixInfo[1],'D99')+'|'+DTOC(laBackSize[1,3]))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        *N000682,1 04/16/2013 HES Globlization changes[End  ]

        DO CASE
          CASE lnDummy=1
            lcBakupExt="D99"
          CASE lnDummy=2
            *-- Report the backup state
            *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	            laFixInfo[8] ="Canceled By User"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[8] =LANG_Cancelled_ByUser
laFixInfo[8] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancelled_ByUser,loFormSet.GetHeaderText("LANG_Cancelled_ByUser",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 04/17/2013 HES Globalization changes[End  ]
            lcBakupExt=" "
          CASE lnDummy=3
            = LfRelLibr()
            *-- Report the backup state
            *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	            laFixInfo[8] ="Canceled By User"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[8] =LANG_Cancelled_ByUser
laFixInfo[8] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancelled_ByUser,loFormSet.GetHeaderText("LANG_Cancelled_ByUser",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 04/17/2013 HES Globalization changes[End  ]
            *-- Report the File state
            *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	            laFixInfo[10]="Not Fixed"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10]=LANG_Not_Fixed
laFixInfo[10]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Not_Fixed,loFormSet.GetHeaderText("LANG_Not_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 04/17/2013 HES Globalization changes[End  ]
            RETURN
        ENDCASE
      ELSE  &&-- this else means Auto-overwrite is the passed parameter
        *-- Adjust the the backup name we will overwrite it from empty to "D99"
        lcBakupExt="D99"
      ENDIF
    CASE lcBakupExt # "D01"
      *-- To get a date for the message
      =ADIR(laBackSize,FORCEEXT(laFixInfo[1],"D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0))+ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))))
      IF llOverAsk
        *-- Over write the last backup (XXX) ?
        *-- Date  : XX/XX/XX     <Yes>,<No>
        llBackOver=IIF(gfModalGen('QRM00359B00006','Dialog',laBackSize[1,1]+'|'+DTOC(laBackSize[1,3]))=1,.T.,.F.)
      ENDIF
      IF llBackOver
        *-- This line give me the ext of the last backup
        lcBakupExt = "D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0)) +ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))
      ENDIF
  ENDCASE

  *-- Now we have a lcBakupExt maybe old (to overwrite) maybe new (to creat)
  *-- We don't know till now , But we can .
  IF !EMPTY(lcBakupExt)  &&-- May go empty in CASE lnDummy=2 from case EMPTY(lcBakupExt)
    IF FILE(FORCEEXT(laFixInfo[1],lcBakupExt)) &&-- this an overwrite case
      =ADIR(laBackSize,FORCEEXT(laFixInfo[1],STRTRAN(lcBakupExt,'D','?')))
      FOR I=1 TO ALEN(laBackSize,1)
        lnBackSize=lnBackSize+laBackSize[I,2]
      ENDFOR
      *-- Insufficient Disk Space , Fix without backup? <Fix>,<Cancel>
      IF lnDrivSize+lnBackSize < lnFileSize AND gfModalGen('QRM00360B00037','Dialog')=2
      *N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	        laFixInfo[7] ="Insufficient Disk Space"
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[7] =LANG_Insuf_DS
laFixInfo[7] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Insuf_DS,loFormSet.GetHeaderText("LANG_Insuf_DS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

      *N000682,1 04/17/2013 HES Globalization changes[End  ]
        *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10]=LANG_Not_Fixed
laFixInfo[10]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Not_Fixed,loFormSet.GetHeaderText("LANG_Not_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

        = LfRelLibr()
        RETURN
      ENDIF
    ELSE &&-- Creat new backup
      IF lnDrivSize < lnFileSize
        =ADIR(laBackSize,FORCEEXT(laFixInfo[1],"D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0))+ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))))
        FOR I=1 TO ALEN(laBackSize,1)
          lnBackSize=lnBackSize+laBackSize[I,2]
        ENDFOR
        IF lnDrivSize+lnBackSize < lnFileSize
          *-- Insufficient Disk Space , Fix without backup? <Fix>,<Cancel>
          IF gfModalGen('QRM00360B00037','Dialog')=2

            *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	            laFixInfo[7] ="Insufficient Disk Space"
*!*	            laFixInfo[10]="Not Fixed"
            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[7] =LANG_Insuf_DS
laFixInfo[7] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Insuf_DS,loFormSet.GetHeaderText("LANG_Insuf_DS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10]=LANG_Not_Fixed
laFixInfo[10]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Not_Fixed,loFormSet.GetHeaderText("LANG_Not_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

            *N000682,1 04/16/2013 HES Globlization changes[End  ]

            = LfRelLibr()
            RETURN
          ENDIF
        ELSE
          *--Insufficient Disk Space,Overwrite the last backup?
          *-- Name : XXX
          *-- Date : XX/XX/XX   <Overwrite>,<Skip>,<Cancel>
          lnDummy=gfModalGen('QRM00361B00036','Dialog','Insufficient Disk Space,Overwrite the last backup?'+'|'+laBackSize[1,1]+'|'+DTOC(laBackSize[1,3]))
          DO CASE
            CASE lnDummy=1
              lcBakupExt = "D"+IIF(VAL(SUBSTR(lcBakupExt,2))-1 < 10 ,'0',SPACE(0)) +ALLTRIM(STR(INT(VAL(SUBSTR(lcBakupExt,2))-1),2))
            CASE lnDummy=2
              lcBakupExt = " "
               *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	              laFixInfo[8]="Canceled By User"
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[8]=LANG_Cancelled_ByUser
laFixInfo[8]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Cancelled_ByUser,loFormSet.GetHeaderText("LANG_Cancelled_ByUser",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

              *N000682,1 04/16/2013 HES Globlization changes[End  ]
            CASE lcBakupExt = 3
              *N000682,1 04/16/2013 HES Globlization changes[Start]
*!*	              laFixInfo[7] ="Insufficient Disk Space"
*!*	              laFixInfo[10]="Not Fixed"
              *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[7] =LANG_Insuf_DS
laFixInfo[7] =IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Insuf_DS,loFormSet.GetHeaderText("LANG_Insuf_DS",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

              *N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10]=LANG_Not_Fixed
laFixInfo[10]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Not_Fixed,loFormSet.GetHeaderText("LANG_Not_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

              *N000682,1 04/16/2013 HES Globlization changes[End ]
              = LfRelLibr()
              RETURN
          ENDCASE
        ENDIF
      ENDIF
    ENDIF
  ENDIF

  IF !EMPTY(lcBakupExt)
    laFixInfo[8]=FORCEEXT(laFixInfo[1],lcBakupExt)
    COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"CDX")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.D','.C')
    IF FILE(laFixInfo[1])
      COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    ENDIF
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"FPT")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.C','.F')
    IF FILE(laFixInfo[1])
      COPY FILE &laFixInfo[1] TO &laFixInfo[8]
    ENDIF
    laFixInfo[1]=FORCEEXT(laFixInfo[1],"DBF")
    laFixInfo[8]=STRTRAN(laFixInfo[8],'.F','.D')
  ENDIF

ENDIF &&-- llBackup
=FIXDBF(laFixInfo[1],1,64)
SELECT 0
USE &laFixInfo[1] EXCLUSIVE
REINDEX
USE
*N000682,1 04/17/2013 HES Globalization changes[Start]
*!*	laFixInfo[10]="Fixed"
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*laFixInfo[10]=LANG_Fixed
laFixInfo[10]=IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Fixed,loFormSet.GetHeaderText("LANG_Fixed",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

*N000682,1 04/17/2013 HES Globalization changes[End  ]
= LfRelLibr()
RETURN


*!*************************************************************
*! Name      : LfRelLibr
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/12/2000
*! Purpose   : Release the libarary if it not setting by another prog.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : = LfRelLibr()
*!*************************************************************
FUNCTION LfRelLibr

IF !(oAriaApplication.defaultpath+"FOXTOOLS.FLL" $ lcSetLibr)
  RELEASE LIBRARY (oAriaApplication.defaultpath+"FOXTOOLS.FLL")
ENDIF
IF !(oAriaApplication.defaultpath+"FOXTOOLS.FLL" $ lcSetLibr)
  RELEASE LIBRARY (oAriaApplication.defaultpath+"FFIX4_25.FLL" )
ENDIF
*-- End of LfRelLibr











************* THE FOLLOWING IS FOR TEST ONLY, REMOVE WHEN SHIP THE PROGRAM
*!*************************************************************
*! Name        : gfOpenTable
*! Developer   : Ahmad Shoukry Mohammed (ASM)
*! Date        : 03/28/2005
*! Purpose     : Create a Remote Table Object if Trying to open Fox or Sql Table Defined in the Sysem Files
*! Tracking #  : N039155,1 ASM 03/28/2005 Converting Programs and Screens to work With SQL.
****************************************************************************
*! Parameters  : File Name, Index Tag, Open Mode ("EX" or "SH"), Alias Name, Force Open
*!*************************************************************
*! Returns            :  True  ----> If passed file is open by this function
*!                       False ----> If passed file is already open.
*!*************************************************************
FUNCTION _gfOpenTable
LPARAMETERS NFILE,lcIndex,MODE,lcAliasNam,llForceOp,lcCompId,llFastTable
LOCAL lnTable, lnL

lcAliasNam = IIF(TYPE('lcAliasNam')#'C' OR EMPTY(lcAliasNam),JUSTSTEM(NFILE),lcAliasNam)
lnTable = gfGetRemoteTable(SET("Datasession"),lcAliasNam)

IF lnTable=0 && No Remote Table Object was Found

  lnL = ALEN(oAriaApplication.laRemoteTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')='O'
    lnL = lnL + 1
    DIMENSION oAriaApplication.laRemoteTable[lnL]
  ENDIF
  *WAIT WINDOW NFILE+', '+lcIndex+', '+PADR(SET("Datasession"),10) TIMEOUT 0.5
  lcIndex = IIF(EMPTY(lcIndex),'',JUSTSTEM(lcIndex))
  oAriaApplication.laRemoteTable[lnL] = CREATEOBJECT("RemoteTable",JUSTSTEM(NFILE),lcIndex,lcAliasNam,SET("Datasession"),lcCompId,llFastTable)
  IF TYPE('oAriaApplication.laRemoteTable[lnL]')<>'O'
    lnL = MAX(lnL - 1,1)
    DIMENSION oAriaApplication.laRemoteTable[lnL]
    RETURN gfOpenFile(NFILE,lcIndex,MODE,lcAliasNam,llForceOp)
  ENDIF

ELSE && a Remote Table Object Already Exist

  IF !EMPTY(lcIndex)
    *WAIT WINDOW lcIndex TIMEOUT 0.5
    oAriaApplication.laRemoteTable[lnTable].SetOrder(JUSTSTEM(lcIndex))
  ENDIF

ENDIF

RETURN
*--end of gfOpenTable


*********************************************************************
* show hide progress bar 
* I created this function to overcome a conflicting problem with the messagebox, that is , when showing gfModalGen while the prgress bar is running all are stuck 
*********************************************************************
function lfShowHideProgressBar
parameters llShow

if type('oProgress') = 'O'
  oProgress.Visible = llShow
endif 

*-End of lfShowHideProgressBar.
************************************************************
*! Name      : lfFormActivate
*! Developer : TMI - Tarek Mohamed Ibrahim
*! Date      : 10/01/2013
*! Purpose   : activate form method 
************************************************************
FUNCTION lfFormActivate
PARAMETERS loFormSet
IF TYPE('loFormSet.llFirstRun')<>'L'
  loFormSet.AddProperty('llFirstRun')
  loFormset.AriaForm1.pbTag.Click()
  loFormset.AriaForm1.pbTag.Click()
ENDIF 


*- End of lfFormActivate.