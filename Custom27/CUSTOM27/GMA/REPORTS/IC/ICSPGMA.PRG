*:----------------------------------------------------------------------------------------------
*: Program file        : ICSPRUN.PRG
*: Program description : INVENTORY BY STYLE AVAILABILITY REPORT DATA PREPERATION(Running the report)
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (IC)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : C102224
*:----------------------------------------------------------------------------------------------
*!*	*:MODIFICATIONS
*B604906,1 TMI 09/16/2001 Run Sales and Product report each in a separate menu line
*:----------------------------------------------------------------------------------------------


*:**************************************************************************
*:* Name        : lfUpFltExp
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Update the filter expression lcRpExp by mover variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpFltExp()
*:***************************************************************************
FUNCTION lfUpFltExp
=lfUpdOneVr('laRpTCatg' ,'CSTYCATG')
=lfUpdOneVr('laRpTClass','CSTYCLASS')
=lfUpdOneVr('laRpTSbCls','CSTYSBCLAS')
*-- end of lfUpFltExp.

*:**************************************************************************
*:* Name        : lfUpdOneVr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Update lcRpExp for one variable
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfUpdOneVr()
*:***************************************************************************
FUNCTION lfUpdOneVr
PARAMETERS lcTArrName,lcField
PRIVATE lnCnt , lcAddStr
lcAddStr = ''
FOR lnCnt = 1 TO ALEN(&lcTArrName)
  lcAddStr = lcAddStr + PADR(&lcTArrName.[lnCnt],6) + ' | '
NEXT
lcAddStr = IIF(ALLTRIM(lcAddStr)='|','',lcAddStr)
IF !EMPTY(lcAddStr)
  lcRpExp = lcRpExp+[ AND UPPER(]+lcField+[) $ "]+lcAddStr+["]
ENDIF
*-- end of lfUpdOneVr.

*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : When OG Function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRpWhen() ()
*:***************************************************************************
FUNCTION lfwRpWhen
*B604906,1 TMI [Start] Get the parameter to know what report to run ( sales or product )
PRIVATE lcMenuID
*--Return the menu ID
lcMenuID = ALLTRIM(POPUP())+'-'+ PADL(LTRIM(STR(BAR())),2,'0')
*--Return the parameter name which contains the report name to run
IF ASCAN(gaProcess,lcMenuID)>0
  lcRpRepNam = ALLTRIM(gaProcess(ASUBSCRIPT(gaProcess,ASCAN(gaProcess,lcMenuID),1),8))
  lcDispReNm  = IIF('SLS'$lcRpRepNam,'Sales','Product')
  lnDispPos = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcDispReNm'),1)
  laOGObjCnt[lnDispPos] = .F.
  =lfOGShowGet('lnRPOTSMin')
ENDIF

*B604906,1 TMI [End  ]
*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfSRV
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Set Reset Valid Function for Style-InRange
*:***************************************************************************
*:* Called from : Style-InRange
*:***************************************************************************
*:* Passed Parameters : lcParm
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     : = lfSRV('S')
*:***************************************************************************
FUNCTION lfSVSty
PARAMETERS lcParm

SELECT STYLE
IF lcParm = "S" 
  lcStyOrder = ORDER("STYLE")
  *C102224,4 TMI [Start] Browse Style/Color    
  *SET ORDER TO CSTYLE IN STYLE
  SET ORDER TO STYLE IN STYLE
  *C102224,4 TMI [End  ]
  SELECT STYLE
  LOCATE
ELSE  && ELSE Rest case
  IF !EMPTY(lcStyOrder)   
    SET ORDER TO &lcStyOrder IN STYLE
  ENDIF
ENDIF
*-- end of lfSRV.

*:**************************************************************************
*:* Name        : lfFillCode
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Fill mover arrays in OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFillCode()
*:***************************************************************************
FUNCTION lfFillMvrs
DIME laRpSDep[1,1]  ,laRpTDep[1,1],;
     laRpSCatg[1,1] ,laRpTCatg[1,1],;
     laRpSClass[1,1],laRpTClass[1,1],;
     laRpSSbCls[1,1],laRpTSbCls[1,1]
STORE '' TO laRpSDep,laRpTDep,laRpSCatg,laRpTCatg,laRpSClass,laRpTClass,laRpSSbCls,laRpTSbCls

SELECT DISTINCT DEPT + ' - ' + CDEPTDESC ;
FROM ICDEPTHD ;
ORDER BY DEPT ;
INTO ARRAY laRpSDep


*B604906,1 TMI [Start] Get catg,class and subclass from codes file instead of style file
*!* SELECT DISTINCT UPPER(CSTYCATG);
*!* FROM STYLE ;
*!* WHERE !EMPTY(CSTYCATG) ;
*!* INTO ARRAY laRpSCatg
*!* =ASORT(laRpSCatg)
*!* 
*!* SELECT DISTINCT UPPER(CSTYCLASS) ;
*!* FROM STYLE ;
*!* WHERE !EMPTY(CSTYCLASS) ;
*!* INTO ARRAY laRpSClass
*!* =ASORT(laRpSClass)
*!* 
*!* SELECT DISTINCT UPPER(CSTYSBCLAS) ;
*!* FROM STYLE ;
*!* WHERE !EMPTY(CSTYSBCLAS) ;
*!* INTO ARRAY laRpSSbCls
*!* =ASORT(laRpSSbCls)

SELECT CODES
SET ORDER TO IDRLTFNAME
LOCATE
SELECT CCODE_NO+' - '+CDISCREP ;
FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCSTYCATG  ' ;
INTO ARRAY laRpSCatg
=ASORT(laRpSCatg)

LOCATE
SELECT CCODE_NO+' - '+CDISCREP ;
FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCSTYCLASS ' ;
INTO ARRAY laRpSClass
=ASORT(laRpSClass)

LOCATE
SELECT CCODE_NO+' - '+CDISCREP ;
FROM CODES ;
WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'NNCSTYSBCLAS' ;
INTO ARRAY laRpSSbCls
=ASORT(laRpSSbCls)
*B604906,1 TMI [End  ] Get catg,class and subclass from codes file instead of style file
       
*-- end of lfFillMvrs.

*:**************************************************************************
*:* Name        : lfvDept
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Valid mover function for Departments
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvDept()
*:***************************************************************************
FUNCTION lfvDept
=gfMover(@laRpSDep,@laRpTDep,'Select Departments',.T.,'')
*-- end of lfvDept.

*:**************************************************************************
*:* Name        : lfvCatg
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Valid mover function for Categories
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCatg()
*:***************************************************************************
FUNCTION lfvCatg
=gfMover(@laRpSCatg,@laRpTCatg,'Select Categories',.T.,'')
*-- end of lfvCatg.

*:**************************************************************************
*:* Name        : lfvClass
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Valid mover function for Classes
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvClass()
*:***************************************************************************
FUNCTION lfvClass
=gfMover(@laRpSClass,@laRpTClass,'Select Classes',.T.,'')
*-- end of lfvClass.

*:**************************************************************************
*:* Name        : lfvSbClass
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : Valid mover function for sub classes
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSbClass()
*:***************************************************************************
FUNCTION lfvSbClass
=gfMover(@laRpSSbCls,@laRpTSbCls,'Select Sub Classes',.T.,'')
*-- end of lfvSbClass.

*:**************************************************************************
*:* Name        : lfSupprs
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2001
*:* Purpose     : Return the needed value for llSupprs variable
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSupprs()
*:***************************************************************************
FUNCTION lfSupprs
RETURN .F.
*-- end of lfSupprs.



*:**************************************************************************
*:* Name        : lfvViewRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2001
*:* Purpose     : Apply the run button of the OG
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvViewRep()
*:***************************************************************************
*C102224,4
FUNCTION lfvViewRep

IF !lfIcsprun()
  RETURN
ENDIF

*--Restore old variables at criteria time
*RESTORE FROM (gcWorkDir+'icsprun.MEM') ADDITIVE

*--Set output to window
lnOutput = 0 

*--Run the EXE
=lfFinalRun()

*-- end of lfvViewRep.

*:**************************************************************************
*:* Name        : lfvRunRep
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2001
*:* Purpose     : Apply the Preview button of the OG
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvRunRep()
*:***************************************************************************
*C102224,4
FUNCTION lfvRunRep
IF !lfIcsprun()
  RETURN
ENDIF

*--Restore old variables at criteria time
*RESTORE FROM (gcWorkDir+'icsprun.MEM') ADDITIVE

*--Set output to printer
lnOutput = 1

*--Run the EXE
=lfFinalRun()

*-- end of lfvRunRep.

*:**************************************************************************
*:* Name        : lfIcsprun
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2001
*:* Purpose     : Restore variables from saved memo file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfIcsprun()
*:***************************************************************************
FUNCTION lfIcsprun
PRIVATE llRet
llRet = .F.
*--Restore old variables at criteria time
IF FILE(gcWorkDir+'icsprun.MEM')
  llRet = .T.
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Please run the collecting data program first.')
ENDIF  
RETURN llRet 
*-- end of lfMemFile.


*:**************************************************************************
*:* Name        : lfFinalRun
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/05/2001
*:* Purpose     : Running the exe
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfFinalRun()
*:***************************************************************************
FUNCTION lfFinalRun

*--Update the lcRpExp
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

lcRpExp=lcOGHDFlt+IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,' AND (','')+;
        lcOGFxFlt+IIF(lnOGVr>0,IIF(lnOgFx>0,' AND ',''),'')+lcOGVrFlt+;
        IIF((lnOGFx+lnOGVr>0) AND lnOGHd>0,')','')

*--Get needed variables
lcRpDtDir = gcDataDir
lcRpWrDir = gcWorkDir
lcRpSysDir = gcSysHome
lcRpRpDir = gcRepHome
lcRpImDir = gcImagDir
lcRpComp = gcAct_Comp
lcRpDevice = gcDevice
lcRpDefPth = gcDef_Path

*--Get the SQL SERVER name
lcSQLSvr = gfGetMemVar('M_SQLSVR')
*--Check that lcSQLSvr is not empty
IF EMPTY(lcSQLSvr)
  WAIT WINDOW 'Please write the SQL SERVER name in the IC setup.'
  RETURN
ENDIF

*-- save all variables to a mem file

SAVE TO (gcWorkDir+lcTmpMemo+'.MEM')

lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')

lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  *--Now run the VFP exe program that runs the report
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
    ",gcRepHome+'IC\'+[ICSPRUN.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
  llPoDisp = .F.
  RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
ENDIF
*-- end of lfFinalRun.

*B604906,1 TMI [Start]
*:**************************************************************************
*:* Name        : lfTitleSup
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/25/2001
*:* Purpose     : Supress the title "Report to run" in Collecting data ONLY
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfTitleSup()
*:***************************************************************************
FUNCTION lfTitleSup
RETURN .F.
*-- end of lfTitleSup.
*B604906,1 TMI [End  ]
