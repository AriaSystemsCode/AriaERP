*:----------------------------------------------------------------------------------------------
*: Program file        : ICSPGMA.PRG
*: Program description : INVENTORY BY STYLE AVAILABILITY REPORT DATA PREPERATION
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (IC)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : C102223
*:----------------------------------------------------------------------------------------------
*:MODIFICATIONS
*:*C102223,4 TMI 07/04/2001 Add an entry to the IC SETUP contains the SQLServer name
*:*B604906,1 TMI 09/26/2001 Get catg,class and subclass from codes file instead of style file
*:----------------------------------------------------------------------------------------------

*--Defining syrepuvr variables
*C102223,4 TMI [Start] Get the SQL Server name from the IC SETUP(CUSTOMIZED FOR GMA)
*--Get the SQL SERVER name
lcSQLSvr = gfGetMemVar('M_SQLSVR')
*--Check that lcSQLSvr is not empty
IF EMPTY(lcSQLSvr)
  =gfModalGen("INM000000B00000","DIALOG",'','','Please enter the SQL server name in the IC setup.')
  RETURN
ENDIF
*C102223,4 TMI [Start]

PRIVATE lcSlct,lcVarName
lcSlct = SELECT()
=gfOpenFile(gcSysHome+'SYREPUVR','FLD_NAME','SH')
SELECT SYREPUVR
SEEK 'ICSPGMA '
SCAN REST WHILE CREP_ID = 'ICSPGMA ' FOR UPPER(SUBSTR(MFLD_NAME,3,2)) = 'RP'
  lcVarName = SYREPUVR.MFLD_NAME
  PRIVATE &lcVarName
  &lcVarName = ''
ENDSCAN
USE IN SYREPUVR
SELECT(lcSlct)

*--Initialize lcRpExp var
lcRpExp = '.T.'

*C102224,4 TMI [Start] Suppress measument unit
*llMeasur = .T.
*C102224,4 TMI [End  ]

*--Calling the option grid
lcExpr = gfOpGrid('ICSPGMA' , .T.)


*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*--Update the filter expression lcRpExp by mover variables.
=lfUpFltExp()

*--Initialize color data variables
STORE 0 TO lnRpClrLen,lnRpClrPos

*--Get color data
=lfClrDtl()

*--Define arrays for PO cost sheet types
DIME laSetupP[5,2],laTypeP[1],laTypeD[1],laTypeF[1],laTypeM[1],laTypeS[1]
STORE '' TO laTypeP,laTypeD,laTypeF,laTypeM,laTypeS
=lfGetType()

lcRpDtDir = gcDataDir
lcRpWrDir = gcWorkDir
lcRpSysDir = gcSysHome
lcRpRpDir = gcRepHome
lcRpImDir = gcImagDir
lcRpComp = gcAct_Comp
lcRpDevice = gcDevice
lcRpDefPth = gcDef_Path
lcTmpMemo = gfTempName()

*-- save all variables to a mem file
*SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE l* 
SAVE TO (gcWorkDir+lcTmpMemo+'.MEM')
*C102224,4 TMI [Start] Save all criterias to memo file
SAVE TO (gcWorkDir+'icsprun.MEM')
*C102224,4 TMI [End  ] 
lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  *--Now run the VFP exe program that connects to SQL SERVER  and collects data
  WAIT WINDOW NOWAIT 'Wait while collecting data and sending it to SQL Server.'  
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcRepHome+'IC\'+[ICSPCLCT.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   llPoDisp = .F.
   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
ENDIF

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
*:* Name        : lfClrDtl
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/19/2001
*:* Purpose     : Get color details
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClrDtl()
*:***************************************************************************
FUNCTION lfClrDtl
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnRpClrLen = LEN(laItemSeg[lnCount,3])
    lnRpClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--end function lfGetClrD

*:**************************************************************************
*:* Name        : lfRepVars
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/20/2001
*:* Purpose     : Defining SYREPUVR variables
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfRepVars()
*:***************************************************************************
FUNCTION lfRepVars
PRIVATE lcSlct,lcOrd,lcVarName
lcOrd = ''
lcSlct = SELECT()
lcOrd = ORDER('SYREPUVR')

SELECT SYREPUVR
SET ORDER TO 1
SEEK 'ICSPGMA '
SCAN REST WHILE CREP_ID = 'ICSPGMA ' FOR UPPER(SUBSTR(MFLD_NAME,3,2)) = 'RP'
  lcVarName = SYREPUVR.MFLD_NAME
  PRIVATE &lcVarName
  &lcVarName = ''
ENDSCAN

IF !EMPTY(lcOrd)
  SET ORDER TO  &lcOrd IN SYREPUVR
ENDIF
SELECT(lcSlct)
*-- end of lfRepVars.

*:**************************************************************************
*:* Name        : lfGetType
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/01
*:* Purpose     : Get Types for style PO cost sheet
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetType()
*:***************************************************************************
FUNCTION lfGetType

PRIVATE     lcPType1,lcPType2,lcPType3,lcPType4,lcPType5,lnArrCnt
STORE '' TO lcPType1,lcPType2,lcPType3,lcPType4,lcPType5
STORE 0  TO lnArrCnt

laSetupP[1,1]  = 'M_cIType1'
laSetupP[2,1]  = 'M_cIType2'
laSetupP[3,1]  = 'M_cIType3'
laSetupP[4,1]  = 'M_cIType4'
laSetupP[5,1]  = 'M_cIType5'

*-- Use gfGetMemVar once per all variables.
=gfGetMemVar(@laSetupP, gcAct_Comp)  && get array for PO

*--lasetup for PO
lcPType1   = laSetupP[1,2]
lcPType2   = laSetupP[2,2]
lcPType3   = laSetupP[3,2]
lcPType4   = laSetupP[4,2]
lcPType5   = laSetupP[5,2]
FOR lnCheck = 1 TO 5
   lcType  = EVAL("lcPType" + STR(lnCheck,1))   
   =lfPType('laType'+lcType)
ENDFOR
*-- end of lfGetType.

*:**************************************************************************
*:* Name        : lfPType
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/24/2001
*:* Purpose     : Fill arrays laType
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPType()
*:***************************************************************************
FUNCTION lfPType
PARAMETERS lcArrName
IF EMPTY(&lcArrName.[1])
   &lcArrName.[1] = STR(lnCheck,1)
ELSE
   lnAryCount=ALEN(&lcArrName.,1)   
   DIMENSION &lcArrName.[lnAryCount+1]
   &lcArrName.[lnAryCount+1] = STR(lnCheck,1)
ENDIF
*-- end of lfPType.

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
*:* Example     :  = llSupprs()
*:***************************************************************************
FUNCTION lfSupprs
RETURN .T.
*-- end of lfSupprs.

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
RETURN .T.
*-- end of lfTitleSup.
*B604906,1 TMI [End  ]
