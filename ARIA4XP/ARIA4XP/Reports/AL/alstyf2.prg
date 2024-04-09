*:***************************************************************************
*: Program file  : ALSTYAL
*: Program desc. : ALLOCATION REPORT FORM #2 
*: Date          : 07/15/2007
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar[MMT](N037686)
*:***************************************************************************
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [T20100512.0026]
*:***************************************************************************
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
*!*	IF oAriaApplication.MULTIINST 
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\AL\ALSTYF2.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\AL\ALSTYF2.FXP WITH .F.,.F.   
*!*	ELSE
*!*	  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
*!*	  DO lcSrvRpt+"AL\ALSTYF2.FXP" WITH .F.,.F.
*!*	ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
PARAMETERS lcRequestID, lcXMLFileName, ClientID
*T20100512.0026 Hassan 2010 05 23 [END]
IF TYPE('lcXMLFileName') = 'C'
PRIVATE loAgent
loAgent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")

LOCAL loEnvironment
loEnvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
loEnvironment.ClientID = ClientID
loEnvironment.ConnectionsRefresh()
*T20100512.0026 Hassan 2010 05 23 [END]

LOCAL lcCurrentProcedure
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary\"), "", -1, 1, 1)
lcCurrentProcedure = STRTRAN(UPPER(loEnvironment.Aria40SystemFilesPath), UPPER("SQLDictionary"), "", -1, 1, 1)
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID)
 DO (lcCurrentProcedure + "SRVPRGS\SY\ariamain.fxp") WITH loAgent.GetRequestCompany(lcRequestID, ClientID), ClientID
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loProgress
loProgress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")

loProgress.Percent = 0
loProgress.Description = "Opening Data Files..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]


lcOldLib = SET("Classlib")
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
*goAriaEnvironment = CREATEOBJECT("AriaEnvironment", loAgent.GetRequestCompany(lcRequestID, ClientID))
*lfRestFromXML(FILETOSTR(lcXMLFileName))
goAriaEnvironment = oAriaEnvironment
goAriaEnvironment.xml.RestoreFromXML(FILETOSTR(lcXMLFileName),.T.)
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
*oAriaEnvironment.Xml.RestoreFromXMl(lcXMLVariable)


* MAH
*-- goAriaEnvironment.gcDevice = 'FILE'
IF LEFT(gcDevice, 7) = "PRINTER"
  goAriaEnvironment.gcDevice = "PRINTER"
ELSE
  goAriaEnvironment.gcDevice = "FILE"
ENDIF
goAriaEnvironment.gcDevice = 'FILE'
* MAH

lcOrdlTmp = goAriaEnvironment.Cursors.GetCursorTempName()
lcStyTemp = goAriaEnvironment.Cursors.GetCursorTempName()

goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'PIKTKT' ,'PIKTKT','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'ORDLINE' ,'ORDLINE','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'STYLE' ,'STYLE','SH','Style_X')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'CUSTOMER' ,'CUSTOMER','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'ORDHDR' ,'ORDHDR','SH')
goAriaEnvironment.remotetableaccess.OpenTable(goAriaEnvironment.DataDir + 'Scale' ,'Scale','SH')

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ENDIF 
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

*IF looGscroll.llOGFltCh
  lfCrtTemp()
  lfCollectData()
*ELSE
* USE oAriaEnvironment.WorkDir +  lcOrdlTmp + ".DBF" IN 0  
*ENDIF   
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE  
  IF looGscroll.llOGFltCh
    lfCrtTemp()
    lfCollectData()
  ELSE
    USE oAriaApplication.WorkDir +  lcOrdlTmp + ".DBF" IN 0  
  ENDIF   
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ] 

IF USED(lcOrdlTmp) AND RECCOUNT(lcOrdlTmp) = 0
  IF TYPE('lcXMLFileName') <> 'C'
    =gfModalGen('TRM00052B40011','ALERT')
  ENDIF   
  USE IN (lcOrdlTmp)
  RETURN 
ELSE
  IF USED(lcOrdlTmp)
    USE IN (lcOrdlTmp)
  ENDIF 
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

*lodisplayreport  = CREATEOBJECT("Report")
goAriaEnvironment.report.OGLastForm = 'ALSTYAL'

*goAriaEnvironment.report.llCrystal = .T.
*goAriaEnvironment.report.llCrystal = .F.

goAriaEnvironment.report.OGPlatform = 'WINDOWS'

DIMENSION goAriaEnvironment.report.laCRParams[2,2]

goAriaEnvironment.report.laCRParams[1,1] = 'ReportName'
goAriaEnvironment.report.laCRParams[1,2] = 'Allocation Report By Style' 

*llMultWH = .T.

goAriaEnvironment.report.laCRParams[2,1] = 'llMultWH'
goAriaEnvironment.report.laCRParams[2,2] = IIF(llMultWH,1,0)

goAriaEnvironment.report.cCROrientation = 'P'


goAriaEnvironment.report.cTextRepType  = goAriaEnvironment.cTextRepType
*goAriaEnvironment.report.cTextRepType = 'HTM'


DIMENSION goAriaEnvironment.report.lacrTABLES[1]  && array For Temp Table & pathes 
goAriaEnvironment.report.lacrTABLES[1]= goAriaEnvironment.WorkDir+lcOrdlTmp+'.DBF' 


loProgress.Percent = 0.9
loProgress.Description = "Printing Report..."
*T20100512.0026 Hassan 2010 05 23 [BEGIN]
*loAgent.UpdateObjectProgress(lcRequestID, loProgress)
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
*T20100512.0026 Hassan 2010 05 23 [END]

PRIVATE loProxy
*loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

*HUS
*IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
*HUS
  goAriaEnvironment.report.print(goAriaEnvironment.report.OGLastForm)

  loProgress.Percent = 1.0
  loProgress.Description = "Printing Report..."
  *T20100512.0026 Hassan 2010 05 23 [BEGIN]
  *loAgent.UpdateObjectProgress(lcRequestID, loProgress)
   loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
  *T20100512.0026 Hassan 2010 05 23 [END]
  
*HUS
*ENDIF
*HUS

SET CLASSLIB TO &lcOldLib 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE 
  DIMENSION loOGScroll.laCRParams[2,2]

  loOGScroll.laCRParams[1,1] = 'ReportName'
  loOGScroll.laCRParams[1,2] = 'Allocation Report By Style' 

  loOGScroll.laCRParams[2,1] = 'llMultWH'
  loOGScroll.laCRParams[2,2] = IIF(llMultWH,1,0)

  loogScroll.cCROrientation = 'P'

  DIMENSION loOgScroll.lacrTABLES[1]  && array For Temp Table & pathes 
  loOgScroll.lacrTABLES[1]= oAriaApplication.WorkDir+lcOrdlTmp+'.DBF' 
  =gfDispRe ()
ENDIF
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/15/2007
*! Purpose   : Load Settings before Report starts (When Func.)
*!*************************************************************
*! Called from : option grid of ALSTYAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************
FUNCTION lfwOgWhen

DIMENSION laOrdStru[1,4],laStyStru[1,4]

DECLARE laRpSource[5],laRpTarget[5]
STORE 'Open'     TO laRpSource[1] , laRpTarget[1] 
STORE 'Released' TO laRpSource[2] , laRpTarget[2]
STORE 'Pulled'   TO laRpSource[3] , laRpTarget[3]
STORE 'Hold'   TO laRpSource[4] , laRpTarget[4]

STORE 'Complete' TO laRpSource[5] , laRpTarget[5]
lcRpStatus = 'OXPHC'             && Variable that hold Status Exprission


= gfOpenTable(oAriaApplication.DataDir + 'PIKTKT' ,'PIKTKT','SH')
= gfOpenTable(oAriaApplication.DataDir + 'ORDLINE' ,'ORDLINE','SH')
= gfOpenTable(oAriaApplication.DataDir + 'STYLE' ,'STYLE','SH','Style_X')
= gfOpenTable(oAriaApplication.DataDir + 'CUSTOMER' ,'CUSTOMER','SH')
= gfOpenTable(oAriaApplication.DataDir + 'ORDHDR' ,'ORDHDR','SH')
*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : Sameh (SSE)
*! Date      : 03/25/1999
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : option grid of POPRLB.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
*
FUNCTION lfAdjSeg

STORE 0 TO lnFPos , lnDPos , lnZPos   , lnGPos , lnCPos , lnOPos , lnTPos , ;
           lnQPos , lnSPos , lnMajPos
STORE 0 TO lnMajLen
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
*--Get Major Length

FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it,
  *-- display the style major instead (style major will browse from the
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1
    LOOP
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos   = lnC
      lnClrPos = laMajSeg[lnC,4]
      lnClrPic = LEN(laMajSeg[lnC,3])
      *IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Only These Colors'
      *ENDIF
  ENDCASE
ENDFOR
*-- end of lfAdjSeg.
*!*************************************************************
*! Name      : lfvOStatus  
*! Developer : Mariam Mazhar[MMT]
*! Date      : 15/07/2007
*! Purpose   : - Evaluate Status expression.
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

= LFOGMover(@laRpSource,@laRpTarget,'Select PikTkt Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Released','X',;
                              IIF(laRpTarget[lnI] = 'Pulled','P' , ;
                              IIF(laRpTarget[lnI] = 'Hold','H' , ;
                              IIF(laRpTarget[lnI] = 'Complete','C','')))))                              
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'OXPHC', ALLTRIM(lcRpStatus))


*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

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
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : RefreshStatus
*! Developer : Mariam Mazhar (MMT) 
*! Date      : 07/16/2007
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
*!*************************************************************
*! Name      : lfCrtTemp
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/15/2007
*! Purpose   : Printing loop for the main report
*!*************************************************************
FUNCTION lfCrtTemp


IF USED(lcOrdlTmp)
 SELECT (lcOrdlTmp)
 ZAP 
ENDIF 

*CREATE the temp file to be bounded with the crystal report
DIMENSION laTableStruct[62,4]

laTableStruct[1,1] = 'Style'
laTableStruct[1,2] = "C"
laTableStruct[1,3] = 19
laTableStruct[1,4] = 0


laTableStruct[2,1] = 'BEGN1'
laTableStruct[2,2] = "N"
laTableStruct[2,3] = 7
laTableStruct[2,4] = 0

laTableStruct[3,1] = 'BEGN2'
laTableStruct[3,2] = "N"
laTableStruct[3,3] = 7
laTableStruct[3,4] = 0

laTableStruct[4,1] = 'BEGN3'
laTableStruct[4,2] = "N"
laTableStruct[4,3] = 7
laTableStruct[4,4] = 0

laTableStruct[5,1] = 'BEGN4'
laTableStruct[5,2] = "N"
laTableStruct[5,3] = 7
laTableStruct[5,4] = 0

laTableStruct[6,1] = 'BEGN5'
laTableStruct[6,2] = "N"
laTableStruct[6,3] = 7
laTableStruct[6,4] = 0

laTableStruct[7,1] = 'BEGN6'
laTableStruct[7,2] = "N"
laTableStruct[7,3] = 7
laTableStruct[7,4] = 0

laTableStruct[8,1] = 'BEGN7'
laTableStruct[8,2] = "N"
laTableStruct[8,3] = 7
laTableStruct[8,4] = 0

laTableStruct[9,1] = 'BEGN8'
laTableStruct[9,2] = "N"
laTableStruct[9,3] = 7
laTableStruct[9,4] = 0


laTableStruct[10,1] = 'TOTBGN'
laTableStruct[10,2] = "N"
laTableStruct[10,3] = 9
laTableStruct[10,4] = 0


laTableStruct[11,1] = 'Stk1'
laTableStruct[11,2] = "N"
laTableStruct[11,3] = 7
laTableStruct[11,4] = 0


laTableStruct[12,1] = 'Stk2'
laTableStruct[12,2] = "N"
laTableStruct[12,3] = 7
laTableStruct[12,4] = 0

laTableStruct[13,1] = 'Stk3'
laTableStruct[13,2] = "N"
laTableStruct[13,3] = 7
laTableStruct[13,4] = 0


laTableStruct[14,1] = 'Stk4'
laTableStruct[14,2] = "N"
laTableStruct[14,3] = 7
laTableStruct[14,4] = 0


laTableStruct[15,1] = 'Stk5'
laTableStruct[15,2] = "N"
laTableStruct[15,3] = 7
laTableStruct[15,4] = 0

laTableStruct[16,1] = 'Stk6'
laTableStruct[16,2] = "N"
laTableStruct[16,3] = 7
laTableStruct[16,4] = 0

laTableStruct[17,1] = 'Stk7'
laTableStruct[17,2] = "N"
laTableStruct[17,3] = 7
laTableStruct[17,4] = 0

laTableStruct[18,1] = 'Stk8'
laTableStruct[18,2] = "N"
laTableStruct[18,3] = 7
laTableStruct[18,4] = 0

laTableStruct[19,1] = 'totStk'
laTableStruct[19,2] = "N"
laTableStruct[19,3] = 9
laTableStruct[19,4] = 0

laTableStruct[20,1] = 'cWare'
laTableStruct[20,2] = "C"
laTableStruct[20,3] = 6
laTableStruct[20,4] = 0

laTableStruct[21,1] = 'ACCOUNT'
laTableStruct[21,2] = "C"
laTableStruct[21,3] = 5
laTableStruct[21,4] = 0

laTableStruct[22,1] = 'STORE'
laTableStruct[22,2] = "C"
laTableStruct[22,3] = 8
laTableStruct[22,4] = 0

laTableStruct[23,1] = 'STNAME'
laTableStruct[23,2] = "C"
laTableStruct[23,3] = 30
laTableStruct[23,4] = 0

laTableStruct[24,1] = 'PRIORITY'
laTableStruct[24,2] = "C"
laTableStruct[24,3] = 3
laTableStruct[24,4] = 0

laTableStruct[25,1] = 'STATUS'
laTableStruct[25,2] = "C"
laTableStruct[25,3] = 1
laTableStruct[25,4] = 0

laTableStruct[26,1] = 'START'
laTableStruct[26,2] = "D"
laTableStruct[26,3] = 8
laTableStruct[26,4] = 0

laTableStruct[27,1] = 'COMPLETE'
laTableStruct[27,2] = "D"
laTableStruct[27,3] = 8
laTableStruct[27,4] = 0

laTableStruct[28,1] = 'QTY1'
laTableStruct[28,2] = "N"
laTableStruct[28,3] = 7
laTableStruct[28,4] = 0

laTableStruct[29,1] = 'QTY2'
laTableStruct[29,2] = "N"
laTableStruct[29,3] = 7
laTableStruct[29,4] = 0

laTableStruct[30,1] = 'QTY3'
laTableStruct[30,2] = "N"
laTableStruct[30,3] = 7
laTableStruct[30,4] = 0

laTableStruct[31,1] = 'QTY4'
laTableStruct[31,2] = "N"
laTableStruct[31,3] = 7
laTableStruct[31,4] = 0

laTableStruct[32,1] = 'QTY5'
laTableStruct[32,2] = "N"
laTableStruct[32,3] = 7
laTableStruct[32,4] = 0

laTableStruct[33,1] = 'QTY6'
laTableStruct[33,2] = "N"
laTableStruct[33,3] = 7
laTableStruct[33,4] = 0

laTableStruct[34,1] = 'QTY7'
laTableStruct[34,2] = "N"
laTableStruct[34,3] = 7
laTableStruct[34,4] = 0

laTableStruct[35,1] = 'QTY8'
laTableStruct[35,2] = "N"
laTableStruct[35,3] = 7
laTableStruct[35,4] = 0

laTableStruct[36,1] = 'totQTY'
laTableStruct[36,2] = "N"
laTableStruct[36,3] = 7
laTableStruct[36,4] = 0

laTableStruct[36,1] = 'GROUP'
laTableStruct[36,2] = "C"
laTableStruct[36,3] = 1
laTableStruct[36,4] = 0


laTableStruct[37,1] = 'ORDER'
laTableStruct[37,2] = "C"
laTableStruct[37,3] = 6
laTableStruct[37,4] = 0

laTableStruct[38,1] = 'PIK1'
laTableStruct[38,2] = "N"
laTableStruct[38,3] = 7
laTableStruct[38,4] = 0

laTableStruct[39,1] = 'PIK2'
laTableStruct[39,2] = "N"
laTableStruct[39,3] = 7
laTableStruct[39,4] = 0

laTableStruct[40,1] = 'PIK3'
laTableStruct[40,2] = "N"
laTableStruct[40,3] = 7
laTableStruct[40,4] = 0

laTableStruct[41,1] = 'PIK4'
laTableStruct[41,2] = "N"
laTableStruct[41,3] = 7
laTableStruct[41,4] = 0

laTableStruct[42,1] = 'PIK5'
laTableStruct[42,2] = "N"
laTableStruct[42,3] = 7
laTableStruct[42,4] = 0

laTableStruct[43,1] = 'PIK6'
laTableStruct[43,2] = "N"
laTableStruct[43,3] = 7
laTableStruct[43,4] = 0

laTableStruct[44,1] = 'PIK7'
laTableStruct[44,2] = "N"
laTableStruct[44,3] = 7
laTableStruct[44,4] = 0

laTableStruct[45,1] = 'PIK8'
laTableStruct[45,2] = "N"
laTableStruct[45,3] = 7
laTableStruct[45,4] = 0

laTableStruct[46,1] = 'totPIK'
laTableStruct[46,2] = "N"
laTableStruct[46,3] = 7
laTableStruct[46,4] = 0

laTableStruct[47,1] = 'SZ1'
laTableStruct[47,2] = "C"
laTableStruct[47,3] = 5
laTableStruct[47,4] = 0

laTableStruct[48,1] = 'SZ2'
laTableStruct[48,2] = "C"
laTableStruct[48,3] = 7
laTableStruct[48,4] = 0

laTableStruct[49,1] = 'SZ3'
laTableStruct[49,2] = "C"
laTableStruct[49,3] = 5
laTableStruct[49,4] = 0

laTableStruct[50,1] = 'SZ4'
laTableStruct[50,2] = "C"
laTableStruct[50,3] = 5
laTableStruct[50,4] = 0

laTableStruct[51,1] = 'SZ5'
laTableStruct[51,2] = "C"
laTableStruct[51,3] = 5
laTableStruct[51,4] = 0

laTableStruct[52,1] = 'SZ6'
laTableStruct[52,2] = "C"
laTableStruct[52,3] = 5
laTableStruct[52,4] = 0

laTableStruct[53,1] = 'SZ7'
laTableStruct[53,2] = "C"
laTableStruct[53,3] = 5
laTableStruct[53,4] = 0

laTableStruct[54,1] = 'SZ8'
laTableStruct[54,2] = "C"
laTableStruct[54,3] = 5
laTableStruct[54,4] = 0


laTableStruct[55,1] = 'ALO1'
laTableStruct[55,2] = "N"
laTableStruct[55,3] = 7
laTableStruct[55,4] = 0

laTableStruct[56,1] = 'ALO2'
laTableStruct[56,2] = "N"
laTableStruct[56,3] = 7
laTableStruct[56,4] = 0

laTableStruct[57,1] = 'ALO3'
laTableStruct[57,2] = "N"
laTableStruct[57,3] = 7
laTableStruct[57,4] = 0

laTableStruct[58,1] = 'ALO4'
laTableStruct[58,2] = "N"
laTableStruct[58,3] = 7
laTableStruct[58,4] = 0

laTableStruct[59,1] = 'ALO5'
laTableStruct[59,2] = "N"
laTableStruct[59,3] = 7
laTableStruct[59,4] = 0

laTableStruct[60,1] = 'ALO6'
laTableStruct[60,2] = "N"
laTableStruct[60,3] = 7
laTableStruct[60,4] = 0

laTableStruct[61,1] = 'ALO7'
laTableStruct[61,2] = "N"
laTableStruct[61,3] = 7
laTableStruct[61,4] = 0

laTableStruct[62,1] = 'ALO8'
laTableStruct[62,2] = "N"
laTableStruct[62,3] = 7
laTableStruct[62,4] = 0

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

goAriaEnvironment.Cursors.createcursor(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.) 
*=gfCrtTmp(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.)

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE 
  =gfCrtTmp(lcOrdlTmp ,@laTableStruct,'Style',lcOrdlTmp ,.F.)
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

*!*************************************************************
*! Name      : lfCollectData
*! Developer : Mariam Mazhar[MMT]
*! Date      : 15/07/2007
*! Purpose   : Collecting report data
*!*************************************************************
FUNCTION lfCollectData

IF USED(lcStyTemp) AND RECCOUNT(lcStyTemp) > 0
  USE IN (lcStyTemp)
ENDIF

SELECT Style_X
DIMENSION laFileSt[1,18]
=AFIELDS(laFileSt)

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

goAriaEnvironment.Cursors.createcursor(lcStyTemp ,@laFileSt,'Style',lcStyTemp,.T.) 

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE 
=gfCrtTmp(lcStyTemp ,@laFileSt,'Style',lcStyTemp,.T.)
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

lnMajorLen = LEN(lcStylePic)

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

lnPosStyle = ASCAN(laogFXflt,'STYLE.CSTYMAJOR')
lcStyFile = ''
llSeleStyle = .F.
IF lnPosStyle <> 0 
  lnPosStyle = ASUBSCRIPT(laogFXflt,lnPosStyle,1)
  lcStyFile   =  laogFXflt[lnPosStyle,6]
  IF !EMPTY(lcStyFile) AND USED(lcStyFile)
    SELECT (lcStyFile)
    LOCATE 
    IF !EOF()
      llSeleStyle = .T.
    ENDIF
  ENDIF 
ENDIF 

lnPosLoc = ASCAN(laogFXflt,'PIKTKT.CWARECODE')
lcLocFile = ''
llLocSelect = .F.
IF lnPosLoc <> 0 
  lnPosLoc = ASUBSCRIPT(laogFXflt,lnPosLoc,1)
  lcLocFile   =  laogFXflt[lnPosLoc,6]
  IF !EMPTY(lcLocFile) AND USED(lcLocFile)
    SELECT (lcLocFile)
    LOCATE 
    IF !EOF()
      llLocSelect = .T.
    ENDIF
  ENDIF 
ENDIF 

ldStart = DTOC({})
ldEnd = DTOC({})
lnPosDate = ASCAN(laogVRflt,'PIKTKT.DATE')
IF lnPosDate  <> 0 
  lnPosDate = ASUBSCRIPT(laogVRflt,lnPosDate ,1)
  ldStart = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],1,10))
  ldEnd   = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],12,21))
ENDIF   

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE 
  lnPosStyle = ASCAN(loOGScroll.laogFXflt,'STYLE.CSTYMAJOR')
  lcStyFile = ''
  llSeleStyle = .F.
  IF lnPosStyle <> 0 
    lnPosStyle = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosStyle,1)
    lcStyFile   =  loOGScroll.laogFXflt[lnPosStyle,6]
    IF !EMPTY(lcStyFile) AND USED(lcStyFile)
      SELECT (lcStyFile)
      LOCATE 
      IF !EOF()
        llSeleStyle = .T. 
      ENDIF
    ENDIF 
  ENDIF   
  
  lnPosLoc = ASCAN(loOGScroll.laogFXflt,'PIKTKT.CWARECODE')
  lcLocFile = ''
  llLocSelect = .F.
  IF lnPosLoc <> 0 
    lnPosLoc = ASUBSCRIPT(loOGScroll.laogFXflt,lnPosLoc,1)
    lcLocFile   =  loOGScroll.laogFXflt[lnPosLoc,6]
    IF !EMPTY(lcLocFile) AND USED(lcLocFile)
      SELECT (lcLocFile)
      LOCATE 
      IF !EOF()
        llLocSelect = .T.
      ENDIF
   ENDIF 
  ENDIF   
  
  ldStart = {}
  ldEnd = {}
  lnPosDate = ASCAN(loOGScroll.laogVRflt,'PIKTKT.DATE')
  IF lnPosDate  <> 0 
    lnPosDate = ASUBSCRIPT(loOGScroll.laogVRflt,lnPosDate ,1)
    ldStart = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],1,10))
    ldEnd   = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],12,21))
  ENDIF  
  
  ldStart = DTOC({})
  ldEnd = DTOC({})
  lnPosDate = ASCAN(laogVRflt,'PIKTKT.DATE')
  IF lnPosDate  <> 0 
    lnPosDate = ASUBSCRIPT(laogVRflt,lnPosDate ,1)
    ldStart = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],1,10)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],1,10))
    ldEnd   = IIF(EMPTY(SUBSTR(laOGVRFlt[lnPosDate ,6],12,21)),DTOC(CTOD("")),SUBSTR(laOGVRFlt[lnPosDate ,6],12,21))
  ENDIF    
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]


SELECT PIKTKT

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

=goAriaEnvironment.remotetableaccess.SeekRecord('')

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ELSE 
  =gfSeek('')
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

*!*	SCAN FOR IIF(!EMPTY(lcRpStatus),Status $ lcRpStatus, .T.) AND;
*!*			 IIF(llLocSelect,SEEK(PIKTKT.CWARECODE,lcLocFile),.T.) AND ;
*!*			 IIF(!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)), BETWEEN(PIKTKT.DATE,CTOD(ldStart),CTOD(ldEnd)),.T.)	
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
IF llSeleStyle 
  SELECT ORDLINE
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.remotetableaccess.setorderto('ORDLINES')
  ELSE
    gfsetorder('ORDLINES')
  ENDIF
  SELECT(lcStyFile)
  SCAN 
    lcStyMajor = SUBSTR(&lcStyFile..cStyMajor,1,lnMajorLen)
    SELECT ORDLINE
    IF TYPE('lcXMLFileName') = 'C'
      =oAriaEnvironment.remotetableaccess.SeekRecord(lcStyMajor)
    ELSE
      =gfSeek(lcStyMajor)
  	ENDIF
    SCAN REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcStyMajor FOR CORDTYPE = 'O' AND !EMPTY(PIKTKT)
	    IF TYPE('lcXMLFileName') = 'C'
	      =oAriaEnvironment.remotetableaccess.SeekRecord(ORDLINE.PIKTKT,'PIKTKT','PIKTKT')
	    ELSE
	      =gfSeek(ORDLINE.PIKTKT,'PIKTKT','PIKTKT')
  		ENDIF
      IF !EMPTY(lcRpStatus) AND !(PIKTKT.Status $ lcRpStatus)
        LOOP
      ENDIF 
      IF llLocSelect AND !SEEK(PIKTKT.CWARECODE,lcLocFile)
        LOOP
      ENDIF 
      IF!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)) and !BETWEEN(PIKTKT.DATE,CTOD(ldStart),CTOD(ldEnd))
        LOOP
      ENDIF   
      m.cWare = PIKTKT.CWARECODE
      SELECT ORDLINE
      lcOrder  = ORDER
      lcPikTkt = Piktkt
      SELECT Ordhdr 
      IF TYPE('lcXMLFileName') = 'C'
        =goAriaEnvironment.remotetableaccess.SeekRecord('O'+lcOrder)     
      ELSE 
        =gfSeek('O'+lcOrder)  
      ENDIF      
      SELECT ORDLINE 
      STORE 0 TO m.totStk,m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,m.Stk1,;
                 m.Stk2,m.Stk3,m.Stk5,m.Stk4,m.Stk6,m.Stk7,m.Stk8
      SCATTER MEMVAR MEMO
      m.Priority = ordhdr.Priority
      m.Status = ordhdr.Status
      IF TYPE('lcXMLFileName') = 'C'
        m.STNAME = IIF(goAriaEnvironment.remotetableaccess.SeekRecord(IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,30),'')
        =goAriaEnvironment.remotetableaccess.SeekRecord(ordline.Style,'Style_X','Style')
        =goAriaEnvironment.remotetableaccess.SeekRecord('S'+Style_X.Scale,'Scale')
      ELSE
        m.STNAME = IIF(gfSEEK(IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,30),'')
        =gfSeek(ordline.Style,'Style_X','Style')
        =gfSeek('S'+Style_X.Scale,'Scale')     
      ENDIF 
      FOR lnI = 1 TO 8
        lcI =ALLTRIM(STR(lnI))
        m.Sz&lcI = scale.sz&lcI       
        m.STK&lcI= Style_X.STK&lcI
        m.totStk =  m.totStk + m.STK&lcI
      ENDFOR 
      INSERT INTO (lcOrdlTmp) FROM MEMVAR
      IF SEEK(m.style,lcStyTemp)
        REPLACE  &lcStyTemp..ALO1   WITH &lcStyTemp..ALO1   + m.PIK1 ,;
                 &lcStyTemp..ALO2   WITH &lcStyTemp..ALO2   + m.PIK2 ,;
                 &lcStyTemp..ALO3   WITH &lcStyTemp..ALO3   + m.PIK3 ,;
                 &lcStyTemp..ALO4   WITH &lcStyTemp..ALO4   + m.PIK4 ,;
                 &lcStyTemp..ALO5   WITH &lcStyTemp..ALO5   + m.PIK5 ,;
                 &lcStyTemp..ALO6   WITH &lcStyTemp..ALO6   + m.PIK6 ,;
                 &lcStyTemp..ALO7   WITH &lcStyTemp..ALO7   + m.PIK7 ,;
                 &lcStyTemp..ALO8   WITH &lcStyTemp..ALO8   + m.PIK8 ,;
                 &lcStyTemp..TOTALO WITH &lcStyTemp..TOTALO + m.TOTPIK
      
      ELSE  && add new style with new picked quantity
        INSERT INTO (lcStyTemp)                                             ;
               (STYLE,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
                VALUES (&lcOrdlTmp..Style,&lcOrdlTmp..Pik1,&lcOrdlTmp..Pik2,&lcOrdlTmp..Pik3,&lcOrdlTmp..Pik4,;
                                  &lcOrdlTmp..Pik5,&lcOrdlTmp..Pik6,&lcOrdlTmp..Pik7,&lcOrdlTmp..Pik8,;
                                  &lcOrdlTmp..TotPik)
      ENDIF  
    ENDSCAN 
  ENDSCAN 
ELSE 
  SELECT ORDLINE
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.remotetableaccess.setorderto('ORDLINE')
  ELSE
    gfsetorder('ORDLINE')
  ENDIF
  SELECT PIKTKT
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
SCAN 
    IF !EMPTY(lcRpStatus) AND !(Status $ lcRpStatus)
      LOOP
    ENDIF 
    IF llLocSelect AND !SEEK(PIKTKT.CWARECODE,lcLocFile)
      LOOP
    ENDIF 	
    IF!EMPTY(CTOD(ldStart)) AND !EMPTY(CTOD(ldEnd)) and !BETWEEN(PIKTKT.DATE,CTOD(ldStart),CTOD(ldEnd))
      LOOP
    Endif	
		 	 
   lcOrder  = ORDER
   lcPikTkt = Piktkt
   SELECT Ordhdr 
   
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
   IF TYPE('lcXMLFileName') = 'C'
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
   
   =goAriaEnvironment.remotetableaccess.SeekRecord('O'+lcOrder)		 
   
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
   ELSE 
     =gfSeek('O'+lcOrder)	
   ENDIF 
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
   
   SELECT ORDLINE
   
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
   IF TYPE('lcXMLFileName') = 'C'
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
      
   =goAriaEnvironment.remotetableaccess.SeekRecord('O'+lcOrder)
   
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
   ELSE 
     =gfSeek('O'+lcOrder)	
   ENDIF 
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]   
   
   m.cWare = PIKTKT.CWARECODE
   SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = 'O'+lcOrder FOR Piktkt=lcPikTkt
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]   
*!*       IF llSeleStyle AND !SEEK(SUBSTR(ORDLINE.STYLE,1,lnMajorLen ),lcStyFile)
*!*         LOOP 
*!*       ENDIF 
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
     STORE 0 TO m.totStk,m.Sz1,m.Sz2,m.Sz3,m.Sz4,m.Sz5,m.Sz6,m.Sz7,m.Sz8,m.Stk1,;
     		    m.Stk2,m.Stk3,m.Stk5,m.Stk4,m.Stk6,m.Stk7,m.Stk8
     SCATTER MEMVAR MEMO
     m.Priority = ordhdr.Priority
     m.Status = ordhdr.Status
     
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
     IF TYPE('lcXMLFileName') = 'C'
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
     
     m.STNAME = IIF(goAriaEnvironment.remotetableaccess.SeekRecord(IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,30),'')
     =goAriaEnvironment.remotetableaccess.SeekRecord(ordline.Style,'Style_X','Style')
     =goAriaEnvironment.remotetableaccess.SeekRecord('S'+Style_X.Scale,'Scale')
     
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
     ELSE
       m.STNAME = IIF(gfSEEK(IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE),'CUSTOMER'),SUBSTR(CUSTOMER.STNAME,1,30),'')
       =gfSeek(ordline.Style,'Style_X','Style')
       =gfSeek('S'+Style_X.Scale,'Scale')     
     ENDIF 
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
     
     FOR lnI = 1 TO 8
       lcI =ALLTRIM(STR(lnI))
       m.Sz&lcI = scale.sz&lcI       
       m.STK&lcI= Style_X.STK&lcI
       m.totStk =  m.totStk + m.STK&lcI
     ENDFOR 
     INSERT INTO (lcOrdlTmp) FROM MEMVAR
     * E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
	  IF SEEK(m.style,lcStyTemp)
	    
	    REPLACE &lcStyTemp..ALO1   WITH &lcStyTemp..ALO1   + m.PIK1 ,;
	            &lcStyTemp..ALO2   WITH &lcStyTemp..ALO2   + m.PIK2 ,;
	            &lcStyTemp..ALO3   WITH &lcStyTemp..ALO3   + m.PIK3 ,;
	            &lcStyTemp..ALO4   WITH &lcStyTemp..ALO4   + m.PIK4 ,;
	            &lcStyTemp..ALO5   WITH &lcStyTemp..ALO5   + m.PIK5 ,;
	            &lcStyTemp..ALO6   WITH &lcStyTemp..ALO6   + m.PIK6 ,;
	            &lcStyTemp..ALO7   WITH &lcStyTemp..ALO7   + m.PIK7 ,;
	            &lcStyTemp..ALO8   WITH &lcStyTemp..ALO8   + m.PIK8 ,;
	            &lcStyTemp..TOTALO WITH &lcStyTemp..TOTALO + m.TOTPIK
	  
	  ELSE  && add new style with new picked quantity
	    INSERT INTO (lcStyTemp)                                             ;
	             (STYLE,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
	      VALUES (&lcOrdlTmp..Style,&lcOrdlTmp..Pik1,&lcOrdlTmp..Pik2,&lcOrdlTmp..Pik3,&lcOrdlTmp..Pik4,;
	                                &lcOrdlTmp..Pik5,&lcOrdlTmp..Pik6,&lcOrdlTmp..Pik7,&lcOrdlTmp..Pik8,;
	                                &lcOrdlTmp..TotPik)
	  ENDIF  
     * E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
   ENDSCAN 
ENDSCAN 

* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
ENDIF 
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]
*SELECT (lcStyFile)

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF TYPE('lcXMLFileName') = 'C'
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

loProgress.Description = "Collecting Data..."
loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]


	SELECT (lcOrdlTmp)



IF RECCOUNT() > 0
*!*	*-- Scan (lcOrdlTmp) to get sum of Pik1,Pik2,...etc for each style  
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [Start]
*!*	SCAN

*!*	  


*!*	  *-- if found update the field with the new summation
*!*	  IF SEEK(style,lcStyTemp)
*!*	    
*!*	    REPLACE &lcStyTemp..ALO1   WITH &lcStyTemp..ALO1   + PIK1 ,;
*!*	            &lcStyTemp..ALO2   WITH &lcStyTemp..ALO2   + PIK2 ,;
*!*	            &lcStyTemp..ALO3   WITH &lcStyTemp..ALO3   + PIK3 ,;
*!*	            &lcStyTemp..ALO4   WITH &lcStyTemp..ALO4   + PIK4 ,;
*!*	            &lcStyTemp..ALO5   WITH &lcStyTemp..ALO5   + PIK5 ,;
*!*	            &lcStyTemp..ALO6   WITH &lcStyTemp..ALO6   + PIK6 ,;
*!*	            &lcStyTemp..ALO7   WITH &lcStyTemp..ALO7   + PIK7 ,;
*!*	            &lcStyTemp..ALO8   WITH &lcStyTemp..ALO8   + PIK8 ,;
*!*	            &lcStyTemp..TOTALO WITH &lcStyTemp..TOTALO + TOTPIK
*!*	  
*!*	  ELSE  && add new style with new picked quantity
*!*	    INSERT INTO (lcStyTemp)                                             ;
*!*	             (STYLE,Alo1,Alo2,Alo3,Alo4,Alo5,Alo6,Alo7,Alo8,TotAlo)     ;
*!*	      VALUES (&lcOrdlTmp..Style,&lcOrdlTmp..Pik1,&lcOrdlTmp..Pik2,&lcOrdlTmp..Pik3,&lcOrdlTmp..Pik4,;
*!*	                                &lcOrdlTmp..Pik5,&lcOrdlTmp..Pik6,&lcOrdlTmp..Pik7,&lcOrdlTmp..Pik8,;
*!*	                                &lcOrdlTmp..TotPik)
*!*	  ENDIF  

*!*	ENDSCAN  
IF TYPE('lcXMLFileName') <> 'C'
  WAIT WINDOW "Collecting Data..." NOWAIT 
ENDIF  
* E302688,2 MMT 07/28/2010 Changing the Allocation by Style report to be called from one program [End]

 SELECT (lcOrdlTmp)
 lnDataCnt = RECCOUNT()
 LOCATE 
 SCAN 
 
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
   IF TYPE('lcXMLFileName') = 'C'
   * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ] 
   
    lnPerCent = RECNO()/lnDataCnt
    IF MOD(RECNO(),CEILING(lnDataCnt/ 10)) = 0
      loProgress.Percent = lnPerCent * 0.9
	  loProgress.Description = "Collecting Data..."
	  loAgent.UpdateObjectProgress(lcRequestID, loProgress, ClientID)
    ENDIF
    
    * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
 
    ENDIF
    * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]     
    
   IF SEEK(Style,lcStyTemp)
   
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
     IF TYPE('lcXMLFileName') = 'C'
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]   
    
     =goAriaEnvironment.remotetableaccess.SeekRecord(Style,'Style_X','Style')
     
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
     ELSE 
       =gfSEEK(Style,'Style_X','Style')
     ENDIF 
     * E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]
     
     FOR lnI = 1 TO 8
       lcI =ALLTRIM(STR(lnI))
       REPLACE BEGN&lcI WITH STK&lcI - (Style_X.ALO&lcI - &lcStyTemp..ALO&lcI )
       REPLACE ALO&lcI WITH Style_X.ALO&lcI 
     ENDFOR 
     REPLACE TOTBGN WITH Style_X.TOTSTK - (Style_X.TOTALO - &lcStyTemp..TOTALO)
   ENDIF
 ENDSCAN 
ENDIF  
