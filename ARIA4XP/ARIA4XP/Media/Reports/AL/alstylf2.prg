*:***************************************************************************
*: Program file  : ALSTYAL
*: Program desc. : ALLOCATION REPORT FORM #2 
*: Date          : 07/15/2007
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar[MMT](N037686)
**********************************************************************************
*! E302857,1 HES 02/10/2011 Avoid 'X:\Aria4xp\SRVRPTS' Fixed Path [T20110206.0017]
*:********************************************************************************

* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [Start]
IF oAriaApplication.MULTIINST
  *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- BEGIN  
*!*	  SET PROCEDURE TO X:\ARIA4XP\SRVRPTS\AL\ALSTYF2.FXP ADDITIVE
*!*	  DO X:\ARIA4XP\SRVRPTS\AL\ALSTYF2.FXP WITH .F.,.F.   
  SET PROCEDURE TO oAriaApplication.CLIENTSRVREPORTHOME+"AL\ALSTYF2.FXP" ADDITIVE
  DO oAriaApplication.CLIENTSRVREPORTHOME+"AL\ALSTYF2.FXP" WITH .F.,.F.     
  *! E302857,1 HES 02/10/2011 Avoid Fixed Path ------- END  
ELSE
  lcSrvRpt = STRTRAN(UPPER(oAriaApplication.ReportHome),'REPORTS','SRVRPTS')
  DO lcSrvRpt+"AL\ALSTYF2.FXP" WITH .F.,.F.
ENDIF 
* E302688,1 HES 04/19/2010 Changing the Allocation by Style report to be called from one program [End  ]

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