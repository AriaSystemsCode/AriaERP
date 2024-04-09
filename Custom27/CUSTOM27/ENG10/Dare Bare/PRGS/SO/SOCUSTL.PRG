*:****************************************************************
*: Program file  : SOCUSTL.PRG
*: Program desc. : Customer line Number Enquiry
*: For screen    : SOCUSTL.SPX
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI -  (ABD)
*: Date          : 01/02/2003
*: Tracking Job Number : C#200448,1
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvItemNo , lfCrteTemp , lfCollData
*:               :            : lfBrowsLn , lfvBrows   , lpDetEsc
*:               :            : lfBrowTrap, lfTraps    , lfBrowUPS
*:               :            : lfShowLins,
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200448,1.
*:****************************************************************
*:Modifications  :
*:B607118,1 ABD - 04/17/2003 Fix bug Operator/Operand type mismatch,
*:B607118,1 ABD -            This error does not happen if you do 
*:B607118,1 ABD -            not have the sales order screen open.
*:****************************************************************
*:
STORE '' TO lcItemNo , lcTempCur , lcTempLine , lcTempCur
STORE .F. To llOpenLne , llOpenHdr , llBrawse , llFromBrow
STORE SPACE(0) TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap
STORE 1 To lnUpsMarks

*B607118,1 ABD - Call gfsetup function only to fix bug Operator/Operand type mismatch,
*B607118,1 ABD - This error does not happen if you do not have the sales order screen open. [Begin]
IF !gfSetup()
  RETURN
ENDIF
*B607118,1 ABD - [End]

*-- Function to create the temp files. 
=lfCrteTemp ()

PUSH KEY
ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowLins)
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lpDetEsc
lcEscTrap = ON('KEY','ESC')
DO (gcScrDir+gcWinAppl+"\SOCUSTL.SPX")
POP KEY

ON KEY LABEL ESC     &lcHldEsc

IF llOpenLne
  =gfCloseFile('ORDLINE')
ENDIF

IF llOpenHdr
  =gfCloseFile('ORDHDR')
ENDIF
*-- End Of Code.
*:*************************************************************
*: Name      : lfvItemNo
*: Developer : Abdou Elgendy
*: Date      : 01/02/2003
*: Purpose   : Function to Valid on the Item_no.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid
*:*************************************************************
*: Example   : = lfvItemNo ()
*:*************************************************************
*:
FUNCTION lfvItemNo
Parameters llBrawse
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- craete temp File for browse.
IF !USED(lcTempCur)
  SELECT DISTINCT Item_No;
  FROM Ordline  ;
  WHERE !EMPTY(Item_No);
  INTO TABLE (lcTempCur);
  Order By 1
  INDEX On Item_No Tag (lcTempCur)
ENDIF



llBrawse = IIF(llBrawse,llBrawse,IIF('?' $ lcItemNo,.T.,.F.))

IF MDOWN() .OR. (EMPTY(ALLTRIM(lcItemNo)) .AND. !llBrawse)
  SELECT (lcTempLine)
  DELETE ALL
  SELECT(lnAlias)
  RETURN
ENDIF

*-- seek for the item no.

IF !llBrawse .AND. !SEEK(ALLTRIM(lcItemNo),lcTempCur)
  IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'There is no data match your line no.') = 2
    SELECT (lcTempLine)
    DELETE ALL
    lcItemNo  =''
    _CUROBJ = OBJNUM(lcItemNo)
    SELECT(lnAlias)
    RETURN
  ELSE
    llBrawse = .T.
  ENDIF

ENDIF

IF llBrawse
  lcPBrowTit = "Customer line number enquiry"         && Browse title.
  lcOldBrFld = lcBrFields
  DIMENSION latemp[1]                && array holr the return value from ariabrow
  laTemp = ''
  SELECT (lcTempCur)
  lcBrFields = [Item_No:H="Item No"]
  llReturn = ARIABROW('',lcPBrowTitl,gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Item_No','laTemp')
  lcBrFields = lcOldBrFld
  
  IF EMPTY(laTemp)
    lcItemNo  =''
    _CUROBJ = OBJNUM(lcItemNo)
    SELECT (lcTempLine)
    DELETE ALL
    = lfBrowsLn()
  ELSE
    lcItemNo = ALLTRIM(laTemp[1])
  ENDIF
ENDIF

IF !EMPTY(lcItemNo)
  = lfCollData()
ENDIF

SHOW GET lcItemNo

SELECT(lnAlias)
RETURN

*-- End OF lfvItemNo
*:*************************************************************
*: Name      : lfCrteTemp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/02/2003
*: Purpose   : Function to Craete the temp files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfCrteTemp ()
*:*************************************************************
*:
FUNCTION lfCrteTemp
PRIVATE lnAlias

lnAlias = SELECT()

lcTempCur = gfTempName()
lcTempLine = gfTempName()

llOpenLne =gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
llOpenHdr =gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
=AFIELDS(laFileStru)

CREATE TABLE (lcTempLine) (Item_No C(19),STYLE C(19),Order C(6),CustPo C(15) )
INDEX ON Item_No + Order + Style Tag (lcTempLine)

SELECT(lnAlias)
*-- End OF lfCrteTemp
*:*************************************************************
*: Name      : lfCollData
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/02/2003
*: Purpose   : Function Collete the correct data.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfCrteTemp ()
*:*************************************************************
*:
FUNCTION lfCollData
Private llFondData



SELECT (lcTempLine)
DELETE ALL

SELECT ORDLINE
lnLentemNo = LEN(ALLTRIM(lcItemNo))


SCAN  For LEFT(Item_No,lnLentemNo) = ALLTRIM(lcItemNo)
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempLine) FROM MEMVAR
ENDSCAN

SELECT (lcTempLine)
LOCATE
IF EOF()
  *-- message : different files.
  =gfModalGen("QRM00000B00000",.F.,.F.,.F.,'There is no data match your line no.')
  lcItemNo = ''
  _CUROBJ = OBJNUM(lcItemNo)
ENDIF


= lfBrowsLn()
RETURN

*-- End of lfCollData
*:*************************************************************
*: Name      : lfBrowsLn
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/02/2003
*: Purpose   : Browse Invoice Cartons Details.
*:*************************************************************
*: Calls     : lfShowUPS()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfBrowsLn()
*:*************************************************************
FUNCTION lfBrowsLn



SELECT (lcTempLine)
BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnUpsMarks,'>',' '):H=' ' :R :1 :W=.F.,;
       Item_No  :H='Line number'   :P= 'XXXXXXXXXXXXXXXXXXXX' :R  ,;
       STYLE    :H='Style  '       :P= 'XXXXXXXXXXXXXXXXXXXX' :R  ,;
       Order    :H='Order  '       :P= 'XXXXXX' :R                ,;       
       CustPo :H='Cust Po '        :P= 'XXXXXXXXXXXXXXXX' :R      ;
       WINDOW SOCUSTL1  ;
       IN WINDOW SOCUSTL;
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
	   VALID lfvBrows()  ;
       WHEN lfShowLins() ;
       TITLE lcBrowLins

*-- end of lfBrowsLn
*:*************************************************************
*: Name      : lfvBrows
*: Developer : Abdou Elgendy
*: Date      : 01/02/2003
*: Purpose   : VALID Function for the Style Color Size browse
*:*************************************************************
*: Called from : None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfvBrows

llFromBrow = .T.

*-- End OF lfvBrows
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou Elgendy
*: Date      : 01/02/2003
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

ACTIVATE WINDOW SOCUSTL2
_CUROBJ = OBJNUM(PbClose)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lfBrowTrap
*: Developer : Abdou Elgendy
*: Date      : 01/02/2003
*: Purpose   : Function to trap the keys for the Browse
*:*************************************************************
*: Called from :  None.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfBrowTrap

*-- IF The window on top is the Browse
IF WONTOP(lcBrowLins)
  llFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL ALT+A
  lcEscTrap = ON('KEY' , 'ESC')                && Varible to hold the ESC key traping
  lcCtrETrap = ON('KEY' , 'CTRL+END')          && Varible to hold the CTRL+END key traping
  lcCtrHTrap = ON('KEY' , 'CTRL+HOME')         && Varible to hold the CTRL+HOME key traping
  lcCtrNTrap = ON('KEY' , 'CTRL+ENTER')        && Varible to hold the CTRL+ENTER key traping
  ON KEY LABEL ESC DO lfTraps WITH 3
  ON KEY LABEL CTRL+END DO lfTraps WITH 4
  ON KEY LABEL CTRL+HOME DO lfTraps WITH 5
  ON KEY LABEL CTRL+ENTER DO lfTraps WITH 6
ENDIF    && End of IF

RETURN .F.

*-- End Of lfBrowTrap
*:*************************************************************
*: Name      : lfTraps
*: Developer : Abdou Elgendy
*: Date      : 01/02/2003
*: Purpose   : Function to be called from some traped keys
*:*************************************************************
*: Called from : lfBrowTrap()
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : 1 , 2
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfTraps

PARAMETERS lnTrap

DO CASE 
  *-- Case TAB
  CASE lnTrap = 1
    IF WONTOP()= lcBrowLins
      glFromBrow = .T.
      ACTI WINDOW SOCUSTL2 TOP
    ELSE
      _CUROBJ = _CUROBJ +1
    ENDIF

  *-- Case BACKTAB
  CASE lnTrap = 2
    ACTI WINDOW SOCUSTL0 TOP
    
  *-- Case ESC
  CASE lnTrap = 3
   DO  lpDetEsc
    
  *-- Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *-- Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
  
  *-- Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN
  
ENDCASE

*-- End OF lfTraps.
*:*************************************************************
*: Name      : lfBrowUPS
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/02/2003
*: Purpose   : Deactivate function for Cartons details screen
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfBrowUPS()
*:*************************************************************
FUNCTION lfBrwUnTrp


*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcBrowLins) .AND. llFromBrow
  llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrowLins)
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL CTRL+END &lcCtrETrap
  ON KEY LABEL CTRL+HOME &lcCtrHTrap
  ON KEY LABEL CTRL+ENTER &lcCtrNTrap
ENDIF

RETURN .F.

*-- End OF lfBrowUPS.
*:*************************************************************
*: Name      : lfShowLins
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 01/02/2003
*: Purpose   : Show function for Cartons details screen
*:*************************************************************
*: Calls     : lfRefresh()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None 
*:*************************************************************
*: Example   : =lfShowLins()
*:*************************************************************
FUNCTION lfShowLins

SELECT (lcTempLine)
lnUpsMarks = RECNO()
SHOW WINDOW (lcBrowLins) REFRESH SAME
SHOW GETS WINDOW 'SOCUSTL0' ONLY
=lfRefresh('SOCUSTL0')

RETURN .F.

*-- End OF lfShowLins
*:*************************************************************

