*:****************************************************************
*: Program file  : HLSETUP.PRG
*: Program desc. : System Initial Setup.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 11/13/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfvApData , lfvApImage , lfvOk
*:               :            : lfvCancel , lpClose    . 
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
PARAMETER llLogic , lcCompID , lcCompPath , lcInstModl 

EXTERNAL ARRAY laData,laKeyField
DECLARE laKeyField [1,4]
 
IF !gfSetup()
  RETURN
ENDIF  

 
IF !WEXIST(gcBaseWind)
  lcScFields = 'nSetctdfc , nSetlbfrt , nSetinfrt , nSetfrtso , nSetfrtim,'+;
               'nSetfrtdo , nSetinsim , nSetinsdo , nSetshmim , nSetshmdo,'+;
               'cSetasdvm , cSetasdvl , cSetardir , cSetimgdr'
  SELECT HLSETUP
  SCATTER FIELDS &lcScFields TO laData
ENDIF


PUSH KEY
ON KEY

ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose

*-- Do screen code. 

DO (gcScrDir+gcAct_Appl+"\Hlsetup.SPX")
POP KEY

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfvApData
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/14/2001
*: Purpose   : Function to get the Aria apparel system data.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [13]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvApData ()
*:*************************************************************
*
FUNCTION lfvApData
PARAMETERS lcVarNam

lcNewPath = GETDIR() 
&lcVarNam = IIF(!EMPTY(lcNewPath),lcNewPath,&lcVarNam)
SHOW GET &lcVarNam 

RETURN .T.

*-- End Of lfvApData
*:*************************************************************
*: Name      : lfvApImage
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/14/2001
*: Purpose   : Function to get the Aria apparel system images.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcVarNam = ladata [14]
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvApImage ()
*:*************************************************************
*
FUNCTION lfvApImage
PARAMETERS lcVarNam

lcNewPath = GETDIR() 
&lcVarNam = IIF(!EMPTY(lcNewPath),lcNewPath,&lcVarNam)
SHOW GET &lcVarNam

RETURN .T.

*-- End Of lfvApImage.
*:*************************************************************
*: Name      : lfvOk
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/14/2001
*: Purpose   : Function to get the Aria apparel system images.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvOk ()
*:*************************************************************
*
FUNCTION lfvOk

SELECT HLSETUP
GO TOP
IF EOF()
  APPEND BLANK
ENDIF  

GATHER FROM laData FIELDS &lcScFields 

glQuitting = .T.   

*-- End Of lfvOk.
*:*************************************************************
*: Name      : lfvCancel
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/14/2001
*: Purpose   : Function to get the Aria apparel system images.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfvCancel ()
*:*************************************************************
*
FUNCTION lfvCancel

*-- Check if the user change any field and after .
*- Text Message   :- Are you sure you want to cancel this session
*- Text Message   :- and lose all changes?
*- Text Number    :- 00031
*- button message :- OK
*- button Number  :- 00006

IF llCUpdate .AND. gfModalGen('QRM00031B00006','Alert') = 2
  RETURN
ENDIF

CLEAR READ
glQuitting = .T.   

*-- End Of lfvCancel.
*:*************************************************************
*: Name      : lpClose
*: Developer : Abdou ElGendi
*: Date      : 12/04/2000
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpClose
*:*************************************************************
*
PROCEDURE lpClose

_CUROBJ = OBJNUM(pbCancel)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
