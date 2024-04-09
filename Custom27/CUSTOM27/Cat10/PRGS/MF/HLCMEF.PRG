*:****************************************************************
*: Program file  : HLCMEF.PRG
*: Program desc. : Multiple Entry Form.
*: System        : Aria Apparel System (A27).
*: Module        : H.L.C. System. (HL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200246,1
*: Date          : 01/01/2002
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfwLine   , lfvOk   , lfvCancel
*:               :            : lfvStatus , lfvCutA , lfvSewA
*:               :            : lfvCutB   , lfvSewB , lfGetCode
*:               :            : lfGetCode .
*:               -------------------------------------------------
*:               : PROCEDURE  : lfShow    , lpClose .
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#200246,1.
*:****************************************************************
*:Modifications  :
*:****************************************************************
*:
EXTERNAL ARRAY laData,laScrMode

lcVendor  = ALLTRIM(laVendCod((ASCAN(laVendcod,laData[1])/2),1))
lccinvno  = laData[2]  && Variable to hold the Invoice no 
lcInvType = IIF(laData[8]='R','Regulars',IIF(laData[8]='S','Seconds','Balances'))

SELECT (lcTempMef)
lsLine = 1
Ddexporta = &lcTempMef..dexporta
DdExportB = &lcTempMef..dExportB

PUSH KEY
ON KEY LABEL ESC 
ON KEY LABEL ESC DO lpClose
DO (gcScrDir+gcAct_Appl+"\Hlcmef.SPX")
POP KEY

RETURN
*-- End of program code.
*:*************************************************************
*: Name      : lfwLine
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/19/2001
*: Purpose   : To high light the new line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfwLine ()
*:*************************************************************
*:
FUNCTION lfwLine

** To high light the new line **

lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat

SHOW GETS ONLY

*-- End Of lfwLine.
*:*************************************************************
*: Name      : lfShow
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 12/19/2001
*: Purpose   : Show function
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lfShow
*:*************************************************************
*:
PROCEDURE lfShow

EXTERNAL ARRAY laScrMode
DO CASE
  CASE laScrMode[2]                      && View Mode
    SHOW GETS DISABLE ONLY
    SHOW GET pbOk     ENABLE
  OTHERWISE                                 && Add or Edit Mode
    SHOW GET lcVendor 
    SHOW GET lccinvno
    SHOW GET pbCancel ENABLE
    SHOW GET lCutA ENABLE
	SHOW GET lSewA ENABLE
	SHOW GET lCutB ENABLE
    SHOW GET lSewB ENABLE
	SHOW GET ddExportA ENABLE
	SHOW GET ddExportB ENABLE
ENDCASE

SHOW GET lsLine   ENABLE

*-- End OF lfShow.
*:*************************************************************
*: Name      : lfvOk.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Ok Button.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvOk ()
*:*************************************************************
*:
FUNCTION lfvOk

llUpdMef = .T.

*-- End OF lfvOk. 
*:*************************************************************
*: Name      : lfvCancel.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Canncel Button.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCancel ()
*:*************************************************************
*:
FUNCTION lfvCancel

llUpdMef = .F.

*-- End Of lfvCancel.
*:*************************************************************
*: Name      : lfvStatus.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid status for the current line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvStatus ()
*:*************************************************************
*:
FUNCTION lfvStatus

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat

REPLACE dexporta WITH Ddexporta ,;
        dExportB WITH DdExportB
SHOW GET lsLine

*-- End OF lfvStatus.
*:*************************************************************
*: Name      : lfvCutA.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid the cut at status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCutA ()
*:*************************************************************
*:
FUNCTION lfvCutA

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

REPLACE lCutB WITH .F.
lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat
SHOW GET lCutB 

SHOW GET lsLine

*-- End Of lfvCutA.
*:*************************************************************
*: Name      : lfvSewA.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid the Sew at status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvSewA ()
*:*************************************************************
*:
FUNCTION lfvSewA

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

REPLACE lSewB WITH .F.

lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat
SHOW GET lSewB 

SHOW GET lsLine

*-- End OF lfvSewA
*:*************************************************************
*: Name      : lfvCutB.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid the cut at status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvCutB ()
*:*************************************************************
*:
FUNCTION lfvCutB

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

REPLACE lCutA WITH .F.

REPLACE dExportB WITH laData[3]
lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat

SHOW GET lCutA 

SHOW GET dExportB

SHOW GET lsLine

*-- End OF lfvCutB.
*:*************************************************************
*: Name      : lfvSewB.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid the Sew at status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfvSewB ()
*:*************************************************************
*:
FUNCTION lfvSewB

REPLACE cStatus WITH SUBSTR('MMA',AT(cStatus,'SMA'),1)  && Modify

REPLACE lSewA WITH .F.

REPLACE dExportB WITH laData[3]
lsline = SUBSTR(lfGetCode(ctsuchwcd,'CTSUCHWCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsuconcd,'CTSUCONCD'),1,16)+'  '+SUBSTR(lfGetCode(ctsugarcd,'CTSUGARCD'),1,16)+'  '+ctsuqtcat
SHOW GET lSewA 

SHOW GET dExportB

SHOW GET lsLine

*-- End OF lfvSewB.
*:*************************************************************
*: Name      : lfGetCode.
*: Developer : Abdou Elgendy. [ABD]
*: Date      : 01/03/2002
*: Purpose   : Valid the Sew at status.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None.
*:*************************************************************
*: Example   : = lfGetCode ()
*:*************************************************************
*:
FUNCTION lfGetCode
PARAMETERS lcCode,lcField

lcDisc=''
lcDisc = gfCodDes(lcCode,lcField)

RETURN lcDisc 

*-- End OF lfGetCode.
*:*************************************************************
*: Name      : lpClose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/19/2001
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

IF laScrMode [3] .OR. laScrMode [4]
  _CUROBJ = OBJNUM(pbCancel)
ELSE
  _CUROBJ = OBJNUM(pbOk)
ENDIF  
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpClose
*:*************************************************************
