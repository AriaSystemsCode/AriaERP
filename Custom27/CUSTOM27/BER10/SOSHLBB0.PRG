*:**************************************************************************
*: Program file  : SOSHLBB0
*: Program desc. : Customized Shipping Labels Program which calls Label Screen
*: System        : Aria Advantage Series VER. 2.7
*: Module        : SO,AR,AL
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Date          : 05/24/2000
*: Reference     : C101790
*:**************************************************************************
*C101790,1 SSE 05/24/2000 Custom Shipping labels for Berkley Shirts.
*:**************************************************************************
*

DO (gcScrDir + 'SOSHLBB0.SPX')

*!**************************************************************************
*! Name      : lfBrows
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : To browse the ranges of labels notes.
*!**************************************************************************
*
FUNCTION lfBrows
SELECT (lclblTbl)
BROWSE FOR Invoice = InvHdr.Invoice FIELDS ;
       nFrom         :H = 'From'                :R :3,;
       nTo           :H = 'To'                  :R :2,;       
       Style         :H = 'Style'               :R :16,;
       Note1         :H = 'Note1'               :R :18,;
       Note2         :H = 'Note2 '              :R :18,;
       TotQty        :H = 'Quantity'            :R :8,;
       Vendor        :H = 'Vendor'              :R :8,;
       Sku           :H = 'Sku #'               :R :10;
       WINDOW     SOSHLBB1;
       IN WINDOW  SOSHLBB0 ;
       WHEN lfwBrow()      ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       NOEDIT            ;
       LOCK 0            ;
       TITLE lcBrowTitl
*-- End of lfBrows.

*!**************************************************************************
*! Name      : lfReadAct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : READ Activate function of SOSHLBB0.
*!**************************************************************************
*
FUNCTION lfReadAct

ON KEY LABEL CTRL+Q    
ON KEY LABEL CTRL+W    
ON KEY LABEL CTRL+HOME 
ON KEY LABEL CTRL+END  
ON KEY LABEL ESC 
ON KEY LABEL TAB 
ON KEY LABEL BACKTAB 
*-- End of lfReadAct.

*!**************************************************************************
*! Name      : lfvDeact
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : READ Deactivate function of screen SOSHLBB0.
*!**************************************************************************
*
FUNCTION lfvDeact
IF WONTOP() = lcBrowTitl
  ON KEY LABEL CTRL+Q    lnDummy = 1
  ON KEY LABEL CTRL+W    lnDummy = 1
  ON KEY LABEL CTRL+HOME GO TOP
  ON KEY LABEL CTRL+END  GO BOTTOM
  ON KEY LABEL ESC DO lpTrapKey WITH 'SOSHLBB2', 'PbClose', .T.
  ON KEY LABEL TAB DO lpTrapKey WITH 'SOSHLBB2', 'lnTo'
  ON KEY LABEL BACKTAB DO lpTrapKey WITH 'SOSHLBB2', 'PbClose'
ENDIF
RETURN .F.
*-- End of lfvDeact.

*!**************************************************************************
*! Name      : lfwBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : To process the browse WHEN.
*!**************************************************************************
*
FUNCTION lfwBrow
PRIVATE lcAlias 

lcAlias = ALIAS()
SELECT (lclblTbl)
lnFrom   = nFrom
lnTo     = nTo
lnQty    = TotQty 
lcStyle  = Style
lcVendor = Vendor
lcSku    = Sku
lcNote1  = Note1
lcNote2  = Note2
SHOW GETS WINDOW SOSHLBB2 
=lfRefresh('SOSHLBB2') 
SELECT (lcAlias)
*-- End of lfwBrow.

*!**************************************************************************
*! Name      : lfvTo
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : To validate the TO fields.
*!**************************************************************************
*
FUNCTION lfvTo
lnAlias = SELECT()
SELECT (lclblTbl)

IF lnFrom > lnTo
  =gfModalGen("TRM000000B00000","DIALOG",'','','Labels range error. Please retry.!')    
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
IF lnTo = 0
  =gfModalGen("TRM000000B00000","DIALOG",'','','The numbers of labels should be greater than one.')    
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

IF lnTo > lnLabels
  =gfModalGen("TRM000000B00000","DIALOG",'','','The maxiumn numbers of labels should not exceed '+ALLTRIM(STR(lnLabels))+'.')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

REPLACE nTo     WITH lnTo ,;
        Labels WITH lnLabels
 
lnRecNo = RECNO()
SKIP

*--See whether this is the last record or lnTo = last label.
IF EOF()
  *--Get the current record.
  SKIP -1
   IF lnTo < lnLabels
    *--Insert a new record
    INSERT INTO (lclblTbl) (Invoice,Labels,nFrom,nTo) VALUES ;
                           (InvHdr.Invoice,lnLabels,nTo+1,lnLabels)
    *--Get the current record.              
    SKIP -1
  ENDIF  
ELSE
  *--We modify
  SCAN REST WHILE Invoice = InvHdr.Invoice
   IF nTO <= lnTo OR nTo >= lnLabels
      *--This record is included in the above range, delete.
      DELETE
      SKIP
      *--See whether this is the last record or lnTo = last label.
      IF EOF() AND lnTo < lnLabels
        *--Get the current record.
        SKIP -1
        *--Insert a new record
        INSERT INTO (lclblTbl) (Invoice,Labels,nFrom,nTo) VALUES ;
                               (InvHdr.Invoice,lnLabels,nTo+1,lnLabels)
        *--Get the current record.              
        GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,TOP)
        EXIT
      ENDIF
      SKIP -1
    ELSE
      *--Leave it and adjust the nFrom field.
      REPLACE nFrom WITH lnTo+1
      EXIT
    ENDIF  
  ENDSCAN
ENDIF
GO IIF(BETWEEN(lnRecNo,1,RECCOUNT()),lnRecNo,TOP)
SHOW WINDOW (lcBrowTitl) REFRESH SAME
SELECT (lnAlias)
*-- End of lfvTo.

*!**************************************************************************
*! Name      : lfvFields
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : To validate get fields.
*!**************************************************************************
*
FUNCTION lfvFields
PARAMETERS lcFldName,lcFldVal
PRIVATE lcAlias 

lcAlias = ALIAS()
SELECT (lclblTbl)
REPLACE &lcFldName WITH (lcFldVal)
SHOW WINDOW (lcBrowTitl) REFRESH SAME
IF VARREAD() = 'LCSKU' 
  IF nTo < lnLabels
    SKIP
    lnFrom = nFrom
    lnTo   = nTo
    STORE '' TO lcStyle,lcNote1,lcNote2,lcVendor,lcSku
    STORE 0  TO lnQty
  ENDIF  
  SHOW GETS WINDOW SOSHLBB2 
  =lfRefresh('SOSHLBB2') 
  _CUROBJ = OBJNUM(pbclose)
ENDIF
SELECT (lcAlias)
*-- End of lfvFields.

*!**************************************************************************
*! Name      : lpTrapKey
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/24/2000
*! Purpose   : Trap of keys
*!**************************************************************************
*
PROCEDURE lpTrapKey
PARAMETERS lcWindName, lcObjName, llToCheck

ACTIVATE WINDOW (lcWindName)
_CUROBJ = OBJNUM(&lcObjName)
IF llToCheck
  KEYBOARD CHR(13) CLEAR
ENDIF
*-- End of lpTrapKey.
