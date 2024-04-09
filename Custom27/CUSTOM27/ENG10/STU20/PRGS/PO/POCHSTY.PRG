*:***************************************************************************
*: Program file  : POCHSTY.PRG
*: Program desc. : CHANGE STYLE NUMBER FOR STUNCROFT LTD.
*: Date          : 07/03/2001
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order - (PO)
*: Developer     : Ahmed Maher (AMH)
*: Tracking Job Number: C200207
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POCHSTY
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*-- Initializing the necessary variables.
*C200207,4 AMH Add variables to get the style information [Start]
*STORE 0         TO lnstylewid,lnMjrWid,lncolorwid
*STORE SPACE(0)  TO lcStyHdr,lcStyPict,lcCurStyle,lcNMjrPt
STORE 0         TO lnstylewid,lnMjrWid,lnNonMjLen,lnMjorCnt,lnColorStr
STORE 0         TO lnColorLen,lnSizePos,lnSizeLen
STORE SPACE(0)  TO lcStyHdr,lcStyPict,lcCurStyle,lcNMjrPt,lcMajorPic
STORE .F.       TO llExtSizSc
PRIVATE laStySeg
DECLARE laStySeg[1,1]
*C200207,4 AMH [End]
STORE .F.       TO llBrowse

IF !gfSetup()
  RETURN
ENDIF  

*-- Get the necessary variables.
lcStyHdr   = gfItemMask('HI')
*C200207,4 AMH Calling function lfStylInfo to get the style information [Start]
*lcStyPict  = gfItemMask('PI')
*lcNMjrPt   = gfItemMask('PN')
llExtSizSc = gfGetMemVar('M_USEEXSSC',gcAct_Comp)
=lfStylInfo()
lnstylewid = LEN(lcStyPict)
*lnMjrWid   = LEN(gfItemMask('PM'))
*lncolorwid = LEN(lcNMjrPt)
*C200207,4 AMH [End]

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lfvCancel

DO (gcScrDir+gcWinAppl+"\POCHSTY.SPX")

ON KEY LABEL ESC     &lcHldEsc

*!*************************************************************
*! Name      : lfvPo
*: Date      : 07/03/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : validate Po #.
*!*************************************************************
*! Example            : =lfvPo()
*!*************************************************************
*
FUNCTION lfvPo

IF llBrowse .OR. (!EMPTY(lcPo) .AND. !SEEK('P'+lcPo,'POSHDR'))
  lcPoBrow=lcPo
  SELECT POSHDR
  DO POSBrow WITH lcPoBrow,'','P'
  lcPo=lcPoBrow
  llBrowse = .F.
ENDIF
IF EMPTY(lcPo)
  lcCurStyle = SPACE(19)
ELSE
  IF POSHDR.STATUS <> 'H'
    =gfModalGen("INM000000B00000","DIALOG",'','',"Select Hold P/O's only.")
    lcPo = ''
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  =SEEK('P'+lcPo,'POSLN')
  lcCurStyle = SUBSTR(POSLN.STYLE,1,lnMjrWid)
  SHOW GET lcPo       DISABLE
  SHOW GET ibPo       DISABLE
  SHOW GET lcNewStyle ENABLE
  SHOW GET ibStyle    ENABLE
  SHOW GET pbCancel,1 PROMPT gcBmpHome + "CAN.BMP"
ENDIF
SHOW GET lcCurStyle
*-- end of lfvPo.

*!*************************************************************
*! Name      : lfvStyle
*: Date      : 07/03/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : validate New Style File.
*!*************************************************************
*! Example            : =lfvStyle()
*!*************************************************************
*
FUNCTION lfvStyle

PRIVATE lcAlias
lcAlias = ''
SELECT STYLE
SET ORDER TO TAG CSTYLE
IF llBrowse .OR. (!EMPTY(lcNewStyle) .AND. !SEEK(PADR(lcNewStyle,19),'STYLE'))
  llbrowse = .F.
  lcNewStyle = gfStyBrw('M',lcNewStyle,'',.F.)
ENDIF
IF !EMPTY(lcNewStyle)
  IF ALLTRIM(lcCurStyle) == ALLTRIM(lcNewStyle)
    =gfModalGen("INM000000B00000","DIALOG",'','','The new style must be different from the P/O style.')
    lcNewStyle = ''
    SET ORDER TO TAG Style
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF STYLE.MAKE
    =gfModalGen("INM000000B00000","DIALOG",'','','Select only a Purchased style.')
    lcNewStyle = ''
    SET ORDER TO TAG Style
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF !SEEK(STYLE.CSTYMAJOR,'BOM')
    =gfModalGen("INM000000B00000","DIALOG",'','',"Style : "+ALLTRIM(lcNewStyle)+" does not have a cost sheet, Cannot proceed.")
    lcNewStyle = ''
    SET ORDER TO TAG Style
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  IF SEEK(PADR(lcCurStyle,19))
    =gfOpenFile(gcDataDir+'STYLE', gcDataDir+'STYLE', 'SH' , @lcAlias , .T.)
    SELECT STYLE
    SET ORDER TO TAG Style
    SCAN REST WHILE STYLE = SUBSTR(lcCurStyle,1,lnMjrWid)
      *C200207,4 AMH Seek for color only [Start]
      *IF SEEK(STUFF(lcNewStyle,lnMjrWid+1,lncolorwid+1,SUBSTR(STYLE,lnMjrWid+1,lncolorwid+1)),lcAlias)
      lcStyCol = STUFF(lcNewStyle,lnColorStr-1,lnColorLen+1,SUBSTR(STYLE,lnColorStr-1,lnColorLen+1))
      IF SEEK(SUBSTR(lcStyCol,1,lnColorStr+lnColorLen-1),lcAlias)
        *IF STYLE.SCALE <> EVALUATE(lcAlias+'.SCALE')
        lcStyColSz = STUFF(lcStyCol,lnSizePos-1,lnSizeLen+1,SUBSTR(STYLE,lnSizePos-1,lnSizeLen+1))
        IF !SEEK(lcStyColSz,lcAlias)
      *C200207,4 AMH [End]
          *=gfModalGen("INM000000B00000","DIALOG",'','','One or more scales in the style : '+;
                      ALLTRIM(lcCurStyle)+' does not exsit in the style : '+ALLTRIM(lcNewStyle)+', Cannot proceed.')
          =gfModalGen("INM000000B00000","DIALOG",'','','Scale/Colour Incompatible, Cannot proceed.')

          lcNewStyle = ''
          _CUROBJ = _CUROBJ
          EXIT
        ENDIF
      ELSE
        *=gfModalGen("INM000000B00000","DIALOG",'','','One or more colors in the style : '+;
                    ALLTRIM(lcCurStyle)+' does not exsit in the style : '+ALLTRIM(lcNewStyle)+', Cannot proceed.')
        =gfModalGen("INM000000B00000","DIALOG",'','','Scale/Colour Incompatible, Cannot proceed.')

        lcNewStyle = ''
        _CUROBJ = _CUROBJ
        EXIT
      ENDIF
    ENDSCAN
    =gfCloseFile(lcAlias)
  ENDIF
ENDIF
SET ORDER TO TAG Style
IF !EMPTY(lcPo) .AND. !EMPTY(lcNewStyle)
  SHOW GET pbProceed ENABLE
ELSE
  SHOW GET pbProceed DISABLE
ENDIF
*-- end of lfvStyle.

*!*************************************************************
*! Name      : lfvProceed
*: Date      : 07/03/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Proceed button.
*!*************************************************************
*! Example            : =lfvProceed()
*!*************************************************************
*
FUNCTION lfvProceed

PRIVATE lcStuffSty
SELECT POSLN
SEEK 'P'+lcPo
DO WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'P'+lcPo
  lcStuffSty = STUFF(STYLE,1,lnMjrWid,SUBSTR(lcNewStyle,1,lnMjrWid))
  =lfRebalanc('STYLE',STYLE,'WIP','TOTWIP','POSLN',-1)
  =lfRebalanc('STYLE',STYLE,'NWO','NTOTWO','POSLN',-1)
  =lfRebalanc('STYLE',lcStuffSty,'WIP','TOTWIP','POSLN',1)
  =lfRebalanc('STYLE',lcStuffSty,'NWO','NTOTWO','POSLN',1)
  =lfRebalanc('STYDYE',STYLE+CWARECODE+DYELOT,'WIP','TOTWIP','POSLN',-1)
  =lfRebalanc('STYDYE',STYLE+CWARECODE+DYELOT,'NWO','NTOTWO','POSLN',-1)
  =lfRebalanc('STYDYE',lcStuffSty+CWARECODE+DYELOT,'WIP','TOTWIP','POSLN',1)
  =lfRebalanc('STYDYE',lcStuffSty+CWARECODE+DYELOT,'NWO','NTOTWO','POSLN',1)
  REPLACE STYLE WITH lcStuffSty
  SEEK 'P'+lcPo+SUBSTR(lcCurStyle,1,lnMjrWid)
ENDDO

SELECT CUTPICK
SEEK '2'+lcPo
DO WHILE TranCd+cTktNo+Style = '2'+lcPo
  REPLACE STYLE WITH STUFF(STYLE,1,lnMjrWid,SUBSTR(lcNewStyle,1,lnMjrWid))
  IF SEEK('O'+ORDER+CORDLINE,'ORDLINE')
    =lfRebalanc('STYLE',ORDLINE.STYLE,'ORD','TOTORD','ORDLINE',-1)
    =lfRebalanc('STYLE',STYLE,'ORD','TOTORD','ORDLINE',1)
    =lfRebalanc('STYDYE',ORDLINE.STYLE+ORDLINE.CWARECODE+ORDLINE.DYELOT,'ORD','TOTORD','ORDLINE',-1)
    =lfRebalanc('STYDYE',STYLE+ORDLINE.CWARECODE+ORDLINE.DYELOT,'ORD','TOTORD','ORDLINE',1)
    REPLACE ORDLINE.STYLE WITH STYLE
    IF SEEK('O'+ORDER+CORDLINE,'ORDCANLN')
      REPLACE ORDCANLN.STYLE WITH STYLE
    ENDIF
  ENDIF
  SEEK '2'+lcPo+SUBSTR(lcCurStyle,1,lnMjrWid)
ENDDO

=gfModalGen("INM000000B00000","DIALOG",'','','Styles have been successfully changed.')
=lfvCancel()
*-- end of lfvProceed.

*!*************************************************************
*! Name      : lfvCancel
*: Date      : 07/03/2001
*: Developer : Ahmed Maher (AMH)
*! Purpose   : Valid function of the Cancel button.
*!*************************************************************
*! Example            : =lfvCancel()
*!*************************************************************
*
FUNCTION lfvCancel

IF EMPTY(lcPo)
  DO GFCPCLOSE
ELSE
  STORE '' TO lcPo,lcCurStyle,lcNewStyle
  SHOW GET lcPo       ENABLE
  SHOW GET ibPo       ENABLE
  SHOW GET lcCurStyle
  SHOW GET lcNewStyle DISABLE
  SHOW GET ibStyle    DISABLE
  SHOW GET pbCancel,1 PROMPT gcBmpHome + "CLOSE1.BMP"
  SHOW GET pbProceed DISABLE
  _CUROBJ = OBJNUM(lcPO)
ENDIF
*-- end of lfvCancel.

*!*************************************************************
*! Name       : lfRebalanc
*: Date       : 07/05/2001
*: Developer  : Ahmed Maher (AMH)
*! Purpose    : Rebalance style/stydye files.
*! Parameters : lcFile   ---> file to rebalance
*!              lcKey    ---> the key of the style for rebalance it
*!              lcField  ---> field to rebalance
*!              lcTotFld ---> total field to rebalance
*!              lcFrom   ---> alias of qty fields
*!              lnSign   ---> 1 for (+) , -1 for (-)
*!*************************************************************
*! Example    : =lfRebalanc()
*!*************************************************************
*
FUNCTION lfRebalanc
PARAMETERS lcFile, lcKey, lcField, lcTotFld, lcFrom, lnSign

PRIVATE lnI, lcI, lnAlias
DECLARE laQty[9]
IF SEEK(lcKey,lcFile)
  lnAlias = SELECT(0)
  SELECT (lcFile)
  lnI = 0
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    REPLACE &lcField&lcI. WITH &lcField&lcI. + (lnSign * &lcFrom..QTY&lcI.)
  ENDFOR
  REPLACE &lcTotFld. WITH &lcTotFld. + (lnSign * &lcFrom..TOTQTY)
  SELECT (lnAlias)
ENDIF
*-- end of lfRebalanc.

*!*************************************************************
*! Name      : lfStylInfo
*! Developer : Ahmed Maher (AMH)
*! Date      : 07/26/2001
*! Purpose   : Prepare the needed style information.
*!*************************************************************
*! Calls     : gfItemMask
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : lfStylInfo()
*!*************************************************************
*!C200207,4
*!*************************************************************
*
FUNCTION lfStylInfo

*-- Get the non major picture.
lcNMjrPt   = gfItemMask("PN")

*-- Picture of the style major.
lcMajorPic = gfItemMask("PM")

*-- Lenght of the the style major.
lnMjrWid = LEN(lcMajorPic)

*-- Get the non-major lenght.
lnNonMjLen = LEN(lcNMjrPt)

*-- Get the whole style picture.
lcStyPict  = gfItemMask("PI")

*-- Count of the major part.
lnMjorCnt  = gfItemMask("SM")
  
*-- Fill an array with the segments strucure, & loop in it to 
*-- know if there a color segment in the style code strucure.
=gfItemMask(@laStySeg)
FOR lnCnt = lnMjorCnt + 1 TO ALEN(laStySeg,1)
  IF laStySeg[lnCnt , 1] = "C"
    *-- Var. hold the start position of the color segment in the style code strucure.
    lnColorStr = laStySeg[lnCnt , 4]
    *-- Var. hold the color segment lenght in the style code strucure.
    lnColorLen = LEN(laStySeg[lnCnt , 3])
  ELSE
    *-- See if there is extended size scale in the style structure or not.
    IF llExtSizSc .AND. laStySeg[lnCnt , 1] = "S"
      lnSizePos  = laStySeg[lnCnt , 4]
      lnSizeLen  = LEN(laStySeg[lnCnt , 3])
    ENDIF
  ENDIF
ENDFOR
*-- end of lfStylInfo.