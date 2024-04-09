*:***************************************************************************
*: Program file  : POSSHDB.PRG
*: Program desc. : Dorby Style Cost Sheet
*: For screen    : POSSHDB.SCX
*:        System : Aria Advantage Series.
*:        Module : Style Purchase Order (PO)
*:        Date   : 08/30/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO POSSHDB
*:***************************************************************************
*: Due to C101627,1 [Customized Style Cost Sheet Screen]
*:***************************************************************************
*:Modifications :
*:---------------
*: B802578,1 MAB 09/02/1999 
*:           1- Say From Buyer Field.
*:           2- Browse Buyer Field.
*:           3- Selling price = nSellPrice {Custom Field in PosHdr File}.
*:***************************************************************************
*:

IF !gfSetup()
  RETURN
ENDIF  

=gfOpenFile(gcDataDir+"STYLE","CSTYLE",'SH')
SELECT STYLE
GO TOP
IF EOF()
  *** Message : "There is no records in the ð file can not proceed."
  ***           "                     < Ok >                       "
  =gfModalGen("TRM44010B00000" , "DIALOG" , "Style")
  RETURN
ENDIF  

*-- Save Setup Parameters in memvar [Begin]
*-- Save Setup Parameters in memvar [End  ]
*-- Fill Setups Memory Variables. [Begin]
DIMENSION laSetups[12,2]
laSetups[1,1]  = 'M_CISLBL1'
laSetups[2,1]  = 'M_CISLBL2'
laSetups[3,1]  = 'M_CISLBL3'
laSetups[4,1]  = 'M_CISLBL4'
laSetups[5,1]  = 'M_CISLBL5'

laSetups[6,1]  = 'M_CITYPE1'
laSetups[7,1]  = 'M_CITYPE2'
laSetups[8,1]  = 'M_CITYPE3'
laSetups[9,1]  = 'M_CITYPE4'
laSetups[10,1] = 'M_CITYPE5'

laSetups[11,1] = 'M_STYMARK'  && Top Based on Cost, else based on Price.
laSetups[12,1] = 'M_PODSPPRC'  && Selling price from PO screen.

=gfGetMemVar(@laSetups,gcAct_Comp)

PRIVATE lnAccess
lnAccess = 0
DIMENSION laTypes[5]
FOR lnAccess = 6 TO 10
  laTypes[lnAccess - 5] = laSetups[lnAccess,2]
ENDFOR


*-- Fill Setups Memory Variables. [End  ]

*-- Open Required Files [Begin]
=gfOpenFile(gcDataDir+"CODES","CODES",'SH')
=gfOpenFile(gcDataDir+"POSHDR","POSHDR",'SH')
=gfOpenFile(gcDataDir+"POSLN","POSLN",'SH')
=gfOpenFile(gcDataDir+"SCALE","SCALE",'SH')
=gfOpenFile(gcDataDir+"BOMLINE","Mfgopr",'SH')
=gfOpenFile(gcDataDir+"FABRIC","FABRIC",'SH')
=gfOpenFile(gcDataDir+"Ctktbom","Ctktbom",'SH')
*-- Open Required Files [End  ]
SELECT POSLN
llSelType = (TYPE("nSelPrice") = "N")

*-- Declare Select mode variables. [Begin]
STORE .F. TO llBrowse , llZapped
STORE '' TO lcCustomer , lcDesc , lcSeason , lcDivision , lcScale ,;
            lcSz1 , lcSz2 , lcSz3 , lcSz4 ,;
            lcSz5 , lcSz6 , lcSz7 , lcSz8 , lcPoRec , lcOldValue

STORE '' TO lcLstExpr && POs Range...

STORE 0 TO lnTotalCst , lnGross , lnGrossPer , lnSelling
lcMyScrFld = "lcCustomer,lcDesc,lcSeason,lcDivision,lcScale,lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8"

STORE "DISABLE" TO lcPoState , lcClrState
DIMENSION laColors[1]
laColors[1] = "N/A"
lnColors = 1
*-- Declare Select mode variables. [End  ]

*-- Define Screens Variables and Temporary file [Begin]
*-- lcMajorTtl : Variable Hold Major Title    
*-- lcMajorPic : Variable Hold Major Picture      
*-- lcNnMajTtl : Variable Hold Non   Major Title    
*-- lcNnMajPic : Variable Hold Non   Major Picture  
*-- lnClrStart : Variable Hold Color Start Position
*-- lnClrLen   : Variable Hold Color Length
STORE '' TO lcMajorTtl , lcMajorPic , lcNnMajTtl
STORE 0 TO lnClrStart , lnClrLen , lnBrRecNo
=lfStyMasks()
lnItmWid   = LEN(lcMajorPic)

lcCostCh1  = gfTempName()  && Child window 1 (Validation Region).
lcCostCh2  = gfTempName()  && Child window 2 (Browse Section).
lcCostCh3  = gfTempName()  && Child window 2 (Bottom Says Section).

lcBomLines = gfTempName()  && Temporary File Name.
lcCostBrow = 'Costing Details'
*-- Define Screens Variables and Temporary file [End  ]

*-- Create Temporary File... [Begin]
DIMENSION laTempStru[1,4]

SELECT BOMLINE
lnFldsCnt =AFIELDS(laTempStru)

DIMENSION laTempStru[lnFldsCnt+7,4]
laTempStru[lnFldsCnt+1,1] = "CCATGDESC"
laTempStru[lnFldsCnt+1,2] = "C"
laTempStru[lnFldsCnt+1,3] = 15
laTempStru[lnFldsCnt+1,4] = 0

laTempStru[lnFldsCnt+2,1] = "UOMUSE"
laTempStru[lnFldsCnt+2,2] = "C"
laTempStru[lnFldsCnt+2,3] = 3
laTempStru[lnFldsCnt+2,4] = 0

laTempStru[lnFldsCnt+3,1] = "cDesc"
laTempStru[lnFldsCnt+3,2] = "C"
laTempStru[lnFldsCnt+3,3] = 20
laTempStru[lnFldsCnt+3,4] = 0

laTempStru[lnFldsCnt+4,1] = "nTotCost"
laTempStru[lnFldsCnt+4,2] = "N"
laTempStru[lnFldsCnt+4,3] = 13
laTempStru[lnFldsCnt+4,4] = 2

laTempStru[lnFldsCnt+5,1] = "nCatOccur"
laTempStru[lnFldsCnt+5,2] = "N"
laTempStru[lnFldsCnt+5,3] = 3
laTempStru[lnFldsCnt+5,4] = 0

laTempStru[lnFldsCnt+6,1] = "nLinOccur"
laTempStru[lnFldsCnt+6,2] = "N"
laTempStru[lnFldsCnt+6,3] = 3
laTempStru[lnFldsCnt+6,4] = 0

laTempStru[lnFldsCnt+7,1] = "nLinPrice"
laTempStru[lnFldsCnt+7,2] = "N"
laTempStru[lnFldsCnt+7,3] = 12
laTempStru[lnFldsCnt+7,4] = 3

CREATE TABLE (gcWorkDir+lcBomLines) FROM ARRAY laTempStru
ZAP
INDEX ON ccatgtyp+cbomtyp+item+iclr+mfgcode TAG (lcBomLines) OF ;
         (gcWorkDir+lcBomLines+'.CDX')
*-- Create Temporary File... [End  ]

*-- Make Relation Between files.

PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCostBrow)

DO (gcScrDir+gcWinAppl+'\POSSHDB.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcCostBrow)
IF USED(lcBomLines)
  USE IN (lcBomLines)
ENDIF

IF FILE(gcWorkDir+lcBomLines+'.DBF')
  ERASE &gcWorkdir.&lcBomLines..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcBomLines+'.CDX')
  ERASE &gcWorkdir.&lcBomLines..CDX          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcBomLines+'.FPT')
  ERASE &gcWorkdir.&lcBomLines..FPT          && Erase the Temp file.
ENDIF
*-- end of program code.


**********************************************************************
**************** Style Mask Function ***************
**********************************************************************
*!**************************************************************************
*! Name      : lfStyMasks
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Example     : = lfStyMasks()
*!**************************************************************************
*
FUNCTION lfStyMasks
PRIVATE lnI , lnSegLen
STORE 0 TO lnI , lnClrStart , lnSegLen
lnMajSeg = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
STORE '' TO lcMajorTtl , lcMajorPic , lcNnMajTtl , lcNnMajPic

FOR lnI = 1 TO ALEN(laMajSegs,1)
  *-- Evaluate Major title and picture
  lnSegLen = LEN(laMajSegs[lnI,3])
  IF lnI <= lnMajSeg
    = lfEvlMajNn(@lcMajorTtl , @lcMajorPic)
  ELSE  && else Evaluate Non Major title and picture
    = lfEvlMajNn(@lcNnMajTtl , @lcNnMajPic)
    IF laMajSegs[lnI,1] = "C"
      lnClrStart = laMajSegs[lnI,4]
      lnClrLen   = lnSegLen
    ENDIF
  ENDIF
ENDFOR
*-- End Of lfStyMasks.

*!**************************************************************************
*! Name      : lfEvlMajNn
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Evaluate Style Masks.
*!**************************************************************************
*! Passed Parameters : 1 - (lcTltValue) 
*!                     2 - (lcPicValue)
*!**************************************************************************
*! Example     : = lfEvlMajNn()
*!**************************************************************************
*
FUNCTION lfEvlMajNn
PARAMETERS lcTltValue , lcPicValue
IF !EMPTY(lcTltValue)
  lcTltValue = lcTltValue + laMajSegs[lnI-1,6]
  lcPicValue = lcPicValue + laMajSegs[lnI-1,6]
ENDIF
lcTltValue = lcTltValue + PADR(laMajSegs[lnI,2],lnSegLen)
lcPicValue = lcPicValue + laMajSegs[lnI,3]
*-- End Of lfEvlMajNn.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************
*!*************************************************************
*! Name      : lfDispBrow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

*-- Release browse window if found.
IF WEXIST(lcCostBrow)
  RELEASE WINDOW (lcCostBrow)
ENDIF

SELECT (lcBomLines)
GO TOP
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO(lcBomLines)=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "cCatgDesc   :R :H= '      '     :8  ,"+;
             "cItem=IIF('Sub Total'$cDesc,'',Item)   :R :H= 'Item'   :10 ,"+;
             "cIClr=IIF('Sub Total'$cDesc,'',IClr)   :R :H= 'I.Clr' ," +;
             "cMfg=IIF('Sub Total'$cDesc,'',MfgCode) :R :H= 'MFG' :10 ," +;
             "cDesc       :R :H= 'Description' ," +;
             "UOMUse      :R :H= 'UOM' ," +;
             "UnitQty     :R :H='Unt Qty' :P='@Z 999.99' ," +;
             "UnitCost    :R :H='Unt Cost' :P='@Z 9999.99' ," +;
             "nTotCost    :R :H='Total Cst' :P='@Z 99999.99' "

BROWSE FIELDS &lcBrowFild     ;
       WINDOW (lcCostCh2)     ;
       WHEN lfwBrows()        ;
       VALID :F lfvBrow()     ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcCostBrow
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(lcBomLines)
SHOW WINDOW (lcCostBrow) REFRESH
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Valid Browse function
*!*************************************************************
*
FUNCTION lfvBrow
IF !WONTOP(lcCostBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCostBrow)
ENDIF
*-- end of lfvBrow.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcCostBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 
ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcCostBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCostBrow)
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
  _CUROBJ = OBJNUM(pbTop)

ELSE  && Backtab Case.

  ACTIVATE WINDOW (lcCostCh1)
  IF laScrMode[1]
    _CUROBJ = OBJNUM(lcMajor)
  ELSE
    IF "N/A" $ laColors[1]
      _CUROBJ = OBJNUM(lcPo)
    ELSE
      _CUROBJ = OBJNUM(lnColors)
    ENDIF  
  ENDIF  

ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************
*!*************************************************************
*! Name      : lpShow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Control Showing Screen
*!*************************************************************
*
PROCEDURE lpShow
STORE "DISABLE" TO laCtrStat[7] , laCtrStat[8] , laCtrStat[10]

ACTIVATE WINDOW gwcContrl1		&& Activate control panel window
SHOW GET pbEdt  DISABLE
SHOW GET pbDlt  DISABLE
SHOW GET pbbrws DISABLE

IF laScrMode[1]
  STORE '' TO lcMajor , lcPo
  =lfSelMode()
ELSE
  lcMajor = PADR(lcMajor,lnItmWid)
  =lfViewMode("COLD")
ENDIF
=lfRefScr()
*-- end of lpShow.

*!*************************************************************
*! Name      : lfSelMode
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/26/1999
*! Purpose   : Select mode Variables
*!*************************************************************
*
FUNCTION lfSelMode
STORE '' TO lcCustomer , lcDesc , lcSeason , lcDivision , lcScale ,;
            lcSz1 , lcSz2 , lcSz3 , lcSz4 ,;
            lcSz5 , lcSz6 , lcSz7 , lcSz8

STORE 0 TO lnTotalCst , lnGross , lnGrossPer , lnSelling

STORE "DISABLE" TO lcPoState , lcClrState
DIMENSION laColors[1]
laColors[1] = "N/A"
lnColors = 1

IF RECCOUNT(lcBomLines) > 0
  SELECT(lcBomLines)
  ZAP
  llZapped = .T.
ENDIF
*-- end of lfSelMode.

*!*************************************************************
*! Name      : lfViewMode
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/26/1999
*! Purpose   : View Mode Variables
*!*************************************************************
*
FUNCTION lfViewMode
PARAMETERS lcModeAct

IF lcModeAct = "COLD"
  lcPoState = "ENABLE"

  *-- Variables per Style
  lcDesc = Style.Desc
  
ELSE  && Select Default Color.
  
  lcClrState = "ENABLE"
  *-- Get Colors and fill array (Default is first color) 
  
  SELECT BOMLINE  
  SELECT DISTINCT SUBSTR(Style,lnClrStart,lnClrLen) FROM BOMLINE ;
    WHERE cimtyp+ctktno = "I" + lcPo AND Style = lcMajor ;
    INTO ARRAY laColors
  
  lnColors = 0
  
  *B802578,1 Say From Buyer Field. [Begin]
  *lcCustomer = POSHDR.Account
  lcCustomer = POSHDR.Buyer
  *B802578,1 Say From Buyer Field. [End  ]
  
  =lfvColor()
ENDIF
*-- end of lfViewMode.

*!*************************************************************
*! Name      : lfRefScr
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Refresh screen Controls
*!*************************************************************
*
FUNCTION lfRefScr
PRIVATE lcCurScrFd , lcCurrVar
ACTIVATE WINDOW (lcCostCh1) TOP
SHOW GET lcPo &lcPoState
SHOW GET ibOrdBrow &lcPoState
SHOW GET lnColors &lcClrState

lcCurScrFd = lcMyScrFld
DO WHILE !EMPTY(lcCurScrFd)
  lcCurrVar  = SUBSTR(lcCurScrFd,1,ATC(",",lcCurScrFd)-1)
  lcCurScrFd = STRTRAN(lcCurScrFd,lcCurrVar+",","")
  IF EMPTY(lcCurrVar)
    lcCurrVar  = lcCurScrFd
    lcCurScrFd = ""
  ENDIF
  IF !EMPTY(lcCurrVar)
    SHOW GET &lcCurrVar
  ENDIF  
ENDDO

ACTIVATE WINDOW (lcCostCh3) TOP
SHOW GET lnTotalCst
SHOW GET lnGross
SHOW GET lnGrossPer
SHOW GET lnSelling

IF llZapped
  llZapped = .F.
  =lfDispBrow()
ELSE
  SHOW WINDOW (lcCostBrow) REFRESH
ENDIF  
*-- end of lfRefScr.

**********************************************************************
********************** Validation Functions **************************
**********************************************************************
*!*************************************************************
*! Name      : lfvStyle
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Style validation
*!*************************************************************
*
FUNCTION lfvStyle
IF MDOWN()
  RETURN
ENDIF


PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT Style
SET ORDER TO cStyle

*-- If come from BitMap button or the key is found
IF llBrowse OR (!EMPTY(lcMajor) AND !SEEK(lcMajor))
  llBrowse  = .F. 		            && Disable flag to its intial state   
  lcMajor = gfStyBrw('M','','',.F.) && Calling global Style browse 	
  IF EMPTY(lcMajor)	                && If ESCAPE pressed wait in your position	
    _CUROBJ = _CUROBJ
  ENDIF 						    && Ending ESCAPE pressed
ENDIF							    && Ending seek condition

IF !EMPTY(lcMajor)

  IF lfFndInBom()
    lcPoRec   = "P" + PO
    laScrMode = .F.
    laScrMode[2] = .T.
    SHOW GETS  

  ELSE    

    *Message : 34167 "No PO found for Style "  + lcMajor + " can not display the cost sheet."
    *Button  : 00000 < Ok >
    lcMessage = ALLTRIM(lcMajorTtl) + ' ' + ALLTRIM(lcMajor)
    =gfModalGen("INM34167B00000" , "DIALOG" , lcMessage)
    
    SET FILTER TO
    SELECT POSLN
    lcMajor = ''
    _CUROBJ = _CUROBJ
  ENDIF  

ENDIF
SELECT(lnAlias)
*-- end of lfvstyle.

*!*************************************************************
*! Name      : lfvPo
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/25/1999
*! Purpose   : Style PO validation
*!*************************************************************
*
FUNCTION lfvPo
IF MDOWN()
  RETURN
ENDIF

PRIVATE lnAlias
lnAlias = SELECT(0)

*-- If come from BitMap button or the key is found
IF llBrowse OR (!EMPTY(lcPo) AND !SEEK("P"+lcPo,"POSHDR"))
  llBrowse  = .F. 		            && Disable flag to its intial state   

  =gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')

  *B802578,1 Add Customer field to PO Browse.
  lcBrFields = [PO        :R :H='PO #':8,]+;
               [Status    :R :H='S':2,]+;
               [Buyer     :R :H='Customer':20,]+;
               [Vendor    :R :H='Vendor':11,]+;
               [lcVnName = ApVendor.cVenComp :R :H='Name':18,]+;
	           [Complete  :R :H='Complete':8,]+;
  	           [nStyOrder :R :H='Tot.Qty.':7,]+;
               [POTotal   :R :H='Amount':10,]+;
               [Receive   :R :H='Receive':7,]+;
               [Open      :R :H='Open':7]

  SELECT POSHDR
  SET RELATION TO Vendor INTO APVendor
  SET FILTER TO

  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  =SEEK(lcPoRec)
  =lfFndInBom()

  = gfBrows('','PO','laTemp')  
  SET RELATION OFF INTO APVendor
  lcPo = laTemp[1]

  IF EMPTY(lcPo)	                && If ESCAPE pressed wait in your position	
    _CUROBJ = _CUROBJ
  ENDIF 						    && Ending ESCAPE pressed

ENDIF							    && Ending seek condition

IF !EMPTY(lcPo)
  SELECT POSHDR
  SET FILTER TO
  =lfViewMode("HOT")
ENDIF
SELECT(lnAlias)
*-- end of lfvPo.

*!*************************************************************
*! Name      : lfvColor
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/26/1999
*! Purpose   : Color Valid Function
*!*************************************************************
*
FUNCTION lfvColor
IF TYPE("lcOldValue") <> "N"
  lcOldValue = 1
ENDIF

IF lnColors = lcOldValue
  RETURN
ENDIF

PRIVATE lcLastCatg , lnCatgCost , lnType , lcType , lnCatOccur , lcCatgUpdt,;
        lnSelPrice
PRIVATE lnAlias
lnAlias = SELECT(0)

IF lnColors = 0
  lnColors = 1
ENDIF

*Variables per PO for default color.
SELECT STYLE
SET ORDER TO
LOCATE FOR Style = lcMajor AND ;
       SUBSTR(Style,lnClrStart,lnClrLen) = PADR(laColors[lnColors],lnClrLen)
lcSeason   = Style.Season
lcDivision = Style.cDivision
lcScale    = Style.Scale
=SEEK("S"+lcScale,"SCALE")
lcSz1 = Scale.Sz1
lcSz2 = Scale.Sz2
lcSz3 = Scale.Sz3
lcSz4 = Scale.Sz4
lcSz5 = Scale.Sz5
lcSz6 = Scale.Sz6
lcSz7 = Scale.Sz7
lcSz8 = Scale.Sz8
*-- Fill Temp. File

IF RECCOUNT(lcBomLines) > 0
  SELECT(lcBomLines)
  ZAP
  llZapped = .T.
ENDIF

SET ORDER TO PosLns IN PosLn

SELECT BOMLINE
SCAN FOR cimtyp+ctktno+coprcode+ctype+style+sclr+;
         IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr = "I" + lcPo
  IF cType = "1" AND (Style = lcMajor) AND ;
     SUBSTR(Style,lnClrStart,lnClrLen) = PADR(laColors[lnColors],lnClrLen)
    
    DO CASE
      CASE ccatgtyp = "F"
        lcCatgUpdt = "1"
      CASE ccatgtyp = "T"
        lcCatgUpdt = "2"
      CASE ccatgtyp = "P"
        lcCatgUpdt = "3"
      CASE ccatgtyp = "D"
        lcCatgUpdt = "4"
      CASE ccatgtyp = "M"
        lcCatgUpdt = "5"
      OTHERWISE 
        lcCatgUpdt = ''
    ENDCASE
    
    *B802578,1 Compute selling price from new custom PO Header field [Begin]
    *=SEEK(Style+"P"+ctktno+STR(LineNo,6),"POSLN")
    *lnSelPrice = IIF(llSelType AND POSLN.nSelPrice != 0,;
    *             POSLN.nSelPrice,STYLE.PRICEA)
    IF TYPE("POSHDR.nSellPrice") = "N"
      lnSelPrice = POSHDR.nSellPrice
    ELSE  && Field was not Numeric .
      WAIT WINDOW "PO Selling price must be numeric ! "  TIMEOUT 2
      lnSelPrice = 0
    ENDIF
    *B802578,1 Compute selling price from new custom PO Header field [End  ]

    *-- if you found this line before.
    IF SEEK(lcCatgUpdt+cbomtyp+item+iclr+mfgcode,lcBomLines)
      
      SELECT (lcBomLines)

      REPLACE nLinOccur WITH nLinOccur + 1 ,;
              UnitQty   WITH UnitQty + BOMLINE.UnitQty   ,;
              UnitCost  WITH UnitCost + BOMLINE.UnitCost ,;
              nTotCost  WITH nTotCost + BOMLINE.UnitQty * BOMLINE.UnitCost ,;
              nLinPrice WITH nLinPrice + lnSelPrice
      
    ELSE
    
      SCATTER MEMVAR
      lnType = ASCAN(laTypes,m.cCatgtyp)
      m.cCatgDesc = ''
      IF lnType > 0
        m.cCatgDesc = laSetups[lnType,2]
      ENDIF  
    
      SELECT (lcBomLines)
      IF m.ccatgtyp $ "FT"
        =SEEK(Item + IClr,"FABRIC")
        m.UOMUse = Fabric.UomUse
        m.cDesc  = FABRIC.Desc
      ELSE
        IF lnType > 0
          lcType = STR(lnType,1)
          =SEEK("I"+m.ctktno+lcType+m.item+m.iclr+m.mfgcode+m.dyelot,"Ctktbom")
          m.UOMUse = Ctktbom.Uom
          m.cDesc  = Ctktbom.Desc
        ENDIF  
      ENDIF  
    
      m.ccatgtyp = lcCatgUpdt

      m.nLinOccur = 1
      m.nTotCost  =  m.UnitQty * m.UnitCost

      *m.nLinPrice = IIF(laSetups[12,2],POSLN.nSelPrice,STYLE.PRICEA)
      m.nLinPrice  = lnSelPrice

      INSERT INTO (lcBomLines) FROM MEMVAR   
    
    ENDIF
  
  ENDIF  
ENDSCAN

USE (gcWorkDir+lcBomLines) IN 0 AGAIN ALIAS AddTotal
SELECT (lcBomLines)
GO TOP
lcLastCatg = ccatgtyp
STORE 0 TO lnCatOccur , lnCatgCost , lnTotalCst

SCAN
  IF ccatgtyp <> lcLastCatg
    lnTotalCst = lnTotalCst + lnCatgCost
    m.cDesc    = "Sub Total"
    m.nTotCost = lnCatgCost
    INSERT INTO AddTotal FROM MEMVAR   
    lcLastCatg = ccatgtyp
    lnCatgCost = 0
    lnCatOccur    = 0
  ENDIF

  lnCatOccur  = lnCatOccur + 1
  REPLACE nCatOccur WITH lnCatOccur ,;
          UnitQty   WITH UnitQty/nLinOccur,;
          UnitCost  WITH UnitCost/nLinOccur ,;
          nTotCost  WITH nTotCost/nLinOccur ,;
          nLinPrice WITH nLinPrice/nLinOccur
  
  lnCatgCost  = lnCatgCost + nTotCost
  SCATTER MEMVAR BLANK
  m.ccatgtyp = ccatgtyp
  m.lineno   = lineno
  m.cbomtyp  = cbomtyp
  m.item     = item
  m.iclr     = iclr
  m.mfgcode  = mfgcode
ENDSCAN

lnTotalCst = lnTotalCst + lnCatgCost
m.cDesc    = "Sub Total"
m.nTotCost = lnCatgCost
INSERT INTO AddTotal FROM MEMVAR   

REPLACE ALL cCatgDesc WITH " " FOR nCatOccur <> 1

USE IN AddTotal
GO TOP

*-- Define Bottom screen values. [Begin]
*-- if user enter selling price in PO Screen.
lnSelling  = nLinPrice
lnTotalCst = lnTotalCst

lnGross = lnSelling - lnTotalCst
*-- if Top Market (Based on Cost.)
IF laSetups[11,2] = "T"
  lnGrossPer = IIF(lnTotalCst=0,0,lnGross / lnTotalCst * 100)
ELSE
  lnGrossPer = IIF(lnSelling=0,0,lnGross / lnSelling * 100)
ENDIF
*-- Define Bottom screen values. [End  ]

=lfRefScr()
=lfwBrows()
SELECT(lnAlias)
*-- end of lfvColor.

*!*************************************************************
*! Name      : lfWOldVal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/26/1999
*! Purpose   : Get Color Old Value.
*!*************************************************************
*
FUNCTION lfWOldVal
lcOldValue = EVALUATE(SYS(18))
*-- end of lfWOldVal.


*!*************************************************************
*! Name      : lfWOldVal
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 08/26/1999
*! Purpose   : Check existance of style in BOMLINE File.
*!*************************************************************
*
FUNCTION lfFndInBom
PRIVATE llFindSty , lcFiltExpr
SELECT BOMLINE
=SEEK("I")
lcLstExpr = ''
SCAN REST FOR PADR(Style,lnItmWid) = lcMajor
  lcLstExpr = lcLstExpr + "P" + ctktNo + '|'
ENDSCAN
llFindSty = !EMPTY(lcLstExpr)

IF llFindSty
  lcFiltExpr = [cStyType+Po$lcLstExpr AND !(Status $ "HX")]
  
  SELECT POSHDR
  SET FILTER TO &lcFiltExpr
  GO TOP
  
  llFindSty = !EOF()
ENDIF
RETURN llFindSty
*-- end of lfFndInBom.