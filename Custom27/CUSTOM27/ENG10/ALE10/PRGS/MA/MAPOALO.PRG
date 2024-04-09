*:****************************************************************************************
*: Program file  : MAPOALO.PRG
*: Program desc. : MATERIAL PO ALLOCATION SCREEN [For Cust ENG10]
*: System        : Aria Apparel System [A27].
*: Module        : Material Module  [MA]
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#200149,1
*: Date          : 02/01/2001
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : lpshow    , lfvData1  , lfBrowLine , lfPOLines , lfwBrowUp
*:               :            : lfvAddSls , lfvOrderNo, lfCrTmpFls  ,lpSavScr  ,
*:               ------------------------------------------------------------------------
*:               : PROCEDURE  : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#200149,1          : 
*:****************************************************************************************
*:Modifications :
*:****************************************************************************************
*:

*-- Declaration variables.
EXTERNAL ARRAY laData , laDefProc ,laScrMode
STORE .F. TO llBrowse,llOGFltCh ,IISel
STORE ''  TO lcPo , lcScFields, lcSleOrd
DIMENSION laData[1]
STORE 0 To lnRecNo
lcDet_Ttl = "P/O Lines"

STORE "" TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWindTitle ,lcDetTmp
lcOrdType = "O"
lcMark    = '>'
lcUnMark   = SPACE(1)

*-- End Declaration variables.



IF !gfSetup()
  RETURN
ENDIF  

lcScFields  = 'POMAT'

IF !WEXIST(gcBaseWind)
  *--Program screen windows temprorary names.
  lcWinCh0    = gftempname()     &&Temp name for Windows.
  lcWinCh1    = gftempname()     &&Temp name for Windows.
  lcWinCh2    = gftempname()     &&Temp name for Windows.
  lcWinCh3    = gftempname()     &&Temp name for Windows.  
  *-- Temp File.
  lcDetTmp   = gfTempName()
  = lfCrTmpFls ()

ENDIF
*--ToolBar external proc.
laDefProc[7]  = .F.      && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.      && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .T.      && Disable the control panel close proc. (lpClsScr)


=lfPOLines()
PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDet_Ttl)
ON KEY LABEL TAB     DO lptab 

DO (gcScrDir+gcWinAppl+'\MAPOALO.SPX')
ON KEY LABEL ALT+B
POP KEY
*-- If exit normal erase the temp file.


IF glQuitting
  RELEASE WINDOW (lcDet_Ttl)
ELSE
  DEACTI WINDOW (lcDet_Ttl)
ENDIF

*-- End of program code.
*##G#-#-# General functions.

*:*************************************************************
*: Name          : lpshow
*: Developer     : ABDOU ELGENDI - (ABD)
*: Date          : 02/01/2001
*: Purpose       : Show folder when Screen Mode Changes.
*:*************************************************************
*: Calls         : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example       : =lpShow()
*:*************************************************************
*
FUNCTION lpShow

SELECT (lcBaseFile)

IF TYPE('lcBaseFile')='C' AND TYPE('laUsrFields') # "U" AND !EMPTY(laUsrFields)
  SHOW GET pbUsrFields DISABLE
ENDIF
IF TYPE('lcBaseFile')='C' AND TYPE(lcBaseFile+'.lHasNotes') = "L"
  SHOW GET pbNotePad DISABLE 
ENDIF
IF !EMPTY(lcSydKey)
  SHOW GET pbAudTrail DISABLE
ENDIF

DO CASE
  *-- S E L E C T   M O D E -----------------------------------------------
  CASE laScrMode[1]  && Select mode. 
    SHOW GET pbDlt     DISABLE
    SELECT(lcDetTmp)
    ZAP
    SHOW WINDOW  (gcBaseWind) REFRESH
  *-- V I E W   M O D E ---------------------------------------------------
  CASE laScrMode[2]  && View mode.
    = lfPOLines ()
    SHOW WINDOW  (gcBaseWind) REFRESH
    SHOW GET pbDlt     DISABLE
  ***--- E D I T   M O D E-------------------------------------------------
  CASE laScrMode[3]  && Edit mode.

  *-- A D D   M O D E -----------------------------------------------------
  CASE laScrMode[4]  && Add mode. 
ENDCASE

*--End Of lpShow.

*##0#-#-# Main functions.
*:*************************************************************
*: Name          : lfvData1
*: Developer     : ABDOU ELGENDI - (ABD)
*: Date          : 02/01/2001
*: Purpose       : Valid function for PO Number
*:*************************************************************
*: Calls         : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example       : =lfvData1()
*:*************************************************************
*
FUNCTION lfvData1
Parameter lcPram
lcOldFiltr = ''
lnAlias = SELECT()
llMdown = MDOWN()
IF llMdown
  RETURN
ENDIF

lcPram   = IIF(TYPE('lcPram') = 'C',lcPram,'')
lcPOType = 'P'
SELECT POFHDR
lcOldFiltr = FILTER()
SET FILTER TO POFHDR.Status = 'O'
IF llBrowse .OR. lcPram = 'B' .OR. !EMPTY(laData[1]) AND !laScrMode[4]
  laData[1] = IIF(EMPTY(laData[1]),'?',laData[1])
  IF !SEEK('P'+laData[1],"POFHDR")
    DECLARE laBrow[1]
    laBrow    =' '
    lcVendor = ''
    lcTitle  = 'Material Purchase Orders'
    lcBrFields = "POMAT   :R :H='P/O #':8,"+;
                 "Status  :R :H='S':2,"+;	         	           	         
                 "Vendor  :R :H='Vendor':10,"+;
	             "ApVendor.cVenComp :R :H='Name':18,"+;
	             "Complete:R :H='Complete':8,"+;
  	             "NFABORDER  :R :H='Tot.Qty.':7,"+;	         
                 "POTotal :R :H='Amount':10,"+;
                 "NFBRECEIVE :R :H='Receive':7,"+;
                 "NPO_OPEN   :R :H='Open':7"
    llOpnApVen = gfOpenFile(gcDataDir+'ApVendor',gcDataDir+'VenCode','SH')
    CLEAR TYPEAHEAD
    SELECT POFHDR
    SET RELATION TO Vendor INTO ApVendor
    =SEEK(lcVendor+lcPOType)
    llRetValue = gfBrows('lcVendor+lcPOType',"PoMat,Vendor","laBrow",lcTitle)
    IF llRetValue
      lcPOBrow  = laBrow[1]
    ELSE
      lcPOBrow  = ''
    ENDIF
    laData[1] = lcPOBrow  
    _CUROBJ=OBJNUM(laData[1])
  ENDIF
  IF !EMPTY(laData[1])
    IF POFHDR.Status = 'X'
      lcStatus = 'Cancelled'    
    ENDIF 
    laScrMode[1] = .F.
    laScrMode[2] = .T.
    SHOW GETS
  ENDIF
  llBrowse  = .F.
ENDIF  

SET FILTER TO &lcOldFiltr
SELECT(lnAlias )

*-- End OF lfvData1.
*:*************************************************************
*: Name          : lfBrowLine
*: Developer     : ABDOU ELGENDI - (ABD)
*: Date          : 02/01/2001
*: Purpose       : browse lines for the selected P/O
*:*************************************************************
*: Calls         : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example       : =lfBrowLine()
*:*************************************************************
*
FUNCTION lfBrowLine

lnAlias = SELECT()
SELECT (lcDetTmp)
lnRecNo = RECNO(lcDetTmp)
lcBrowFlds = [lcMarker=IIF(RECNO() = lnRecNo ,'>',' '):1:H=' ':W=.F.,]+;
             [Fabric  :R:H='Item' :10,]+;
             [Color   :R:H='Color':10]

BROWSE FIELDS &lcBrowFlds;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       VALID :F lfvBrows();       
       WHEN lfwBrowUp();
       TITLE lcDet_Ttl ;
       WINDOW (lcWinCh2) IN WINDOW (gcBaseWind)

SELECT(lnAlias)

*-- End Of lfBrowLine.
*:*************************************************************
*: Name        : lfPOLines
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/01/2001
*: Purpose     : get all lines from the POFLN for the selected P/O
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : =lfvData1()
*:*************************************************************
*
FUNCTION lfPOLines
PRIVATE lcTempOrds , lnlineno
STORE '' TO lcTempOrds
SELECT(lcDetTmp)
ZAP

*-- Select the PO liens from the poflin file
SELECT POFLN
SCAN FOR cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) = 'P'+laData[1] .AND. Trancd = '1'
  STORE 0 To lnlineno
  
  SCATTER MEMVAR MEMO
  INSERT INTO (lcDetTmp) FROM MEMVAR
  
  *-- get the ass. SO to the selected PO# . 
  IF SEEK(POMAT+FABRIC+COLOR,'MAPOALO') 
    SELECT MAPOALO
    SCAN FOR POMAT+FABRIC+COLOR+ORDER = POFLN.POMAT+POFLN.FABRIC+POFLN.COLOR
     lcTempOrds = lcTempOrds + IIF(lnlineno > 0 ,"|" + ORDER, ORDER)
     lnlineno = lnlineno + 1
    ENDSCAN
    *-- Save the variable to the filed.
    SELECT (lcDetTmp)
    REPLACE mOrdrsPo WITH lcTempOrds
    lcTempOrds = ''
    SELECT POFLN
  ENDIF
ENDSCAN

SELECT(lcDetTmp)
GOTO TOP

*-- End Of lfPOLines.
*:*************************************************************
*: Name      : lfwBrowUp
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/01/2001
*: Purpose   : Refresh record pointer.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfwBrowUp()
*:*************************************************************
*: Due to  C#200155,1.
*:*************************************************************
*
FUNCTION lfwBrowUp

lnRecNo = RECNO(lcDetTmp)
REPLACE ALL cMarker WITH lcUnMark
GO lnRecNo
REPLACE cMarker WITH lcMark

SHOW WINDOW (lcDet_Ttl) REFRESH
SHOW WINDOW  (gcBaseWind) REFRESH

*-- End Of lfwBrowUp.
*:*************************************************************
*: Name      : lfvAddSls
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/01/2001
*: Purpose   : Add sales order to item color.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfvAddSls ()
*:*************************************************************
*
FUNCTION lfvAddSls

*-- Paramater that pass to  Program OGINLIST.PRG

*-- 1- LcOgDataTyp  ----- > "C"
*-- 2- LcOgPicture  ----- > field picture (order)
*-- 3- LnOgSize     ----- > 7
*-- 4- LcOgVld      ----- > " lfVorder() .and. lfwOldVal() "
*-- 5- LcvldEntt    ----- > "E"
*-- 6- LcOgVldent   ----- > ""
*-- 7- LcOgVar      ----- >  "lcVal "   variable hold the select order  "000001|000002|000004|000008"
*-- 8- LnFldLenth   ----- >  7


*-- restore memo field to variable.

SELECT (lcDetTmp)

IF EMPTY(mOrdrsPo)
  lcSleOrd = ''
ELSE
  *-- Get the file in variable
  lcSleOrd = ALLTRIM(&lcDetTmp..mOrdrsPo)
ENDIF

lcOldValue = lcSleOrd
IISel = OgInList("C","@! XXXXXX",6,"lfvOrderNo()","E","","lcSleOrd",7)

*-- save all changes.
IF lcOldValue <> lcSleOrd
  = gfUpdate() 
ENDIF


*-- Add this order to the Memo filed Or leave it Empty.
SELECT (lcDetTmp)
*-- Save the variable to the filed

REPLACE mOrdrsPo WITH lcSleOrd

*-- End Of lfvAddSls
*:*************************************************************
*: Name      : lfvOrderNo
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/01/2001
*: Purpose   : Add sales order to item color.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example   : = lfvOrderNo()
*:*************************************************************
*
FUNCTION lfvOrderNo
PRIVATE laBrowArr , lcOldFilt ,lnAlias,lcOldFiltr
DECLARE laBrowArr[1]
STORE '' TO laBrowArr , lcOldFiltr
STORE 0 To lnAlias


*-- lcOgValue : - variable holed the entered value in the inlist filed.
IF !EMPTY(lcOgValue) .AND. (!SEEK('O'+lcOgValue,'ORDHDR')) ;
   .OR. !(ORDHDR.Status $ 'OH'))
  lnAlias = SELECT (0)
  SELECT ORDHDR
  lcOldFiltr = FILTER()
  lcOldBros = lcBrFields
  SET FILTER TO ORDHDR.Status $ 'OH'
  lcOrder = lcOgValue

  lcBrFields = [Order:H="Order#",]+;
               [status:1:H="S",]+;
               [ACCOUNT:H="Acct",]+;
               [store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
               IIF(lcOrdType='T',[Dept:5:H="Dept.",],'')+;
               [CustPo=IIF(multipo,'*Multi_PO*',custpo):H="Cust. P.O#",]+;
               [Open:H="Open.Qty.",]+;
               [OpenAmt:H="Open.Amt.",]+;
               [Ship:H="Ship.Qty.",]+;
               [Shipamt:H="Ship.Amt.",]+;
               [start:H="Start",]+;
               [Complete:H="Complete",]+;
               [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
               [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
               [Note1:6:H="Notes"]
  lcOrder = IIF(ARIABROW("lcOrdType","Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','ORDER','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
  
  lcOgValue  = lcOrder
  lcBrFields = lcOldBros
  llBrowse = .F.
  SET FILTER TO &lcOldFiltr
  SELECT (lnAlias)
ENDIF


*-- End OF lfvOrderNo
*:*************************************************************
*: Name        : lfCrTmpFls
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/01/2001
*: Purpose     : Create the temp file.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : =lfCrTmpFls()
*:*************************************************************
*
FUNCTION lfCrTmpFls
PRIVATE lnAlias
lnAlias = 0 

lnAlias = SELECT(0)
SELECT POFLN

=AFIELDS(laMlnTmp)
lnNewFld = ALEN(laMlnTmp,1)

DIMENSION laMlnTmp[lnNewFld+2,4]

laMlnTmp[lnNewFld + 1,1] = 'cMarker'
laMlnTmp[lnNewFld + 1,2] = 'C'
laMlnTmp[lnNewFld + 1,3] = 1
laMlnTmp[lnNewFld + 1,4] = 0

laMlnTmp[lnNewFld + 2,1] = 'mOrdrsPo'
laMlnTmp[lnNewFld + 2,2] = 'M'
laMlnTmp[lnNewFld + 2,3] = 10
laMlnTmp[lnNewFld + 2,4] = 0


CREATE DBF (gcWorkDir+lcDetTmp) FROM ARRAY laMlnTmp
=gfOpenFile(gcWorkDir+lcDetTmp,'','EX')
INDEX ON cMatType+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7) TAG lcDetTmp
SELECT(lnAlias)

*-- End Of lfCrTmpFls

*:*************************************************************
*: Name        : lpSavScr
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 02/01/2001
*: Purpose     : Save/Update P/O.
*:*************************************************************
*: Calls       : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example     : DO lpSavScr
*:*************************************************************
*
PROCEDURE lpSavScr
PRIVATE laTempOrds , lcTempOrd
DIMENSION laTempOrds[1]
STORE '' TO lcTempOrd , laTempOrds


SELECT (lcDetTmp)
GOTO TOP


SCAN
  *-- Delete all related recored for this po,item,color
  SELECT MAPOALO
  DELETE ALL FOR POMAT+FABRIC+COLOR = &lcDetTmp..POMAT+&lcDetTmp..FABRIC+&lcDetTmp..COLOR
  SELECT (lcDetTmp)
  lcTempOrd = &lcDetTmp..mOrdrsPo
  IF !EMPTY(lcTempOrd)
    *-- globel function to get the variable in array without seperator.
    = gfSubstr(ALLTRIM(lcTempOrd), @laTempOrds,"|")
    FOR I = 1 To ALEN(laTempOrds)
      IF !SEEK(POMAT+FABRIC+COLOR+ALLTRIM(laTempOrds[I]),'MAPOALO')
        SCATTER MEMVAR MEMO
        SELECT MAPOALO
        APPEND BLANK
        REPLACE POMAT     WITH &lcDetTmp..Pomat      ,;
                FABRIC    WITH &lcDetTmp..Fabric     ,;
                COLOR     WITH &lcDetTmp..Color      ,;
                ORDER     WITH ALLTRIM(laTempOrds[I]),;
                dAdd_Date WITH gdSysDate             ,;
                cAdd_Time WITH TIME()                ,;
                cAdd_User WITH gcUser_id
      ENDIF
    ENDFOR  
  ENDIF  
  SELECT (lcDetTmp)
  lcTempOrd = ''
ENDSCAN

SELECT POFHDR
=gfObj_Lock(.F.)

*-- End OF lpSavScr
*:*************************************************************
*: Name      : lfvBrows
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/01/2001
*: Purpose   : Valid Browse function.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  =lfvBrows()
*:*************************************************************
*
FUNCTION lfvBrows

IF WONTOP() # (lcDet_Ttl)
  = gfStopBrow()
ENDIF

*-- End OF lfvBrows.
*:*************************************************************
*: Name      : lpTab
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 02/01/2001
*: Purpose   : Tab key trapping.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpTab
*:*************************************************************
*
PROCEDURE lpTab

IF WONTOP() = lcDet_Ttl
  ACTIVATE WINDOW (lcWinCh3)
  _CUROBJ=OBJNUM(PbSlsOrd)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF

RETURN

*-- End OF lpTab.
*:*************************************************************