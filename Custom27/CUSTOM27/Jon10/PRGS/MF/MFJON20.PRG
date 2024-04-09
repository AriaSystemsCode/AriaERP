*!*************************************************************************
*! PROGRAM   : MFJON200     CUSTOMER : Jonathan Cass
*! AUTHER    : Mohamed Shokry (MHM)
*! Date      : 12/13/2000
*! Purpose   : Custom Contractor Management
*! Reference : *C102090,1
*! Paremeters: None
*! Notes     : 
*!*************************************************************************

lcCutTkt = SPACE(6)
STORE SPACE(0) TO lcStyle,lcColor,lcCont,lcMfg,lcUnit,lcRef,lcStatus,lcoMfg,;
                  lcoUnit,lcoRef,lcTranCD,lcMfgDesc,lcoMfgD,lcColD,lcContD,;
                  lnClrPos,lnClrLen,lcSeparat
                  
*-- To get the None major segment
=lfvNSeg()
STORE {} TO ldIssue,ldReceive, ldCompl,ldoIssue, ldoReceive

STORE 0 TO lnCutQty, lnQty, lnoQty,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,;
           lnQty7,lnQty8,lnTotQty,lnOQty1,lnOQty2,lnOQty3,lnOQty4,lnOQty5,;
           lnOQty6, lnOQty7,lnOQty8,lnCnt
STORE .F. TO llBrowse, llQPerSz , llAsked
lcMode = ''
*--get majorlength
lcMajorPic = gfItemMask('PM')
lnMajorLen = LEN(lcMajorPic)

DIMENSION laScrMode[4]
STORE .F. TO laScrMode
laScrMode[1] = .T.
llAllColors = .T.

=gfOpenFile(gcDataDir+'apVendor','VenCode','SH')  
IF !FILE(gcDataDir+'JON200.DBF')
  =gfModalGen(.F.,.F.,.F.,.F.,"File JON200.DBF doesn't exist, can't proceed.")
  RETURN
ELSE
  =gfOpenFile(gcDataDir+'JON200','JON200','SH')
ENDIF
=gfOpenFile(gcDataDir+'CUTTKTH','CUTTKTH','SH')
=gfOpenFile(gcDataDir+'CutTktL','CutTktL','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'CODES','CODES','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')

*-- Create Care Label Array [Begin]
lnMfg = 1
DIMENSION laMFG [1,2]
STORE "" TO laMFG

DIMENSION laCodInfo [1,10]
laCodInfo[1,01] = "MFGCODE"
laCodInfo[1,02] = "laMfg"
laCodInfo[1,03] = "lnMfg"
laCodInfo[1,04] = ""
laCodInfo[1,05] = .F.            
laCodInfo[1,06] = .F.            
laCodInfo[1,09] = ""
laCodInfo[1,10] = "COPRCODE"
=gfwCodePop ( @laCodInfo, "MFGCODE" ,"L" )

DO (gcScrDir+gcWinAppl+'\MFJON20.SPX')

*********************************************************************
* Function   : lfvCutTkt
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the cuttkt field
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvCutTkt

IF LASTKEY()=QESCKEY
  CLEAR GETS
  RETURN
ENDIF
SELECT CUTTKTH
IF llBrowse OR ATC('?',lcCutTkt) <> 0
  DO CutBrow WITH lcCutTkt,'',"Status $ 'OH'"
  llBrowse = .F.
  IF EMPTY(lcCutTkt)
    _CUROBJ  = _CUROBJ
  ENDIF
ELSE
  IF !EMPTY(lcCutTkt) 
    IF SEEK(lcCutTkt)
      IF  !(CUTTKTH.Status $ 'OH')
        = gfModalGen(.F.,"DIALOG",.f.,.f.,'You may only select open or on hold cutting tickets. ')
        lcCutTkt=SPACE(6)
      ENDIF
    ELSE
      DO CutBrow WITH lcCutTkt,'',"Status $ 'OH'"
      IF EMPTY(lcCutTkt)
        _CUROBJ  = _CUROBJ
      ENDIF
    ENDIF
  ENDIF
ENDIF
IF !EMPTY(lcCutTkt)
  SHOW GET pbIssue  ENABLE
  lcStyle  = CUTTKTH.Style
  lcSeparat=SUBSTR(STYLE.Style,lnMajorLen+1,1)
  ldCompl  = CUTTKTH.Complete
  lnCutQty = CUTTKTH.Pcs_Bud
ELSE
  SHOW GET pbIssue  DISABLE
  lcStyle  = SPACE(12)
  ldCompl  = {}
  lnCutQty = 0
ENDIF
=lfRefresh()

*********************************************************************
* Function   : lfvClose
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Close button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvClose

_CUROBJ = OBJNUM(pbClose)
DO CASE 
  * If Select mode , terminate the program
  CASE laScrMode[1]
    CLEAR READ
  * If view mode, go to select mode
  CASE laScrMode[2]
    STORE .F. TO laScrMode
    laScrMode[1] = .T.  
    =lfShow()
    _CUROBJ=OBJNUM(lcCutTkt)
  * If edit mode , restore old values, then go to view mode
  CASE laScrMode[3]
    IF gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,;
                    "Are you sure? You will lose all changes.") = 1

      lnMfg      = laMFG(lnMfg,1)
      lcUnit     = lcoUnit 
      lnQty      = lnoQty 
      ldIssue    = ldoIssue 
      ldReceive  = ldoReceive
      lcRef      = lcoRef
      *B602033,1 AMM Get the original quantities
      FOR lnCount = 1 TO 8
        lcCount        = STR(lnCount,1)
        lnQty&lcCount  = nQty&lcCount
      ENDFOR
      lnQty = nTotQty
      STORE .F. TO laScrMode
      laScrMode[2] = .T.
      =lfShow()
    ENDIF
  * If add Mode, go to select mode
  CASE laScrMode[4]
    IF gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,;
                  "Are you sure? You will lose all changes.") = 1
      STORE .F. TO laScrMode
      laScrMode[1] = .T.
      =lfShow()
      _CUROBJ=OBJNUM(lcCutTkt)    
    ENDIF
ENDCASE

*********************************************************************
* Function   : lfvIssue
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Issue button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvIssue

IF EMPTY(lcCutTkt) OR laScrMode[4]
  RETURN
ENDIF

* Issued date default is current date
ldIssue   = DATE()
* Receive date default is the cuttkt completion date
ldReceive = CUTTKTH.Complete
* The new record is of type Issue
lcTranCD  = 'I'
lcStatus  = 'Issued'
llAsked = .F.

* Enable all objects to enter the new data
SHOW GET lcCutTkt  DISABLE
SHOW GET ibCutTkt  DISABLE
SHOW GET lcCont    ENABLE
SHOW GET ibCont    ENABLE
SHOW GET lnMfg     ENABLE
SHOW GET lcUnit    ENABLE
SHOW GET lnQty     DISABLE
SHOW GET ldIssue   ENABLE
SHOW GET ldReceive ENABLE
SHOW GET lcRef     ENABLE
SHOW GET lcCont    ENABLE
SHOW GET ibCont    ENABLE
lnChoise   = gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,"Do you want to enter quantities per style/color ?")
* If the user chose not to enter by style/color
IF lnChoise = 2
  lcColor = '******'
  lcColD  = 'All Colors'
  SHOW GET lcColor DISABLE
  SHOW GET ibColor DISABLE
  _CUROBJ = OBJNUM(lcCont)
ELSE
  lcColor = SPACE(6)
  lcColD  = SPACE(15)
  SHOW GET lcColor ENABLE
  SHOW GET ibColor ENABLE
  _CUROBJ = OBJNUM(lcColor)
ENDIF
* Set screen mode to add mode
STORE .F. TO laScrMode
laScrMode[4] = .T.
=lfShow()


*********************************************************************
* Function   : lfvAccept
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Accept button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvAccept

SELECT JON200
* Add Mode
IF laScrMode[4]
  DO CASE
    CASE EMPTY(lcColor)
        = gfModalGen(.F.,.f.,.f.,.f.,'You have to enter a Color, cannot accept.')
      _CUROBJ = OBJNUM(lcColor)
      RETURN
    CASE EMPTY(lcCont)
      = gfModalGen(.F.,.f.,.f.,.f.,'You have to enter a Contractor, cannot accept.')
      _CUROBJ = OBJNUM(lcCont)
      RETURN      
    CASE lcTranCD='I' .AND. lnQty = 0
      = gfModalGen(.F.,.f.,.f.,.f.,'Issued quantity must be greater than zero, cannot accept.')
      IF llQPerSz
        _CUROBJ = OBJNUM(pbQPerSz)
      ELSE
        _CUROBJ = OBJNUM(lnQty)
      ENDIF
      RETURN      
  ENDCASE
  IF lcTranCD='I' 
    *-- If a record of all colors is already issues, display a message
    *-- and return
    IF SEEK(lcCuttkt+SUBSTR(lcStyle,1,lnMajorLen)+'******'+lcCont+laMFG(lnMfg,2)+lcTranCD,'JON200'))
      = gfModalGen(.F.,.f.,.f.,.f.,'All colors are already issued '+;
            		 'for cutting ticket/style '+;
            		  lcCutTkt +'/' + ALLTRIM(SUBSTR(lcStyle,1,lnMajorLen))+; 	    
            		 ', operation ' + ALLTRIM(laMFG(lnMfg,1)) + ;
            		 ' to contractor ' + ALLTRIM(lcCont) +;
            		 '. Cannot issue.')
            		 
      RETURN         		 
    ELSE
      *-- iF lcColor = '*****'
      *-- If a record of any color is already issues, display a message
      *-- and return
	  IF lcColor = '******'
        IF SEEK(lcCuttkt+SUBSTR(lcStyle,1,lnMajorLen))
          LOCATE REST FOR  cContract+cMfg+cTranCD=lcCont+laMFG(lnMfg,2)+lcTranCD
          IF FOUND() 
            = gfModalGen(.F.,.f.,.f.,.f.,'One or more colors are already issued '+;
            			   'for cutting ticket/style '+;
            			    lcCutTkt +'/' + ALLTRIM(SUBSTR(lcStyle,1,lnMajorLen))+; 	    
            			   ', operation ' + ALLTRIM(laMFG(lnMfg,1)) + ;
            			   ' to contractor ' + ALLTRIM(lcCont) +;
            			   '. Cannot issue all colors.')
            			   
            RETURN      
          ENDIF  
        ENDIF   
      ELSE
        *-- iF a record of the selected color already exists, 
        *-- display a message and return
        IF SEEK(lcCuttkt+SUBSTR(lcStyle,1,lnMajorLen)+lcColor+lcCont+laMFG(lnMfg,2)+lcTranCD,'JON200')
           = gfModalGen(.F.,.f.,.f.,.f.,'Cannot issue cutting ticket/style/color '+;
    			   lcCutTkt +'/' + ALLTRIM(SUBSTR(lcStyle,1,lnMajorLen)) + '/' + ;
    			   ALLTRIM(lcColor) + ', operation ' + ;
    			   ALLTRIM(laMFG(lnMfg,1)) + ' to contractor ' + ;
    			   ALLTRIM(lcCont) + ' more than once.')
    			   
          RETURN
        ENDIF  			   
      ENDIF
    ENDIF  
  ENDIF
  * Add a new record
  INSERT INTO JON200 (cCuttkt,cStyle,cColor,cContract,cMfg,cUom,nTotQty,dIssue,;
                      dReceive,cRef,cTranCD,nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,;
                      nQty7,nQty8,lQtyPerSz) ;
  VALUES ;
     (lcCutTkt,SUBSTR(lcStyle,1,lnMajorLen),lcColor,lcCont,laMFG(lnMfg,2),lcUnit,lnQty,ldIssue,ldReceive,;
      lcRef,lcTranCD,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,;
      llQPerSz)

  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  =lfShow()
****** Edit mode ****************
ELSE
  IF lcTranCD='I' .AND. lnQty = 0
    = gfModalGen(INM00000,.f.,.f.,.f.,'Issued quantity must be greater than zero, cannot accept.')
    IF llQPerSz
      _CUROBJ = OBJNUM(pbQPerSz)
    ELSE
      _CUROBJ = OBJNUM(lnQty)
    ENDIF
    RETURN
  ENDIF

  SELECT JON200
  REPLACE cMfg        WITH laMFG(lnMfg,2),;
          cUOM        WITH lcUnit,;
          nTotQty     WITH lnQty,;
          dIssue      WITH ldIssue,;
          dReceive    WITH ldReceive,;
          cRef        WITH lcRef,;
          nQty1       WITH lnQty1,;
          nQty2       WITH lnQty2,;
          nQty3       WITH lnQty3,;
          nQty4       WITH lnQty4,;
          nQty5       WITH lnQty5,;
          nQty6       WITH lnQty6,;
          nQty7       WITH lnQty7,;
          nQty8       WITH lnQty8,;
          lQtyPerSz   WITH llQPerSz
  * After saving, go to view mode
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
  =lfShow()
ENDIF

*********************************************************************
* Function   : lfvModify
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Modify button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvModify
PRIVATE lnCount,lcCount
* Store old values before modifying
lcoMfg     = laMFG(lnMfg,1)
lcoUnit    = lcUnit
ldoIssue   = ldIssue
ldoReceive = ldReceive
lcoRef     = lcRef
* Go to modify mode
STORE .F. TO laScrMode
laScrMode[3] = .T.
=lfSHOW()

*********************************************************************
* Function   : lfvDelete
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Delete button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvDelete
IF gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,"Are you sure you wish to delete this record?")=2
  RETURN
ENDIF
SELECT JON200
* If the record is ISSUE and has received lines, don't delete
IF cTranCD='I'
  lcExpr = lcCutTkt+SUBSTR(lcStyle,1,lnMajorLen)+lcColor+lcCont+laMFG(lnMfg,2)
  IF SEEK(lcExpr+'R')
    = gfModalGen(.f.,.f.,.f.,.f.,'This issue has one or more receivings, Cannot delete it.')
    =SEEK(lcExpr+'I')
    RETURN
  ENDIF
  =SEEK(lcExpr+'I')
ENDIF

DELETE
* Go to Select mode
STORE .F. TO laScrMode
laScrMode[1]=.T.
=lfShow()

*********************************************************************
* Function   : lfvRec
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Receive button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvRec
PRIVATE lnIssue,lcExpr, lnOldQty
lnOldQty = lnQty
SELECT JON200
IF cTranCD='R'
  = gfModalGen(.F.,.f.,.f.,.f.,'This is a received line.')
  RETURN
ENDIF
lnIssue = JON200.nTotQty
lcExpr = lcCutTkt+SUBSTR(lcStyle,1,lnMajorLen)+lcColor+lcCont+laMFG(lnMfg,2)
SUM nTotQty FOR cCutTkt+cStyle+cColor+cContract+cMfg+cTranCD=lcExpr+'R' ;
            TO lnCount
lnQty = lnIssue - lnCount
lnOQty= lnQty
IF lnQty <= 0
  = gfModalGen(.F.,.f.,.f.,.f.,'This Issue is completely received .')
  =SEEK(lcExpr+'I')
  lnQty = lnOldQty
  RETURN
ELSE
  * Set the status to 'RECEIVE'
  lcTranCD = 'R'
  lcStatus = 'Received'
ENDIF

* Go to Add mode
Store .F. TO laScrMode
laScrMode[4] = .T.
=lfShow()
SHOW GET lcRef     ENABLE
SHOW GET ldReceive ENABLE
IF llQPerSz
  SHOW GET pbQPerSz 
  _CUROBJ = OBJNUM(pbQPerSz)
ELSE
  SHOW GET lnQty     ENABLE
  _CUROBJ = OBJNUM(lnQty)
ENDIF
SHOW GET pbReceive DISABLE


*********************************************************************
* Function   : lfvNotes
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Notes button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvNotes

=AriaZoom('Notes', .T.)

*********************************************************************
* Function   : lfvColor
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the color field
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvColor
PRIVATE lnAlias, llMouseDown
lnAlias = SELECT(0)
lcTempCur = gfTempName()
IF llBrowse .OR. !SEEK(lcCutTkt+SUBSTR(lcStyle,1,lnMajorLen)+lcColor,'CutTktL')
  DECLARE laData[1] && array to get values from browse
  STORE '' TO laData
  SELECT DISTINCT Style , color FROM CUTTKTL ;
      WHERE cuttkt+style+color+dyelot+trancd+STR(RECNO(),7) = lcCutTkt;
      INTO DBF (gcWorkDir+lcTempCur) 
  SELECT (lcTempCur)
  LOCATE
  SCAN
    REPLACE COLOR WITH SUBSTR(STYLE,lnClrPos,lnClrLen)
  ENDSCAN    
  lcBrFields = [Style:H="Style",Color:H="Color"]
  =ARIABROW("","Contractor Management",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                   gnBrFSCol2,"","","Color", "laData")
  lcColor=laData[1]
  USE
  llBrowse = .F.
ENDIF
IF !EMPTY(lcColor)
  SELECT CODES
  lcColD = LOOKUP(CODES.cfld_name,'D'+lcColor+ 'N'+'COLOR     ',CODES.cCode_No,'codes')
ELSE
  lcColD = ''
ENDIF
=lfRefresh()
SELECT (lnAlias)


*********************************************************************
* Function   : lfvCont
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the contractor field
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvCont
PRIVATE lnAlias
lnAlias =SELECT(0)

DO CASE
  CASE (llBrowse .OR. (!EMPTY(lcCont) .AND. !SEEK(lcCont,'apVendor') ) )
    = gfAPVnBrow(@lcCont,.T.)
ENDCASE

lcContD = IIF(EMPTY(lcCont), '', ApVendor.cVenComp)
llBrowse = .F.
=lfRefresh()
SELECT (lnAlias)

*********************************************************************
* Function   : lfvQty
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Quantity field
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvQty
PARAMETERS llSzQty
PRIVATE lcExpr, lnCount, lnIssue,lnRecNo

IF !llSzQty .AND. lnQty < 0
  = gfModalGen(.F.,.f.,.f.,.f.,'Quantity cannot be less than zero')
  lnQty = lnOQty
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
lcExpr = lcCutTkt+SUBSTR(lcStyle,1,lnMajorLen)+lcColor+lcCont+laMFG(lnMfg,2)
SELECT JON200
* If the record is RECEIVE, compare issued qty with sum of all received lines
IF lcTranCD = 'R'
  lnRecNo = RECNO()
  IF SEEK(lcExpr+'I')
    lnIssue = JON200.nTotQty
    SUM nTotQty FOR cCutTkt+cStyle+cColor+cContract+cMfg+cTranCD=lcExpr+'R' ;
            TO lnCount
    IF (laScrMode[4] .AND. lnCount+lnQty > lnIssue) .OR. ;
               (laScrMode[3] .AND. lnCount+lnQty-lnOQty > lnIssue)
       = gfModalGen(.F.,.f.,.f.,.f.,'The received quantity cannot be greater than the Issued quantity.')
       _CUROBJ = _CUROBJ
       lnQty = lnOQty
       RETURN .F.
    ENDIF
    IF lnRecNo <= RECCOUNT()
      GOTO (lnRecNo)
    ENDIF
  ENDIF 
ELSE
  IF laScrMode[3]
    lnRecNo = RECNO()
    SUM nTotQty FOR cCutTkt+cStyle+cColor+cContract+cMfg+cTranCD=lcExpr+'R' ;
             TO lnCount  
    =SEEK(lcExpr+'I')
    IF lnQty < lnCount
      = gfModalGen(.F.,.f.,.f.,.f.,'Issued quantity must be greater than total of received quantities. ')
      _CUROBJ = _CUROBJ
      lnQty = lnOQty
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*********************************************************************
* Function   : lfvBrowse
* Auther     : Mohamed Shokry
* Desc       : Valid function of the Browse button
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvBrowse
PRIVATE lcFilt
* If the cuttkt is empty, browse all cuttkt's, else browse this cuttkt only
SELECT JON200
DECLARE laData[4] && array to get values from browse
STORE '' TO laData

lcBrFields = [cCutTkt:H="CutTkt",cStyle:H="Style",cColor:H="Color",;
                cContract:H="Contractor",cMFG:H="MFG",nTotQty:H="Quantity":P="999999",;
                Type=IIF(cTranCD='I','Issued','Received'):H="Type",;
                cUOM:H="UOM",dIssue:H="Issue Date",dReceive:H="Rec. Date",;
                cRef:H="Reference"]

=ARIABROW(IIF(EMPTY(lcCutTkt),'',"FOR JON200.cCutTkt ='" + lcCutTkt+"'"),;
             "Contractor Management",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, ;
                 gnBrFSCol2,"","","cStyle,cColor,cContract,cMfg,cTranCD",;
                 "laData")
IF !EMPTY(laData[2])
  lcCutTkt = cCutTkt
  lcStyle  = cStyle
  lcColor  = cColor
  lcCont   = cContract
  lnMfg = ASUBSCRIPT(laMFG,ASCAN(laMFG,cMfg),1)
  *lnScan = ASCAN(laMfg,cMfg)
  *IF lnScan > 0 
    *lnScan1 = CEILING(lnScan/2)
    *laMFG(lnMfg,1) = laMFG(lnScan1,1)
    *lnMfg = CEILING(lnScan/2)
  *ENDIF
  lcUnit   = cUOM
  lnQty    = nTotQty
  ldIssue  = dIssue
  ldReceive= dReceive
  lcRef    = cRef
  lcTranCD = cTranCD
  FOR lnCount = 1 TO 8
    lcCount        = STR(lnCount,1)
    lnQty&lcCount  = nQty&lcCount
    lnOQty&lcCount = nQty&lcCount
  ENDFOR
  lnOQty = lnQty
  llQPerSz = lQtyPerSz
  lnTotQty = lnQty
  lcStatus = IIF(cTranCD='I','Issued','Received') 
  SELECT CODES
  lcColD   = IIF(lcColor = '******', 'All Colors', ;
  				 LOOKUP(cfld_name,'D'+lcColor+ 'N'+'COLOR     ',CODES.cCode_No,'CODES'))

  IF SEEK(lcCont,'APVENDOR')
    lcContD=ApVendor.cVenComp
  ELSE
    lcContD = SPACE(30)
  ENDIF

  IF SEEK(lcCutTkt,'CUTTKTH')
    ldCompl  = CUTTKTH.Complete
    lnCutQty = CUTTKTH.Pcs_Bud
  ENDIF
  
  SELECT JON200
  STORE .F. TO laScrMode
  laScrMode[2] = .T.
  =lfSHOW()
ELSE
  STORE SPACE(0) TO lcCutTkt,lcStyle,lcColor,lcCont,lcMfg,lcMfgDesc,lcUnit,;
                    lcRef,lcStatus,lcColD,lcContD
  STORE 0 TO  lnQty 
  STORE {} TO ldIssue, ldReceive
  STORE .F. TO laScrMode
  laScrMode[1] = .T.
  =lfSHOW()
ENDIF


*********************************************************************
* Function   : lfShow
* Auther     : Mohamed Shokry
* Desc       : Display the screen objects due to screen mode
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfShow

DO CASE
  * Select Mode
  CASE laScrMode[1]
    * In select mode , initialize all variables
    lcCutTkt = SPACE(6)
    STORE SPACE(0) TO lcStyle,lcColor,lcCont,lcMfg,lcMfgDesc,lcUnit,lcRef,;
                      lcStatus,lcColD,lcContD
    STORE {} TO ldIssue,ldReceive, ldCompl
    STORE 0 TO lnCutQty, lnQty, lnoQty,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,;
               lnQty6,lnQty7,lnQty8,lnTotQty,lnOQty1,lnOQty2,lnOQty3,;
               lnOQty4,lnOQty5, lnOQty6, lnOQty7,lnOQty8
    STORE .F. TO llQPerSz , llAsked
    lcMode = ''
    
    SHOW GET lcCutTkt  ENABLE
    SHOW GET ibCutTkt  ENABLE
    SHOW GET lcColor   DISABLE
    SHOW GET ibColor   DISABLE
    SHOW GET lcCont    DISABLE
    SHOW GET ibCont    DISABLE
    SHOW GET lnMfg     DISABLE
    SHOW GET ibMfg     DISABLE
    SHOW GET lcUnit    DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET ldIssue   DISABLE
    SHOW GET ldReceive DISABLE
    SHOW GET lcRef     DISABLE
    
    SHOW GET pbIssue   DISABLE
    SHOW GET pbAccept  DISABLE
    SHOW GET pbDelete  DISABLE
    SHOW GET pbModify  DISABLE
    SHOW GET pbReceive DISABLE
    SHOW GET pbBrowse  ENABLE
    SHOW GET pbNotes   DISABLE
    SHOW GET pbClose,1 PROMPT '\?\<Close' ENABLE 
    SHOW GET pbQperSz  DISABLE
  * View Mode
  CASE laScrMode[2]
    SHOW GET lcCutTkt  DISABLE
    SHOW GET ibCutTkt  DISABLE
    SHOW GET lcColor   DISABLE
    SHOW GET ibColor   DISABLE
    SHOW GET lcCont    DISABLE
    SHOW GET ibCont    DISABLE
    SHOW GET lnMfg     DISABLE
    SHOW GET ibMfg     DISABLE
    SHOW GET lcUnit    DISABLE
    SHOW GET lnQty     DISABLE
    SHOW GET ldIssue   DISABLE
    SHOW GET ldReceive DISABLE
    SHOW GET lcRef     DISABLE
    
    SHOW GET pbIssue   DISABLE
    SHOW GET pbAccept  DISABLE
    SHOW GET pbDelete  ENABLE
    SHOW GET pbModify  ENABLE
    SHOW GET pbReceive ENABLE
    SHOW GET pbBrowse  ENABLE
    SHOW GET pbNotes   ENABLE
    SHOW GET pbClose,1 PROMPT '\?\<Select' ENABLE 
    IF llQPerSz
      SHOW GET pbQperSz  ENABLE
    ELSE
      SHOW GET pbQperSz  DISABLE    
    ENDIF
  * Edit Mode 
  CASE laScrMode[3]
    SHOW GET lcCutTkt  DISABLE
    SHOW GET ibCutTkt  DISABLE
    SHOW GET lcColor   DISABLE
    SHOW GET ibColor   DISABLE
    SHOW GET lcCont    DISABLE
    SHOW GET ibCont    DISABLE
    * Disable MFG,UOM,dIssue if the record is RECEIVED
    IF lcTranCD = 'I'
      SHOW GET lnMfg     ENABLE
      SHOW GET ibMfg     ENABLE
      SHOW GET lcUnit    ENABLE
      SHOW GET ldIssue   ENABLE
    ELSE
      SHOW GET lnMfg     DISABLE
      SHOW GET ibMfg     DISABLE
      SHOW GET lcUnit    DISABLE
      SHOW GET ldIssue   DISABLE
    ENDIF
    IF llQPerSz
      SHOW GET lnQty     DISABLE
      SHOW GET pbQperSz  ENABLE
    ELSE    
      SHOW GET lnQty     ENABLE
      SHOW GET pbQperSz  DISABLE
    ENDIF
    SHOW GET ldReceive ENABLE
    SHOW GET lcRef     ENABLE
    
    SHOW GET pbIssue   DISABLE
    SHOW GET pbAccept  ENABLE
    SHOW GET pbDelete  DISABLE
    SHOW GET pbModify  DISABLE
    SHOW GET pbReceive DISABLE
    SHOW GET pbBrowse  DISABLE
    SHOW GET pbNotes   ENABLE
    SHOW GET pbClose,1 PROMPT '\?\<Cancel' ENABLE 

    
  * Add Mode
  CASE laScrMode[4]
    SHOW GET pbIssue   DISABLE
    SHOW GET pbAccept  ENABLE
    SHOW GET pbDelete  DISABLE
    SHOW GET pbModify  DISABLE
    SHOW GET pbReceive DISABLE
    SHOW GET pbBrowse  DISABLE
    SHOW GET pbNotes   DISABLE
    SHOW GET pbClose,1 PROMPT '\?\<Cancel' ENABLE 
    SHOW GET pbQperSz  ENABLE
    
ENDCASE
=lfRefresh()


*********************************************************************
* Function   : lfvQPerSz
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of the Quantity per size button.
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************

FUNCTION lfvQPerSz

PRIVATE lnCount,lcCount,laFMode

IF lcTranCD # 'R' .AND. laScrMode[4] .AND. !llAsked
  llQPerSz = .T.
  SHOW GET lnQty    DISABLE
ENDIF
IF !llQPerSz
  = gfModalGen(.F.,.f.,.f.,.f.,'Quantity is not by size, Cannot proceed.')
  RETURN
ENDIF
DO CASE
  CASE laScrMode[1]
    RETURN
  CASE laScrMode[2] OR laScrMode[3]
    lcMode=IIF(laScrMode[2],'DISABLE','ENABLE')
    FOR lnCount = 1 TO 8
      lcCount        = STR(lnCount,1)
      lnOQty&lcCount = lnQty&lcCount
    ENDFOR
    lnOQty   = lnQty
    lnTotQty = lnQty
  CASE laScrMode[4]
    IF !llAsked .AND. lcTranCD # 'R'
      STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lQty6,lnQty7,lnQty8,lnTotQty
      llAsked = .T.  
    ELSE
      FOR lnCount = 1 TO 8
        lcCount        = STR(lnCount,1)
        lnOQty&lcCount = lnQty&lcCount
      ENDFOR
      lnOQty   = lnQty
      lnTotQty = lnQty
    ENDIF
    
    lcMode = 'ENABLE'
ENDCASE
IF SEEK(SUBSTR(lcStyle,1,lnMajorLen ) +lcSeparat+ IIF(lcColor='******','',lcColor),'STYLE') .AND. SEEK('S'+Style.Scale,'SCALE')
  lnCnt = Scale.Cnt
ELSE
  lnCnt = 0
ENDIF
DIMENSION laFMode[8]
STORE 'DISABLE' TO laFMode
FOR lnCount = 1 TO lnCnt
  laFMode[lnCount] = IIF(lcMode='ENABLE','ENABLE','DISABLE')
ENDFOR
DO (gcScrDir+gcWinAppl+'\MFJON20A.SPX')
SHOW GET lnQty
*********************************************************************
* Function   : lfvSzQty
* Auther     : Mohamed Shokry (MHM)
* Desc       : Valid function of all size quantity fields in the screen.
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvSzQty

lcCurQty=VARREAD()
IF EVAL(VARREAD()) < 0
  = gfModalGen(.F.,.f.,.f.,.f.,'Quantity cannot be less than zero')
  &lcCurQty = EVAL('lnOQty'+RIGHT(VARREAD(),1))
  _CUROBJ = _CUROBJ
ENDIF
lnTotQty = lnQty1+lnQty2+lnQty3+lnQty4+lnQty5+lnQty6+lnQty7+lnQty8
=lfRefresh()


*********************************************************************
* Function   : lfvOk
* Auther     : Mohamed Shokry
* Desc       : Valid function of Ok button.
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvOk
IF laScrMode[2]
  CLEAR READ
ELSE
  lnQty = lnTotQty
  IF lfvQty(.T.)
    CLEAR READ
  ENDIF
ENDIF

*********************************************************************
* Function   : lfvCancel
* Auther     : Mohamed Shokry
* Desc       : Valid function of Cancel button.
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************

FUNCTION lfvCancel

IF !laScrMode[2]
  PRIVATE lnCount,lcCount
  FOR lnCount = 1 TO 8
    lcCount        = STR(lnCount,1)
    lnQty&lcCount = lnOQty&lcCount
  ENDFOR
  lnQty = lnOQty
  lnTotQty = lnoQty
ENDIF
CLEAR READ

*********************************************************************
* Function   : lfvUnit
* Auther     : Mohamed Shjokry
* Desc       : Valid function of Unit field.
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvUnit
IF laScrMode[4] .AND. !llAsked
  IF gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,"Do you want to enter quantities by size?") = 1
    llQPerSz = .T.
    SHOW GET lnQty    DISABLE
    SHOW GET pbQPerSz ENABLE
    =lfvQPerSz()
  ELSE
    SHOW GET lnQty    ENABLE
    SHOW GET pbQPerSz DISABLE
    _CurObj = OBJNUM(lnQty)
    llQPerSz = .F.  
  ENDIF
  llAsked = .T.
ENDIF
*********************************************************************
* Function   : lfvNSeg
* Auther     : Mohamed Shokry
* Desc       : Get nonmajor seg. (color)
* Date       : 12/17/2000
* Ref        : *C102090,1
*********************************************************************
FUNCTION lfvNSeg
*--Get NonMajor 
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
