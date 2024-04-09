*!**************************************************************************
*! Name      : RFSMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/18/2002
*! Purpose   : RFS Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C200253,1
*!**************************************************************************
*! Modifications:
*! B606347,1 ADEL 08/06/02 Make some enahance.
*!**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.



*!**************************************************************************
*! Name      : lfDoAll
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Do every thing accordiling to the sttinges allowed.m
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfDoAll

PRIVATE lcOldSty,lcOldColor,lnOldTot,lnOldQty1,lnOldQty2,lnOldQty3,lnOldQty4
PRIVATE lnOldQty5,lnOldQty6,lnOldQty7,lnOldQty8
STORE '' TO lcOldSty,lcOldColor
STORE 0  TO lnOldTot,lnOldQty1,lnOldQty2,lnOldQty3,lnOldQty4,lnOldQty5,lnOldQty6,lnOldQty7,lnOldQty8,;
            M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8
        

DO CASE
  CASE TYPE('llAddBar') = 'L' AND llAddBar
    *--Add 'Add S/O lines' bar to Option menu.
    DEFINE BAR 17 OF _INQURYPOP PROMPT 'Add S/O lines' SKIP FOR (lnActFolder<>2) ;
           OR EMPTY(laData[2]) OR llMultiSt  KEY 'F8'    
    lcMjrTtl  = gfItemMask('HM')           
    lcMajP    = gfItemMask('PM')
    lnItemSzs = gfItemMask('PI')
  CASE TYPE('llFunBar ') = 'L' AND llFunBar
    lnstylewid= LEN(lcMajP)
    lcSepart  = SUBSTR(lnItemSzs,lnstylewid+1,1)
    
    IF USED(lcTmpOrdn) AND RECCOUNT(lcTmpOrdn) <>0 
      SELECT (lcTmpOrdn)
      ZAP
    ENDIF  
    m.Gros_Price = 0
    SELECT IIF((laScrMode[3] .OR. laScrMode[4]),lcOrdLine,'OrdLine')
    lcWhile = ""
    IF (laScrMode[3] .OR. laScrMode[4])
      lcWhile = ""
    ELSE
      =SEEK(lcOrdType+laData[1])
      lcWhile = "WHILE cordtype+order+STR(lineno,6) = lcOrdType+laData[1]"
    ENDIF
    SCAN &lcWhile
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpOrdn) FROM MEMVAR
      REPLACE &lcTmpOrdn..Sty    WITH SUBSTR(Style,1,lnMajorLen),;
              &lcTmpOrdn..Color  WITH SUBSTR(Style,lnMajorLen+2),;
              &lcTmpOrdn..CurPos WITH '0',;
              &lcTmpOrdn..Amount WITH &lcTmpOrdn..TotQty*&lcTmpOrdn..Price
    ENDSCAN
    SELECT (lcTmpOrdn)
    GO BOTT
    lnLastRec =  RECNO()
    LOCATE
    STORE " " TO lcQty1Ttl,lcQty2Ttl,lcQty3Ttl,lcQty4Ttl,lcQty5Ttl,lcQty6Ttl,lcQty7Ttl,lcQty8Ttl
    IF EOF() AND (laScrMode[4] OR laScrMode[3])
      APPEND BLANK
      REPLACE CURPOS WITH  "1",;
              STORE  WITH laData[3]
      lnLastRec =  RECNO()
      lladdline = .T.
    ENDIF
    *--Clear Traps
    lcOldW    = ON("KEY","CTRL+W")
    =lfClearKey()
    lcOldEsc = ON("KEY","ESC")
    ON KEY LABEL ESC lnDumm = 1
    lcOldDown = ON("KEY","DNARROW")
    lcOldD    = ON("KEY","CTRL+D")
    ON KEY LABEL CTRL+W lnDummy = 1
    *--Set New Traps
    DO lpTrap
    llFirstt = .T.
    lnOldPrice = 0
    
    DO (gcScrDir+gcWinAppl+"\SORFSOR.SPX")
    ON KEY LABEL ESC &lcOldEsc
    ON KEY LABEL CTRL+D &lcOldD
    ON KEY LABEL DNARROW &lcOldDown
    ON KEY LABEL CTRL+W &lcOldW
    =lfDOrdScr()
    
  CASE TYPE('llCrtTmp') = 'L' AND llCrtTmp AND !USED(lcTmpOrdn)
    *--Create the new custom order line screen temp file.
    DO lpCrtTmp

    
ENDCASE

*!**************************************************************************
*! Name      : lpCrtTmp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Creat a new temp file.
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
PROCEDURE lpCrtTmp


SELECT ORDLINE
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+4,4]
laFileStru[lnFileStru+1,1] = 'CurPos'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'Sty'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 12
laFileStru[lnFileStru+2,4] = 0

laFileStru[lnFileStru+3,1] = 'Color'
laFileStru[lnFileStru+3,2] = 'C'
laFileStru[lnFileStru+3,3] = 6
laFileStru[lnFileStru+3,4] = 0

laFileStru[lnFileStru+4,1] = 'Amount'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 12
laFileStru[lnFileStru+4,4] = 2

DECLARE laIndex[2,2]
laIndex[1,1] = 'CurPos+cOrdType+ORDER+STORE+STYLE+STR(LINENO,6)'
laIndex[1,2] = 'ORDLINST'
laIndex[2,1] = 'CurPos+cOrdType+ORDER+STR(LINENO,6)'
laIndex[2,2] = 'ORDLINE'
=gfCrtTmp(lcTmpOrdn,@laFileStru,@laIndex)
IF USED(lcTmpOrdn)
  USE IN (lcTmpOrdn)
ENDIF
USE (gcWorkDir+lcTmpOrdn) EXCL IN 0

SET ORDER TO TAG 'ORDLINE' IN (lcTmpOrdn)



*!**************************************************************************
*! Name      : lfBrowOrdL
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Browse lines
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfBrowOrdL

llZedSize = SEEK('SZ','SCALE')
IF SEEK('SZ','SCALE')
  lcQty1Ttl = Scale.Sz1
  lcQty2Ttl = Scale.Sz2
  lcQty3Ttl = Scale.Sz3
  lcQty4Ttl = Scale.Sz4
  lcQty5Ttl = Scale.Sz5
  lcQty6Ttl = Scale.Sz6
  lcQty7Ttl = Scale.Sz7
  lcQty8Ttl = Scale.Sz8
ELSE
  lcQty1Ttl = "Qty1"
  lcQty2Ttl = "Qty2"
  lcQty3Ttl = "Qty3"
  lcQty4Ttl = "Qty4"
  lcQty5Ttl = "Qty5"
  lcQty6Ttl = "Qty6"
  lcQty7Ttl = "Qty7"
  lcQty8Ttl = "Qty8"
ENDIF
*B606347,4 SSH [Start] Fix syntax error.
*lcFields = [cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+;
           [Sty   :H=lcMjrTtl :P = "!!!!!!!!!!!!" :v=lfvMajor() :W=lfStyWhen(),]+;
           [Color :H=lcNMjrTl :P = "!!!!!!" :V=lfvNonMjr() :W=lfColWhen(),Desc1 :15 :H='Description':R),]+;
           [TotQty:H='TotQty' :W=  lfTotWhen() :V=lfvPiec(),Qty1 :H=lcQty1Ttl :W= lfwQty(1) :V=lfvQty(1) :P = "99999",]+;
           [Qty2  :H=lcQty2Ttl   :W= lfwQty(2) :V=lfvQty(2):P = "99999",Qty3 :H=lcQty3Ttl :W= lfwQty(3) :V=lfvQty(3):P = "99999",]+;
           [Qty4  :H=lcQty4Ttl   :W= lfwQty(4):V=lfvQty(4):P = "99999",]+;
           [Qty5  :H=lcQty5Ttl   :W= lfwQty(5) :V=lfvQty(5):P = "99999",Qty6 :H=lcQty6Ttl :W= lfwQty(6) :V=lfvQty(6):P = "99999",]+;
           [Qty7  :H=lcQty7Ttl   :W= lfwQty(7) :V=lfvQty(7):P = "99999",Qty8 :H=lcQty8Ttl :W= lfwQty(8) :V=lfvQty(8):P = "99999",]+;
           [Price :H='Price'  :W= !EMPTY(STY) AND !EMPTY(Color) AND lfwGPrice() .AND. llESTYPSO .AND. lfOldvalu() :V= lfvGPric() AND lfvQty(9) :P = "99999999.99" ,Amount :H='Amount' :R]

lcFields = [cMarker =IIF(RECNO()=lnMarker,'>',' '):H=' ':R:1:W=.F.,]+;
           [Sty   :H=lcMjrTtl :P = "!!!!!!!!!!!!" :v=lfvMajor() :W=lfStyWhen(),]+;
           [Color :H=lcNMjrTl :P = "!!!!!!" :V=lfvNonMjr() :W=lfColWhen(),Desc1 :15 :H='Description':R,]+;
           [TotQty:H='TotQty' :W=  lfTotWhen() :V=lfvPiec(),Qty1 :H=lcQty1Ttl :W= lfwQty(1) :V=lfvQty(1) :P = "99999",]+;
           [Qty2  :H=lcQty2Ttl   :W= lfwQty(2) :V=lfvQty(2):P = "99999",Qty3 :H=lcQty3Ttl :W= lfwQty(3) :V=lfvQty(3):P = "99999",]+;
           [Qty4  :H=lcQty4Ttl   :W= lfwQty(4):V=lfvQty(4):P = "99999",]+;
           [Qty5  :H=lcQty5Ttl   :W= lfwQty(5) :V=lfvQty(5):P = "99999",Qty6 :H=lcQty6Ttl :W= lfwQty(6) :V=lfvQty(6):P = "99999",]+;
           [Qty7  :H=lcQty7Ttl   :W= lfwQty(7) :V=lfvQty(7):P = "99999",Qty8 :H=lcQty8Ttl :W= lfwQty(8) :V=lfvQty(8):P = "99999",]+;
           [Price :H='Price'  :W= !EMPTY(STY) AND !EMPTY(Color) AND lfwGPrice() .AND. llESTYPSO .AND. lfOldvalu() :V= lfvGPric() AND lfvQty(9) :P = "99999999.99" ,Amount :H='Amount' :R]

*B606347,4 SSH [END] Fix syntax error.
SELECT (lcTmpOrdn)
LOCATE
lnMarker = RECNO()
IF (laScrMode[3] .OR. laScrMode[4])
  BROWSE FIELDS &lcFields ;
         FOR !DELETED()   ;
         WINDOW SORFSOR1  ;
         IN WINDOW SORFSOR;
         NOMENU           ;         
         NOWAIT           ;
         SAVE             ;
	     NOCLEAR          ;
         WHEN lfWLineS()  ;
         TITLE lcOrdWin
ELSE
  BROWSE FIELDS &lcFields ;
         FOR !DELETED()   ;
         WINDOW SORFSOR1  ;
         IN WINDOW SORFSOR;
         NOMENU           ;         
         NOAPPEND         ;
         NODELETE         ;         
         NOWAIT           ;
         NOEDIT           ;
         SAVE             ;
         NOCLEAR          ;
         WHEN lfWLineS()  ;
         TITLE lcOrdWin
ENDIF         

*!**************************************************************************
*! Name      : lfWLineS
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When Browse function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfWLineS

IF  RECNO()  = lnLastRec
  ON KEY LABEL DNARROW  DO lpAddItem
ELSE
  ON KEY LABEL DNARROW
ENDIF 
ON KEY LABEL TAB 
m.Gros_Price = Gros_Price
IF lcLastStyle = lcEmptySty  AND SEEK (STYLE,'STYLE')
  SKIP IN STYLE
  lcLastStyle = STYLE.STYLE
ENDIF



lnMarker = RECNO(lcTmpOrdn)
SHOW WINDOW (lcOrdWin) REFRESH SAME

PROCEDURE lpAddItem

IF WONTOP () = lcOrdWin
  lnRec = RECNO()
  IF RECNO() = lnLastRec AND !SEEK("1") AND (laScrMode[3] .OR. laScrMode[4])
    GO IIF(BETWEEN(lnRec,1,RECCOUNT()),lnRec,RECNO())
    =SEEK(STYLE,'STYLE')

    IF lcLastStyle = lcEmptySty  AND !EOF('STYLE')
      SKIP IN STYLE
      lcLastStyle = STYLE.STYLE
    ENDIF
    IF !EMPTY(Sty) .AND. !EMPTY(Color)
      APPEND BLANK
      *B606347,4 SSH [Start] Do not default the style with the next one.
      *REPLACE Sty    WITH SUBSTR(lcLastStyle,1,lnMajorLen),;
              Color  WITH SUBSTR(lcLastStyle,lnMajorLen+2),;
              Store  WITH laData[3],;
              CurPos WITH '0',;
              Amount WITH TotQty*Price

      REPLACE Sty    WITH "",;
              Color  WITH "",;
              Store  WITH laData[3],;
              CurPos WITH '0',;
              Amount WITH TotQty*Price
      *B606347,4 SSH [END]
      IF SEEK(Sty+lcSepart+Color,'Style')
         *B606347,4 SSH [Start] Commented out to not default the style with the next one.
         *REPLACE  Style WITH STYLE.STYLE,;
                  Desc1 WITH Style.Desc1
         *B606347,4 SSH [END].
      ENDIF
      lnLastRec =  RECNO()
      lladdline = .T.
      *=lfvNonMjr()
      *B606347,1 (Begin) Always stand at the style field
      =lfBrowOrdL()
      GO IIF(BETWEEN(lnLastRec,1,RECCOUNT()),lnLastRec,RECNO())
      *B606347,1 (End)
    ENDIF
  ENDIF
ENDIF  


PROCEDURE lpTrap

IF laScrMode[3] OR laScrMode[4]
  ON KEY LABEL CTRL+D DO lpDelete
ENDIF  
ON KEY LABEL DNARROW  DO lpAddItem
IF WONTOP () <> lcOrdWin
  ON KEY LABEL TAB ACTIVATE WINDOW (lcOrdWin)
ELSE
  ON KEY LABEL TAB 
ENDIF


PROCEDURE  lpffDelete

DELETE 
=lfBrowOrdL()


*!**************************************************************************
*! Name      : lfvMajor
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Style validation
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvMajor


lcOrd = ORDER('Style')
SET ORDER TO CSTYLE IN STYLE

IF (!EMPTY(Sty) AND !SEEK(Sty,'STYLE')) OR lfNotFound()
  lcMajor = gfStyBrw('M',"","",.F.)
  lladdline = .T.  
  IF !SEEK(lcMajor,'STYLE') .AND. !EMPTY(lcMajor)
    lcMajor = gfStyBrw('M',lcMajor,lcNonMjr,.T.)
  ENDIF
  IF EMPTY(lcMajor)
    IF EMPTY(lcOldSty)
      SCATTER MEMVAR BLANK
      M.CurPos = '1'
      m.Store = laData[3]
      GATHER MEMVAR
    ELSE
      REPLACE STY WITH lcOldSty
    ENDIF  
  ELSE
    laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,TotQty,0)
    laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,TotQty*Price,0)
    laData[41] = laData[41] - TotQty
    laData[42] = laData[42] - TotQty*Price
    
    SCATTER MEMVAR BLANK
    m.Store = laData[3]
    GATHER MEMVAR
    REPLACE STY    WITH lcMajor;
            CURPOS WITH  "1"
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    SELECT (lcTmpOrdn)
  ENDIF
*B606347,1 (Begin) If the new style entered is an existing one for the old color, repplace the new info
ELSE
  IF !EMPTY(COLOR)  
     REPLACE  Style WITH STYLE.STYLE,;
              Desc1 WITH Style.Desc1
    =SEEK(STYLE,'STYLE')
    IF !EOF('STYLE')
      SKIP IN STYLE
      lcLastStyle=STYLE.Style
    ELSE
       STORE lcEmptySty TO lcLastStyle   
    ENDIF  
         
  ENDIF
*B606347,1 (End)  
ENDIF
SET ORDER TO &lcOrd IN STYLE


*!**************************************************************************
*! Name      : lfStyWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When Style function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfStyWhen

lcOldSty = Sty
IF !EMPTY(STY) AND !EMPTY(COLOR) AND !EMPTY(STYLE) AND TOTQTY <> 0
  RETURN .F.
ENDIF


*!**************************************************************************
*! Name      : lfTotWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When TotQty function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfTotWhen


ON KEY LABEL DNARROW

lnOldTot = TotQty
lnOldQty1= Qty1
lnOldQty2= Qty2
lnOldQty3= Qty3
lnOldQty4= Qty4
lnOldQty5= Qty5
lnOldQty6= Qty6
lnOldQty7= Qty7
lnOldQty8= Qty8

RETURN !EMPTY(STY) AND !EMPTY(Color) AND SEEK(STYLE,'STYLE') AND !EMPTY(Style.PrePak)



*!**************************************************************************
*! Name      : lfvNonMjr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Color validation
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvNonMjr
PARAMETERS llStores


llFound = .F.
IF SEEK(Sty+lcSepart+Color,'Style') AND TOTQTY <> 0
   IF COLOR <> lcOldColor
     lnLine    = LineNo
     lcStyle   = Sty+lcSepart+Color
     lcOrdType = cOrdType
     lcOrder   = Order
     lnOldPrice = price
     *SCAT MEMVAR BLANK
     M.Sty    = Sty
     m.Color  = Color
     m.Store  = laData[3]
     m.LineNo = lnLine
     m.Style  = lcStyle
     m.Order  = lcOrder
     m.cOrdType = lcOrdType
     GATHER MEMVAR
     REPLACE  Style WITH STYLE.STYLE,;
              Desc1 WITH Style.Desc1
  ENDIF            
  llFound = .T.
ELSE
  STORE '' TO lcMajor,lcNonMjr
  IF (!EMPTY(Color) OR !EMPTY(lcOldColor)) AND !SEEK(Sty+lcSepart+Color,'Style')
    IF !EMPTY(Sty)
      lcMajor  = Sty
      lcNonMjr = gfStyBrw('N',Sty,IIF(!EMPTY(Color),Color,lcOldColor),.F.)
    ELSE
      lcItemCd = gfStyBrw('I',Sty+lcSepart+Color,'',.F.)
      lcMajor  = SUBSTR(lcItemCd ,1,lnstylewid)
      lcNonMjr = SUBSTR(lcItemCd ,lnstylewid+2)
    ENDIF
    IF !EMPTY(lcNonMjr)
       REPLACE Sty   WITH lcMajor,;
               Color WITH lcNonMjr
    ELSE
      REPLACE Color WITH  lcOldColor
    ENDIF
    IF SEEK(Sty+lcSepart+Color,'Style')
       REPLACE  Style WITH STYLE.STYLE,;
                Desc1 WITH Style.Desc1
    ENDIF
  ELSE
    REPLACE  Style WITH STYLE.STYLE,;
             Desc1 WITH Style.Desc1
    llAddLine = .T.
  ENDIF
  IF Sty = lcOldSty AND Color  = lcOldColor AND LINENO <> 0
    REPLACE AMOUNT WITH TOTQTY*PRICE
    RETURN
  ELSE
    SCATT MEMVAR BLANK  
  ENDIF
ENDIF  
lnRecLNo  = RECNO()
*--Prevent the user changing or deleteing style has allocated Or shipped quantities
IF laScrMode [3] .AND. !lladdline .AND. Style <> m.Style 
  DO CASE
    CASE PICKED 
     *-- Message : 32079
     *-- Order line already XXXX ; Cannot modify style.
     *-- Button  : 00000
     *-- Ok
     =gfModalGen('INM32079B00000','ALERT','piked')
     STORE Style TO m.Style
     _CUROBJ = OBJNUM(m.Style)
     llBrowse = .F.  
     RETURN

    CASE TOtQty <> Totbook
      =gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
      SELECT IIF(llStores,lcTempline,lcOrdLine)
      IF !SEEK('O'+Order+STR(lineno,6),'Ordcanln') .AND. Totbook <> Ordcanln.TotQty
        *-- Message : 32079
        *-- Order line already XXXX ; Cannot modify style.
        *-- Button  : 00000
        *-- Ok
        =gfModalGen('INM32079B00000','ALERT','shiped')
        =gfCloseFile('ORDCANLN')
        STORE Style TO m.Style
        _CUROBJ = OBJNUM(m.Style)
        llBrowse = .F.  
        RETURN
     ENDIF
  ENDCASE
  
ENDIF

IF SEEK(Style,'Style')
  *-- Message : 32017
  *-- Style scale not found in the scale file. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32018
  *-- This is a canceled style. Cannot accept.
  *-- Button : 00000 
  *-- Ok

  *-- Message : 32019
  *-- This style/color is on hold. 
  *-- Button : 32003
  *-- Accept Reenter

  *-- Message : 32020
  *-- Styles restricted to XXX!
  *-- Button : 00000
  *-- Ok

  *-- Message : 32021
  *-- Style XXX date is 
  *-- Button : 32003
  *-- Accept Reenter
  SET RELATION OFF INTO STYLE
  IF (!SEEK('S'+Style.Scale,'Scale') .AND. ;
      gfModalGen('TRM32017B00000','ALERT')=1) ;
   .OR. (Style.Status='X' .AND. ;
        gfModalGen('TRM32018B00000','ALERT')=1) ;
   .OR. (Style.Status='H' .AND. ;
        gfModalGen('QRM32019B32003','ALERT')=2) ;
   .OR. (Style.cDivision <> laData[15] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','division '+ALLTRIM(laDivision[lnDivision,1]))=1) ;
   .OR. IIF(ASCAN(laEvntTrig , PADR('STYLEVALID',10)) <> 0 AND laSetups[15,2] = .F.,.F.,(ALLTRIM(laData[14])<>'*' AND TRIM(Style.Season)<>'Y' AND Style.Season<>laData[14] .AND. ;
        gfModalGen('TRM32020B00000','ALERT','season '+ALLTRIM(laSeasons[lnSeason,1]))=1)) ;
   .OR. (!EMPTY(Style.Start) .AND. Style.Start > laData[9] .AND. ;
        gfModalGen('QRM32021B32003','ALERT','start|'+DTOC(Style.Start))=2) ;
   .OR. (!EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2)  
    SET RELATION TO STYLE INTO STYLE ADDITIVE

    SCATTER MEMVAR BLANK
    m.Store = laData[3]
    GO IIF(BETWEEN(lnRecLNo,1,RECCOUNT()),lnRecLNo,RECNO())
    GATHER MEMVAR
    REPLACE CurPos WITH '1'
    RETURN
  ENDIF
  m.Style = Style
  m.lContract = lcOrdType<>'C' .AND. lfvContPri(m.Style,'m.Gros_Price','m.Price','m.Disc_Pcnt')
  IF !m.lContract
    *--Select alternative price level
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
  ENDIF
  
  SELECT (lcTmpOrdn)
  IF m.Gros_Price < 0
    SCATTER MEMVAR BLANK
    m.Store = laData[3]    
    GATHER MEMVAR
    SET RELATION TO STYLE INTO STYLE ADDITIVE
    RETURN
  ENDIF

  IF laSetups[5,2]='Y' .AND. !SEEK(m.Style+laData[31]+SPACE(10),'StyDye')
    *-- Message : 40012
    *-- Style/color xxx is not assigned to warehouse xxx
    *-- Button : 40002
    *-- Add Reenter
    IF gfModalGen('QRM40012B40002','ALERT',TRIM(m.Style)+'|'+TRIM(laData[31]))=1
      DO gpAdStyWar WITH m.Style,SPACE(10),laData[31]
    ELSE
      SCATTER MEMVAR BLANK
      m.Store = laData[3]      
      GATHER MEMVAR
      SET RELATION TO STYLE INTO STYLE ADDITIVE
      RETURN
    ENDIF
  ENDIF
  m.Desc1    = Style.Desc1
  m.Scale    = Style.Scale
  m.PrePak   = Style.PrePak
  m.Comm1    = IIF(Style.Commission,laData[28],0)
  m.Comm2    = IIF(Style.Commission,laData[30],0) 
  m.Season   = Style.Season

  *-- Save Cust PO # in OrdLine if not Multi Store. 
  IF !laData[7]
    m.CustPo   = &lcOrdHdr..CustPo
  ENDIF  

  m.Gl_Sales = ALLTRIM(laData[53])+Style.cSlsGlLink
  m.Gl_Sales = IIF(laSetups[4,2]='Y' AND SEEK('02'+m.Gl_Sales,'Gl_Link'),m.Gl_Sales,'DEFDEF')
  m.Flag     = IIF(llStores,'',IIF(llAddLine OR m.Flag='N','N','M'))
  *--Get Style discount percent and Calculate net price
  IF !m.lContract
    *-- Add new validation on the discount code up on 
    *--add new option about whole sale or retail sale
    *-- get the cDiscCode From stydye in every case
    lcDiscCode  = IIF(SEEK(m.Style+laData[31]+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
    m.Disc_Pcnt = 0 
    IF !EMPTY(ALLTRIM(lcDiscCode))
      *-- Get the disecound related filed to now which 
      *-- type whole Sale Or Retail sale Or Both.
      DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
      STORE '' To lcDisType , ldstartDte ,ldEndDate
      *-- Array to get the Discount affect for DecCode.
      laDisType[1,1]  = 'CCOSTAFECT'
      laDisType[1,2]  = 'lcDisType'
      *-- Array to get the start date For DescCode.
      lastartDte[1,1] = 'START'
      lastartDte[1,2] = 'ldstartDte'
      *-- Array to get the end date For DescCode.
      laEndDate[1,1]  = 'DENDATE'
      laEndDate[1,2]  = 'ldEndDate'
      = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
      = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
      = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
      lnDisc_Pcnt = 0
      IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(laData[8],ldstartDte,ldEndDate)
        lnDisc_Pcnt = m.Disc_Pcnt
        =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
        m.Disc_Pcnt = lnDisc_Pcnt
      ENDIF
    ENDIF  
    m.Price     = m.Gros_Price*(100-m.Disc_Pcnt)/100  
  ENDIF
  IF !EOF('STYLE')
    SKIP IN STYLE
    lcLastStyle=STYLE.Style
  ELSE
     STORE lcEmptySty TO lcLastStyle   
  ENDIF  

  SELECT (lcTmpOrdn)
  lnRec = RECNO()
  SET ORDER TO TAG ORDLINST
  IF SEEK("0"+lcOrdType+laData[1]+m.Store+m.Style) AND  RECNO() <> lnRec
    *-- Message : 32029
    *-- This style/color had been entered on this order
    *-- Button : 00000
    *-- Ok
    =gfModalGen('INM32029B00000','ALERT',IIF(lcOrdType='C','contract','order'))
  ENDIF
  SET ORDER TO TAG ORDLINE
  =SEEK(lcOrdType+laData[1]+STR(m.LineNo,6))
  GO IIF(BETWEEN(lnRec,1,RECCOUNT()),lnRec,RECNO())  
  IF llAddLine
    *--if bulk order we get line N# from tempfile [Start]
    *SCATT MEMVAR BLANK
    m.Style = Style
    m.Desc1 = Desc1
    IF llFromBulk
      GO BOTT
      m.LineNo    = &lcTmpOrdn..LineNo + 1
      *--Save last line no. [Begin]
      lnLines     = m.LineNo
    ELSE
      lnLines     = lnLines + 1
      m.LineNo    = lnLines
    ENDIF
    m.Account   = laData[2]
    m.Order     = laData[1]
    m.cOrdType  = lcOrdType
    m.cWareCode = laData[31]
    *-- Default complete date .
    m.Complete  = laData[10]
    *--The bullk order doesn't get delpeted upon entering the conf.[start]
  ELSE
    *-- Update order totals properly
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      STORE IIF(lnCount > Scale.cnt,0,Qty&lcCount)  TO m.Qty&lcCount
      STORE IIF(lnCount > Scale.cnt,0,Book&lcCount) TO m.Book&lcCount
    ENDFOR
    m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
    m.TotBook= m.Book1+m.Book2+m.Book3+m.Book4+m.Book5+m.Book6+m.Book7+m.Book8
    IF !EMPTY(m.Prepak) AND SEEK('P'+m.Scale+m.Prepak,'Scale') AND ;
       MOD(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,Scale.PPTot) <> 0 
      m.Prepak=''
      m.PpQty = 0 
    ENDIF
    =SEEK('S'+m.Scale,'Scale')
    lcOldValue = PRICE
    =lfvNPr(llStores) AND lfChkTotQ(llStores)
  ENDIF
  *--ADEL REVIEW Call function to add adarnment to new line
  =lfAdrLine()

  SELECT (lcTmpOrdn)
  GO IIF(BETWEEN(lnRecLNo,1,RECCOUNT()),lnRecLNo,RECNO())
  =RLOCK()
  m.CurPos = '0'
  m.cOrdType = lcOrdType
  lcColor = Color
  lcSty   = Sty
  IF !llFound
    STORE 0  TO M.QTY1,M.QTY2,M.QTY3,M.QTY4,M.QTY5,M.QTY6,M.QTY7,M.QTY8,M.TOTQTY,m.Amount
  ENDIF  
  m.Store = laData[3]
  GATHER MEMVAR
  REPLACE Sty  WITH lcSty,;
          COLOR WITH lcColor
  UNLOCK
  llAddLine  = .F.
  llCUpdate  = .T.
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  IF lContract .OR. !llESTYPSO
    SHOW GET m.Price      DISABLE
    SHOW GET m.Gros_Price DISABLE
    SHOW GET m.Disc_Pcnt  DISABLE
  ENDIF
ENDIF
=SEEK('S'+STYLE.SCALE,'SCALE')


*!**************************************************************************
*! Name      : lfColWhen
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When Color validation
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfColWhen


lcOldColor = Color
IF !EMPTY(STY) AND !EMPTY(COLOR) AND !EMPTY(STYLE) AND TOTQTY <> 0
  RETURN .F.
ENDIF


FUNCTION ChkSyCol

RETURN (!EMPTY(STY) AND !EMPTY(Color))



PROCEDURE lpDelete
PARAMETERS llStores

llOneRec =  .F.
lnGoRec = RECNO()
COUNT TO lnTotRec
IF lnTotRec = 1
  LOCATE
  IF EMPTY(STYLE) AND EMPTY(Color)
    RETURN
  ELSE
    llOneRec =  .T.
  ENDIF  
ELSE
  GO IIF(BETWEEN(lnGoRec,1,RECCOUNT()),lnGoRec,RECNO())    
ENDIF
IF EMPTY(STYLE) AND EMPTY(Color)
  RETURN
ENDIF

IF Picked AND !EMPTY(PikTkt)
  *-- Message : 32056
  *-- This XXXXX has been picked. To remove first release the pick ticket.
  *-- Button : 00000 
  *-- Ok
  =gfModalGen('TRM32056B00000','ALERT',lcStyHdr)
  RETURN
ENDIF

*-- Message : 32035
*-- Are you sure you want to remove this order line?
*-- Button : 32000
*-- Yes No
IF gfModalGen('QRM32035B32000','ALERT',IIF(lcOrdType='C','contract','order'))= 1

  *-- Confirm removing if order line has quantity allocated.
  *-- Message : 32062
  *-- Order line has allocated quantity.
  *-- Allocation will be released.
  *-- Button : 32005
  *-- Proceed Cancel
  IF TotCut > 0 AND gfModalGen('QRM32062B32005','ALERT') = 2
    RETURN
  ENDIF
  *-- (End)
  IF llStores
    lnTotPcs = lnTotPcs - m.TotQty
    lnTotAmt = lnTotAmt - m.TotQty*m.Price
  ELSE
    SELECT (lcTmpOrdn)
    =RLOCK()    
    REPLACE FLAG WITH IIF(FLAG='N',FLAG,'M')
    UNLOCK

    IF TotCut > 0 
      *-- Inhance openning files to speed up transaction
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      =SEEK(IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6),'CutPick')
      SET ORDER TO TAG 'CUTPKORD' IN (lcAlocated)
      SELECT CutPick
      SCAN REST WHILE TranCd+Order+cOrdLine = ;
                      IIF(Style.Make,'1','2')+m.Order+STR(m.LineNo,6)
        IF !SEEK(CutPick.TranCd+CutPick.cTktNo+CutPick.cTktLineNo+;
                 CutPick.Order+CutPick.cOrdLine,lcAlocated)
          INSERT INTO (lcAlocated) ;
          (cTktno,TranCd,cTktLineNo,Order,cOrdLine,Style) VALUES ;
          (CutPick.cTktNo,CutPick.Trancd,CutPick.cTktLineNo,CutPick.Order,;
           CutPick.cOrdLine,CutPick.Style)
        ENDIF
        SELECT (lcAlocated)
        =RLOCK()
        REPLACE Qty1   WITH 0 ,;
                Qty2   WITH 0 ,;
                Qty3   WITH 0 ,;
                Qty4   WITH 0 ,;
                Qty5   WITH 0 ,;
                Qty6   WITH 0 ,;
                Qty7   WITH 0 ,;
                Qty8   WITH 0 ,;
                TotQty WITH 0
        UNLOCK
        SELECT (lcOrdHdr)
        =RLOCK()
        REPLACE TotCut WITH TotCut - CutPick.TotQty
        UNLOCK
      ENDSCAN
      =gfCloseFile('CUTPICK')
      SELECT (lcTmpOrdn)
    ENDIF
    IF laScrMode[4] .AND. !EMPTY(laData[43]) .AND. ;
       SEEK(STR(BulkLineNo,6),lcBulkOrd) 
      SELECT (lcBulkOrd)
      =RLOCK()
      REPLACE USED1 WITH USED1 - m.Qty1 ,;
              USED2 WITH USED2 - m.Qty2 ,;
              USED3 WITH USED3 - m.Qty3 ,;
              USED4 WITH USED4 - m.Qty4 ,;
              USED5 WITH USED5 - m.Qty5 ,;
              USED6 WITH USED6 - m.Qty6 ,;
              USED7 WITH USED7 - m.Qty7 ,;
              USED8 WITH USED8 - m.Qty8 ,;
              TOTUSED WITH TOTUSED - m.TotQty
      UNLOCK
    ENDIF
    laData[35] = laData[35] - IIF(FLAG='N' OR llUpdBook,TotQty,0)
    laData[36] = laData[36] - IIF(FLAG='N' OR llUpdBook,TotQty*Price,0)
    laData[39] = laData[39] + IIF(FLAG='N' OR llUpdBook,0,TotQTy)
    laData[40] = laData[40] + IIF(FLAG='N' OR llUpdBook,0,TotQTy)*Price
    laData[41] = laData[41] - TotQty
    laData[42] = laData[42] - TotQty*Price
    SELECT (lcOrdHdr)
    =RLOCK()
    REPLACE BOOK      WITH laData[35] ,;
            BOOKAMT   WITH laData[36] ,;
            CANCEL    WITH laData[39] ,;
            CANCELAMT WITH laData[40] ,;
            OPEN      WITH laData[41] ,;
            OPENAMT   WITH laData[42]
    UNLOCK
    IF laScrMode[3] AND !llUpdBook
      =gfwCodePop(@laCodes,'CCANCRESON','D')
      INSERT INTO (lcOrdCanLn) ;
       (cOrdType,Order,LineNo,Cancelled,cCancReson,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
        Style,Account,Store,Dyelot,Price) VALUES ;
       (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Qty1,m.Qty2,m.Qty3,;
        m.Qty4,m.Qty5,m.Qty6,m.Qty7,m.Qty8,m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,;
        m.Style,m.Account,m.Store,m.Dyelot,m.Price)      


      SELECT (lcOrdCanLn)
      STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
      STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
      =gfwCodePop(@laCodes,'CCANCRESON','T')
      STORE gdSysDate TO m.Cancelled
      DO (gcScrDir+"SOORDCLN.SPX")
      STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
      =gfAdd_Info(lcOrdCanLn)
    ENDIF 
  ENDIF
  
  IF llBomVarnt
    IF ASCAN(laEvntTrig , PADR('DOSCRN',10)) <> 0      
      *-- Do nothing validation is in JLMAIN
    ELSE
      SELECT (lcT_BomVar)
      =SEEK("SO"+laData[1]+STR(&lcTmpOrdn..LineNo,6))
      SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcTmpOrdn..LineNo,6)
        REPLACE cStatus WITH SUBSTR('DDS',AT(cStatus,'SMA'),1)
      ENDSCAN
      DELETE ALL FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(&lcTmpOrdn..LineNo,6)
    ENDIF     
  ENDIF
  SELECT (lcTmpOrdn)
  IF llOneRec
    SCATTER MEMVAR BLANK
    m.Store = laData[3]    
    GATHER MEMVAR
    REPLACE CURPOS WITH  "1"
  ELSE
    DELETE
  ENDIF
  SKIP -1
  IF BOF()
    GO TOP
  ENDIF
  lnGoRec = RECNO()
  lcDispDet = IIF(EOF(),'DISABLE','')
  SCATTER MEMVAR
  IF llStores
    SHOW WINDOW (lcBrTtlM) REFRESH SAME
    SHOW GETS WINDOW 'SOMULTI2' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI3' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI4' &lcDispDet ONLY
    SHOW GETS WINDOW 'SOMULTI5' &lcDispDet ONLY
    =lfRefresh('SOMULTI2')
    =lfRefresh('SOMULTI3')
    =lfRefresh('SOMULTI4')
    SHOW GET pbClose ENABLE
    _CUROBJ = OBJNUM(ibInv200E)
  ELSE
    SHOW WINDOW (lcOrdBrow) REFRESH SAME
    SHOW GETS WINDOW (lcWinCh32)  &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh321) &lcDispDet ONLY
    SHOW GETS WINDOW (lcWinCh322) &lcDispDet ONLY
    =lfRefresh(lcWinCh321)
    _CUROBJ = OBJNUM(ibBrowOrd)
  ENDIF
  IF EOF()
    SHOW GET llMultiSt ENABLE
    SHOW GET pbNew     ENABLE 
    SHOW GET pbPacks   ENABLE
    IF llMultiSt
      SHOW GET pbTemplate ENABLE
    ENDIF
   _CUROBJ = OBJNUM(pbNew)
  ENDIF
  STORE .T. TO llCUpdate
ENDIF
SELECT (lcTmpOrdn)
LOCATE
GO BOTT
lnLastRec =  RECNO()
=lfBrowOrdL()
GO IIF(BETWEEN(lnGoRec,1,RECCOUNT()),lnGoRec,RECNO())
IF SEEK (STYLE,'STYLE')
  SKIP IN STYLE
  lcLastStyle = STYLE.STYLE
ENDIF



*!**************************************************************************
*! Name      : lfvPiec
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Totqty Validation function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvPiec
PARAMETERS llStores,llFrmGBrw

IF TotQty < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  REPLACE TOTQTY WITH lnOldTot
  RETURN
ENDIF

=SEEK(style,'style')
IF EMPTY(Style.PrePak) 
  IF  !llFrmGBrw .AND. Scale.Cnt > 1 
    RETURN
  ENDIF
ENDIF

IF !EMPTY(Style.PrePak) .AND. SEEK('P'+Style.Scale+Style.PrePak,'Scale')
  *-- Message : 32030
  *-- Total quantity is not evenly divisible by the prepak quantity xxx
  *-- Button : 00000 
  *-- Ok
  IF MOD(TotQty,Scale.PPTot) <> 0 .AND. ;
    gfModalGen('TRM32030B00000','ALERT',STR(Scale.PPTot,4))=1
    REPLACE TotQty WITH lnOldTot 
    =SEEK('S'+Style.PrePak,'Scale')
    RETURN .F.
  ENDIF
  m.Qty1 = TotQty*Scale.PP1/Scale.PPTot
  m.Qty2 = TotQty*Scale.PP2/Scale.PPTot
  m.Qty3 = TotQty*Scale.PP3/Scale.PPTot
  m.Qty4 = TotQty*Scale.PP4/Scale.PPTot
  m.Qty5 = TotQty*Scale.PP5/Scale.PPTot
  m.Qty6 = TotQty*Scale.PP6/Scale.PPTot
  m.Qty7 = TotQty*Scale.PP7/Scale.PPTot
  m.Qty8 = TotQty*Scale.PP8/Scale.PPTot
  IF Picked .AND.  ;
     (m.Qty1 < Pik1 .OR. m.Qty2 < Pik2 .OR. m.Qty3 < Pik3 .OR. m.Qty4 < Pik4 .OR. ;
      m.Qty5 < Pik5 .OR. m.Qty6 < Pik6 .OR. m.Qty7 < Pik7 .OR. m.Qty8 < Pik8)
    *-- Message : 32015
    *-- Quantity cannot be below than what has been picked for this style/color
    *-- Button : 00000
    *-- Ok
    =gfModalGen('TRM32015B00000','ALERT')
    =SEEK('S'+m.Scale,'Scale')
    RETURN
  ENDIF
  REPLACE QTY1 WITH m.Qty1,QTY2 WITH m.Qty2,QTY3 WITH m.Qty3,QTY4 WITH m.Qty4,;
          QTY5 WITH m.Qty5,QTY6 WITH m.Qty6,QTY7 WITH m.Qty7,QTY8 WITH m.Qty8
ELSE
  IF Scale.Cnt = 1
    m.Qty1 = TotQty
    *-- Validate TotQty as 1st size qty in case of single size scale.
    IF m.Qty1 < 0 
      *--Message : 42000
      *--Negative values are not allowed.
      *--Button  : 40011
      *--Ok
      = gfModalGen('TRM42000B40011','DIALOG')
      REPLACE TOTQTY WITH lnOldTot
      RETURN
    ENDIF

    IF m.Qty1 <> Qty1
      IF Picked AND !EMPTY(PikTKt) AND m.Qty1 < Pik1
        *--Message : 32015
        *--Quantity cannot be below than what has been picked for this style/color
        *--Button : 00000
        *--     Ok
        =gfModalGen('TRM32015B00000','ALERT')
        m.Qty1 = &lcTmpOrdn..Qty1
        RETURN
      ENDIF
      *Check the sold out date of the style edited its quantity
      *Message :Style sold out date is XX/XX/XXXX
      *   < Accept >  < Reenter >
      IF !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
           gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
        m.Qty1 = &lcTmpOrdn..Qty1
        RETURN
      ENDIF

      IF m.Qty1 < Cut1
        IF !gfUserPriv('MF','MFCUTKT')
          llIgnorAll = .T.  
          *-- Message: This order has an allocated quantity, you may need to edit the 
          *-- quantities on the cutting ticket, but the access to do this is denied.'
          =gfModalGen('INM32075B00000','DIALOG')
        ENDIF
        *Message : 32016
        *Size xxxxx has xxx pieces allocated from orders.
        *Edit the detail allocated quantity from the order lines to keep 
        *track of the allocated quantity, Ignore tracking the allocation 
        *for only the quantity being modified, or for all later modifications.
        *Button : 32002
        *Edit Allo. Qty./Ignore/Ignore All/Cancel
    
        lnChoice = IIF(llIgnorAll,3,;
        gfModalGen('QRM32016B32002','ALERT',ALLTRIM(SCALE.Sz1)+'|'+ALLTRIM(STR(Cut1,5))))
        llIgnorAll = (lnChoice=3)
        IF lnChoice =4
          STORE Qty1 TO m.Qty1
          RETURN
        ENDIF
        lnAlias = SELECT()
        =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
        SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
        IF !SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6),lcAlocated) .AND. ;
           SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6),'CutPick')
          SELECT CutPick
          SCAN REST WHILE TranCd+Order+cOrdLine = ;
            IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6)
            SCATTER TO laCutPick
            INSERT INTO (lcAlocated) FROM ARRAY laCutPick
          ENDSCAN
        ENDIF
        =gfCloseFile('CUTPICK')
        DO CASE
          CASE lnChoice =1
            IF !lfEditAlo('1')
              m.Qty1 = &lcTmpOrdn..Qty1
            ENDIF    
          OTHERWISE
            SELECT (lcAlocated)
            =SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6))
            SCAN REST WHILE TranCd+Order+cOrdLine = ;
                IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6)
              SELECT (lcOrdHdr)
              =RLOCK()
              REPLACE TotCut WITH TotCut - &lcAlocated..Qty1
              UNLOCK
              SELECT (lcTmpOrdn)
              =RLOCK()
              REPLACE Cut1 WITH Cut1 - &lcAlocated..Qty1 ,;
                      TotCut     WITH TotCut     - &lcAlocated..Qty1
              UNLOCK
              SELECT (lcAlocated)
              =RLOCK()
              REPLACE TotQty     WITH TotQty - Qty1 ,;
                      Qty1 WITH 0
              UNLOCK
            ENDSCAN
        ENDCASE
        SELECT (lnAlias)
      ENDIF
    ENDIF
  ENDIF
ENDIF
=SEEK('S'+Style.Scale,'Scale')
IF m.Qty1=Qty1 AND m.Qty2=Qty2 AND m.Qty3=Qty3 AND m.Qty4=Qty4 AND ;
   m.Qty5=Qty5 AND m.Qty6=Qty6 AND m.Qty7=Qty7 AND m.Qty8=Qty8 AND m.TotQty=TotQty
  RETURN
ENDIF
M.TOTQTY = TOTQTY
lnRec = RECNO()
IF !lfChkTotQ()
  GO IIF(BETWEEN(lnRec,1,RECCOUNT()),lnRec,RECNO())
ENDIF
*--- SSH
FLUSH
*----------------

*!**************************************************************************
*! Name      : lfwQty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When qtys function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfwQty
PARAMETERS lnQty

IF  RECNO()  = lnLastRec
  ON KEY LABEL DNARROW  DO lpAddItem
ELSE
  ON KEY LABEL DNARROW
ENDIF 

lcQty = STR(lnQty,1)
lnOldQty&lcQty = Qty&lcQty
M.QTY&lcQty = QTY&lcQty


RETURN !EMPTY(STY) AND !EMPTY(Color) AND SEEK('S'+STYLE.SCALE,'SCALE') AND SCALE.CNT >=lnQty

*!*************************************************************
*! Name      : FUNCTION lfChkTotQ
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 07/01/1996
*! Purpose   : Check total pieces againest total quantity entered
*!*************************************************************
*! Calls     : gfModalGen(),lfvNPr()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfChkTotQ()
*!*************************************************************
*! C102646
FUNCTION lfChkTotQ
PARAMETERS llStores

*-- Message : 32031
*-- Quantity out of balance! xxx pieces ok
*-- Button : 32000 
*-- Yes NO
IF m.TotQty <> m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8 AND ;
   gfModalGen('QRM32031B32000','ALERT',ALLTRIM(STR(m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8,8)))=2
  RETURN(.F.)
ENDIF
*-- Message : 32030
*-- Total quantity is not evenly divisible by the prepak quantity xxx
*-- Button : 00000 
*-- Ok
SELECT (lcTmpOrdn)
IF Qty1 <> lnOldQty1 OR Qty2 <> lnOldQty2 OR Qty3 <> lnOldQty3 OR Qty4 <> lnOldQty4 OR ;
   Qty5 <> lnOldQty5 OR Qty6 <> lnOldQty6 OR Qty7 <> lnOldQty7 OR Qty8 <> lnOldQty8
  STORE .T. TO llCUpdate
  m.TotQty = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  *-- Get style price according to ordered quantity
  IF !m.lContract AND lcPriceLvl='Q'
    *-- Select alternative price level
    m.Gros_Price = lfGetprice(m.Style,lcPriceLvl,m.TotQty)
    m.Price = m.Gros_Price*(100-m.Disc_Pcnt)/100
  ENDIF
  IF laScrMode[4] AND !EMPTY(cFromOrder) .AND. SEEK(STR(BulkLineNo,6),lcBulkOrd)
    SELECT (lcBulkOrd)
    =RLOCK()
    REPLACE USED1   WITH USED1 - &lcOrdLine..Qty1 + m.Qty1 ,;
            USED2   WITH USED2 - &lcOrdLine..Qty2 + m.Qty2 ,;
            USED3   WITH USED3 - &lcOrdLine..Qty3 + m.Qty3 ,;
            USED4   WITH USED4 - &lcOrdLine..Qty4 + m.Qty4 ,;
            USED5   WITH USED5 - &lcOrdLine..Qty5 + m.Qty5 ,;
            USED6   WITH USED6 - &lcOrdLine..Qty6 + m.Qty6 ,;
            USED7   WITH USED7 - &lcOrdLine..Qty7 + m.Qty7 ,;
            USED8   WITH USED8 - &lcOrdLine..Qty8 + m.Qty8 ,;
            TOTUSED WITH Used1+Used2+Used3+Used4+Used5+Used6+Used7+Used8
    UNLOCK
    SELECT (lcTmpOrdn)
  ENDIF  
  =SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
  laData[35] = laData[35] - TotBook
  laData[36] = laData[36] - TotBook*Price
  
  =RLOCK()
  REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+m.Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
          Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+m.Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
          Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+m.Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
          Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+m.Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
          Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+m.Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
          Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+m.Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
          Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+m.Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
          Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+m.Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
          TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
          FLAG WITH IIF(FLAG='N','N','M')
  UNLOCK
  *-- Initialize value to store old cancelled Qty. [Begin]
  PRIVATE lnOldCanLn
  lnOldCanLn = 0
  *-- Store order line calceled quantity
  IF Flag <> 'N' AND !llUpdBook
    IF OrdLine.Qty1 > m.Qty1 OR OrdLine.Qty2 > m.Qty2 OR OrdLine.Qty3 > m.Qty3 OR ;
       OrdLine.Qty4 > m.Qty4 OR OrdLine.Qty5 > m.Qty5 OR OrdLine.Qty6 > m.Qty6 OR  ;
       OrdLine.Qty7 > m.Qty7 OR OrdLine.Qty8 > m.Qty8

      IF !SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)  
        =gfwCodePop(@laCodes,'CCANCRESON','D')        
        INSERT INTO (lcOrdCanLn) ;
        (cOrdType,Order,LineNo,Cancelled,cCancReson,Price) VALUES ;
        (lcOrdType,laData[1],m.LineNo,gdSysDate,laCanReason[lnCanReason,2],m.Price)
      ENDIF  
    ENDIF
    IF SEEK(lcOrdType+laData[1]+STR(m.LineNo,6),lcOrdCanLn)
      SELECT (lcOrdCanLn)
      lnOldCanLn = TotQty
      REPLACE Qty1 WITH MAX(OrdLine.Qty1- m.Qty1,0) ,;
              Qty2 WITH MAX(OrdLine.Qty2- m.Qty2,0) ,;
              Qty3 WITH MAX(OrdLine.Qty3- m.Qty3,0) ,;
              Qty4 WITH MAX(OrdLine.Qty4- m.Qty4,0) ,;
              Qty5 WITH MAX(OrdLine.Qty5- m.Qty5,0) ,;
              Qty6 WITH MAX(OrdLine.Qty6- m.Qty6,0) ,;
              Qty7 WITH MAX(OrdLine.Qty7- m.Qty7,0) ,;
              Qty8 WITH MAX(OrdLine.Qty8- m.Qty8,0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      REPLACE ACCOUNT WITH ORDLINE.ACCOUNT,;
              STYLE   WITH ORDLINE.STYLE,;
              STORE   WITH ORDLINE.STORE,;
              DYELOT  WITH ORDLINE.DYELOT
      IF TotQty > 0
        STORE lcOrdCanLn TO laCodes[6,7],laCodes[6,8]
        STORE 'lcOrdType+laData[1]+STR(m.LineNo,6)' TO laCodes[6,9]
        =gfwCodePop(@laCodes,'CCANCRESON','T')
        STORE Cancelled TO m.Cancelled
        DO (gcScrDir+"SOORDCLN.SPX")
        STORE '' TO laCodes[6,7],laCodes[6,8],laCodes[6,9]
      ELSE
        DELETE
      ENDIF
      SELECT (lcTmpOrdn)
    ENDIF
  ENDIF  
  laData[35] = laData[35] + TotBook
  laData[36] = laData[36] + TotBook*Price
  
  IF FLAG <> 'N' AND !llUpdBook
    *-- Fix bug that when update Qty per size The cancelled qty 
    *-- ABD Is not getting updated Correct. [Begin]
    lnTotCanQty = 0
    lnTotCanQty = &lcOrdCanLn..TotQty       
    laData[39] = MAX(laData[39] - lnOldCanLn + lnTotCanQty,0)
    laData[40] = MAX(laData[40] - (lnOldCanLn*Price) + (lnTotCanQty*Price),0)
  ENDIF  
  laData[41] = laData[41] - lnOldTot+TOtQty
  laData[42] = laData[42] - lnOldTot*Price+ TOtQty*Price
 
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOK      WITH laData[35] ,;
          BOOKAMT   WITH laData[36] ,;
          CANCEL    WITH laData[39] ,;
          CANCELAMT WITH laData[40] ,;
          OPEN      WITH laData[41] ,;
          OPENAMT   WITH laData[42]
  UNLOCK
  SELECT (lcTmpOrdn)
  =RLOCK()
  m.TotQty = m.Qty1+m.Qty2+m.Qty3+m.Qty4+m.Qty5+m.Qty6+m.Qty7+m.Qty8
  M.Gros_Price = IIF(Gros_Price=0,Price,Gros_Price)
  GATHER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
                       Gros_Price,Price
  REPLACE TOTQTY WITH M.TOTQTY,;
          AMOUNT WITH M.TOTQTY*PRICE
  UNLOCK
ENDIF


*!**************************************************************************
*! Name      : lfvQty
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Qtys Validation function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvQty
PARAMETERS lnQty

lnOldTot = TOTQTY

IF lnQty <> 9
  lcQty = STR(lnQty,1)
  IF Qty&lcQty < 0 
    *-- Message : 42000
    *-- Negative values are not allowed.
    *-- Button  : 40011
    *-- Ok
    = gfModalGen('TRM42000B40011','DIALOG')
    REPLACE Qty&lcQty WITH lnOldQty&lcQty
    RETURN
  ENDIF
  IF Qty&lcQty <> lnOldQty&lcQty

    IF Picked AND !EMPTY(PikTKt) AND Qty&lcQty < Pik&lcQty
      *-- Message : 32015
      *-- Quantity cannot be below than what has been picked for this style/color
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM32015B00000','ALERT')
      REPLACE Qty&lcQty WITH lnOldQty&lcQty
      RETURN
    ENDIF
    *--  Check the sold out date of the style edited its quantity
    *--  Message :Style sold out date is XX/XX/XXXX
    *--    < Accept >  < Reenter >
    IF !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
        gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
        REPLACE Qty&lcQty WITH lnOldQty&lcQty
      RETURN
    ENDIF
    IF Qty&lcQty < Cut&lcQty
      *-- If the user has no access on CT, inform the user and ignore all.
      IF !gfUserPriv('MF','MFCUTKT')
        llIgnorAll = .T.  
        *-- Message: This order has an allocated quantity, you may need to edit the 
        *-- quantities on the cutting ticket, but the access to do this is denied.'
        =gfModalGen('INM32075B00000','DIALOG')
      ENDIF
      *-- Message : 32016
      *-- Size xxxxx has xxx pieces allocated from orders.
      *-- Edit the detail allocated quantity from the order lines to keep 
      *-- track of the allocated quantity, Ignore tracking the allocation 
      *-- for only the quantity being modified, or for all later modifications.
      *-- Button : 32002
      *-- Edit Allo. Qty./Ignore/Ignore All/Cancel
    
      lnChoice = IIF(llIgnorAll,3,;
      gfModalGen('QRM32016B32002','ALERT',ALLTRIM(SCALE.Sz&lcQty)+'|'+ALLTRIM(STR(Cut&lcQty,5))))
      llIgnorAll = (lnChoice=3)
      IF lnChoice =4
        *--Let the fields 'Qty..' be refrenced by the alias Exceplicitly      
        REPLACE Qty&lcQty WITH lnOldQty&lcQty
        RETURN
      ENDIF
      lnAlias = SELECT()
      =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
      SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
      IF !SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6),lcAlocated) .AND. ;
         SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6),'CutPick')
        SELECT CutPick
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
          IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6)
          SCATTER TO laCutPick
          INSERT INTO (lcAlocated) FROM ARRAY laCutPick
        ENDSCAN
      ENDIF
      =gfCloseFile('CUTPICK')
      DO CASE
        CASE lnChoice =1
          IF !lfEditAlo(lcQty)
            REPLACE Qty&lcQty WITH lnOldQty&lcQty
          ENDIF    
        OTHERWISE
          SELECT (lcAlocated)
          =SEEK(IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6))
          SCAN REST WHILE TranCd+Order+cOrdLine = ;
              IIF(Style.Make,'1','2')+&lcTmpOrdn..Order+STR(&lcTmpOrdn..LineNo,6)
            SELECT (lcOrdHdr)
            =RLOCK()
            REPLACE TotCut WITH TotCut - &lcAlocated..Qty&lcQty
            UNLOCK
            SELECT (lcTmpOrdn)
            =RLOCK()
            REPLACE Cut&lcQty  WITH Cut&lcQty  - &lcAlocated..Qty&lcQty ,;
                    TotCut     WITH TotCut     - &lcAlocated..Qty&lcQty
            UNLOCK
            SELECT (lcAlocated)
            =RLOCK()
            REPLACE TotQty     WITH TotQty - Qty&lcQty ,;
                    Qty&lcQty WITH 0
            UNLOCK
          ENDSCAN
      ENDCASE
      SELECT (lnAlias)
    ENDIF
  ENDIF
ENDIF   


REPLACE TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8,;
        AMOUNT WITH (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8) * PRICE

=SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
laData[35] = laData[35] - TotBook
laData[36] = laData[36] - TotBook*Price
=RLOCK()
REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
        Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
        Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
        Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
        Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
        Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
        Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
        Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
        TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
        FLAG WITH IIF(FLAG='N','N','M')
UNLOCK
laData[35] = laData[35] + TotBook
laData[36] = laData[36] + TotBook*Price
laData[41] = laData[41] - lnOldTot+TOtQty
laData[42] = laData[42] - lnOldTot*Price+ TOtQty*Price
SELECT (lcOrdHdr)
=RLOCK()
REPLACE BOOK      WITH laData[35] ,;
        BOOKAMT   WITH laData[36] ,;
        CANCEL    WITH laData[39] ,;
        CANCELAMT WITH laData[40] ,;
        OPEN      WITH laData[41] ,;
        OPENAMT   WITH laData[42]
UNLOCK
SELECT (lcTmpOrdn)



*!**************************************************************************
*! Name      : lfvGPric
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Gross Price Validation function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvGPric
PARAMETERS llStores,llFrmGBrw
PRIVATE lcOrdAlias
IF Price < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  REPLACE PRICE WITH lcOldValue
  RETURN
ENDIF

IF (!llFrmGBrw .AND. Price <> lcOldValue)

  IF llBomVarnt .AND. (!llFrmGBrw .AND. Price < lcOldValue)
    lcOrdAlias = ALIAS()
    SELECT (lcT_BomVar)
    SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
    SELECT (lcOrdAlias)
    IF m.Gros_Price < lnAddPrice
      *** There is style positions for this line, you cannot ***
      *** change the price to be less than the price adder.  ***
      *** <  Ok  > ***
      =gfModalGen("INM32073B00000" , "DIALOG")
      REPLACE PRICE WITH lcOldValue
      RETURN
    ENDIF
  ENDIF
  =lfvNPr(llStores,.F.,llFrmGBrw)
ENDIF
*--- SHS
FLUSH

FUNCTION lfOldvalu
lcOldValue = PRICE

*!**************************************************************************
*! Name      : lfvNPr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Net Price Validation function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfvNPr
PARAMETERS llStores,llPercent,llFrmGBrw

IF Price < 0 
  *-- Message : 42000
  *-- Negative values are not allowed.
  *-- Button  : 40011
  *-- Ok
  = gfModalGen('TRM42000B40011','DIALOG')
  REPLACE TOTQTY WITH lnOldTot
ENDIF


IF Price <> lnOldPrice
  SELECT (lcTmpOrdn)
  laData[36] = laData[36] + TotBook*(Price - lnOldPrice)
  IF !llFrmGBrw
    laData[42] = laData[42] + TotQty*(Price - lnOldPrice)
  ENDIF
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE BOOKAMT WITH laData[36] ,;
          OPENAMT WITH laData[42]
  UNLOCK
  SELECT (lcTmpOrdn)
  REPLACE Gros_Price WITH Price,;
          Flag       WITH IIF(Flag='N' .OR. llStores,Flag,'M') 
  m.Gros_Price = Gros_Price
  UNLOCK
ENDIF
REPLACE AMOUNT WITH (QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8)*PRICE




*!**************************************************************************
*! Name      : lfClsSc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Close Button Validation function
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfClsSc
IF laScrMode[3] OR laScrMode[4]
  SELECT (lcOrdLine)
  DELETE ALL
  lcSetDel = SET("DELETE")
  SET DELETE OFF
  SELECT (lcTmpOrdn)
  *--Don't remove this line as when changing the price and price the Close button
  *--whithout pressing enter, it updates the detail order liens with the old price
  WAIT WINDOW "Update order lines" TIMEOUT .1
  FLUSH
  SCAN FOR !EMPTY(STYLE)
    SCATTER MEMVAR MEMO
    m.Store = laData[3]
    INSERT INTO (lcOrdLine) FROM MEMVAR
    IF DELETED()
      SELECT (lcOrdLine)
      DELETE
      SELECT (lcTmpOrdn)
    ENDIF
  ENDSCAN
  SET DELETE &lcSetDel
ENDIF  
CLEAR READ



*!**************************************************************************
*! Name      : lfWClose
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : When function for Close Button
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfWClose

ON KEY LABEL TAB ACTIVATE WINDOW (lcOrdWin)
IF llFirstt
  KEYBOARD "{TAB}"
  llFirstt = .F.
ENDIF  

*!**************************************************************************
*! Name      : lfClrTab
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Clear tapping
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfClrTab

ON KEY LABEL TAB

*!**************************************************************************
*! Name      : lfActBr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Activate the browse
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*! C102646
FUNCTION lfActBr

ACTIVATE WINDOW (lcOrdWin)


*!*************************************************************
*! Name      : FUNCTION lfwGPrice
*! Developer : Wael Aly Mohamed
*! Date      : 07/01/1996
*! Purpose   : When function for Style Gross price
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwGPrice()
*!*************************************************************
FUNCTION lfwGPrice
PARAMETERS llStores


lnOldPrice = Price
PRIVATE lcPriceCatg,lnPrice,lcOrdAlias
IF m.lContract 
  RETURN(.F.)
ENDIF
IF llBomVarnt
  lcOrdAlias = ALIAS()
  SELECT (lcT_BomVar)
  *--Collect the price adder for the style positions.
  SUM nPriceAdd FOR cIdType+cCost_Id+STR(LineNo,6) = "SO" + laData[1] + STR(m.LineNo,6) TO lnAddPrice
  SELECT (lcOrdAlias)
  *-- If the editted price less than the price adder, inform the user.
  IF lnAddPrice > 0
    RETURN
  ENDIF
ENDIF

*-- Message : 32032
*-- Style price xxx does not match order price.  Default to style price?
*-- Button : 32000
*-- Yes No
lcPriceCatg = IIF(lcPriceLvl='Q',;
              IIF(Style.nAtQtyC>0 AND m.TotQty >= Style.nAtQtyC,'C',;
              IIF(Style.nAtQtyB>0 AND m.TotQty >= Style.nAtQtyB,'B','A')),;
              IIF(INLIST(lcPriceLvl,'A','B','C'),lcPriceLvl,'A'))
lnPrice = IIF(!llMulCurr OR laData[33]=gcBaseCurr,Style.Price&lcPriceCatg,;
                       gfStyPrice(m.Style,lcPriceCatg,laData[33]))

IF lnPrice>0 .AND. m.Gros_Price <> lnPrice .AND. ;
  gfModalGen('QRM32032B32000','ALERT',ALLTRIM(STR(lnPrice,12,2))+;
  '|'+IIF(lcOrdType='C','contract','order'))=1

  m.Gros_Price = lnPrice
  m.Price = ROUND(m.Gros_Price*(100-m.Disc_Pcnt)/100,2)
  SELECT (lcTmpOrdn)
  REPLACE Gros_Price WITH m.Gros_Price,;
          Price WITH m.Price
  =lfvNPr(llStores)
ENDIF


*!*************************************************************
*! Name      : lfNotFound
*! Developer : Adel Moahmmed El Gazzar (ADEL)
*! Date      : 07/01/1996
*! Purpose   : check if the same color applys to the new entered style
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfwGPrice()
*!*************************************************************
FUNCTION lfNotFound
*--B606347,1 
PRIVATE lcOrd
lcOrd = ORDER('Style')
SET ORDER TO STYLE IN STYLE
llGetSty = !EMPTY(Color) AND !SEEK(Sty+lcSepart+Color,'Style')
SET ORDER TO &lcOrd IN STYLE
RETURN llGetSty
