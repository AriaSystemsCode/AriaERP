*:************************************************************************
*: File      : ALCALOF.PRG                                              :*
*: System    : ARIA 4.0 XP                                              :*
*: Modules   : AL                                                       :*
*: Program   : Amend Call Off.                                          :*
*: Developer : Ahmed Salah Shalaby - SSH                                :*
*: Date      : 02/10/2007
*: Issue NO. : 200735,1                                                 :*
*: Ticket#   : T20061215.0012                                           :*
*:************************************************************************
*: Modifications:                                                       :* 
*: B608216,1 SSH 08/09/2007 not update order line in some cases
*: C200863,1 MMT 10/11/2007 Custom Despatch performance report[T20070822.0012]
*: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[T20120330.0001]
*:************************************************************************

LOCAL lcModUsedIn

DO FORM (oAriaApplication.ScreenHome + "AL\ALCALOF")

*--End [POACFRV.PRG]...


*!*************************************************************
*! Name      : lfFormInit
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Initialize Screen Properties.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormInit()
*!*************************************************************
FUNCTION lfFormInit
LPARAMETERS oFormSet

*--Screen Variables.

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    oFormSet.ncolorlen = LEN(laItemSeg[lnCount,3])
    oFormSet.nColorpos= laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR


*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    oFormSet.nStylen = LEN(laItemSeg[lnCount,3])
    oFormSet.nStyPo= laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
oFormSet.nSizPos = oFormSet.nColorpos+oFormSet.ncolorlen
WITH oFormSet

  *--Open needed files.
  *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
  *DIMENSION laOpenFile[8,4]
  DIMENSION laOpenFile[9,4]
  *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[END]    
  laOpenFile[1,1] = 'STYLE'
  laOpenFile[1,2] = 'STYLE'
  laOpenFile[1,4] = 'Style'

  laOpenFile[2,1] = 'SCALE'
  laOpenFile[2,2] = 'SCALE'
  laOpenFile[2,4] = 'Scale'

  laOpenFile[3,1] = 'OrdHdr'
  laOpenFile[3,2] = 'OrdHdr'
  laOpenFile[3,4] = 'OrdHdr'

  laOpenFile[4,1] = 'OrdLine'
  laOpenFile[4,2] = 'OrdLine'
  laOpenFile[4,4] = 'OrdLine'

  laOpenFile[5,1] = 'alcaloff'
  laOpenFile[5,2] = 'alcaloff'
  laOpenFile[5,4] = 'alcaloff'
  
  laOpenFile[6,1] = 'alurnhst'
  laOpenFile[6,2] = 'Alurnpk'
  laOpenFile[6,4] = 'alurnhst'
  
  laOpenFile[7,1] = 'altrkmnf'
  laOpenFile[7,2] = 'altrkmnf'
  laOpenFile[7,4] = 'altrkmnf'

  laOpenFile[8,1] = 'PikTkt'
  laOpenFile[8,2] = 'PikTkt'
  laOpenFile[8,4] = 'PikTkt'
  *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
  laOpenFile[9,1] = 'STYDYE'
  laOpenFile[9,2] = 'STYDYE'
  laOpenFile[9,4] = 'STYDYE'
  *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[END]    
  FOR I = 1 To ALEN(laOpenFile,1)
    IF !USED(laOpenFile[I,4])
      laOpenFile[I,3] = gfOpenTable(oAriaApplication.DataDir+laOpenFile[I,1],laOpenFile[I,2], "SH",laOpenFile[I,4])
    ENDIF
  ENDFOR

  *--Create the Temporary BOMLine file
  =lfCrtTempFl(oFormSet)

  *--Create the Temporary BOMLine file
  =lfAdjustGrid(oFormSet)
ENDWITH

*!*************************************************************
*! Name      : lfActPad
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActPad(oFOrmSet)
*!*************************************************************
FUNCTION OldlfActPad
LPARAMETERS oFormSet


*!*************************************************************
*! Name      : lfFormDeact
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFormDeact(This)
*!*************************************************************
FUNCTION lfFormDeact
LPARAMETERS oFormSet

RELEASE PAD _Option OF _MSYSMENU

*!*************************************************************
*! Name      : lpShow
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/25/2004
*! Purpose   : Show Screen Mode Changes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfUnCompSession(),
*!                          gfCPSave(),()
*!*************************************************************
*! Passed Parameters  : oFormSet --> FormSet Object Reference.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow(This)
*!*************************************************************
FUNCTION lpShow
LPARAMETERS oFormSet

DO CASE
  CASE oFormSet.ActiveMode = 'S'  && Select mode.

    oFormSet.AriaForm1.kbCALOFF.KeyTextBox.Value = SPACE(6)
    oFormSet.AriaForm1.kbCALOFF.Enabled = .T.
    oFormSet.AriaForm1.MsStore.Enabled = .T.
    
    *--Refresh the temporary file and the browse in case of selecting a new transaction.
    IF USED (oFormSet.lcTmpLine)
      SELECT (oFormSet.lcTmpLine)
      BLANK  ALL
      DELETE ALL
    ENDIF

  CASE oFormSet.ActiveMode = 'V'  && View mode. 
    
  CASE oFormSet.ActiveMode = 'E'  && Edit mode. 
    oFormSet.Ariaform1.kbCALOFF.Enabled=.F.
  CASE oFormSet.ActiveMode = 'A'  && Add  mode.     
ENDCASE

*--Refresh the Refresh toolbar button.

*--Adjust Grid Columns Read Only Properties.
WITH oFormSet.AriaForm1.grdCallOff
  .ActivateCell(1,1)
ENDWITH

RETURN IIF(oFormSet.ActiveMode = 'S' , 0, .T.)



*:*************************************************************
*! Name      : lfCreate
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/26/2004
*! Purpose   : To create the temproray bom line file.
*:*************************************************************
*! Calls     : 
*!             Procedures : lfActCst
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  : oFormSet -> FormSet ObjectControl.
*!                      llToVew  -> Not ask to create yes/no.
*!*************************************************************
*! Returns            : llLoop -> Not Valid selections.
*!*************************************************************
*! Example   : =lfCreate()
*!*************************************************************
FUNCTION lfCreate
LPARAMETERS oFormSet, llToVew


*:*************************************************************
*! Name      : lfSavScr
*! Developer : Wael M. Abo-Shawareb
*! Date      : 09/15/2004
*! Purpose   : Save / Update.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
PROCEDURE lfSavScr
LPARAMETERS oFormSet
PRIVATE lcTempAls,lcCurrAls,lnIndex,lcIndex,lndifferances,llOrderFound

lcCurrAls = SELECT(0)
lcTempAls=oFOrmSet.lcTmpLine
WAIT WINDOW 'Start Updating...' NOWAIT

SELECT (lcTempAls)
SCAN FOR lModified
  lndifferances = 0
  SCATTER MEMVAR MEMO
  SELECT ALCALOFF
  IF gfSeek(m.cCallOff)
    LOCATE FOR ccalloff+cmsstore+cdepot+style = m.cCallOff +m.cMsStore+m.cDepot+m.Style;
           AND order+style+STR(lineno,6) = m.Order+m.Style+STR(m.lineno,6) AND PikTkt=m.PikTkt
    IF FOUND()
      * Correct OrdLine Pointer
      SELECT OrdLine
      IF gfSeek("O"+m.order)
      
        LOCATE REST FOR cordtype+order+STR(lineno,6)="O"+m.order+STR(m.lineno,6) AND PikTkt=m.PikTkt
        llOrderFound=FOUND()
        *: B608216,1 SSH 08/09/2007 not update order line in some cases
        IF !llOrderFound
          =gfSeek("O"+m.order)
          LOCATE REST FOR cordtype+order+STR(lineno,6)="O"+m.order AND PikTkt=m.PikTkt AND Style=m.style
          llOrderFound=FOUND()
        ENDIF
         *: B608216,1 SSH 08/09/2007 not update order line in some cases
      ENDIF
      FOR lnIndex=1 TO 8
        SELECT ALCalOff
        lcIndex = ALLTRIM(STR(lnIndex))
        lndifferances = EVALUATE("Qty"+lcIndex) - ABS(EVALUATE("m.Qty"+lcIndex))
        REPLACE ("Qty"+lcIndex) WITH EVALUATE("m.Qty"+lcIndex)
        IF llOrderFound AND lndifferances>0
          SELECT OrdLine
          *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
          =gfSeek(Ordline.Style,'Style','Style')
          =gfSeek(Ordline.Style+Ordline.cWareCode,'StyDye','StyDye')
          *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[END]    
          IF  MAX(EVALUATE("Pik"+lcIndex)-lndifferances,0)>0
            REPLACE ("Pik"+lcIndex) WITH MAX(EVALUATE("Pik"+lcIndex)-lndifferances,0),;
                    TotPik WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8
            *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
            SELECT Style
            =gfReplace('ALO&lcIndex. With ALO&lcIndex. - lndifferances')
            =gfReplace('TOTALO With TOTALO - lndifferances')
            SELECT STYDYE
            =gfReplace('ALO&lcIndex. With ALO&lcIndex. - lndifferances')
            =gfReplace('TOTALO With TOTALO - lndifferances')
            *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[END]     
          ELSE
            SELECT OrdLine
            =gfSeek("O"+m.order)
            LOCATE REST FOR cordtype+order+STR(lineno,6)="O"+m.order AND PikTkt=m.PikTkt AND Style=m.style AND (EVALUATE("Pik"+lcIndex)-lndifferances)>0
            IF FOUND()
              REPLACE ("Pik"+lcIndex) WITH MAX(EVALUATE("Pik"+lcIndex)-lndifferances,0),;
                      TotPik WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8
              *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
              =gfSeek(Ordline.Style,'Style','Style')
              =gfSeek(Ordline.Style+Ordline.cWareCode,'StyDye','StyDye')
              SELECT Style
              =gfReplace('ALO&lcIndex. With ALO&lcIndex. - lndifferances')
              =gfReplace('TOTALO With TOTALO - lndifferances')
              SELECT STYDYE
              =gfReplace('ALO&lcIndex. With ALO&lcIndex. - lndifferances')
              =gfReplace('TOTALO With TOTALO - lndifferances')
		  	  *: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[End]    
            ENDIF
          ENDIF
        ENDIF
      ENDFOR
      SELECT ALCalOff
      REPLACE TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      IF TotQty=0
        *C200863,1 MMT 10/11/2007 Custom Despatch performance report[Start]
        *DELETE
        *C200863,1 MMT 10/11/2007 Custom Despatch performance report[End]
      ENDIF
      SELECT OrdLine
      REPLACE TotPik WITH Pik1+Pik2+Pik3+Pik4+Pik5+Pik6+Pik7+Pik8
      SCATTER MEMVAR MEMO
      IF TotPik=0
        REPLACE PikDate WITH {},;
                PikTkt  WITH "",;
                Picked  WITH .F.
        IF gfSeek("O"+m.order)
          *: B608216,1 SSH 08/09/2007 add condition piktkt not empty
          *LOCATE REST WHILE cordtype+order+STR(lineno,6)="O"+m.order AND PikTkt=m.PikTkt
          LOCATE REST WHILE cordtype+order+STR(lineno,6)="O"+m.order AND PikTkt=m.PikTkt AND !EMPTY(PikTkt)
          *: B608216,1 SSH 08/09/2007 add condition piktkt not empty
          IF !FOUND()
            SELECT PikTkt
            IF gfSeek(m.PikTkt)
              REPLACE Status WITH "X"
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDIF
  * Remove URN labels
  SELECT AlURNhst
  =gfSeek(m.cCallOff)
  DELETE FOR cCallOff = m.cCallOff
ENDSCAN
SELECT ALCalOff
=gfTableUpdate('ALCalOff')
SELECT PikTkt
=gfTableUpdate('PikTkt')
SELECT OrdLine
=gfTableUpdate('OrdLine')
SELECT AlURNhst
=gfTableUpdate('AlURNhst')
*: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[Start]    
SELECT STYLE
=gfTableUpdate()
SELECT STYDYE
=gfTableUpdate()
*: B609916,1 MMT 05/14/2012 Call off program does not update style and stydye[End]    
SELECT(lcCurrAls)


RETURN .T.

*!*************************************************************
*! Name      : lfCrtTempFl
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : To create BOMLine temprory file
*!             that is needed by this program.
*!*************************************************************
*! Call      : gfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTempFl
LPARAMETERS oFormSet

*--Temp file.
LOCAL lcTempCurs, lcStatement
lcTempCurs  = gfTempName()
lcTmpLine   = gfTempName()
oFormSet.lctmpline = lcTmpLine
SELECT alcaloff
SELECT *,.F. as lModified FROM alcaloff INTO Cursor (lcTmpLine) READWRITE

RETURN



*!*************************************************************
*! Name      : lfAdjustGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : Assign Control Sources and Properties for the Grid
*!*************************************************************
*! Call      : lfAdjustGrid()
*!*************************************************************
FUNCTION lfAdjustGrid
LPARAMETERS oFOrmSet,llByCallOff,llByMSStore

WITH oFormSet.AriaForm1.grdCallOff
  .RecordSource = oFOrmSet.lcTmpLine
  .Column13.ControlSource   = "gfCodDes(" + oFOrmSet.lcTmpLine + ".CMSSTORE, 'CMSSTORE')"
  .Column15.ControlSource   = "lfGetSclDsc(SUBSTR("+oFOrmSet.lcTmpLine+".Style,"+ALLTRIM(STR(oFormSet.nSizPos))+")"+")"
  .Column14.ControlSource   = "gfCodDes(SUBSTR("+oFOrmSet.lcTmpLine+".Style,"+ALLTRIM(STR(oFormSet.nColorpos))+","+ALLTRIM(STR(oFormSet.ncolorlen))+")"+",'COLOR')"
  .Column1.ControlSource    = oFOrmSet.lcTmpLine + '.Order'
  .Column2.ControlSource    = oFOrmSet.lcTmpLine + '.PikTkt'
  .Column3.ControlSource    = "SUBSTR("+oFOrmSet.lcTmpLine + ".Style,1,"+ALLTRIM(STR(oFormSet.nStylen))+")"
  .Column4.ControlSource    = oFOrmSet.lcTmpLine + '.Qty1'
  .Column5.ControlSource    = oFOrmSet.lcTmpLine + '.Qty2'
  .Column6.ControlSource    = oFOrmSet.lcTmpLine + '.Qty3'
  .Column7.ControlSource    = oFOrmSet.lcTmpLine + '.Qty4'
  .Column8.ControlSource    = oFOrmSet.lcTmpLine + '.Qty5'
  .Column9.ControlSource    = oFOrmSet.lcTmpLine + '.Qty6'
  .Column10.ControlSource   = oFOrmSet.lcTmpLine + '.Qty7'
  .Column11.ControlSource   = oFOrmSet.lcTmpLine + '.Qty8'
  .Column12.ControlSource   = oFOrmSet.lcTmpLine + '.TotQty'
ENDWITH

*!*************************************************************
*! Name      : lfGridNav
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004             
*! Purpose   : Navigate in the grid columns
*!*************************************************************
*! Call      : lfGridNav()
*!*************************************************************
FUNCTION lfGridNav
LPARAMETERS oFormSet, lnStep, lcGridCurs

LOCAL lnAlias, lnI
lnAlias = SELECT()

oFormSet.AriaForm1.LockScreen = .T.

SELECT (lcGridCurs)

lnI = 0
DO WHILE EMPTY(cCatgTyp)
  IF EOF() OR BOF()
    EXIT
  ENDIF
  lnI = lnI + lnStep
  SKIP lnStep
ENDDO
IF EOF() OR BOF()
  SKIP -lnI
ENDIF

oFormSet.AriaForm1.LockScreen = .F.
SELECT (lnAlias)
RETURN


*!*************************************************************
*! Name      : lfRestoreGrid
*! Developer : Wael M. Abo-Shawareb
*! Date      : 08/29/2004
*! Purpose   : Remove Grid Control Sources
*!*************************************************************
*! Call      : lfRestoreGrid()
*!*************************************************************
FUNCTION lfRestoreGrid
LPARAMETERS grdContrrol

LOCAL lnI

WITH grdContrrol
  .RecordSource = ''
  
  FOR lnI = 1 TO .ColumnCount
    .Columns(lnI).ControlSource  = ''
  ENDFOR
ENDWITH

RETURN

FUNCTION lfGetSclDsc
LPARAMETERS lcScale

PRIVATE lnOldAls,ln2Ret
lnOldAls = SELECT(0)
SELECT Scale
*type+scale+prepak
=gfSeek("S"+lcScale)
ln2Ret = Scale.cScl_Desc
SELECT(lnOldAls)
RETURN(ln2Ret)
