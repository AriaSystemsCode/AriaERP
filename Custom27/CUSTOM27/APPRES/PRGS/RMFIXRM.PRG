*:***************************************************************************
*: Program file  : RMFIXRM.PRG
*: Program desc. : Fix the worng files that related to RM problem.
*: System        : Aria Apparel System (A27).
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : ABDOU ELGENDI - (ABD) Due to B#606726,1
*: Date          : 11/24/2002
*:***************************************************************************
*: Calls :  
*:         Procedures : 
*:               -----------------------------------------------
*:          Functions : 
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*: Notes   : .... 
*:***************************************************************************
*: Example : DO RMFIXFILE
*:***************************************************************************
*:Modifications :
*:***************************************************************************
*:

IF !lfOpenFile()
  RETURN
ENDIF

*-- Give the user Message if he Sure to do that.
*-- Message Text : Are you sure you want to run the Fix Program now?
IF gfModalGen('INM00000B32000','F','ALERT',' ','Are you sure you want to run the Fix Program now?') = 2
  RETURN
ENDIF

lcTmpStyle = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpStyle) (Style c(19))
INDEX ON Style To (lcTmpStyle)

SELECT STYLE
SCAN
  SCATTER MEMVAR MEMO
  Wait Window 'Fix Style File, Style # ' + m.Style NoWait
  =SEEK('S'+m.Scale,'SCALE')
  lnScaleCnt  = SCALE.Cnt
  llUpdate = .F.
  For  lnCount = 1 To 8
    lcCount = STR(lnCount,1)
    IF lnCount > lnScaleCnt .AND. M.Stk&lcCount # 0
      M.Stk&lcCount = 0
      llUpdate = .T.
    ENDIF
  ENDFOR
  IF llUpdate
    m.TotStk =  M.Stk1+M.Stk2+M.Stk3+M.Stk4+M.Stk5+M.Stk6+M.Stk7+M.Stk8
    M.nstkval  = Ave_Cost*m.TotStk
    GATHER MEMVAR MEMO
    IF !SEEK(m.Style,lcTmpStyle)
      Insert Into (lcTmpStyle) From Memvar
    ENDIF
  ENDIF
ENDSCAN


SELECT STYDYE
SET RELATION TO STYLE INTO STYLE
SCAN
  SCATTER MEMVAR MEMO
  Wait Window 'Fix StyDye File, Style # ' + m.Style NoWait  
  =SEEK('S'+Style.Scale,'SCALE')
  lnScaleCnt  = SCALE.Cnt
  llUpdate = .F.
  For  lnCount = 1 To 8
    lcCount = STR(lnCount,1)
    IF lnCount > lnScaleCnt .AND. M.Stk&lcCount # 0
      M.Stk&lcCount = 0
      llUpdate = .T.
    ENDIF
  ENDFOR
  IF llUpdate
    M.TotStk   =  M.Stk1+M.Stk2+M.Stk3+M.Stk4+M.Stk5+M.Stk6+M.Stk7+M.Stk8
    M.nstkval  = Ave_Cost * M.TotStk
    GATHER MEMVAR MEMO
  
    IF !SEEK(m.Style,lcTmpStyle)
      Insert Into (lcTmpStyle) From Memvar
    ENDIF
    
  ENDIF

ENDSCAN
SELECT STYDYE
SET RELATION TO

SELECT RETLINE
Locate
STORE 0 To lnTotPcs , lnGrosamt , lnAmount , lnDisc_amt
lcStatus = ''
lcrmemo  = cRmemo
SCAN For cstygrade = '3'
  IF SEEK(cRmemo,'RETHDR') .AND.  RETHDR.Status # 'V'
    IF lcrmemo # cRmemo .AND. llUpdate
      = lfUpdHdr ()
      STORE 0 To lnTotPcs , lnGrosamt , lnAmount , lnDisc_amt
      lcrmemo  = cRmemo
    ENDIF 
  
   llUpdate = .F.
    SCATTER MEMVAR MEMO
    Wait Window 'Fix RETLINE File, C/M # ' + m.cRmemo NoWait
    = SEEK(m.cRetSty,'STYLE')
    =SEEK('S'+Style.Scale,'SCALE')
    lnScaleCnt  = SCALE.Cnt
    llUpdate = .F.
    For  lnCount = 1 To 8
      lcCount = STR(lnCount,1)
      IF lnCount > lnScaleCnt .AND. M.Qty&lcCount # 0
        M.Qty&lcCount = 0
        llUpdate = .T.
      ENDIF
    ENDFOR
    IF llUpdate
      m.TotQty   =  M.Qty1+M.Qty2+M.Qty3+M.Qty4+M.Qty5+M.Qty6+M.Qty7+M.Qty8
      m.Amount   = Price * m.TotQty
      m.Disc_amt = (Gros_Price * M.TotQty) * Disc_pcnt / 100
      GATHER MEMVAR MEMO
      *-- Variable to Update the header file.
      lnTotPcs   = lnTotPcs + TotQty
      lnAmount   = lnAmount + Amount
      lnDisc_amt = lnDisc_amt + Disc_amt
      lnGrosamt  = lnGrosamt + (Gros_Price * TotQty)

      *-- Update the style inv. Journal.
      SELECT styinvjl
      *- ctrcode+coprcode+clotno+ctrtype+style+cwarecode
      IF SEEK(RETLINE.cRmemo)
        LOCATE REST WHILE ctrcode+coprcode+clotno+ctrtype+style+cwarecode = ;
                          RETLINE.cRmemo;
        FOR Style = RETLINE.cRetSty .AND. STR(lineno,4) = PADL(ALLTRIM(RETLINE.cret_linNo),4)  .AND. ctrtype = '7';
        
        IF FOUND()
          SCATTER MEMVAR MEMO
          For  lnCount = 1 To 8
            lcCount = STR(lnCount,1)
            IF lnCount > lnScaleCnt .AND. M.nStk&lcCount # 0
             M.nStk&lcCount = 0
            ENDIF
          ENDFOR
          M.nTotStk  =  M.nStk1+M.nStk2+M.nStk3+M.nStk4+M.nStk5+M.nStk6+M.nStk7+M.nStk8
          M.nstkval  =  nCost * M.nTotStk
          GATHER MEMVAR MEMO
        ENDIF
      ENDIF
      SELECT RETLINE
      IF !SEEK(m.cRetSty,lcTmpStyle)
        Insert Into (lcTmpStyle) From Memvar
      ENDIF

    ENDIF
  ENDIF
  lcStatus  = RETHDR.Status
  lcLastCrm = cRmemo
ENDSCAN

IF llUpdate  .AND. lcStatus # 'V'
  lcrmemo = lcLastCrm
  = lfUpdHdr ()
ENDIF


SELECT styinvjl
SET ORDER TO styinvjl
SELECT (lcTmpStyle)
STORE 0 TO lnPrvsqty  , lnPrvsval
SCAN
  STORE 0 TO lnPrvsqty  , lnPrvsval
  SELECT styinvjl
  =SEEK(&lcTmpStyle..Style)
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = &lcTmpStyle..Style
    Wait Window 'Update styinvjl : csession# ' + csession Nowait
    REPLACE nPrvsqty  WITH lnPrvsqty ,;
            nPrvsval  WITH lnPrvsval
    lnPrvsqty  = lnPrvsqty  + nTotStk
    lnPrvsval  = lnPrvsval + (nTotStk * nCost)
  ENDSCAN
  SELECT (lcTmpStyle)
ENDSCAN

WAIT CLEAR 
*-- Give the user Message if he Sure to do that.
*-- Message Text : 
=gfModalGen('INM00000B00000','F','ALERT',' ','Fix process completed successfully.')

USE IN (lcTmpStyle)
ERASE (gcWorkDir+lcTmpStyle+".DBF")
ERASE (gcWorkDir+lcTmpStyle+".CDX")

=lfClosFile()

*-- End OF Code
*:***************************************************************************
FUNCTION lfUpdHdr
Private lnAlias

lnAlias = SELECT (0)

SELECT RETHDR
IF SEEK(lcrmemo)
  REPLACE Pieces     WITH lnTotPcs  ,;
          Gross_Amt  WITH lnGrosamt ,;
          Disc_Amt   WITH lnDisc_amt,;
          Amount     WITH lnAmount  ,;
          TotCredit  WITH lnAmount
        
ENDIF
SELECT (lnAlias )
*- End OF lfUpdHdr
*:***************************************************************************

FUNCTION lfOpenFile
PRIVATE lReturnFlg
lReturnFlg =.T.

llReturn1 =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
llReturn2 =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
llReturn3 =gfOpenFile(gcDataDir+'STYINVJL',gcDataDir+'MFGOPR','SH')
llReturn4 =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
llReturn5 =gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
llReturn6 =gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINE','SH')

IF !llReturn1 .OR. !llReturn2 .OR. !llReturn3 .OR. !llReturn4 .OR. !llReturn5 ;
   .OR. !llReturn6
   lReturnFlg = .F.
ENDIF

RETURN lReturnFlg
*-- End Of lfOpenFile
*:***************************************************************************

FUNCTION lfClosFile

=gfCloseFile('STYLE')
=gfCloseFile('STYDYE')
=gfCloseFile('STYINVJL')
=gfCloseFile('SCALE')
=gfCloseFile('RETHDR')
=gfCloseFile('RETLINE')

*-- End Of lfOpenFile
*:***************************************************************************
