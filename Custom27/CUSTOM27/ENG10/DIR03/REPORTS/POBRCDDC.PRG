*:***************************************************************************
*: Program file  : POBRCDDC.PRG
*: Program desc. : CUSTOMIZED Bar Code for DCC 
*: Date          : 12/04/2005
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Order (PO)
*: Developer     : Mostafa Mahmoud Rawash (MMR)
*: Tracking Job Number: C130331
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO POBRCDDC
*:***************************************************************************
IF lcPoTyp = "M"
  RETURN
ENDIF  
PRIVATE lnClrLen , lnClrPos
Store 0 TO lnClrLen , lnClrPos,lnStyLnGl,lnStyPosGl,lnRec1,lnRec2
PRIVATE lcOldSelect
lcOldSelect=select()
DIMENSION laTempStru[1,4]
STORE SPACE(0) TO lcClrDesc,laTempStru
=lfClrDesc()
lcTmpBars = gfTempName()
lnFildLen = AFIELDS(laTempStru)
*-- Add Code fields.
DIMENSION laTempStru[lnFildLen + 12, 4]
laTempStru[lnFildLen + 1, 1] = 'cClrDesc'
laTempStru[lnFildLen + 1, 2] = 'C'
laTempStru[lnFildLen + 1, 3] = 30
laTempStru[lnFildLen + 1, 4] = 0

laTempStru[lnFildLen + 2, 1] = 'cSty'
laTempStru[lnFildLen + 2, 2] = 'C'
laTempStru[lnFildLen + 2, 3] = 35
laTempStru[lnFildLen + 2, 4] = 0

laTempStru[lnFildLen + 3, 1] = 'cStyDesc'
laTempStru[lnFildLen + 3, 2] = 'C'
laTempStru[lnFildLen + 3, 3] = 33
laTempStru[lnFildLen + 3, 4] = 0

laTempStru[lnFildLen + 4, 1] = 'cPo'
laTempStru[lnFildLen + 4, 2] = 'C'
laTempStru[lnFildLen + 4, 3] = 6
laTempStru[lnFildLen + 4, 4] = 0

laTempStru[lnFildLen + 5, 1] = 'cClrDesc1'
laTempStru[lnFildLen + 5, 2] = 'C'
laTempStru[lnFildLen + 5, 3] = 30
laTempStru[lnFildLen + 5, 4] = 0

laTempStru[lnFildLen + 6, 1] = 'cSty1'
laTempStru[lnFildLen + 6, 2] = 'C'
laTempStru[lnFildLen + 6, 3] = 35
laTempStru[lnFildLen + 6, 4] = 0

laTempStru[lnFildLen + 7, 1] = 'cStyDesc1'
laTempStru[lnFildLen + 7, 2] = 'C'
laTempStru[lnFildLen + 7, 3] = 33
laTempStru[lnFildLen + 7, 4] = 0

laTempStru[lnFildLen + 8, 1] = 'cPo1'
laTempStru[lnFildLen + 8, 2] = 'C'
laTempStru[lnFildLen + 8, 3] = 6
laTempStru[lnFildLen + 8, 4] = 0

laTempStru[lnFildLen + 9, 1] = 'cClrDesc2'
laTempStru[lnFildLen + 9, 2] = 'C'
laTempStru[lnFildLen + 9, 3] = 30
laTempStru[lnFildLen + 9, 4] = 0

laTempStru[lnFildLen + 10, 1] = 'cSty2'
laTempStru[lnFildLen + 10, 2] = 'C'
laTempStru[lnFildLen + 10, 3] = 35
laTempStru[lnFildLen + 10, 4] = 0

laTempStru[lnFildLen + 11, 1] = 'cStyDesc2'
laTempStru[lnFildLen + 11, 2] = 'C'
laTempStru[lnFildLen + 11, 3] = 33
laTempStru[lnFildLen + 11, 4] = 0

laTempStru[lnFildLen + 12, 1] = 'cPo2'
laTempStru[lnFildLen + 12, 2] = 'C'
laTempStru[lnFildLen + 12, 3] = 6
laTempStru[lnFildLen + 12, 4] = 0

CREATE DBF (gcWorkDir+lcTmpBars) ;
   FROM ARRAY laTempStru
SELECT(lcOldSelect)
USE   
SELECT (lcDetail)
GO TOP
IF !EMPTY(lcFstPo) AND !EMPTY(lcLstPo)
    SEEK lcPoTyp + lcFstPo
    lcWlExpr = "cStyType = lcPoTyp AND Po<=lcLstPo"
ELSE
    SEEK lcPoTyp
    lcWlExpr = "cStyType = lcPoTyp"
ENDIF
lnLbCount = 1  
SCAN REST WHILE &lcWlExpr FOR &lcRpExp
  lcStyle = PADR(POSLN.Style,lnStyLen)
  lcStyCd = lfRemSepr(lcStyle)
  =SEEK(lcStyle,'STYLE')
  =SEEK('S'+STYLE.Scale,'SCALE')
  FOR I=1 TO SCALE.Cnt
    lcSz = STR(I,1)
    IF &lcDetail..Qty&lcSz > 0
      lcZDesc = SCALE.SZ&lcSz 
      lcStySz = lcStyle + lcSz
      lnQty   = CEILING(POSLN.Qty&lcSz * (lnPerc/100))
      SELECT (lcTmpBars)
      FOR lnQtyCount = 1 TO lnQty
        IF llLibInst
          IF lnLBCount =1
            APPEND BLANK
          ENDIF
          DO CASE
            CASE lnlbCount =1
              REPLACE cCode    WITH lcStySz,;
                      cStyle   WITH lcStyCd+lcSz,; 
                      cClrDesc WITH gfCodDes(SUBSTR(STYLE.Style,lnClrPos,lnClrLen) , 'COLOR'),;
                      cStyDesc WITH LEFT(STYLE.DESC1,33),;
                      cSty     WITH LEFT(ALLTRIM(STYLE.cStyMajor),12)+' '+IIF(EMPTY(SCALE.cDIM1) AND EMPTY(SCALE.cDIM2),'',LEFT(ALLTRIM(SCALE.cDim1),4))+' '+lcZDesc,; 
                      cPo      WITH EVAL(lcDetail+'.PO')
            CASE lnlbCount =2
              REPLACE cCode1    WITH lcStySz,;
                      cStyle1   WITH lcStyCd+lcSz,; 
                      cClrDesc1 WITH gfCodDes(SUBSTR(STYLE.Style,lnClrPos,lnClrLen) , 'COLOR'),;
                      cStyDesc1 WITH LEFT(STYLE.DESC1,33),;
                      cSty1     WITH LEFT(ALLTRIM(STYLE.cStyMajor),12)+' '+IIF(EMPTY(SCALE.cDIM1) AND EMPTY(SCALE.cDIM2),'',LEFT(ALLTRIM(SCALE.cDim1),4))+' '+lcZDesc,;
                      cPo1      WITH EVAL(lcDetail+'.PO')
            CASE lnlbCount =3          
              REPLACE cCode2    WITH lcStySz,;
                      cStyle2   WITH lcStyCd+lcSz,; 
                      cClrDesc2 WITH gfCodDes(SUBSTR(STYLE.Style,lnClrPos,lnClrLen) , 'COLOR'),;
                      cStyDesc2 WITH LEFT(STYLE.DESC1,33),;
                      cSty2     WITH LEFT(ALLTRIM(STYLE.cStyMajor),12)+' '+IIF(EMPTY(SCALE.cDIM1) AND EMPTY(SCALE.cDIM2),'',LEFT(ALLTRIM(SCALE.cDim1),4))+' '+lcZDesc,;
                      cPo2      WITH EVAL(lcDetail+'.PO')
          ENDCASE
          lnlbCount = lnlbCount  + 1
          IF lnlbCount = 4
            lnlbCount = 1
          ENDIF
        ELSE
          SELECT (lcTmpBars)
          APPEND BLANK
          REPLACE cCode    WITH lcStySz,;
                  cStyle   WITH lcStyCd+lcSz,; 
                  cClrDesc WITH gfCodDes(SUBSTR(STYLE.Style,lnClrPos,lnClrLen) , 'COLOR'),;
                  cStyDesc WITH LEFT(STYLE.DESC1,33),;
                  cSty     WITH LEFT(ALLTRIM(STYLE.cStyMajor),12)+' '+IIF(EMPTY(SCALE.cDIM1) AND EMPTY(SCALE.cDIM2),'',LEFT(ALLTRIM(SCALE.cDim1),4))+' '+lcZDesc,;
                  cPo      WITH EVAL(lcDetail+'.PO')
        ENDIF
      ENDFOR
    ENDIF
  ENDFOR  
ENDSCAN
WAIT CLEAR

SELECT (lcTmpBars)
GO TOP
*!*************************************************************
*! Name      : lfClrDesc
*! Developer : Mostafa Rawash
*! Date      : 12/04/2005
*! Purpose   : GET Style,Color lengths
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfClrDesc()
*!*************************************************************
FUNCTION lfClrDesc
*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--end function lfClrDesc
