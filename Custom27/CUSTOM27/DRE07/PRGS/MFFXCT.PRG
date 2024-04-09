*:**************************************************************************
*: Program     : ARFXGL.PRG
*: Date        : 08/12/2003
*: Description : Fix the missing dyelot, issued and used in the CTKTBOM and BOMLINE
*: Developer   : Khalid Mohi El-Din (KHM)
*:**************************************************************************
*
PRIVATE lcDataDir
lcDataDir = GETDIR('','Select Company Data.')
IF EMPTY(lcDataDir) OR !FILE(lcDataDir+'CTKTBOM.DBF')
  WAIT WINDOW 'Wrong company data directory!' TIMEOUT 3
  RETURN 
ENDIF

*-- Open needed files.
*-- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
IF !USED('CTKTBOM')
  USE (lcDataDir+'cTktBom.DBF') IN 0 SHARE ORDER TAG cTktBom
ENDIF

*-- cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr
IF !USED('BomLine')
  USE (lcDataDir+'BomLine.DBF') IN 0 SHARE ORDER TAG Mfgopr
ENDIF
*-- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession
IF !USED('MatInvJl')
  USE (lcDataDir+'MatInvJl.DBF') IN 0 SHARE ORDER TAG MatInvJl
ENDIF
SELECT cTktBom
IF SEEK ("M")
  SCAN REST WHILE cIMTyp+CutTkt+Typ+Item+IClr+MfgCode+Dyelot = "M" ;
            FOR cCatgTyp $ "FT" AND Issue_Qty = 0
     
     lcTktNo  = CutTkt
     lcFabClr = LEFT(Item,7)+IClr       
     
     *-- cfabric+ccolor+cwarecode+cdyelot+crsession+cisession     
     IF SEEK(lcFabClr, 'MatInvJl')
       SELECT MatInvJl
       LOCATE REST WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = lcFabClr;
                   FOR cIMTyp = "M" AND cTranType = "4" AND cTran = lcTktNo AND !EMPTY(cDyelot)
       IF FOUND()
         m.Dyelot = cDyelot
         SUM REST nIssued, nIssued - nReceived TO lnIssue, lnUsed ;
             WHILE cFabric+cColor+cWareCode+cDyelot+cRSession+cISession = lcFabClr;
             FOR cIMTyp = "M" AND cTranType = "4" AND cTran = lcTktNo AND cDyelot = m.Dyelot
       
         SELECT cTktBom
         REPLACE Issue_Qty WITH lnIssue,;
                 Used_Qty  WITH lnUsed ,;
                 Dyelot    WITH m.Dyelot
         SCATTER MEMVAR
                 
       *-- cimtyp+ctktno+coprcode+ctype+style+sclr+IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr
         IF SEEK("M"+lcTktNo,'BomLine')
           SELECT BomLine
           LOCATE REST WHILE cIMTyp+cTktNo+cOprCode+cType+Style+SClr+;
                             IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr = "M"+lcTktNo;
                       FOR cCatgTyp = m.cCatgTyp AND Item = m.Item AND IClr = m.IClr
           
           IF FOUND()
             REPLACE REST Dyelot WITH m.Dyelot;
                     WHILE cIMTyp+cTktNo+cOprCode+cType+Style+SClr+;
                          IIF(ccatgtyp$"MDP",PADR(mfgcode,12),item)+iclr = "M"+lcTktNo;
                     FOR cCatgTyp = m.cCatgTyp AND Item = m.Item AND IClr = m.IClr           
           ENDIF
         ENDIF
       ENDIF            
     ENDIF
            
  ENDSCAN
ENDIF