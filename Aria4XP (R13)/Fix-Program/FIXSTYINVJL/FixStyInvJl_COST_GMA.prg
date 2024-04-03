lcDatDir = GETDIR('','Select Company DBFs folder')
IF EMPTY(lcDatDir) OR !DIRECTORY(ALLTRIM(lcDatDir))
  MESSAGEBOX("Invalid company DBFs folder.")
  RETURN .F.
ENDIF
set step on 
lcDatDir  = ADDBS(lcDatDir)
USE (lcDatDir+"STYINVJL.DBF") SHARED ORDER STYINVJL   && STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6)
USE (lcDatDir+"STYINVJL.DBF") SHARED again IN 0 ALIAS 'STYINVA' ORDER MFGOPR   && CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE
USE (lcDatDir+"GLDIST.DBF") SHARED IN 0 ORDER  GLDISTNO   && TRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY
USE (lcDatDir+"GLBATCH.DBF") SHARED IN 0 ORDER  BATCHNO
USE (lcDatDir+"GLTRNSHD.DBF") SHARED IN 0 ORDER  BATCHTRN&& cbatchno+ctranno
USE (lcDatDir+"GLTRNSDT.DBF") SHARED IN 0 ORDER  BATCHTRN&& cbatchno+ctranno
USE (lcDatDir+"GLPTRNDT.DBF") SHARED IN 0 ORDER  BATCHTRN&& cbatchno+ctranno
USE (lcDatDir+"GLPTRNHD.DBF") SHARED IN 0 ORDER  BATCHTRN && cbatchno+ctranno
SELECT DISTINCT STYLE , cWareCode FROM STYINVJL WHERE !DELETED() AND nCost > 1000 and year(dtrdate)>=2020 INTO CURSOR 'STYLES' ORDER BY STYLE,cWareCode
SELECT 'STYLES'
LOCATE
SCAN 
  lnTotStk  = 0
  lnTotStkVal = 0
  lnAveCost = 0
  llFirst = .T.
  SELECT STYINVJL
  IF SEEK(STYLES.STYLE+STYLES.cWareCode)
    SCAN REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = STYLES.STYLE+STYLES.cWareCode FOR !DELETED()
      lnOldStkVal =  STYINVJL.nSTKVAL
      IF llFirst AND CIRTYPE = 'R'
        llFirst = .F.
        lnAveCost  = STYINVJL.NCOST
        lnTotStk  = lnTotStk +  nTotStk 
        lnTotStkVal = lnTotStkVal +  nTotStk * lnAveCost 
        LOOP 
      ELSE
        lnAveCost =  IIF(lnTotStk <> 0 , ROUND(lnTotStkVal / lnTotStk ,3),lnAveCost)
      ENDIF
      IF  (STYINVJL.CIRTYPE = 'I' AND !'Auto cost adj.' $ STYINVJL.reference ) OR (STYINVJL.CIRTYPE = 'R' AND ('Auto cost adj.' $ STYINVJL.reference))
	    REPLACE nprvsqty WITH lnTotStk  ,;
	              nprvsval  with lnTotStkVal in  STYINVJL 
        lnTotStk  = lnTotStk +  nTotStk 
        lnTotStkVal = lnTotStkVal +  nTotStk * lnAveCost  
	    REPLACE nCost WITH lnAveCost,;
	            nStkVal WITH lnAveCost * ntotstk in  STYINVJL 
      ELSE
        IF STYINVJL.CIRTYPE = 'I' AND 'Auto cost adj.' $ STYINVJL.reference 
           =SEEK(styinvjl.ctrcode,'STYINVA')
           SELECT STYINVA
           LOCATE REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE =styinvjl.ctrcode FOR cirtype ='R' AND dtrdate =  styinvjl.dtrdate AND ;
           csession  =  styinvjl.csession  AND !'Auto cost adj.' $ STYINVJL.reference AND STYLE  = styinvjl.STYLE  AND cWareCode  = styinvjl.cWareCode  
           IF FOUND()
             REPLACE nprvsqty WITH lnTotStk  ,;
	                 nprvsval  with lnTotStkVal in  STYINVJL 
             lnTotStk  = lnTotStk +  nTotStk 
             lnTotStkVal = lnTotStkVal +  nTotStk * STYINVA.nCost
             REPLACE nCost WITH STYINVA.nCost,;
                     nStkVal WITH STYINVA.nCost * ntotstk in  STYINVJL 
           ENDIF 
         else
           if styinvjl.ctrtype = '4'          
             =SEEK(styinvjl.ctrcode,'STYINVA')
             SELECT STYINVA
             LOCATE REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE =styinvjl.ctrcode FOR cirtype ='I' AND ctrType = '3' AND ;
             !'Auto cost adj.' $ STYINVJL.reference AND STYLE  = styinvjl.STYLE  AND cWareCode  = styinvjl.cWareCode  
             IF FOUND()
               REPLACE nprvsqty WITH lnTotStk  ,;
	                   nprvsval  with lnTotStkVal in  STYINVJL 
	           lnTotStk  = lnTotStk +  nTotStk 
               lnTotStkVal = lnTotStkVal +  nTotStk * STYINVA.nCost
               REPLACE nCost WITH STYINVA.nCost,;
                     nStkVal WITH STYINVA.nCost * ntotstk in  STYINVJL         
             ENDIF
           endif
        ENDIF
      ENDIF
      lcTranType = ''
      DO CASE 
        CASE styinvjl.ctrtype = '1'
          lcTranType = 'IA'  
        CASE styinvjl.ctrtype = '3'
          lcTranType = 'IN'  
        CASE styinvjl.ctrtype = '4'
          lcTranType = 'VI'  
        CASE styinvjl.ctrtype = '0'
          lcTranType = 'VR' 
        CASE styinvjl.ctrtype = '2'
          lcTranType = 'IP'            
      ENDCASE  
      if !Empty(lcTranType )  and styinvjl.ctrtype $ '1234'
      select  GLDIST
      = SEEK(styinvjl.ctrcode+lcTranType )
      SCAN REST WHILE TRAN_NO+TRAN_TYPE+GLSESSION+CATG_KEY = styinvjl.ctrcode + lcTranType FOR ABS(nglAmount ) = ABS(lnOldStkVal) 
        REPLACE nglAmount WITH IIF(GLDIST.nglAmount < 0 ,-1 ,1) * ABS(STYINVJL.nstkval) IN GLDIST
        IF GLDIST.CCURRCODE = 'USD'
          REPLACE neqvamnt WITH IIF(neqvamnt < 0 , -1 * ABS(STYINVJL.nstkval),ABS(STYINVJL.nstkval)) IN GLDIST
        ENDIF   
        if GLDIST.POSTED ='X'
          if seek(GLDIST.glbatch,'GLBATCH')
            if GLBATCH.cbatstat  = 'P'
              if seek(GLDIST.glbatch+GLDIST.ctrnsledn,'GLPTRNDT')
                Select GLPTRNDT
                locate rest while cbatchno+ctranno = GLDIST.glbatch+GLDIST.ctrnsledn for cacctcode  = GLDIST.GLACCOUNT
                if found()
                  repl namount with namount - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL)
                  if seek(GLDIST.glbatch+GLDIST.ctrnsledn,'GLPTRNHD')
                    if GLPTRNDT.cdrorcr ='C'
                      repl NTRNTOTCR with NTRNTOTCR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLPTRNHD
                    else
                      repl NTRNTOTDR with NTRNTOTDR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLPTRNHD
                    endif
                  ENDIF
                endif  
              ENDIF
            ENDIF
            if seek(GLDIST.glbatch+GLDIST.ctrnsledn,'GLTRNSDT')
                Select GLTRNSDT
                locate rest while cbatchno+ctranno = GLDIST.glbatch+GLDIST.ctrnsledn for cacctcode  = GLDIST.GLACCOUNT
                if found()
                  repl namount with namount - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL)
                  if seek(GLDIST.glbatch+GLDIST.ctrnsledn,'GLTRNSHD')
                    if GLTRNSDT.cdrorcr ='C'
                      repl NTRNTOTCR with NTRNTOTCR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLTRNSHD
                      repl NBATOTCR with NBATOTCR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLBATCH
                    else
                      repl NTRNTOTDR with NTRNTOTDR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLTRNSHD
                      repl NBATOTDR with NBATOTDR - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLBATCH
                    endif
                    try
                    repl NBATCNTOT with NBATCNTOT - abs(lnOldStkVal) + abs(STYINVJL.nSTKVAL) in GLBATCH
                    catch
                    endtry 
                  ENDIF
                endif  
            ENDIF
            
          ENDIF
        ENDIF
      ENDSCAN 
   ENDIF
  ENDSCAN  
   ENDIF  
ENDSCAN 
close ALL
messagebox('STYINVJL Cost is fixed.')