*- T20140424.0006 
*- I forgot to add the scale KKA 
* Tarek 
*---------------------
CLOSE DATABASES
USE STYLE IN 0
USE STYDYE IN 0

lcWH = 'DAL   '

*- create a temp cursor to loop over to avoid key updates problems
SELECT * FROM style WHERE scale = 'KKC' INTO CURSOR STYLE_KKC

SELECT STYLE_KKC
SCAN FOR SCALE = 'KKC'
  SCATTER MEMVAR memo
  =lfPopulateVars()
  *T20140424.0006 TMI 08/03/2014 11:44 [Start] 
  WAIT WINDOW NOWAIT M.STYLE
  *T20140424.0006 TMI 08/03/2014 11:44 [End  ] 
  
  IF !SEEK(M.STYLE,'STYLE','STYLE')
    INSERT INTO STYLE FROM MEMVAR 
  ENDIF
  
  IF seek(lcWH+STYLE_KKC.style,'stydye','stydyew') 
    SELECT stydye
    SCATTER MEMVAR memo
    
    =lfPopulateVars()   
    IF !SEEK(lcWH + m.Style ,'stydye','stydyew')     
      INSERT INTO stydye FROM memvar
    ENDIF 
    
  ENDIF 
ENDSCAN


*--------------------------------------------------------------------------------------------
*  lfPopulateVars
*--------------------------------------------------------------------------------------------
FUNCTION lfPopulateVars

  m.STYLE = LEFT(M.STYLE,18)+'A'
  m.Scale = 'KKA'

  m.TOTPLAN = m.PLAN1     
  STORE 0 TO m.PLAN2     ,m.PLAN3     ,m.PLAN4     ,m.PLAN5     ,m.PLAN6     ,m.PLAN7     ,m.PLAN8     ,;
         m.ORD1      ,m.ORD2      ,m.ORD3      ,m.ORD4      ,m.ORD5      ,m.ORD6      ,m.ORD7      ,m.ORD8      ,TOTORD    ,;
         m.WIP1      ,m.WIP2      ,m.WIP3      ,m.WIP4      ,m.WIP5      ,m.WIP6      ,m.WIP7      ,m.WIP8      ,TOTWIP    ,;
         m.STK1      ,m.STK2      ,m.STK3      ,m.STK4      ,m.STK5      ,m.STK6      ,m.STK7      ,m.STK8      ,TOTSTK    ,;
         m.ALO1      ,m.ALO2      ,m.ALO3      ,m.ALO4      ,m.ALO5      ,m.ALO6      ,m.ALO7      ,m.ALO8      ,TOTALO    ,;
         m.SHP1      ,m.SHP2      ,m.SHP3      ,m.SHP4      ,m.SHP5      ,m.SHP6      ,m.SHP7      ,m.SHP8      ,TOTSHP    ,;
         m.RET1      ,m.RET2      ,m.RET3      ,m.RET4      ,m.RET5      ,m.RET6      ,m.RET7      ,m.RET8      ,TOTRET    ,;
         m.RA1       ,m.RA2       ,m.RA3       ,m.RA4       ,m.RA5       ,m.RA6       ,m.RA7       ,m.RA8       ,TOTRA     ,;
         m.INTRANS1  ,m.INTRANS2  ,m.INTRANS3  ,m.INTRANS4  ,m.INTRANS5  ,m.INTRANS6  ,m.INTRANS7  ,m.INTRANS8  ,TOTINTRN  ,;
         m.NWO1      ,m.NWO2      ,m.NWO3      ,m.NWO4      ,m.NWO5      ,m.NWO6      ,m.NWO7      ,m.NWO8      ,NTOTWO    ,;
         m.NSTKVAL
  
  m.cadd_user = 'TMI@ARIA'
  M.DADD_DATE = DATE()
  M.CADD_TIME = TIME()
  M.CEDIT_USER = ''
  M.CEDIT_TIME = ''
  M.DEDIT_DATE = {}
