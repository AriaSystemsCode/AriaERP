*---- Fix Program to Fix Fabric Field in style file For Cathy Daniel
*--- By SSH 
*--- Due to B#604892
SET TALK ON
USE H:\ARIA27\dbfs\01\style.dbf Again alias "Style27" SHARED in 0
SET ORDER TO 1 IN STYLE27
USE G:\ARIA\data1\style.dbf aGAIN ALIAS "style20" SHARED in 0
set order to 1 in style20

USE H:\ARIA27\dbfs\01\fabric.dbf Again alias "fabric27" SHARED in 0
select fabric27
index on citem tag citm of h:\citm.cdx
SELECT STYLE27
SCAN
  m.Season = SUBSTR(style,1,AT('-',STYLE)-1)
  m.Style  = SUBSTR(style,AT('-',STYLE)+1,7)
  m.Color  = SUBSTR(style,AT('-',STYLE)+10,3)
  IF SEEK(alltrim(m.Season+m.Style+m.Color),"STYLE20")
    lcKeey = STYLE20.Item_Seas +"-"+ STYLE20.Item_Style +"-"+ STYLE20.Item_Patt
    wait window "new" nowait
    lc2Rep = IIF(SEEK(lcKeey,'Fabric27'),Fabric27.Fabric,"")
  else
    lc2Rep = ""
  ENDIF
  WAIT WINDOW 'FIXING STYLE # ' + STYLE27.STYLE NOWAIT
  REPLACE STYLE27.FABRIC WITH lc2Rep
ENDSCAN

SELECT FABRIC27
DELETE TAG CITEM