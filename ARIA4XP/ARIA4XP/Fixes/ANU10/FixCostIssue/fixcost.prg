_SCREen.Visible = .F. 
lcSysFilesDir = GETDIR('','Select SysFiles Folder')
IF !DIRECTORY(lcSysFilesDir)
  MESSAGEBOX("Invalid SysFiles Folder")
  RETURN .F.
ENDIF

lcSysFilesDir = ADDBS(lcSysFilesDir)
USE (lcSysFilesDir +'SYCCOMP.DBF') IN 0 SHARED 
SELECT syccomp
SCAN FOR lrunfroma4
  lcConnStr = "Driver={SQL Server};server="+ALLTRIM(CCONSERVER)+";DATABASE="+ALLTRIM(CCONDBNAME)+;
                ";uid="+ALLTRIM(CCONUSERID)+";pwd="+ALLTRIM(CCONPASWRD)
  lnConHand = SQLSTRINGCONNECT(lcConnStr)
  IF lnConHand >=1
    * HES include some other fabrics
*!*	    lnLocSel =SQLEXEC(lnConHand,"Select DISTINCT Style,cWareCode from ITEMLOC WHERE STyle in ('79I8D       -WIN','79I8D       -BLK','79I8D       -WHT')",'ItemLoc')    
    lnLocSel =SQLEXEC(lnConHand,"Select DISTINCT Style,cWareCode from ITEMLOC WHERE STyle in ('79I2D       -CAC','79I2D       -SHI','79I2D       -WAL','79I2D       -SLT','79I8D       -PIA','79I8D       -POP','79I8D       -WIN','79I8D       -BLK','79I8D       -WHT')",'ItemLoc')
    * HES
    IF lnLocSel>0
      SELECT 'ItemLoc'
      SCAN 
        lcStyle = ItemLoc.Style
        lcLoc =  ItemLoc.cWareCode
        lnItmJrlSel =SQLEXEC(lnConHand,"Select * from ITEMJRNL Where Style =?m.lcStyle and cWareCode =?m.lcLoc order by DTRDATE,csession",'ItemJrnl')
        IF  lnItmJrlSel > 0
          SELECT ItemJrnl
          m.nCost = 0
          llUpdate = .F.
          SCAN 
            IF cIRTYPE = 'R'
              m.nCost  = ItemJrnl.nCost
              llUpdate = .T.
            ELSE
              IF llUpdate AND ABS(ItemJrnl.nCost - m.nCost)>0
                m.Rec_NO = ItemJrnl.Rec_NO 
                m.oldCost = ItemJrnl.nCost
                m.cISession = ItemJrnl.cISession
                STRTOFILE("Fabric:"+lcStyle+"   CWareCode:"+lcLoc+" New Cost="+STR(m.nCost ,13,3)+" RecNo:"+ m.Rec_NO+"  Session:"+ItemJrnl.cISession+chr(13)+CHR(10                ),ADDBS(FullpaTH(""))+"Result.Txt",1)
                lnItmJrlSel =SQLEXEC(lnConHand,"Update ITEMJRNL Set nCost = ?m.nCost ,nStkVal = ?m.nCost* ntotstk,cEdit_user =?m.oldCost Where Style =?m.lcStyle and cWareCode =?m.lcLoc and rec_no = ?m.Rec_NO and cISession = ?m.cISession",'Result')              
              ENDIF
            ENDIF
          ENDSCAN 
        ENDIF 
      ENDSCAN 
    ENDIF
  ENDIF
ENDSCAN 
MESSAGEBOX("Done,Pease check file:"+ADDBS(FullpaTH(""))+"Result.Txt")