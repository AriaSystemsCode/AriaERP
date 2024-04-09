SET CPDIALOG OFF
SET RESOURCE OFF
SET TALK OFF
_Screen.Visible = .F.
CLOSE ALL

lcAria27SysFiles = GETDIR('',"Select Aria27 SysFiles Folder")
IF !DIRECTORY(lcAria27SysFiles) OR !FILE(ADDBS(lcAria27SysFiles)+'syfrmcdh.dbf')
  MESSAGEBOX('Invalid SysFiles Folder')
  RETURN .F.
ENDIF
lcAria27SysFiles = ADDBS(lcAria27SysFiles)
USE lcAria27SysFiles +'Syccomp.dbf' SHAR IN 0 ORDER 1
SELECT SYCCOMP
SCAN FOR lrunfroma4
  lcConnStr = "Driver={SQL Server};server="+ALLTRIM(SYCCOMP.CCONSERVER)+";DATABASE="+ALLTRIM(SYCCOMP.CCONDBNAME)+;
    ";uid="+ALLTRIM(SYCCOMP.CCONUSERID)+";pwd="+ALLTRIM(SYCCOMP.CCONPASWRD)
  lnConHand=SQLSTRINGCONNECT(lcConnStr)
  IF lnConHand>0
    lnBomHeader = SQLEXEC(lnConHand,"select * from bomheadr where ccosttype <>'P' AND CCSTSHTMPL <>''",'BomHeadr')
    IF lnBomHeader>0
      SELECT 'BomHeadr'
      SCAN
        lcCostShtTmp = BomHeadr.CCSTSHTMPL
        lnBomFile =  SQLEXEC(lnConHand,"Select * From BOM where CINVTYPE ='"+BomHeadr.CINVTYPE +;
          "' AND CITMMAJOR = '"+BomHeadr.CITMMAJOR +"' AND CCSTSHTTYP ='"+;
          BomHeadr.CCSTSHTTYP +"' AND CCSTSHT_ID='"+BomHeadr.CCSTSHT_ID+"'",'BomFile')
        SELECT 'BomFile'
        INDEX ON CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) TAG 'BomFile'
        lnTmpBomHeadr = SQLEXEC(lnConHand,"select * from bomheadr where ccosttype ='P' AND ccstsht_id='"+lcCostShtTmp+"'",'TmpBomHeadr')
        lnTmpBomFile =  SQLEXEC(lnConHand,"Select * From BOM where CINVTYPE ='"+TmpBomHeadr.CINVTYPE +;
          "' AND CITMMAJOR = '"+TmpBomHeadr.CITMMAJOR +"' AND CCSTSHTTYP ='"+;
          TmpBomHeadr.CCSTSHTTYP +"' AND CCSTSHT_ID='"+TmpBomHeadr.CCSTSHT_ID+"'",'TmpBomFile')


        lnCstElmFile  =SQLEXEC(lnConHand,"select * from cstelmfm where ccosttype ='P' AND ccstelmtmp='"+lcCostShtTmp+"'",'Tmpcstelmfm')
        SELECT 	'Tmpcstelmfm'
        INDEX ON STR(nrltDcstno,6) TAG 'Tmpcstelm'
        SELECT 'TmpBomFile'
        INDEX ON CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID+TYP+CITMMASK+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) TAG 'TmpBomFile'
        SELECT 'BomFile'
        SCAN
          IF SEEK(CINVTYPE+SPACE(19)+CCSTSHTTYP+lcCostShtTmp+TYP+'            -******'+MFGCODE+CINVTYPC+ITEM+STR(NLINENO,6) , 'TmpBomFile')
            SELECT 	'Tmpcstelmfm'
            
            IF SEEK(STR(BomFile.NLINENO,6))
              SCAN REST WHILE STR(nrltDcstno,6) = STR(BomFile.NLINENO,6)
                m.ccosttype = BomHeadr.ccosttype
                m.CCSTSHTTYP = BomHeadr.CCSTSHTTYP 
                m.CINVTYPE = BomHeadr.CINVTYPE
                m.CITMMAJOR =  BomHeadr.CITMMAJOR
                m.CCSTSHT_ID = BomHeadr.CCSTSHT_ID
                lnCstElmUp = SQLEXEC(lnConHand,"INSERT INTO [cstelmfm] ([cadd_time],[cadd_user],[caffectatt]"+;
                  ",[cattrval1],[cattrval2],[cattrval3],[cattrval4],[cbasedon],[ccstelemno],[ccstelmtmp]"+;
                  ",[cformulaln],[clineno],[clok_time],[clok_user],[coperator],[dadd_date],[dlok_date]"+;
                  ",[lclsbrac],[llok_stat],[lopnbrac],[nbomtotqty],[nclsbrac],[nopnbrac],[npercent]"+;
                  ",[nrltdcstno],[nuntcost],[ccosttype],[ccstshttyp],[cinvtype],[citmmajor],[ccstsht_id])"+;
                  " VALUES('"+Tmpcstelmfm.cadd_time+"','"+Tmpcstelmfm.cadd_user+"','"+Tmpcstelmfm.caffectatt +"','"+;
                  Tmpcstelmfm.cattrval1+"','"+Tmpcstelmfm.cattrval2+"','"+Tmpcstelmfm.cattrval3+"','"+;
                  Tmpcstelmfm.cattrval4+"','"+Tmpcstelmfm.cbasedon+"','"+Tmpcstelmfm.ccstelemno+"','"+;
                  Tmpcstelmfm.ccstelmtmp+"','"+Tmpcstelmfm.cformulaln+"','"+Tmpcstelmfm.clineno+"','"+;
                  Tmpcstelmfm.clok_time+"','"+Tmpcstelmfm.clok_user+"','"+Tmpcstelmfm.coperator+"','"+;
                  DTOS(Tmpcstelmfm.dadd_date)+"','"+DTOS(Tmpcstelmfm.dlok_date)+"','"+IIF(Tmpcstelmfm.lclsbrac,"1","0")+"','"+;
                  IIF(Tmpcstelmfm.llok_stat,"1","0")+"','"+IIF(Tmpcstelmfm.lopnbrac, "1","0")+"','"+STR(Tmpcstelmfm.nbomtotqty,7,3)+"','"+;
                  STR(Tmpcstelmfm.nclsbrac, 1,0)+"','"+STR(Tmpcstelmfm.nopnbrac, 1,0)+"','"+STR(Tmpcstelmfm.npercent, 5,2)+"','"+;
                  STR(Tmpcstelmfm.nrltDcstno, 6,0)+"','"+STR(Tmpcstelmfm.nuntcost, 11,3)+"','"+m.ccosttype+"','"+;
                  m.CCSTSHTTYP+"','"+m.CINVTYPE+"','"+m.CITMMAJOR+"','"+m.CCSTSHT_ID+"')")
              ENDSCAN
            ENDIF
          ENDIF  
          ENDSCAN
        ENDSCAN
        *USE IN 'BomHeadr'
        =SQLDISCONNECT(lnConHand)
      ENDIF
    ENDIF
  ENDSCAN

  MESSAGEBOX("CSTELMFM is updated Successfully")
