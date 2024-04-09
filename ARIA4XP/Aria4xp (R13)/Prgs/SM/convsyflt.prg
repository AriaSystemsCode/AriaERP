LPARAMETERS lcA27Sys
SET STEP ON 
USE ADDBS(lcA27Sys)+'syurpflt.DBF' SHAR ALIAS 'syurpflt_A' IN 0
SELECT syurpflt_A
SCAN FOR !DELETED()
  WAIT WINDOW 'Updating Reports Filters table' NOWAIT
  lnDelConnHandler= 0
  SCATTER MEMO MEMVAR 
  lcSelectStat = "Select * from SYURPFLT  where cRep_ID= '"+m.cRep_ID+"' AND cFltr_ID='"+STRTRAN(m.cFltr_ID,"'","''")+"' AND cUser_ID='"+m.cUser_ID+"'"
  lnRemoteResultSel = oAriaApplication.RemoteSystemData.Execute(lcSelectStat ,;
                                  '',"SYURPFLT1","",oAriaApplication.Aria5SystemManagerConnection,;
                                  3,"",SET("Datasession" ))  
  IF lnRemoteResultSel > 0 AND EOF("SYURPFLT1")                                 
    lcInsStatement = "INSERT INTO SYURPFLT (cRep_ID,cFltr_ID,cFltr_Des,lAccByAll,lsys_data,cUser_ID,lDefault,lselect,MREPHDFLT,mRepFxFlt,mRepVrFlt,mUsrDVar) VALUES "+;
                     "('"+m.cRep_ID+;
                     "','"+STRTRAN(m.cFltr_ID,"'","''")+"','"+STRTRAN(m.cFltr_Des,"'","''")+"',"+IIF(m.lAccByAll,"1","0")+",0,'"+m.cUser_ID+"',"+IIF(m.lDefault ,"1","0")+","+;
                     IIF(m.lselect,"1","0")+",?m.MREPHDFLT,?m.mRepFxFlt,?m.mRepVrFlt,?m.mUsrDVar)"
    lnRemoteResult = oAriaApplication.RemoteSystemData.Execute(lcInsStatement ,;
                                  '',"SYURPFLT1","",oAriaApplication.Aria5SystemManagerConnection,;
                                  3,"",SET("Datasession" ),.T., @lnDelConnHandler)  
    IF lnRemoteResult > 0
      SQLCOMMIT(lnDelConnHandler)
    ELSE
      =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnRemoteResult ,.T.)
      SQLROLLBACK(lnDelConnHandler)
    ENDIF 
  ENDIF    
ENDSCAN
USE IN 'syurpflt_A'
