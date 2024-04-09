**
PROCEDURE lpUpdateTableStructure
 LPARAMETERS lca27sysfiles, lbindexonly, sydfiles, sydflfld, sydfield, sydindex, updtables
 SET STEP ON
 USE SHARED (lca27sysfiles)+'\syccomp.dbf' ALIAS syccomp IN 0
 SELECT syccomp
 LOCATE
 SET CLASSLIB TO "main.vcx"
 objremotedataaccess = CREATEOBJECT("remotedataaccess")
 ndatasessionid = SET("Datasession")
 SCAN FOR lrunfroma4=.T.
    lccompid = ccomp_id
    lcconnstr = lpcreateconnstr(ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd)
    lnhandle = SQLSTRINGCONNECT(lcconnstr)
    SELECT (updtables)
    LOCATE
    SCAN
       lctable = cfile_nam
       lcfile_ttl = cfile_ttl
       WAIT WINDOW TIMEOUT 1 "Update "+lctable+"-"+ALLTRIM(lcfile_ttl)+" In company "+lccompid+", Please Wait."
       IF  .NOT. lbindexonly=.T.
          SET KEY TO PADR(ALLTRIM(lctable), 30) IN (sydflfld)
          objremotedataaccess.mcreatetable(lccompid, lctable, lcfile_ttl, sydflfld, sydfield, sydindex, ndatasessionid, lnhandle, .F., lcconnstr)
       ENDIF
       SET KEY TO PADR(ALLTRIM(lctable), 30) IN (sydindex)
       objremotedataaccess.mcreateindex(lccompid, lctable, lcfile_ttl, sydindex, ndatasessionid, lnhandle, lcconnstr)
    ENDSCAN
    SELECT syccomp
 ENDSCAN
 RELEASE objremotedataaccess
 RELEASE CLASSLIB "main.vcx"
 USE IN syccomp
ENDPROC
**
FUNCTION lpCreateConnStr
 LPARAMETERS ccondriver, cconserver, ccondbname, cconuserid, cconpaswrd
 LOCAL lcconnstr
 lcconnstr = ""
 DO CASE
    CASE ccondriver='SQL'
       lcconnstr = "Driver={SQL Server};server="+ALLTRIM(cconserver)+";DATABASE="+ALLTRIM(ccondbname)+";uid="+ALLTRIM(cconuserid)+";pwd="+ALLTRIM(cconpaswrd)
    CASE ccondriver='FOX'
       lcconnstr = "Driver={Microsoft Visual FoxPro Driver};UID=;PWD=;SourceDB="+ALLTRIM(ccondbname)+";SourceType=DBF;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;"
 ENDCASE
 RETURN lcconnstr
ENDFUNC
**
FUNCTION gfSubStr
 PARAMETER lcstring, lnaryorpos, lcsepta
 lcsubstr = ' '
 lnarydim = 1
 lnaryrows = 1
 lnarycols = 1
 lcsepta = IIF(TYPE('lcSepta')='C', lcsepta, ',')
 IF LEN(ALLTRIM(lcsepta))>1
    lccolsep = SUBSTR(lcsepta, 2, 1)
    lcsepta = LEFT(lcsepta, 1)
    lnarydim = IIF(OCCURS(lcsepta, lcstring)>0, OCCURS(lcsepta, lcstring)+IIF(RIGHT(lcstring, 1)<>lcsepta, 1, 0), lnarydim)
    lnarycols = IIF(OCCURS(lccolsep, lcstring)>0, OCCURS(lccolsep, lcstring)+IIF(RIGHT(lcstring, 1)<>lccolsep, 1, 0), lnarydim)
    lnaryrows = (lnarydim+(lnarycols-1))/lnarycols
    lnarydim = lnarydim+(lnarycols-1)
    lcstring = STRTRAN(lcstring, lccolsep, lcsepta)
 ELSE
    lnarydim = IIF(OCCURS(lcsepta, lcstring)>0, OCCURS(lcsepta, lcstring)+IIF(RIGHT(lcstring, 1)<>lcsepta, 1, 0), lnarydim)
 ENDIF
 DO CASE
    CASE TYPE('lnAryOrPos')='U'
       lnaryorpos = 1
    CASE TYPE('lnAryOrPos')$'C,L'
       IF lnarycols>1
          DIMENSION lnaryorpos[lnaryrows, lnarycols]
       ELSE
          IF ALEN(lnaryorpos, 2)>0
             DIMENSION lnaryorpos[lnarydim, ALEN(lnaryorpos, 2)]
          ELSE
             DIMENSION lnaryorpos[lnarydim]
          ENDIF
       ENDIF
       lnaryorpos = ' '
 ENDCASE
 FOR lnarelem = 1 TO lnarydim
    IF TYPE('lnAryOrPos')='N'
       lnarelem = lnaryorpos
    ENDIF
    DO CASE
       CASE lnarelem=1
          lcsubstr = SUBSTR(lcstring, 1, IIF(lcsepta$lcstring, AT(lcsepta, lcstring)-1, LEN(lcstring)))
       CASE lnarelem=lnarydim
          lcsubstr = SUBSTR(lcstring, AT(lcsepta, lcstring, lnarelem-1)+1)
          lcsubstr = IIF(RIGHT(lcsubstr, 1)=lcsepta, SUBSTR(lcsubstr, 1, LEN(lcsubstr)-1), lcsubstr)
       CASE lnarelem>1
          lcsubstr = SUBSTR(lcstring, AT(lcsepta, lcstring, lnarelem-1)+1, AT(lcsepta, lcstring, lnarelem)-AT(lcsepta, lcstring, lnarelem-1)-1)
    ENDCASE
    IF TYPE('lnAryOrPos')='N'
       RETURN lcsubstr
    ENDIF
    IF lnarycols>1
       lnaryorpos[(MOD((lnarelem-1), lnaryrows))+1, INT((lnarelem-1)/lnaryrows)+1] = lcsubstr
    ELSE
       lnaryorpos[lnarelem] = lcsubstr
    ENDIF
 ENDFOR
ENDFUNC
**
