*** 
*** ReFox XII  #UK122952  David Stephenson  Aria Systems Ltd [VFP90]
***
 PARAMETER lcrequestid, lcxmlfilename, clientid
 IF TYPE('lcXMLFileName')='C'
    PUBLIC gcrequestid, gcclientid
    gcrequestid = lcrequestid
    gcclientid = clientid
    PRIVATE loagent
    loagent = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.AriaRequestAgent")
    PRIVATE loprogress
    loprogress = CREATEOBJECT("Aria.DataTypes.RequestHandler.AriaRequestProgress")
    loprogress.percent = 0
    loprogress.description = "Opening Data Files..."
    loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
    LOCAL loenvironment
    loenvironment = CREATEOBJECT("Aria.Environment.AriaEnviromentVariables")
    loenvironment.clientid = clientid
    LOCAL lccurrentprocedure
    lccurrentprocedure = loenvironment.aria40sharedpath
    loenvironment.connectionsrefresh()
    LOCAL lcrequestcompany, lcclientroot, lcenvoutput
    lcrequestcompany = loagent.getrequestcompany(lcrequestid, clientid)
    lcclientroot = loenvironment.aria40sharedpath
    lcenvoutput = loenvironment.getaria27companydataconnectionstring(lcrequestcompany)
    DO (lccurrentprocedure+"SRVPRGS\SY\ariamain.fxp") WITH lcrequestcompany, clientid, lccurrentprocedure, loenvironment
    oariaenvironment.xml.restorefromxml(FILETOSTR(lcxmlfilename), .T.)
    PUBLIC gcact_appl
    gcact_appl = 'PO'
    oariaenvironment.report.gcact_appl = 'PO'
    oariaenvironment.activemoduleid = 'PO'
    oariaenvironment.requestid = lcrequestid
    IF LEFT(gcdevice, 7)="PRINTER"
       oariaenvironment.gcdevice = "PRINTER"
    ELSE
       oariaenvironment.gcdevice = "FILE"
    ENDIF
    oariaenvironment.report.ccrorientation = 'L'
    SET STEP ON
    llogfltch = .T.
    lfwrepwhen()
 ELSE
    loogscroll.ccrorientation = 'L'
 ENDIF
 IF llrpuppl
    CREATE CURSOR 'StyleScle' (stycode C (19), cnt N (3), fit C (10))
    SELECT 'StyleScle'
    INDEX ON stycode TAG 'StyleScle'
 ENDIF
 IF llrpaupj
    ldprjfrm = DATE(YEAR(DATE()), MONTH(DATE()), 1)-1
 ENDIF
 IF llogfltch
    lcrppurc = ''
    lnpospur = ASCAN(laogfxflt, "LCRPPURC")
    IF lnpospur>0
       lnpospur = ASUBSCRIPT(laogfxflt, lnpospur, 1)
       lcrppurc = laogfxflt(lnpospur, 6)
    ENDIF
    IF TYPE('lcXMLFileName')='C'
       ldprjfrm = DATE()
    ENDIF
    IF llrpaupj
       ldprjfrm = DATE(YEAR(DATE()), MONTH(DATE()), 1)-1
    ENDIF
    IF lfcolctdata()
       SELECT (lcacttemp)
    ELSE
       IF TYPE('lcXMLFileName')<>'C'
          = gfmodalgen('TRM00052B40011', 'ALERT')
       ENDIF
       RETURN
    ENDIF
 ENDIF
 SELECT (lcacttemp)
 IF llrpspzr
    LOCATE
    SCAN
       lnnewordertot = 0
       FOR lncntord = 1 TO 20
          lnretvalue = lfgetneword(lncntord)
          lnnewordertot = lnnewordertot+IIF(TYPE('lnRetValue')<>'N', 0, lnretvalue)
       ENDFOR
       IF lnnewordertot=0
          DELETE
       ENDIF
    ENDSCAN
    LOCATE
 ENDIF
 IF llrpuppl
    IF  .NOT. USED('Style_Up')
       = gfopentable("Style", "Style", 'SH', 'Style_Up')
    ENDIF
    SELECT (lcacttemp)
    LOCATE
    SCAN FOR  .NOT. DELETED()
       SELECT 'StyleScle'
       LOCATE FOR SUBSTR(stycode,1,LEN(stycode)-1) = SUBSTR(&lcacttemp..STYLE ,1,LEN(stylescle.stycode)-1) AND fit =  &lcacttemp..fit AND !DELETED()
       IF FOUND()
          lncntsz = 1
          SCAN REST FOR SUBSTR(stycode,1,LEN(stycode)-1) = SUBSTR(&lcacttemp..STYLE ,1,LEN(stylescle.stycode)-1) AND fit =  &lcacttemp..fit AND !DELETED()
             IF gfseek(stylescle.stycode, 'Style_Up', 'Style')
                SELECT 'Style_Up'
                FOR lni = 1 TO stylescle.cnt
                   lci = STR(lni, 1)
                   REPLACE plan&lci. WITH IIF(EVAL(lcacttemp+'.nSlsMon') > 0, CEILING(EVAL(lcacttemp+'.TOTSZ'+ALLTRIM(STR(lncntsz))) / EVAL(lcacttemp+'.nSlsMon')), 0)
                   lncntsz = lncntsz+1
                ENDFOR
                REPLACE totplan WITH plan1+plan2+plan3+plan4+plan5+plan6+plan7+plan8
                = gfreplace('')
             ENDIF
          ENDSCAN
       ENDIF
    ENDSCAN
    SELECT 'Style_Up'
    = gftableupdate()
    SELECT (lcacttemp)
    LOCATE
 ENDIF
 IF TYPE('lcXMLFileName')='C'
    oariaenvironment.report.oglastform = lcrpform
    loprogress.percent = 0.9 
    loprogress.description = "Printing Report..."
    loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
    PRIVATE loproxy
    loproxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")
    oariaenvironment.report.print(oariaenvironment.report.oglastform)
    loprogress.percent = 1.0 
    loprogress.description = "Printing Report..."
    loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
 ELSE
    = gfdispre()
 ENDIF
 
**
PROCEDURE lfwrepwhen
 IF  .NOT. USED('STYLE')
    = gfopentable(oariaapplication.datadir+'STYLE', oariaapplication.datadir+'STYLE', 'SH')
 ENDIF
 IF  .NOT. USED('SCALE')
    = gfopentable(oariaapplication.datadir+'SCALE', oariaapplication.datadir+'SCALE', 'SH')
 ENDIF
 IF  .NOT. USED('SCALEHD')
    = gfopentable(oariaapplication.datadir+'SCALEHD', oariaapplication.datadir+'EXTSCALE', 'SH')
 ENDIF
 IF  .NOT. USED('ORDLINE')
    = gfopentable(oariaapplication.datadir+'ORDLINE', oariaapplication.datadir+'ORDLINES', 'SH')
 ENDIF
 IF  .NOT. USED('ORDCANLN')
    = gfopentable(oariaapplication.datadir+'ORDCANLN', oariaapplication.datadir+'ORDCANLN', 'SH')
 ENDIF
 IF  .NOT. USED('ITEM')
    = gfopentable(oariaapplication.datadir+'ITEM', oariaapplication.datadir+'STYLE', 'SH')
 ENDIF
 IF  .NOT. USED('BOM')
    = gfopentable('BOM', 'MULTIBOM')
 ENDIF
 lndivpos = lfcheckfilter(3, "STYLE.CDIVISION", 2)
 lnpatpos = lfcheckfilter(3, "STYLE.PATTERN", 2)
 lnsespos = lfcheckfilter(3, "STYLE.SEASON", 2)
 lnpurpos = lfcheckfilter(3, "STYLE.CPURCODE", 2)
 lngropos = lfcheckfilter(3, "STYLE.CSTYGROUP", 2)
 lnfabpos = lfcheckfilter(1, "ITEM.CSTYMAJOR", 2)
ENDPROC
**
FUNCTION lfcheckfilter
 LPARAMETERS lnarraytype, lcfilter, lnrettyp
 LOCAL lcreturn, lnpos
 IF TYPE('lcXMLFileName')='C'
    DO CASE
       CASE lnarraytype=1
          lnpos = ASCAN(laogfxflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(laogfxflt, lnpos, 1)
             lcreturn = laogfxflt(lnpos, 6)
          ENDIF
       CASE lnarraytype=2
          lnpos = ASCAN(laoghdflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(laoghdflt, lnpos, 1)
             lcreturn = laoghdflt(lnpos, 6)
          ENDIF
       CASE lnarraytype=3
          lnpos = ASCAN(laogvrflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(laogvrflt, lnpos, 1)
             lcreturn = laogvrflt(lnpos, 6)
          ENDIF
    ENDCASE
 ELSE
    DO CASE
       CASE lnarraytype=1
          lnpos = ASCAN(loogscroll.laogfxflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(loogscroll.laogfxflt, lnpos, 1)
             lcreturn = loogscroll.laogfxflt(lnpos, 6)
          ENDIF
       CASE lnarraytype=2
          lnpos = ASCAN(loogscroll.laoghdflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(loogscroll.laoghdflt, lnpos, 1)
             lcreturn = loogscroll.laoghdflt(lnpos, 6)
          ENDIF
       CASE lnarraytype=3
          lnpos = ASCAN(loogscroll.laogvrflt, lcfilter)
          IF lnpos>0
             lnpos = ASUBSCRIPT(loogscroll.laogvrflt, lnpos, 1)
             lcreturn = loogscroll.laogvrflt(lnpos, 6)
          ENDIF
    ENDCASE
 ENDIF
 IF lnrettyp=1
    RETURN lcreturn
 ELSE
    RETURN lnpos
 ENDIF
ENDFUNC
**
FUNCTION lfvldprjmn
 IF lnmnprj>=0 .AND. lnmnprj<=12
    RETURN
 ELSE
    gfmodalgen('TRM34215B00000', 'ALERT')
    lnmnprj = _SCREEN.activeform.activecontrol.oldvalue
    RETURN .F.
 ENDIF
ENDFUNC
**
FUNCTION lfvldslsmn
 IF lnmnsls>0 .AND. lnmnsls<=12
    RETURN
 ELSE
    gfmodalgen('TRM34215B00000', 'ALERT')
    lnmnsls = _SCREEN.activeform.activecontrol.oldvalue
    RETURN .F.
 ENDIF
ENDFUNC
**
PROCEDURE lfcretstytemps
 IF USED(lcmnthtemp) .AND. RECCOUNT(lcmnthtemp)>0
    SELECT (lcmnthtemp)
    USE IN (lcmnthtemp)
 ENDIF
 IF  .NOT. USED(lcmnthtemp)
    lni = 1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'STYLE'
    latempstru1[lni, 2] = 'C'
    latempstru1[lni, 3] = 18
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'FIT'
    latempstru1[lni, 2] = 'C'
    latempstru1[lni, 3] = 10
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'GROUP'
    latempstru1[lni, 2] = 'C'
    latempstru1[lni, 3] = 1
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'YEAR'
    latempstru1[lni, 2] = 'N'
    latempstru1[lni, 3] = 4
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'CYEAR'
    latempstru1[lni, 2] = 'C'
    latempstru1[lni, 3] = 2
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'MONTH'
    latempstru1[lni, 2] = 'N'
    latempstru1[lni, 3] = 2
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'CMONTH'
    latempstru1[lni, 2] = 'C'
    latempstru1[lni, 3] = 9
    latempstru1[lni, 4] = 0
    lni = ALEN(latempstru1, 1)+1
    DIMENSION latempstru1[lni, 4]
    latempstru1[lni, 1] = 'TOTMONTH'
    latempstru1[lni, 2] = 'N'
    latempstru1[lni, 3] = 6
    latempstru1[lni, 4] = 0
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru1, 1)+1
       DIMENSION latempstru1[lni, 4]
       latempstru1[lni, 1] = 'BOK'+lcx
       latempstru1[lni, 2] = 'N'
       latempstru1[lni, 3] = 6
       latempstru1[lni, 4] = 0
    ENDFOR
    DIMENSION laindeces[1, 2]
    laindeces[1, 1] = 'STYLE+FIT+GROUP+STR(YEAR,4)+STR(MONTH,2)'
    laindeces[1, 2] = 'STYMNTH'
    = gfcrttmp(lcmnthtemp, @latempstru1, @laindeces)
 ENDIF
 IF USED(lcacttemp) .AND. RECCOUNT(lcacttemp)>0
    SELECT (lcacttemp)
    USE IN (lcacttemp)
 ENDIF
 IF  .NOT. USED(lcacttemp)
    lni = 1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'STYLE'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 18
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'STY_DESC'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 60
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'GROUP'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 1
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'HASMORE'
    latempstru[lni, 2] = 'L'
    latempstru[lni, 3] = 1
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'STY_COLR'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 60
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TOT_SIZ'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 2
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'FIT'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 10
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'YEAR'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 4
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'MONTH'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 2
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'PATTERN'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 10
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'SUPPLIER'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 19
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'DATE'
    latempstru[lni, 2] = 'D'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'USER_ID'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 10
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'COMPANY'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 2
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'CRTN_QTY'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 3
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TOTSTK'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 6
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TOTWIP'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 6
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TOTSZS'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 6
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'DIM1DESC'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 5
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'STYCOMP'
    latempstru[lni, 2] = 'C'
    latempstru[lni, 3] = 12
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'dProduce'
    latempstru[lni, 2] = 'D'
    latempstru[lni, 3] = 8
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'nSlsMon'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 4
    latempstru[lni, 4] = 0
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'nstyfrsprj'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 2
    latempstru[lni, 4] = 0
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru, 1)+1
       DIMENSION latempstru[lni, 4]
       latempstru[lni, 1] = 'SIZ'+lcx
       latempstru[lni, 2] = 'C'
       latempstru[lni, 3] = 5
       latempstru[lni, 4] = 0
    ENDFOR
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru, 1)+1
       DIMENSION latempstru[lni, 4]
       latempstru[lni, 1] = 'STK'+lcx
       latempstru[lni, 2] = 'N'
       latempstru[lni, 3] = 6
       latempstru[lni, 4] = 0
    ENDFOR
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru, 1)+1
       DIMENSION latempstru[lni, 4]
       latempstru[lni, 1] = 'WIP'+lcx
       latempstru[lni, 2] = 'N'
       latempstru[lni, 3] = 6
       latempstru[lni, 4] = 0
    ENDFOR
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru, 1)+1
       DIMENSION latempstru[lni, 4]
       latempstru[lni, 1] = 'TOTSZ'+lcx
       latempstru[lni, 2] = 'N'
       latempstru[lni, 3] = 6
       latempstru[lni, 4] = 0
    ENDFOR
    FOR lnx = 1 TO 20
       lcx = ALLTRIM(STR(lnx))
       lni = ALEN(latempstru, 1)+1
       DIMENSION latempstru[lni, 4]
       latempstru[lni, 1] = 'UKWIP'+lcx
       latempstru[lni, 2] = 'N'
       latempstru[lni, 3] = 6
       latempstru[lni, 4] = 0
    ENDFOR
    lni = ALEN(latempstru, 1)+1
    DIMENSION latempstru[lni, 4]
    latempstru[lni, 1] = 'TOTUKWIP'
    latempstru[lni, 2] = 'N'
    latempstru[lni, 3] = 6
    latempstru[lni, 4] = 0
    DIMENSION laindeces1[1, 2]
    laindeces1[1, 1] = 'PATTERN+STYLE+FIT+GROUP'
    laindeces1[1, 2] = 'STYINF'
    = gfcrttmp(lcacttemp, @latempstru, @laindeces1)
 ENDIF
 SELECT (lcacttemp)
 SET RELATION TO STYLE+fit+GROUP INTO &lcmnthtemp
 SET SKIP TO &lcmnthtemp
ENDPROC
**
PROCEDURE lfaddmonth
 PARAMETER lcstyle, lnmonth, lnyear, lcfit
 LOCAL lnx, lnz, lny, lddate
 lddate = DATE(lnyear, lnmonth, 1)
 lcyear = SUBSTR(STR(lnyear, 4), 3, 2)
 lddate = DATE(lnyear, lnmonth, 14)
 lcmonth = CMONTH(lddate)
 SELECT (lcmnthtemp)
 APPEND BLANK
 REPLACE style WITH lcstyle, month WITH lnmonth, year WITH lnyear, fit WITH lcfit, cmonth WITH lcmonth, cyear WITH lcyear, group WITH '0'
 lcscl = RIGHT(lcstyle, 2)
 SELECT scale
 SET ORDER TO SCALE
 = SEEK('S'+lcscl)
 DIMENSION lascl[1, 2]
 STORE '' TO lascl
 lnlen = 1
 lncnt = 1
 SCAN REST FOR scale.cscl_desc=lcfit WHILE type+scale='S'+lcscl
    DIMENSION lascl[lnlen, 2]
    lascl[lnlen, 1] = scale.scale
    lascl[lnlen, 2] = lncnt
    lncnt = lncnt+scale.cnt
    lnlen = lnlen+1
 ENDSCAN
 SELECT ordcanln
 SET ORDER TO ORDCANLN
 SELECT ordline
 SET ORDER TO ORDLINES
 SET RELATION TO "S"+scale INTO scale
 SET RELATION ADDITIVE TO 'O'+ordline.order+STR(ordline.lineno, 6) INTO ordcanln
 LOCATE
 gfseek(lcstyle)
 lnbokcntr = 0
 lcy = ''
 lcc = ''
 lcz = ''
 llexit = .F.
 lcbok = ''
 FOR lnc = 1 TO lngrpsszs
    lcbok = 'm.BOK'+ALLTRIM(STR(lnc))
    STORE 0 TO &lcbok
 ENDFOR
 m.totmonth = 0
 lcchfit = lcfit
 llerror = .F.
 lncntr = 0
 IF  .NOT. USED('Ordhdr_ENT')
    = gfopentable('ORDHDR', 'ORDHDR', 'SH', 'Ordhdr_ENT')
 ENDIF
 SELECT ordline
 SCAN REST FOR cordtype='O' .AND. scale.cscl_desc=lcfit WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno, 6)=lcstyle
    = gfseek(ordline.cordtype+ordline.order, 'Ordhdr_ENT')
    IF ( .NOT. (MONTH(ordhdr_ent.entered)=lnmonth .AND. YEAR(ordhdr_ent.entered)=lnyear)) .OR. ordhdr_ent.status='X'
       LOOP
    ENDIF
    lnpos = ASCAN(lascl, ordline.scale)
    lnbokcntr = lascl(lnpos+1)
    FOR lnz = 1 TO scale.cnt
       IF lnbokcntr>20
          REPLACE totmonth WITH m.totmonth
          m.totmonth = 0
          APPEND BLANK
          lncntr = lncntr+1
          REPLACE style WITH lcstyle, month WITH lnmonth, year WITH lnyear, fit WITH lcfit, cmonth WITH lcmonth, cyear WITH lcyear, group WITH ALLTRIM(STR(lncntr))
          lnbokcntr = 1
       ENDIF
       lcy = ALLTRIM(STR(lnbokcntr))
       lcz = ALLTRIM(STR(lnz))
       m.bok&lcy = ordline.book&lcz - ordcanln.qty&lcz
       SELECT (lcmnthtemp)
       REPLACE bok&lcy WITH bok&lcy + m.bok&lcy
       m.totmonth = m.totmonth + m.bok&lcy
       lnbokcntr = lnbokcntr+1
    ENDFOR
    IF llerror
       EXIT
    ENDIF
 ENDSCAN
 lnselected = SELECT()
 = gfseek(lcstyle, 'Style', 'Style')
 lcscale = SUBSTR(style.scale, 1, lnscalelen)
 SELECT bom
 = gfsqlrun("Select * From BOM where Item Like '"+SUBSTR(lcstyle, 1, lnmajlen)+"%' AND CCATGTYP='S'", 'BOM')
 SELECT scale
 = gfseek('S'+lcscale)
 DIMENSION lastylescale[1, 4]
 lastylescale = ''
 lncntsty = 1
 SCAN REST FOR scale.cscl_desc=lcfit WHILE type+scale+prepak='S'+lcscale
    lcitemscale = scale.scale
    SELECT bom
    SCAN FOR '~'+lcitemscale$mszcrosref .AND. (SUBSTR(bom.item, lcclrpos, lnclrlen)=REPLICATE('*', lnclrlen) .OR. SUBSTR(bom.item, lcclrpos, lnclrlen)=SUBSTR(lcstyle, lcclrpos, lnclrlen))
       lccrosreffld = mszcrosref
       DIMENSION lasizesarr[1]
       lasizesarr = ''
       = gfsubstr(lccrosreffld, @lasizesarr, CHR(13))
       FOR lnx = 1 TO ALEN(lasizesarr, 1)
          IF  .NOT. '~'+lcitemscale$lasizesarr(lnx)
             LOOP
          ENDIF
          DIMENSION lastylescale[lncntsty, 4]
          lastylescale[lncntsty, 1] = PADR(ALLTRIM(bom.citmmask), lnmajlen)
          lastylescale[lncntsty, 2] = IIF(SUBSTR(bom.citmmask, lcclrpos, lnclrlen)=REPLICATE('*', lnclrlen), SUBSTR(lcstyle, lcclrpos, lnclrlen), SUBSTR(bom.citmmask, lcclrpos, lnclrlen))
          lastylescale[lncntsty, 3] = lasizesarr(lnx)
          lastylescale[lncntsty, 4] = bom.nestbomqty
          lncntsty = lncntsty+1
       ENDFOR
    ENDSCAN
 ENDSCAN
 IF  .NOT. EMPTY(lastylescale(1, 1))
    FOR lnf = 1 TO ALEN(lastylescale, 1)
       lcscanstyle = PADR(lastylescale(lnf, 1), lnmajlen)+lcclrsep+PADR(lastylescale(lnf, 2), lnclrlen)+lcsclspr+SUBSTR(lastylescale(lnf, 3), 1, lnscllen)
       lnposcomm = ATC(',', lastylescale(lnf, 3))
       lnparentsize = VAL(SUBSTR(lastylescale(lnf, 3), lnposcomm+1, 1))
       lcparentscale = SUBSTR(lastylescale(lnf, 3), 1, lnscllen)
       lnposwng = ATC('~', lastylescale(lnf, 3))
       lnposcomm = ATC(',', lastylescale(lnf, 3), 2)
       lncompsize = VAL(SUBSTR(lastylescale(lnf, 3), lnposcomm+1, 1))
       lccompscale = SUBSTR(lastylescale(lnf, 3), lnposwng+1, lnscllen)
       SELECT ordline
       gfseek(lcscanstyle)
       SCAN REST WHILE style=lcscanstyle
          = gfseek(ordline.cordtype+ordline.order, 'Ordhdr_ENT')
          IF ( .NOT. (MONTH(ordhdr_ent.entered)=lnmonth .AND. YEAR(ordhdr_ent.entered)=lnyear)) .OR. ordhdr_ent.status='X'
             LOOP
          ENDIF
          lnpos = ASCAN(lascl, lccompscale)
          IF lnpos=0
             LOOP
          ENDIF
          lnbokcntr = lascl(lnpos+1)
          = SEEK(lcstyle+lcfit+ALLTRIM(STR(lncntr))+STR(lnyear, 4)+STR(lnmonth, 2), lcmnthtemp)
          lnz = lnparentsize
          IF lnbokcntr>20 .AND.  .NOT. SEEK(lcstyle+lcfit+ALLTRIM(STR(lncntr+1))+STR(lnyear, 4)+STR(lnmonth, 2), lcmnthtemp)
             REPLACE totmonth WITH m.totmonth
             m.totmonth = 0
             APPEND BLANK
             lncntr = lncntr+1
             REPLACE style WITH lcstyle, month WITH lnmonth, year WITH lnyear, fit WITH lcfit, cmonth WITH lcmonth, cyear WITH lcyear, group WITH ALLTRIM(STR(lncntr))
             lnbokcntr = 1
          ENDIF
          lcy = ALLTRIM(STR(lnbokcntr+lncompsize-1))
          lcz = ALLTRIM(STR(lnz))
          m.bok&lcy =(ordline.book&lcz - ordcanln.qty&lcz)*lastylescale[lnf,4]
          SELECT (lcmnthtemp)
          REPLACE bok&lcy WITH bok&lcy + m.bok&lcy
          m.totmonth = m.totmonth + m.bok&lcy
          lnbokcntr = lnbokcntr+1
          IF llerror
             EXIT
          ENDIF
       ENDSCAN
    ENDFOR
 ENDIF
 SELECT (lnselected)
 SELECT (lcmnthtemp)
 REPLACE totmonth WITH m.totmonth
ENDPROC
**
FUNCTION lfcolctdata
 lnscalelen = gfgetmemvar('M_EXTWIDTH')
 lnmajlen = LEN(gfitemmask('PM'))
 lnlststy = 0
 DIMENSION laitemseg[1]
 PRIVATE lncount, lnscllen, lnclrlen, lcclrpos, lcclrsep, lcsclspr
 STORE 0 TO lnsclpos, lcclrpos
 IF TYPE('lcXMLFileName')='C'
    itemmask = CREATEOBJECT("GetItemMask")
    = itemmask.do(@laitemseg)
 ELSE
    = gfitemmask(@laitemseg)
 ENDIF
 FOR lncount = 1 TO ALEN(laitemseg, 1)
    IF laitemseg(lncount, 1)='S'
       lnscllen = LEN(laitemseg(lncount, 3))
       lnsclpos = laitemseg(lncount, 4)
    ENDIF
    IF laitemseg(lncount, 1)='C'
       lnclrlen = LEN(laitemseg(lncount, 3))
       lcsclspr = ALLTRIM(laitemseg(lncount, 6))
       lcclrpos = laitemseg(lncount, 4)
    ENDIF
    IF laitemseg(lncount, 1)='F'
       lcclrsep = ALLTRIM(laitemseg(lncount, 6))
    ENDIF
 ENDFOR
 = lfcretstytemps()
 cstytemp = lfcheckfilter(1, "STYLE.STYLE", 1)
 lnudcnt = 0
 IF  .NOT. EMPTY(cstytemp) .AND. USED(cstytemp)
    SELECT (cstytemp)
    COUNT FOR  .NOT. DELETED() TO lnudcnt
 ENDIF
 IF EMPTY(cstytemp) .OR. (USED(cstytemp) .AND. lnudcnt=0)
    cstytemp = gftempname()
    SELECT style
    lcflterexpr = "IIF(!EMPTY(laOgVrFlt[lnDivPos,6]),CDIVISION $ laOgVrFlt[lnDivPos,6], .T.) AND "+"IIF(!EMPTY(laOgVrFlt[lnSesPos,6]),SEASON $ laOgVrFlt[lnSesPos,6], .T.) AND "+"IIF(!EMPTY(laOgVrFlt[lnGroPos,6]),CSTYGROUP $ laOgVrFlt[lnGroPos,6], .T.) AND "+"IIF(!EMPTY(laOgVrFlt[lnPatPos,6]),PATTERN $ laOgVrFlt[lnPatPos,6], .T.) AND "+"IIF(!EMPTY(laOgVrFlt[lnPurPos,6]),CPURCODE $ laOgVrFlt[lnPurPos,6], .T.)"
    lncnt = 0
    IF  .NOT. EMPTY(laogfxflt(lnfabpos, 6))
       SELECT (laogfxflt(lnfabpos, 6))
       COUNT FOR  .NOT. DELETED() TO lncnt
       IF lncnt>0
          lcflterexpr = lcflterexpr+" AND SEEK(FABRIC, laOgFxFlt[lnFabPos,6])"
       ENDIF
    ENDIF
    SELECT style
    COPY FIELDS STYLE TO (oariaapplication.workdir+cstytemp) FOR &lcflterexpr 
    USE (oariaapplication.workdir+cstytemp) IN 0 EXCLUSIVE
    SELECT &cstytemp
    INDEX ON 'STYLE' TAG &cstytemp
 ENDIF
 SELECT style
 SET RELATION TO "S"+scale INTO scale
 SET RELATION ADDITIVE TO SUBSTR(scale, 1, 2) INTO scalehd
 DIMENSION laselstyl[1]
 STORE '' TO laselstyl
 lncout = 1
 IF  .NOT. USED('ORDLINE_F')
    = gfopentable('ORDLINE', 'ORDLINES', 'SH', 'ORDLINE_F')
 ENDIF
 IF  .NOT. USED('ORDHDR_F')
    = gfopentable('ORDHDR', 'ORDHDR', 'SH', 'ORDHDR_F')
 ENDIF
 IF  .NOT. USED('SCALE_F')
    = gfopentable('SCALE', 'SCALE', 'SH', 'SCALE_F')
 ENDIF
 SELECT ordline_f
 SET RELATION TO "S"+scale INTO scale_f
 SET RELATION ADDITIVE TO cordtype+order INTO ordhdr_f
 SELECT (cstytemp)
 LOCATE
 lndatacnt = RECCOUNT(cstytemp)
 IF TYPE('lcXMLFileName')='C'
    loprogress.description = "Collecting Data..."
    loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
 ELSE
    opross = CREATEOBJECT('ariaprogressbar')
    opross.totalprogress = RECCOUNT()
    opross.autocenter = .F.
    opross.top = 0
    lnoldtop = loogscroll.parent.top
    loogscroll.parent.top = loogscroll.parent.top+opross.top+(opross.height+50)
    opross.visible = .T.
    opross.show()
    lnpreprec = 0
 ENDIF
 SCAN FOR  .NOT. EOF()
    SELECT style
    SET ORDER TO STYLE
    SEEK(&cstytemp..STYLE)
    IF style.status<>'A'
       LOOP
    ENDIF
    IF IIF(llrppjbs, style.nstyfrsprj=0, lnmnprj=0)
       LOOP
    ENDIF
    IF ASCAN(laselstyl, SUBSTR(style, 1, 18)+scale.cscl_desc)<>0
       LOOP
    ENDIF
    SELECT (cstytemp)
    m.STYLE = SUBSTR(&cstytemp..STYLE,1,LEN(&cstytemp..STYLE)-1)
    m.nstyfrsprj = IIF( .NOT. llrppjbs, lnmnprj, style.nstyfrsprj)
    IF TYPE('lcXMLFileName')='C'
       lnpercent = RECNO()/lndatacnt
       IF MOD(RECNO(), CEILING(lndatacnt/10))=0
          loprogress.percent = lnpercent*0.9 
          loprogress.description = 'Collecting Data for Style : '+m.style+' With Fit : '+scale.cscl_desc
          loagent.updateobjectprogress(lcrequestid, loprogress, clientid)
       ENDIF
    ELSE
       lnpreprec = lnpreprec+1
       opross.currentprogress(lnpreprec)
       opross.show()
       opross.caption = 'Collecting Data for Style : '+m.style+' With Fit : '+scale.cscl_desc
    ENDIF
    DIMENSION laselstyl[lncout]
    laselstyl[lncout] = m.style+scale.cscl_desc
    m.date = ldprjfrm
    m.company = oariaapplication.activecompanyid
    m.user_id = oariaapplication.user_id
    lncolpos = AT('-',&cstytemp..STYLE)
    m.sty_colr = gfcoddes(PADR(SUBSTR(&cstytemp..STYLE,14,3),6),'COLOR')
    SELECT style
    SET ORDER TO STYLE
    SEEK(&cstytemp..STYLE)
    m.pattern = style.pattern
    m.supplier = style.vendor
    m.stycomp = style.cvensty
    m.crtn_qty = style.qty_ctn
    m.sty_desc = style.desc1
    m.fit = scale.cscl_desc
    m.dim1desc = scalehd.cdim1desc
    lcalis = SELECT()
    ldproduce = {}
    SELECT ordline_f
    = gfseek(m.style)
    LOCATE REST FOR scale_f.cscl_desc=m.fit WHILE style=m.style
    IF  .NOT. FOUND()
       ldproduce = lfgetproducedatefromcomp(m.style, m.fit)
    ELSE
       SCAN REST FOR scale_f.cscl_desc=m.fit WHILE style=m.style
          IF EOF('ORDHDR_F')
             LOOP
          ENDIF
          IF EMPTY(ldproduce)
             ldproduce = ordhdr_f.entered
          ELSE
             IF ordhdr_f.entered<ldproduce
                ldproduce = ordhdr_f.entered
             ENDIF
          ENDIF
       ENDSCAN
    ENDIF
    m.dproduce = ldproduce
    IF EMPTY(ldproduce)
       LOOP
    ENDIF
    IF ldproduce>ldprjfrm
       LOOP
    ENDIF
    SELECT (lcalis)
    lny = 1
    lcy = ''
    lcx = ''
    lnnosizs = 0
    lntotstk = 0
    lntotwip = 0
    m.totukwip = 0
    STORE 0 TO m.ukwip1, m.ukwip2, m.ukwip3, m.ukwip4, m.ukwip5, m.ukwip6, m.ukwip7, m.ukwip8, m.ukwip9, m.ukwip10, m.ukwip11, m.ukwip12, m.ukwip13, m.ukwip14, m.ukwip15, m.ukwip16, m.ukwip17, m.ukwip18, m.ukwip19, m.ukwip20
    lnt = 1
    lcwipcnt = ''
    lcstkcnt = ''
    lctotszcnt = ''
    lcsizcnt = ''
    lnrecnom = RECNO()
    SCAN REST FOR scale.cscl_desc=m.fit WHILE style=m.style
       IF llrpuppl
          INSERT INTO StyleScle VALUES (style.style, scale.cnt, m.fit)
       ENDIF
       FOR lnx = 1 TO scale.cnt
          lcwipcnt = 'm.WIP'+ALLTRIM(STR(lnt))
          lcstkcnt = 'm.STK'+ALLTRIM(STR(lnt))
          lctotszcnt = 'm.TOTSZ'+ALLTRIM(STR(lnt))
          lcsizcnt = 'm.SIZ'+ALLTRIM(STR(lnt))
          lcwipuk = 'm.UKWIP'+ALLTRIM(STR(lnt))
          STORE 0 TO &lcwipuk 
          STORE 0  TO &lcwipcnt   
          STORE 0  TO &lcstkcnt   
          STORE 0  TO &lctotszcnt 
          STORE '' TO &lcsizcnt   
          lnt = lnt+1
       ENDFOR
    ENDSCAN
    GOTO lnrecnom
    lnw = 0
    lns = 0
    lnc = 1
    lnz = 0
    lnallsizes = 0
    llsecgro = .F.
    lnnewgrpszs = 0
    FOR lnm = 0 TO 100
       lcm = ALLTRIM(STR(lnm))
       IF TYPE('lntotWip&lcM') = 'N' 
          lntotwip&lcm = 0
       ELSE
          EXIT
       ENDIF
       IF TYPE('lntotStk&lcM') = 'N' 
          lntotstk&lcm = 0  
       ENDIF
       IF TYPE('lnNoSizs&lcM') = 'N' 
          lnnosizs&lcm = 0
       ENDIF
    ENDFOR
    SCAN REST FOR scale.cscl_desc=m.fit WHILE style=m.style
       lfgetukwip(style)
       FOR lnx = 1 TO scale.cnt
          lcy = ALLTRIM(STR(lny))
          lcx = ALLTRIM(STR(lnx))
          lcw = ALLTRIM(STR(lnw))
          lcs = ALLTRIM(STR(lns))
          lcz = ALLTRIM(STR(lnz))
          m.wip&lcy = STYLE.wip&lcx
          IF lnc>20
             lnnosizs&lcz = 20 
             lnnewgrpszs = (scale.cnt+1)-lnx
             llsecgro = .T.
             lnw = lnw+1
             lns = lns+1
             lnz = lnz+1
             lcw = ALLTRIM(STR(lnw))
             lcs = ALLTRIM(STR(lns))
             lcz = ALLTRIM(STR(lnz))
             lnc = 1
          ENDIF
          IF TYPE('lntotWip&lcW') = 'U' 
             lntotwip&lcw = 0
          ENDIF
          lntotwip&lcw = lntotwip&lcw + STYLE.wip&lcx
          m.stk&lcy = STYLE.stk&lcx- STYLE.ord&lcx
          IF TYPE('lntotStk&lcS') = 'U' 
             lntotstk&lcs = 0
          ENDIF
          lntotstk&lcs = lntotstk&lcs +  STYLE.stk&lcx - STYLE.ord&lcx
          SELECT posln_a
          LOCATE
          DO WHILE  .NOT. EOF()
             lckeypo = cbusdocu+cstytype+po+cinvtype+style+STR(lineno, 6)
             STORE 0 TO m.qtyuk&lcx  
             SCAN REST WHILE cbusdocu+cstytype+po+cinvtype+style+STR(lineno, 6)=lckeypo
                IF trancd<>'1'
                   m.qtyuk&lcx. = MAX(m.qtyuk&lcx.-posln_a.qty&lcx. ,0 )
                ELSE
                   m.qtyuk&lcx. = m.qtyuk&lcx. + posln_a.qty&lcx. 
                ENDIF
             ENDSCAN
             m.ukwip&lcy = m.ukwip&lcy +  m.qtyuk&lcx. 
             IF TYPE('m.TOTUKWIP')<>'N'
                m.totukwip = 0
             ENDIF
             m.totukwip = m.totukwip + m.qtyuk&lcx. 
          ENDDO
          lnc = lnc+1
          m.siz&lcy = SCALE.sz&lcx
          lny = lny+1
       ENDFOR
       IF TYPE('lnNoSizs&lcZ') = 'U'
          lnnosizs&lcz = 0
       ENDIF
       IF llsecgro
          lnnosizs&lcz = lnnewgrpszs
          llsecgro = .F.
       ELSE
          lnnosizs&lcz = lnnosizs&lcz + SCALE.CNT
       ENDIF
    ENDSCAN
    FOR lnb = 0 TO lnz
       lcb = ALLTRIM(STR(lnb))
       lnallsizes = lnallsizes + lnnosizs&lcb
    ENDFOR
    lngroups = INT((lny-1)/20)
    lngrpsszs = lnallsizes
    lnmonth = MONTH(ldprjfrm)
    lnyear = YEAR(ldprjfrm)
    FOR lny = 1 TO lnmnsls
       lncnt = lnmonth+1-lny
       IF lncnt<=0
          lnyear = lnyear-1
          m.year = lnyear
          m.month = lncnt+12
          lfaddmonth(m.style, m.month, m.year, m.fit)
          lnyear = lnyear+1
       ELSE
          m.year = lnyear
          m.month = lncnt
          lfaddmonth(m.style, m.month, m.year, m.fit)
       ENDIF
    ENDFOR
    IF MONTH(ldproduce)=MONTH(ldprjfrm) .AND. YEAR(ldproduce)=YEAR(ldprjfrm)
       lncnt = 1
    ELSE
       ldnextmnth = GOMONTH(ldproduce, 1)
       lncnt = 2
       DO WHILE MONTH(ldnextmnth)<>MONTH(ldprjfrm) .OR. YEAR(ldnextmnth)<>YEAR(ldprjfrm)
          ldnextmnth = GOMONTH(ldnextmnth, 1)
          lncnt = lncnt+1
       ENDDO
    ENDIF
    m.nslsmon = lncnt
    m.nslsmon = MIN(lnmnsls, m.nslsmon)
    lntotbokqty = 0
    lntotsizs = 0
    m.totszs = 0
    lca = ''
    lcx = ''
    lcg = ''
    FOR lng = 0 TO lngroups
       lcg = ALLTRIM(STR(lng))
       FOR lny = 1 TO lnnosizs&lcg
          lnx = lny+lng*20
          lcx = ALLTRIM(STR(lnx))
          lcy = ALLTRIM(STR(lny))
          m.wip&lcy = m.wip&lcx
          m.stk&lcy = m.stk&lcx
          m.siz&lcy = m.siz&lcx
       ENDFOR
       m.totwip  = lntotwip&lcg 
       m.totstk  = lntotstk&lcg 
       m.tot_siz = lnnosizs&lcg 
       m.group = lcg
       IF lnnosizs&lcg  < 20
          FOR x = 1 + lnnosizs&lcg TO 20
             lccx = ALLTRIM(STR(x))
             m.siz&lccx = ''
             m.wip&lccx = 0
             m.stk&lccx = 0
          ENDFOR
       ENDIF
       INSERT INTO &lcacttemp FROM MEMVAR 
    ENDFOR
    IF lnallsizes>0
       SELECT (lcacttemp)
       LOCATE
       SCAN FOR style=m.style .AND. fit=m.fit
          SKIP
          IF VAL(group)>0
             SKIP -1
             REPLACE hasmore WITH .T.
          ELSE
             SKIP -1
          ENDIF
          lngroupsz = tot_siz
          lngroupid = group
          lntotsizs = 0
          SELECT (lcmnthtemp)
          LOCATE
          FOR lny = 1 TO lngroupsz
             lcy = ALLTRIM(STR(lny))
             SUM bok&lcy TO lntotbokqty FOR STYLE = m.STYLE AND fit = m.fit AND GROUP = lngroupid
             SELECT (lcacttemp)
             REPLACE totsz&lcy WITH lntotbokqty 
             lntotsizs = lntotsizs+lntotbokqty
             SELECT (lcmnthtemp)
          ENDFOR
          SELECT (lcacttemp)
          REPLACE totszs WITH lntotsizs
       ENDSCAN
    ENDIF
    lncout = lncout+1
 ENDSCAN
 IF TYPE('lcXMLFileName')<>'C'
    opross = .NULL.
    loogscroll.parent.top = lnoldtop
 ENDIF
 SELECT (lcacttemp)
 lnrecont = RECCOUNT()
 IF lnrecont>0
    RETURN .T.
 ELSE
    RETURN .F.
 ENDIF
ENDFUNC
**
FUNCTION lfgetneword
 PARAMETER lnidx
 lnreturn1 = 0
 lcidx = IIF(lnidx<10, STR(lnidx, 1), STR(lnidx, 2))
 lnfrstvalue = &lcacttemp..wip&lcidx + &lcacttemp..stk&lcidx
 lnvalue = lnfrstvalue  / IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1)
 lnsubvalue = &lcacttemp..nstyfrsprj - lnvalue
 lnreturn1 = IIF(lnvalue < &lcacttemp..nstyfrsprj , lnsubvalue * IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1) , 0)
 IF lnreturn1 = 0 OR lnreturn1 = 0.00 OR EMPTY(&lcacttemp..siz&lcidx) OR &lcacttemp..totsz&lcidx = 0
    lnreturn1 = ''
 ENDIF
 RETURN lnreturn1
ENDFUNC
**
FUNCTION lfgetmnstk
 PARAMETER lnidx
 lnreturn2 = 0
 lcidx = IIF(lnidx<10, STR(lnidx, 1), STR(lnidx, 2))
 lnreturn2 = (&lcacttemp..wip&lcidx +  &lcacttemp..stk&lcidx) / IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1)
 IF lnreturn2 = 0 OR lnreturn2 = 0.00 OR EMPTY(&lcacttemp..siz&lcidx)
    lnreturn2 = ''
 ENDIF
 RETURN lnreturn2
ENDFUNC
**
FUNCTION lfvlddate
 IF ldprjfrm>DATE()
    gfmodalgen('TRM40135B00000', 'Alert')
    ldprjfrm = DATE()
    RETURN .F.
 ENDIF
ENDFUNC
**
PROCEDURE lfsetfun
 PARAMETER lcparam
 DO CASE
    CASE lcparam='S'
       lcfltexp = 'IIF(!EMPTY(laOgVrFlt[lnDivPos,6]),CDIVISION $ laOgVrFlt[lnDivPos,6], .T.) and '+'IIF(!EMPTY(laOgVrFlt[lnSesPos,6]),SEASON $ laOgVrFlt[lnSesPos,6], .T.)    and '+'IIF(!EMPTY(laOgVrFlt[lnGroPos,6]),CSTYGROUP $ laOgVrFlt[lnGroPos,6], .T.) and '+'IIF(!EMPTY(laOgVrFlt[lnPatPos,6]),PATTERN $ laOgVrFlt[lnPatPos,6], .T.)   and '+'IIF(!EMPTY(laOgVrFlt[lnPurPos,6]),CPURCODE $ laOgVrFlt[lnPurPos,6], .T.)  and '+'IIF(!EMPTY(laOgFxFlt[lnFabPos,6]) and RECCOUNT(laOgFxFlt[lnFabPos,6]) > 0 ,   '+'SEEK(FABRIC, laOgFxFlt[lnFabPos,6]), .T.) '
       SELECT style
       SET FILTER TO &lcfltexp
    OTHERWISE
 ENDCASE
ENDPROC
**
FUNCTION lfgetdefpur
 lcretpval = ''
 IF  .NOT. USED('CODES_A')
    = gfopentable('CODES', 'CCODE_NO', 'SH', 'CODES_A')
 ENDIF
 IF gfseek('D'+'CFRCSTGRP', 'CODES_A', 'CCODE_NO')
    lcretpval = codes_a.ccode_no
 ENDIF
 RETURN (lcretpval)
ENDFUNC
**
PROCEDURE lfgetukwip
 LPARAMETERS lcstyle
 lnoldsel = SELECT()
 IF  .NOT. USED('POSLN_A')
    = gfopentable('POSLN', 'POSLN', 'SH', 'POSLN_A')
 ENDIF
 SELECT 'POSLN_A'
 = gfsqlrun("Select POSLN.PO,POSLN.cStytype,POSLN.CINVTYPE,POSLN.STYLE,POSLN.[LINENO],POSLN.cBusdocu  ,POSLN.QTY1,POSLN.QTY2 ,POSLN.QTY3 ,POSLN.QTY4 ,POSLN.QTY5 ,POSLN.QTY6 ,POSLN.QTY7 ,POSLN.QTY8,POSLN.TRANCD "+" From POSLN INNER JOIN POSHDR ON  POSLN.cBusdocu = POSHDR.cBusdocu and POSLN.CSTYTYPE = POSHDR.cSTYTYPE"+" AND POSLN.PO = POSHDR.PO WHERE POSHDR.cStytype = 'P' and POSHDR.cBusdocu = 'P' AND POSHDR.CFRCSTGRP= '"+lcrppurc+"'   and POSLN.STYLE = '"+lcstyle+"' AND POSLN.TRANCD IN ('1','2','4','5') AND POSHDR.STATUS ='O'", 'POSLN_A')
 SELECT (lnoldsel)
ENDPROC
**
FUNCTION lfgetmnstkuk
 PARAMETER lnidx
 lnreturn2 = 0
 lcidx = IIF(lnidx<10, STR(lnidx, 1), STR(lnidx, 2))
 lnreturn2 = (&lcacttemp..ukwip&lcidx +  &lcacttemp..stk&lcidx) / IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1)
 IF lnreturn2 = 0 OR lnreturn2 = 0.00 OR EMPTY(&lcacttemp..siz&lcidx)
    lnreturn2 = ''
 ENDIF
 RETURN lnreturn2
ENDFUNC
**
FUNCTION lfgetneworduk
 PARAMETER lnidx
 lnreturn1 = 0
 lcidx = IIF(lnidx<10, STR(lnidx, 1), STR(lnidx, 2))
 lnfrstvalue = &lcacttemp..ukwip&lcidx + &lcacttemp..stk&lcidx
 lnvalue = lnfrstvalue  / IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1)
 lnsubvalue = &lcacttemp..nstyfrsprj - lnvalue
 lnreturn1 = IIF(lnvalue < &lcacttemp..nstyfrsprj , lnsubvalue * IIF(IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1) <> 0 ,  IIF(&lcacttemp..nslsmon > 0, &lcacttemp..totsz&lcidx / &lcacttemp..nslsmon, 1),1) , 0)
 IF lnreturn1 = 0 OR lnreturn1 = 0.00 OR EMPTY(&lcacttemp..siz&lcidx) OR &lcacttemp..totsz&lcidx=0
    lnreturn1 = ''
 ENDIF
 RETURN lnreturn1
ENDFUNC
**
FUNCTION lfgetproducedatefromcomp
 LPARAMETERS lcselestyle, lcselefit
 STORE {} TO ldreturndate
 = gfseek(lcselestyle, 'Style', 'Style')
 lcscale = SUBSTR(style.style, lnsclpos, lnscalelen)
 SELECT bom
 = gfsqlrun("Select * From BOM where Item Like '"+SUBSTR(lcselestyle, 1, lnmajlen)+"%' AND CCATGTYP='S'", 'BOM')
 SELECT scale_f
 = gfseek('S'+lcscale)
 DIMENSION lastylescale[1, 4]
 lastylescale = ''
 lncntsty = 1
 SCAN REST FOR scale_f.cscl_desc=lcselefit WHILE type+scale+prepak='S'+lcscale
    lcitemscale = scale_f.scale
    SELECT bom
    SCAN FOR '~'+lcitemscale$mszcrosref .AND. (SUBSTR(bom.item, lcclrpos, lnclrlen)=REPLICATE('*', lnclrlen) .OR. SUBSTR(bom.item, lcclrpos, lnclrlen)=SUBSTR(lcselestyle, lcclrpos, lnclrlen))
       lccrosreffld = mszcrosref
       DIMENSION lasizesarr[1]
       lasizesarr = ''
       = gfsubstr(lccrosreffld, @lasizesarr, CHR(13))
       FOR lnx = 1 TO ALEN(lasizesarr, 1)
          IF "~"+scale.scale$lasizesarr(lnx)
             DIMENSION lastylescale[lncntsty, 4]
             lastylescale[lncntsty, 1] = PADR(ALLTRIM(bom.citmmask), lnmajlen)
             lastylescale[lncntsty, 2] = IIF(SUBSTR(bom.citmmask, lcclrpos, lnclrlen)=REPLICATE('*', lnclrlen), SUBSTR(lcselestyle, lcclrpos, lnclrlen), SUBSTR(bom.citmmask, lcclrpos, lnclrlen))
             lastylescale[lncntsty, 3] = lasizesarr(lnx)
             lastylescale[lncntsty, 4] = bom.nestbomqty
             lncntsty = lncntsty+1
          ENDIF
       ENDFOR
    ENDSCAN
 ENDSCAN
 IF  .NOT. EMPTY(lastylescale(1, 1))
    IF USED('CompStyles')
       USE IN 'CompStyles'
    ENDIF
    CREATE CURSOR 'CompStyles' (style C (19))
    SELECT 'CompStyles'
    INDEX ON style TAG 'CompStyles'
    FOR lnf = 1 TO ALEN(lastylescale, 1)
       lcscanstyle = PADR(lastylescale(lnf, 1), lnmajlen)+lcclrsep+PADR(lastylescale(lnf, 2), lnclrlen)+lcsclspr+SUBSTR(lastylescale(lnf, 3), 1, lnscllen)
       IF  .NOT. SEEK(lcscanstyle, 'CompStyles', 'CompStyles')
          INSERT INTO CompStyles (style) VALUES (lcscanstyle)
       ENDIF
    ENDFOR
    IF USED('CompStyles') .AND. RECCOUNT('CompStyles')>0
       SELECT 'CompStyles'
       LOCATE
       SCAN
          SELECT ordline_f
          = gfseek(compstyles.style)
          SCAN REST WHILE style=compstyles.style
             IF EOF('ORDHDR_F')
                LOOP
             ENDIF
             IF EMPTY(ldreturndate)
                ldreturndate = ordhdr_f.entered
             ELSE
                IF ordhdr_f.entered<ldreturndate
                   ldreturndate = ordhdr_f.entered
                ENDIF
             ENDIF
          ENDSCAN
       ENDSCAN
    ENDIF
 ENDIF
 RETURN ldreturndate
ENDFUNC
**
PROCEDURE lfvautoclc
 ldprjfrm = DATE(YEAR(DATE()), MONTH(DATE()), 1)-1
ENDPROC
**
*** 
*** ReFox - all is not lost ... 
***
