*:**********************************************************************
*: Program file        : MAWIPAC.PRG
*: Program description : Material WIP GL- Account Report
*: Module              : Material (MA)
*: Developer           : Mariam Mazhar (MMT)
*: Tracking Job Number : #N037643        
*: Date                : 04/04/2011
*:**********************************************************************
#INCLUDE R:\aria4xp\reports\ma\mawipac.h
 loogScroll.cCROrientation = 'P'
 lcrpmakbuy='I' && This Will be Here till the MFG Screens are converted to Aria4 and the option in OG will be supressed too.
 llEntDate = .F.
 ldlowdat = {}
 ldhigdat = {}
 lnPosEntDate = ASCAN(loOgScroll.laOgFxFlt,"POFHDR.ENTERED")
 IF lnPosEntDate> 0 
   lnPosEntDate=  ASUBSCRIPT(loOgScroll.laOgFxFlt,lnPosEntDate,1)
   lcEntDate =loOgScroll.laOgFxFlt[lnPosEntDate,6]
   IF !EMPTY(lcEntDate)
     ldlowdat  = IIF(EMPTY(SUBSTR(loOgScroll.laOgFxFlt[lnPosEntDate,6],1,10)),CTOD(""),CTOD(SUBSTR(loOgScroll.laOgFxFlt[lnPosEntDate,6],1,10)))
     ldhigdat  = IIF(EMPTY(SUBSTR(loOgScroll.laOgFxFlt[lnPosEntDate,6],12,21)),CTOD(""),CTOD(SUBSTR(loOgScroll.laOgFxFlt[lnPosEntDate,6],12,21)))
     llEntDate = .T.
   ENDIF  
 ENDIF
 IF loOGScroll.llOGFltCh   &&If Filter Changed
   llgllink = (ALLTRIM(UPPER(gfgetmemvar('M_Link_GL',oAriaApplication.ActivecompanyID )))='Y')
   llaplink = ('AP' $ oAriaApplication.CompanyInstalledModules)
   IF llgllink
  *!*      IF !USED('cTktBom')
  *!*        =gfOpenTable('cTktBom','cTktBom') 
  *!*      ENDIF  
      
      IF !USED('POSLN')
        =gfOpenTable('POSLN','POSLN') 
      ENDIF  

  *!*      IF !USED('MMFGORDD')
  *!*        =gfOpenTable('MMFGORDD','MMFGORDD') 
  *!*      ENDIF  
      
      IF !USED('POSHDR')
        =gfOPenTable('POSHDR','POSHDR')
      ENDIF
      
  *!*      IF !USED('mmfgordh')  
  *!*        =gfOPenTable('mmfgordh','mmfgordh')
  *!*      ENDIF  
      llUseClr1  = .F.
      lcClr1File =""
      lcMFGSel = ''
      llMFGSel= .F.
      lcPOSel = ''
      llPOSel= .F.
      lcFabSel = ''
      llFabSel= .F.
      lcctpost  = ''
      lnPosStat= ASCAN(loogscroll.laOgFxFlt,"POSHDR.STATUS")
      IF lnPosStat> 0 
        lnPosStat= ASUBSCRIPT(loogscroll.laOgFxFlt,lnPosStat,1)
        lcctpost=IIF(!EMPTY(loogscroll.laOgFxFlt[lnPosStat,6]),loogscroll.laOgFxFlt[lnPosStat,6],'')
      ENDIF 
      IF EMPTY(lcCtPoSt)
        lcCtPoSt = 'A'
      ENDIF
      lcglcode = lcrplnkcod
      lclintmnam = ''
      lcglacnt = lcrpglact
      lchdrfl = ''
      STORE '' TO lclowsty, lchigsty
      lcwhilcon = ''
      lldumyret = lfsetrel()
      lcseltdpo = ''
      lcseltdmf = ''
      lcmmflt = '.T.'
      = lfcretfltr()
      WAIT WINDOW NOWAIT LANG_MAT_WIPGLACC_COLLDATA

      lldummy = lfclectdat()
      WAIT CLEAR
     ELSE
       = gfmodalgen('TRM38195B00000','ALERT')
       RETURN 
     ENDIF
    ENDIF
    
    IF USED(lcRpTmpFil)  
      SELECT &lcRpTmpFil
      GOTO TOP
    ELSE
      = gfmodalgen('TRM00052B00000','DIALOG')
    ENDIF  
    
    IF EOF()
       = gfmodalgen('TRM00052B00000','DIALOG')
    ELSE
       SELECT &lcRpTmpFil
       GOTO TOP
       DO gfdispre WITH EVALUATE('lcRpForm')
    ENDIF
    
 SET DEVICE TO SCREEN
 RETURN

*!*************************************************************
*! Name      : lfclectdat
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfclectdat
 DIMENSION laTmpStru[13,4]
 
 laTmpStru[1,1] = 'wipacc'
 laTmpStru[1,2] = 'C'
 laTmpStru[1,3] = 24
 laTmpStru[1,4] = 0
 
 laTmpStru[2,1] = 'ctpono'
 laTmpStru[2,2] = 'C'
 laTmpStru[2,3] = 6
 laTmpStru[2,4] = 0

 laTmpStru[3,1] = 'type'
 laTmpStru[3,2] = 'C'
 laTmpStru[3,3] = 1
 laTmpStru[3,4] = 0

 laTmpStru[4,1] = 'ctype'
 laTmpStru[4,2] = 'C'
 laTmpStru[4,3] = 1
 laTmpStru[4,4] = 0

 laTmpStru[5,1] = 'trandate'
 laTmpStru[5,2] = 'D'
 laTmpStru[5,3] = 8
 laTmpStru[5,4] = 0

 laTmpStru[6,1] = 'trantype'
 laTmpStru[6,2] = 'C'
 laTmpStru[6,3] = 1
 laTmpStru[6,4] = 0

 laTmpStru[7,1] = 'DESC'
 laTmpStru[7,2] = 'C'
 laTmpStru[7,3] = 20
 laTmpStru[7,4] = 0

 laTmpStru[8,1] = 'QTY'
 laTmpStru[8,2] = 'N'
 laTmpStru[8,3] = 13
 laTmpStru[8,4] = 3

 laTmpStru[9,1] = 'cost'
 laTmpStru[9,2] = 'N'
 laTmpStru[9,3] = 12
 laTmpStru[9,4] = 7

 laTmpStru[10,1] = 'lldontshow'
 laTmpStru[10,2] = 'L'
 laTmpStru[10,3] = 1
 laTmpStru[10,4] = 0

 laTmpStru[11,1] = 'llfabric'
 laTmpStru[11,2] = 'L'
 laTmpStru[11,3] = 1
 laTmpStru[11,4] = 0

 laTmpStru[12,1] = 'cvendcode'
 laTmpStru[12,2] = 'C'
 laTmpStru[12,3] = 8
 laTmpStru[12,4] = 0
 
 laTmpStru[13,1] = 'cvendname'
 laTmpStru[13,2] = 'C'
 laTmpStru[13,3] = 30
 laTmpStru[13,4] = 0
 
 =gfCrtTmp(lcRpTmpFil,@laTmpStru,"WIPAcc+Type+CtPoNo+cType",lcRpTmpFil,.F.)
 SELECT(lcRpTmpFil)
 INDEX ON ctype+ctpono TAG ctktno
 INDEX ON type+ctpono TAG tktno
 SET ORDER TO (lcRpTmpFil)
 
 lctmpcurs = loogscroll.gftempname()
 DIMENSION laTmp2Stru[5,4]

 laTmp2Stru[1,1] = 'ctpono'
 laTmp2Stru[1,2] = 'C'
 laTmp2Stru[1,3] = 6
 laTmp2Stru[1,4] = 0

 laTmp2Stru[2,1] = 'item'
 laTmp2Stru[2,2] = 'C'
 laTmp2Stru[2,3] = lnmajlength              
 laTmp2Stru[2,4] = 0

 laTmp2Stru[3,1] = 'clr'
 laTmp2Stru[3,2] = 'C'
 laTmp2Stru[3,3] = lenclrlen                
 laTmp2Stru[3,4] = 0
 
 laTmp2Stru[4,1] = 'csession'
 laTmp2Stru[4,2] = 'C'
 laTmp2Stru[4,3] = 6
 laTmp2Stru[4,4] = 0

 laTmp2Stru[5,1] = 'lcissret'
 laTmp2Stru[5,2] = 'C'
 laTmp2Stru[5,3] = 1
 laTmp2Stru[5,4] = 0


 =gfCrtTmp(lctmpcurs,@laTmp2Stru,"ctpono+item+clr+csession+lcissret",lctmpcurs,.F.)
 SELECT (lctmpcurs)
 lctmpnam = LOOGSCROLL.gftempname()
 =gfOpenTable('GLDIST','GLDISTAC','SH',lctmpnam)
 llusepotm = .F.
 llusecttm = .F.
 llusepotm =  llPOSel
 llusecttm =  llMFGSel
 
 IF llaplink
   IF !USED('ApvInvDt') 
     =gfOpenTable('ApvInvDt','Item','SH')
   ENDIF
   IF !USED('APDIST') 
     =gfOpenTable('APDIST','Invvend','SH')
   ENDIF
 ENDIF 
 
 DO CASE
    CASE lcrpmakbuy='B'
       = lfusetmp(llusecttm,'M')
       = lfusetmp(llusepotm,'T')
    CASE lcrpmakbuy='M'
       = lfusetmp(llusecttm,'M')
    CASE lcrpmakbuy='I'
       = lfusetmp(llusepotm,'T')
 ENDCASE
 IF llaplink 
   IF USED('ApvInvDt') 
     =gfCloseTable('ApvInvDt')
   ENDIF
   IF USED('APDIST') 
     =gfCloseTable('APDIST')
   ENDIF
 ENDIF
*!*************************************************************
*! Name      : lfcolpomf
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : select files to Collect Data from
*!*************************************************************
FUNCTION  lfcolpomf
 PARAMETER lc2colfrm, lctrno
 PRIVATE llfundtr, llmf
 lclintmnam = loogscroll.gftempname()
 =gfOpenTable(lc2colfrm,lc2colfrm,'SH',lclintmnam)
 *USE IN 0 SHARED gcdatadir+(lc2colfrm) AGAIN ALIAS (lclintmnam)
 llmf = (UPPER(lc2colfrm)=='MMFGORDD')
 SELECT (lc2colfrm)
 =gfSetOrder(lc2ColFrm)
 *SET ORDER TO &lc2ColFrm
 GOTO TOP
 lchdrfl = IIF(llmf, 'mmfgordH', 'POSHDR')
 lldummy = IIF(llmf, lfgetlin('T'), lfgetlin('P') .AND. lfgetlin('R'))
 =gfCloseTable(lclintmnam)
 *USE IN (lclintmnam)
*
*!*************************************************************
*! Name      : lfsetrel
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Set relation
*!*************************************************************
FUNCTION  lfsetrel
 SELECT poshdr
 SET RELATION TO link_code+'013' INTO gl_link
 GOTO TOP
*!*   SELECT mmfgordh
*!*   SET RELATION TO link_code+'013' INTO gl_link
*!*   GOTO TOP
*!*************************************************************
*! Name      : lfcutdet
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Get PO/MFGPO Details
*!*************************************************************
FUNCTION  lfcutdet
 PARAMETER lcctpot
 SELECT(lc2colfrm)
 =gfSetOrder(lc2colfrm)
* SET ORDER TO (lc2colfrm) IN (lc2colfrm)
 IF BETWEEN(RECNO(lc2colfrm), 1, RECCOUNT(lc2colfrm))
    GOTO RECNO(lc2colfrm) IN (lc2colfrm)
 ENDIF
 IF lcctpot='T'
    IF gfSEEK(lcctpot+lcdettrno, 'cTktBom')
       SELECT ctktbom
       SCAN REST WHILE CIMTYP+CUTTKT+TYP+CINVTYPE+ITEM+MFGCODE+DYELOT =lcctpot+lcdettrno
          lctype = typ
          llfabtrim = (ccatgtyp='F') .OR. (ccatgtyp='T' .AND. trim_invt)
          llstycomp = (ccatgtyp='S')
          lcdesc = IIF(llfabtrim .OR. llstycomp .OR. ccatgtyp='T', SUBSTR(item, 1, lnmajlength)+'      '+RIGHT(item,  lenclrlen), desc)
          IF llstycomp
             lcdesc = item
          ENDIF
          lcitem = item
          lciclr = iclr
          lcmfgcode = mfgcode
          DO CASE
             CASE llfabtrim
                SET ORDER TO MatInvJl IN matinvjl
                IF SEEK(SUBSTR(lcitem, 1, 7)+lciclr, 'MatInvJl') .AND. SEEK(SUBSTR(lcitem, 1, 7)+lciclr, 'Fabric')
                   SELECT matinvjl
                   SCAN FOR ctktno=lcdettrno .AND. BETWEEN(dtrandate, ldlowdat, ldhigdat) .AND. (EMPTY(lcglcode) .OR. lcglacnt=IIF(EMPTY(cglmatadj), gl_link.glacnt, cglmatadj)) .AND. ALLTRIM(cimtyp)=lcctpot WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession=SUBSTR(lcitem, 1, 7)+lciclr
                      SELECT bomcost
                      LOCATE FOR cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=lcctpot+lcdettrno .AND. cisession=matinvjl.cisession
                      IF (FOUND() .AND. ctktbom.typ=bomcost.cbomtype) .OR.  .NOT. FOUND()
                         SELECT &lcRpTmpFil
                         APPEND BLANK
                         REPLACE wipacc WITH IIF(EMPTY(matinvjl.cglmatadj), gl_link.glacnt, matinvjl.cglmatadj), ctpono WITH lcdettrno, type WITH lcctpot, desc WITH lcdesc, ctype WITH lctype, trandate WITH matinvjl.dtrandate, trantype WITH IIF(matinvjl.nissued>0, 'I', 'R'), qty WITH IIF(matinvjl.ctrantype='4' .AND. trantype='R', -1*matinvjl.nreceived, matinvjl.nissued), cost WITH IIF(matinvjl.nunitcost<>0, matinvjl.nunitcost, matinvjl.nuntcstbuy/IIF(fabric.conv=0, 1, fabric.conv)), llfabric WITH .T.
                         REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor), cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                         SELECT (lctmpcurs)
                         APPEND BLANK
                         REPLACE ctpono WITH lcdettrno, item WITH SUBSTR(lcitem, 1, 7), clr WITH lciclr, csession WITH matinvjl.cisession, lcissret WITH IIF(matinvjl.nissued>0, 'I', 'R')
                      ENDIF
                   ENDSCAN
                ENDIF
             CASE llstycomp
                SELECT styinvjl
                = SEEK(lcitem)
                SCAN REST FOR ctrcode=lcdettrno .AND. BETWEEN(dtrdate, ldlowdat, ldhigdat) .AND. (EMPTY(lcglcode) .OR. lcglacnt=IIF(EMPTY(cadjacct), gl_link.glacnt, cadjacct)) WHILE style=lcitem
                   SELECT &lcRpTmpFil
                   APPEND BLANK
                   REPLACE wipacc WITH IIF(EMPTY(styinvjl.cadjacct), gl_link.glacnt, styinvjl.cadjacct), ctpono WITH lcdettrno, type WITH lcctpot, desc WITH lcdesc, ctype WITH lctype, trandate WITH styinvjl.dtrdate, trantype WITH styinvjl.cirtype, qty WITH -1*styinvjl.ntotstk, cost WITH styinvjl.ncost
                   REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor), cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                ENDSCAN
          ENDCASE
          SELECT (lclintmnam)
          SET ORDER TO Mmfgordd
          SELECT (lclintmnam)
          = SEEK(lcdettrno)
          SCAN REST FOR trancd='2' WHILE cmfgordno+cfabric+color+dyelot+trancd=lcdettrno
             IF !(BETWEEN(&lcLinTmNam..drecvdate,ldLowDat,ldHigDat))
                LOOP
             ENDIF
             lcrsession = crsession
             lcmainitem = PADR(cfabric, 19)
             lcmainfab = PADR(color, 6)
             SELECT bomline
             = SEEK(lcctpot+'2'+lcdettrno)
             SCAN REST FOR cbomtyp+style+sclr+item+iclr+mfgcode+crsession=lctype+lcmainitem+lcmainfab+lcitem+lciclr+lcmfgcode+lcrsession WHILE cimtyp+ctype+ctktno+STR(lineno, 6)+cbomtyp+style+sclr+item+iclr+mfgcode=lcctpot+'2'+lcdettrno
                SELECT &lcRpTmpFil
                APPEND BLANK
                REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt), CtPoNo   WITH lcDetTrNo, Type     WITH lcCtPoT, Desc     WITH lcDesc, cType    WITH BomLine.CBomTyp, TranDate WITH &lcLinTmNam..dRecvDate, TranType WITH 'E', Qty      WITH -1*(BomLine.ItemQty), Cost     WITH IIF(lcCtPoT='M',BomLine.UnitCost,IIF(PoFHdr.Rate<>0 AND BomLine.cCatgTyp='P',BomLine.UnitCost/PoFHdr.Rate,BomLine.UnitCost))
                REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor), cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                IF lcctpot='M'
                   lneqcose = bomline.unitcost
                ELSE
                   IF bomline.ccatgtyp='P'
                      lcexsin2 = ''
                      lcexsin1 = gfgetexsin(@lcexsin2,poshdr.cpricecur)
                      lnEqCose = BomLine.UnitCost &lcExSin1. PoSHdr.nPricerat
                   ELSE
                      IF bomline.ccatgtyp='D'
                         lcexsin2 = ''
                         lcexsin1 = gfgetexsin(@lcexsin2,pofhdr.cdutycur)
                         lnEqCose = BomLine.UnitCost &lcExSin1. PoFHdr.nDutyrat
                      ELSE
                         lneqcose = bomline.unitcost
                      ENDIF
                   ENDIF
                ENDIF
                REPLACE cost WITH lneqcose
             ENDSCAN
          ENDSCAN
       ENDSCAN
       SELECT bomcost
       SET ORDER TO Pobomcls
       = SEEK(lcctpot+lcdettrno)
       IF BETWEEN(RECNO(lc2colfrm), 1, RECCOUNT(lc2colfrm))
          GOTO RECNO(lc2colfrm) IN (lc2colfrm)
       ENDIF
       SCAN REST FOR BETWEEN(dtrandate, ldlowdat, ldhigdat) .AND. ccosttype$'MPD' .AND. (EMPTY(lcglcode) .OR. lcglacnt=IIF(EMPTY(cwipacnt), gl_link.glacnt, cwipacnt)) WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=lcctpot+lcdettrno
          lcdesc = IIF(SEEK(lcctpot+lcdettrno+cbomtype+SPACE(19)+SPACE(06)+mfgcode+cdyelot, 'CTKTBOM'), ctktbom.desc, '')
          SELECT &lcRpTmpFil
          APPEND BLANK
          REPLACE wipacc WITH IIF(EMPTY(bomcost.cwipacnt), gl_link.glacnt, bomcost.cwipacnt), ctpono WITH lcdettrno, type WITH lcctpot, ctype WITH bomcost.cbomtype, desc WITH lcdesc, trandate WITH bomcost.dtrandate, qty WITH bomcost.ntotqty, cost WITH bomcost.ntotcst/bomcost.ntotqty, trantype WITH 'V'
          REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor), cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
          IF lcctpot='M'
             lneqcose = IIF(bomcost.ntotqty<>0, bomcost.ntotcst/bomcost.ntotqty, 0)
          ELSE
             IF bomcost.ccosttype='P'
                lcexsin2 = ''
                lcexsin1 = gfgetexsin(@lcexsin2,poshdr.cpricecur)
                lnEqCose = (BomCost.nTotCst &lcExSin1. PoSHdr.nPricerat) / BomCost.nTotQty
             ELSE
                IF bomcost.ccosttype='D'
                   lcexsin2 = ''
                   lcexsin1 = gfgetexsin(@lcexsin2,poshdr.cdutycur)
                   lnEqCose = (BomCost.nTotCst &lcExSin1. PoSHdr.nDutyrat) / BomCost.nTotQty
                ELSE
                   lneqcose = IIF(bomcost.ntotqty<>0, bomcost.ntotcst/bomcost.ntotqty, 0)
                ENDIF
             ENDIF
          ENDIF
          REPLACE cost WITH lneqcose
       ENDSCAN
    ENDIF
 ELSE
    = lfgetrc(lcdettrno,lcctpot)
    = lfgetapinv(IIF(lcctpot='P', 'L', 'F'))
 ENDIF
 SELECT (lc2colfrm)
 RETURN
*
*!*************************************************************
*! Name      : lfstatus
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Return Status Desc.
*!*************************************************************
FUNCTION lfstatus
 PARAMETER lcstatus
 DO CASE
    CASE lcstatus='A'
       RETURN LANG_MAT_WIPGLACC_ACTUAL
    CASE lcstatus='C'
       RETURN LANG_MAT_WIPGLACC_COMPLETE
    CASE lcstatus='H'
       RETURN LANG_MAT_WIPGLACC_HOLD
    CASE lcstatus='O'
       RETURN LANG_MAT_WIPGLACC_OPEN
    CASE lcstatus$'SL'
       RETURN LANG_MAT_WIPGLACC_CLOSE
    CASE lcstatus='X'
       RETURN LANG_MAT_WIPGLACC_CANC
    OTHERWISE
       RETURN ('')
 ENDCASE
*

*!*************************************************************
*! Name      : lfwogwhen
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : When function of OG
*!*************************************************************
FUNCTION  lfwogwhen
IF !llFirst 
	lcSqlStatment   = "SELECT ItemLoc.STYLE,ItemLoc.TOTWIP  AS ONORDER,ItemLoc.TOTSTK AS ONHAND ,"+;
					 "ItemLoc.TOTORD,WIP1,DYELOT,ITEM.CSTYMAJOR   FROM ItemLoc(INDEX = STYDYE),ITEM(INDEX = CSTYLE) WHERE ITEMLOC.dyelot ='' AND ITEMLOC.STYLE = ITEM.STYLE AND ITEMLOC.CINVTYPE = ITEM.CINVTYPE "
	lcSqlStatment   = lcSqlStatment   +" AND ITEM.CINVTYPE = 0002"
	lcTable ='ItemLoc'
	lnConnectionHandlar = loOGScroll.oRDA.sqlrun(lcSqlStatment,lcCursorLoc,lcTable,oAriaApplication.ActiveCompanyConStr,3,;
	                                      'BROWSE',SET("DATASESSION"))

	IF lnConnectionHandlar >= 1
	  SELECT(lcCursorLoc)
	  lnBuffering = CURSORGETPROP("Buffering",lcCursorLoc)
	   =CURSORSETPROP("Buffering",3,lcCursorLoc)
	  SELECT (lcCursorLoc)
	  INDEX ON CSTYMAJOR TAG &lcCursorLoc
	  SET ORDER TO TAG &lcCursorLoc
	  SET RELATION TO 
	  LOCATE 
	ENDIF 
	llFirst = .T.
ENDIF
 
 DIMENSION lasetups[4, 2]
 lasetups[ 1, 1] = 'M_CTSLBL1'
 lasetups[ 2, 1] = 'M_CTSLBL2'
 lasetups[ 3, 1] = 'M_CTSLBL3'
 lasetups[ 4, 1] = 'M_CTSLBL4'
 = gfgetmemvar(@lasetups,gcact_comp)
 FOR lnind = 1 TO 4
    lcind = STR(lnind, 1)
    lcTDesc&lcInd = laSetups[lnInd,2]
 ENDFOR
 STORE LANG_MAT_WIPGLACC_MERCH TO lcpdesc1, lcrdesc1
 STORE LANG_MAT_WIPGLACC_FREIGHT TO lcpdesc4, lcrdesc4
 STORE LANG_MAT_WIPGLACC_TAX  TO lcpdesc2, lcrdesc2
 STORE LANG_MAT_WIPGLACC_QUOTA TO lcpdesc3, lcrdesc3
*
*!*************************************************************
*! Name      : lfvlnkcod
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Validate Link Code
*!*************************************************************
FUNCTION  lfvlnkcod

LOCAL lnAlias, llRetVal, lcControl, lcCurVar, lnPos
 lnAlias   = SELECT(0)
 lcControl = loOGScroll.FocusControl
 lcCurVar  = loOGScroll.&lcControl.
 lnPos     = lcCurVar.Parent.nRowIndex
 lcCurVal  = PADR(lcCurVar.Value, 6)
 llRetVal  = .T.
 IF !USED('gl_link')
   =gfOpenTable('gl_link','Gl_link1')
 ENDIF
 SELECT gl_link
 =gfSetOrder('Gl_link1')
 lcCodeVal =  lcCurVal  
 IF  .NOT. EMPTY(lccodeval)
    SELECT gl_link
    IF  .NOT. gfSEEK('05'+ALLTRIM(lccodeval))
       llnothing = lfgllinkbr()
    ELSE
       llnothing = .T.
    ENDIF
    lcRPLnkCod = IIF(llNothing,GL_LINK.Link_Code,'')
    SELECT gl_link
    =gfSetOrder('Gl_link')
    IF SEEK(&lcCodeVar+'013')
       lcrpglact = gl_link.glacnt
    ELSE
       lcrpglact = SPACE(3)
    ENDIF
 ENDIF
*
*!*************************************************************
*! Name      : lfgllinkbr
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Browse Link Codes
*!*************************************************************
FUNCTION lfgllinkbr
 PRIVATE lcfields, labrow, lncuralias, lccurtag, llreturn, lctag, lcbrfields, lcfile_ttl
 DIMENSION labrow[ 1]
 STORE SPACE(0) TO lcfields, labrow
 llreturn = .F.
 lncuralias = SELECT(0)
 lctag = ORDER()
 lcfields = 'link_code'
 lcbrfields = "Link_Code    :H='"+LANG_MAT_WIPGLACC_LINKCODE+"',"+"Description=GL_LINK.LinkDesc :H='"+LANG_MAT_WIPGLACC_DESC+"'"
 lcfile_ttl = LANG_MAT_WIPGLACC_LINKCODES
 lc2sek = '05'
 SELECT gl_link
 =gfSetOrder('Gl_link1')
 =gfSeek("")
 LOCATE
 DIMENSION latemp[ 1]
 llreturn = gfbrows('lc2Sek','LinkType,link_code','laTemp')
 SELECT (lncuralias)
 SET ORDER TO (lctag)
 RETURN llreturn

*!*************************************************************
*! Name      : lfcretfltr
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Create Filter
*!*************************************************************
*
FUNCTION  lfcretfltr


lcFabSel = ''
llFabSel= .F.
lnPosFab= ASCAN(loogscroll.laOgFxFlt,"FABRIC.FABRIC")
IF lnPosFab> 0 
  lnPosFab = ASUBSCRIPT(loogscroll.laOgFxFlt,lnPosFab,1)
  lcFabSel = IIF(!EMPTY(loogscroll.laOgFxFlt[lnPosFab,6]),loogscroll.laOgFxFlt[lnPosFab,6],'')
  IF !EMPTY(lcFabSel) AND USED(lcFabSel)
    SELECT(lcFabSel)
    LOCATE
    IF !EOF()
      llFabSel= .T.
    ENDIF
  ENDIF  
ENDIF 
*PO
lcPOSel = ''
llPOSel= .F.
lnPosPO= ASCAN(loogscroll.laOgFxFlt,"POFHDR.POMAT")
IF lnPosPO> 0 
  lnPosPO = ASUBSCRIPT(loogscroll.laOgFxFlt,lnPosPO,1)
  lcPOSel  = IIF(!EMPTY(loogscroll.laOgFxFlt[lnPosPO,6]),loogscroll.laOgFxFlt[lnPosPO,6],'')
  IF !EMPTY(lcPOSel) AND USED(lcPOSel)
    SELECT(lcPOSel)
    LOCATE
    IF !EOF()
      llPOSel= .T.
    ENDIF
  ENDIF  
ENDIF 

*MFG
lcMFGSel = ''
llMFGSel= .F.
lnPosMFG= ASCAN(loogscroll.laOgFxFlt,"MMFGORDH.CMFGORDNO")
IF lnPosMFG> 0 
  lnPosMFG = ASUBSCRIPT(loogscroll.laOgFxFlt,lnPosMFG,1)
  lcMFGSel  = IIF(!EMPTY(loogscroll.laOgFxFlt[lnPosMFG,6]),loogscroll.laOgFxFlt[lnPosMFG,6],'')
  IF !EMPTY(lcMFGSel) AND USED(lcMFGSel)
    SELECT(lcMFGSel)
    LOCATE
    IF !EOF()
      llMFGSel= .T.
    ENDIF
  ENDIF  
ENDIF 


llUseClr1  = .F.
lnClr1Pos = ASCAN(loOgScroll.laOgFxFlt,"FABRIC.COLOR")
IF lnClr1Pos > 0 
  lnClr1Pos  = ASUBSCRIPT(loOgScroll.laOgFxFlt,lnClr1Pos,1)
  lcClr1Sel =IIF(!EMPTY(loOgScroll.laOgFxFlt[lnClr1Pos ,6]),loOgScroll.laOgFxFlt[lnClr1Pos,6],'')
  IF !EMPTY(lcClr1Sel) 
    lcClr1File = loOGScroll.gfTempName()
    llUseClr1= IIF(LEN(lcClr1Sel)>0,.T.,.F.) AND lfConvertToCursor(lcClr1Sel,'CSTYCLR',lcClr1File)
  ENDIF   
ENDIF   







*!*************************************************************
*! Name      : lfsrvtrans
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Set/Rest of MFG PO
*!*************************************************************
FUNCTION  lfsrvtrans
 PARAMETER lcparm
 DO CASE
    CASE lcparm='S'
       SET ORDER TO VENCODE IN apvendor
       SELECT poshdr
       SET RELATION TO poshdr.vendor INTO apvendor ADDITIVE
    CASE lcparm='R'
       SELECT poshdr
       SET RELATION TO
 ENDCASE
*
*!*************************************************************
*! Name      : lfclrred
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Refresh OG
*!*************************************************************
FUNCTION  lfclrred
CLEARREAD()
*
*!*************************************************************
*! Name      : lfusetmp
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Use selected Trans. Temp. cursors
*!*************************************************************
FUNCTION  lfusetmp
 PARAMETER llusetmp, lcctpo
 PRIVATE lnoldals
 lnoldals = SELECT(0)
 IF lcctpo='M'
    IF llusetmp
       SELECT (lcMFGSel)
       SCAN
          = lfcolpomf('mmfgordd',cmfgordno)
       ENDSCAN
    ELSE
       = lfcolpomf('mmfgordd','')
    ENDIF
 ELSE
    IF llusetmp
       SELECT (lcPOSel)
       SCAN
          = lfcolpomf('POSLN',po)
       ENDSCAN
    ELSE
       = lfcolpomf('POSLN','')
    ENDIF
 ENDIF
 SELECT (lnoldals)
*
*!*************************************************************
*! Name      : lfsrvpo
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Set/ReSet of PO#
*!*************************************************************
FUNCTION  lfsrvpo
 PARAMETER lcparm
 PRIVATE lcalias, llhavesty
 DO CASE
    CASE lcparm='S'
      IF !USED('POSHDR')
        =gfOpenTable('POSHDR','POSHDR','SH')
       ENDIF
       SELECT POSHDR
       =gfSqlRun("Select * FROM POSHDR WHERE cStyType ='M' AND CBUSDOCU IN ('P','R')",'POSHDR',.F.,lcPoshdr)
       SELECT (lcPoshdr)
       SET ORDER TO Vencode IN apvendor
       SET RELATION TO vendor INTO apvendor
       SELECT (lcPoshdr)
       LOCATE 
    CASE lcparm='R'
       SELECT POSHDR
       SET FILTER TO 
       SET RELATION TO
       SET KEY TO
 ENDCASE
*
*!*************************************************************
*! Name      : lfgetlin
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Get Lins
*!*************************************************************
FUNCTION  lfgetlin
 PARAMETER lcporet
 llfundtr = IIF(llmf, gfSEEK(lctrno), gfSEEK(IIF(lcporet='P','PM',"RM")+lctrno))
 lctrwhcnd = IIF(llmf, 'cMfgOrdNo', 'CBUSDOCU+CSTYTYPE+PO')
 SELECT (lc2colfrm)
 IF llfundtr
    SELECT (lchdrfl)
    
    = IIF(llmf, gfSEEK(lctrno), gfSEEK(IIF(lcporet='P','PM',"RM")+lctrno))
    SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,IIF(lcporet='P','PM',"RM")+lcTrNo) FOR ;
         IIF(lcCtPoSt='A' ,.T., &lcHdrFl..Status $ lcCtPoSt) .AND.  !EMPTY(&lcHdrFl..Link_Code) .AND.  IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)
       =gfSeek(&lcHdrFl..Link_Code+'013','gl_link','gl_link')
       lcGlAcnt =   gl_link.glacnt
       lcdettrno = IIF(llmf, cmfgordno, po)
       SELECT (lc2colfrm)
       = IIF(llmf, gfSEEK(lcdettrno), gfSEEK(IIF(lcporet='P','PM',"RM")+lcdettrno))
       SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcDetTrNo,IIF(lcporet='P','PM',"RM")+lcDetTrNo) FOR;
       IIF(llFabSel ,IIF(llmf,SEEK(cfabric,lcFabSel),Seek(SUBSTR(STYLE,1,lnmajlength),lcFabSel)),.T.) AND ;
       IIF(llUseClr1,IIF(llmf,SEEK(COLOR,lcClr1File),Seek(RIGHT(STYLE,lenclrlen),lcClr1File)),.T.)
       
          lcdettrno = IIF(llmf, cmfgordno, PO)
          SELECT &lcRpTmpFil
          WAIT WINDOW NOWAIT LANG_MAT_WIPGLACC_COLLTRAN +lcdettrno
          LOCATE FOR type+ctpono=IIF(llmf, 'T', lcporet)+lcdettrno
          IF  .NOT. FOUND()
             = lfcutdet(IIF(llmf, 'T', IIF(lcporet='P','PM',"RM")))
             lnrecno = RECNO('POSLN')
          ENDIF
          SELECT (lc2colfrm)
          IF &lcHdrFl..Status $ 'LS' AND IIF(llMf , &lc2ColFrm..TranCd = '1' ,  IIF(SEEK(lcPORET+lcDetTrNo) ,&lc2ColFrm..TranCd = '1',.F.))
             PRIVATE lntmind, lctmind, lncloseqty
             lncloseqty = 0
             FOR lntmind = 1 TO 4
                lctmind = STR(lntmind, 1)
                lnCloseQty = lnCloseQty+(&lcHdrFl..neActCost&lcTmInd - &lcHdrFl..nlan_cost&lcTmInd)
             ENDFOR
             SELECT &lcRpTmpFil
             LOCATE FOR ctpono=lcdettrno .AND. type=IIF(llmf, 'T', lcporet) .AND. trantype='S'
             IF  .NOT. FOUND()
                SELECT (lctmpnam)
                SET ORDER TO GlDistNo DESCENDING
                ldClDate = IIF(SEEK(lcDetTrNo + 'MC',lcTmpNam),&lcTmpNam..Tran_Date,{})
                IF BETWEEN(ldcldate, ldlowdat, ldhigdat)
                   SELECT &lcRpTmpFil
                   APPEND BLANK
                   REPLACE wipacc WITH IIF(EMPTY(lcglcode), gl_link.glacnt, lcglacnt),;
                           ctpono WITH lcdettrno,;
                           type WITH IIF(llmf, 'T', lcporet),;
                           desc WITH ' ',;
                           ctype WITH '9',;
                           trandate WITH ldcldate,;
                           trantype WITH 'S',;
                           qty WITH 1,;
                           cost WITH lncloseqty*-1
                ENDIF
             ENDIF
          ENDIF
          IF  .NOT. EOF()
             GOTO IIF(lnrecno>RECCOUNT('POsLN'), RECCOUNT('POsLN'), lnrecno) IN posln
          ELSE
             GOTO RECCOUNT('POSLN') IN poSln
          ENDIF
          IF !(IIF(UPPER(lc2ColFrm) ="POSLN" , BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat), BETWEEN(&lc2ColFrm..drecvdate,ldLowDat,ldHigDat)) .AND. TranCd $"234")
             LOOP
          ENDIF
       ENDSCAN
    ENDSCAN
 ENDIF
 RETURN
*
*!*************************************************************
*! Name      : lfgetapinv
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Payable Inv. lines
*!*************************************************************
FUNCTION  lfgetapinv
 PARAMETER lcporet
 SELECT apvinvdt
 IF gfSEEK(lcporet+lcdettrno)
    SCAN REST WHILE cimtyp+ctktno+clotno+STR(lineno, 6)+cbomtype+coprcode+item+color+cdyelot=lcporet+lcdettrno FOR (EMPTY(lcglcode) .OR. lcglacnt=IIF(EMPTY(capdglact), gl_link.glacnt, capdglact)) AND ;
    IIF(llFabSel ,Seek(ALLTRIM(item),lcFabSel),.T.) AND ;
    IIF(llUseClr1,SEEK(COLOR,lcClr1File),.T.)
       SELECT apdist
       = gfSEEK(apvinvdt.capinvno+apvinvdt.cvendcode)
       LOCATE REST FOR capdactid='D' .AND. ALLTRIM(STR(napdlinno))==ALLTRIM(apvinvdt.capvilno) WHILE cinvno+cvendcode+capdtrtyp=apvinvdt.capinvno+apvinvdt.cvendcode
       IF IIF(llEntDate,!(BETWEEN(apdist.dapdtrdat, ldlowdat, ldhigdat)),.F.)
          LOOP
       ENDIF
       SELECT &lcRpTmpFil
       APPEND BLANK
       REPLACE wipacc WITH IIF(EMPTY(apvinvdt.capdglact), gl_link.glacnt, apvinvdt.capdglact),;
               ctpono WITH lcdettrno,;
               type WITH lcctpot,;
               ctype WITH apvinvdt.cbomtype,;
               desc WITH ALLTRIM(apvinvdt.capinvno)+'  '+ALLTRIM(apvinvdt.cvendcode),;
               trandate WITH apdist.dapdtrdat,;
               qty WITH apvinvdt.naptotqty,;
               cost WITH apvinvdt.napprice,;
               trantype WITH 'V'
       REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
               cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
       REPLACE cost WITH apdist.neqvamnt/qty
    ENDSCAN
 ENDIF
*
*!*************************************************************
*! Name      : lfgetrc
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : get Received lines
*!*************************************************************
FUNCTION lfgetrc
 PARAMETER lcdettrno, lcctpot
 SELECT (lc2colfrm)
 =gfSetOrder(lc2colfrm)
 *SET ORDER TO (lc2colfrm) IN (lc2colfrm)
 IF BETWEEN(RECNO('POSLN'), 1, RECCOUNT('POSLN'))
    GOTO RECNO('POSLN') IN POSLN
 ENDIF
 SELECT POSLN
 =gfSetOrder('POSLN')
 *SET ORDER TO POFLN
 SELECT POSLN
 = gfSEEK(lcctpot+lcdettrno)
 SCAN REST WHILE CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD =lcctpot+lcdettrno FOR trancd='2' AND ;
    IIF(llFabSel ,IIF(llmf,SEEK(cfabric,lcFabSel),Seek(SUBSTR(STYLE,1,lnmajlength),lcFabSel)),.T.) AND ;
    IIF(llUseClr1,IIF(llmf,SEEK(COLOR,lcClr1File),Seek(RIGHT(STYLE,lenclrlen),lcClr1File)),.T.)

    SELECT &lcRpTmpFil
    FOR lnind = 1 TO 4
       lcinde = STR(lnind, 1)
       IF POSLN.nLan_Cost&lcInde <> 0 .AND. IIF(llEntDate,BETWEEN(POSLN.dPostDate,ldLowDat,ldHigDat),.T.)
          APPEND BLANK
          REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                  CtPoNo   WITH lcDetTrNo,;
                  Type     WITH lcCtPoT,;
                  Desc     WITH POSlN.STYLE,;
                  cType    WITH STR(lnInd,1),;
                  TranDate WITH POSLN.dPostDate,;
                  TranType WITH IIF(POSLN.cBusDocu = 'R',' ','E'),;
                  Qty      WITH IIF(POSLN.cBusDocu= 'R',POSLN.TotQty,-1*POSLN.TotQty),;
                  Cost     WITH POSLN.nlan_cost&lcInde
          REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                  cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
       ENDIF
    ENDFOR
 ENDSCAN
*
*!*************************************************************
*! Name      : lfgetvend
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : get Vendor Name
*!*************************************************************
FUNCTION lfgetvend
 PARAMETER lcvendcode
 PRIVATE lcoldalias, lcvendname
 lcvendname = ''
 lcoldalias = ALIAS()
 IF !USED('APVENDOR')
    = gfOpenTable('APVENDOR','VENCODE','SH')
 ENDIF
 SELECT apvendor
 =gfSetOrder('VENCODE')
 IF gfSEEK(lcvendcode)
   lcvendname = apvendor.cvencomp
 ENDIF
 SELECT (lcoldalias)
 RETURN lcvendname
*
*!*************************************************************
*! Name      : lfsumfab1
*! Developer : Mariam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION lfsumfab1
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec

LOCAL lnAlias
lnAlias = SELECT()

lnTotcomp = 0
SELECT(lcCursorLoc)
IF RECCOUNT() != 0
  lnFabRec = RECNO('ITEM')
  SELECT(lcCursorLoc)
  LOCATE 
  IF SEEK(lcFab)
    SUM &lcCOMP TO lnTotcomp WHILE cstymajor=lcFab AND DYELOT =''
  ENDIF 
  SELECT ITEM
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  

SELECT(lnAlias)

RETURN INT(lnTotcomp)

*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 04/04/2011
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
CASE  ALLTRIM(lcFieldName) = 'CSTYCLR'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
ENDCASE 

 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.