*!**************************************************************************
*! Name      : STCMAIN.PRG
*! Developer : AEG (Abdelrahman Essam)
*! Date      : 10/19/2016
*! Purpose   : styleclub Custom Process Program.
*!  
*!  
*! Ticket id T20160720.0020
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications *C201882,1 AEG 2016-20-08  modify melwod customization to recive by style only and copy scale codes styles from source company 
*:***************************************************************************

Parameter lcevntfun, lcfunpars



lcfunpars = Iif(Type('lcFunPars') = 'C', lcfunpars, '')
lcfuntorun = 'lf' + Alltrim(lcevntfun) + '(' + lcfunpars + ')'
llretvalue = Evaluate(lcfuntorun)
Return llretvalue



*:**************************************************************************
*:* Name        : UPCSKU
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 10/19/2016
*:* Purpose     : melwod customization for recieving po
*:**************************************************************************
FUNCTION lfupcsku 



lcCuralias=ALIAS()
IF SEEK(EDILIBDT.cPartCode ,'EDIACPRT','PARTNER') .AND. EDIACPRT.lInterComp .AND. ALLTRIM(EDIPD.Cmapset)='MWD'
           
  STORE .F. TO llFoundST , llFoundUP, llFoundSK
  lcPartStup = IIF(EMPTY(EDIACPRT.cPoRcvLine),'US',EDIACPRT.cPoRcvLine)
  lcSourceStyle = PADR(MStyl,LEN(lcMjrPic))+lcStyleSep+PADR(MColor,lnClrLen)+lcClrSep+MSCALE
  IF !EMPTY(MCOMPID)
   llVStyle=lfVStyle(MCOMPID,lcSourceStyle )
  ENDIF
ENDIF
 SELECT &lcCuralias.
RETURN .F.

*:**************************************************************************
*:* Name        : lfVStyle
*:* Developer   : MMT - Mariam 
*:* Date        : 10/19/2016
*:* Purpose     : melwod customization for recieving po
*:**************************************************************************
FUNCTION lfVStyle
PARAMETERS lcSrcCompID,lcStyleID

lcSourceDir = STRTRAN(UPPER(oAriaApplication.DataDir),oAriaApplication.ActiveCompanyID,lcSrcCompID)
IF !USED('STYLE')
USE (oAriaApplication.DataDir+"STYLE.DBF") SHARED IN 0 ORDER STYLE
ENDIF
IF !USED('STYDYE')
USE (oAriaApplication.DataDir+"STYDYE.DBF") SHARED IN 0 ORDER STYDYE
ENDIF
IF !USED('OLD_STYLE')
USE (lcSourceDir +"STYLE.DBF") SHARED IN 0 AGAIN ORDER STYLE ALIAS 'OLD_STYLE'   
ENDIF
IF !SEEK(lcStyleID,'OLD_STYLE','STYLE') OR !lfVScale(SUBSTR(OLD_STYLE.Scale,1,1))
  RETURN .F.
ENDIF
IF !SEEK(lcStyleID,'STYLE','STYLE')
    SELECT  'OLD_STYLE'
      SCATTER MEMO MEMVAR FIELDS Style,cStyMajor,cDye_flg,Status,cStyGroup,Scale,Desc,Desc1,Season,cStyGrade,Royalty,qty_ctn,Commission,link_code,Make,;
								Nmcost1,Nmcost2,Nmcost3,Nmcost4,Nmcost5,Nmcost6,Nmcost7,Nicost1,Nicost2,Nicost3,Nicost4,Nicost5,Nicost6,Nicost7,CpriceCur,;
								cdutyCur,Ave_cost,totCost,LinvSty,cPurCode,PriceA,PriceB,PriceC,cSlSGLLink,cDiscCode,lDetCost,CDivision,ltaxAble 

      oSeqObj = CREATEOBJECT('GetSequence')

      m.cStyGroup  = lfvCodeNo('CSTYGROUP',m.cStyGroup,.T.)
      m.Season     = lfvCodeNo('SEASON',m.Season,.T.)
      m.Royalty	   = lfvCodeNo('ROYALTY',m.Royalty,.F.)
      m.cPurCode   = lfvCodeNo('CPURCODE',m.CPURCODE,.F.)
      m.cDiscCode  = lfvCodeNo('CDISCCODE',m.cDiscCode,.F.)
      m.CDivision  = lfvCodeNo('CDIVISION',m.CDIVISION,.T.)				
      m.cSlSGLLink = 'DEF'
      m.link_code = 'DEFDEF'
      m.cadd_user = 'MEL'
      m.dadd_date = DATE()  
      m.cDefWare = lcDefWare
          
      INSERT INTO 'Style' FROM MEMVAR 
      oSeqObj = .NULL.

      IF !Seek(m.Style+lcDefWare, 'STYDYE', 'STYDYE')
         Insert into 'STYDYE' (Style,cWareCode,desc,gl_link, Ave_Cost,cadd_user,dadd_date) values (m.Style,lcDefWare,m.Desc,m.link_code,m.Ave_cost,'MEL',DATE())
      ENDIF

ENDIF
  USE IN 'OLD_STYLE' 


RETURN .T.

*:**************************************************************************
*:* Name        : lfvCodeNo
*:* Developer   : MMT - Mariam 
*:* Date        : 10/19/2016
*:* Purpose     : melwod customization for recieving po
*:**************************************************************************
FUNCTION lfvCodeNo
PARAMETERS lcCodeName,lcCodeId, llEDITABLE

lcRetValue = ''
lcCodeDesc =''
lnAlias = SELECT()
IF !USED('Codes')
USE (oAriaApplication.DataDir+"Codes.DBF") SHARED IN 0 ORDER CCODE_NO
ENDIF

IF !USED('Src_Code')
  USE (lcSourceDir+"Codes.DBF") SHARED IN 0 AGAIN ORDER CCODE_NO ALIAS 'Src_Code'
ENDIF  								
IF SEEK('N'+lcCodeId+"N"+PADR(ALLTRIM(UPPER(lcCodeName)),10), 'Src_Code','CODES')
  lcCodeDesc = Src_Code.cDiscRep
ELSE
  RETURN ''  
ENDIF

SELECT Codes
set order to tag CCODE_NO
=SEEK('N'+PADR(ALLTRIM(UPPER(lcCodeName)),10))
LOCATE REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(ALLTRIM(UPPER(lcCodeName)),10) FOR ;
                  ALLTRIM(UPPER(CDISCREP)) = ALLTRIM(UPPER(lcCodeDesc))
IF FOUND()
  lcRetValue = Codes.CCODE_NO
ELSE
    IF llEDITABLE
      IF SEEK('N'+PADR(ALLTRIM(UPPER(lcCodeName)),10)+lcCodeId,'CODES','CCODE_NO')
        lcRetValue = Codes.CCODE_NO
      ELSE
        ** Add the code into the target company
        IF !USED('Src_Code')
          USE (lcSourceDir+"Codes.DBF") SHARED IN 0 AGAIN ORDER CCODE_NO ALIAS 'Src_Code'
        ENDIF  
        SELECT 'Src_Code'
        IF SEEK('N'+PADR(ALLTRIM(UPPER(lcCodeName)),10)+lcCodeId)
          SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM ='N'+PADR(ALLTRIM(UPPER(lcCodeName)),10)+lcCodeId
            SCATTER MEMO MEMVAR 
            INSERT INTO 'Codes' FROM MEMVAR 
          ENDSCAN
          lcRetValue = lcCodeId
        ENDIF
        USE IN 'Src_Code'
      ENDIF
      
    ELSE && Not Editable codes
      lcNewCodeSeq =  oSeqObj.DO('CCODE_NO')
      IF !USED('Src_Code')
        USE (lcSourceDir+"Codes.DBF") SHARED IN 0 AGAIN ORDER CCODE_NO ALIAS 'Src_Code'
      ENDIF  
      SELECT 'Src_Code'
      IF SEEK('N'+PADR(ALLTRIM(UPPER(lcCodeName)),10)+lcCodeId)
        SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM ='N'+PADR(ALLTRIM(UPPER(lcCodeName)),10)+lcCodeId
          SCATTER MEMO MEMVAR 
          m.CCODE_NO = lcNewCodeSeq 
          INSERT INTO 'Codes' FROM MEMVAR 
        ENDSCAN
        lcRetValue = lcNewCodeSeq 
      ENDIF
      USE IN 'Src_Code'
    ENDIF
ENDIF                  

SELECT(lnAlias)
RETURN (lcRetValue)

*:**************************************************************************
*:* Name        : lfVScale 
*:* Developer   : MMT - Mariam 
*:* Date        : 10/19/2016
*:* Purpose     : melwod customization for recieving po
*:**************************************************************************

FUNCTION lfVScale 
PARAMETERS lcScaleID
lnOldAlias = SELECT()
llSRetValue = .F.
IF!USED('SCALE')
USE (oAriaApplication.DataDir+"SCALE.DBF") SHARED IN 0 ORDER Scale
ENDIF
IF!USED('SCALEHD')
USE (oAriaApplication.DataDir+"SCALEHD.DBF") SHARED IN 0 ORDER EXTSCALE   && CEXTSCALE
ENDIF

USE (lcSourceDir+"SCALE.DBF") SHARED IN 0 ORDER Scale AGAIN ALIAS 'SRC_SCALE'
USE (lcSourceDir+"SCALEHD.DBF") SHARED IN 0 ORDER EXTSCALE  AGAIN ALIAS 'SRC_SCALEHD' && CEXTSCALE

IF !SEEK(lcScaleID,'SCALEHD') 
  =SEEK(lcScaleID,'SRC_SCALEHD')
  SELECT SRC_SCALEHD
  SCATTER MEMO MEMVAR
  m.cadd_user = 'MEL'
  m.dadd_date = DATE()
  
  INSERT INTO 'SCALEHD' FROM MEMVAR
  SELECT SRC_SCALE
  =SEEK('S'+lcScaleID) 
  SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcScaleID
    SCATTER MEMO MEMVAR 
  m.cadd_user = 'MEL'
  m.dadd_date = DATE()
    INSERT INTO 'SCALE' FROM MEMVAR
  ENDSCAN
  llSRetValue = .T.
ELSE
  =SEEK(lcScaleID,'SRC_SCALEHD')
  IF SRC_SCALEHD.nnofcodes != scalehd.nnofcodes OR SRC_SCALEHD.cdim1desc != scalehd.cdim1desc OR SRC_SCALEHD.cdim2desc != scalehd.cdim2desc OR;
     SRC_SCALEHD.cdim3desc != scalehd.cdim3desc OR SRC_SCALEHD.cscaledes !=scalehd.cscaledes OR SRC_SCALEHD.nnoofdim !=scalehd.nnoofdim

    =FPUTS(EReport , ' Scale Conflict. Scale# '+ALLTRIM(lcScaleID))
    RFlag = .F.

    llSRetValue = .F. 
  ELSE
    SELECT SRC_SCALE
    =SEEK('S'+lcScaleID) 
    llSRetValue = .T.
    SCAN REST WHILE TYPE+SCALE+PREPAK = 'S'+lcScaleID
      IF !SEEK(SRC_SCALE.TYPE+SRC_SCALE.SCALE+SRC_SCALE.PREPAK,'SCALE','SCALE')

        =FPUTS(EReport , ' Scale Conflict. Scale# '+ALLTRIM(lcScaleID))
        RFlag = .F.
        llSRetValue = .F.
        EXIT
      ELSE
        IF scale.cscl_desc != SRC_SCALE.cscl_desc OR scale.cdim1 != SRC_SCALE.cdim1 OR scale.cdim2  != SRC_SCALE.cdim2  OR ;
		   scale.cnrfcode1 != SRC_SCALE.cnrfcode1 OR scale.cnrfcode2 != SRC_SCALE.cnrfcode2 OR scale.cnrfcode3 != SRC_SCALE.cnrfcode3 OR ;
		   scale.cnrfcode4 != SRC_SCALE.cnrfcode4 OR scale.cnrfcode5 != SRC_SCALE.cnrfcode5 OR scale.cnrfcode6 != SRC_SCALE.cnrfcode6 OR ;
		   scale.cnrfcode7 != SRC_SCALE.cnrfcode7 OR scale.cnrfcode8 != SRC_SCALE.cnrfcode8 OR scale.cnt != SRC_SCALE.cnt OR ;
		   scale.pp1 != SRC_SCALE.pp1 OR scale.pp2 != SRC_SCALE.pp2 OR scale.pp3 != SRC_SCALE.pp3 OR scale.pp4 != SRC_SCALE.pp4 OR ;
		   scale.pp5 != SRC_SCALE.pp5 OR scale.pp6 != SRC_SCALE.pp6 OR scale.pp7 != SRC_SCALE.pp7 OR scale.pp8 != SRC_SCALE.pp8 OR ;
		   scale.pptot != SRC_SCALE.pptot OR scale.sz1 != SRC_SCALE.sz1 OR scale.sz2 != SRC_SCALE.sz2 OR scale.sz3 != SRC_SCALE.sz3 OR ;
		   scale.sz4 != SRC_SCALE.sz4 OR scale.sz5 != SRC_SCALE.sz5 OR scale.sz6 != SRC_SCALE.sz6 OR scale.sz7 != SRC_SCALE.sz7 OR ;
		   scale.sz8 != SRC_SCALE.sz8 

           =FPUTS(EReport , ' Scale Conflict. Scale# '+ALLTRIM(lcScaleID))
           RFlag = .F.

           llSRetValue = .F.
           EXIT
		ENDIF   
      ENDIF
    ENDSCAN  
  ENDIF
ENDIF
USE IN 'SRC_SCALEHD'
USE IN 'SRC_SCALE'

SELECT(lnOldAlias)
RETURN llSRetValue  
**
*:**************************************************************************

*:**************************************************************************
*:* Name        : RESVST
*:* Developer   : AEG - Abdelrahman Essam
*:* Date        : 10/19/2016
*:* Purpose     : receive style with no UPC or SKU for melwod customization
*:**************************************************************************
FUNCTION lfresvst


IF SEEK(PADR(MStyl,LEN(lcMjrPic))+lcStyleSep+PADR(MColor,lnClrLen)+lcClrSep+MSCALE,'STYLE') .AND. SEEK('S' + STYLE.SCALE , 'SCALE') 
    IF SCALE.CNT = 1
       lcSize  = '1'
       MSize = PADR(ALLTRIM(SCALE.SZ1),5)
    ELSE
     FOR lnCount = 1 TO SCALE.CNT
      lcCount = STR(lnCount,1)
 
      IF PADR(ALLTRIM(SCALE.SZ&lcCount),5) = PADR(ALLTRIM(MSize),5)
       lcSize = lcCount
       EXIT
      ENDIF
     ENDFOR
    ENDIF

   IF !EMPTY(lcSize)
    MPrePck = .F.
    MOrd    = 6
   ENDIF
ENDIF

return .T.
