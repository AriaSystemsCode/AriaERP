_SCREEN.Visible = .F. 
SET CPDIALOG OFF 
SET RESOURCE OFF 
lcSYSPath = GETDIR("","Select system File Path directory")
IF EMPTY(lcSYSPath)
  MESSAGEBOX('Selected Directory is Incorrect')
  RETURN .F.
ENDIF
lcSYSPath = ADDBS(lcSYSPath)

USE  (lcSYSPath+"SYCCOMP.DBF") SHARED  IN 0 
SELECT SYCCOMP
SCAN FOR lrunFromA4
  lcDatapath = ADDBS(ALLTRIM(syccomp.ccom_ddir))
  USE (lcDatapath +'Setups.dbf') IN 0 SHARED 
  =SEEK('ICM_USEEXSSC','SetUps','MODVAR')
  IF !(ALLTRIM(Setups.mdata_def) =".T.")
    USE IN Setups
    LOOP 
  ENDIF
*!*	  USE (lcDatapath +'Style.dbf') IN 0 ORDER cStyle SHARED 
*!*	  USE (lcDatapath +'Scale.dbf') IN 0 ORDER Scale SHARED 
  lcConnStr = "Driver={SQL Server};server="+ALLTRIM(SYCCOMP.CCONSERVER)+";DATABASE="+ALLTRIM(SYCCOMP.CCONDBNAME)+;
                ";uid="+ALLTRIM(SYCCOMP.CCONUSERID)+";pwd="+ALLTRIM(SYCCOMP.CCONPASWRD)
  lnSqlConn = SQLSTRINGCONNECT(lcConnStr)
  IF lnSqlConn > 0
    lnBomFile = SQLEXEC(lnSqlConn ,"Select * from BOM where ccatgtyp In ('F','T') and convert(CHAR,MSZCROSREF) <> ' '",'BOM_FILE')
    IF lnBomFile > 0
	  lcMScale   = '*  '
      SELECT 'BOM_FILE'
      SCAN 
        lcRet = ''
*!*		    =SEEK(BOM_FILE.citmmajor,'STYLE')
*!*		    lcSScale = STYLE.SCALE
*!*		    =SEEK('S'+lcSScale,'Scale')
        DIMENSION laSizesArr[1]
        laSizesArr = ''
        =gfSubStr(BOM_FILE.msizes ,@laSizesArr,CHR(13))
        FOR lnA =1 TO ALEN(laSizesArr,1)
          lnSclStrt = ATC('~',laSizesArr[lnA])
          IF lnSclStrt > 0
            lcScaleStr = SUBSTR(laSizesArr[lnA],lnSclStrt +1)
            lcSScale = SUBSTR(laSizesArr[lnA],1,lnSclStrt -1)
            DIMENSION laScalesArr[1]
            laScalesArr = ''
            =gfSubStr(lcScaleStr,@laScalesArr,',')
            FOR lnB = 1 TO ALEN(laScalesArr,1)
	          lcRet = lcRet +  lcSScale + ',' + laScalesArr[lnB] + '~' + lcMScale + ',1' + CHR(13)  
            ENDFOR
          ENDIF  
        ENDFOR
		IF !(ALLTRIM(BOM_FILE.MSZCROSREF) == lcRet )
		  lcReNo = BOM_FILE.REC_NO
		  lcCINVTYPE = BOM_FILE.CINVTYPE
		  lcCITMMAJOR = BOM_FILE.CITMMAJOR
		  lcCCSTSHTTYP = BOM_FILE.CCSTSHTTYP
		  lcCCSTSHT_ID =BOM_FILE.CCSTSHT_ID
		  lcTYP = BOM_FILE.TYP
		  lcCITMMASK = BOM_FILE.CITMMASK
		  lcMFGCODE = BOM_FILE.MFGCODE
		  lcCINVTYPC = BOM_FILE.CINVTYPC
		  lcITEM = BOM_FILE.ITEM
		  lnNLINENO = BOM_FILE.NLINENO
		  STRTOFILE('Style:'+BOM_FILE.CITMMAJOR+'  RecordNo:'+BOM_FILE.REC_NO+CHR(13)+CHR(10),'BomFixedLines.txt',1)
		  BomFilefix= SQLEXEC(lnSqlConn ,"update BOM Set MSZCROSREF = '"+lcRet+"' WHERE CINVTYPE='"+lcCINVTYPE+;
		  				  "' AND CITMMAJOR='"+lcCITMMAJOR +;
		  				  "' AND CCSTSHTTYP='"+lcCCSTSHTTYP+"' AND CCSTSHT_ID= '"+lcCCSTSHT_ID+;
		  				  "' AND TYP='"+lcTYP+"' AND CITMMASK='"+lcCITMMASK+;
		  				  "' AND MFGCODE='"+lcMFGCODE +"' AND ITEM='"+lcITEM+;
		  				  "' AND NLINENO = '"+STR(lnNLINENO ,6)+"' AND REC_NO = '"+lcReNo +"'")
		ENDIF
      ENDSCAN 
*!*	      USE IN STYLE
*!*	      USE IN SCALE
      USE IN Setups
      IF USED('BOM_FILE')
        use in 'BOM_FILE'
      ENDIF
    ENDIF  
  ENDIF
ENDSCAN 
CLOSE ALL 

FUNCTION gfSubStr
PARAMETERS lcString,lnAryOrPos,lcSepta

lcSubstr  =' '
lnAryDim  = 1
lnAryRows = 1
lnAryCols = 1
lcSepta   = IIF(TYPE('lcSepta')='C',lcSepta,',') 

IF LEN(ALLTRIM(lcSepta))>1
  lcColSep  = SUBSTR(lcSepta,2,1)
  lcSepta   = LEFT(lcSepta,1)
  lnAryDim  = IIF(OCCURS(lcSepta,lcString)>0,;
              OCCURS(lcSepta,lcString)+;
              IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
              lnAryDim)
  lnAryCols = IIF(OCCURS(lcColSep,lcString)>0,;
              OCCURS(lcColSep,lcString)+;
              IIF(RIGHT(lcString,1)<>lcColSep,1,0),;
              lnAryDim)
  lnAryRows = (lnAryDim+(lnAryCols-1)) / lnAryCols
  lnAryDim  = lnAryDim +(lnAryCols-1)     
  lcString  = STRTRAN(lcString,lcColSep,lcSepta)
ELSE
  lnAryDim = IIF(OCCURS(lcSepta,lcString)>0,;
             OCCURS(lcSepta,lcString)+;
             IIF(RIGHT(lcString,1)<>lcSepta,1,0),;
             lnAryDim)
ENDIF

*** Chek if second parameter array or numeric
DO CASE
  *** If no parameter found assume firest part of string
  CASE TYPE ('lnAryOrPos')='U'
    lnAryOrPos = 1

  *** If array strich it to hold all string parts
  CASE TYPE ('lnAryOrPos') $ 'C,L'    
    IF lnAryCols > 1
      DIMENSION lnAryOrPos[lnAryRows,lnAryCols]
    ELSE
      IF ALEN(lnAryOrPos,2) > 0
        DIMENSION lnAryOrPos[lnAryDim,ALEN(lnAryOrPos,2)]
      ELSE
        DIMENSION lnAryOrPos[lnAryDim]
      ENDIF  

    ENDIF
    lnAryOrPos  = ' '

ENDCASE

FOR lnArElem  = 1 TO lnAryDim
  IF TYPE ('lnAryOrPos')='N'
    lnArElem = lnAryOrPos
  ENDIF  

  DO CASE
    *** In case of firest string part
    CASE lnArElem = 1
      lcSubstr = SUBSTR(lcString,1,;
      IIF(lcSepta $ lcString,AT(lcSepta,lcString)-1,LEN(lcString)))

    *** In case of last string part
    CASE lnArElem = lnAryDim
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1)
      lcSubstr = IIF(RIGHT(lcSubstr,1)=lcSepta,;
                 SUBSTR(lcSubstr,1,LEN(lcSubstr)-1),lcSubstr)
    *** In case of any string part from the meddel
    CASE lnArElem > 1
      lcSubstr = SUBSTR(lcString,AT(lcSepta,lcString,lnArElem-1)+1,;
                 AT(lcSepta,lcString,lnArElem)-;
                 AT(lcSepta,lcString,lnArElem-1)-1)
  ENDCAS

  IF TYPE ('lnAryOrPos')='N'
    RETURN lcSubstr
  ENDIF  
  
  IF lnAryCols > 1
    lnAryOrPos[((lnArElem-1)%lnAryRows)+1,INT((lnArElem-1)/lnAryRows)+1] = lcSubstr
  ELSE
    lnAryOrPos[lnArElem] = lcSubstr
  ENDIF
ENDFOR


