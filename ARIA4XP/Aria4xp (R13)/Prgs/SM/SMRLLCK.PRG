*:************************************************************************
*:  Program File: ARIA4XP\PRGS\SM\SMRLLCK.PRG
*:  Module      : System Manager
*:  Desc.       : Release Lock Program
*:  System      : Aria 4XP
*:  Developer   : MMT - Mariam Mazhar
*:  Date        : 07/20/2022
*:  Reference   : *E612591,1[T20220615.0005]
*:************************************************************************
#INCLUDE R:\Aria4xp\Prgs\sm\SMRLLCK.h 
DECLARE laRpEntDsc[1,1] ,laRpEntNo[1,1]
STORE SPACE(0) TO laRpEntDsc, laRpEntNo
lcIndexExpression = ''
lcIndexExpr = ''
lcsearchfield = ''
lcExpr = gfOpGrid('SMRLLCK' , .T.)&&,.F.,.F.,.T.,.T.)
RETURN

*!*************************************************************
*! Name      : lfvSelEntity
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/20/2022
*! Purpose   : Validation function of the Entity type selection
*!*************************************************************
FUNCTION lfvSelEntity

IF ASCAN(LAOGOBJTYPE,'laOGFxFlt[1,6]') > 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'laOGFxFlt[1,6]'),1)
  LAOGOBJCNT[LNPOS] = (UPPER(ALLTRIM(lcRpEntity)) != "SELECT")
  = LFOGSHOWGET('laOGFxFlt[1,6]')
ENDIF

*lcFileData
lnDataPos = ASCAN(laOGFxFlt,'lcFileData')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOGFxFlt,lnDataPos,1)
  IF USED(laOGFxFlt[lnDataPos ,6])
    ZAP IN (laOGFxFlt[lnDataPos ,6])
    laOGFxFlt[lnDataPos ,6]=""
  ENDIF
ENDIF


IF UPPER(ALLTRIM(lcRpEntity)) = "SELECT"
  RETURN
ENDIF

lcVar=''
lcIndexName=''
lcbrowflds=''
lcbrowTable=''
SELECT (lcEntitySetting)
LOCATE FOR entityName = lcRpEntity  
lcfile = ALLTRIM(&lcEntitySetting..MasterTable)
IF !USED(ALLTRIM(lcfile))
  =gfOpenTable(ALLTRIM(lcfile),ALLTRIM(&lcEntitySetting..MasterTableIndex),'SH')
ENDIF  
lcIndexExpression = ALLTRIM(&lcEntitySetting..MasterTableIndexExpression)
lcExpVar = UPPER(ALLTRIM(&lcEntitySetting..IndexExpressionDefaultValue)) 
lcIndexExpr = STRTRAN(lcExpVar ,UPPER(ALLTRIM(&lcEntitySetting..searchfield)),"")
lcIndexExpr = STRTRAN(lcIndexExpr ,',','+')
lcsearchfield = STRTRAN(&lcEntitySetting..searchfield,",","+")
lcVar = lcsearchfield 
IF RIGHT(lcIndexExpr ,1)='+'
  lcIndexExpr =SUBSTR(lcIndexExpr ,1,LEN(lcIndexExpr)-1)
ENDIF  
oxml =  CREATEOBJECT("MSXML2.DOMDocument")
oxml.Loadxml(aLLTRIM(&lcEntitySetting..ExtraData1))
loRoot = oxml.childNodes(0)
lcBrowField  =''
FOR lnX = 0 TO loRoot.childNodes(0).childNodes.Length-1
  lcFldName = ''
  lcDatType =''
  lnWidth = ''
  lnDecimals = 0
  lcDesc = ''
  FOR lnZ =  0 TO loRoot.childNodes(0).childNodes(lnX).childNodes.Length-1
    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='NAME'
	  lcFldName =  ALLTRIM(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
	  LOOP
    ENDIF
    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='DESCRIPTION'
      lcDesc =  ALLTRIM(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
      LOOP
    ENDIF
  ENDFOR 
  lcBrowField = lcBrowField +lcFldName +":H='"+lcDesc +"',"
ENDFOR		  
lcbrwFields = SUBSTR(lcBrowField ,1,LEN(lcBrowField )-1)
lcbrowflds=lcbrwFields
lcbrowTable=lcfile
lcTableName=lcbrowTable
lcTag="'"+lcVar+"'"
lcForExp = lcIndexExpr 

*!*************************************************************
*! Name      : lfogwhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/20/2022
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfogwhen
laRpEntDsc [1,1]=  "Select Entity"
laRpEntNo[1,1]=  "SELECT"  
*lcEntitySetting = gfTempName()
lnEntTypSel = oAriaApplication.RemoteCompanyData.execute("Select EntityType.id as 'entityName',EntityType.Oid,EntityTypeSettings.*, CAST(EntityTypeSettings.ExtraData as text) as 'ExtraData1'  from [EntityType] Inner Join"+; 
                                                         "[EntityTypeSettings] ON [EntityTypeSettings].entitytypeoid= EntityType.oid",'',;
                                                         lcEntitySetting,"",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
IF USED(lcEntitySetting) AND RECCOUNT(lcEntitySetting) > 0
  SELECT (lcEntitySetting)
  LOCATE
  SCAN
    DIMENSION laRpEntDsc [ALEN(laRpEntDsc ,1)+1,1],laRpEntNo [ALEN(laRpEntNo ,1)+1,1]
    laRpEntDsc [ALEN(laRpEntDsc ,1),1] = EVALUATE(lcEntitySetting+'.entityName')
    laRpEntNo[ALEN(laRpEntNo,1),1] = EVALUATE(lcEntitySetting+'.entityName')
  ENDSCAN
ENDIF

IF ASCAN(LAOGOBJTYPE,'laOGFxFlt[1,6]') > 0
  LNPOS= ASUBSCRIPT(LAOGOBJTYPE,ASCAN(LAOGOBJTYPE,'laOGFxFlt[1,6]'),1)
  LAOGOBJCNT[LNPOS] = (UPPER(ALLTRIM(lcRpEntity)) != "SELECT")
  = LFOGSHOWGET('laOGFxFlt[1,6]')
ENDIF

*!*************************************************************
*! Name      : lfClearLock
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/20/2022
*! Purpose   : Clear Lock function
*!*************************************************************
FUNCTION lfClearLock
IF UPPER(ALLTRIM(lcRpEntity)) = "SELECT"
  RETURN
ENDIF


IF gfModalGen('QRM00000B00006', .F., .F., .F., LANG_WARNING) =2
  RETURN .F.
ENDIF
lcSelRec = ''
lnDataPos = ASCAN(laOGFxFlt,'lcFileData')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOGFxFlt,lnDataPos,1)
  IF !EMPTY(laOGFxFlt[lnDataPos ,6]) and USED(laOGFxFlt[lnDataPos ,6])
    lcSelRec = laOGFxFlt[lnDataPos ,6]
  ENDIF
ENDIF
IF !EMPTY(lcSelRec) AND USED(lcSelRec ) 
  SELECT(lcSelRec )
  LOCATE 
  IF EOF()
    lcSelRec=''
  ENDIF
ENDIF

lcEntitySet = lcEntitySetting
SELECT (ALLTRIM(&lcEntitySet..MasterTable))
lcExpr =STRTRAN(lcIndexExpression,",","+")
IF !EMPTY(lcIndexExpr)
  =gfSeek(EVALUATE(lcIndexExpr))
ELSE
  =gfSeek('')  
ENDIF   
IF !EMPTY(lcIndexExpr)
	SCAN REST WHILE &lcExpr = IIF(!EMPTY(lcIndexExpr),EVALUATE(lcIndexExpr),'') FOR IIF(!EMPTY(lcSelRec),SEEK(EVALUATE(lcsearchfield),lcSelRec),.T.)
	  REPLACE  llok_stat WITH .F. ;
	           dlok_date WITH {} ;
	           clok_user WITH "" ;
	           clok_time WITH "" 
	  =gfReplace('')         
	ENDSCAN 
ELSE
  SCAN FOR IIF(!EMPTY(lcSelRec),SEEK(EVALUATE(lcsearchfield),lcSelRec),.T.)
    REPLACE  llok_stat WITH .F. ;
             dlok_date WITH {} ;
             clok_user WITH "" ;
             clok_time WITH "" 
    =gfReplace('')         
  ENDSCAN 
ENDIF	
SELECT (ALLTRIM(&lcEntitySet..MasterTable))
=gfTableUpdate()
=gfModalGen ('INM00282B00000','Alert')

