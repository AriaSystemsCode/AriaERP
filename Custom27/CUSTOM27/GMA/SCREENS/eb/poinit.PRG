LPARAMETERS oForm
DoDefault(oForm)
DIMENSION ThisFORMSET.ArrChkAccPrc[1,2]
STORE '' TO ThisFORMSET.ArrChkAccPrc
This.StylePriceObject    = CREATEOBJECT('StylePrice')
This.RelatedFieldsObject = CREATEOBJECT('GetRelatedFields')

LOCAL oGetMemVar , oItemMask , laItemMask , lnSegCount

oGetMemVar          = CREATEOBJECT('GetMemVar')
This.MultiCurrency  = oGetMemVar.Do('llMulCurr')

This.StyleSeparator = ''
oGetTempName = CREATEOBJECT('GetTempName')
This.BulkOrders = oGetTempName.DO()
*B038018,1 NMM Use variable name for temp order header file instead of fixed name.
This.lcOrdHdr  = oGetTempName.DO()
This.lcOrdLine = oGetTempName.DO()
This.lcAlocated = oGetTempName.DO()
This.lcOrdCanLn = oGetTempName.DO()
*B038018,1 NMM (End)

RELEASE oGetTempName
oItemMask           = CREATEOBJECT('GetItemMask')
This.StyleTitle     = ALLTRIM(oItemMask.Do("HI"))
This.StyleSeparator = ''
DIMENSION laItemMask[1,1]
=oItemMask.Do(@laItemMask)
FOR lnSegCount = 1 TO ALEN(laItemMask , 1)
  IF laItemMask[lnSegCount , 7]
    This.StyleSeparator = laItemMask[lnSegCount , 6]
    EXIT
  ENDIF
ENDFOR
oItemMask  = .NULL.

DIMENSION This.FilesToOpen[24,ALEN(This.FilesToOpen , 2)]
This.FilesToOpen       = ''

This.FilesToOpen[1,1]  = 'D'
This.FilesToOpen[1,2]  = 'ORDHDR'
This.FilesToOpen[1,5]  = 'ORDHDR'
This.FilesToOpen[1,7]  = 5

This.FilesToOpen[2,1]  = 'D'
This.FilesToOpen[2,2]  = 'ORDLINE'
This.FilesToOpen[2,5]  = 'ORDLINE'
This.FilesToOpen[2,7]  = 5

This.FilesToOpen[3,1]  = 'D'
This.FilesToOpen[3,2]  = 'GL_LINK'
This.FilesToOpen[3,3]  = This.LinkToGL
This.FilesToOpen[3,5]  = 'GL_LINK1'
This.FilesToOpen[3,7]  = 1

This.FilesToOpen[4,1]  = 'D'
This.FilesToOpen[4,2]  = 'StyleUPC'
This.FilesToOpen[4,3]  = This.UPCUse
This.FilesToOpen[4,5]  = 'StyUPCN'
This.FilesToOpen[4,7]  = 1

This.FilesToOpen[5,1]  = 'D'
This.FilesToOpen[5,2]  = 'Style'
This.FilesToOpen[5,5]  = 'Style'
This.FilesToOpen[5,7]  = 1

This.FilesToOpen[6,1]  = 'D'
This.FilesToOpen[6,2]  = 'Spck_Hdr'
This.FilesToOpen[6,5]  = 'Spck_Hdr'
This.FilesToOpen[6,7]  = 1

This.FilesToOpen[7,1]  = 'D'
This.FilesToOpen[7,2]  = 'Spck_Lin'
This.FilesToOpen[7,5]  = 'Spck_Lin'
This.FilesToOpen[7,7]  = 1

This.FilesToOpen[8,1]  = 'D'
This.FilesToOpen[8,2]  = 'Scale'
This.FilesToOpen[8,5]  = 'Scale'
This.FilesToOpen[8,7]  = 1

This.FilesToOpen[9,1]  = 'D'
This.FilesToOpen[9,2]  = 'CUSTDEPT'
This.FilesToOpen[9,5]  = 'CUSTDEPT'
This.FilesToOpen[9,7]  = 1

This.FilesToOpen[10,1] = 'D'
This.FilesToOpen[10,2] = 'Codes'
This.FilesToOpen[10,5] = ''
This.FilesToOpen[10,7] = 1

This.FilesToOpen[11,1] = 'D'
This.FilesToOpen[11,2] = 'SalesRep'
This.FilesToOpen[11,3] = ('SR' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[11,5] = 'SalesRep'
This.FilesToOpen[11,7] = 1

This.FilesToOpen[12,1] = 'D'
This.FilesToOpen[12,2] = 'Rep_Div'
This.FilesToOpen[12,3] = ('SR' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[12,5] = 'Rep_Div'
This.FilesToOpen[12,7] = 1

This.FilesToOpen[13,1] = 'D'
This.FilesToOpen[13,2] = 'StSpcPrc'
This.FilesToOpen[13,3] = This.StyleSpcPrice
This.FilesToOpen[13,5] = 'StSpcPrc'
This.FilesToOpen[13,7] = 1

This.FilesToOpen[14,1] = 'D'
This.FilesToOpen[14,2] = 'ORDHDR'
This.FilesToOpen[14,4] = 'ORDHDR1'
This.FilesToOpen[14,5] = 'OrdCust'
This.FilesToOpen[14,7] = 1

This.FilesToOpen[15,1] = 'D'
This.FilesToOpen[15,2] = 'ORDLINE'
This.FilesToOpen[15,4] = 'ORDLINE1'
This.FilesToOpen[15,5] = 'OrdLinSt'
This.FilesToOpen[15,7] = 1

This.FilesToOpen[16,1] = 'D'
This.FilesToOpen[16,2] = 'CutTktH'
This.FilesToOpen[16,3] = ('MF' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[16,5] = 'CUTTKTH'
This.FilesToOpen[16,7] = 1

This.FilesToOpen[17,1] = 'D'
This.FilesToOpen[17,2] = 'PosHdr'
This.FilesToOpen[17,3] = ('PO' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[17,5] = 'POSHDR'
This.FilesToOpen[17,7] = 1

This.FilesToOpen[18,1] = 'D'
This.FilesToOpen[18,2] = 'CutTktL'
This.FilesToOpen[18,3] = ('MF' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[18,5] = 'CUTTKTL'
This.FilesToOpen[18,7] = 1

This.FilesToOpen[19,1] = 'D'
This.FilesToOpen[19,2] = 'PosLn'
This.FilesToOpen[19,3] = ('PO' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[19,5] = 'POSLN'
This.FilesToOpen[19,7] = 1

This.FilesToOpen[20,1] = 'D'
This.FilesToOpen[20,2] = 'CUTPICK'
This.FilesToOpen[20,3] = ('MF' $ oAriaApplication.CompanyInstalledModules) .OR.;
                         ('PO' $ oAriaApplication.CompanyInstalledModules)
This.FilesToOpen[20,5] = 'CUTORD'
This.FilesToOpen[20,7] = 1

This.FilesToOpen[21,1] = 'D'
This.FilesToOpen[21,2] = 'StyDye'
This.FilesToOpen[21,5] = 'StyDye'
This.FilesToOpen[21,7] = 1

This.FilesToOpen[22,1] = 'D'
This.FilesToOpen[22,2] = 'ORDCANLN'
This.FilesToOpen[22,5] = 'Ordcanln'
This.FilesToOpen[22,7] = 1

This.FilesToOpen[23,1] = 'D'
This.FilesToOpen[23,2] = 'FACTOR'
This.FilesToOpen[23,3] = This.LinkToGL
This.FilesToOpen[23,5] = 'FACTOR'
This.FilesToOpen[23,7] = 1


This.FilesToOpen[24,1] = 'D'
This.FilesToOpen[24,2] = 'PACKS'
This.FilesToOpen[24,3] = FILE(oAriaApplication.DataDir+'PACKS.DBF')
This.FilesToOpen[24,5] = 'CPACK'
This.FilesToOpen[24,7] = 1


oObject = This
=gfDoTriger('EBMAIN',PADR('OPNFLS',10))


This.ScatterFields     = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,' +;
                         'MULTIPO,ENTERED,START,COMPLETE,cTermCode,' +;
                         'SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,' +;
                         'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,' +;
                         'PRIORITY,CFACCODE,REP1,COMM1,REP2,COMM2,' +;
                         'CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,' +;
                         'BOOKAMT,SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,' +;
                         'OPENAMT,CFROMORDER,CANCELLED,DECL_DATE,' +;
                         'DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,' +;
                         'NCURRUNIT,Alt_ShpTo'

