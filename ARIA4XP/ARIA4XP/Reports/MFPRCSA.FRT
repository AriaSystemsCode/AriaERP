  3   @                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              %ORIENTATION=0
PAPERSIZE=1
COLOR=1
                         Arial                                                         EVAL(lcMGroup)                                                EVAL(lcInGroup)                                               	lfGetOp()                                                     Courier New                                                   .F.                                                           *STRTRAN(lcBOMTit,'#')+' Bill of Materials'                    Arial                                                         lcBOMTit+SPACE(2)+cuttkt                                      "@I"                                                          Arial                                                         lcTime                                                        Arial                                                         "Time"                                                        Arial                                                         ":"                                                           Arial                                                         	gdSysDate                                                     Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         ALLTRIM(STR(_pageno))                                         Arial                                                         "Item"                                                        Arial                                                         "Color"                                                       Arial                                                         "Width"                                                       Arial                                                         "Vendor"                                                      Arial                                                         "UOM"                                                         Arial                                                         "Yield"                                                       Arial                                                         
"Unt Cost"                                                    Arial                                                         LLRPCost                                                      
"Required"                                                    Arial                                                         "Issued"                                                      Arial                                                         ''                                                            Courier New                                                   .F.                                                           substr(item,1,lnMajor)                                        Arial                                                         substr(item,lnMajor+2,lnNMajor)                               Arial                                                         tuom.cuom_v                                                   Arial                                                         untqty                                                        Arial                                                         untcost                                                       Arial                                                         LLRPCost                                                      req_qty                                                       Arial                                                         	issue_qty                                                     Arial                                                         desc                                                          Arial                                                         "Description :"                                               Arial                                                         dyelot                                                        Arial                                                         'cLotNo' $ lcMGroup                                           Fabric.width                                                  Arial                                                         cCatgTyp = 'F'                                                Fabric.vendor                                                 Arial                                                         cCatgTyp = 'F'                                                "Pulled by"                                                   Arial                                                         "Date :"                                                      Arial                                                         "[  CONTINUED NEXT PAGE  ]"                                   Arial                                                         !llEndGroup                                                   lfEndGroup()                                                  Arial                                                         lfStGroup()                                                   Arial                                                         lcHead1                                                       Arial                                                         lcHead2                                                       Arial                                                         	lcCostItm                                                     Arial                                                         &IIF('cLotNo' $ lcMGroup,'Dyelot :','')                        Arial                                                         W'Lot No #'+SPACE(2)+IIF('cLotNo' $ lcMGroup, cLotNo,lcLotNo) +SPACE(2) +ALLTRIM(lcOper)                                       "@I"                                                          Arial                                                         ='cLotNo' $ lcMGroup AND oariaapplication.activemoduleid<>'MA'                                                                 	"Pattern"                                                     Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          ":"                                                           Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  ":"                                                           Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          lcStyMaj                                                      Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  lcPattrn                                                      Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          eIIF(oAriaApplication.ActiveModuleID='MF','STYLE',IIF(oAriaApplication.ActiveModuleID='MA','Item',''))                         Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  "Page"                                                        Arial                                                         ":"                                                           Arial                                                         lcGetTtl                                                      ''                                                            lfGetTitle()                                                  Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               YTop = 0
Left = 0
Width = 0
Height = 0
DataSource = .NULL.
Name = "Dataenvironment"
                                     %ORIENTATION=0
PAPERSIZE=1
COLOR=1
                         Arial                                                         EVAL(lcMGroup)                                                EVAL(lcInGroup)                                               	lfGetOp()                                                     Courier New                                                   .F.                                                           *STRTRAN(lcBOMTit,'#')+' Bill of Materials'                    Arial                                                         lcBOMTit+SPACE(2)+cuttkt                                      "@I"                                                          Arial                                                         lcTime                                                        Arial                                                         "Time"                                                        Arial                                                         ":"                                                           Arial                                                         	gdSysDate                                                     Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         ALLTRIM(STR(_pageno))                                         Arial                                                         "Item"                                                        Arial                                                         "Color"                                                       Arial                                                         "Width"                                                       Arial                                                         "Vendor"                                                      Arial                                                         "UOM"                                                         Arial                                                         "Yield"                                                       Arial                                                         
"Unt Cost"                                                    Arial                                                         LLRPCost                                                      
"Required"                                                    Arial                                                         "Issued"                                                      Arial                                                         ''                                                            Courier New                                                   .F.                                                           substr(item,1,lnMajor)                                        Arial                                                         substr(item,lnMajor+2,lnNMajor)                               Arial                                                         tuom.cuom_v                                                   Arial                                                         untqty                                                        Arial                                                         untcost                                                       Arial                                                         LLRPCost                                                      req_qty                                                       Arial                                                         	issue_qty                                                     Arial                                                         desc                                                          Arial                                                         "Description :"                                               Arial                                                         dyelot                                                        Arial                                                         'cLotNo' $ lcMGroup                                           Fabric.width                                                  Arial                                                         cCatgTyp = 'F'                                                Fabric.vendor                                                 Arial                                                         cCatgTyp = 'F'                                                "Pulled by"                                                   Arial                                                         "Date :"                                                      Arial                                                         "[  CONTINUED NEXT PAGE  ]"                                   Arial                                                         !llEndGroup                                                   lfEndGroup()                                                  Arial                                                         lfStGroup()                                                   Arial                                                         lcHead1                                                       Arial                                                         lcHead2                                                       Arial                                                         	lcCostItm                                                     Arial                                                         &IIF('cLotNo' $ lcMGroup,'Dyelot :','')                        Arial                                                         W'Lot No #'+SPACE(2)+IIF('cLotNo' $ lcMGroup, cLotNo,lcLotNo) +SPACE(2) +ALLTRIM(lcOper)                                       "@I"                                                          Arial                                                         ='cLotNo' $ lcMGroup AND oariaapplication.activemoduleid<>'MA'                                                                 	"Pattern"                                                     Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          ":"                                                           Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  ":"                                                           Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          lcStyMaj                                                      Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  lcPattrn                                                      Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          eIIF(oAriaApplication.ActiveModuleID='MF','STYLE',IIF(oAriaApplication.ActiveModuleID='MA','Item',''))                         Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  "Page"                                                        Arial                                                         ":"                                                           Arial                                                         lcGetTtl                                                      ''                                                            lfGetTitle()                                                  Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               YTop = 0
Left = 0
Width = 0
Height = 0
DataSource = .NULL.
Name = "Dataenvironment"
                                     EVAL(lcMGroup)                                                EVAL(lcInGroup)                                               	lfGetOp()                                                     "Item"                                                        "Color"                                                       "Width"                                                       "Vendor"                                                      "UOM"                                                         "Yield"                                                       
"Required"                                                    "Issued"                                                      ''                                                            SUBSTR(ITEM,1,LNMAJOR)                                        SUBSTR(ITEM,LNMAJOR+2,LNNMAJOR)                               TUOM.CUOM_V                                                   untqty                                                        IIF(LLRPCOST,untcost,'')                                      req_qty                                                       	issue_qty                                                     desc                                                          "Description :"                                               dyelot                                                        Fabric.width                                                  Fabric.vendor                                                 "Pulled by"                                                   "Date :"                                                      "[  CONTINUED NEXT PAGE  ]"                                   lfEndGroup()                                                  lfStGroup()                                                   lcHead1                                                       lcHead2                                                       	lcCostItm                                                     &IIF('cLotNo' $ lcMGroup,'Dyelot :','')                        dIIF(OARIAAPPLICATION.ACTIVEMODULEID='MF','STYLE',IIF(OARAAPPLICATION.ACTIVEMODULEID='MA','Item',''))                          lcPattrn                                                      lcStyMaj                                                      "Date :"                                                      "Time :"                                                      "Page :"                                                      	gdSysDate                                                     lcTime                                                        ALLTRIM(STR(_pageno))                                         *STRTRAN(lcBOMTit,'#')+' Bill of Materials'                    "@I"                                                          W'Lot No #'+SPACE(2)+IIF('cLotNo' $ lcMGroup, cLotNo,lcLotNo) +SPACE(2) +ALLTRIM(lcOper)                                       lcBOMTit+SPACE(2)+cuttkt                                      "@I"                                                          nIIF(OARIAAPPLICATION.ACTIVEMODULEID='MF','Pattern :',IIF(OARIAAPPLICATION.ACTIVEMODULEID='MA','Pattern :',''))                ^IIF(OARIAAPPLICATION.ACTIVEMODULEID='MF',':',IIF(OARIAAPPLICATION.ACTIVEMODULEID='MA',':',''))                                IIF(LLRPCOST,'Unt Cost','')                                   LCGETTTL                                                      ''                                                            lfGetTitle()                                                  %ORIENTATION=0
PAPERSIZE=1
COLOR=1
                         Arial                                                         EVAL(lcMGroup)                                                EVAL(lcInGroup)                                               	lfGetOp()                                                     Courier New                                                   .F.                                                           *STRTRAN(lcBOMTit,'#')+' Bill of Materials'                    Arial                                                         lcBOMTit+SPACE(2)+cuttkt                                      "@I"                                                          Arial                                                         lcTime                                                        Arial                                                         "Time"                                                        Arial                                                         ":"                                                           Arial                                                         	gdSysDate                                                     Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         "Date"                                                        Arial                                                         ":"                                                           Arial                                                         ALLTRIM(STR(_pageno))                                         Arial                                                         "Item"                                                        Arial                                                         "Color"                                                       Arial                                                         "Width"                                                       Arial                                                         "Vendor"                                                      Arial                                                         "UOM"                                                         Arial                                                         "Yield"                                                       Arial                                                         
"Unt Cost"                                                    Arial                                                         LLRPCost                                                      
"Required"                                                    Arial                                                         "Issued"                                                      Arial                                                         ''                                                            Courier New                                                   .F.                                                           substr(item,1,lnMajor)                                        Arial                                                         substr(item,lnMajor+2,lnNMajor)                               Arial                                                         tuom.cuom_v                                                   Arial                                                         untqty                                                        Arial                                                         untcost                                                       Arial                                                         LLRPCost                                                      req_qty                                                       Arial                                                         	issue_qty                                                     Arial                                                         desc                                                          Arial                                                         "Description :"                                               Arial                                                         dyelot                                                        Arial                                                         'cLotNo' $ lcMGroup                                           Fabric.width                                                  Arial                                                         cCatgTyp = 'F'                                                Fabric.vendor                                                 Arial                                                         cCatgTyp = 'F'                                                "Pulled by"                                                   Arial                                                         "Date :"                                                      Arial                                                         "[  CONTINUED NEXT PAGE  ]"                                   Arial                                                         !llEndGroup                                                   lfEndGroup()                                                  Arial                                                         lfStGroup()                                                   Arial                                                         lcHead1                                                       Arial                                                         lcHead2                                                       Arial                                                         	lcCostItm                                                     Arial                                                         &IIF('cLotNo' $ lcMGroup,'Dyelot :','')                        Arial                                                         W'Lot No #'+SPACE(2)+IIF('cLotNo' $ lcMGroup, cLotNo,lcLotNo) +SPACE(2) +ALLTRIM(lcOper)                                       "@I"                                                          Arial                                                         ='cLotNo' $ lcMGroup AND oariaapplication.activemoduleid<>'MA'                                                                 	"Pattern"                                                     Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          ":"                                                           Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  ":"                                                           Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          lcStyMaj                                                      Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  lcPattrn                                                      Arial                                                         $oAriaApplication.ActiveModuleID='MF'                          eIIF(oAriaApplication.ActiveModuleID='MF','STYLE',IIF(oAriaApplication.ActiveModuleID='MA','Item',''))                         Arial                                                         LoAriaApplication.ActiveModuleID='MF' OR oAriaApplication.ActiveModuleID='MA'                                                  "Page"                                                        Arial                                                         ":"                                                           Arial                                                         lcGetTtl                                                      ''                                                            lfGetTitle()                                                  Arial                                                         Courier New                                                   Arial                                                         Arial                                                         Arial                                                         Arial                                                         Arial                                                         dataenvironment                                               YTop = 0
Left = 0
Width = 0
Height = 0
DataSource = .NULL.
Name = "Dataenvironment"
                                     EVAL(lcMGroup)                                                EVAL(lcInGroup)                                               	lfGetOp()                                                     "Item"                                                        "Color"                                                       "Width"                                                       "Vendor"                                                      "UOM"                                                         "Yield"                                                       
"Required"                                                    "Issued"                                                      ''                                                            SUBSTR(ITEM,1,LNMAJOR)                                        SUBSTR(ITEM,LNMAJOR+2,LNNMAJOR)                               TUOM.CUOM_V                                                   untqty                                                        IIF(LLRPCOST,untcost,'')                                      req_qty                                                       	issue_qty                                                     desc                                                          "Description :"                                               dyelot                                                        Fabric.width                                                  Fabric.vendor                                                 "Pulled by"                                                   "Date :"                                                      "[  CONTINUED NEXT PAGE  ]"                                   lfEndGroup()                                                  lfStGroup()                                                   lcHead1                                                       lcHead2                                                       	lcCostItm                                                     &IIF('cLotNo' $ lcMGroup,'Dyelot :','')                        dIIF(OARIAAPPLICATION.ACTIVEMODULEID='MF','STYLE',IIF(OARAAPPLICATION.ACTIVEMODULEID='MA','Item',''))                          lcPattrn                                                      lcStyMaj                                                      "Date :"                                                      "Time :"                                                      "Page :"                                                      	gdSysDate                                                     lcTime                                                        ALLTRIM(STR(_pageno))                                         *STRTRAN(lcBOMTit,'#')+' Bill of Materials'                    "@I"                                                          W'Lot No #'+SPACE(2)+IIF('cLotNo' $ lcMGroup, cLotNo,lcLotNo) +SPACE(2) +ALLTRIM(lcOper)                                       lcBOMTit+SPACE(2)+cuttkt                                      "@I"                                                          nIIF(OARIAAPPLICATION.ACTIVEMODULEID='MF','Pattern :',IIF(OARIAAPPLICATION.ACTIVEMODULEID='MA','Pattern :',''))                ^IIF(OARIAAPPLICATION.ACTIVEMODULEID='MF',':',IIF(OARIAAPPLICATION.ACTIVEMODULEID='MA',':',''))                                IIF(LLRPCOST,'Unt Cost','')                                   LCGETTTL                                                      ''                                                            lfGetTitle()                                                  ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                                     ORIENTATION=0PAPERSIZE=1                               