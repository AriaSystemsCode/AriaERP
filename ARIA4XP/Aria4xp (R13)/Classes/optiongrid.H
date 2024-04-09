*-- Option Grid Messages ..... BEGIN
#DEFINE LANG_CHANGEMODE                   "Changing report mode, Please wait."
#DEFINE LANG_PRINTCRITERIA                "Printing Selection Criteria, Please wait."
#DEFINE LANG_GENERATE                     "Generating Option Grid, Please wait."
*-- Option Grid Messages ..... END

*-- Option Grid Toolbar and Selection ..... BEGIN
*-- Destinations:
#DEFINE LANG_OUTPUT_PRINTER                "Printer"
#DEFINE LANG_OUTPUT_TEXT                   "Text"
#DEFINE LANG_OUTPUT_GRAPHICAL              "Graphics"
#DEFINE LANG_OUTPUT_TEXTFILE               "Text File"
#DEFINE LANG_OUTPUT_HTMLFILE               "HTML File"
#DEFINE LANG_OUTPUT_PDFFILE                "PDF File"
#DEFINE LANG_OUTPUT_XMLFILE                "XML File"
#DEFINE LANG_OUTPUT_EXCEL97                "Excel Workbook"
#DEFINE LANG_OUTPUT_EXCEL05                "Excel 5.0"
#DEFINE LANG_OUTPUT_EXCEL02                "Excel 2.0"
#DEFINE LANG_OUTPUT_BATCH	               "Batch Server"
#DEFINE LANG_EXPORT2EXCEL                "Exporting to Excel Workbook, Please Wait ..."
#DEFINE LANG_GENERATEHTM                 "Generating HTM file, Please Wait ..."
#DEFINE LANG_GENERATEXML                 "Generating XML formats, Please wait ..."
*-- Option Grid Toolbar and Selection ..... END

#DEFINE PICT_GRAPHICS					   ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "Graphics.BMP"
#DEFINE PICT_TEXT   					   ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "Text.BMP"
#DEFINE PICT_PRINT                         ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "Print.BMP"
#DEFINE PICT_PDF                           ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "PDF.BMP"
#DEFINE PICT_HTML                          ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "HTML.BMP"
#DEFINE PICT_XML                           ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "XML.BMP"
#DEFINE PICT_EXCEL                         ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "EXCEL.BMP"

*-- Send Email Constants
#DEFINE LANG_VIACONTACTS                 "From Contacts ..."
#DEFINE LANG_VIAOUTLOOK                  "From Outlook ..."

#DEFINE PICT_CONTACTS                    ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "CONTACT.BMP"
#DEFINE PICT_OUTLOOK                     ADDBS(ALLTRIM(oAriaApplication.BitmapHome)) + "AddsBook.BMP"

#DEFINE LANG_CANTLOADMAIL                "Couldn't instantiate Outlook object."

#DEFINE LANG_CUSTOMERTEMPLATE  "Customer Template ..."
#DEFINE LANG_VENDORTEMPLATE  "Vendor Template ..."

#DEFINE LANG_SAVECOPY  "Save a local copy?"
#DEFINE LANG_SENTOKAY  " was sent successfully. "
#DEFINE LANG_SENTFAIL  "Couldn't sent "
#DEFINE LANG_NORECIPIENTS  "There must be at least one mail or distribution list in the To, Cc, or Bcc box."
#DEFINE LANG_INVALIDMAILFORMATS  "Invalid email format."

* B038318,1 SMM [START]
*-- The Message that appears on Asking the user to enter outlook profile
#DEFINE LANG_ENTERPROFILE                 "Enter Outlook Profile"
* B038318,1 SMM [END]
* N037249,1 SMM [START]
*-- Loading Setting
#DEFINE LANG_LOADUDFSETTING    "Loading pre-defined values, Please wait."
* N037249,1 SMM [END]

*N000682,1 MMT 11/20/2012 Globalization changes[Start]
#DEFINE LANG_OPTIONGRID_OR "<< == .OR.  == >>"
#DEFINE LANG_OGTOOLBAR_SELECTSETTING  "Select User Setting"
#DEFINE LANG_OGTOOLBAR_RESETDEFSETTING "Reset to Default Setting"
#DEFINE LANG_OGTOOLBAR_SAVESETTING "Save User Setting"
#DEFINE LANG_OGTOOLBAR_DELETESETTING "Delete User Setting"
#DEFINE LANG_OGTOOLBAR_DEFAULT "Default"
#DEFINE LANG_OGTOOLBAR_ORDDETSTY "Order Detail / Style"
#DEFINE LANG_OGTOOLBAR_ORDDETACC "Order Detail / Account"
#DEFINE LANG_OGTOOLBAR_SORTBY 'Sort By'
#DEFINE LANG_OGTOOLBAR_PRINTSELCRITERIA 'Print Selection Criteria'
#DEFINE LANG_OGTOOLBAR_SELECTOUTPUTFORMAT 'Select Output Format'
#DEFINE LANG_OGTOOLBAR_PRINT 'Print'
#DEFINE LANG_OGTOOLBAR_EXPORTREPORT 'Export Report'
#DEFINE LANG_OGTOOLBAR_PRINTPREVIEW 'Print Preview'
#DEFINE LANG_OGTOOLBAR_MAILREC "Mail Recipient (as Attachment)"
#DEFINE LANG_OGTOOLBAR_FAXREC "Fax Recipient"
#DEFINE LANG_OGTOOLBAR_SENDBATCH "Send to Batch Server"
#DEFINE LANG_OGTOOLBAR_CUSTFLTRS "Customize Filters"
#DEFINE LANG_OGTOOLBAR_CLOSE "Close Option Grid"
#DEFINE LANG_OGTOOLBAR_OK "Ok"
#DEFINE LANG_OGOBJECT_YES "Yes" 
#DEFINE LANG_OGOBJECT_NO "No" 
#DEFINE LANG_OGOBJECT_IS "is"  
#DEFINE LANG_OGOBJECT_NOT "Not"
#DEFINE LANG_OGOBJECT_BETWEEN "between" 
#DEFINE LANG_OGOBJECT_INLIST "in list"
#DEFINE LANG_OGTOOLBAR_CANCEL "Cancel"
#DEFINE LANG_OGOBJECT_LIKE "Like"
#DEFINE LANG_SELECT "Select"
#DEFINE LANG_RANGE "Range"
#DEFINE LANG_PDF "PDF Format"
#DEFINE LANG_XML "XML Format"
#DEFINE LANG_TEXT "Text Format"
#DEFINE LANG_XLS "Excel Format Type XLS"
#DEFINE LANG_XLSX "Excel Format Type XLSX"
#DEFINE LANG_HTML "HTML Format"
#DEFINE LANG_LESSTHANOREQUAL "Less Or Equal"
#DEFINE LANG_GREATERTHANOREQUAL "Greater Or Equal"
#DEFINE LANG_ALL "All"
*N000682,1 MMT 11/20/2012 Globalization changes[End]
*E303593,1 MMT 07/28/2015 Give user the ability to select fields to export to excel in OG[T20150715.0014][Start]
#DEFINE LANG_SELECTFIELD 'Select Fields to Export'
*E303593,1 MMT 07/28/2015 Give user the ability to select fields to export to excel in OG[T20150715.0014][End]