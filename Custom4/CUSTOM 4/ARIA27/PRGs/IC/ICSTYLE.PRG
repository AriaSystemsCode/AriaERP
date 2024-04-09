*:************************************************************************
*: Program file  : ICSTYLE.PRG
*: Program desc. : Add,Change,Delete and Inquire Styles.
*: For screen    : ICSTYLE.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: TAK - Timour Abdalla Khalil 
*:************************************************************************
*: Passed Parameters  : Style Major    => lcpStyle
*:                      Style NonMajor => lcpColor
*:************************************************************************
*: Example :DO gpDoProg WITH 'AWRICSTYLE',.F.,'IC',"'"+m.Style+"',''"
*:************************************************************************
*: Modification       :
*E300803,1 SWK 05/25/98 Do the following changes in the style screen:-
*E300803,1             1) Change the total fields in the cut/sold folder to
*E300803,1                have the total of all sizes in case of using 
*E300803,1                extended size scale 
*E300803,1             2) Add a lead time field
*E300803,1             3) change the Inv. Jornal browse to get the size 
*E300803,1                scale heading  
*E300803,1             4) Add a new record for the totals on the browse of OTS  
*E300803,1             5) Add 2 At Qty's fields
*E300803,1             6) Add new field to indicate if style is using detail 
*E300803,1                costing or not and it will be defaulted fron the 
*E300803,1                setups at add mode
*E300803,1             7) Add new field to indicate if the style is inventory 
*E300803,1                style or not
*E300803,1             8) Add a new discount code  
*E300803,1             9) Add a new purchasing code for imported styles
*E300889,1 TAK 06/23/1998 Added to control the location editing if the
*E300889,1                system was setup to Point of Sale.
*E300954,1 SWK 08/09/1998 1- Allow using dyelots for imported styles
*E300954,1                2- Refuse to change the primary fabric if 
*E300954,1                  it used in the style cost sheet 
*E301022,1 HDM 09/23/1998 Make some styles Taxable by adding Taxable field [Y/N]
*E301023,1 HDM 09/23/1998 Change prepack fields status According to company
*E301023,1                setup.
*E301023,1                1) Use M_PREPACK ~ N|S|D
*E301023,1                     I N => No prepacks Used
*E301023,1                    II S => Same prepack for buy & sell
*E301023,1                   III D => Diffrent prepacks for buy & Sell
*E301023,1                2) CASE N => ALL prepack fields disappear from the screen
*E301023,1                   CASE S => As It Is
*E301023,1                   CASE D => User will choose from a popup [Buy or Sell]
*B602179,1 AMM 11/25/1998 Erase temporary files when quitting the program
*B602296,1 HDM 12/01/1998 ignore adding record in case style structure
*B602296,1                has scl as nonmajor
*B206323,1 HDM 12/06/1998 Modify Related Fields to 'Y' Instead of 'N'
*E301091,1 HDM 12/15/1998 Add Some shipment header fields in 'Intransit Browse'
*E301091,1                By adding a relation between poshdr and shpmnthdr files
*B602378,1 HDM 12/21/1998 In case the company is a (POS) Cut & Sold Folder Qty is 0
*B602372,1 HDM 12/21/1998 Incorrect Wo And WIP Qty in case of all colors
*B602340,1 HDM 12/12/1998 Incorrecr Plan Quantity in General Folder
*E301077,7 TAK 12/29/1998 Reduce the number of opened files at start running 
*E301077,7                and open it with object when needed.
*B602143,1 HDM 12/30/1998 Dublication in saving costing in BOM File
*B602143,1 HDM 12/30/1998 Price Discount Shouldn't Up to 100 %
*B602386,1 HDM 12/31/1998 If Company ESS The style saved with no scale information
*B602386,1                in stydye file
*B602334,1 HDM 12/31/1998 Incorrect Prepack fields behaviour
*B602498,1 HDM 02/07/1999 Add N/A To Discount code popup
*B801907,1 HDM 02/07/1999 Update Description Fields in case the colors changed
*E301134,1 HDM 02/09/1999 Allow PO/CT Browses to display dyelots
*B602392,1 HDM 02/01/1999 Redundency in plan values if Extended Size scale Used
*B602514,1 HDM 02/18/1999 Case ESS Incorrect Tot. Inv. in Sut&Sold Folder at location level
*B801907,1 HDM 02/22/1999 If long Description is not a system generated U Can Update it
*B602447,1 HDM 02/28/1999 Alias Not found if company Multi currency
*B602642,1 HDM 03/04/1999 Values dublication in Style And Bom Files Case the company uses ESS
*E300789,1 Hesham 03/07/99 remove using ccomp_id from codes file 
*B602647,1 HDM 03/08/1999 We should not restore lnTBrk(Break Size) to 1
*B602608,1 HDM 03/08/1999 Not all Fabric Fields Should be replaced
*B602631,1 HDM 03/11/1999 Error In Cut & Sold Folder :
*B602631,1                The Default WareHouse Not The Selected One
*B602667,1 HDM 03/15/1999 If Nither PO Nor MF Installed Disable Detail costing check box
*E301176,1 HDM 03/22/1999 Prevent programs from displaying notepad icon
*E301176,1                  as it's now controlled globally
*B802053,1 HDM 03/25/1999 In Case of ESS We have to make sure that scale is not in the sum
*B802053,1                operation as we ar in All Scales Mode (Cut & Sold Folder)
*B602701,1 HDM 03/30/1999 Bug Of Changing the active cursor so in-correct information appears in
*B602701,1                WO & WIP Browse
*B602714,1 HDM 03/30/1999 disable Fabric/Trim/Sty. Comp. case Det. Cst. <No> For both
*B602714,1                detail costing Yes/No
*E500252,1 HDM 03/30/1999 Control The Style Type PopUp According to Installed Modules
*B802112,1 HDM 04/01/1999 Change Scan Exp. To COLLECT Style UPCs to scan for the whole style 
*B802112,1                not laData[1] as it containes major name only
*B602753,1 HDM 04/07/1999 Stop Calling NotePad Program In lpSavScr as the global save
*B602753,1                Will Call it
*B802155,1 HDM 04/18/1999 Call Picking ticket program
*B602812,1 HDM 04/18/1999 Fixing bug of incorrect TotCost Saving in Style File
*B602742,1 HDM 04/19/1999 Fixing bug of (Variable nMCost1 not found)
*B802136,1 HDM 04/21/1999 Include Dyelot field in Allocation browse
*B802143,1 HDM 04/21/1999 Overwrite Control panel browse and browse using gfStyBrw
*B802143,1                instead of gfBrowse
*B802185,1 HDM 04/22/1999 We have to re-assign value to laData[21] Case Po OR MF Not Installed
*B602831,1 TAK 04/27/1999 Optimize the screen speed.
*B602846,1 TAK 04/29/1999 Fix bugs in case of extended size scale.
*B802177,1 TAK 04/29/1999 Fixed of not saving the color prices.
*B602872,1 TAK 05/05/1999 Fixed of not saving in BOM proporly.
*B802091,1 TAK 05/11/1999 Added style taxeble depends on setup yes or no.
*B602855,1 TAK 05/12/1999 Fixed alias not found and wrong show for some fields.
*B602901,1 TAK 05/16/1999 Fixed variable not found when you return from Fabric inquiry.
*E301228,1 TAK 05/18/1999 In inventory journal browse add a new fields to display 
*E301228,1                this fields is stkval,LineNo,Prev. stk,Prev. stk value
*E301228,1                also don't display un needed sizes in this browse.
*C200077,1 TAK 05/20/1999 Added to show On-Shelf inventory in cut & sold folder.
*B602938,1 HDM 05/26/1999 Fixed bug numeric overflow if PPrice modified to zero.
*E301209,4 TAK 05/28/1999 Change the major scale code to be one or two chracters
*E301209,4                depends on ic setup in case of extended size scale.
*B602963,1 TAK 05/28/1999 Display any cost fields in inventory journal depend
*B602963,1                on with costing preveleges access flag. 
*E301248,1 AHM 06/03/1999 Added to display a new type 'I' in journal brow as an ajustment.
*C200076,1 SSE 06/06/1999 Added defaults for 2 new User Fields that appear in custom screen. 
*E301268,1 HDM 06/21/1999 filter vendor browse by sup. type
*B603032,1 TAK 06/30/1999 Optimaize journal browse speed.
*B603032,1                Change the Opn.Orders button to be Sls.Orders.
*B802387,1 TAK 06/30/1999 Fixed the bug of not saving the price levels in the edit mode
*B603036,1 TAK 07/04/1999 Fixed empty major when return from toolbar browse.
*B602948,1 TAK 07/12/1999 Fixed empty screen for piktkt in to piktkt generated.
*B802425,1 TAK 07/13/1999 Fixed wrong order inquiry if select order in cut & sold folder.
*B802445,1 TAK 07/20/1999 Fixed syntax error when you try to browse without precing enter or tab.
*B603085,1 WAB 08/01/1999 when click the in transit butt. the date,airway bill, vessel, ref fields 
*B603085,1                come from a different shipment no.
*B802476,1 HDM 08/02/1999 if Point of Sale is installed we should allow the user 
*                         to make the style as imported
*B802473,1 HDM 08/02/1999 - Fixing bug of incorect record pointer in the WIP & WO temp. cursor
*                         - Distroy the relation between posln and Shpmthdr in the 'in transit'
*                           Button validation
*B802469,1 WAB 08/12/1999 Delete Date,price Fields from browse fileds when browse In-transit transactions 
*B802558,1 ALB 08/26/1999 Fixed Bug to Browse only the Received quantites
*B802558,1                (not canceled)also wehn inquiring for Returned quantites in 2nd Folder   
*B802556,1 ALB 08/30/1999 Fixed Bug of having invalid Prices calculated when entering markup > 99%
*B603144,1 Walid Abou El-magd (WAM) 09/08/1999
*B603144,1                -1) Enable the default location in edit mode .
*B603144,1                -2) Eleminate the WIP from the OTS calculation of the first column. 
*B802618,1 TAK 09/16/1999 Fixed overwiting of long description when add/remove colors.
*B802574,1 TAK 09/16/1999 Fixed of not updating the BOM if no detail costing and domestic style.
*B603196,1 ABD 10/18/1999 Fixed the bug In Cut & Sold enquiry in Sytle screen 
*B603196,1                Sales order Browse shows the header completion date for each 
*B603196,1                order line instead of complete date per line 1.
*B603256,1 KHM 11/01/1999 Fix the bug of not displaying the dye and
*B603256,1                adornment orders in the WIP & WO buttons.
*B603268,1 TAK 11/10/1999 Added to check on WIP at warehouse level in style deletion.
*B603284,1 MAN 11/16/1999 Remove the Type column from the SO Browse
*B802794,1 AMM 11/22/1999 Fix the bug of going to first style when the program is called 
*B802794,1 AMM            with certain style as parameter
*E500304,1 WAB 11/25/1999 THE unallocated qty depend on setting in IC Module
*E500304,1 WAB            (STQ-Alocated qty) or (Wip+stq-alocated qty) and 
*E500304,1 WAB            Call The O.T.S global function and comment all lines 
*E500304,1 WAB            of the O.T.S local function.
*B802854,1 SAM 12/30/1999 Fixed the bug of displaying a wrong non major 
*B802854,1 SAM            discription if folder 1(general) is not active
*C101705,1 ARH 01/11/2000 Added the start date to the open orders browse in 
*C101705,1 ARH            the cut & sold folder
*B603407,1 NAD 01/25/2000 Fixed the bug variable CSTYTYQE not found
*B603434,1 SSH 05/02/00 Fix the bug of saving the first color data
*B603434,1 SSH          for all color in case of select all color
*B603434,1 SSH          even if you do not change these data.
*B803136,1 BWA 22/03/2000 Fix the bug in Calculating the total Unallocated inventory field. 
*B603348,1  Abdou on 02/02/2000
*B603348,1  Abd 02/02/2000 Increase The Picture of Fileds gross Price,Average Cost
*B603348,1                 & Most of fileds to become 9999999999.99 in screen icstyle5
*B603348,1                 Folder costing Up on increasing fileds UntCost & TotCoat 
*B603258,1 ADEL 04/11/2000 Fix the bug of not enabling the user to View foreign prices in View mode.
*B603561,1 ADEL 04/11/2000 Fix the bug of adding unnecessary record for each style/color in STYPRICE 
*B603561,1                 file in case of EXTENDED SIZE SCALE.
*C200120,1 KHM 06/12/2000  Adding a custom process to open and update the
*C200120,1                IcDeptHd and IcDeptDt
*B603684,1 AME 06/14/2000  Fix the Bug of not viewing the cancelled po qty in cut/sold 
*B603684,1                 Wo  Browse .
*E301049,1 ADEL 06/19/2000 Freeze the color short description filed also in 'Color'
*E301049,1                 folder browse for the user to scroll in the rest of the browse
*E301049,1                 fields while he can see the color & its short description
*B603140,1 ADEL 06/21/2000 If the imported style uses detail costing don't change cost for any color as we
*B603140,1                 edit them in Edit mode within style screen.
*B603123,1 ADEL 06/21/2000 Fixed overwiting of long description when add/remove colors.
*B802807,1 ADEL 07/04/2000 Add 'Status' field to the returns browse to show the status of returned credit memo
*B803097,1 ADEL 07/04/2000 Assign the first color foiegn price to each new color added.
*B603670,1 ADEL 07/04/2000 Show only the Finished Good Inv. warehouses.
*B603604,1 ADEL 07/09/2000 Enable the user to zero out the gross price.
*B603711,1 ADEL 07/09/2000 Enable the user to inquire WIP-WO-INTRANSIT-RETURNS-..... if PS is installed
*B603711,1                 even if AR,PO,RM aren't installed.
*B603722,1 AMH 07/11/2000 Fix Numeric overflow message when adding a color to an existing style
*B603797,1 ADEL 08/01/2000 When deleting any style(color)s you must delete UPC recods. 
*B603797,1                 Before doing that you must check the status of this style being deleted
*B603797,1                 if it is marked for deletion "D" in EDICATGD file or it doesn't exist at
*B603797,1                 all , then delete, otherwise don't.
*B803536,1 MHM 08/01/2000 Fix of not install MF , PO  Module and not found 2 var M_LImpcost,M_LMFcost
*B603711,2 AMH 08/20/2000 Do not display the Purchase Order button in case of (PO,MF,RM) not Installed and 
*B603711,2                the user inquire WIP-WO-INTRANSIT-RETURNS-..... if PS is installed
*B603711,1                 even if AR,PO,RM aren't installed.
*B603859,1 MHM 08/29/2000  When you click on the WIP button to get the Po details,a message is displayed 
*B603859,1                 indicating that there are no PO's for the style/colour
*B603855,1 AMH 08/30/2000 Fix updating cost items when copy an existing style to new one
*B603875,1 KHM 09/04/2000 Fix the bug of not defaulting the Break Size popup 
*B603875,1                to the first size in case of adding a new style (England).
*B803629,1 MHM 08/29/2000 Fix not copy custom field in case of copy style from another
*B603899,1 ADEL 9/18/2000 Fix the bug of "File 'EDICATGH'" not found.
*B803666,1 ABD 09/27/2000 Fix the bug of "File 'EDICATGH'" does not Exist.
*B803786,1 ABD 11/07/2000 Fix Bug that when changing the discount to N/A from any 
*B803786,1 ABD            Other selection does not work. Upon saving, the prior value 
*B803786,1 ABD            That was in this field before the change gets saved.
*B603713,1 AMH 10/19/2000 Fix numeric overflow when save style with very large price
*C102045,1 ABD 11/28/2000 Made changes on the style screen ot update retail discound.
*C102077,1 HBG 01/02/2001 Check if enterd Pattern is Found in Pattern file or not in 
*C102077,1 HBG            case of SP module installed
*C200155,1 ABD 01/29/2001 Add new triger to add priceA and retail price to sizes in case 
*C200155,1 ABD            company is extended size scale for specific customer.
*B604192,1 AME 02/11/2001 Add new triger to add TOTCOST to sizes in case company is 
*B604192,1 AME            extended size scale for specific customer.
*B604057,1 AAN 12/25/2000 Fix Bug that The system is allowing users to change the size scale for a style
*B604057,1 AAN            even though there are outstanding orders.
*B604016,1 MHM 11/14/2000 Fix not copy custom field in case of copy style from another
*B604057,4 AAN 02/22/2001 Fix Bug that The message is wrong and make the message appears after the browsing.
*B604115,4 KAM 22/02/2001 KAM assign false value to llAllClrs
*E301525,1 HS  01/02/2001 When modifying a style(s) that already exist, and if the
*E301525,1                Inter-Company EDI module is installed and setup and the
*E301525,1                "System Type" system setup is set to "Back Office", mark any
*E301525,1                "Selection Codes" that contain the edited style(s) as
*E301525,1                "To Be Transmitted".
*E301586,1 ABD 03/19/2001 Add a new feature in the Style screen to import 
*E301586,1 ABD            Style that is previously exported from the PDM module. 
*B604320,1 ABD 03/25/2001 Add the 20 character from long description to the short desc.
*B604408,1 WAB 04/18/2001 fix the bug of file does not exist while inporting from PDM Module
*B804125,1 KAM 04/18/2001 fix bug if MF installed and PO not installed
*C200171,1 TMI 05/02/2001 add triger to generate upb no ehne create a new style color 
*B604432,1 KAM 05/03/2001 use 'PDMPATRN.DBF' intead of 'SPPATRN.DBF'
*B804054,1 KAM 05/06/2001 allow all sizes for styel to remove[start]
*B804289,1 WAB 05/18/2001 fix the bug of Variable lanwcondinf not found 
*B804219,1 ADEL05/28/2001 Allow adding total Plan for Cathy Danyels.
*B604573,1 WAB 06/22/2001 Change the PDMpatern fiel structru 
*B804271,1 ASH 07/11/2001 Fix the bug of 'Alias not found' in case of MA module not installed.
*C102390,1 TMI 07/29/2001 Change the default value for the dyelot flag to be checked by default
*B604862,1 ASH 09/03/2001 Fix the bug 'Subscript out of bound' if you pressed ESC from the pattern browse.
*B604864,1 HBG 09/03/2001 Check for warehouse of the order line not the header in (Cut & Sold) Folder.
*B604913,1 ASH 09/16/2001 Fix the bug of infinite loop in the pattern field.
*B604965,1 ABD 09/30/2001 Fix problem in the Planning Qty when Company use Use extended size scale
*B604965,1 ABD            The planning field calculate all planning Qty for all color when add new color,
*B604965,1 ABD            We will not update the Planning Qty in case use Use extended size scale.
*C200235,1 ABD 10/02/2001 Custom Trigger for Cathy Daniels to export Style to the old system.
*B804468,1 HBG 10/18/2001 Check for warehouse of the order line not the header in (Cut & Sold) 
*B804468,1                Folder in case of all color and specific warehouses.
*C102466,1 ABD 10/21/2001 Restrict adding "Obsolete" colors to style.
*C102452,1 HBG 11/21/2001 New Custom Style Classification Screen For GMA.   
*B605442,1 ASH 01/29/2002 Fix the bug of wrong values in the Cut&Sold folder in case of selecting all scales in Ext. Size scale. 
*E301800,1 BWA 01/21/2002 Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
*C102528,1 ASH 02/12/2002 Add an option to browse the style SKU's. (Done as standard)
*B605451,1 ASH 02/25/2002 Display a message if the style default location not found in warehouse file.
*B605469,1 HBG 03/18/2002 Fix bug of editing Special/Costing fields on style does not save change
*B604323,1 ASH 03/26/2002 Fix bug of not assigning another location to the same style.
*C200307,1 AMH 03/27/2002 Custom Assign coded notes screen for Collage.
*B605797,1 ASH 04/09/2002 Fix the bug of the WIP in the cut&sold folder is different than the Qty which appeared in the browse.
*B605866,1 ASH 04/21/2002 The program allows the user to change the style from non domestic to domestic even if it has WIP.
*C102564,1 BWA 04/24/2002 Add 3 fields to let know if the style has changed or not.
*B605958,1 ASH 06/02/2002 Fix the bug of incorrect GL link code at the location level.
*C200388,1 ADEL 08/06/02  Add auto search menu bar for Dare Bare to search object links.
*B606391,1 BWA 08/18/2002 Fix the bug of copying the right memo fields in case use user fields.
*B606345,1 TMI 09/23/2002 Get non-major part of style to correct the refresh when using the toolbar browse
*B606503,1 ADEL 09/25/02 When modifying a warehouse, don't overwrite the original record with 
*B606503,1               zero-valued values.
*B604357,1 TMI 09/30/2002 Fix the bug that Intransit button still show qty after reciving
*B606534,1 ASH 10/14/2002 Wrong open qty in the WIP browse in case of Extended size scale.
*B606580,1 ASH 10/29/2002 Fix problem of browsing all the scales in the cut&sold folder (WIP,WO,SHP....) and not the selected scale.
*B606741,1 RAE 12/09/2002 Unallocated QTY total is not calculated properly.
*C200431,1 RAE 12/16/2002 Add Custom pricing screen.
*B606719,1 ABD 01/15/2003 Round the average cost at screen style5.scx only.
*B606859,1 ASH 02/16/2003 Fix the bug of wrong calculating total inv. in case of using dyelots.
*C200519,1 TMI 03/13/2003 Add bar for GMA titled "Customer Packing Info"
*C200490,1 WAB 03/18/2003 Add A custom bar for MEMO FAshion to View Caron details 
*B606743,1 WAB 04/07/2003 WO and WIP figures in Cut and Sold using dyelots
*B607253,1 HBG 12/05/2003 Get the Group from ICDEPTHD file not from ICCLSSTP ftile 
*B606680,1 ABD 06/10/2003 Fix Bug While copying from exist Style.
*B606876,1 ABD 06/15/2003 Display Error in the costing tap of the style screen.
*C200576,1 ABD 06/23/2003 The weidth of the pattern field Should be less than or equal 3.
*B607367,1 KHM 06/29/2003 Fix bug of displaying a message when changing the long description
*B607367,1                of a specific color.
*B607286,1 ABD 06/30/2003 Fix bug that the user can remove the warehouse from style 
*B607286,1 ABD            screen although the warehouse  has a stock or orders.
*B607310,1 KHM 07/08/2003 Filter on the sales orders that have cOrdType = 'O' in orders button.
*B607006,1 KHM 07/16/2003 Fix the bug of displaying the cancelled RA
*B607394,1 ABD 07/30/2003 Fix bug that the user lost the duty and freight after saving and edit it again.
*B119967,1 ABD 10/20/2003 Fix bug that the total inventory fields at the cut and sold folder 
*B119967,1 ABD            Show incorrect fingers in case the warehouse code starts with space.
*C120205,1 TMI 11/12/2003 Send a mail to a list of users when a style/color is added.
*B120221,1 ABD 11/30/2003 Fix bug that the costing fields didn't update with a new value, 
*B120221,1 ABD            in case creating new style copied from style exist.
*B120791,1 MHM 12/30/2003 Fix bug of removing the values of the user defined fields if change Desc., 
*B120959,1 TMI 01/12/2004 Browse from current selected style in the gfStyBrw global function
*E037853,1 HBG 02/16/2004 Change the width of Key field in EDITRANS to 40 char
*B122006,1 MHM 03/30/2004 1- Fix bug of removing the values of the user defined fields in case of EX. Size
*B122006,1                2- Change alltrim to work only in case of CHR only 
*B122043,1 ASH 04/19/2004 Don't display the BID P/O in the Work order browse.
*B123029,1 NNA 06/07/2004 fix bug that if detail costing [NO] and you get the costing Elements Zero
*B123029,1 NNA            [from the style screen] the bom file doesn't Remove Record that becomes Zero
*B123215,1 MHM 07/06/2004 fix bug that changing primary fabric overwrites price A.
*B122822,1 NNA 07/15/2004 Fix bug that if you created a Style with coping from another style that has
*B122822,1 NNA            style cost sheet it will copy Cost's element and the Stock Value Too
*B123627,1 MHM 07/29/2004 Fix bug If we modify one of the user defined fields at the style level, 
*B123627,1                the system copies the user defined fields values assigned to the first 
*B123627,1                color to the rest of the colors. This happens with the user defined 
*B123627,1                fields that haven't been modified.
*C038291,1 TMI 08/05/2004 Add 'Measurments' screen for Vijay 
*C200597,1 TMI 08/19/2004 Call a customized tracekey function for Bong Hwa
*C122981,1 NNA 08/26/2004 Blank the color cost when we add a new color to a Style for Eric Javits (ERI02)
*B124482,1 MHM 09/14/2004 The system does not save the changes we made in the style color level
*B124451,1 MHM 10/28/2004 Fix Bug of Custom Classfication for GMA did not coped in case of Copy styles
*C123616,1 TMI 11/10/2004 Comment a trigger and call it from within memmain.prg / where the bar is added via ADDBAR trigger
*B038753,1 WSH 12/20/2004 Update Inventory Jounal file with correct value when cost changes
*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method standar and no adjustment reasons in codes file .
*B039133,1 NNA 03/17/2005 Fix Bug that in Some Cases if you Edit in the Style screen and changed in
*B039133,1 NNA            in the Long Desc. then Save you'll find that it didn't save
*B128198,1 NNA 06/14/2005 Fix bug that happen if you select a style/color then pressed on Fabric Button,you'll get an massage
*B128198,1 NNA            'This fabric not found' even if there is a fabric appeare in the fabric field on the screen
*B129334,1 NNA 08/29/2005 Fix bug that if you deleted a style you find that it didn't deleted from the CSTPRICE file (Custom for DAV15)
*B129334,1 NNA            so this Causes a problem when a style is deleted and then recrated with a different price scale or
*B129334,1 NNA            a colour is deleted and then added - as the Customer Pricing Screen shows the prices from the 
*B129334,1 NNA            deleted records and not the new records
*B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy 
*B130339,1 TMI 12/21/2005 Do not update the nICost1 , ... , nICost5 by the cost entered in style screen if all colors selected
*B039660,1 NNA 02/14/2006 (Begin) update the Style class fields with a default value 'A' if we're in the Add mode
*B040114,1 TMI 05/03/2006 Fix a bug that a style with stock 0 is deleted while it has transations in styinvjl other than '129'
*B607775,1 ASH 08/28/2006 Fix error 'Alias not found' while deleting a style.
*B607888,1 TMI 12/19/2006 Fix a bug that when adding a new color the tax break of the first scale is copied to all scales ( ticket # T20060816.0008 )
*B608056,1 MHM 04/22/2007 Fix a bug that deducting recived QTY from Intranset in case of interlocation PO
*B608355,1 NNA 11/17/2007 Fix bug that if the MF module is installed and PO module is not installed and detail costing is 
*B608355,1 NNA            set to yes, when you add a new style you'll find that detail costing option is setting to False
*B608765,1 TMI 12/17/2008 Remove the code of B608355 and write the correct change of the original problem ( when Detail costing is Yes , we need the check box to be checked )
*B608872,1 TMI 05/26/2009 Open the ORDLINE file when the SO module is installed
*B609027,1 TMI 10/04/2009 be sure the ORDLINE file is opened before checking that AL is installed
*:**************************************************************************************************
PARAMETERS lcpStyle,lcpColor

*-IF the program is running stand alone or to view passed style.
*B802794,1 AMM Move this part after the function gfSetup()
*llExCalled = (PARAMETERS()=2) OR TYPE('lcpStyle')='C'
*B802794,1 AMM end
EXTERNAL ARRAY laData , laDefProc
DECLARE lafoldwinds[3,2],lastats1[3,2],laMyes_no[2,2],laCyes_no[2,2],;
        laStyGrd[3,2],laDamQlt[2,2],lascale[1,2],laUPCNum[1],lawareh[1,2],;
        lalocation[1],laDefWhs[1,2],laSegInfo[1,9],laExtSz[1]

*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
STORE .T.  to llRsonCode 
*B126312,1 EIH 03/13/2005 [End]
*B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy  [Begin]
DECLARE laCostFld[5,2]
laCostFld = 0
*B128128,1 [End]

STORE '' TO lcNMjrDisc , lcNMjrRlt,lcFolGets
DECLARE laNMjr[1,2]
laNMjr[1,1] = 'CLRLNAME'
laNMjr[1,2] = 'lcNmjrRlt'
DECLARE laTaxable[2]
laTaxable[1] = 'Yes'
laTaxable[2] = 'No'
*HBG FIX BUG OF VARIABLE 'lcDyeStat' NOT FOUND IN CASE OF MULTI SESSION [Begin]
STORE ' ' TO lcDyeStat
*HBG {End]
*ASH [Begin] Fix bug of lcDefCod not found in case of open an screen and return back to the style screen and try to add a new location.
STORE ' ' TO lcDefCod
*ASH {End]
*--Variables initialization.
*-Program main variables.
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,lcWinCh4,lcWinC41,lcWinC42,lcWinCh5

STORE ' ' TO laSea,laDiv,laGrp,laRoy,laTax,laCodInfo,lafoldwinds,lastats1,layes_no,lascale,laUPCNum,lawareh,laDefWhs,lalocation
STORE ' ' TO laDisc,laPurc
STORE ' ' TO lcScFields,lcBrFields,lcfolder,lcfoldprnt,lcOldValue,laSegInfo,laExtSz,lcOtsPrd
STORE .F. TO llDyelot,llAvgCost,llPackUse,llMultiwh,llmultiloc,llLink_Gl,llStyComm,llStyMark,llBasdOnDiv,llUpc,llImpCost,llMulCurr,llCostPrv,llwarehous,llMScale
STORE ' ' TO lcCostImp,lcISlbl1,lcISlbl2,lcISlbl3,lcISlbl4,lcISlbl5,lcIType1,lcIType2,lcIType3,lcIType4,lcIType5,lcICost1,lcICost2,lcICost3,lcICost4,lcICost5,;
             lcMSlbl1,lcMSlbl2,lcMSlbl3,lcMSlbl4,lcMSlbl5,lcMType1,lcMType2,lcMType3,lcMType4,lcMType5,lcMCost1,lcMCost2,lcMCost3,lcMCost4,lcMCost5,;
             lcMajor,lcNonMjr,lcMjrTtl,lcNMjrTl,lcItemTl,lcMjrPct,lcNMjrPt,lcIMjrPt,lcCdDesc,lcSepart,lcNMKey,lcFreeDc,lcSysType
STORE .F. TO llReadCTmp   &&,laSegInfo

*B804289,1 WAB (START) intialize the variable before the gfsetup()
STORE "" TO laWarhDisc ,laNwCodinf
lnWarhDisc = 1
*B804289,1 WAB (END)
*B804219,1 (Begin)
llPlanSt = .F.
llCatRtrn = .F.
*B804219,1 (End)

*C200235,1 ABD - Define New variables when add note for cathy daniels.[Begin]
STORE '' TO lcStylOldN , lcNwStyleN
*C200235,1 ABD - [End]

*C102466,1 ABD - Restrict adding "Obsolete" colors to style, define
*C102466,1 ABD - Array to hold the Osbsolete colors. [Begin]
DECLARE laObsColor [1]
*C102466,1 ABD - [End]



*--B801907,1 HDM Flag tell lpSave to save the long description or not[start]
*B802091,1 Added initialize of default Tax and system use taxes.
STORE .F. TO llAlwsFrst,llDefTax,llSysTax
*--B801907,1 HDM Flag tell lpSave to save the long description or not[end]

*--HDM E301023,1 Variable to hold prepack status From company Setup[start]
STORE ' ' TO lcClrLName
DECLARE laClrLname[1,2]
laClrLname[1,1] = 'CLRLNAME'
laClrLname[1,2] = 'lcClrLName'

STORE 25 TO lnXPos
STORE 25 TO lnGetXPos
lcPrepack = gfGetMemVar('M_PREPACK',gcAct_Comp)
STORE .F. TO llOnGross
STORE .T. TO llDispMsg,llPreColor
*--HDM E301023,1 Variable to hold prepack status[end]

*--HDM E301022,1 [start]
*-- Variable to hold the USA country code to show Taxable Field
lcUStates  = 'USA'
*--HDM E301022,1 [end]
lcEgypt = 'EGYPT'

*--HDM E301023,1 Defin an array to hold 2 values for prepack [buy & sell]{start}

DIMENSION laPrePaTyp[2]
laPrePaTyp[1] = 'Sell'
laPrePaTyp[2] = 'Buy'

*--HDM E301023,1 Defin an array to hold 2 values for prepack [buy & sell]{end}

*E300803,1 SWK 05/25/98  Add this variable to contain the nuber 
*E300803,1               of the scale used in case of using extended size scale
STORE ' ' TO lcSqlScl
*E300803,1 (End)

*E301209,4 Initialize the scale length and picture.
lnScaleLen= 1
lcScalePic='X'
*E301209,4 End.
*B603123,1 (Begin) Add a variable to check if the long desc applies for all colors.
llAllClrs = .F.
*B603123,1 (End)
STORE 0   TO lnstylewid,lncolorwid,lnMajSeg,lnNonSeg

STORE ' ' TO lcTmpNdNo,lcColorfil,lctpstyle,lcwarefil,lctpstydye,lctmpstypr,lccursname,lctmpots
gcReadColr = gcObjColor
lnSessNo   = gnProgCopy
lcEngland  = 'ENG'

*--Control variables in this program.
llstymode   = .T.       && Style Mode (Style/warehouse level).
llDown      = .F.       && Flag to control Up/Down in Cut&Sold Folder. 
llAllColors = .F.       && Colors Mode (one /all).
llAllScales = .F.       && Scales Mode (one /all).
lcWareCode  = SPACE(6)  && Active warehouse if not style mode.
STORE 1   TO lnActFolder,lncurobj
STORE .F. TO llbrowse,glUpdated,llCopySt,llacsdefwr,llPickTkt,llCUpdate,llInquiry,llAlowNew,llNoNMjrs
STORE .T. TO llAddLine,llShowm,llFirstTime,llGetInfo

*--HDM
STORE '' TO lcTrmInvM
*--HDM

*-Key window variables.
STORE ' ' TO lcfrmstyle
*-First folder variables.
STORE ''  TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8,lcfabric,lcNmjrDisc
STORE 0   TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
STORE 0   TO lnstatus,lndomestic,lncommiss,lnGrade,lnloc,lnDamQlt
STORE 0.0 TO lnMarkupA,lnMarkupB,lnMarkupC,lnMarkupR
STORE 1   TO lnSea,lnDiv,lnGrp,lnRoy,lnTax,lnDefWhs

*C102452,1 HBG 21/11/2001 Variables of Style classification Screen from option menu [Begin]
STORE 1  TO lnDept,lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
STORE ' ' TO lcDept,lcGroup,lcCatgry,lcClass,lcScls1,lcScls2,lcScls3
DIMENSION laDept[1],laGroup[1,2],laCatgry[1,2],laClass[1,2],laSClas1[1,2],laSClas2[1,2],;
          laSClas3[1,2]
STORE ' ' TO laDept,laGroup,laCatgry,laClass,laSClas1,laSClas2,laSClas3,lcDepDesc,lcGrpDesc
llSvUsrFld = .F.
DIMENSION laGropInfo[1,10]   && Codes array for Style Group
DIMENSION laCatgInfo[1,10]   && Codes array for Category
DIMENSION laClasInfo[1,10]   && Codes array for Class
DIMENSION laCls1Info[1,10]   && Codes array for Sub Class 1
DIMENSION laCls2Info[1,10]   && Codes array for Sub Class 2
DIMENSION laCls3Info[1,10]   && Codes array for Sub Class 3
STORE SPACE(0) TO laGropInfo,laCatgInfo,laClasInfo,laCls1Info,laCls2Info,laCls3Info
*C102452,1 [End]

*E300803,1 SWK 05/25/98 Add these 2 variavles for the purchasing and discount codes
STORE 1   TO lnDisc,lnPurc
*E300803,1 (End)

STORE .F. TO lldye
STORE .F. TO llInvSty,llDetCost,llDspClrDs
STORE 8   TO lnscalecnt

*-Second folder variables.
STORE ' ' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8,lcDefWare,lcSclDesc,llMFCost
*B602831,1 Added variable for plan insted of calculate it each time (lnTPlan).
STORE 0   TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x,;
             lnCHold1,lnCHold2,lnCHold3,lnCHold4,lnCHold5,lnCHold6,lnCHold7,lnCHold8,lnCHold9,;
             lnCOpen1,lnCOpen2,lnCOpen3,lnCOpen4,lnCOpen5,lnCOpen6,lnCOpen7,lnCOpen8,lnCOpen9,;
             lnCActl1,lnCActl2,lnCActl3,lnCActl4,lnCActl5,lnCActl6,lnCActl7,lnCActl8,lnCActl9,;
             lnInTrn1,lnInTrn2,lnInTrn3,lnInTrn4,lnInTrn5,lnInTrn6,lnInTrn7,lnInTrn8,lnTInTrn,;
             lnTPlan1,lnTPlan2,lnTPlan3,lnTPlan4,lnTPlan5,lnTPlan6,lnTPlan7,lnTPlan8,lnTPlan9
*C200077,1 Define Alocated inventory for Piktkt with status 'P'.
STORE 0   TO lnOnShf1,lnOnShf2,lnOnShf3,lnOnShf4,lnOnShf5,lnOnShf6,lnOnShf7,lnOnShf8,lnOnShf9
llOnShelf = .F.             
lcPCAoptn = ' '
*C200077,1 End.

*E300803,1 SWK 05/25/98 Store zero in the total fields used in the Cut and sold folder
*B606741,1 RAE Declare lnTotUnAll to store Unallocated QTY total.[start]
*STORE 0   TO lnTotPrPak,lnTotWip,lnTotWO,lnCTTotStk,lnTotAlo,lnTotOrd,lnToTShp,lnTOTRA,lnTOTRET
STORE 0 TO lnTotPrPak , lnTotWip , lnTotWO  , lnCTTotStk , lnTotAlo , lnTotOrd ,;
           lnToTShp   , lnTOTRA  , lnTOTRET , lnTotUnAll
*B606741,1 RAE [end]

*E300803,1 (End)

STORE ' ' TO lcexkey,lcmesgx,lcwcondt,lcfcondt

lnCntlScl = 1
lcCExp1 = 'Prepack'
lcCExp2 = 'Plan'
lcCExp3 = 'W.O.'
lcCExp4 = 'W.I.P.'
lcCExp5 = '-'
lcCExp6 = 'Tot. Invt.'
lcCExp7 = 'Unal.Invt.'
lcCExp8 = 'Allo.Invt.'
lcCExp9 = 'Tot. Avail'
*B603032,1 Start.
*lcCExp10='Opn.Orders'
lcCExp10= 'Sls.Orders'
*B603032,1 End.
lcCExp11= 'O.T.S.'

*-Thered folder variables.
STORE ' ' TO lcSLinkCde,lcLinkDesc,lcRetSty
STORE 1   TO lnprexrate,lnchexrate,lnCurrUnt1,lnCurrUnt2,lnTBrk
STORE 0   TO lnTaxPr,lnTax,lnUPC

*-Forth folder variables.
STORE 0   TO lnMarker,lnLines,lncol_no,lnBarNo,lnbrrecno,lcBrowcTtl

*-Fifth folder variables.
STORE ' ' TO lcpricurr,lcchgcurr,lcfrnsmbl1,lcfrnsmbl2,lcfrnsmbl3,lcfrnsmbl4,lcfrnsmbl5,lcBaseSmbl
STORE ' ' TO lccostlbl1,lccostlbl2,lccostlbl3,lccostlbl4,lccostlbl5,lcRateTxt1,lcRateTxt2,lcCstMth
STORE 0.0 TO lncostval0,lncostval1,lncostval2,lncostval3,lncostval4,lncostval5,lnCostPrc2,lnCostPrc3,lnCostPrc4,lnCostPrc5
STORE 0.0 TO lnCostPrc0,lnFCostPr2,lnFCostPr3,lnFCostPr4,lnFCostPr5
STORE '*' TO lcPMethod,lcDMethod
STORE '/' TO lcUnPMeth,lcUnDMeth
STORE gdSysDate TO ldRateDt1,ldRateDt2
lnPuPrepak = 1

llLineCost = .F.

*--Cheking of Modules instaled flags.
*gcCmpModules="AP,GL,SR,SO,PO,MA,MF,AR,IC,AL,RM"
llApIstall = (OCCURS('AP',gcCmpModules)<>0)
*--HDM B802112,1 [Start]

*--TMI C200171 [Start] Check if EDI Module installed
llNCInstl = (OCCURS('NC',gcCmpModules)<>0)
*--TMI C200171 [End  ]

*llEDIInstl = (OCCURS('UP',gcCmpModules)<>0)
*B803666,1 ABD This flag should be true only when UPC modile is Install , that 
*B803666,1 ABD If EB module is install and UPC not install the program open files
*B803666,1 ABD That depend on Install UPC module.[begin]
*llEDIInstl = (OCCURS('UP',gcCmpModules)<>0) .OR. (OCCURS('EB',gcCmpModules)<>0)
llEDIInstl = (OCCURS('UP',gcCmpModules)<>0)
*B803666,1 ABD [End]
*--HDM B802112,1 [End]
llMFInstld = (OCCURS('MF',gcCmpModules)<>0)
llARInstld = (OCCURS('AR',gcCmpModules)<>0)
llSOInstld = (OCCURS('SO',gcCmpModules)<>0)
llALInstld = (OCCURS('AL',gcCmpModules)<>0)
llPOInstld = (OCCURS('PO',gcCmpModules)<>0)
llRMInstld = (OCCURS('RM',gcCmpModules)<>0)    
llMAInstld = (OCCURS('MA',gcCmpModules)<>0)

*C102077,1 HBG 01/02/2001 Check if SP module installed , and If it is installed
*C102077,1                open SPPATRN file[Begin]

*E301586,1 ABD - Get the install modules only not all module
*E301586,1 ABD - For the  company. [Begin]
*llSPInstld = (OCCURS('SP',gcCmpModules)<>0)
llSPInstld = (OCCURS('SP',gcComp_Mdl)<>0)
*E301586,1 ABD - [End]
*B604408,1 - WAB (Start) commit this line and mover aftre gfsetup()
*IF llSPInstld
*  llOpnPatrn =  gfOpenFile(gcDataDir+'SPPATRN','Cpaternid','SH')
*  E301586,1 ABD - Open the Pdmstyle & PDMlog Files. [Begin]
*  llOpPDMSty =  gfOpenFile(gcDataDir+'PDMSTYLE','Cstymajor','SH')
*  llOpPDMLog =  gfOpenFile(gcDataDir+'PDMLOG','PDMITEMCD','SH')
*  E301586,1 ABD - [End]
*ENDIF
llOpnPatrn = .F.
llOpPDMSty = .F.
llOpPDMLog = .F.
*B604408,1 - WAB (End)
*E301586,1 ABD - define new variables for imported style. [Begin]
*-- Variable Hold True If we import style
llImprtSty = .F.
llNotImprt = .T.
*E301586,1 ABD - [End]

*C102077,1[End]

*--HDM B602667,1[Start] initialize variable to tell if detail
*--                     costing controled by main system or by PRG
STORE .F. TO llCtrlDetC

IF llMFInstld .OR. llPOInstld
  STORE .F. TO llCtrlDetC
ELSE
  STORE .T. TO llCtrlDetC
ENDIF
*--HDM B602667,1[End] 

*C102452,1 HBG 21/11/2001 Flag to check the trigger of GMA for Style classification Screen from option menu [Begin]
llGMATrig = .F.
*C102452,1 [End]

*E301152,1 Hesham (Start)
*IF !lfSegInfo() 
*  *--Item structure not found, Cannot Proceed.
*  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
*  glQuitting = .T.
*  RETURN
*ENDIF
*--HDM [start]
*--Adding Check For Color Segment Exsisting
*--If !exists Don't display The Color Discription SayField
*FOR I=1 TO ALEN(laSegInfo,1)
*  IF laSegInfo[I,3] ='C'
*    llDspClrDs = .T.
*  ENDIF
*ENDFOR
*--HDM [end]
*E301152,1 Hesham (End)

*C200307,1 AMH Define variable to open STYCDNOT file for customer Collage [Start]
PRIVATE llOpnStyNt,lcTmpStyNt
llOpnStyNt = .F.
STORE ' ' TO lcTmpStyNt
*C200307,1 AMH [End]

*B604357,1 TMI [Start] Defind temp name for PosLn
lcTmpPosln = gfTempName()
*B604357,1 TMI [End  ] 

IF !gfSetup()
  RETURN
ENDIF

*C102452,1 HBG 21/11/2001 Trigger of GMA for Style classification Screen from option menu [Begin]
IF ASCAN(laEvntTrig, PADR('ADDMPAR',10)) <> 0
  llGMATrig = .T.
ENDIF
*C102452,1 [End]

*B604432,4 KAM 05/08/2001 declare variable to enable or disable the pattern object[start]
 STORE "ENABLE" TO lcGetPatED
*B604432,4 KAM[end]

*B604408,1 - WAB (Start) check first if the files is exist becuase in case of PDM module is installed 
*B604408,1 - WAB (Start)  the needed files for inmprtoing still not prepared by PDM team
IF llSPInstld
  *B604432,1 KAM check for existing file 'PDMPATRN.DBF' instead of 'SPPATRN.DBF' [START]
  *llOpnPatrn = FILE(gcDataDir+'SPPATRN.DBF')
  llOpnPatrn = FILE(gcDataDir+'PDMPATRN.DBF')
  *B604408,1 KAM[end]
  llOpnPdm   = FILE(gcDataDir+'PDMSTYLE.DBF')
  llOpnPdm   = FILE(gcDataDir+'PDMLOG.DBF')
  IF llOpnPatrn
    *B604432,1 KAM open 'PDMPATRN instead of 'SPPATRN' [START]
    *llOpnPatrn =  gfOpenFile(gcDataDir+'SPPATRN','Cpaternid','SH')
    llOpnPatrn =  gfOpenFile(gcDataDir+'PDMPATRN','Cpattern','SH')
    *B604432,1 KAM[end]
  ENDIF
  IF llOpnPdm
    llOpPDMSty =  gfOpenFile(gcDataDir+'PDMSTYLE','Cstymajor','SH')
    llOpPDMLog =  gfOpenFile(gcDataDir+'PDMLOG','PDMITEMCD','SH')
  ENDIF
ENDIF
*B604408,1 - WAB (End)


*E301152,1 Hesham (Start)
IF !lfSegInfo() 
  *--Item structure not found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Item structure not found')
  glQuitting = .T.
  RETURN
ENDIF
*--HDM [start]
*--Adding Check For Color Segment Exsisting
*--If !exists Don't display The Color Discription SayField
FOR I=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[I,3] ='C'
    llDspClrDs = .T.
  ENDIF
ENDFOR
*--HDM [end]
*E301152,1 Hesham (End)

*E500304,1 WAB - get llTotAvlbl setting 
*E500304,1 WAB - llTotAvlbl = .F. --> avlbel qty = stq - alocated qty
*E500304,1 WAB - llTotAvlbl = .T. --> avlbel qty = WIP + stq - alocated qty
*E500304,1 WAB - START

llTotAvlbl = gfGetMemVar('M_TOTAVLBL')  

*E500304,1 WAB - END

*--Data checking.
GO TOP IN SCALE
IF EOF('SCALE')
  *--Scale file is Empty, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Scale file is Empty')
  glQuitting = .T.
  RETURN
ENDIF
GO TOP IN CODES
*-- HDM B602297,1 12/01/1998 if there is no colors in the system cannot proceed[start]
SELEC CODES
SET ORDER TO TAG CCODE_NO

*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
IF !SEEK("N"+'CADJREASON','CODES')
  llRsonCode = .F.
ENDIF
*B126312,1 EIH 03/13/2005 [End]

*E300789,1 Hesham (Start)
*IF !SEEK(gcAct_Comp+PADR('COLOR',6))
IF llDspClrDs AND !SEEK(PADR('NCOLOR',6))
*E300789,1 Hesham (End)
  *--No colors found, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','No colors found')
  glQuitting = .T.
  RETURN
ENDIF
*-- HDM B602297,1 12/01/1998 if there is no colors in the system cannot proceed[end]
*E300789,1 Hesham (Start)
*IF EOF('CODES')
IF llDspClrDs AND EOF('CODES')
*E300789,1 Hesham (End)
  *--Codes file is Empty, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Codes file is Empty')
  glQuitting = .T.
  RETURN
ENDIF
*--HDM Adding Check If Locations file is empty[START]
GO TOP IN WAREHOUS
IF EOF('WAREHOUS')
  *--Locations file is Empty, Cannot Proceed.
  =gfModalGen('QRM42080B42001','DIALOG','Locations file is Empty')
  glQuitting = .T.
  RETURN
ELSE
  SELECT WAREHOUS
  LOCATE FOR LSTYINV
  IF !FOUND()
    =gfModalGen('QRM42080B42001','DIALOG','No finished goods Locations found')
    RETURN
  ENDIF

ENDIF

*--HDM Adding Check If Locations file is empty[END]

lcBrowcTtl = UPPER(SUBSTR(lcNMjrTl,1,1))+SUBSTR(lcNMjrTl,2)
lcbrowcttl = ALLTRIM(lcbrowcttl)

lcbrowcttl = IIF(SUBSTR(ALLTRIM(lcbrowcttl),1,1)= '-',SUBSTR(ALLTRIM(lcbrowcttl),2),lcbrowcttl)

*E300803,1 SWK 05/25/98 redimenssion the laData array
*DIME laData[72]
*DIMENSION laData[81]

*HDM Redimension laData ==> 84 for cBuyPrepk , Gros_Price , Discount [strat]
*C102452,1 HBG 21/11/2001 IF Trigger of GMA Redimension laData to add the custom fields of GMA[Begin]
*DIMENSION laData[84]
IF llGMATrig
  DIMENSION laData[90]
ELSE
  DIMENSION laData[84]
ENDIF  
*C102452,1 [End}
*HDM Redimension laData ==> 84 for cBuyPrepk[end]
*E300803,1 (End)

*--Initialize the Folders array.
lcwfoldchng = '=lfActFolder()'  && function to control shows after change the folder
lcfoldpush  = 'pbFolder'        && push button name for the next folder
lnFolderCEnd= 103.00
lnfolderrend=   2.00

*--ToolBar external proc.
laDefProc[1]  = .F.              && Disable the control panel prev proc.(lpTopScr)
laDefProc[2]  = .F.              && Disable the control panel prev proc.(lpBtmScr)
laDefProc[3]  = .F.              && Disable the control panel prev proc.(lpNxtScr)
laDefProc[4]  = .F.              && Disable the control panel prev proc.(lpPrvScr)
laDefProc[7]  = .F.              && Disable the control panel delete proc.(lpDelScr)
laDefProc[9]  = .F.              && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.              && Disable the control panel close proc. (lpClsScr)

*--Control pannel additional elements.
*E301176,1 HDM 03/22/1999 [Start] Prevent programs from displaying notepad icon
*                           as it's now controlled globally

*DECLARE laPanelObj[3,3]

DECLARE laPanelObj[2,3]
STORE '' TO laPanelObj
laPanelObj[1,1] = 'pbObjlnk'
laPanelObj[1,2] = gcBmpHome+'RELATE.BMP'
laPanelObj[1,3] = [VALID lfvObjLnk() MESSAGE 'Style Picture' DISABLE ]
laPanelObj[2,1] = 'pbPacks'
laPanelObj[2,2] = gcBmpHome+'PACKS1.BMP'
laPanelObj[2,3] = [VALID lfvPackBrw() MESSAGE 'Style Packs' DISABLE ]

*laPanelObj[3,1] = 'pbNotePad'
*laPanelObj[3,2] = gcBmpHome+'NOTES2.BMP'
*laPanelObj[3,3] = [VALID lfvNotes() MESSAGE 'Style notes' DISABLE ]

*E301176,1 HDM 03/22/1999 [End]

*--Use extended size scale.  
llMScale   = gfGetMemVar('M_USEEXSSC')

*C200155,1 ABD Add new triger to add priceA and retail price to sizes in case 
*C200155,1 ABD company is extended size scale for specific customer.
*B604192,1 AME[START] Add new triger to add TOTCOST  to sizes in case company is
*B604192,1 AME[START] extended size scale for specific customer.
*IF llMScale .AND. ASCAN(laEvntTrig , PADR('LLUPDAT_PR',10)) <> 0
IF llMScale .AND. (ASCAN(laEvntTrig , PADR('LLUPDAT_PR',10)) <> 0   .OR. ASCAN(laEvntTrig , PADR('LLUPDAT_CS',10)) <> 0 )  
*B604192,1 AME[END]
  STORE '' TO lcTmpStypc
  lcTmpStypc = gfTempName()
ENDIF  
*C200155,1 ABD [End]

*--Open needed files.
*E301525,1 HS Change this line to open EDI files [Begin]
*DIME laOpFile[23]
DIMENSION laOpFile[26]
*E301525,1 HS Change this line to open EDI files [End]
laOpFile=.F.
=lfCntFiles(.F.)


IF !WEXIST(gcBaseWind)

  *--Initialize Style browse fields.  
*  lcBrFields = [Style:19,DESC:13:H="Description",]+;
*               [Season:6:H="Season",Cdivision:6:H="Division",Pricea]+;
*               [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*               [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
*               [Fabric:9:h="Fabric"]

*--HDM lcBrFields = [Style:19,= gfRltFld(lcNonMjr , @laNMjr , 'COLOR'),lcNMjrDisc = gfCodDes(lcNonMjr,'COLOR'),DESC:13:H="Description",]+;
*               [Season:6:H="Season",Cdivision:6:H="Division",Pricea]+;
*               [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*               [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
*               [Fabric:9:h="Fabric"]



  *--Initialize Style screen fields variables.
  *--To know which laData is the field, Use the following expresion.
  *--No = (5*(row-1)) + colum
  ***********=*----1-----*----2-----*----3-----*----4-----*----5-----****
  *E300803,1 SWK 05/25/98 Change the screen fields due to the added fields cPurCode,lDetCost
*  lcScFields ='Style     ,Ave_Cost  ,Desc      ,Desc1     ,Pattern   ,'+;
*              'Scale     ,Prepak    ,Season    ,cDivision ,cStyGroup ,'+;
*              'Fabric    ,nStyWeight,Qty_Ctn   ,nFrgnPrice,nCurrRate ,'+;
*	           'Start     ,Location  ,Soldout   ,Link_code ,Status    ,'+;
*              'Make      ,Commission,cDye_flg  ,PriceA    ,PriceB    ,'+;
*              'PriceC    ,cPriceCur ,cDutyCur  ,cComCode  ,Picture   ,'+;
*	           'Plan1     ,Plan2     ,Plan3     ,Plan4     ,Plan5     ,'+;
*              'Plan6     ,Plan7     ,Plan8     ,w   ,nMCost1   ,'+;
*              'nMCost2   ,nMCost3   ,nMCost4   ,nMCost5   ,nICost1   ,'+;
*              'nICost2   ,nICost3   ,nICost4   ,nICost5   ,nPrCost2  ,'+;
*              'nPrCost3  ,nPrCost4  ,nPrCost5  ,MarkA     ,MarkB     ,'+;
*              'MarkC     ,TotCost   ,Content1  ,Content2  ,nSugRetPri,'+;
*              'cConsInfo1,cConsInfo2,cTaxCode  ,nTaxBreak ,cVenSty   ,'+;
*              'Vendor    ,Royalty   ,cSlsGLLink,cAdd_User ,cRetSty   ,'+;
*              'cDefWare  ,cStyGrade ,cRetSty2  ,LeadTime  ,lInvSty   ,'+;
*              'nAtQtyB   ,nAtQtyC   ,cDiscCode ,cPurCode  ,lDetCost  ,'+;
*              'nMarkRet' 
  *E300803,1 (End)
*--HDM E301022,1 Change the screen fields due to the added field lTaxable 
*                It will take the laData[30] instead of Picture field

  *--Initialize Style screen fields variables.
  *--To know which laData is the field, Use the following expresion.
  *--No = (5*(row-1)) + colum
  ***********=*----1-----*----2-----*----3-----*----4-----*----5-----****

  lcScFields ='Style     ,Ave_Cost  ,Desc      ,Desc1     ,Pattern   ,'+;
              'Scale     ,Prepak    ,Season    ,cDivision ,cStyGroup ,'+;
              'Fabric    ,nStyWeight,Qty_Ctn   ,nFrgnPrice,nCurrRate ,'+;
              'Start     ,Location  ,Soldout   ,Link_code ,Status    ,'+;
              'Make      ,Commission,cDye_flg  ,PriceA    ,PriceB    ,'+;
              'PriceC    ,cPriceCur ,cDutyCur  ,cComCode  ,lTaxable  ,'+;
              'Plan1     ,Plan2     ,Plan3     ,Plan4     ,Plan5     ,'+;
              'Plan6     ,Plan7     ,Plan8     ,TotPlan   ,nMCost1   ,'+;
              'nMCost2   ,nMCost3   ,nMCost4   ,nMCost5   ,nICost1   ,'+;
              'nICost2   ,nICost3   ,nICost4   ,nICost5   ,nPrCost2  ,'+;
              'nPrCost3  ,nPrCost4  ,nPrCost5  ,MarkA     ,MarkB     ,'+;
              'MarkC     ,TotCost   ,Content1  ,Content2  ,nSugRetPri,'+;
              'cConsInfo1,cConsInfo2,cTaxCode  ,nTaxBreak ,cVenSty   ,'+;
              'Vendor    ,Royalty   ,cSlsGLLink,cAdd_User ,cRetSty   ,'+;
              'cDefWare  ,cStyGrade ,cRetSty2  ,LeadTime  ,lInvSty   ,'+;
              'nAtQtyB   ,nAtQtyC   ,cDiscCode ,cPurCode  ,lDetCost  ,'+;
              'nMarkRet  ,cBuyPrepk ,Gros_Price,Disc_Pcnt '
  
  *hbg
  IF llGMATrig
    lcScFields = lcScFields + ',dept      ,cstycatg  ,cstyclass ,cstysbclas,cstysbcls2,cstysbcls3'
  ENDIF
  *hbg              
    
  SELECT STYLE
  SET ORDER TO TAG STYLE
  SCATTER FIELDS &lcScFields MEMO TO laData BLANK
 
  *--Company setup parameters.
  
  llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
  lcCstMth   = gfGetMemVar('M_Cost_Met')
  llAvgCost  = (lcCstMth <> 'S')
  llPackUse  = gfGetMemVar('M_pack')     ='Y'
  llMultiwh  = gfGetMemVar('M_WareHouse')='Y'
  llmultiloc = gfGetMemVar('M_WareLoc')  ='Y'
  llLink_Gl  = gfGetMemVar('M_Link_GL')  ='Y'
  llStyComm  = gfGetMemVar('M_sty_com')  ='Y'
  llStyMark  = gfGetMemVar('M_stymark')  ='T'
  llBasdOnDiv= gfGetMemVar('M_div_link') ='Y'
  llUpc      = gfGetMemVar('M_UPC_USE')  ='Y'
  *--HDM
  lcTrmInvM  = gfGetMemVar('M_TINVT')
  *-- B803536,1 MHM 08/01/2000  [End]
  * llImpCost  = gfGetMemVar('M_LImpCost')
  *B804125,1 KAM 04/18/2001 we separate checking for MF installed from PO installed [start]
  *IF ('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl)
  *  llImpCost  = gfGetMemVar('M_LImpCost')
  *  *E300803,1 SWK 05/25/98 Get the setup of the manifacturing cost
  *  llMFCost   = gfGetMemVar('M_LMFCost')
  *  *E300803,1 (End)
  *ELSE
  *  llImpCost = .F.
  *  llMFCost  = .F.
  *ENDIF
  *--assign .F. value to llImpCost,llMFCost
  llImpCost = .F.
  llMFCost  = .F.
  IF 'MF' $ gcComp_Mdl
    llMFCost   = gfGetMemVar('M_LMFCost') 
  ENDIF
  IF 'PO' $ gcComp_Mdl
    llImpCost  = gfGetMemVar('M_LImpCost')
  ENDIF
  *B804125,1 KAM [end]

  lcCostImp  = gfGetMemVar('M_cCostImp')
  lcISlbl1   = gfGetMemVar('M_cISLbl1')
  lcISlbl2   = gfGetMemVar('M_cISLbl2')
  lcISlbl3   = gfGetMemVar('M_cISLbl3')
  lcISlbl4   = gfGetMemVar('M_cISLbl4')
  lcISlbl5   = gfGetMemVar('M_cISLbl5')
  lcIType1   = gfGetMemVar('M_cIType1')
  lcIType2   = gfGetMemVar('M_cIType2')
  lcIType3   = gfGetMemVar('M_cIType3')
  lcIType4   = gfGetMemVar('M_cIType4')
  lcIType5   = gfGetMemVar('M_cIType5')      
  lcMSlbl1   = gfGetMemVar('M_cMSLbl1')
  lcMSlbl2   = gfGetMemVar('M_cMSLbl2')
  lcMSlbl3   = gfGetMemVar('M_cMSLbl3')
  lcMSlbl4   = gfGetMemVar('M_cMSLbl4')
  lcMSlbl5   = gfGetMemVar('M_cMSLbl5')
  lcMType1   = gfGetMemVar('M_cMType1')
  lcMType2   = gfGetMemVar('M_cMType2')
  lcMType3   = gfGetMemVar('M_cMType3')
  lcMType4   = gfGetMemVar('M_cMType4')
  lcMType5   = gfGetMemVar('M_cMType5')      
  lcOtsPrd   = gfGetMemVar('M_OTSPRIOD')

  llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
  *C102390,1 TMI [Start] Get the user access to the "dyelot" checkbox.
  llDyeltPrv  = gfUserPriv('IC','ICSTYLE','DYELOT')
  *--lcDyeStat --> hold the status of llDye variable for current user
  lcDyeStat = IIF(llDyeltPrv,'ENABLE','DISABLE')  
  *C102390,1 TMI [End  ] 
  
  llMulCurr  = gfGetMemVar('llMulCurr')
  llUpc = ( llUpc AND llEDIInstl )
  *E300889,1 Initialize variable holds the system type setup.
  lcSysType = gfGetMemVar('M_SYSTYPE')
  lcSysType = IIF(EMPTY(lcSysType),'M',lcSysType)
  llstymode = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).

  *E301209,4 Read the scale length and picture.
  lnScaleLen = gfGetMemVar('M_EXTWIDTH')     && Extended size Scale ID Length.
  lcScalePic = REPLICATE('X',lnScaleLen)     && Extended size scale ID picture.
  *E301209,4 End.

  *B802091,1 Read the Taxeble default setup.
  llDefTax  = (gfGetMemVar('CDEFSTYTAX')='Y')
  llSysTax  = (gfGetMemVar('M_TAX')='Y')
  *B802091,1 End.
 
  *C200307,1 AMH Open STYCDNOT file for customer Collage [Start]
  IF ASCAN(laEvntTrig , PADR('SAVSTYNT',10)) <> 0
    IF !USED('STYCDNOT')
      llOpnStyNt = gfOpenFile(gcDataDir+'STYCDNOT',gcDataDir+'STYCDNOT','SH')
      lcTmpStyNt = gfTempName()
    ENDIF
  ENDIF
  *C200307,1 AMH [End]

  *-Initialize the status array.
  laStats1[1,1]    = 'Active'
  laStats1[1,2]    = 'A'
  laStats1[2,1]    = 'On Hold'
  laStats1[2,2]    = 'H'
  laStats1[3,1]    = 'Cancelled'
  laStats1[3,2]    = 'X'
  *-Initialize the upcarray.
  laUPCNum[1]       ='N/A'
  *-Initialize the style domestic array.
  laMyes_no[1,1]  = 'Yes'
  laMyes_no[1,2]  = .T.
  laMyes_no[2,1]  = 'No'
  laMyes_no[2,2]  = .F.
  *-Initialize the style commession array.
  laCyes_no[1,1]  = 'Yes'
  laCyes_no[1,2]  = .T.
  laCyes_no[2,1]  = 'No'
  laCyes_no[2,2]  = .F.
  *-Initialize the style grade array.
  laStyGrd[1,1]   = 'First Quality'
  laStyGrd[1,2]   = '1'
  laStyGrd[2,1]   = 'Second Quality'
  laStyGrd[2,2]   = '2'
  laStyGrd[3,1]   = 'Damaged'
  laStyGrd[3,2]   = '3'
  laDamQlt[1,1]   = 'Second Quality'
  laDamQlt[1,2]   = '2'
  laDamQlt[2,1]   = 'Damaged'
  laDamQlt[2,2]   = '3'
  *-Initialize the Scale array.
  lascale[1,1] = 'N/A'
  lascale[1,2] = 1

  *--Program screen windows.
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  lcWinCh4    = gfTempName()
  lcWinC41    = 'C' + gfTempName()
  lcWinC42    = gfTempName()
  lcWinCh5    = gfTempName()
  lcfolder    = gfTempName()        && Folder Window Name
  lcfoldprnt  = gcBaseWind          && window parent name for the folder
  lnActFolder = 1                   && active folder
  *--No nun majors found in structure.
  llNoNMjrs   = ( lnColorWid=0 OR (llMScale AND lnNonSeg=1 ) )
  
  lafoldwinds[1,1] = 'General'
  lafoldwinds[1,2] = lcWinCh1
  lafoldwinds[2,1] = 'Cut & Sold'
  lafoldwinds[2,2] = lcWinCh2
  lafoldwinds[3,1] = 'Special'
  lafoldwinds[3,2] = lcWinCh3

  IF llNoNMjrs
    IF llCostPrv
      DECLARE lafoldwinds[4,2]
      lafoldwinds[4,1] = 'Costing'
      lafoldwinds[4,2] = lcWinCh5
    ENDIF
  ELSE
 
    DECLARE lafoldwinds[4,2]
    lafoldwinds[4,1] = lcNMjrTl
    lafoldwinds[4,2] = lcWinCh4
    IF llCostPrv
      DECLARE lafoldwinds[5,2]
      lafoldwinds[5,1] = 'Costing'
      lafoldwinds[5,2] = lcWinCh5
    ENDIF
  ENDIF

  IF llmultiloc
    *-Initialize the array.
    DECLARE lalocation[1]
    lalocation[1] = 'N/A'
  ENDIF
  llwarehous=llmultiwh
  IF llmultiwh
    *B603670,1 (Begin) Show only the Finished Good Inv. warehouses.
    *lcSitCond = IIF(lcSysType = 'P',"cSiteId = gcCurSite",".T.")
    lcSitCond = "lStyInv AND "+IIF(lcSysType = 'P',"cSiteId = gcCurSite",".T.")
    *B603670,1 (End)
    SELECT WAREHOUS
    I=1
    SCAN FOR &lcSitCond
      DECLARE laDefWhs[I,2]
      laDefWhs[I,1]=cWareCode
      laDefWhs[I,2]=I
      I=I+1 
    ENDSCAN
    lnDefWhs=0

    *-Initialize the array.
    *E300889,1 Select def. warehous array depends on system type setup.
    DECLARE lawareh[1,2]
    lawareh[1,1]  = IIF(lcSysType = 'P',laDefWhs[1,1],'All   ')
    lawareh[1,2]  = 1
    lcwarecode    = lawareh[1,1]
  ENDIF
  *** Initialize the "Codes Information" array.
  *E300803,1 SWK 05/25/98 redimenssion  the laCodInfo array
  *DIMENSION laCodInfo [4,10]
  DIMENSION laCodInfo [6,10]
  *E300803,1 (End)
  laCodInfo = SPACE(0)

  *** Fill the "Codes Information" array.
  *-Season array.
  laCodInfo[1,01] = "SEASON"
  laCodInfo[1,02] = "laSea"
  laCodInfo[1,03] = "lnSea"
  laCodInfo[1,04] = ""
  laCodInfo[1,05] = .F.            
  laCodInfo[1,06] = .F.            
  laCodInfo[1,07] = "STYLE"     
  laCodInfo[1,08] = "STYLE"     
  laCodInfo[1,09] = "lcMajor+lcNMKey"     
  laCodInfo[1,10] = "Season"     
  *-Division array.
  laCodInfo[2,01] = "CDIVISION"
  laCodInfo[2,02] = "laDiv"
  laCodInfo[2,03] = "lnDiv"
  laCodInfo[2,04] = ""
  laCodInfo[2,05] = .F.            
  laCodInfo[2,06] = .F.            
  laCodInfo[2,07] = "STYLE"     
  laCodInfo[2,08] = "STYLE"     
  laCodInfo[2,09] = "lcMajor+lcNMKey"
  laCodInfo[2,10] = "cDivision"     
  *-Style Group array.
  laCodInfo[3,01] = "CSTYGROUP"
  laCodInfo[3,02] = "laGrp"
  laCodInfo[3,03] = "lnGrp"
  laCodInfo[3,04] = ""
  laCodInfo[3,05] = .F.            
  laCodInfo[3,06] = .F.            
  laCodInfo[3,07] = "STYLE"     
  laCodInfo[3,08] = "STYLE"     
  laCodInfo[3,09] = "lcMajor+lcNMKey"
  laCodInfo[3,10] = "cStyGroup"     
  *-Royalty array.
  laCodInfo[4,01] = "ROYALTY"
  laCodInfo[4,02] = "laRoy"
  laCodInfo[4,03] = "lnRoy"
  laCodInfo[4,04] = ""
  laCodInfo[4,05] = .F.            
  laCodInfo[4,06] = .T.            
  laCodInfo[4,07] = "STYLE"     
  laCodInfo[4,08] = "STYLE"     
  laCodInfo[4,09] = "lcMajor+lcNMKey"
  laCodInfo[4,10] = "Royalty"     
  
  *E300803,1 SWK 05/25/98 Fill the added element of the array
  *--Disc array
  laCodInfo[5,01] = "CDISCCODE"
  laCodInfo[5,02] = "laDisc"
  laCodInfo[5,03] = "lnDisc"
  laCodInfo[5,04] = ""
  laCodInfo[5,05] = .F.
  *--HDM B602498,1 Add N/A To Discount code popup[start]
  laCodInfo[5,06] = .F.
  laCodInfo[5,06] = .T.
  *--HDM B602498,1 Add N/A To Discount code popup[start]
  laCodInfo[5,07] = "STYLE"
  laCodInfo[5,08] = "STYLE"
  laCodInfo[5,09] = "lcMajor+lcNMKey"
  laCodInfo[5,10] = "CDISCCODE"
  *--Purchase array
  laCodInfo[6,01] = "CPURCODE"
  laCodInfo[6,02] = "laPurc"
  laCodInfo[6,03] = "lnPurc"
  laCodInfo[6,04] = ""
  laCodInfo[6,05] = .F.            
  laCodInfo[6,06] = .F.            
  laCodInfo[6,07] = "STYLE"     
  laCodInfo[6,08] = "STYLE"     
  laCodInfo[6,09] = "lcMajor+lcNMKey"     
  laCodInfo[6,10] = "CPURCODE"     
  *E300803,1 (End)
  
  IF (ALLTRIM(gcContCode) = lcEngland)
    *E300803,1 SWK 05/25/98 Redimenssion the laCodInfo in case of England
    *DIMENSION laCodInfo [5,10]
    **-Tax code array.
    *laCodInfo[5,01] = "CTAXCODE"
    *laCodInfo[5,02] = "laTax"
    *laCodInfo[5,03] = "lnTax"
    *laCodInfo[5,04] = ""
    *laCodInfo[5,05] = .F.            
    *laCodInfo[5,06] = .F.            
    *laCodInfo[5,07] = "STYLE"     
    *laCodInfo[5,08] = "STYLE"     
    *laCodInfo[5,09] = "lcMajor+lcNMKey"
    *laCodInfo[5,10] = "cTaxCode"     
    DIMENSION laCodInfo [7,10]
    **-Tax code array.
    laCodInfo[7,01] = "CTAXCODE"
    laCodInfo[7,02] = "laTax"
    laCodInfo[7,03] = "lnTax"
    laCodInfo[7,04] = ""
    laCodInfo[7,05] = .F.            
    laCodInfo[7,06] = .F.            
    laCodInfo[7,07] = "STYLE"     
    laCodInfo[7,08] = "STYLE"     
    laCodInfo[7,09] = "lcMajor+lcNMKey"
    laCodInfo[7,10] = "cTaxCode"     
    *E300803,1 SWK (End)
    DIMENSION laTax[1]
    laTax[1] = ""
  ENDIF
  
  *** Initialize Codes arrays.
  DIMENSION  laSea[1],laDiv[1],laGrp[1],laRoy[1]
  DIMENSION  laDisc[1],laPurc[1]
  STORE "" TO laSea,laDiv,laGrp,laRoy
  STORE "" TO laDisc,laPurc

  = gfwCodePop ( @laCodInfo, "SEASON"   ,"N" )
  = gfwCodePop ( @laCodInfo, "CDIVISION" ,"N" )
  = gfwCodePop ( @laCodInfo, "CSTYGROUP","N" )
  = gfwCodePop ( @laCodInfo, "ROYALTY"  ,"N" )
  = gfwCodePop ( @laCodInfo, "CDISCCODE"  ,"N" )
  = gfwCodePop ( @laCodInfo, "CPURCODE"  ,"N" )  
  IF (ALLTRIM(gcContCode) = lcEngland)
    = gfwCodePop ( @laCodInfo, "CTAXCODE","N" )  
  ENDIF
  
    
  *C102045,1 ABD Add new array to hold the DiscCode for every warehouse. 
  *C102045,1 ABD And use it in the Popup. [Begin]
  DIMENSION  laWarhDisc[1]
  STORE "" TO laWarhDisc ,laNwCodinf
  lnWarhDisc = 1
  *-- Dimenssion  the laNwCodinf array.
  DIMENSION  laNwCodinf[1,10] 
  STORE SPACE(0) TO laNwCodinf
  laNwCodinf[1,01] = "CDISCCODE"
  laNwCodinf[1,02] = "laWarhDisc"
  laNwCodinf[1,03] = "lnWarhDisc"
  laNwCodinf[1,04] = ""
  laNwCodinf[1,05] = .F.
  laNwCodinf[1,06] = .F.
  laNwCodinf[1,06] = .T.
  laNwCodinf[1,07] = lcwarefil
  laNwCodinf[1,08] = "cwarecode"
  laNwCodinf[1,09] = "lcMajor+lcNMKey"
  laNwCodinf[1,10] = "CDISCCODE"
  = gfwCodePop ( @laNwCodinf, "CDISCCODE"  ,"N" )
  lcDefCod = ''  && variable hold the default discCode for the selected style.
  *C102045,1 ABD [End]

  IF llMulCurr
    *--Initialize Price and duty currencies and forign cost elements.
    STORE gcbasecurr TO lcpricurr,lcchgcurr
    STORE 0  TO lnfrncost1,lnfrncost2,lnfrncost3,lnfrncost4,lnfrncost5
    STORE 1  TO lnprexrate, lnchexrate,lnCurrUnt1,lnCurrUnt2
    STORE SET('CURRENCY',1) TO lcfrnsmbl1,lcfrnsmbl2,lcfrnsmbl3,lcfrnsmbl4,lcfrnsmbl5,lcBaseSmbl
  ENDIF

  *--Temp files initialization.
  lccolorfil = gftempname()     &&Temp file for style non majors (colors).
  lcwarefil  = gftempname()     &&Temp file for style warehouses.
  lctmpstypr = gftempname()     &&Temp file for style foriegn prices.
  *--Temp files name for Cut & Sold folder.
  lctpstyle  = gftempname()
  lctpstydye = gftempname()
  lccursname = gftempname()
  lctmpOTS   = gftempname()

  SELECT StyDye
  =AFIELDS(lafilestru)
  lnfilestru = ALEN(lafilestru,1)
  DIMENSION lafilestru[lnFileStru+1,4]
  lafilestru[lnFileStru+1,1] = 'CSTATUS'
  lafilestru[lnFileStru+1,2] = 'C'
  lafilestru[lnFileStru+1,3] = 1
  lafilestru[lnFileStru+1,4] = 0
  CREATE DBF (gcWorkDir+lcwarefil) FROM ARRAY lafilestru
  SELECT (lcwarefil)
  INDEX ON cwarecode TAG cwarecode Unique
  SET ORDER TO       TAG cwarecode

  SELECT STYPRICE
  =AFIELDS(lafilestru)
  lnfilestru = ALEN(lafilestru,1)
  DIMENSION lafilestru[lnFileStru+1,4]
  lafilestru[lnFileStru+1,1] = 'CSTATUS'
  lafilestru[lnFileStru+1,2] = 'C'
  lafilestru[lnFileStru+1,3] = 1
  lafilestru[lnFileStru+1,4] = 0
  CREATE DBF (gcWorkDir+lctmpstypr) FROM ARRAY lafilestru
  SELECT (lcTmpStyPr)
  INDEX ON cCurrCode TAG lctmppr1 Unique
  INDEX ON Style+cCurrCode TAG (lctmpstypr) Unique ADDI
  SET ORDER TO TAG (lctmpstypr)

  SELECT STYLE
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cstatus'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 1
  laFileStru[lnFileStru+1,4] = 0
  CREATE TABLE (gcWorkDir+lcColorfil) FROM ARRAY laFileStru
  SELECT (lcColorfil)
  INDEX ON Style TAG colororder
  SET ORDER TO TAG colororder
  SELECT (lcBaseFile)
  
  *C200120,1 KHM 06/12/2000 (Begin) Adding the following custom process
  *C200120,1                in order to open the IcDeptHd and IcDeptDt 
  *C200120,1                files in order to assing styles to them.
  IF ASCAN(laEvntTrig , PADR('OPNDEPFL',10)) <> 0
    =gfDoTriger('ICSTYLE',PADR('OPNDEPFL',10))
  ENDIF
  *C200120,1 KHM 06/12/2000 (End)
 
ELSE
  *B602901,1 Start.
  *IF !USED(lctpstyle) OR EOF(lctpstyle)
  *  SELECT STYLE
  *  SET ORDER TO TAG STYLE  
  *ENDIF  
  IF USED(lctpstyle) AND !EOF(lctpstyle)
    SELECT (lctpstyle)
  ELSE
    SELECT STYLE
    SET ORDER TO TAG STYLE  
  ENDIF  
  *B602901,1 End. 
ENDIF


PUSH KEY
*--Activate Options pad.
=lfActPad()

*B802794,1 AMM Moved from above to be after gfSetup()
llExCalled = (PARAMETERS()=2) OR TYPE('lcpStyle')='C'
*B802794,1 AMM end
*--Called from external program.
IF llExCalled
  llNoShow = .F.
  SELECT STYLE
  lcStySeek=lcpStyle+IIF(EMPTY(lcpColor),'',lcSepart+lcpColor)
  =SEEK(lcStySeek)
  lcMajor = PADR(STYLE.CSTYMAJOR,lnstylewid)
  STORE .F. TO laScrMode
  STORE .T. TO laScrMode[2]
  SCATTER FIELDS &lcScFields TO laData
  *B802794,1 AMM Clear keyboard buffer
  CLEAR TYPEAHEAD
  *B802794,1 AMM end
ENDIF

*--Call screen.
DO (gcScrDir+gcWinAppl+"\ICSTYLE.SPX")
ON KEY
POP KEY
*--Colse internaly opened files.
=lfCntFiles(.T.)

RELEASE PAD _Option OF _MSYSMENU

*--Non major exists in structure.
IF ! llNoNMjrs
  RELEASE WINDOW (lcBrowcTtl)
ENDIF

*--Normal exit.
IF glQuitting
  SELECT (lcColorfil)
  USE
  ERASE (gcWorkDir+lcColorfil+'.DBF')
  ERASE (gcWorkDir+lcColorfil+'.CDX')
  SELECT (lcTmpStyPr)
  USE
  ERASE (gcWorkDir+lcTmpStyPr+'.DBF')
  ERASE (gcWorkDir+lcTmpStyPr+'.CDX')
  SELECT (lcWarefil)
  USE
  ERASE (gcWorkDir+lcWarefil+'.DBF')
  ERASE (gcWorkDir+lcWarefil+'.CDX')
  *B602179,1 AMM Erase temporary files when quitting the program
  IF USED(lctpstyle)
    USE IN (lctpstyle)
    ERASE (gcWorkDir+lctpstyle+'.DBF')
    ERASE (gcWorkDir+lctpstyle+'.CDX')
  ENDIF
  IF USED(lctpstydye)
    USE IN (lctpstydye)
    ERASE (gcWorkDir+lctpstydye+'.DBF')
    ERASE (gcWorkDir+lctpstydye+'.CDX')
  ENDIF
  IF USED(lctmpOTS)
    USE IN (lctmpOTS)
  ENDIF
  IF USED(lccursname)
    USE IN (lccursname)
  ENDIF
  *B602179,1 AMM end  

  *C102077,1 HBG 01/02/2001 Close SPPATRN file[Begin]
  IF llSPInstld AND llOpnPatrn 
    *B604432,1 KAM put 'PDMPATRN' instead of 'SPPATRN'[start]
    *=gfCloseFile('SPPATRN')
    =gfCloseFile('PDMPATRN')
    *B604432,1 KAM[end]
  ENDIF
  *C102077,1 [End]

  *E301586,1 ABD - Close the 2 files that was open before in Case Sp
  *E301586,1 ABD - Module is install. [Begin]
  *IF llSPInstld .AND. llOpPDMSty
  IF USED('PDMSTYLE')
    =gfCloseFile('PDMSTYLE')
  ENDIF
  
  IF USED('PDMLOG')
    =gfCloseFile('PDMLOG')
  ENDIF
  *E301586,1 ABD - [End]
  
  *C200307,1 AMH Close STYCDNOT file for customer Collage [Start]
  IF ASCAN(laEvntTrig , PADR('SAVSTYNT',10)) <> 0
    IF llOpnStyNt
      USE IN STYCDNOT
    ENDIF
  ENDIF  
  *C200307,1 AMH [End]
  *C038291,3  TMI [Start] Close a temp files when close the style screen "for Vijay"
  IF ASCAN(laEvntTrig , PADR('OPINSTPD',10)) <> 0
    =gfDoTriger('POSTY',PADR('CLOSETMP',10))
  ENDIF     
  *C038291,3  TMI [End  ] 
  
ENDIF
RETURN
*-End[Style]...

*:*************************************************************

*##G#-#-# General functions.


*!*************************************************************
*! Name      : lpShow.
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Show Screen objects when Mode Changes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfClearInfo(),lfChngFolder(),
*!                          lfgetinfo(),lfvNavigate()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lpShow()
*!*************************************************************
FUNCTION lpShow
PRIVATE lnPrevAl , lcPrevOrd


DO CASE
  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]  && Select mode. 
    
    *C102452,1 HBG 21/11/2001 Save user defined fields in style file after finishing global save[Begin]
    IF llGMATrig 
      =lfSavUserF()
    ENDIF  
    *C102452,1 [End]
    
    *B604432,4 KAM assign value to variable[start]
    lcGetPatED="ENABLE"
    *B604432,4 KAM [end]

    *C200235,1 ABD - Update the note pad after ading new style [Begin]
    IF ASCAN(laEvntTrig , PADR('UPDNOTE',10)) <> 0 .AND. !EMPTY (lcMajor)
      lcNwStyleN  = 'F'+lcMajor
      lcStylOldN  = 'F'+STRTRAN(SUBSTR(lcMajor,1,12),'-')
      = gfDoTriger('ICSTYLE',PADR('UPDNOTE',10))
    ENDIF      
    *C200235,1 ABD - [End]

  
    lnAlias=SELECT()
    SELECT STYLE
    
    SCATTER FIELDS &lcScFields MEMO TO laData BLANK
    lcMajor = SPACE(lnStyleWid)
    STORE ' ' TO lcNonMjr,lcNMKey
    =lfClearInfo(.T.) 
    SHOW GET ibStyle   ENABLE
    SHOW GET ibColor   ENABLE
    *B604432,4 KAM use the variable  content ineated of explicitly word[start]
    *SHOW GET ibPattern ENABLE
    SHOW GET ibPattern &lcGetPatED
    *B604432,4 KAM[end]
  	lnlastfold = lnactfolder
    lnActFolder= 1
    = lfChngFolder(lnActFolder)
    = gfwCodePop ( @laCodInfo, "SEASON"    ,"N" )
    = gfwCodePop ( @laCodInfo, "CDIVISION" ,"N" )
    = gfwCodePop ( @laCodInfo, "CSTYGROUP" ,"N" )
    = gfwCodePop ( @laCodInfo, "ROYALTY"   ,"N" )  
    = gfwCodePop ( @laCodInfo, "CDISCCODE" ,"N" )  
    = gfwCodePop ( @laCodInfo, "CPURCODE"  ,"N" )  

    IF (ALLTRIM(gcContCode) = lcEngland)
      = gfwCodePop ( @laCodInfo, "CTAXCODE","N" )  
    ENDIF
    SHOW GET pbPacks DISABLE
    *SHOW GET pbNotePad DISABLE
    SHOW GET pbObjlnk DISABLE
    *-HDM
    IF lcPrepack='D'
      SHOW GET laData[7]
    ENDIF
    lnCostVal0 = 0.0
    lnCostPrc0 = 0.0
    
    SHOW GET lnCostVal0
    SHOW GET lnCostPrc0
    
    *--HDM
    SELECT(lnAlias)
    SHOW GET ibFolder[2] DISABLE
    SHOW GET ibFolder[3] DISABLE
    SHOW GET ibFolder[4] DISABLE
    SHOW GET ibFolder[5] DISABLE


  ***--- V I E W   M O D E ---***
  CASE laScrMode[2]  && View mode.
    
    *C102452,1 HBG 21/11/2001 Save user defineg fields in style file after finishing global save[Begin]
    IF llGMATrig 
      =lfSavUserF()
    ENDIF  
    *C102452,1 [End]
    
    IF !USED(lctpstyle) OR EOF(lctpstyle) OR ;
          SUBSTR(STYLE.Style,1,lnStyleWid) <> lcMajor
         
       lcMajor =SUBSTR(STYLE.Style,1,lnStyleWid)
       *--HDM B602700,1 03/25/1999 [Start] Don't Empty the NonMajor Segment
       *--                         as According to opening file enhancement
       *--                         there is no temp files created in such mode
       * lcNonMjr=''
       *--HDM B602700,1 03/25/1999 [End]
       llstymode = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
       llGetInfo = .T.
       SELECT STYLE
       SET ORDER TO TAG STYLE  

    ELSE
       SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))    
    ENDIF
    IF llGetInfo
      =lfClearInfo(.F.) 
      *E301152,1 Hesham (Start)
      *=lfgetinfo()
      lcFolGets = ''
      *E301152,1 Hesham (End)
    ENDIF
    *B603258,1 (Begin) Get the style forien prices.
    *--Create style prices temp.
    IF llMulCurr
      lcNMKey  = lcSepart+IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)
      *B604203,4 KAM 27/03/2001[start]
      *SELECT (lctmpstypr)
      *GO TOP
      *IF EOF() OR (Style<>lcMajor+lcNMKey)
      *  SELECT STYPRICE
      *  =SEEK(lcMajor+lcNMKey)
      *  SCAN WHILE Style=lcMajor+lcNMKey
      *    SCATTER MEMVAR
      *    SELECT (lctmpstypr)
      *    APPEND BLANK
      *    GATHER MEMVAR 
      *    REPLACE cStatus WITH 'S'
      *  ENDSCAN
       
      *--seek first occurance for style and asssign style to lcFieldSty
      *--scan in styprice price for that style
      SELECT (lctmpstypr)
      GO TOP
      IF EOF() OR (Style<>lcMajor+lcNMKey)
        SELECT STYPRICE
        =SEEK(lcMajor+lcNMKey)
        lcFieldSty=STYLE
        SCAN WHILE Style=lcFieldSty
          SCATTER MEMVAR
          SELECT (lctmpstypr)
          APPEND BLANK
          GATHER MEMVAR 
          REPLACE cStatus WITH 'S'
        ENDSCAN
     *B604203,4 KAM [end]       
        SELECT (lctmpstypr)
        INDEX ON cCurrCode TAG lctmppr1 Unique
        INDEX ON Style+cCurrCode TAG (lctmpstypr) Unique ADDI
        SET ORDER TO TAG (lctmpstypr)
      ENDIF
    ENDIF
    *B603258,1 (End)
    lnMarkupA = laData[54]
    lnMarkupB = laData[55]
    lnMarkupC = laData[56]
    lnMarkupR = laData[81]
    SHOW GET lnLoc ENABLE
    IF llPackUse
      SHOW GET pbPacks ENABLE
    ELSE
      SHOW GET pbPacks DISABLE    
    ENDIF
    *SHOW GET pbNotePad ENABLE
    SHOW GET pbObjlnk  ENABLE
    SHOW GET lcWareCode
    
    *C200307,1 AMH Get data from STYCDNOT file [Start]
    IF ASCAN(laEvntTrig , PADR('GETSTYNT',10)) <> 0
      =gfDoTriger('ICSTYLE',PADR('GETSTYNT',10))
    ENDIF
    *C200307,1 AMH [End]
     
    =lfChngFolder(lnActFolder)

    *B604965,1 ABD - Calculate the Plan Qty before change the folder. [Begin]
    IF llMScale .AND. llallcolors .AND. lnActFolder = 4
      =lfTotPrePak()
      SHOW GET ibPrepak DISABLE
      SHOW GET laData[31] DISABLE
      SHOW GET laData[32] DISABLE
      SHOW GET laData[33] DISABLE
      SHOW GET laData[34] DISABLE
      SHOW GET laData[35] DISABLE
      SHOW GET laData[36] DISABLE
      SHOW GET laData[37] DISABLE
      SHOW GET laData[38] DISABLE
      SHOW GET laData[39] DISABLE
      
    ENDIF
    *B604965,1 ABD - [End]
 
    IF lnActFolder=1
      SHOW GET pbFrnPrc  ENABLE
      IF !EMPTY(laData[11])
      	SHOW GET pbfabric ENABLE
      ELSE
      	SHOW GET pbfabric DISABLE
      ENDIF
    ENDIF 
    IF llMScale
      IF llAllScales
        SHOW GET pbDlt ENABLE    
        IF lnActFolder=2
          IF lnCntlScl = 1
            SHOW GET pbPrvsScl DISABLE
          ELSE
            SHOW GET pbPrvsScl ENABLE
          ENDIF     
          IF lnCntlScl=ALEN(laExtSz) OR SUBSTR(lcNonMjr,lnColorWid-2,3)<>'***'
            SHOW GET pbNxtScl DISABLE
          ELSE
            SHOW GET pbNxtScl ENABLE    
          ENDIF
        ENDIF
      ELSE
        SHOW GET pbPrvsScl DISABLE
        SHOW GET pbNxtScl DISABLE
        SHOW GET pbDlt DISABLE
      ENDIF
    ENDIF
    *--Navigation for all colors case. 
    =lfvNavigate()
*--HDM E301036,1[START]
   laData[6] = STYLE.SCALE
   IF !EMPTY(laData[6]) .AND. !llMScale
     =lfvscale()
   ENDIF
   lnPuPrepak = 1
   laData[7] = STYLE.PREPAK
   SHOW GET laData[7]

   IF !EMPTY(laData[7]) .AND. lcPrepack <> 'N'
     =lfvPrepak()
   ENDIF
*--HDM E301036,1[END]
   

  ***--- E D I T   M O D E ---***
  CASE laScrMode[3]  && Edit mode. 
    *B604432,4 KAM check if 'SP' module instaleed or not and 'pdmpatrn.dbf' open or not[start] 
    IF llSPInstld AND llOpnPatrn 
      lcGetPatED="ENABLE"    && assign 'enble' according to condition 
    ELSE
      lcGetPatED="DISABLE"   && assign 'disable' according to condition 
    ENDIF
    *B604432,4 KAM[end]
    =lfgetinfo()
    SELECT STYLE
    =gfObj_lock(.F.)
    SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
    =lfChngFolder(lnActFolder)
    SHOW GET ibStyle   DISABLE
    SHOW GET ibColor   DISABLE    
    *B604432,4 KAM use the variable content instead of explicitly word[start]
    *SHOW GET ibPattern DISABLE
    SHOW GET ibPattern &lcGetPatED
    *B604432,4 KAM[end]
    SHOW GET laData[5] ENABLE
    SHOW GET pbPacks DISABLE
    *SHOW GET pbNotePad ENABLE
    SHOW GET pbObjlnk  ENABLE
    
    IF lcPrepack='D'
      SHOW GET laData[7]
    ENDIF
    IF lcPrepack $'SD'
      SHOW GET laData[82] DISABLE
    ENDIF
    IF lnActFolder=1
      SHOW GET pbFrnPrc ENABLE
    ELSE
      SHOW GET pbFrnPrc DISABLE
    ENDIF
  	SHOW GET pbfabric DISABLE
    IF llMScale
      SHOW GET laData[6] DISABLE
      SHOW GET ibScale   DISABLE
    ENDIF
    *-- C200171 TMI [Start] Disable scale in edit mode        
    IF llNCInstl .AND. ASCAN(laEvntTrig , 'GNUPCCLR  ') <> 0 
      SHOW GET laData[6] DISABLE  
      SHOW GET ibScale   DISABLE
    ENDIF
    *-- C200171 TMI [End  ] Disable scale in edit mode    
    
    IF llDetCost
      SHOW GET pbEdtCost DISABLE
    ENDIF
    *--HDM B602667,1[Start]
    IF llCtrlDetC
      llDetCost = .F.
      SHOW GET llDetCost DISABLE
    ENDIF
    *--HDM B602667,1[End]

    SHOW GET laData[82] DISABLE
    SHOW GET laData[7] && ENABLE
    IF llAllScales
      SHOW GET ibPrepak DISABLE
      SHOW GET laData[7] DISABLE
      SHOW GET laData[82] DISABLE     && HDM
      SHOW GET laData[31] DISABLE
      SHOW GET laData[32] DISABLE
      SHOW GET laData[33] DISABLE
      SHOW GET laData[34] DISABLE
      SHOW GET laData[35] DISABLE
      SHOW GET laData[36] DISABLE
      SHOW GET laData[37] DISABLE
      SHOW GET laData[38] DISABLE
    ENDIF
     
*    *B804219,1 (Begin)
     IF ((llMScale .AND. !llAllScales)  OR (!llMScale .AND. !llAllColors )) AND !EMPTY(laData[7]) AND lcPrePack <> 'N'
       *--Enable the total plan field.
       llPlanSt = .T.
     ELSE
       llPlanSt = .F.
     ENDIF
*    IF ASCAN(laEvntTrig , PADR('ENBLPLN',10)) <> 0
*      =gfDoTriger('ICSTYLE',PADR('ENBLPLN',10))
*    ENDIF
*    *B804219,1 (End)

	*C102390,1 TMI [Start] Enable/disable the dyelot flag depends on 'DYELOT' subprocess.
    SHOW GET llDye &lcDyeStat
	*C102390,1 TMI [End  ]
    
    *C102452,1 HBG 21/11/2001 Disable popup of group in the style screen , it'll be updated 
    *C102452,1                from style classification screen[Begin]
    IF llGMATrig
      SHOW GET lnGrp  DISABLE
    ENDIF 
    *C102452,1 [End]
    *B128128,1 HBG 11/20/2005 Save the orginal cost to the array [Begin]
    For lnI =1 To 5
      lcI = STR(lnI,1)
      laCostFld[lnI,1] = lnCostVal&lcI
    ENDFOR
    *B128128,1 [End]


  ***--- A D D   M O D E ---***
  CASE laScrMode[4]  && Add mode. 
    
    *C102452,1 HBG 21/11/2001 Reset values of variables in style clasification screen in new mode [Begin]
    IF llGMATrig 
      STORE 1 TO lnDept,lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
    ENDIF  
    *C102452,1 [End]
    
    *B604432,4 KAM check if 'SP' module instaleed or not and 'pdmpatrn.dbf' open or not[start] 
    IF llSPInstld AND llOpnPatrn 
      lcGetPatED="ENABLE"     && assign 'enable ' according to condition 
    ELSE
      lcGetPatED="DISABLE"    && assign 'disable' according to condition
    ENDIF
    *B604432,4 KAM[end]
  
    SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
    =lfChngFolder(lnActFolder)
    SHOW GET ibStyle   DISABLE
    SHOW GET ibColor   DISABLE    
    *B604432,4 KAM use the content variable instead of explicitly word [start]
    *SHOW GET ibPattern DISABLE
    SHOW GET ibPattern &lcGetPatED
    *B604432,4 KAM[end]
    SHOW GET laData[5] ENABLE
    SHOW GET lnLoc     DISABLE
    SHOW GET pbPacks DISABLE
    *SHOW GET pbNotePad DISABLE
    SHOW GET pbObjlnk  DISABLE
    SHOW GET pbFabric DISABLE
    SHOW GET pbFrnPrc  ENABLE
    SHOW GET laData[82] DISABLE
    SHOW GET laData[7] ENABLE

    IF llAllScales
      SHOW GET ibPrepak DISABLE    
      SHOW GET laData[82] DISABLE     && HDM
      SHOW GET laData[7] DISABLE    
      SHOW GET laData[31] DISABLE          
      SHOW GET laData[32] DISABLE          
      SHOW GET laData[33] DISABLE          
      SHOW GET laData[34] DISABLE          
      SHOW GET laData[35] DISABLE          
      SHOW GET laData[36] DISABLE          
      SHOW GET laData[37] DISABLE          
      SHOW GET laData[38] DISABLE                                              
    ENDIF
    IF !llCopySt
      = gfwCodePop ( @laCodInfo, "SEASON"   ,"D" )
      = gfwCodePop ( @laCodInfo, "CDIVISION","D" )
      = gfwCodePop ( @laCodInfo, "CSTYGROUP","D" )
      = gfwCodePop ( @laCodInfo, "ROYALTY"  ,"D" )  
      = gfwCodePop ( @laCodInfo, "CDISCCODE","D" )  
      = gfwCodePop ( @laCodInfo, "CPURCODE" ,"D" )  

      laData[8] =laSea[lnSea,2]
      laData[9] =laDiv[lnDiv,2]
      laData[10]=laGrp[lnGrp,2]  
      laData[67]=laRoy[lnRoy,2]    
      laData[78]=laDisc[lnDisc,2]    
      laData[79]=laPurc[lnPurc,2]    
      
      IF (ALLTRIM(gcContCode) = lcEngland)
        = gfwCodePop ( @laCodInfo, "CTAXCODE","D" )  
        laData[63]=laTax[lnTax,2]    
        DIME laTermAry[1,2]
        laTermAry[1,1] = 'NTAXRATE'
        laTermAry[1,2] = 'lnTaxPr'  
        =gfRltFld(laData[63],@laTermAry, 'CTAXCODE')
      ENDIF
      STORE 0.0 TO lncostval0,lncostval1,lncostval2,lncostval3,lncostval4,lncostval5,laData[57],laData[2]
      STORE 0.0 TO lnCostPrc0,lnCostPrc2,lnCostPrc3,lnCostPrc4,lnCostPrc5
    ELSE
      IF llDetCost
        STORE 0.0 TO lncostval0,lncostval1,lncostval2,lncostval3,lncostval4,lncostval5,laData[57],laData[2]
        STORE 0.0 TO lnCostPrc0,lnCostPrc2,lnCostPrc3,lnCostPrc4,lnCostPrc5
      ENDIF
      lcSvStyle=lcMajor
      lcMajor  =lcFrmStyle
      = gfwCodePop ( @laCodInfo, "SEASON"   ,"T" )
      = gfwCodePop ( @laCodInfo, "CDIVISION","T" )
      = gfwCodePop ( @laCodInfo, "CSTYGROUP","T" )
      
      = gfwCodePop ( @laCodInfo, "ROYALTY"  ,"T" )  
      = gfwCodePop ( @laCodInfo, "CDISCCODE"  ,"T" )
      = gfwCodePop ( @laCodInfo, "CPURCODE"  ,"T" )
      *=lfvvldplan()
      IF (ALLTRIM(gcContCode) = lcEngland)
        = gfwCodePop ( @laCodInfo, "CTAXCODE","T" )
      ENDIF
      lcMajor=lcSvStyle
    ENDIF
    *--HDM B602667,1[Start]
    IF llCtrlDetC
      llDetCost = .F.
      SHOW GET llDetCost DISABLE
    ENDIF
    *--HDM B602667,1[End]
 
    *C102452,1 HBG 21/11/2001 Diable popup of group in the style screen , it'll be updated 
    *C102452,1                from style clasification screen[Begin]
    IF llGMATrig
      SHOW GET lnGrp DISABLE
    ENDIF 
    *C102452,1 [End]

	*C200076,1 (Begin) Default custom fields.
	*If company does not have User fields then don't show it.
	*B604016,1 MHM 11/14/2000 we comment this lines accourding to Fix it in global  [START]
    *B803629,1 MHM 08/29/2000 [start]
	*IF !EMPTY(laUsrFields[1,1])
	*IF !EMPTY(laUsrFields[1,1]) .AND. ASCAN(laEvntTrig , PADR('INI_VAR',10)) <> 0
	*B803629,1 MHM 08/29/2000 [end]
    *  = gfDoTriger('ICSTYLE','INI_VAR')
	*ENDIF
	*B604016,1 MHM 11/14/2000   [END]
	*C200076,1 End.

	*C102390,1 TMI [Start] Enable/disable the dyelot flag depends on 'DYELOT' subprocess
    SHOW GET llDye &lcDyeStat
	*C102390,1 TMI [End  ]

    *C200307,1 AMH Get data from STYCDNOT file [Start]
    IF ASCAN(laEvntTrig , PADR('GETSTYNT',10)) <> 0
      =gfDoTriger('ICSTYLE',PADR('GETSTYNT',10))
    ENDIF
    *C200307,1 AMH [End]

ENDCASE

*-- HDM E500252,1 03/30/1999 [Start] Control The Style Type PopUp According to Installed Modules

DO CASE
  CASE !llPOInstld .AND. llMFInstld

    *-- B802476,1 [Start] if Point of Sale is installed we should allow the user 
    *---          to make the style as imported
    IF llMFInstld AND OCCURS('PS',gcCmpModules) <> 0 AND (laScrMode[3] OR laScrMode[4])
      SHOW GET lnDomestic ENABLE
    ENDIF
    
    
    IF llMFInstld AND OCCURS('PS',gcCmpModules) = 0
      SHOW GET lnDomestic DISABLE
      *-- HDM B802185,1[Start] We have to re-assign value to laData[21] as well
      laData[21] = .T.
      *-- HDM B802185,1[End]
      lnDomestic = 1
    ENDIF

    *-- B802476,1 [End]
 
  CASE llPOInstld .AND. !llMFInstld
    *HDM B802185,1[Start] We have to re-assign value to laData[21] as well
    laData[21] = .F.
    *HDM B802185,1[End]
    lnDomestic = 2
    SHOW GET lnDomestic DISABLE

  
ENDCASE
*-- HDM E500252,1 03/30/1999 [End]

*SHOW GET laData[7] ENABLE
lnPuPrepak = 1
=lfvvldplan('laData[39]')
*--HDM

*--Disable all Codes used in structure.
IF laScrMode[3] OR laScrMode[4]
  =lfDibCods(lcMajor)
ENDIF

llGetInfo = .T.

RETURN

*!*************************************************************
*! Name      : lfSegInfo
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Biuld array for segment information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : array elements as :
*!-1 MAJOR,NONMAJOR
*!-2 SEGMENT NUMBER
*!-3 SEGMENT TYPE 'FOTQZCDGS'
*!   F-> Free , O-> Other , T->Make , Q->Quality ,Z->Season
*!   C-> Color , D->Division ,G->Group ,S->Size
*!-4 SEGMENT START
*!-5 SEGMENT SIZE
*!-6 SEGMENT FILE
*!-7 SEGMENT FILE TAG
*!-8 SEGMENT FILE KEY
*!-9 SEGMENT FILE FIELD
*!*************************************************************
*! Example   : =lfSegInfo()
*!*************************************************************
FUNCTION lfSegInfo

lnAlias=SELECT()
llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
IF !SEEK('U1','ICISTRU')
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
  RETURN .F.
ENDIF
*E301152,1 Hesham (Start)
*IF USED('ICISTRU') AND llStruOp
*  USE IN ICISTRU
*ENDIF
*E301152,1 Hesham (End)

DIME laMajSeg[1,1]
=gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')
lnNonSeg  = gfItemMask('SN')  
lcItemTl  = gfItemMask('HI')
lcMjrTtl  = gfItemMask('HM')
lcNMjrTl  = gfItemMask('HN')
lcMjrPct  = gfItemMask('PM')
lcNMjrPt  = gfItemMask('PN')
lcIMjrPt  = gfItemMask('PI')
lnstylewid=LEN(lcMjrPct)
lncolorwid=LEN(lcNMjrPt)
lcSepart  =SUBSTR(lcIMjrPt,lnstylewid+1,1)

*--HDM RE-Position style & color Fields [start]
lnXPos    = 2.88 + lnstylewid + 0.89
lnGetXPos = 2.88 + lnXPos + 0.89
*--HDM RE-Position style & color Fields [end]

FOR lnCnt=1 TO ALEN(laMajSeg,1)
  DIME laSegInfo[lnCnt,9]
  IF lnCnt<=lnMajSeg
     laSegInfo[lnCnt,1] = 'M' 
  ELSE
    IF lnCnt<=lnNonSeg+lnMajSeg
      laSegInfo[lnCnt,1] = 'N' 
    ELSE
      laSegInfo[lnCnt,1] = ' ' 
    ENDIF
  ENDIF
  laSegInfo[lnCnt,2]=STR(lnCnt,1)
  laSegInfo[lnCnt,3]=laMajSeg[lnCnt,1]
  laSegInfo[lnCnt,4]=laMajSeg[lnCnt,4]
  laSegInfo[lnCnt,5]=LEN(laMajSeg[lnCnt,3])
  laSegInfo[lnCnt,6]=IIF(laMajSeg[lnCnt,1]$'FOQT','ICSEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','CODES'))
  laSegInfo[lnCnt,7]=IIF(laMajSeg[lnCnt,1]$'FOQT','SEGVAL',;
                     IIF(laMajSeg[lnCnt,1]='S','SCALE','Idrltfname'))
  
  laSegInfo[lnCnt,8]=IIF(laMajSeg[lnCnt,1]$'FOQT',STR(lnCnt,1),;
                     IIF(laMajSeg[lnCnt,1]='S','S','NN'))
  IF laMajSeg[lnCnt,1]$'CZDGF'
    DO CASE
      CASE laSegInfo[lnCnt,3]='C'
        lcCdTyp = 'COLOR     '
      CASE laSegInfo[lnCnt,3]='Z'
        lcCdTyp = 'SEASON    '
      CASE laSegInfo[lnCnt,3]='D'
        lcCdTyp = 'CDIVISION '
      CASE laSegInfo[lnCnt,3]='G'
        lcCdTyp = 'CSTYGROUP '
      CASE laSegInfo[lnCnt,3]='F'
        lcCdTyp  =''
        lcFreeDc = laMajSeg[lnCnt,2]
    ENDCASE    
    laSegInfo[lnCnt,8] = laSegInfo[lnCnt,8]+lcCdTyp
  ENDIF
  laSegInfo[lnCnt,9] = IIF(laMajSeg[lnCnt,1]$'FOQT',"ciSegVal",;
                       IIF(laMajSeg[lnCnt,1]='S',"Scale","cCode_no"))
ENDFOR
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfCntFiles
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Open Or Close internely used files.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llCloseFl -> .T. for close files
*!                                   .F. for open files
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfCntFiles
PARA llCloseFl

lnAlias=SELECT()
IF llCloseFl
  IF USED('SycCurr') AND laOpFile[2]
    USE IN SycCurr
  ENDIF
  IF USED('OrdHdr') AND laOpFile[3]
    USE IN OrdHdr
  ENDIF
  IF USED('OrdLine') AND laOpFile[4]
    USE IN OrdLine
  ENDIF
  IF USED('Piktkt') AND laOpFile[5]
    USE IN Piktkt
  ENDIF
  IF USED('POShdr') AND laOpFile[6]
    USE IN POShdr
  ENDIF
  IF USED('POSLN') AND laOpFile[7]
    USE IN POSLN
  ENDIF
  IF USED('Shpmthdr') AND laOpFile[8]
    USE IN Shpmthdr
  ENDIF
  IF USED('InvHdr') AND laOpFile[9]
    USE IN InvHdr
  ENDIF
  IF USED('InvLine') AND laOpFile[10]
    USE IN InvLine
  ENDIF
  IF USED('RETHdr') AND laOpFile[11]
    USE IN RETHdr
  ENDIF
  IF USED('RETLine') AND laOpFile[12]
    USE IN RETLine
  ENDIF
  IF USED('RetAuth') AND laOpFile[13]
    USE IN RetAuth
  ENDIF
  IF USED('RALine') AND laOpFile[14]
    USE IN RALine
  ENDIF
  IF USED('CUTTKTh') AND laOpFile[15]
    USE IN CUTTKTh
  ENDIF
  IF USED('CUTTKTl') AND laOpFile[16]
    USE IN CUTTKTl
  ENDIF
  IF USED('SycExch') AND laOpFile[17]
    USE IN SycExch
  ENDIF
  IF USED('STYLEUPC') AND laOpFile[19]
    USE IN STYLEUPC
  ENDIF
  IF USED('CUSTOMER') AND laOpFile[21]
    USE IN CUSTOMER
  ENDIF
  IF USED('FABRIC') AND laOpFile[20]
    USE IN FABRIC
  ENDIF
  IF USED('ICSegVal') AND laOpFile[22]
    USE IN ICSegVal
  ENDIF
  IF llMScale AND USED('ScaleHd') AND laOpFile[23]
    USE IN ScaleHd
  ENDIF
  *E301525,1 HS Add these lines to close EDI files [Begin]
  IF USED('EDICatgH') AND laOpFile[24]
    USE IN EDICatgH
  ENDIF
  IF USED('EDICatgD') AND laOpFile[25]
    USE IN EDICatgD
  ENDIF
  IF USED('EDITRANS') AND laOpFile[26]
    USE IN EDITRANS
  ENDIF
  *E301525,1 HS Add these lines to close EDI files [End]
  
  SELECT STYLE
ELSE
*E301077,7 comment opening of this files, it will opended when needed.
* laOpFile[2] = gfOpenFile(gcSysHome+'SycCurr' ,'cCurrCode','SH')
*  IF llSOInstld
*    laOpFile[3] = gfOpenFile(gcDataDir+'OrdHdr' ,'OrdHdr'  ,'SH')
*    laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
*  ENDIF
*  IF llPOInstld
*    laOpFile[7] = gfOpenFile(gcDataDir+'POSLN' ,'POSLNS'  ,'SH')
*  ENDIF 
*  IF llSOInstld OR llARInstld
*    laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
*  ENDIF
*  laOpFile[17] = gfOpenFile(gcSysHome+'SycExch','Currency','SH')
*--- B602631,1 HDM    03/11/1999 Error In Opening CT Files [Done By Hesham][Start]
*  IF llMFInstld
*    laOpFile[15]= gfOpenFile(gcDataDir+'CuttktH','CuttktH' ,'SH')
*    laOpFile[16]= gfOpenFile(gcDataDir+'CuttktL','CuttktLs','SH')
*  ENDIF
*--- B602631,1 HDM    03/11/1999 Error In Opening CT Files [Done By Hesham][End]
  IF llMAInstld 
    laOpFile[20]= gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  ENDIF 
  laOpFile[22] = gfOpenFile(gcDataDir+'ICSegVal','Segval','SH')
  IF llMScale
    laOpFile[23]=gfOpenFile(gcDataDir+'scalehd','Extscale','SH')
  ENDIF

  SELECT GL_LINK
  SET ORDER TO TAG GL_LINK1  
  SELECT STYLE
  SET ORDER TO TAG STYLE  
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : gfCPBrows()
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose : ToolBar Browse Icon Valid function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION gfCPBrows

lnAlias=SELECT()
*--HDM [START]
*SELECT STYLE
*IF llAllColors
*  SET ORDER TO TAG CSTYLE
*ENDIF
*--HDM [End]
*lcBrFields = IIF(llAllColors,[cStyMajor:19:H="Style",],[Style:19,])+;
*             [Desc:13:H="Description",]+;
*             [Season:6:H="Season",cdivision:6:H="Division",Pricea]+;
*             [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*             [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
*             [Fabric:9:h="Fabric"]

*lcBrFields = IIF(llAllColors,[cStyMajor:19:H="Style",],[Style:19,])+;
*             [Desc:13:H="Description",]+;
*             [Season:6:H="Season",cdivision:6:H="Division",Pricea]+;
*             [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*             [totord:7:h="Orders",]+;
*             [Fabric:9:h="Fabric"]

DIME laTempData[1]
STORE '' TO laTempData
*--HDM B802143,1 Call gfStyBrw instead of gfBrowse [START]
*B120959,1  TMI [Start] Force browse
PRIVATE llSavBrwVl
llSavBrwVl = llbrowse
llbrowse = .T.
*B120959,1  TMI [End  ] 
DO CASE
  CASE EMPTY(lcMajor) AND EMPTY(lcNonMjr)
    lcItemCd = gfStyBrw('I','','',.F.)
    IF !EMPTY(lcItemCd)
      llShowData = .T.
      llAllColors = .F.
    ELSE
      llShowData = .F.
    ENDIF
  CASE !EMPTY(lcMajor) AND EMPTY(lcNonMjr)
    *B120959,1  TMI [Start] Browse starting from Current selected major style ( with colors )
    *lcNonMjr = gfStyBrw('I','','',.F.)
    lcNonMjr = gfStyBrw('I',lcMajor,'',.F.)
    *B120959,1  TMI [End  ] 
    IF !EMPTY(lcNonMjr)
      llShowData = .T.
    ELSE
      llShowData = .F.
    ENDIF
  CASE !EMPTY(lcMajor) AND !EMPTY(lcNonMjr)
    IF llAllColors
      *B603036,1 Start.
      *B120959,1  TMI [Start] Browse from current major style ( major Only )
      *lcBrMaj= gfStyBrw('M','','',.T.)
      lcBrMaj= gfStyBrw('M',lcMajor,'',.F.)
      *B120959,1  TMI [End  ] 
      *lcMajor = gfStyBrw('M','','',.T.)
      IF !EMPTY(lcBrMaj)
        lcMajor=lcBrMaj
      *B603036,1 End.
        llShowData = .T.
      ELSE
        llShowData = .F.
      ENDIF

    ELSE
      *B120959,1  TMI [Start] Browse from current style color
      *lcItemCd = gfStyBrw('I','','',.F.)
      lcItemCd = gfStyBrw('I',STYLE.STYLE,'',.F.)
      *B120959,1  TMI [End  ] 
      IF !EMPTY(lcItemCd)
        *B606345,1 TMI 09/23/2002 (Begin) Get the non major from the browse.
        lcNonMjr = SUBSTR(lcItemCd,lnStyleWid+2,lnColorWid)
        *B606345,1 TMI 09/23/2002 (End)
        llShowData = .T.
      ELSE
        llShowData = .F.
      ENDIF
    ENDIF
ENDCASE
*B120959,1  TMI [Start] Restore "llbrowse" variable value
llbrowse = llSavBrwVl
*B120959,1  TMI [End  ] 

*llShowData = gfBrows(.f.,'style','laTempData')
*SELECT STYLE
*SET ORDER TO TAG STYLE  
*--HDM B802143,1 [END]

*lcBrFields = [Style:19,DESC:13:H="Description",]+;
*           [Season:6:H="Season",cdivision:6:H="Division",Pricea]+;
*           [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*           [totord:7:h="Orders",OTS=(TOTWIP+TOTSTK-TOTORD):7:H="O.T.S.",]+;
*           [Fabric:9:h="Fabric"]

*lcBrFields = [Style:19,DESC:13:H="Description",]+;
*             [Season:6:H="Season",cdivision:6:H="Division",Pricea]+;
*             [:6:h="Price",totWip:7:h="WIP",totstk:7:h="Stock",]+;
*             [totord:7:h="Orders",]+;
*             [Fabric:9:h="Fabric"]

SELECT(lnalias)
IF llShowData
  IF laScrmode[1]
    lnlastfold  = lnactfolder
    lnactfolder = 1
  ENDIF
  lascrmode   = .F.
  lascrmode[2]= .T.
  llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
  *-- HDM B602631,1 Assign The Default ware house case POS[Start]
  lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
  *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
  *-- HDM B602631,1 Assign The Default ware house case POS[End]
  SHOW GETS
ENDIF
RETURN


*!*************************************************************
*! Name      : lfActPad
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Bulid a new menu pad [Options]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfDispBar()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfActPad

DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions'
ON PAD _Option OF _msysmenu ACTIVATE POPUP _lPopOpt

DEFINE POPUP _lPopOpt MARGIN SHADOW
DEFINE BAR 1 OF _lPopOpt PROMPT "\<Proposed" SKIP FOR (lnactfolder<>2) OR !laData[21]
DEFINE BAR 2 OF _lPopOpt PROMPT "\<Cut     " SKIP FOR (lnactfolder<>2) OR !laData[21]
DEFINE BAR 3 OF _lPopOpt PROMPT "\<Actual  " SKIP FOR (lnactfolder<>2) OR !laData[21]
DEFINE BAR 4 OF _lPopOpt PROMPT "\-" SKIP FOR .T.
*C200077,1 Define an option bar for on shelf inventory.
*DEFINE BAR 5 OF _lPopOpt PROMPT "Style Packs" SKIP FOR (!llPackUse OR !laScrMode[2])
DEFINE BAR 5 OF _lPopOpt PROMPT "\<Total Inventory" SKIP FOR (lnactfolder<>2)
DEFINE BAR 6 OF _lPopOpt PROMPT "\<On-Shelf Inventory" SKIP FOR (lnactfolder<>2)
DEFINE BAR 7 OF _lPopOpt PROMPT "\-" SKIP FOR .T.
DEFINE BAR 8 OF _lPopOpt PROMPT "\<Style Packs" SKIP FOR (!llPackUse OR !laScrMode[2])
*C102528,1 ASH 02/12/2002 (Begin) Add an option to browse the style SKU's. 
DEFINE BAR 9 OF _lPopOpt PROMPT "Style Sk\<us" SKIP FOR (!llPackUse OR !laScrMode[2])
*C102528,1 ASH 02/12/2002 (End)
*C102452,1 HBG 21/11/2001 Add par To style classification screen in option menu[Begin]
IF llGMATrig 
  *C102528,1 ASH 02/12/2002 (Begin) Change GMA bar position.
  *DEFINE BAR 9 OF _lPopOpt PROMPT "\<Style Classification" SKIP FOR (laScrMode[1])
  DEFINE BAR 10 OF _lPopOpt PROMPT "\<Style Classification" SKIP FOR (laScrMode[1])
  *C102528,1 ASH 02/12/2002 (End)
ENDIF  
*C102452,1 [End]

*C200431,1 RAE Add new menu bar. [start]
IF ASCAN(laEvntTrig , PADR('DAVMENU',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('DAVMENU',10))
ENDIF
*C200431,1 RAE [end]
*C200307,1 AMH Add a Custom menu Bar in Option pad for Customer Collage [Start]
IF ASCAN(laEvntTrig , PADR('COLADBAR',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('COLADBAR',10))
ENDIF     
*C200307,1 AMH [End]
*C200490,1 WAB (Start) used the same trigger to add a custom bar # 10
*C200388,1 (Begin) Add tha bar
IF ASCAN(laEvntTrig , PADR('ADDBAR',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('ADDBAR',10))
ENDIF     
*C200388,1 (End)
*C200490,1 WAB (End) 

*C200519,1 TMI [Start] Add bar for GMA titled "Customer Packing Info"
IF ASCAN(laEvntTrig , PADR('ADDCSTPK',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('ADDCSTPK',10))
ENDIF     
*C200519,1 TMI [End  ] 

*C038291,1  TMI [Start] Call the screen 'Style Measurements' for Vijay
IF ASCAN(laEvntTrig , PADR('OPMSMNPD',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('OPMSMNPD',10))
ENDIF     
*C038291,1  TMI [End  ] 

llOnShelf = .F.
lcPCAoptn = ' '
SET MARK OF BAR 5 OF _lPopOpt TO .T.
*IF laData[21]
  =lfDispBar()
*ENDIF
*C200077,1 End.

ON SELECTION POPUP _lPopOpt DO lpOptions
ON KEY LABEL ALT+P ACTIVATE POPUP _lPopOpt

*!*************************************************************
*! Function  : lfDispBar
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : function to display Options pad.
*!*************************************************************
FUNCTION lfDispBar

IF laData[21]
  DO CASE
    CASE lnBarNo =1
      SET MARK OF BAR 1 OF _lPopOpt TO .T.
      SET MARK OF BAR 2 OF _lPopOpt TO .F.
      SET MARK OF BAR 3 OF _lPopOpt TO .F.
    CASE lnBarNo =2
      SET MARK OF BAR 1 OF _lPopOpt TO .F.
      SET MARK OF BAR 2 OF _lPopOpt TO .T.
      SET MARK OF BAR 3 OF _lPopOpt TO .F.
    CASE lnBarNo =3
      SET MARK OF BAR 1 OF _lPopOpt TO .F.
      SET MARK OF BAR 2 OF _lPopOpt TO .F.
      SET MARK OF BAR 3 OF _lPopOpt TO .T.
  ENDCASE
ENDIF
*C200077,1 Added to control on-shelf inventory option.
IF lnBarNo = 5
  SET MARK OF BAR 5 OF _lPopOpt TO .T.
  SET MARK OF BAR 6 OF _lPopOpt TO .F.
ENDIF
IF lnBarNo = 6
  SET MARK OF BAR 5 OF _lPopOpt TO .F.
  SET MARK OF BAR 6 OF _lPopOpt TO .T.
ENDIF
*C200077,1 End.
RETURN



*!*************************************************************
*! Function  : lpOptions
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : function to run the option bar.
*!*************************************************************
PROCEDURE lpOptions

*C200077,1 Make a coresponding changes due to On-shelf inventory option.
*IF BAR() <>5 AND lnBarNo=BAR()
*C102452,1 HBG 21/11/2001 Add bar To style classification screen in option menu[Begin]   
*IF BAR() <>8 AND lnBarNo=BAR()
*C102528,1 ASH 02/12/2002 (Begin) Add new bar for SKU.
*IF IIF(llGMATrig,BAR() <>9,BAR() <>8) AND lnBarNo=BAR()

*C200307,1 AMH Add bar To Assign coded notes screen in option menu [Start]
*IF IIF(llGMATrig,BAR() <>10,BAR() <>9) AND lnBarNo=BAR()
IF IIF(llGMATrig .OR. ASCAN(laEvntTrig , PADR('RUNSTYNT',10)) <> 0,BAR() <>10,BAR() <>9);
   .AND. lnBarNo=BAR()
*C200307,1 AMH [End]

*C102528,1 ASH 02/12/2002 (End)
*C102452,1 [End]
  RETURN
ENDIF
lnBarNo=BAR()
DO CASE
  CASE BAR() =1
    lcPCAoptn = 'P'
    SET MARK OF BAR 1 OF _lPopOpt TO .T.
    SET MARK OF BAR 2 OF _lPopOpt TO .F.
    SET MARK OF BAR 3 OF _lPopOpt TO .F.
    llDummys=IIF(!llDown,lfvSUp(!laScrMode[2]),lfvSDn(!laScrMode[2]))
  CASE BAR() =2
    lcPCAoptn = 'C'
    SET MARK OF BAR 1 OF _lPopOpt TO .F.
    SET MARK OF BAR 2 OF _lPopOpt TO .T.
    SET MARK OF BAR 3 OF _lPopOpt TO .F.
    llDummys=IIF(!llDown,lfvSUp(!laScrMode[2]),lfvSDn(!laScrMode[2]))
  CASE BAR() =3
    lcPCAoptn = 'A'
    SET MARK OF BAR 1 OF _lPopOpt TO .F.
    SET MARK OF BAR 2 OF _lPopOpt TO .F.
    SET MARK OF BAR 3 OF _lPopOpt TO .T.
    *B602855,1 put in in case. 
    llDummys=IIF(!llDown,lfvSUp(!laScrMode[2]),lfvSDn(!laScrMode[2]))
  *C200077,1 Added new bars controls.
  CASE BAR() =5
    llOnShelf = .F.
    SET MARK OF BAR 5 OF _lPopOpt TO .T.
    SET MARK OF BAR 6 OF _lPopOpt TO .F.
    llDummys=IIF(!llDown,lfvSUp(!laScrMode[2]),lfvSDn(!laScrMode[2]))
  CASE BAR() =6
    llOnShelf = .T.
    SET MARK OF BAR 5 OF _lPopOpt TO .F.
    SET MARK OF BAR 6 OF _lPopOpt TO .T.
    llDummys=IIF(!llDown,lfvSUp(!laScrMode[2]),lfvSDn(!laScrMode[2]))
  *CASE BAR() =5
  *C200077,1 End.
  CASE BAR() =8
    =lfvPackBrw()
  *C102528,1 ASH 02/12/2002 (Begin) Add new option to brwose the SKUs.
  CASE BAR() = 9
    =lfvSkubrw()
  *C102528,1 ASH 02/12/2002 (End)
  *C102452,1 HBG 21/11/2001 Add bar To style classification screen in option menu [Begin]
  CASE BAR() = 10
    
    *C200307,1 AMH Run Custom Assign coded notes screen from Bar in Option pad for Customer Collage [Start]
    *=lfvStyClas()
    *C200490,1 WAB used the same trigger to view the carton detials
    IF ASCAN(laEvntTrig , PADR('RUNSTYNT',10)) <> 0
      *C123616,1  TMI [Start] call this function from within memmain.prg / where the bar is added via ADDBAR trigger
      *=gfDoTriger('ICSTYLE',PADR('RUNSTYNT',10))
      *C123616,1  TMI [End  ] 
    ELSE
      *C200388,1 (Begin) Note that BAR 10 will be used for all new custom option as the last standard one
      *C200388,1         is  BAR 9. So the folloing function will be invoked for any new custom bar 10 not for GMA only.
      *C200388,1         So restrict clling it to GMA according to the netry. 
      *=lfvStyClas()
      IF llGMATrig
        =lfvStyClas()
      ENDIF  
      *C200388,1 (End)
    ENDIF     
    *C200307,1 AMH [End]
    *C200388,1 (Begin) Auto Search
    IF ASCAN(laEvntTrig , PADR('GETDIRS',10)) <> 0
      =gfDoTriger('ICSTYLE',PADR('GETDIRS',10))
    ENDIF     
    *C200388,1 (End)
  *C102452,1 [End]
  *C200431,1 RAE call the screen. [start]
  CASE BAR() = 11
    IF ASCAN(laEvntTrig , PADR('OPTNBAR',10)) <> 0
      =gfDoTriger('ICSTYLE',PADR('OPTNBAR',10))
    ENDIF
  *C200431,1 RAE [end]
  
ENDCASE
*B602855,1 Comment out.
*IF BAR() <> 5
*  IF !llDown
*    =lfvSUp(!laScrMode[2])
*  ELSE
*    =lfvSDn(!laScrMode[2])
*  ENDIF
*ENDIF
*C200077,1 End.
*B602855,1 End.
RETURN



*!*************************************************************
*! Name      : lfDibCods
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : To Read and disable codes in structure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetCode()
*!*************************************************************
*! Passed Parameters  : Style code string to check
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfDibCods()
*!*************************************************************
FUNCTION lfDibCods
PARA lcCSgCod

FOR I=1 TO ALEN(laSegInfo,1)
  IF laSegInfo[I,3] $ 'TQGZD'
    lnDCode = lfGetCode(laSegInfo[I,3],laSegInfo[I,8],SUBSTR(lcCSgCod,laSegInfo[I,4],laSegInfo[I,5]),laSegInfo[I,6],.F.,.F.)
    DO CASE
      CASE laSegInfo[I,3] ='T'
        lnDomestic = lnDCode
        laData[21] = (lndomestic = 1)
        SHOW GET lnDomestic DISABLE
      CASE laSegInfo[I,3] ='Q'
        lnGrade = lnDCode
        laData[72]=STR(lnGrade,1)
        SHOW GET lnGrade DISABLE
      CASE laSegInfo[I,3] ='G'
        laData[10]=laGrp[lnGrp,2]
        SHOW GET lnGrp DISABLE      
      CASE laSegInfo[I,3] ='Z'
        laData[8] =laSea[lnSea,2]
        SHOW GET lnSea DISABLE
      CASE laSegInfo[I,3] ='D'
        laData[9] =laDiv[lnDiv,2]
        SHOW GET lnDiv DISABLE
    ENDCASE 
  ENDIF
ENDFOR


*!*************************************************************
*! Name      : lfvNavigate
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function for navigation(top,bottom,next,priveus).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvNavigate

lnAlias=SELECT()
SELECT STYLE
lnSvRc=IIF(!EOF(),RECNO(),0)
=SEEK(lcMajor+lcNMKey)
DO WHILE !BOF() AND Style=lcMajor+lcNMKey
  SKIP -1
ENDDO
lcDisEnb=IIF(!BOF(),'ENABLE','DISABLE')
SHOW GET PBTOP  &lcDisEnb
SHOW GET PBPRVS &lcDisEnb
=SEEK(lcMajor+lcNMKey)
DO WHILE !EOF() AND Style=lcMajor+lcNMKey
  SKIP
ENDDO
lcDisEnb=IIF(!EOF(),'ENABLE','DISABLE')
SHOW GET PBBTM &lcDisEnb
SHOW GET PBNXT &lcDisEnb
GOTO IIF(lnSvRc=0,1,lnSvRc)
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfOldValue
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*:*************************************************************
*! Name      : lfActFolder
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Activate folder when Change.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfvSUp(),lfvSDn()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfActFolder()
*!*************************************************************
FUNCTION lfActFolder

lnAlias=SELECT()
*E301152,1 Hesham (Start)
IF LASCRMODE[2]
  =lfInfoGet()
ENDIF
*E301152,1 Hesham (End)
DO CASE
  *--First Folder < G E N E R A L > ----------------------------
  CASE lnactfolder = 1
    SHOW WINDOW (lcWinCh1) TOP

    IF laScrMode[2] OR laScrMode[1]
      *SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      IF laScrMode[2]
        SHOW GET pbFrnPrc ENABLE
        IF !EMPTY(laData[11])
         	SHOW GET pbfabric ENABLE
        ENDIF
      ENDIF
    ELSE
      SHOW GETS WINDOW (lcWinCh1) ENABLE  ONLY
      IF (laScrMode[3] OR laScrMode[4])
        lcpdspmode  = IIF(laData[57]<>0 AND laData[24]=0,'ENABLE','DISABLE')
        SHOW GET laData[54] &lcpdspmode
        lcpdspmode  = IIF(laData[57]<>0 AND laData[25]=0,'ENABLE','DISABLE')
        SHOW GET laData[55] &lcpdspmode
        lcpdspmode  = IIF(laData[57]<>0 AND laData[26]=0,'ENABLE','DISABLE')
        SHOW GET laData[56] &lcpdspmode    	
        lcpdspmode  = IIF(laData[57]<>0 AND laData[60]=0,'ENABLE','DISABLE')
        SHOW GET laData[81] &lcpdspmode    	
        *C102390,4 TMI [Start] Enable/Disable DyeLot check box depends on User Privelages 
        SHOW GET llDye &lcDyeStat
        *C102390,4 TMI [End  ]            
        
      ELSE
        SHOW GET laData[54] DISABLE
        SHOW GET laData[55] DISABLE
        SHOW GET laData[56] DISABLE
        SHOW GET laData[81] DISABLE
      ENDIF

      SHOW GET pbfabric DISABLE
      IF llMScale AND !laScrMode[4]
        SHOW GET laData[6] DISABLE
        SHOW GET ibScale   DISABLE
      ENDIF
      IF llAllScales
        SHOW GET ibPrepak DISABLE    
        SHOW GET laData[7] DISABLE
        SHOW GET laData[82] DISABLE     && HDM
        SHOW GET laData[31] DISABLE          
        SHOW GET laData[32] DISABLE          
        SHOW GET laData[33] DISABLE          
        SHOW GET laData[34] DISABLE          
        SHOW GET laData[35] DISABLE          
        SHOW GET laData[36] DISABLE          
        SHOW GET laData[37] DISABLE          
        SHOW GET laData[38] DISABLE                                              
      ENDIF
    ENDIF
    IF !laScrMode[2]
      SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC42) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    ENDIF
    *--Disable all Codes used in structure.
    IF laScrMode[3] OR laScrMode[4]
      =lfDibCods(lcMajor)
    ENDIF
    SHOW GET pbKey DISABLE
    *--HDM B602667,1[Start]
    IF llCtrlDetC
      llDetCost = .F.
      SHOW GET llDetCost DISABLE
    ENDIF
    *--HDM B602667,1[END]
    *-- HDM E500252,1 03/30/1999 [Start] Control The Style Type PopUp According to Installed Modules

    DO CASE
      CASE !llPOInstld .AND. llMFInstld
        *-- B802476,1 [Start] if Point of Sale is installed we should allow the user 
        *---          to make the style as imported
        IF llMFInstld AND OCCURS('PS',gcCmpModules) <> 0 AND (laScrMode[3] OR laScrMode[4])
          SHOW GET lnDomestic ENABLE
        ENDIF
    
    
        IF llMFInstld AND OCCURS('PS',gcCmpModules) = 0
          SHOW GET lnDomestic DISABLE
          *-- HDM B802185,1[Start] We have to re-assign value to laData[21] as well
          laData[21] = .T.
          *-- HDM B802185,1[End]
          lnDomestic = 1
        ENDIF

        *-- B802476,1 [End]
      
      CASE llPOInstld .AND. !llMFInstld
        *HDM B802185,1[Start] We have to re-assign value to laData[21] as well
        laData[21] = .F.
        *HDM B802185,1[End]
        lnDomestic = 2
        SHOW GET lnDomestic DISABLE

ENDCASE
*-- HDM E500252,1 03/30/1999 [End]
    
    *C102452,1 HBG 21/11/2001 Disable popup of group in the style screen , it'll be updated 
    *C102452,1                from style classification screen[Begin]
    IF llGMATrig
      SHOW GET lnGrp  DISABLE
    ENDIF 
    *C102452,1 [End]

    
    =lfRefresh(lcWinCh1)
	*--Second Folder < C U T  &  S O L D > -------------------------
  CASE lnactfolder = 2
     *-- HDM B602631,1 Refresh The Cut & Sold Folder Up On the WareHouse PopUp[Start]
     IF laScrMode[2] .AND. llmultiwh
*       =lfvViewLoc()
     ENDIF
     *-- HDM B602631,1 Refresh The Cut & Sold Folder Up On the WareHouse PopUp[End]
     *SHOW WINDOW (lcWinCh2) TOP
     *IF laScrMode[2]
     *  *SHOW GETS WINDOW (lcWinCh2) ENABLE ONLY
     *ELSE
     *  SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
     *ENDIF
    lcShowMod=IIF(!laScrMode[1],'ENABLE','DISABLE')
    SHOW GET pbWHouse &lcShowMod
    SHOW GET pbDyelot &lcShowMod
    lcShowMod=IIF(laScrMode[2],'ENABLE','DISABLE')
    SHOW GET pbDyelots &lcShowMod
    SHOW GET lnLoc  &lcShowMod
    lcShowMod=IIF(laScrMode[2],'ENABLE','DISABLE')
    SHOW GET lcWareCode &lcShowMod
    IF llDown
      SHOW GET pbScrl,1 PROMPT lcCSUp
    ELSE
      SHOW GET pbScrl,1 PROMPT lcCSDn
    ENDIF
    SHOW GET pbScrl ENABLE
    IF llAllScales
      IF lnCntlScl = 1
        SHOW GET pbPrvsScl DISABLE
      ELSE
        SHOW GET pbPrvsScl ENABLE
      ENDIF     
      IF lnCntlScl=ALEN(laExtSz) OR SUBSTR(lcNonMjr,lnColorWid-2,3)<>'***'
        SHOW GET pbNxtScl DISABLE
      ELSE
        SHOW GET pbNxtScl ENABLE    
      ENDIF
    ELSE
      SHOW GET pbPrvsScl DISABLE
      SHOW GET pbNxtScl  DISABLE
    ENDIF
    SHOW GET pbKey DISABLE
    *SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    *SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    *SHOW GETS WINDOW (lcWinC42) DISABLE ONLY
    *SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    IF !llDown
      =lfvSUp(!laScrMode[2])
    ELSE
      =lfvSDn(!laScrMode[2])
    ENDIF
    *B602831,1 Comment the following code since it already 
    *B602831,1 called before in lfvSUp and lfvSDn functions.
*    IF !laScrMode[1]
*      SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
*      =lfRefresh(lcWinCh2)
*    ENDIF
    *B602831,1 End.
    
	*--Therd Folder < S P E C I A L > ----------------------------
  CASE lnactfolder = 3
    *SHOW WINDOW (lcWinCh3) TOP

    *B603875,1 KHM 09/04/2000 (Begin) Commented out.
    *lnTBrk  = STYLE.nTaxBreak
    *B603875,1 KHM 09/04/2000 (End)

    IF laScrMode[2] OR laScrMode[1]
      *SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    ELSE
      SHOW GETS WINDOW (lcWinCh3) ENABLE  ONLY
    ENDIF
    IF laScrMode[2] OR laScrMode[3]
      SHOW GET lnUpc ENABLE
    ELSE
      SHOW GET lnUpc DISABLE
    ENDIF
    IF !laScrMode[2]
      SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
      SHOW GETS WINDOW (lcWinC42) DISABLE ONLY
      SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    ENDIF
    *B602855,1 Added to show ret style.
    SHOW GET lcRetSty

    SHOW GET pbKey DISABLE    
    =lfRefresh(lcWinCh3)
	*--Fourth Folder < C O L O R S > ----------------------------
  CASE lnactfolder = 4 AND ! llNoNMjrs
    *---HDM
    STORE .F. TO llPreColor
    *---HDM

    ON KEY LABEL ALT+B ACTIVATE WINDOW (lcbrowcttl)
    SHOW WINDOW (lcWinCh4) TOP
    IF laScrMode[2] OR laScrMode[1] OR !llAllColors 
      SHOW GETS WINDOW (lcWinC42) DISABLE ONLY
    ELSE
      SHOW GET pbNewClr ENABLE
      GO TOP IN (lccolorfil)
      IF !EOF(lccolorfil)
        SHOW GET pbEdtPrc ENABLE
        IF !llDetCost AND !laData[21]
           SHOW GET pbEdtCost ENABLE
        ENDIF
      ENDIF
    ENDIF
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
    SHOW WINDOW (lcbrowcttl) REFRESH
    SHOW GET pbKey ENABLE

	*--Fifth Folder < C O S T I N G > ----------------------------
  CASE (lnactfolder = 5 AND llCostPrv ) OR ;
       (lnactfolder = 4 AND llNoNMjrs )
    *---HDM
    STORE .F. TO llPreColor
    *---HDM
    laData[21] = IIF(laScrMode[1],.T.,laData[21])
    SHOW WINDOW (lcWinCh5) TOP
    SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
    SHOW GETS WINDOW (lcWinCh3) DISABLE ONLY
    SHOW GETS WINDOW (lcWinC42) DISABLE ONLY
    =lfRefresh(lcWinCh5)
    =lfRefresh(lcWinCh4)
    IF laScrMode[2] OR laScrMode[1]
      SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
      IF laScrMode[2]
        SHOW GET pbCosting ENABLE
      ENDIF 
    ELSE
      IF !llDetCost
        SHOW GETS WINDOW (lcWinCh5) ENABLE ONLY
      ELSE
        SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
        IF !laData[21] AND llMulCurr
          lcDisMode=IIF(SEEK(lcMajor,'BOM'),'DISABLE','ENABLE')
          SHOW GET laData[27] &lcDisMode
          SHOW GET laData[28] &lcDisMode
        ENDIF
      ENDIF
      SHOW GET pbCosting DISABLE
    ENDIF
    SHOW GET pbKey DISABLE
    *--If no detail costing disable all fabric,trim and sty comp cost elements.
    IF !llDetCost AND (laScrMode[3] .OR. laScrMode[4])
      *--Imported style.
      IF lnDomestic = 2
          IF llMulCurr
            FOR I = 1 TO 5
              lcXCst = ALLTRIM(STR(I))
              IF lcIType&lcXCst $'FTS'
                IF lcIType&lcXCst = 'T' AND lcTrmInvM = 'N'
                  LOOP
                ENDIF
                SHOW GET laData[44 + I]   DISABLE
                SHOW GET lnFCostPr&lcXCst DISABLE
              ENDIF
            ENDFOR
          ELSE
            FOR I = 1 TO 5
              lcXCst = ALLTRIM(STR(I))
              IF lcIType&lcXCst $'FTS'
                IF lcIType&lcXCst = 'T' AND lcTrmInvM = 'N'
                  LOOP
                ENDIF
                SHOW GET lnCostVal&lcXCst DISABLE
                SHOW GET lnCostPrc&lcXCst DISABLE
              ENDIF
            ENDFOR
          ENDIF
      *--Domestic style.
      ELSE
        
          *--HDM B602714,1 03/30/1999 [START] Add This condition to disable Fabric/Trim/Sty. Comp.
          *--                                 case Det. Cst. <No>
          *B802574,1 Start, Comment out.
          *IF llMulCurr
          *  FOR I = 1 TO 5
          *    lcXCst = ALLTRIM(STR(I))
          *    IF lcMType&lcXCst $'FTS'
          *      IF lcMType&lcXCst = 'T' AND lcTrmInvM = 'N'
          *        LOOP
          *      ENDIF
          *      SHOW GET laData[44 + I]   DISABLE
          *      SHOW GET lnFCostPr&lcXCst DISABLE
          *    ENDIF
          *  ENDFOR
          *ELSE
          *B802574,1 End.
            FOR I = 1 TO 5
              lcXCst = ALLTRIM(STR(I))
              IF lcMType&lcXCst $'FTS'
                IF lcMType&lcXCst = 'T' AND lcTrmInvM = 'N'
                  LOOP
                ENDIF

                SHOW GET lnCostVal&lcXCst DISABLE
                SHOW GET lnCostPrc&lcXCst DISABLE
              ENDIF
            ENDFOR
          *ENDIF
          *--HDM B602714,1 03/30/1999 [End]
        ENDIF
    ENDIF

ENDCASE
SELECT(lnalias)
IF lnactfolder = 5
  IF llMulCurr
    IF !llDetCost
       *laData[83] = &lcColorFil..Gros_Price
       *laData[84] = &lcColorFil..Disc_Pcnt
      IF laScrMode[3] .OR. laScrMode[4]
        IF lnDomestic = 2
          SHOW GET laData[83] ENABLE
          SHOW GET laData[84] ENABLE
        ELSE
          SHOW GET laData[83] DISABLE
          SHOW GET laData[84] DISABLE
          *SHOW GET lnCostval0 DISABLE
          *SHOW GET lnCostPrc0  DISABLE
        ENDIF
      ENDIF
    ELSE
      SHOW GET laData[83] DISABLE
      SHOW GET laData[84] DISABLE
    ENDIF
  ELSE
    IF !llDetCost
      IF laScrMode[3] .OR. laScrMode[4]
        IF lnDomestic = 2
          SHOW GET lnCostval0  ENABLE
          SHOW GET lnCostPrc0  ENABLE
        ELSE
        *  SHOW GET lnCostval0 DISABLE
        *  SHOW GET lnCostPrc0 DISABLE
          SHOW GET laData[83] DISABLE
          SHOW GET laData[84] DISABLE
        ENDIF
      ENDIF
    *ELSE
    *  SHOW GET lnCostval0 DISABLE
    *  SHOW GET lnCostPrc0 DISABLE
    ENDIF
  ENDIF
ENDIF
IF lnactfolder = 5
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET laData[65] ENABLE
    SHOW GET laData[66] ENABLE
    SHOW GET ibVenBrow  ENABLE
  ENDIF
ENDIF
*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
=lpEditCost()
*B126312,1 EIH 03/13/2005 [End]
RETURN

*:*************************************************************
*! Name      : lfClrTrap
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Trap key.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfClrTrap

IF ! (WONTOP() <> ALLTRIM(lcBrowcTtl))
*IF WONTOP() = lcBrowCTtl
  ON KEY LABEL TAB     DO lpFld4Tab
  ON KEY LABEL BACKTAB DO lpFld4BkT
  ON KEY LABEL ENTER   llRun=IIF((laScrMode[3] OR laScrMode[4]) AND llAllColors AND ;
                                  !EOF(lcColorFil),lfvEdtPrc(),.T.)
ENDIF  

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : READ Activate function of INV700
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER
*B602831,1 disable all folders in select mode.
IF laScrMode[1]
  SHOW GETS WINDOW (lcWinCh1) DISABLE ONLY
  SHOW GET ibFolder[1] ENABLE
  SHOW GET ibFolder[2] DISABLE
  SHOW GET ibFolder[3] DISABLE
  SHOW GET ibFolder[4] DISABLE
  SHOW GET ibFolder[5] DISABLE
ELSE
  SHOW GET ibFolder[1] ENABLE
  SHOW GET ibFolder[2] ENABLE
  SHOW GET ibFolder[3] ENABLE
  SHOW GET ibFolder[4] ENABLE
  SHOW GET ibFolder[5] ENABLE
ENDIF
*B602831,1 End.

*!*************************************************************
*! Name      : lpFld4Tab
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpFld4Tab

IF WONTOP() <> ALLTRIM(lcBrowcTtl)
  _CUROBJ=_CUROBJ+1
ELSE
  IF llAllColors AND (laScrMode[3] OR laScrMode[4])
    ACTIVATE WINDOW (lcWinC42)
    _CUROBJ=OBJNUM(pbNewClr)
  ELSE  
    ACTIVATE WINDOW (lcfolder)  
    _CUROBJ=1
  ENDIF  
ENDIF
RETURN

*!*************************************************************
*! Name      : lpFld4BkT
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpFld4BkT

IF WONTOP(lcBrowCTtl)
  ACTIVATE WINDOW (lcfolder)
  _CUROBJ = OBJNUM(ibFolder(4))
ELSE
  ACTIVATE WINDOW (lcBrowCTtl)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfGetCode
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Control the work areas depends on
*!             control variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnPara ->Code type.
*!                      lcPKey ->Code key. 
*!                      lcCdStr->Code String.
*!                      lcCFile->Code file.
*!                      llCdDs ->Flag get code desc.
*!                      llChkCod->Check code existance.
*!*************************************************************
*! Returns            : lnRetVal ->Segment value) 0 if not.
*!                   IF llChkCod=.T. return will be  .T. or .F.
*!*************************************************************
*! Example   : =lfGetCode()
*!*************************************************************
FUNCTION lfGetCode
PARA lnPara,lcPKey,lcCdStr,lcCFile,llCdDs,llChkCod

lnAlias = SELECT()
lnRetVal= 0
llChkRetF=.T.
lcCdDesc= ''
SELECT (lcCFile)
lcMTag=TAG()
DO CASE
  CASE lnPara $ 'TQ'
    IF !SEEK(lcPKey+lcCdStr)
      llChkRetF = .F.
    ENDIF  
    lnRetVal=IIF(lnPara='T',IIF(icSegVal.cVldEntry='Y',1,2),INT(VAL(icSegVal.cVldEntry))) 
    lcCdDesc=icSegVal.cISgValSd
  CASE lnPara $ 'FO' AND llCdDs
    IF !SEEK(lcPKey+lcCdStr)
      llChkRetF = .F.
    ENDIF  
    lcCdDesc=icSegVal.cISgValSd
  CASE (lnPara $ 'DZGC' AND llCdDs) OR lnPara='C'
    SET ORDER TO TAG CODES
    lcCodeF = ALLTRIM(SUBSTR("CDIVISION SEASON    CSTYGROUP COLOR     ",(ATC(lnPara,'DZGC')*10)-9,10))
    IF !SEEK('N'+PADR(lcCdStr,6)+'N'+lcCodeF)
      llChkRetF = .F.
    ENDIF  
    lcCdDesc=CODES.cDiscrep
  CASE lnPara $ 'DZG' AND !llCdDs
    lcCodeF = ALLTRIM(SUBSTR("CDIVISION SEASON    CSTYGROUP ",(ATC(lnPara,'DZG')*10)-9,10))
    = gfwCodePop ( @laCodInfo, lcCodeF   ,"V,"+lcCdStr )
ENDCASE
SET ORDER TO TAG &lcMTag
SELECT(lnAlias)
RETURN IIF(llChkCod,llChkRetF,lnRetVal)


*!*************************************************************
*! Name      : lfvMajor
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Calls     : 
*!             Procedures : StyCopy.SPX
*!             Functions  : gfStyBrw(),lfGetInfo(),lfCopyStyle()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvMajor

*--HDM B602700,1 03/25/1999[Start] added this variable to tell
*--                        whether the user browsed or not
PRIVATE llCmFrmBrw
STORE .F. TO llCmFrmBrw

*--HDM B602700,1 03/25/1999[End]

*B606391,1 BWA 08/18/2002 Empty the user arrays that hold the wrong memo data.[START]
IF !EMPTY(laUsrFields)
  FOR J = 1 TO ALEN(laUsrFields,1)
    IF ALLTRIM(laUsrFields[J,3]) = "M"
      laUsrFields[J,6] = SPACE(0)
    ENDIF
  ENDFOR
ENDIF
*B606391,1 BWA 08/18/2002.[END]


llCopySt = .F.

*B802445,1 Start,Added check that you should press enter or tab to validate.
IF llBrowse OR LASTKEY()=13 OR LASTKEY()=9
  *--Ok start validate.
ELSE
  RETURN
ENDIF
*B802445,1 End.

IF llbrowse OR !EMPTY(lcMajor) .OR. !SEEK(lcMajor,'STYLE')
  IF llbrowse OR ATC('?',lcMajor) <> 0
    llbrowse = .F.
    lcMajor = gfStyBrw('M',"","",.F.)
    llCmFrmBrw = .T.
  ENDIF
  IF !SEEK(lcMajor,'STYLE') .AND. !EMPTY(lcMajor)
    lcMajor = gfStyBrw('M',lcMajor,lcNonMjr,.T.)

    *E301209,4 Validate returnd segments.
    FOR I=1 TO ALEN(laSegInfo,1)
      IF laSegInfo[I,1] = 'M' AND laSegInfo[I,3] $ 'TQGZDC' AND ;
         !EMPTY( SUBSTR(lcMajor,laSegInfo[I,4],laSegInfo[I,5]) )
        IF !lfGetCode(laSegInfo[I,3],laSegInfo[I,8],SUBSTR(lcMajor,laSegInfo[I,4],laSegInfo[I,5]),laSegInfo[I,6],.T.,.T.)
          WAIT WINDOW 'Selected code is invalid!'
          _CUROBJ=OBJNUM(lcMajor)          
          RETURN
        ENDIF
      ENDIF
    ENDFOR
    *E301209,4 End.

    llCmFrmBrw = .T.
    SELECT (lcColorFil)
    =lfActClrBr()
  ENDIF
    *--HDM B602700,1 03/25/1999[Start] check if the major is empty and user browsed
    *--                          means ESC pressed so go back to major field
    *IF EMPTY(lcMajor)

    IF (EMPTY(lcMajor) .AND. llbrowse) .OR. (llCmFrmBrw .AND. EMPTY(lcMajor))
      _CUROBJ=OBJNUM(lcMajor)
    *--HDM B602700,1 03/25/1999[END]
      llCmFrmBrw = .F.
      RETURN
    ENDIF
    SELECT STYLE
	IF !EMPTY(lcMajor) AND !SEEK(lcMajor)
 	  lascrmode   =.F.
      lascrmode[4]=.T.
	  lcfrmstyle  = SPACE(lnstylewid)
	  llloop      = .T.
      SHOW GET lcMajor 
  	  *--Would you like to copy information from another style.,\<Yes;\<No
      GO TOP IN STYLE
      *E301586,1 ABD - Cheak if Sp module is install we can import styles from it. [Begin]
      *B604408,1 - WAB (Start)- check if the files exist
      *IF  llSPInstld
      IF llSPInstld AND llOpnPdm
      *B604408,1 - WAB (End)
	    DO WHILE llloop AND !EOF('Style')
          *-- Text message : Do you want to copy or import information from another Style?
          *-- Message No.  : 42222.
          *-- Text Button  : \<Copy;\?\<Import;\?\<No
          *-- Button No.   : 36017.
          lnChoise = gfModalGen('QRM42222B36017','DIALOG','style')
          DO CASE
            *-- Copy the style.
            CASE lnChoise = 1
              lcfrmstyle   = SPACE(lnstylewid)
              lcsty100cttl = 'Copy Information From Style'
              llImprtSty   = .F.
              DO (gcScrDir+gcWinAppl+"\StyCopy.SPX")
              llNotImprt = .T.
              IF LASTKEY()=27
                EXIT
              ENDIF
            *-- Import the style
            CASE lnChoise = 2
                DO CASE
                  CASE !EMPTY(lcMajor) .AND. !SEEK('PROTOTYPE'+lcMajor,"PDMLOG")
                    *-- Text message : This style does not exist in the Pdm file ; Cannot proceed.
                    *-- Message No.  : 36176.
                    *-- Text Button  : Ok
                    *-- Button No.   : 00000
                    = gfModalGen('QRM36176B00000','ALERT','style')
                    llNotImprt = .F.
                    EXIT
                  CASE !EMPTY(lcMajor) .AND. SEEK('PROTOTYPE'+lcMajor,"PDMLOG") ;
                                       .AND. PDMLOG.cImpstatus <> 'O'
                    *-- Text message : This style is already imported ; Cannot proceed.
                    *-- Message No.  : 36177.
                    *-- Text Button  : OK
                    *-- Button No.   : 0000
                    = gfModalGen('QRM36177B00000','ALERT','style')
                    llNotImprt = .F.
                    EXIT
                  CASE !EMPTY(lcMajor) .AND. SEEK('PROTOTYPE'+lcMajor,"PDMLOG") ;
                                       .AND. PDMLOG.cImpstatus = 'O'
                    *-- Need to get the fabric if found in the fabric file
                    lnOldAlias = SELECT(0)
                    SELECT PDMstyle
                    lcOldOrder = Order()
                    SET ORDER To Cstymajor
                    *604320,4  AMH Fix case of style doesn't have primary fabric [START]
                    *IF SEEK(lcMajor,'PDMStyle') .AND. !SEEK(PDMstyle.Fabric,'FABRIC')
                    IF SEEK(PADR(lcMajor,19),'PDMStyle') .AND. !EMPTY(PDMstyle.Fabric) .AND. !SEEK(PDMstyle.Fabric,'FABRIC')
                    **604320,4 AMH [END]
                      *-- Text message : Primary fabric for the currently imported 
                      *-- Text message : style doesn't exist in the fabric file. cannot proceed.
                      *-- Message No.  : 42223.
                      *-- Text Button  : OK
                      *-- Button No.   : 0000
                      = gfModalGen('QRM42223B00000','ALERT')
                      llNotImprt = .F.
                      EXIT
                    ELSE
                      llImprtSty   = .T.
                      llNotImprt   = .T. 
                      EXIT
                    ENDIF
                    SET ORDER TO &lcOldOrder In PDMStyle
                    SELECT (lnOldAlias)
                ENDCASE
              *-- Exit from the loop.
            CASE lnChoise = 3
              llImprtSty = .F.
              llNotImprt = .T.
              EXIT
          ENDCASE  
        ENDDO
        *604320,4 AMH Fix case of empty style file [START]
	    DO WHILE llloop AND EOF('Style')
          *-- Text message : Do you wish to import the details of this style?
          *-- Message No.  : 42222.
          *-- Text Button  : \<Copy;\?\<Import;\?\<No
          *-- Button No.   : 36017.
          lnChoise = gfModalGen('QRM00000B42002','DIALOG','style',.F.,'Do you wish to import the details of this style?')
          DO CASE
            *-- Import the style
            CASE lnChoise = 1
                DO CASE
                  CASE !EMPTY(lcMajor) .AND. !SEEK('PROTOTYPE'+lcMajor,"PDMLOG")
                    *-- Text message : This style does not exist in the Pdm file ; Cannot proceed.
                    *-- Message No.  : 36176.
                    *-- Text Button  : Ok
                    *-- Button No.   : 00000
                    = gfModalGen('QRM36176B00000','ALERT','style')
                    llNotImprt = .F.
                    EXIT
                  CASE !EMPTY(lcMajor) .AND. SEEK('PROTOTYPE'+lcMajor,"PDMLOG") ;
                                       .AND. PDMLOG.cImpstatus <> 'O'
                    *-- Text message : This style is already imported ; Cannot proceed.
                    *-- Message No.  : 36177.
                    *-- Text Button  : OK
                    *-- Button No.   : 0000
                    = gfModalGen('QRM36177B00000','ALERT','style')
                    llNotImprt = .F.
                    EXIT
                  CASE !EMPTY(lcMajor) .AND. SEEK('PROTOTYPE'+lcMajor,"PDMLOG") ;
                                       .AND. PDMLOG.cImpstatus = 'O'
                    *-- Need to get the fabric if found in the fabric file
                    lnOldAlias = SELECT(0)
                    SELECT PDMstyle
                    lcOldOrder = Order()
                    SET ORDER To Cstymajor
                    IF SEEK(PADR(lcMajor,19),'PDMStyle') .AND. !EMPTY(PDMstyle.Fabric) .AND. !SEEK(PDMstyle.Fabric,'FABRIC')
                      *-- Text message : Primary fabric for the currently imported 
                      *-- Text message : style doesn't exist in the fabric file. cannot proceed.
                      *-- Message No.  : 42223.
                      *-- Text Button  : OK
                      *-- Button No.   : 0000
                      = gfModalGen('QRM42223B00000','ALERT')
                      llNotImprt = .F.
                      EXIT
                    ELSE
                      llImprtSty   = .T.
                      llNotImprt   = .T. 
                      EXIT
                    ENDIF
                    SET ORDER TO &lcOldOrder In PDMStyle
                    SELECT (lnOldAlias)
                ENDCASE
              *-- Exit from the loop.
            CASE lnChoise = 2
              llImprtSty = .F.
              llNotImprt = .T.
              EXIT
          ENDCASE  
        ENDDO
        *604320,4 AMH [END]

        *-- We will go to select mode again if we import style was not
        *-- Found in the PDMlog file or import brfore that.
        IF !llNotImprt
          lcMajor     = SPACE(lnStyleWid)
          laScrMode   =.F.
          laScrMode[1]=.T.
          _CUROBJ     =OBJNUM(lcMajor)
          llCmFrmBrw  = .F.
          RETURN
        ENDIF
        
        *E301586,1 ABD - Else for if statement. [Begin]
      ELSE
        *E301586,1 ABD - [End]
	    DO WHILE llloop AND !EOF('Style')
          KEYBOARD '{TAB}'
          IF gfModalGen('QRM42005B42002','DIALOG','style')=1
            lcfrmstyle   = SPACE(lnstylewid)
            lcFrom_Tl    = 'Copy from :'
            lcsty100cttl = 'Copy Information From Style'
            DO (gcScrDir+gcWinAppl+"\StyCopy.SPX")
            IF LASTKEY()=27
              EXIT
            ENDIF  
          ELSE
            EXIT
          ENDIF  
        ENDDO
        *E301586,1 ABD - End for if statement. [Begin]
      ENDIF
      *E301586,1 ABD - [End]
	  IF !EMPTY(lcfrmstyle)
        WAIT WINDOW 'Copying Style information...' NOWAIT
		llCopySt = .T.
		llloop   = .F.
  		=lfcopystyle(lcMajor,lcfrmstyle)
    	lascrmode   =.F.
        lascrmode[4]=.T.
		llacsdefwr = .T.
	  ELSE
	  	SELECT STYLE
  		GO BOTTOM
		IF !EOF()
		  SKIP
        ENDIF
	  ENDIF

  	  STORE .T. TO llallcolors,glUpdated
      llstymode   = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
      llAllScales = llMScale
      *-- HDM B602631,1 Assign The Default ware house case POS[Start]
      lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
      *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
      *-- HDM B602631,1 Assign The Default ware house case POS[End]
      laData[1] = lcMajor
      IF !laScrMode[2]
      =lfgetinfo()
   	  *lnlastfold  = lnactfolder
	  *lnactfolder = 1
	  ENDIF
      lcCdDesc=''
      FOR I=1 TO ALEN(laSegInfo,1)
        IF laSegInfo[I,1] = 'M' AND laSegInfo[I,3] $ 'TQGZDC' AND ;
           !EMPTY( SUBSTR(lcMajor,laSegInfo[I,4],laSegInfo[I,5]) )
          = lfGetCode(laSegInfo[I,3],laSegInfo[I,8],SUBSTR(lcMajor,laSegInfo[I,4],laSegInfo[I,5]),laSegInfo[I,6],.T.,.F.)
          laData[3]=ALLTRIM(laData[3])+IIF(EMPTY((laData[3])),'','-')+ALLTRIM(lcCdDesc)
        ENDIF
      ENDFOR
      SHOW GETS
      =lfRefresh()
      WAIT CLEAR
    ENDIF
*  ENDIF

  *--No Non Majors found in structure.
  IF EMPTY(lcfrmstyle) AND llNoNMjrs AND lnNonSeg = 0 AND;
     !EMPTY(lcMajor) AND SEEK(lcMajor,'STYLE')
    laData[1]   = lcMajor
    lascrmode   = .F.
    lascrmode[2]= .T.
    llstymode   = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
    *-- HDM B602631,1 Assign The Default ware house case POS[Start]
    lcWareCode  = IIF(lcSysType='P',lawareh[1,1],'All   ')
    *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
    *-- HDM B602631,1 Assign The Default ware house case POS[End]
    lnlastfold  = lnactfolder
    *E301152,1 Hesham (Start)
        lnactfolder = 1
    *lnactfolder = 2
    IF !laScrMode[2]
      =lfGetInfo()
    ENDIF
    *E301152,1 Hesham (End)
    SHOW GETS
    IF llMultiwh
      *_CUROBJ = OBJNUM(pbWHouse)
      _CUROBJ = OBJNUM(ibFolder[1])
    ELSE
      *--HDM Cut & Sold Folder no longer the active folder in screen startup[start]
      * _CUROBJ = OBJNUM(ibFolder[2])
      _CUROBJ = OBJNUM(ibFolder[1])
      *--HDM Cut & Sold Folder no longer the active folder in screen startup[End]
    ENDIF  
  ENDIF
ENDIF

*B039660,1 NNA 02/14/2006 (Begin) update the Style class fields with a default value 'A' if we're in the Add mode
IF ASCAN(laEvntTrig , PADR('USRFELDS',10)) <> 0 AND laScrMode[4]
  =gfDoTriger('ICSTYLE',PADR('USRFELDS',10))
ENDIF
*B039660,1 (End)

RETURN


*!*************************************************************
*! Name      : lfvNonMjr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate non major part.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetInfo()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvNonMjr

llbrowse = llbrowse OR '?' $ lcNonMjr

lcNonMjr = IIF(llbrowse,'?',lcNonMjr)
IF EMPTY(lcMajor) AND EMPTY(lcNonMjr) 
  RETURN
ENDIF

IF llBrowse OR (LASTKEY()=13 AND !MDOWN()) 
  
  llCanBrow = .T.
  lcNonToSek = lcNonMjr
  IF LEN(RTRIM(lcNonMjr)) < lnColorWid
    lcNonToSek = ''
    lcNxtSep = ''
    FOR I=1 TO ALEN(laSegInfo,1)
      IF laSegInfo[I,1]='N'
        lcSeg = SUBSTR(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5])
        IF EMPTY(lcSeg)
          EXIT
        ENDIF
        IF I < ALEN(laSegInfo,1)
          lcNxtSep = SUBSTR(lcNMjrPt,laSegInfo[I,4]+laSegInfo[I,5]-lnStyleWid-1,1)
          lcNxtSep = IIF(lcNxtSep='X','',lcNxtSep) 
        ENDIF 
        lcNonToSek = lcNonToSek+lcSeg+lcNxtSep
      ENDIF
    ENDFOR
  ENDIF
  lcNonToSek = IIF(EMPTY(lcNonToSek),PADR(lcNonMjr,lnColorWid),lcNonToSek)
  IF SEEK(PADR(lcMajor,lnStyleWid)+lcSepart+lcNonToSek,'STYLE')
    llCanBrow = .F.
  ELSE  
     *B802445,1 Start,if invalid major entered, browse all.
     *=SEEK(PADR(lcMajor,lnStyleWid),'STYLE')  
     IF !SEEK(PADR(lcMajor,lnStyleWid),'STYLE')  
       lcMajor = ' '
       GO TOP
     ENDIF  
     *B802445,1 End.
  ENDIF
  llbrowse = .F.

  IF llCanBrow
    IF !EMPTY(lcMajor)
      lcNonMjr = gfStyBrw('N',lcMajor,lcNonMjr,.F.)
      IF !EMPTY(lcNonMjr) AND SUBSTR(lcNonMjr,1,1) = '*'
        *B602855,1 seek on major alwayes.
        *=SEEK(lcMajor)
        =SEEK(lcMajor,'STYLE')
        *B602855,1 End.
      ENDIF
      *B602855,1 End.
    ELSE
      lcItemCd = gfStyBrw('I',lcMajor+lcSepart+lcNonMjr,'',.F.)
      lcMajor  = SUBSTR(lcItemCd ,1,lnstylewid)
      lcNonMjr = SUBSTR(lcItemCd ,lnstylewid+2,lnColorwid)
    ENDIF
    SELECT (lcColorFil)
    =lfActClrBr()
  ENDIF
  IF EMPTY(lcNonMjr)
    _CUROBJ=OBJNUM(lcNonMjr)
    RETURN
  ENDIF

  IF LEN(RTRIM(lcNonMjr))<lnColorWid
    FOR I=1 TO ALEN(laSegInfo,1)
      IF laSegInfo[I,1]='N'
        lcSeg = SUBSTR(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5])
        IF EMPTY(lcSeg)
          lcSeg=STRTRAN(lcSeg,' ','*') 
        ENDIF
        lcNonMjr = STUFF(lcNonMjr,laSegInfo[I,4]-lnStyleWid-1,laSegInfo[I,5],lcSeg)
      ENDIF
    ENDFOR
  ENDIF

  *lcNMjrRlt  = laNMjr[1,2]
  *laNMjr[1,2] = 'lcNmjrRlt'

  SELECT STYLE
  *--HESHAM
  llallcolors = (!EMPTY(lcMajor) AND (EMPTY(lcNonMjr) OR SUBSTR(lcNonMjr,1,1)='*'))
*  llallcolors = (!EMPTY(lcMajor) AND (EMPTY(lcNonMjr) OR RIGHT(lcNonMjr,1)='*'))  
  *--HESHAM  

  *--HDM
  lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
  lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
  =gfRltFld(lcCodGet , @laNMjr , 'COLOR')
  =lfRefresh(lcWinCh0)
  *--HDM


  laData[1]   = lcMajor
  lascrmode   = .F.
  lascrmode[2]= .T.
  llstymode   = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
  *-- HDM B602631,1 Assign The Default warehouse case POS[Start]
  lcWareCode  = IIF(lcSysType='P',lawareh[1,1],'All   ')
  *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
  *-- HDM B602631,1 Assign The Default warehouse case POS[End]
  lnlastfold  = lnactfolder
  *E301152,1 Hesham (Start)
  lnactfolder = 1
  *lnactfolder = 2

  IF !laScrMode[2]
    =lfGetInfo()
  ENDIF
  *E301152,1 Hesham (End)

  *C102452,1 HBG 21/11/2001 Get the values of style classification screen's fields for this style[Begin]
  IF llGMATrig
    laData[85]  = STYLE.Dept
    laData[10]  = STYLE.cstygroup
    laData[86]  = STYLE.cstycatg
    laData[87]  = STYLE.cstyclass
    laData[88]  = STYLE.cstysbclas
    laData[89]  = STYLE.cstysbcls2
    laData[90]  = STYLE.cstysbcls3  
    
    IF EMPTY(laData[85])
      STORE 1   TO lnDept,lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
      STORE ' ' TO laData[10],laData[86],laData[87],laData[88],laData[89],laData[90]
    ENDIF
  ENDIF
  *C102452,1 [End]


  SHOW GETS
  
  IF llMultiwh
    *_CUROBJ = OBJNUM(pbWHouse)
    _CUROBJ = OBJNUM(ibFolder[1])
  ELSE
    *--HDM Cut & Sold Folder no longer the active folder in screen startup[start]
    *_CUROBJ = OBJNUM(ibFolder[2])
    *--The genera folder is the active one
    _CUROBJ = OBJNUM(ibFolder[1])
    *--HDM Cut & Sold Folder no longer the active folder in screen startup[End]
  ENDIF  
ENDIF

RETURN



*!*************************************************************
*! Name      : lfvPattern
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate Pattern field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetInfo()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvpattern

*B604432,4 KAM is lascrmod[1] use 'gfvactbrow '[start]
STORE '' TO  lcPat  && assign space to lcPat varaible
*B604913,1 ASH 09/16/2001 (Begin) Fix the bug of infinite loop in pattern field.
*IF laScrMode[1] AND !EMPTY(laData[5])
*  =gfvActBrow('laData[5]') 
*  RETURN
*ENDIF
*B604913,1 ASH 09/16/2001 (End)
*B604432,4 KAM [end]
*B604432,1 KAM declare variable for using in call ariabrow[start]
lcPBrowTitl = "Pattern ID"         && Browse title.
lcTmpFld    = lcBrFields           && Save browse fields.

*B604573,1 WAB (Start) change the browse field to browse 8 charcter from cpatt_no and the revesion code
*lcBrFields = [cpatt_no:10:h="Pattern NO",cMark_ID:10:h="Marker Code",cMark_Desc:35:h="Marker Description"]  &&variable hold fields name use in ariabrow
lcBrFields = [cPatt_No = ALLTRIM(LEFT(cPatt_No,8))+CREV_NO:12:h="Pattern NO",cDesc:35:h="Pattern Description",CREVDESC:35:h="Revision Description"]
*B604573,1 WAB (END) 
*B604862,1 ASH 09/03/2001 (Begin) Fix the bug 'Subscript out of bound' if you pressed ESC from the pattern browse.
*DIMENSION latemp[1]                && array holr the return value from ariabrow
DIMENSION latemp[2]                && array holr the return value from ariabrow
*B604862,1 ASH 09/03/2001 (End)

latemp   = ''                      && assign space for array 
*B604432,1,KAM[end]
*C102077,1 HBG 01/02/2001 Check if enterd Pattern is Found in Pattern
*C102077,1                file or not in case of SP module installed [Begin]
*B604408,1 - WAB (Start)- check if the files exist
*IF !EMPTY(laData[5]) AND llSPInstld 
*B604432,4 KAM use llbrowse variable in condition[start]
*IF !EMPTY(laData[5]) AND llSPInstld AND llOpnPatrn 
IF (!EMPTY(laData[5])  OR llBrowse)AND llSPInstld AND llOpnPatrn 
*B604432,4 KAM [end]
*B604408,1 - WAB (End)
  *B604432,1 KAM put 'PDMPATRN' instead of 'SPPATRAN'
  *SELECT SPPATRN
  SELECT PDMPATRN
  *B604432,1 KAM [end]
  GO TOP 
  IF EOF()
    =gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'No Patterns found')
      *E301586,1 ABD - Remark the next line to leave the imported Pattern if we
      *E301586,1 ABD - Import any style [Begin]      
      *B604432,1 KAM  open statement or remove comment remark [start]
      *laData[5] = SPACE(10)
      laData[5] = SPACE(10)
      *B604432,1 KAM [end]
      *E301586,1 ABD - [End]
  ELSE
    *B604432,1 KAM seek in 'PDMPATRN' instead of 'SPPATRAN'
    *IF !SEEK(laData[5],'SPPATRN')
      *=gfModalGen('QRM00000B00000','DIALOG',.F.,.F.,'This Pattern does not exist')
      *E301586,1 ABD - Remark the next line to leave the imported Pattern if we
      *E301586,1 ABD - Import any style [Begin]      
      *laData[5] = SPACE(10)
      *E301586,1 ABD - [End]
    *B604432,4 KAM use llbrowse in condition to enter in not found or user click brow button[start]
    *IF !SEEK(laData[5],'PDMPATRN')
    *wab  ****Index LEFT(cpatt_no,8)+crev_no
    *IF !SEEK(laData[5],'PDMPATRN') OR llBrowse
    lnFldLen  = LEN(ALLTRIM(laData[5]))

    *C200576,1 ABD - The weidth of the pattern field Should be less tahn or equal 3. [Begin]
    *lcPattern = IIF(lnFldLen>3,PADR(LEFT(laData[5],lnFldLen-2),8)+RIGHT(ALLTRIM(laData[5]),2),laData[5])
    lcPattern = IIF(lnFldLen>=3,PADR(LEFT(laData[5],lnFldLen-2),8)+RIGHT(ALLTRIM(laData[5]),2),laData[5])
    *C200576,1 ABD - [End]

    IF !SEEK(lcPattern,'PDMPATRN') OR llBrowse
    *WAB
      IF llBrowse AND !EMPTY(laData[5])
        lcPaT=laData[5]
      ELSE
        laData[5] = SPACE(10)  && assign space for ladata[5]
      ENDIF
      *B604573,1 WAB (Start)  - the field pattern equal 2 fields in the PDM module cpatt_no and cRev_no
      *llReturn = ariabrow('',lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                    gnBrFsRow2, gnBrFsCol2,'','',;
                    'cPatt_No','laTemp')            && call ariabrow to select or cancel
      llReturn = ariabrow('',lcPBrowTitl, gnBrFsRow1, gnBrFsCol1,;
                    gnBrFsRow2, gnBrFsCol2,'','',;
                    'cPatt_No,cRev_no','laTemp')            && call ariabrow to select or cancel
      *B604573,1 WAB (END) 
                    
      IF llBrowse AND EMPTY(latemp[1])
              laData[5] =lcPat
      ELSE
        *B604573,1 WAB (Start)  - the field pattern equal 2 fields in the PDM module cpatt_no and cRev_no
        *laData[5] =latemp[1]
        *B604862,1 ASH 09/03/2001 (Begin) Don't display spaces between the pattern and the revision on.
        *laData[5] =ALLTRIM(LEFT(laTemp[1],8)+laTemp[2])
        laData[5] =ALLTRIM(LEFT(laTemp[1],8))+laTemp[2]
        *B604862,1 ASH 09/03/2001 (End)
        *B604573,1 WAB (END)
      ENDIF
      SHOW GET laData[5]
      _CUROBJ = OBJNUM(laData[3])          && put the cursor on next object         
      *B604432,4 KAM [end]
    ENDIF
  ENDIF
  *khalid
  llBrowse = .F.
ENDIF
*B604432,1 KAM [start]
lcBrFields = lcTmpFld  && assign the old value to lcBrFields
*B604432,1 KAM [end]
*C102077,1 [End]
IF laScrMode[3] OR laScrMode[4]
  RETURN
ENDIF  

IF laScrMode[3] OR laScrMode[4]
  RETURN
ENDIF  

lnAlias=SELECT()
IF llbrowse .OR. EMPTY(lcMajor)
  IF llbrowse .OR. !EMPTY(laData[5])
    llbrowse = .F.
	SELECT STYLE
	SET ORDER TO TAG stylepat
	IF !EMPTY(laData[5]) AND SEEK(laData[5])
	  lckey = laData[5]
	ELSE
      lckey = ''
	  IF BETWEEN(RECNO(0),1,RECCOUNT())
		GO RECNO(0)
	  ELSE
		GO TOP
	  ENDIF
	ENDIF
	lcPbrowtitl = 'Patterns'
    PRIVATE lcBrFields
	lcbrfields = "PATTERN :15:H='Pattern',"+;
                 "STYLE :17:H='Style',"+;
                 "DESC :45:H='Description'"
	DIMENSION latemp[2]
	latemp = ''
	=ariabrow([lcKey],lcPbrowtitl, gnbrfsrow1, gnbrfscol1,;
			gnbrfsrow2, gnbrfscol2, '','',;
			'STYLE,PATTERN','laTemp')
	IF EMPTY(latemp[2])
        STORE SPACE(lnstylewid) TO lcMajor,laData[1]
        lcNonMjr  = SPACE(lnColorwid) 
		laData[5] = SPACE(10)
		SHOW GET lcMajor
		SHOW GET lcNonMjr  
		SHOW GET laData[5]
		_CUROBJ = OBJNUM(lcMajor)
	ELSE
	  STORE SUBSTR(latemp[1],1,lnStyleWid) TO lcMajor,laData[1] 
      lcNonMjr  = SUBSTR(latemp[1],lnStyleWid+2,lnColorWid)
  	  laData[5] = latemp[2]
  	  llallcolors = .F.
  	  SELECT STYLE
	  SET ORDER TO TAG style

      lascrmode   = .F.
      lascrmode[2]= .T.
      llstymode   = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
      *-- HDM B602631,1 Assign The Default warehouse case POS[Start]
      lcWareCode  = IIF(lcSysType='P',lawareh[1,1],'All   ')
      *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
      *-- HDM B602631,1 Assign The Default warehouse case POS[End]
      lnlastfold  = lnactfolder
      *E301152,1 Hesham (Start)
      lnactfolder = 1
      *lnactfolder = 2
	  SHOW GET laData[5]
	  IF !laScrMode[2]
        =lfGetInfo()
      ENDIF  
      *E301152,1 Hesham (End)
      SHOW GETS

	ENDIF
  ENDIF
  SET ORDER TO TAG STYLE IN STYLE

ENDIF
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfvDesc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Update the non major descreption.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvDesc

IF laData[3] = lcOldValue
  RETURN
ENDIF

IF laScrMode[3] AND llAllColors
  *--This description will be the default for all color,Do you wish to continue? y/n
  IF gfModalGen('TRM42145B42002','DIALOG',+'description'+'|'+lcNMjrTl)=2
    laData[3] = lcOldValue
    RETURN
  ENDIF
ENDIF

lnAlias=SELECT()
SELECT (lcColorFil)
lnSvRc=IIF(!EOF(),RECNO(),0)
GO TOP
IF !EOF()
  SEEK lcMajor+lcNMKey
  laData[4] = IIF(!EMPTY(lcOldValue),;
                STRTRAN(Desc1,ALLTRIM(lcOldValue),ALLTRIM(laData[3])),;
                ALLTRIM(laData[3])+' ' + ALLTRIM(Desc1))

  REPLACE REST WHILE Style=lcMajor+lcNMKey ;
          Desc WITH laData[3],;
          Desc1 WITH IIF(!EMPTY(lcOldValue),;
                STRTRAN(Desc1,ALLTRIM(lcOldValue),ALLTRIM(laData[3])),;
                ALLTRIM(laData[3])+' '+ALLTRIM(Desc1))
ENDIF
IF lnSvRc<>0
  GOTO lnSvRc 
ELSE
  GO TOP
ENDIF
SHOW GET laData[4]
SELECT(lnalias)



*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Get screen objects new information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetOptn(),gfRltFld(),lfvviewloc(),
*!                          lfvCurrncy(),lfActClrBr(),lfGetTmp(),
*!                          lfvViewWh(),lfvSUp(),lfvSDn()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfgetinfo
PRIVATE llPrepak
llPrepak = .F.
SELECT STYLE
SET ORDER TO TAG STYLE
lcNonMjr = IIF(llallcolors,STRTRAN(lcNMjrPt,'X','*'),lcNonMjr)
IF laScrMode[2] AND AT('*',lcNonMjr)=0 AND SUBSTR(Style,lnStyleWid+2,lnColorWid)<>lcNonMjr
  lcNonMjr=' '
ENDIF
lcNonMjr = IIF(EMPTY(lcNonMjr),SUBSTR(Style,lnStyleWid+2,lnColorWid),lcNonMjr)
lcNMKey  = lcSepart+IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)
IF !llCopySt
  SEEK lcMajor+lcNMKey
ENDIF
SCATTER FIELDS &lcScFields MEMO TO laData
*B122822,1 NNA 07/15/2004 (Begin) I hold Zero to the Cost If we are creating new style from another
IF llCopySt
  STORE 0 TO laData[45],laData[46],laData[47],laData[48],laData[49],laData[83]
ENDIF
*B122822,1 NNA (End)

*E301586,1 ABD - If we import any style we will get all information
*E301586,1 ABD - in the PDMstyle file. [Begin]
IF llImprtSty

  SCATTER MEMVAR MEMO
  *-- We will store this filed to ladata to put it on
  *-- the screen olso we will store it to memvar to gather
  *-- it in the color file.

  *B604320,1 ABD Add the 20 character from long description to the short desc.
  STORE LEFT(PDMstyle.Desc1,20)      To laData[3]  ,m.Desc
  *B604320,1 ABD [End]
  
  STORE PDMstyle.Desc1      To laData[4]  ,m.Desc1
  STORE PDMstyle.Pattern    To laData[5]  ,m.Pattern
  STORE PDMstyle.Scale      To laData[6]  ,m.Scale
  STORE PDMstyle.Season     TO laData[8]  ,m.Season
  STORE PDMstyle.cDivision  To laData[9]  ,m.cDivision
  STORE PDMstyle.CstyGroup  To laData[10] ,m.CstyGroup
  STORE PDMstyle.Fabric     To laData[11] ,m.Fabric
  STORE PDMstyle.cStatus    To laData[20] ,m.cStatus

  *-- this style should be Domestic style.
  STORE .T.                 To laData[21] , m.Make
  STORE PDMstyle.PriceA     To laData[24] ,m.PriceA
  STORE PDMstyle.PriceB     To laData[25] ,m.PriceB
  STORE PDMstyle.PriceC     To laData[26] ,m.PriceC
  STORE PDMstyle.nSugRetPri To laData[60] ,m.nSugRetPri
  STORE PDMstyle.MarkA      To laData[54] ,m.MarkA
  STORE PDMstyle.MarkB      To laData[55] ,m.MarkB
  STORE PDMstyle.MarkC      To laData[56] ,m.Markc
  STORE PDMstyle.nMarkRet   To laData[81] ,m.nMarkRet
  
  
  
  *-- Element Cost
  *-- Get the color of style.
  lnAlias = SELECT (0)
  SELECT  PDMSTYLE
  lcOldOrder = ORDER()
  SET ORDER TO Style
  SCAN REST WHILE LEFT(Style,lnStyleWid) = lcMajor
    SELECT (lcColorFil)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE STYLE     WITH PDMSTYLE.STYLE   ,;
            Cstymajor WITH PDMSTYLE.cStyMajor
    SELECT PDMSTYLE
  ENDSCAN
  SET ORDER TO &lcOldOrder IN PDMSTYLE
  SELECT (lnAlias)

ENDIF
*E301586,1 ABD - [End]

laData[1] = lcMajor
SHOW GET lcMajor  DISABLE
SHOW GET lcNonMjr DISABLE
llAllScales =(llMScale AND SUBSTR(lcNonMjr,lnColorWid-2,3)='***')
laData[20]  = IIF(EMPTY(laData[20]),'A',laData[20])
lnstatus    = IIF(laData[20]='A',1,IIF(laData[20]='H',2,3))
lndomestic  = IIF(laData[21],1,2)
lncommiss   = IIF(laData[22] OR (laScrMode[4] AND !llCopySt),1,2)
lnGrade     = IIF(EMPTY(laData[72]),1,INT(VAL(laData[72])))

*B608765,1 TMI 12/17/2008 [START] rewrite the fix B608355 in a non-contradictory way
*                                 it can not be possible that lnDomestic be 2 while laData[21] is .T.  
*B608355,1 NNA 11/17/2007 (Begin) Re initiate ladata[21] that because the previous line changes it to False 
*IF llMFInstld AND OCCURS('PS',gcCmpModules) = 0 AND llMFCost
*  SHOW GET lnDomestic DISABLE
*  laData[21] = .T.
*  IF llPOInstld
*   lnDomestic = 2
*  ELSE
*   lnDomestic = 1
*  ENDIF
*ENDIF
*B608355,1 NNA (End)
*B608765,1 TMI 12/17/2008 [END  ] 

*C102390,1 TMI [Start] Initialize Dyelot flag  with 'Y' for Eileen Fisher
IF ASCAN(laEvntTrig , PADR('INITDYE',10)) <> 0
  =gfDoTriger('ICSTYLE','INITDYE')
ENDIF        
*C102390,1 TMI [End  ]

lldye       = IIF((EMPTY(laData[23]) OR laData[23]='N'),.F.,.T.)

*E300803,1 SWK 05/25/98 Variable used for check if inventory style or not
llInvSty    = IIF(laScrMode[4],.T.,IIF(!laData[75],.F.,.T.))
*E300803,1 (End)

*B802091,1 Read taxable from setup in new style and not copy.
*lnPuTaxabl  =IIF(laData[30],1,2) 
lnPuTaxabl = IIF(laScrMode[4] AND !llCopySt,IIF(llDefTax,1,2),IIF(laData[30],1,2))
laData[30] = (lnPuTaxabl=1) 
*B802091,1 End.
*E300803,1 SWK 05/25/98 Variable used for check if using detail costing
*B608765,1 TMI 12/17/2008 [START] comment this code and move it to the correct place
*IF laScrMode[4] AND !llCopySt
*  llDetCost = IIF((laData[21] AND llMFCost) OR (!laData[21] AND llImpCost) ,.T.,.F.)  
*ELSE
*  llDetCost = laData[80]
*ENDIF
*B608765,1 TMI 12/17/2008 [END  ] comment this code and move it to the correct place
*E300803,1 (End)
*B804271,1 ASH 07/11/2001 (Begin) Don't seek in Fabric file unless the MA module not installed.
IF llMAInstld
*B804271,1 ASH 07/11/2001 (End)
  lcfabric    = IIF(EMPTY(laData[11]),SPACE(20),IIF(SEEK(laData[11],'FABRIC'),fabric.desc,'**'+gcerrortxt+'**'))
*B804271,1 ASH 07/11/2001 (Begin) If MA module not installed.
ELSE
  lcFabric    = SPACE(20)
ENDIF
*B804271,1 ASH 07/11/2001 (End)
  
*laData[23] = IIF(!EMPTY(laData[11]) AND SEEK(laData[11],'Fabric'),fabric.cDye_flg,'N')
laData[72]  = lnGrade
laData[22]  = (lncommiss=1)
laData[23]  =IIF(lldye,'Y','N')
*E300803,1 SWK 05/25/98 get the value of inventory style
laData[75]  =IIF(llInvSty,.T.,.F.)
*E300803,1 (End)
laData[19]  =IIF(EMPTY(laData[19]),'DEFDEF',laData[19])
laData[68]  =IIF(EMPTY(laData[68]),'DEF',laData[68])
J=0
FOR I=1 TO 3
  IF lnGrade<>I
    J=J+1
    laDamQlt[J,1]=laStyGrd[I,1]
    laDamQlt[J,2]=laStyGrd[I,2]
  ENDIF
ENDFOR
lnDamQlt = 1
lcRetSty=IIF(lnDamQlt=1,laData[70],laData[73])
GO TOP IN GL_LINK
IF !EOF('GL_LINK')
  lcGllnk6   = laData[19]
  =gfGLbrowse('03',@lcGllnk6,@lcLinkDesc,0)
  laData[19] = lcGllnk6
  lcGllnk3  = laData[68]
  =gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
  laData[68]=lcGllnk3
  lcLinkDesc = IIF(EMPTY(laData[19]),'',lcLinkDesc)
  lcSlinkcde = IIF(EMPTY(laData[68]),'',lcSlinkcde)
ENDIF

*C102045,1 ABD assi. the default CdicCode to use when add new warehouse. [Begin]
lcDefCod = CDISCCODE
*C102045,1 ABD [End]

= gfwCodePop ( @laCodInfo, "SEASON"   ,"T" )
= gfwCodePop ( @laCodInfo, "CDIVISION","T" )
= gfwCodePop ( @laCodInfo, "CSTYGROUP","T" )
= gfwCodePop ( @laCodInfo, "ROYALTY"  ,"T" )  
= gfwCodePop ( @laCodInfo, "CDISCCODE"  ,"T" )
= gfwCodePop ( @laCodInfo, "CPURCODE"  ,"T" )
*--HDM
lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=gfRltFld(lcCodGet , @laNMjr , 'COLOR')
=lfRefresh(lcWinCh0)

IF llPrepak .AND. lcPrepack <> 'N'
*--HDM
  *IF laScrMode[4]
    ladata[7] = &lcColorFil..PREPAK
  *ELSE
  *  ladata[7] = STYLE.PREPAK
*  ENDIF
ENDIF
*=lfRefresh(lcWinCh0)
*B606876,1 ABD - Display Error in the costing tap of the style screen, this case happen becasue 
*B606876,1 ABD - Variable make still .F. and Should be .T. so the following code will handle this case. [Begin]
DO CASE
  CASE !llPOInstld .AND. llMFInstld
    *-- if Point of Sale is installed we should allow the user 
    *-- to make the style as imported
    IF llMFInstld AND OCCURS('PS',gcCmpModules) <> 0 AND (laScrMode[3] OR laScrMode[4])
      SHOW GET lnDomestic ENABLE
    ENDIF
    
    IF llMFInstld AND OCCURS('PS',gcCmpModules) = 0
      SHOW GET lnDomestic DISABLE
      laData[21] = .T.
      lnDomestic = 1
    ENDIF
  CASE llPOInstld .AND. !llMFInstld
    laData[21] = .F.
    lnDomestic = 2
    SHOW GET lnDomestic DISABLE
ENDCASE
*B606876,1 ABD - [End]

*B608765,1 TMI 12/17/2008 [START] this is the correct place for this code after the update that made for laData[21]
IF laScrMode[4] AND !llCopySt
  llDetCost = IIF((laData[21] AND llMFCost) OR (!laData[21] AND llImpCost) ,.T.,.F.)  
ELSE
  llDetCost = laData[80]
ENDIF
*B608765,1 TMI 12/17/2008 [END  ] this is the correct place for this code after the update that made for laData[21]


*B603875,1 KHM 09/04/2000 (Begin) Check if laData[64] = 0 then default 
*B603875,1                lnTBrk and laData[64] to 1.
*lnTBrk = laData[64]
IF laData[64] = 0
  STORE 1 TO laData[64], lnTBrk
ENDIF
*B603875,1 KHM 09/04/2000 (End)
*--HDM

IF (ALLTRIM(gcContCode) = lcEngland)
  = gfwCodePop ( @laCodInfo, "CTAXCODE","T" )  
  DIME laTermAry[1,2]
  laTermAry[1,1] = 'NTAXRATE'
  laTermAry[1,2] = 'lnTaxPr'  
  =gfRltFld(laData[63],@laTermAry, 'CTAXCODE')
ENDIF

*lccostlbl1 = PROPER(ALLTRIM(IIF(laData[21],lcmslbl1,lcislbl1)))
lccostlbl1 = IIF(laData[21],lcmslbl1,lcislbl1)
lccostlbl2 = IIF(laData[21],lcmslbl2,lcislbl2)
lccostlbl3 = IIF(laData[21],lcmslbl3,lcislbl3)
lccostlbl4 = IIF(laData[21],lcmslbl4,lcislbl4)
lccostlbl5 = IIF(laData[21],lcmslbl5,lcislbl5)

IF !EMPTY(laData[6])
  IF llMScale
    *--HDM B802112,1[Start] Fixing Bug of In-correct scales sorting
    *SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,1) INTO ARRAY laExtSz
    *E301209,4 Read scale depend on setup scale length.
    *SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,1) ORDER BY 1 INTO ARRAY laExtSz
    SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
    *E301209,4 End.
    *--HDM B802112,1[End]
  ENDIF
  IF !llAllScales
    IF llMScale
      =SEEK ('S'+SUBSTR(lcNonMjr,lnColorWid-2,3),'SCALE')
    ELSE
      =SEEK ('S'+laData[6],'SCALE')
	ENDIF
	STORE PADL(ALLTRIM(scale.sz1),5) TO lcsize1,lcSz1
	STORE PADL(ALLTRIM(scale.sz2),5) TO lcsize2,lcSz2
	STORE PADL(ALLTRIM(scale.sz3),5) TO lcsize3,lcSz3
	STORE PADL(ALLTRIM(scale.sz4),5) TO lcsize4,lcSz4
	STORE PADL(ALLTRIM(scale.sz5),5) TO lcsize5,lcSz5
	STORE PADL(ALLTRIM(scale.sz6),5) TO lcsize6,lcSz6
	STORE PADL(ALLTRIM(scale.sz7),5) TO lcsize7,lcSz7
	STORE PADL(ALLTRIM(scale.sz8),5) TO lcsize8,lcSz8
    lcSclDesc='Size:'+SCALE.cScl_Desc
	lnScaleCnt=scale.cnt
    FOR I=1 TO lnScaleCnt
      lcI=STR(I,1)
      DIME lascale[I,2]
      lascale[I,1] = SCALE.Sz&lcI
      lascale[I,2] = I
	ENDFOR
    *E300803,1 SWK 05/25/98 comments next lines
	*=SEEK('P'+laData[6]+laData[7],'SCALE')
	*STORE  scale.pp1 TO lnprpak1,lnPrPak1x
	*STORE  scale.pp2 TO lnprpak2,lnPrPak2x
	*STORE  scale.pp3 TO lnprpak3,lnPrPak3x
	*STORE  scale.pp4 TO lnprpak4,lnPrPak4x
	*STORE  scale.pp5 TO lnprpak5,lnPrPak5x
	*STORE  scale.pp6 TO lnprpak6,lnPrPak6x
	*STORE  scale.pp7 TO lnprpak7,lnPrPak7x
	*STORE  scale.pp8 TO lnprpak8,lnPrPak8x
	*=SEEK ('S'+laData[6],'SCALE')
	*E300803,1 (End)
  ELSE
    lnCntlScl = 1
    STORE ' ' TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8
    *B602846,1 Read Ex scl description.
    *lcsize1 = ' Extended '
    *lcsize2 = 'Size Scale'
    *E301209,4 Read scale depend on setup scale length.
    *=SEEK(SUBSTR(laData[6],1,1),'ScaleHd')
    =SEEK(SUBSTR(laData[6],1,lnScaleLen),'ScaleHd')
    *E301209,4 End.
    lcsize1=SUBSTR(ScaleHd.CSCALEDES,1,10)
    lcsize2=SUBSTR(ScaleHd.CSCALEDES,11,30)
    *B602846,1 End.
    *E301209,4 Read scale depend on setup scale length.
    *=SEEK('S'+SUBSTR(laData[6],1,1),'SCALE')
    =SEEK('S'+SUBSTR(laData[6],1,lnScaleLen),'SCALE')
    *E301209,4 End.
    
    STORE PADL(ALLTRIM(scale.sz1),5) TO lcSz1
    STORE PADL(ALLTRIM(scale.sz2),5) TO lcSz2
    STORE PADL(ALLTRIM(scale.sz3),5) TO lcSz3
    STORE PADL(ALLTRIM(scale.sz4),5) TO lcSz4
    STORE PADL(ALLTRIM(scale.sz5),5) TO lcSz5
    STORE PADL(ALLTRIM(scale.sz6),5) TO lcSz6
    STORE PADL(ALLTRIM(scale.sz7),5) TO lcSz7
    STORE PADL(ALLTRIM(scale.sz8),5) TO lcSz8
    lcSclDesc='Size:'+SCALE.cScl_Desc
  ENDIF
  *E300803,1 SWK 05/25/98 Call the function to calculate totals
  =lfTotPrePak()
  *E300803,1 (End)
ELSE
  STORE '' TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8
  STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
  STORE 0  TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
  STORE 0  TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x
ENDIF

STORE 1 TO lnloc,lnUpc
IF !llmultiwh
  *--Get the default warehouse.
  SELECT WareHous
  GO TOP IN WareHous
  laData[71]=WareHous.cWareCode
  IF llmultiloc
    =lfvviewloc()
  ENDIF 
ELSE
  *B605451,1 ASH 02/25/2002 (Begin) Display a message if the style default location not found in warehouse file.
  IF ASCAN(laDefWhs,laData[71]) = 0 AND !laScrMode[4]
    =gfModalGen('QRM42226B42001','DIALOG')
    laData[71] = ''
  ENDIF
  *B605451,1 ASH 02/25/2002 (End)

  IF lcSysType = 'P'
    lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,1)
  ELSE
    lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,;
                  ASUBSCRIPT(laDefWhs,ASCAN(laDefWhs,laData[71]),1) )
  ENDIF
  IF llmultiloc &&.AND. laScrMode[2]
    =lfvviewloc()
  ENDIF 

ENDIF
SELECT STYLE

*--Calculate the optional line.
=lfGetOptn()
*E300803,1 SWK 05/25/98 Call the function to get the totals
=lfTotPrePak()
*E300803,1 (End)
IF llUpc 
  *B603797,1 (Begin) Open UPC files.
  = gfOpenFile(gcDataDir+'EDICATGH','Account','SH')
  = gfOpenFile(gcDataDir+'EDICATGD','Account','SH')
  = gfOpenFile(gcDataDir+'rcylupc','Style','SH')
  *B603797,1 (End)
  laOpFile[19]= gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
  SELECT STYLEUPC
  *IF ! llAllColors AND SEEK(laData[1]) 
  IF ! llAllColors AND SEEK(lcMajor + lcNMKey)
    I = 0
    
    *--HDM B802112,1[Start] Change Scan Exp. To seek the whole style 
    *--                     as laData[1] containes major name only
    *SCAN WHILE STYLE=laData[1]
*B602831,1 Change FOR to WHILE in the following Scan command.   
*   SCAN FOR STYLE = lcMajor + lcNMKey
    SCAN WHILE STYLE = lcMajor + lcNMKey
*B602831,1 End.
    *--HDM B802112,1[End]
    
       I=I+1   
       DECLARE laUPCNum[I]
       *--HDM Fixing Bug of wrong variable assigning in case of ESS[Start]
       *z=STR(I,1)
       Z=STYLEUPC.SIZE
       
       *lcSz='lcSize'+Z
       IF llAllScales
         *E301209,4 Read scale depend on setup scale length.
         *=SEEK('S'+ SUBSTR(ALLTRIM(STYLE),LEN(ALLTRIM(STYLEUPC.STYLE))-2),'SCALE')
         =SEEK('S'+ SUBSTR(ALLTRIM(STYLE),LEN(ALLTRIM(STYLEUPC.STYLE))-IIF(lnScaleLen=1,2,1)),'SCALE')
         *E301209,4 End.
       ELSE
         =SEEK('S'+ laData[6],'SCALE')
       ENDIF
       lcSz = 'SCALE.SZ&Z'
       *lcSz=IIF(llAllScales,'lcSz'+Z,'lcSize'+Z)
       *--HDM Fixing Bug of wrong variable assigning in case of ESS[End]
       lcSiz=ALLTRIM(EVAL(lcSz))+SPACE(5-LEN(ALLTRIM(EVAL(lcSz))))
       laUPCNum[I]=lcSiz+'=>'+cupcnum1+cupcnum2+cupcnum3
    ENDSCAN
  ELSE
    DECLARE laUPCNum[1]
    laUPCNum[1]='N/A'
  ENDIF
ENDIF

IF laData[21] 
  *B603855,1 AMH zero cost items in case copied style and detile cost Yes [START]
  *IF llCopySt
  IF llCopySt .AND. laData[80]
  *B603855,1 AMH zero cost items in case copied style and detile cost Yes [END  ]
    STORE 0 TO laData[40],laData[41],laData[42],laData[43],laData[44]
  ENDIF

  lnCostVal1=laData[40]
  lnCostVal2=laData[41]
  lnCostVal3=laData[42]
  lnCostVal4=laData[43]
  lnCostVal5=laData[44]
  lnCostPrc2=IIF(lnCostval1=0,0,(lnCostVal2/lnCostVal1)*100)
  lnCostPrc3=IIF(lnCostval1=0,0,(lnCostVal3/lnCostVal1)*100)
  lnCostPrc4=IIF(lnCostval1=0,0,(lnCostVal4/lnCostVal1)*100)
  lnCostPrc5=IIF(lnCostval1=0,0,(lnCostVal5/lnCostVal1)*100)        
ELSE
  *B603855,1 AMH zero cost items in case copied style and detile cost Yes [START]
  *IF llCopySt
  IF llCopySt .AND. laData[80]
  *B603855,1 AMH zero cost items in case copied style and detile cost Yes [END  ]
    STORE 0 TO laData[45],laData[46],laData[47],laData[48],laData[49]
  ENDIF
  IF llMulCurr
    STORE 1 TO lnPrExRate,lnChExRate,lnCurrUnt1,lnCurrUnt2
    STORE SET('CURRENCY',1) TO lcfrnsmbl1,lcfrnsmbl2,lcfrnsmbl3,lcfrnsmbl4,lcfrnsmbl5,lcBaseSmbl

    =lfvCurrncy(laData[27],1,.T.)
    =lfvCurrncy(laData[28],2,.T.)
    *--E301023,1 HDM [START] Get The Actual Data For The Gross Price & Discount
    *-- If We use Multy Currency We Have To Validate The Values
      =lfvGPrice('F')
    *--E301023,1 HDM [END]

  ELSE
    lnCostVal1=laData[45]
    lnCostVal2=laData[46]
    lnCostVal3=laData[47]
    lnCostVal4=laData[48]
    lnCostVal5=laData[49]
    FOR I = 2 TO 5

      lcXno = STR(I,1)
      IF lcIType&lcXno $ 'D'
        
        IF STYLE.NPRCOST&lcXno = STYLE.NICOST&lcXno *100 / STYLE.GROS_PRICE
          IF lnCostVal0 = 0
            lnCostVal0 = STYLE.GROS_PRICE
          ENDIF
          lnCostPrc&lcXno=IIF(lnCostval0=0,0,(lnCostVal&lcXno/lnCostVal0)*100)
        ELSE
          lnCostPrc&lcXno=IIF(lnCostval1=0,0,(lnCostVal&lcXno/lnCostVal1)*100)
        ENDIF
      ELSE
        lnCostPrc&lcXno=IIF(lnCostval1=0,0,(lnCostVal&lcXno/lnCostVal1)*100)
      ENDIF
      SHOW GET lnCostPrc&lcXno
    ENDFOR

    *--E301023,1 HDM [START] Get The Actual Data For The Gross Price & Discount
    *-- If We use Multy Currency We Have To Validate The Values
    lnCostPrc0 = laData[84]
    lnCostVal0 = laData[83]
    *--E301023,1 HDM [END]

  ENDIF
ENDIF
lnTotCst   = lnCostVal1+lnCostVal2+lnCostVal3+lnCostVal4+lnCostVal5
lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[24],2))
*B603713,1 AMH [Start] let ladata[54] bounded by max and min value of its field
*laData[54] = IIF(laData[24]=0 OR lnTotCst=0 ,0,((ROUND(laData[24],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
laData[54] = MAX(MIN(IIF(laData[24]=0 OR lnTotCst=0 ,0,;
                        ((ROUND(laData[24],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
*B603713,1 AMH [End]
lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[25],2))
*B603713,1 AMH [Start] let ladata[55] bounded by max and min value of its field
*laData[55] = IIF(laData[25]=0 OR lnTotCst=0 ,0,((ROUND(laData[25],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
laData[55] = MAX(MIN(IIF(laData[25]=0 OR lnTotCst=0 ,0,;
                        ((ROUND(laData[25],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
*B603713,1 AMH [End]
lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[26],2))
*B603713,1 AMH [Start] let ladata[56] bounded by max and min value of its field
*laData[56] = IIF(laData[26]=0 OR lnTotCst=0 ,0,((ROUND(laData[26],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
laData[56] = MAX(MIN(IIF(laData[26]=0 OR lnTotCst=0 ,0,;
                        ((ROUND(laData[26],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
*B603713,1 AMH [End]
lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[60],2))
*B603713,1 AMH [Start] let ladata[81] bounded by max and min value of its field
*laData[81] = IIF(laData[60]=0 OR lnTotCst=0 ,0,((ROUND(laData[60],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
laData[81] = MAX(MIN(IIF(laData[60]=0 OR lnTotCst=0 ,0,;
                        ((ROUND(laData[60],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
*B603713,1 AMH [End]
laData[2]  = IIF(laScrMode[4],laData[57],laData[2])

*--Create warehouses temp.
IF llMultiwh
  SELECT (lcwarefil)
  GO TOP
  IF EOF() OR (Style<>lcMajor+lcNMKey)
    SELECT STYDYE
    SEEK lcMajor+lcNMKey
    SCAN WHILE Style=lcMajor+lcNMKey FOR EMPTY(dyelot)
      SELECT (lcwarefil)
      APPEND BLANK
      *C102045,1 ABD - Remark the next line and add cdiscode to the 
      *C102045,1 ABD - temp file that hold the warehouses. [Begin]
      *REPLACE Style     WITH lcMajor+lcNMKey,;
              Desc      WITH laData[3],; 
              cWareCode WITH STYDYE.cWareCode,;
              gl_link   WITH STYDYE.Gl_Link,;
              cStatus   WITH 'S'

      REPLACE Style     WITH lcMajor+lcNMKey  ,;
              Desc      WITH laData[3]        ,; 
              cWareCode WITH STYDYE.cWareCode ,;
              gl_link   WITH STYDYE.Gl_Link   ,;
              cStatus   WITH 'S'              ,;
              cdisccode WITH  STYDYE.cDiscCode 
      *C102045,1 ABD [End]
    ENDSCAN
    SELECT (lcwarefil)
    INDEX ON cwarecode TAG cwarecode Unique
    SET ORDER TO       TAG cwarecode
  ENDIF
ENDIF
*--Create style prices temp.
IF llMulCurr
  SELECT (lctmpstypr)
  GO TOP
  IF EOF() OR (Style<>lcMajor+lcNMKey)
    SELECT STYPRICE
    SEEK lcMajor+lcNMKey
    SCAN WHILE Style=lcMajor+lcNMKey
      SCATTER MEMVAR
      SELECT (lctmpstypr)
      APPEND BLANK
      GATHER MEMVAR 
      REPLACE cStatus WITH 'S'
    ENDSCAN
    SELECT (lctmpstypr)
    INDEX ON cCurrCode TAG lctmppr1 Unique
    INDEX ON Style+cCurrCode TAG (lctmpstypr) Unique ADDI
    SET ORDER TO TAG (lctmpstypr)
  ENDIF
ENDIF
*-Create Style Non MAJORS temp file.
SELECT (lcColorFil)
GO TOP
IF EOF() OR Style<>lcMajor
  IF llMScale AND laScrMode[4] AND lnNonSeg=1 AND ALEN(laExtSz)>=1 AND !EMPTY(laExtSz[1])
    SELECT (lcColorFil)
    FOR I=1 TO ALEN(laExtSz)
      APPEND BLANK
      GATHER FROM laData FIELDS &lcScFields MEMO    
      REPLACE Style   WITH lcMajor+lcSepart+laExtSz[I],;
              cStatus WITH 'A'
    ENDFOR
  ELSE
    SELECT * , 'S' AS cstatus FROM STYLE ;
        WHERE STYLE = lcMajor ;
      	INTO DBF (gcWorkDir+lccolorfil)
  ENDIF
  INDEX ON Style TAG colororder
  SET ORDER TO   TAG colororder
ENDIF

IF llCopySt
  GO TOP IN (lcColorFil)
  *B604016,1 MHM 11/14/2000 [START]
  FOR j = 1 TO ALEN(laUsrFields,1)
    IF EMPTY(laUsrFields[J,6])
      *E301586,1 ABD - Cheak First if laUsrFields[J,1]) Is empty or not
      *E301586,1 ABD - Because this made a bug "syntax Error" if compay
      *E301586,1 ABD - Have any user def. fields. [Begin]
      *laUsrFields[J,6] = &laUsrFields[J,1]    
      laUsrFields[J,6] = IIF(EMPTY(laUsrFields[J,1]),'',EVAL(laUsrFields[J,1]))
      *E301586,1 ABD [End]
    ELSE
      EXIT
    ENDIF
  ENDFOR
  *B604016,1 MHM 11/14/2000 [END]
  laData[4]=&lcColorFil..Desc1
ENDIF

=lfActClrBr()

*--Get cut & sold information.

=lfGetTmp()

=lfvViewWh(.T.)

IF laScrMode[2]
  IF !llDown
    =lfvSUp(.F.)
  ELSE
    =lfvSDn(.F.)
  ENDIF
ENDIF

=lfRefresh(lcWinCh0)
llGetInfo = .F.
RETURN

*!*************************************************************
*! Name      : lfClearInfo
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Clear screen objects information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfActClrBr()
*!*************************************************************
*! Passed Parameters  : llAllClear->Flag to Clear variables.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfClearInfo
PARA llAllClear

lnAlias=SELECT()
IF llAllClear
  STORE ''  TO lcLinkDesc,lcSLinkCde,lcfabric,lcRetSty
  STORE ''  TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8,lcNmjrDisc
  STORE ''  TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
  STORE 0   TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
  STORE 0   TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x
  STORE 0   TO lnloc,lnUPC,lnbrrecno,lnTBrk,lnTaxPr,lnDefWhs
  STORE 0.0 TO lncostval1,lncostval2,lncostval3,lncostval4,lncostval5,lncostval0
  STORE 0.0 TO lnCostPrc2,lnCostPrc3,lnCostPrc4,lnCostPrc5,lnCostPrc0
  STORE .F. TO lldye
  STORE .T. TO llInvSty
  lnDamQlt   = 1
  *E300803,1 SWK 05/25/98 Initialize the Manifacturing field with .F.
  laData[21] = .F.
  *laData[21] = .T.
  *E300803,1 (eND)
ENDIF
SELECT (lcTmpStyPr)
ZAP
SELECT (lcwarefil)
ZAP
SELECT (lcColorFil)
ZAP
=lfActClrBr()

SELECT(lnalias)
=lfRefresh(lcWinCh0)
=lfRefresh(lcWinCh1)
RETURN


*!*************************************************************
*! Name      : lfvCpFrmSty
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to validate the style code.
*!             Called from screen STYCOPY
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvcpfrmsty

IF llbrowse OR ( !EMPTY(lcfrmstyle) AND !SEEK(lcfrmstyle,'STYLE') )
  lcfrmstyle = gfStyBrw('M','' ,'',.F.)
  SELECT (lcColorFil)
  =lfActClrBr()
  IF EMPTY(lcfrmstyle)
    _CUROBJ = OBJNUM(lcfrmstyle)
  ENDIF  
ENDIF

*C102466,1 ABD - Restrict adding "Obsolete" colors to style, call
*C102466,1 ABD - Check for the copyied colors if status is 
*C102466,1 ABD - Obsolete for one or more color.[Begin]
IF !EMPTY(lcfrmstyle) 
  *-- Function to Check for the copyied colors if
  *-- status is obsolete for one or more color.
  = lfObsColor ()
ENDIF
*C102466,1 ABD - [End]

llbrowse = .F.
RETURN

*!*************************************************************
*! Name      : lfCopyStyle
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to copy style information to new added style.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcNewStyle --> Copy to style lcNewStyle
*!                      lcFrmStyle --> Copy from style lcFrmStyle
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example            : =lfCopyStyle(Style,lcFrmStyle)
*!*************************************************************
FUNCTION lfcopystyle
PARAMETERS lcnewstyle,lcfrmstyle

WAIT WINDOW 'Copying the style information...' NOWAIT

*--Create style WAREHOUSES temp.
IF llMultiwh
  SELECT STYDYE
  SEEK lcfrmstyle
  SCAN WHILE Style=lcfrmstyle FOR EMPTY(dyelot)
      SELECT (lcwarefil)
      APPEND BLANK
      *C102045,1 ABD - Remark the next replace line and add cdiscode to the 
      *C102045,1 ABD - temp file that hold the warehouses when copy style. [Begin]
      *REPLACE Style     WITH lcNewStyle,;
              Desc      WITH STYDYE.Desc,; 
              cWareCode WITH STYDYE.cWareCode,;
              gl_link   WITH STYDYE.Gl_Link,;
              cStatus   WITH 'A'

      REPLACE Style     WITH lcNewStyle      ,;
              Desc      WITH STYDYE.Desc     ,;
              cWareCode WITH STYDYE.cWareCode,;
              gl_link   WITH STYDYE.Gl_Link  ,;
              cStatus   WITH 'A'             ,;
              cdisccode WITH  STYDYE.cDiscCode 
      *C102045,1 ABD - [End]
  ENDSCAN
  SELECT (lcwarefil)
  INDEX ON cwarecode TAG cwarecode Unique
  SET ORDER TO       TAG cwarecode
ENDIF

*--Create style prices temp.
IF llMulCurr
  SELECT STYPRICE
  SEEK lcfrmstyle
  SCAN WHILE Style=lcfrmstyle
    SCATTER MEMVAR
    SELECT (lctmpstypr)
    APPEND BLANK
    GATHER MEMVAR 
    REPLACE Style   WITH lcNewstyle+lcSepart,;
            cStatus WITH 'A'
  ENDSCAN
  SELECT (lctmpstypr)
  INDEX ON cCurrCode TAG lctmppr1 Unique
  INDEX ON Style+cCurrCode TAG (lctmpstypr) Unique ADDI
  SET ORDER TO TAG (lctmpstypr)
ENDIF

IF llMScale AND laScrMode[4] AND lnNonSeg=1 AND ALEN(laExtSz)>=1 AND !EMPTY(laExtSz[1])
  SELECT (lcColorFil)
  FOR I=1 TO ALEN(laExtSz)
    APPEND BLANK
    GATHER FROM laData FIELDS &lcScFields MEMO    
    REPLACE Style   WITH lcMajor+lcSepart+laExtSz[I],;
            cStatus WITH 'A'
  ENDFOR
ELSE
  SELECT * , 'A' AS cstatus    ;
	FROM STYLE               ;
	WHERE STYLE = lcfrmstyle ;
	INTO DBF (gcWorkDir+lccolorfil)
ENDIF

*C102466,1 ABD - Delete colors that copy and the status is obsolete
*C102466,1 ABD - for this copied style color. [Begin]
SCAN
  *-- Delete Obsolete Color From the copyied style.
  IF ASCAN(laObsColor,SUBSTR(STYLE,lnstylewid+2,lncolorwid)) > 0
    BLANK
    DELETE
    LOOP
  ENDIF
ENDSCAN
*C102466,1 ABD - [End]

INDEX ON Style TAG colororder
SET ORDER TO   TAG colororder

*B606680,1 ABD - Assign Zero to new fields in Trans. [Begin]
*REPLACE ALL STYLE WITH lcNewStyle+SUBSTR(Style,lnStyleWid+1,lnColorWid+1),;
      	stk1   WITH 0, stk2   WITH 0, stk3    WITH 0, stk4   WITH 0,;
      	stk5   WITH 0, stk6   WITH 0, stk7    WITH 0, stk8   WITH 0,;
      	alo1   WITH 0, alo2   WITH 0, alo3    WITH 0, alo4   WITH 0,;
      	alo5   WITH 0, alo6   WITH 0, alo7    WITH 0, alo8   WITH 0,;
      	wip1   WITH 0, wip2   WITH 0, wip3    WITH 0, wip4   WITH 0,;
      	wip5   WITH 0, wip6   WITH 0, wip7    WITH 0, wip8   WITH 0,;
      	ord1   WITH 0, ord2   WITH 0, ord3    WITH 0, ord4   WITH 0,;
      	ord5   WITH 0, ord6   WITH 0, ord7    WITH 0, ord8   WITH 0,;
      	shp1   WITH 0, shp2   WITH 0, shp3    WITH 0, shp4   WITH 0,;
      	shp5   WITH 0, shp6   WITH 0, shp7    WITH 0, shp8   WITH 0,;
        ret1   WITH 0, ret2   WITH 0, ret3    WITH 0, ret4   WITH 0,;
        ret5   WITH 0, ret6   WITH 0, ret7    WITH 0, ret8   WITH 0,;
      	ra1    WITH 0, ra2    WITH 0, ra3     WITH 0, ra4    WITH 0,;
      	ra5    WITH 0, ra3    WITH 0, ra7     WITH 0, ra8    WITH 0,;
      	nwo1   WITH 0, nwo2   WITH 0, nwo3    WITH 0, nwo4   WITH 0,;
      	nwo5   WITH 0, nwo6   WITH 0, nwo7    WITH 0, nwo8   WITH 0,;
      	totstk WITH 0, totalo WITH 0, totwip  WITH 0, totord WITH 0,;
      	totshp WITH 0, totret WITH 0, totra   WITH  0,ntotwo WITH 0,;
        dmgqty WITH 0, ave_cost WITH 0
REPLACE ALL STYLE WITH lcNewStyle+SUBSTR(Style,lnStyleWid+1,lnColorWid+1),;
      	stk1   WITH 0, stk2   WITH 0, stk3    WITH 0, stk4   WITH 0,;
      	stk5   WITH 0, stk6   WITH 0, stk7    WITH 0, stk8   WITH 0,;
      	alo1   WITH 0, alo2   WITH 0, alo3    WITH 0, alo4   WITH 0,;
      	alo5   WITH 0, alo6   WITH 0, alo7    WITH 0, alo8   WITH 0,;
      	wip1   WITH 0, wip2   WITH 0, wip3    WITH 0, wip4   WITH 0,;
      	wip5   WITH 0, wip6   WITH 0, wip7    WITH 0, wip8   WITH 0,;
      	ord1   WITH 0, ord2   WITH 0, ord3    WITH 0, ord4   WITH 0,;
      	ord5   WITH 0, ord6   WITH 0, ord7    WITH 0, ord8   WITH 0,;
      	shp1   WITH 0, shp2   WITH 0, shp3    WITH 0, shp4   WITH 0,;
      	shp5   WITH 0, shp6   WITH 0, shp7    WITH 0, shp8   WITH 0,;
        ret1   WITH 0, ret2   WITH 0, ret3    WITH 0, ret4   WITH 0,;
        ret5   WITH 0, ret6   WITH 0, ret7    WITH 0, ret8   WITH 0,;
      	ra1    WITH 0, ra2    WITH 0, ra3     WITH 0, ra4    WITH 0,;
      	ra5    WITH 0, ra3    WITH 0, ra7     WITH 0, ra8    WITH 0,;
      	nwo1   WITH 0, nwo2   WITH 0, nwo3    WITH 0, nwo4   WITH 0,;
      	nwo5   WITH 0, nwo6   WITH 0, nwo7    WITH 0, nwo8   WITH 0,;
      	intrans1 WITH 0,intrans2 WITH 0,intrans3 WITH 0,intrans4 WITH 0,;
      	intrans5 WITH 0,intrans6 WITH 0,intrans7 WITH 0,intrans8 WITH 0,;
      	totstk WITH 0, totalo WITH 0, totwip  WITH 0, totord WITH 0,;
      	totshp WITH 0, totret WITH 0, totra   WITH  0,ntotwo WITH 0,;
        dmgqty WITH 0, ave_cost WITH 0

*B606680,1 ABD - [End]
*totplan WITH 0,

*B606680,1 ABD - Assign Zero to all this field in case detail costing only. [Begin]
*REPLACE ALL nmcost1  WITH 0, nmcost2  WITH 0, nmcost3  WITH 0, nmcost4  WITH 0, nmcost5 WITH 0,;
      		nicost1  WITH 0, nicost2  WITH 0, nicost3  WITH 0, nicost4  WITH 0, nicost5 WITH 0,;
      		nprcost2 WITH 0, nprcost3 WITH 0, nprcost4 WITH 0, nprcost5 WITH 0,;
      		totcost  WITH 0, MarkA    WITH 0, MarkB    WITH 0, MarkC    WITH 0,GROS_PRICE WITH 0

LOCATE
IF lDetCost
  REPLACE ALL nmcost1  WITH 0, nmcost2  WITH 0, nmcost3  WITH 0, nmcost4  WITH 0, nmcost5 WITH 0,;
              nicost1  WITH 0, nicost2  WITH 0, nicost3  WITH 0, nicost4  WITH 0, nicost5 WITH 0,;
              nprcost2 WITH 0, nprcost3 WITH 0, nprcost4 WITH 0, nprcost5 WITH 0,;
              totcost  WITH 0, MarkA    WITH 0, MarkB    WITH 0, MarkC    WITH 0,GROS_PRICE WITH 0
ELSE
  REPLACE ALL ave_cost WITH TotCost
ENDIF
*B606680,1 ABD - [End]

GO TOP IN (lccolorfil)

*B604016,1 MHM 11/14/2000 we comment this lines accourding to Fix it in global with Bug # 604044 [START]
*B803629,1 MHM 08/29/2000 [START]
*IF !EMPTY(laUsrFields[1,1]) .AND. ASCAN(laEvntTrig , PADR('FIL_VAR',10)) <> 0
*  = gfDoTriger('ICSTYLE','FIL_VAR')
*ENDIF
*B604016,1 MHM 11/14/2000 [END]

=SEEK(&lccolorfil..style,'STYLE')
WAIT CLEAR
SELECT STYLE
SEEK lcFrmStyle
RETURN


*!*******************************************************************
*! Name      : lfGetTmp
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Create a temp file for cut & sold folder which
*!           : hold information for all colors on style level
*!           : or on style/warehouse level.
*!           : This function called only in case of select all colors.
*!********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGetTmp

WAIT WINDOW 'Colecting style Cut & Sold information...' NOWAIT
lcSqlScl  = IIF(llAllScales,laExtSz[lnCntlScl],"")
lcSqlWare = IIF(llMultiwh AND !llStyMode,lcWareCode,"")

lcOut="SUM(Ord1) AS Ord1,SUM(Ord2) AS Ord2,SUM(Ord3) AS Ord3,SUM(Ord4) AS Ord4,SUM(Ord5) AS Ord5,SUM(Ord6) AS Ord6,SUM(Ord7) AS Ord7,SUM(Ord8) AS Ord8,SUM(TotOrd) AS TotOrd,"+;
      "SUM(Stk1) AS Stk1,SUM(Stk2) AS Stk2,SUM(Stk3) AS Stk3,SUM(Stk4) AS Stk4,SUM(Stk5) AS Stk5,SUM(Stk6) AS Stk6,SUM(Stk7) AS Stk7,SUM(Stk8) AS Stk8,SUM(TotStk) AS TotStk,"

lcOut=lcOut+"SUM(Wip1) AS Wip1,SUM(Wip2) AS Wip2,SUM(Wip3) AS Wip3,SUM(Wip4) AS Wip4,SUM(Wip5) AS Wip5,SUM(Wip6) AS Wip6,SUM(Wip7) AS Wip7,SUM(Wip8) AS Wip8,SUM(TotWip) AS TotWip,"+;
      "SUM(Alo1) AS Alo1,SUM(Alo2) AS Alo2,SUM(Alo3) AS Alo3,SUM(Alo4) AS Alo4,SUM(Alo5) AS Alo5,SUM(Alo6) AS Alo6,SUM(Alo7) AS Alo7,SUM(Alo8) AS Alo8,SUM(TotAlo) AS TotAlo,"+;
      "SUM(Shp1) AS Shp1,SUM(Shp2) AS Shp2,SUM(Shp3) AS Shp3,SUM(Shp4) AS Shp4,SUM(Shp5) AS Shp5,SUM(Shp6) AS Shp6,SUM(Shp7) AS Shp7,SUM(Shp8) AS Shp8,SUM(TotShp) AS TotShp,"+;
      "SUM(Ret1) AS Ret1,SUM(Ret2) AS Ret2,SUM(Ret3) AS Ret3,SUM(Ret4) AS Ret4,SUM(Ret5) AS Ret5,SUM(Ret6) AS Ret6,SUM(Ret7) AS Ret7,SUM(Ret8) AS Ret8,SUM(TotRet) AS TotRet,"+;
      "SUM(nWo1) AS nWo1,SUM(nWo2) AS nWo2,SUM(nWo3) AS nWo3,SUM(nWo4) AS nWo4,SUM(nWo5) AS nWo5,SUM(nWo6) AS nWo6,SUM(nWo7) AS nWo7,SUM(nWo8) AS nWo8,SUM(nTotWo) AS nTotWo,"+;
      "SUM(Ra1) AS Ra1,SUM(Ra2) AS Ra2,SUM(Ra3) AS Ra3,SUM(Ra4) AS Ra4,SUM(Ra5) AS Ra5,SUM(Ra6) AS Ra6,SUM(Ra7) AS Ra7,SUM(Ra8) AS Ra8,SUM(Totra) AS TotRa"

*--HDM B602514,1Incorrect calculation of totinv[Start]


IF llMscale
  *B602831,1 Change the SQL to be faster and commend the old one.
  *IF llAllColors .AND. llAllScales
  *  SELECT &lcOut;
  *       FROM STYLE ;
  *       WHERE cStyMajor = PADR(lcMajor,19);
  *       AND SCALE = lcSqlScl;
  *       INTO DBF (gcWorkDir+lctpstyle)
  *ELSE
  *--HDM B802053,1 03/25/1999[Start] In Case of ESS We have to make sure that
  *--    scale is not in the sum operation as we ar in All Scales Mode
  *  IF !llAllColors .AND. llAllScales
  *    SELECT &lcOut;
  *         FROM STYLE ;
  *         WHERE SUBSTR(ALLTRIM(STYLE.STYLE),1,LEN(ALLTRIM(STYLE.STYLE))-3)=lcMajor+lcNMKey;
  *         AND Scale = lcSqlScl;
  *         INTO DBF (gcWorkDir+lctpstyle) 
  *  ELSE
  *--HDM B802053,1 03/25/1999[End]
  *    SELECT &lcOut;
  *         FROM STYLE ;
  *        WHERE style = PADR(lcMajor+lcNMKey,19) AND Scale = lcSqlScl; 
  *         INTO DBF (gcWorkDir+lctpstyle) 
  *  ENDIF
  *ENDIF

  SELECT &lcOut;
    FROM STYLE ;
   WHERE Style = lcMajor+lcNMKey AND Scale = lcSqlScl ; 
    INTO DBF (gcWorkDir+lctpstyle) 

ELSE

*B602831,1 Change the SQL to be faster and commend the old one.
*  SELECT &lcOut;
*       FROM STYLE ;
*       WHERE Style = lcMajor+lcNMKey AND Scale = lcSqlScl ; 
*       INTO DBF (gcWorkDir+lctpstyle) 

  SELECT &lcOut;
       FROM STYLE ;
      WHERE Style = lcMajor+lcNMKey ; 
       INTO DBF (gcWorkDir+lctpstyle) 
ENDIF

*--HDM B602514,1Incorrect calculation of totinv[End]

*B119967,1 ABD - Fix bug that the total inventory fields at the cut and sold folder 
*B119967,1 ABD - Show incorrect fingers in case the warehouse code starts with space. [Begin]
*lcSqlWare = PADR(ALLTRIM(lcSqlWare),16)
lcSqlWare = PADR(lcSqlWare,6)
*B119967,1 ABD - [End]

*--HDM B602378,1[START]
*lc   = lcSqlWare
*B602831,1 Alltrim the variable only in it was empty.
*lc  = ALLTRIM(lcSqlWare)
lc   = IIF(EMPTY(lcSqlWare),ALLTRIM(lcSqlWare),lcSqlWare)
*B602831,1 End.
*--HDM B602378,1[END]

lcOf = gcWorkDir+lctpstydye

*--HDM B602514,1Incorrect calculation of totinv[Start]
*SELECT &lcOut;
*     FROM STYDYE ;
*     WHERE cWareCode+Dyelot=lc AND Style IN ;
*     (SELECT Style ;
*        FROM STYLE ;
*       WHERE Style=lcMajor+lcNMKey AND Scale = lcSqlScl  ); 
*     INTO DBF (lcOf) 

*B602831,1 Change the SQL to be faster.
IF llMscale
*  IF !llAllColors
*    SELECT &lcOut;
*         FROM STYDYE ;
*         WHERE cWareCode+Dyelot = lc AND Style = lcMajor + lcNMKey; 
*         INTO DBF (lcOf)
*  ELSE
*    SELECT &lcOut;
*         FROM STYDYE ;
*         WHERE cWareCode + Dyelot = lc AND Style IN ;
*         (SELECT Style ;
*            FROM STYLE ;
*           WHERE Style=lcMajor+lcNMKey AND Scale = lcSqlScl  ); 
*         INTO DBF (lcOf) 
*  ENDIF

  IF llMultiwh AND !llStyMode
    lnS = lnstylewid+1+lnColorWid-2
    SELECT &lcOut;
       FROM STYDYE ;
      WHERE cWareCode+Style+Dyelot = PADR(lc,6)+lcMajor+lcNMKey AND ;
            Dyelot=SPACE(10) AND SUBSTR(Style,lnS,3)=lcSqlScl ;
       INTO DBF (lcOf)

  ELSE
    SELECT * FROM (lctpstyle) INTO DBF (lcOf)  
  ENDIF

ELSE  && Not extended size scale.

  IF llMultiwh AND !llStyMode
*    SELECT &lcOut;
*         FROM STYDYE ;
*         WHERE cWareCode+Dyelot=lc AND Style IN ;
*         (SELECT Style ;
*            FROM STYLE ;
*           WHERE Style=lcMajor+lcNMKey AND Scale = lcSqlScl  ); 
*         INTO DBF (lcOf) 

     SELECT &lcOut;
          FROM STYDYE ;
         WHERE cWareCode+Style+Dyelot = PADR(lc,6)+lcMajor+lcNMKey AND Dyelot=SPACE(10) ;
          INTO DBF (lcOf)
  ELSE
    SELECT * FROM (lctpstyle) INTO DBF (lcOf)
  ENDIF
ENDIF
*B602831,1 End.

*--HDM B602514,1Incorrect calculation of totinv[End]
lcSqlWare=LEFT(lcSqlWare,6)

IF RECCOUNT(lcTpStyle)=0
  SELECT (lcTpStyle)
  APPEND BLANK
ENDIF
IF RECCOUNT(lcTpStyDye)=0
  SELECT (lcTpStyDye)
  APPEND BLANK
ENDIF

SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
GO TOP
IF !llstymode
  laData[2] = IIF(SEEK(STYLE.Style+lcSqlWare,'STYDYE'),STYDYE.Ave_Cost,0)
*B606503,1 ASH 11/07/2002 (Begin) Retrieve the style cost in case of ALL warehouses.
ELSE
  laData[2]  = Style.Ave_cost
*B606503,1 ASH 11/07/2002 (End)    
ENDIF
WAIT CLEAR
RETURN

*:*************************************************************

*##T#-#-# ToolBar functions.

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Cancel/close procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfGetInfo()
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lpClsScr

lnAlias=SELECT()

IF laScrMode[3]
  SELECT (lcWareFil)  
  DELETE ALL FOR cStatus='A'
  REPLACE ALL cStatus WITH 'S'
  SELECT (lcColorFil)
  DELETE ALL FOR cStatus='A'
  laScrMode[3]=.F.
  laScrMode[2]=.T.
*B602831,1 Comment out lfgetinfo() since it already called before.
  llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
  lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
* =lfGetInfo()
  laScrMode[3]=.T.
  laScrMode[2]=.F.
  *C102452,1 HBG 21/11/2001 Reset the values of style classification screen's fields [Begin]
  IF llGMATrig
    STORE 1 TO lnDept,lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
  ENDIF
  *C102452,1 [End]  
ENDIF
*IF EMPTY(laData[1])
   SELECT STYLE
*  GO TOP
*ELSE
*  SELECT(lnAlias)
*ENDIF
RETURN

*!*************************************************************
*! Name      : lpTopScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Top record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpTopScr

SELECT STYLE
GO TOP
lcMajor = SUBSTR(Style,1,lnStyleWid)
IF llAllColors
  lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
ELSE
  lnNMEnd  = AT('*',lcNonMjr)
  IF lnNMEnd=0
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ELSE
    lcNonMjr = STRTRAN(lcNonMjr,SUBSTR(lcNonMjr,1,lnNMEnd-1),;
                                SUBSTR(Style,lnStyleWid+2,lnNMEnd-1))
  ENDIF
ENDIF
STORE .T. TO llGetInfo
llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
*-- HDM B602631,1 Assign The Default warehouse case POS[Start]
lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
*lcWareCode = IIF(lcSysType='P',laData[71],'All   ')

lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=lfRefresh(lcWinCh0)

*-- HDM B602631,1 Assign The Default warehouse case POS[End]
RETURN


*!*************************************************************
*! Name      : lpBtmScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Bottom record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpBtmScr

SELECT STYLE
GO BOTTOM
lcMajor = SUBSTR(Style,1,lnStyleWid)
IF llAllColors
  lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
ELSE
  lnNMEnd  = AT('*',lcNonMjr)
  IF lnNMEnd=0
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ELSE
    lcNonMjr = STRTRAN(lcNonMjr,SUBSTR(lcNonMjr,1,lnNMEnd-1),;
                                SUBSTR(Style,lnStyleWid+2,lnNMEnd-1))
    lcNMKey  = lcSepart+IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)
    =SEEK(lcMajor+lcNMKey)
    DO WHILE Style=lcMajor+lcNMKey
      SKIP -1
    ENDDO
    SKIP
    lcNonMjr = STRTRAN(lcNonMjr,SUBSTR(lcNonMjr,1,lnNMEnd-1),;
                                SUBSTR(Style,lnStyleWid+2,lnNMEnd-1))
  ENDIF
ENDIF  
STORE .T. TO llGetInfo
llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
*-- HDM B602631,1 Assign The Default warehouse case POS[Start]
lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
*lcWareCode = IIF(lcSysType='P',laData[71],'All   ')

lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=lfRefresh(lcWinCh0)

*-- HDM B602631,1 Assign The Default warehouse case POS[End]
RETURN


*!*************************************************************
*! Name      : lpNxtScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Next record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpNxtScr

SELECT STYLE
IF ATC('*',lcNonMjr) > 0
  SEEK lcMajor+lcNMKey
  lnEntNonML=LEN(LEFT(lcNonMjr,ATC('*',lcNonMjr)-1))
ELSE
  lnEntNonML=lnColorWid
ENDIF
*SKIP -1 
SEEK lcMajor+lcNMKey
lcVal = LEFT(Style,lnStyleWid+1+lnEntNonML)
LOCATE REST FOR LEFT(Style,lnStyleWid+1+lnEntNonML) > lcVal

lcMajor = SUBSTR(Style,1,lnStyleWid)
IF llAllColors
  lcNonMjr = STRTRAN(lcNMjrPt,'X','*')
ELSE
  lnNMEnd  = AT('*',lcNonMjr)
  IF lnNMEnd=0
    lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  ELSE
    lcNonMjr = STRTRAN(lcNonMjr,SUBSTR(lcNonMjr,1,lnNMEnd-1),;
                                SUBSTR(Style,lnStyleWid+2,lnNMEnd-1))
  ENDIF
ENDIF
STORE .T. TO llGetInfo
llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
*-- HDM B602631,1 Assign The Default warehouse case POS[Start]
lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
*lcWareCode = IIF(lcSysType='P',laData[71],'All   ')

lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=lfRefresh(lcWinCh0)

*-- HDM B602631,1 Assign The Default warehouse case POS[End]
RETURN


*!*************************************************************
*! Name      : lpPrvScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Previus record in Navigation pb.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpPrvScr

SELECT STYLE
lnNMEnd  = AT('*',lcNonMjr)
IF lnNMEnd=0
  lcMajor  = SUBSTR(Style,1,lnStyleWid)
  lcNonMjr = SUBSTR(Style,lnStyleWid+2,lnColorWid)
ELSE
  SKIP
  DO WHILE Style=lcMajor+lcNMKey
    SKIP -1
  ENDDO
  lcMajor  = SUBSTR(Style,1,lnStyleWid)
  lcNonMjr = STRTRAN(lcNonMjr,SUBSTR(lcNonMjr,1,lnNMEnd-1),;
                              SUBSTR(Style,lnStyleWid+2,lnNMEnd-1))
ENDIF
STORE .T. TO llGetInfo
llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
*-- HDM B602631,1 Assign The Default warehouse case POS[Start]
lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
*lcWareCode = IIF(lcSysType='P',laData[71],'All   ')

lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=lfRefresh(lcWinCh0)

*-- HDM B602631,1 Assign The Default warehouse case POS[End]
RETURN



*:*************************************************************
*! Name     : lpDelScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose  : Delete the style/color.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpDelScr

lnAlias=SELECT()
SELECT STYLE
=gfObj_lock(.F.)
DIMENSION lasource[1],latarget[1]
STORE ' ' TO lasource,latarget
STORE 1 TO lsSource,lnCnt

lnSepMins = lfvGtSclSp()
*--HDM[Start]
=lfGetInfo()
*--HDM[End]
SELECT (lcColorFil)
SEEK lcMajor+lcNMKey
SCAN WHILE Style=lcMajor+lcNMKey
  lcNMjCd=SUBSTR(Style,lnStyleWid+2,IIF(llMScale,lnColorWid-lnSepMins,lnColorWid))
  IF ASCAN(lasource,lcNMjCd)=0
    DIMENSION lasource[lnCnt]
    laSource[lnCnt]=lcNMjCd
    lnCnt=lnCnt+1
  ENDIF 
ENDSCAN

IF ALEN(laSource)>1
  *B603797,1 (Begin) When Pressing Move All in the mover, check all colors not the first one.
  *=gfMover(@lasource,@latarget,"Select Style non majors for deletion",.T.,'lfvClrChk')
  =gfMover(@lasource,@latarget,"Select Style non majors for deletion",.T.,'lfvClrChk',.T.)
  *B603797,1 (End)
  llchk = .F.
  FOR i = 1 TO ALEN(latarget)
	IF !EMPTY(latarget[I])
	  llchk = .T.
	  EXIT
	ENDIF
  ENDFOR

  IF ! llchk
    *--Nothing to delete.
    =gfModalGen('QRM42006B42001','DIALOG')
     SELECT(lnAlias)
	RETURN
  ENDIF
ELSE
  latarget = lasource
  IF !lfvclrchk(1)
    SELECT(lnAlias)
	RETURN
  ENDIF
ENDIF

*--Confirm your Deletion.','\<Yes;\<No.
IF gfModalGen('QRM42007B42002','DIALOG') = 1
  *--Accept deletion, Start Delete...
  =lfAcptDel()

  *C038291,3  TMI [Start] if Add colors are deleted then delete this style from styspec file for "Vijay"
  *B129334,1 NNA 08/29/2005 (Start) use the Same Trigger For David Luke Also to Delete a style from the CSTPRICE Custom file
  IF ASCAN(laEvntTrig,'DELSTY') <> 0
    =gfDoTriger('ICSTYLE','DELSTY')
  ENDIF
  *B129334,1 NNA (END)
  *C038291,3  TMI [End  ] 

  SELECT STYLE
  SCATTER MEMVAR FIELDS &lcscfields BLANK
  llstymode  = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
  *-- HDM B602631,1 Assign The Default warehouse case POS[Start]
  lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
  *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
  *-- HDM B602631,1 Assign The Default warehouse case POS[End]
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GETS
  _CUROBJ = OBJNUM(lcMajor)
ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfvGtSclSp
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function returns the lenth of Scale with Separator.
*!*************************************************************
*! Returns            : len of Scale 3 or 4 if separator exist.
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvGtSclSp

IF llMScale
  llStruOp=gfOpenFile(gcDataDir+'ICISTRU','Segno','SH')
  SELECT ICISTRU
  LOCATE FOR cItemRecty='U' AND cISegType='S'
  IF FOUND()
    SKIP -1
    lnSepMins = IIF(!lSegEndMaj AND !EMPTY(cISegSepr),4,3)
  ELSE
    lnSepMins = 3
  ENDIF
  IF USED('ICISTRU') AND llStruOp
    USE IN ICISTRU
  ENDIF
ELSE
  lnSepMins = 0
ENDIF
RETURN (lnSepMins)


*!*************************************************************
*! Name      : lfvClrChk
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Check color for deletion.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnOption -> mover option no.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvclrchk
PARAMETERS lnOption

IF lnOption=3 OR lnOption=4
  RETURN
ENDIF  

SELECT (lcColorFil)
lcSeekKey=IIF(lnOption=2,lcMajor,lcMajor+lcSepart+laSource(lssource))

*B040114,1  TMI [Start] Check that if a style has transactions 
PRIVATE llSoCntrct,llSoLines,lnRecno
STORE .F. TO llSoCntrct,llSoLines

IF !USED('STYINVJL')
  =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
ENDIF
=SEEK(lcSeekKey,'STYINVJL')
SELECT STYINVJL
LOCATE REST WHILE STYLE+CWARECODE+CSESSION+DTOS(DTRDATE)+CTRCODE+STR(LINENO,6) = lcSeekKey ;
              FOR CTRTYPE $ '345678'
llHasTrns = FOUND()

*-If the style has no entries in the styinvjl file then we need to check if it has RA/PO contracts/SO contracts
IF !llHasTrns
  =SEEK(lcSeekKey,'STYLE')
  DO CASE
  CASE style.totord = 0
    *- Check if style has order contracts
    SELECT ORDLINE
    lnRecno = RECNO('ORDLINE')
    IF SEEK(lcSeekKey,'ORDLINE')
      LOCATE REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcSeekKey ;
              FOR CORDTYPE = 'O'
      llSoLines = FOUND()
      IF !llSoLines
        =SEEK(lcSeekKey,'ORDLINE')
        LOCATE REST WHILE STYLE+DTOS(COMPLETE)+CORDTYPE+ORDER+STORE+STR(LINENO,6) = lcSeekKey ;
                FOR CORDTYPE = 'C'
        llSoCntrct = FOUND()
      ENDIF
    ENDIF
    IF BETWEEN(lnRecno,1,RECCOUNT('ORDLINE'))
      GO (lnRecno)
    ENDIF
  ENDCASE

  *- Open Posln file
  IF !llSoLines .AND. !llSoCntrct .AND. ;
    !USED('POSLN')
    =gfOpenFile(gcDataDir+'POSLN','POSLNS','SH')
  ENDIF
ENDIF
*B040114,1  TMI [End  ] 

*B040114,3  TMI [Start] 
*SEEK(lcSeekKey)
=SEEK(lcSeekKey,lcColorFil)
SELECT &lcColorFil
*B040114,3  TMI [End  ] 
SCAN WHILE Style=lcSeekKey
  =SEEK(&lcColorFil..Style,'STYLE')
  DO CASE
    CASE style.totord <> 0
      *--Quantity on order. Cannot delete.
      =gfModalGen('QRM42008B42001','DIALOG')
      RETURN .F.
    
      *B040114,1  TMI [Start] consider if the stock in -ve
    *CASE lllink_gl .AND. style.totstk > 0
    CASE lllink_gl .AND. style.totstk <> 0
      *B040114,1  TMI [End  ] 
      *--Quantity in inventory. Please adjust the inventory before delete.
      =gfModalGen('QRM42009B42001','DIALOG')
      RETURN .F.
    
      *B040114,1  TMI [Start] consider if the stock is negative
    *CASE !lllink_gl .AND. style.totstk > 0
    CASE !lllink_gl .AND. style.totstk <> 0
      *B040114,1  TMI [End  ] 
      *--Quantity in inventory. Are you sure you want to delete?','\<Yes;\<No.
      IF gfModalGen('QRM42010B42002','DIALOG') = 2
	    RETURN .F.
        *B040114,1  TMI [Start] prevent delete style if it has transactios other than '129'
      ELSE      
        IF llHasTrns
          =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|transactions')
          RETURN .F.
        ENDIF
        *B040114,1  TMI [End  ] 
      ENDIF 
    CASE style.totwip <> 0
      *--Quantity in WIP. Please cancel all WIP first.
      =gfModalGen('QRM42011B42001','DIALOG')
	  RETURN .F.

	*B040114,1  TMI [Start] Do not delete a style that has transactions
	CASE STYLE.TOTSTK = 0 .AND. llHasTrns
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|transactions')
      RETURN .F.	
      *B040114,1  TMI [End  ] 
    
    *B040114,3  TMI [Start] Add more checks    
    *- check style has return autherization     
    CASE STYLE.TOTRA <> 0
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|return authorization(s)')
      RETURN .F.	
    
    *- This style has lines in ORDLINE file with cordtype = 'O'    
    CASE llSoLines
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|sales order(s)')
      RETURN .F.	
    
    *- This style has SO- contracts 
    CASE llSoCntrct    
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|sales order contract(s)')
      RETURN .F.	
    
    *- This style has lines in posln file with PO with type 'P'
    *B607775 ASH 08/28/2006 (Begin) Check if the PO module installed.
    *CASE SEEK(lcSeekKey+'P','POSLN')
    CASE llMFInstld AND SEEK(lcSeekKey+'P','POSLN')
    *B607775 ASH 08/28/2006 (End) 
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|purchase order(s)')
      RETURN .F.	

    *- This style has PO contracts      
    *B607775 ASH 08/28/2006 (Begin) Check if the PO module installed.
    *CASE SEEK(lcSeekKey+'C','POSLN')
    CASE llPOInstld AND SEEK(lcSeekKey+'C','POSLN')
    *B607775 ASH 08/28/2006 (End)
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|purchase order contract(s)')
      RETURN .F.	
      
    *- This style has lines in CUTTKTL file
    *B607775 ASH 08/28/2006 (Begin) Check if the MF module installed.
    *CASE SEEK(lcSeekKey,'CUTTKTL')
    CASE llMFInstld AND SEEK(lcSeekKey,'CUTTKTL')
    *B607775 ASH 08/28/2006 (End)
      =gfModalGen('QRM42265B42001','DIALOG',lcSeekKey+'|cut ticket(s)')
      RETURN .F.	
    
   ENDCASE
   *B040114,3  TMI [End  ]
   
   *B603268,1 Start, Check WIP at warehouse level (case of inter location).
   IF llMultiwh AND STYLE.TotWip = 0 
     SELECT STYDYE
     lnSTDYRec = IIF(EOF(),0,RECNO())
     =SEEK(&lcColorFil..Style) 
     LOCATE REST WHILE Style = &lcColorFil..Style FOR TotWip <> 0
     llSDWipFnd = FOUND()
     IF lnSTDYRec <> 0
       GOTO lnSTDYRec
     ENDIF
     IF llSDWipFnd
       *--Quantity in WIP. Please cancel all WIP first.
       =gfModalGen('QRM42011B42001','DIALOG')
  	   RETURN .F.
     ENDIF     
     SELECT (lcColorFil)     
   ENDIF
   *B603268,1 End.
   *B603797,1 (Begin) Check if the current style\color hase UPCs.
   *B603899,1 (Begin) Make sure UPC is installed before other checckings.
   *IF !lfUPCCheck()
   IF llUpc AND !lfUPCCheck()
   *B603899,1 (End)
     SELECT (lcColorFil)     
     *--Style\Color ..... has UPCs. Cannot delete.
     lcNonM = SUBSTR(STYLE,LEN(lcMjrPct)+2,LEN(lcNMjrPt))
     =gfModalGen('QRM42218B42001','DIALOG',lcMjrTtl+"\"+lcNMjrTl+": "+ALLTRIM(lcMajor)+"\"+ALLTRIM(lcNonM ))
     RETURN .F.
   ENDIF
   SELECT (lcColorFil)
   *B603797,1 (End)
   
ENDSCAN
RETURN .T.


*!*************************************************************
*! Name      : lfAcptDel
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Accept deletion of the style colors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfacptdel

IF llMScale AND lnNonSeg=1 AND ALEN(laExtSz)>=1 AND !EMPTY(laExtSz[1])
 =ACOPY(laExtSz,latarget)
ENDIF

*--No Non Majors found in structure.
IF llNoNMjrs
  DIME latarget[1]
  latarget[1]=''
ENDIF

lntocont = IIF(llallcolors,ALEN(latarget),1)
lcColor  =''
*- Check the style component item type.
lctype=' '
FOR lnx = 1 TO 5
  z=STR(lnx,1)
  IF (laData[21] AND lcmtype&z = 'S') OR (!laData[21] AND lcitype&z = 'S')
	lctype = z
    EXIT
  ENDIF
ENDFOR

llOpnShst = !USED('ICSTYHST')
IF llOpnShst
  =gfOpenFile(gcDataDir+'ICSTYHST','Styhst','SH')
ENDIF

   
SELECT (lcColorFil)
FOR i = 1 TO lntocont
  WAIT WINDOW "Deleting Data For The "+IIF(llallcolors,"Selected Style Colors...","Current Style Color...") NOWAIT

  *-- Go to the style+color in the main style file to get some
  *-- information that can be used in deleteing in other files.
  lcColor = IIF(llallcolors , latarget(i), SUBSTR(lcNMKey,2,LEN(lcNMKey)-1))

  SELECT (lcColorFil)
  SEEK lcMajor+lcSepart+lccolor
  SCAN WHILE Style=lcMajor+lcSepart+lccolor

    *1)--Delete style cost sheet in any.
    SELECT BOM
    SET ORDER TO TAG BomItem
    IF SEEK(lctype+lcMajor)
	  LOCATE REST WHILE Typ+Item=lctype+lcMajor ;
	          FOR LIKE(STRTRAN(ITEM,'*','?'),&lcColorFil..Style)
      IF FOUND()
        *HDM B602122,1 *--Check if the color used in another style[START]
        
        IF SEEK(SUBSTR(CITMMAJOR,1,lnstylewid)+lcSepart+lccolor,'STYLE')

          *--Style : "+style+" is a component in a another style Cost Sheet. Cannot delete.
          =gfModalGen('TRM42012B42001','DIALOG',ALLTRIM(&lcColorFil..Style))	
          SELECT BOM
          SET ORDER TO TAG Bom
          SELECT (lcColorFil)
          LOOP
        ENDIF
        *HDM B602122,1 *--Check if the color used in another style[END]
      ENDIF
	ENDIF
    SELECT BOM
    SET ORDER TO TAG Bom
    IF SEEK(lcMajor)
      IF lmaterial
        LOCATE REST WHILE cItmMajor=lcMajor FOR !lmaterial
      ENDIF
      *E300803,1 SWK 05/25/98 Call TraceKey global function.
      *-- Call TraceKey global function.
      *DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial .AND. ;
                        SUBSTR(CITMMASK,1,LEN(ALLTRIM(lcMajor+lcSepart+lccolor)))=ALLTRIM(lcMajor+lcSepart+lccolor)      
      DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial .AND. ;
                        SUBSTR(CITMMASK,1,LEN(ALLTRIM(lcMajor+lcSepart+lccolor)))=ALLTRIM(lcMajor+lcSepart+lccolor) AND ;
                        gfTraceKey('BOM',lcMajor,'D')
      *E300803,1(End)
    ENDIF 


    *2)--If the syetem is setup to use style packs then delete the
    *----style information from the style packs line file.
    IF llpackuse
      laOpFile[18]=gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLNST','SH')
   	  SELECT spck_lin
      IF SEEK('P'+lcMajor+lcSepart+lccolor)
        *E300803,1 SWK 05/25/98 Call TraceKey global function.
        *-- Call TraceKey global function.
        *BLANK REST FOR Type + Style = 'P'+lcMajor+lcSepart+lccolor
        *BLANK REST FOR (Type + Style = 'P'+lcMajor+lcSepart+lccolor) AND ;
                        gfTraceKey('SPCK_LIN','P'+lcMajor+lcSepart+lccolor,'D')
         BLANK REST FOR (Type + Style = 'P'+lcMajor+lcSepart+lccolor) AND ;
                        gfTraceKey('SPCK_LIN','P'+Account+Pack_ID+lcMajor+lcSepart+lccolor,'D')
        *E300803,1(End)                
   	    =SEEK(SPACE(1))
   	    DELETE REST WHILE  TYPE = SPACE(1)

      ENDIF
  	  IF SEEK('S'+lcMajor+lcSepart+lccolor)
        *E300803,1 SWK 05/25/98 Call TraceKey global function.
        *-- Call TraceKey global function.
        *BLANK REST FOR Type + Style = 'S'+lcMajor+lcSepart+lccolor
        BLANK REST FOR (Type + Style = 'S'+lcMajor+lcSepart+lccolor) AND;
                       gfTraceKey('SPCK_LIN','S'+Account+Pack_ID+lcMajor+lcSepart+lccolor,'D')
        *E300803,1(End)               
    	=SEEK(SPACE(1))
        DELETE REST WHILE  TYPE = SPACE(1)
   	  ENDIF
      IF USED('spck_lin') AND laOpFile[18]
        USE IN spck_lin
      ENDIF
    ENDIF
    *3)-- Delete all style UPC nummber for the current style color.
    IF llUpc AND USED('STYLEUPC') AND SEEK(lcMajor+lcSepart+lccolor,'StyleUPC')
      *B603797,1 (Begin) Remark the following lines and 
      *B603797,1         remove the style records to rcylupc  to be reused agian.
  	  *SELECT styleupc
      *E300803,1 SWK 05/25/98 Call TraceKey global function.
      *-- Call TraceKey global function.
      *BLANK REST FOR (Style = lcMajor+lcSepart+lccolor)
      *BLANK REST FOR (Style = lcMajor+lcSepart+lccolor) AND ;
                     gfTraceKey('STYLEUPC',lcMajor+lcSepart+lccolor,'D')
      *E300803,1(End)               
	  *=SEEK(SPACE(19))
      *DELETE REST WHILE STYLE = SPACE(19)
      SELECT styleupc
      SCAN WHILE style+size = lcMajor+lcSepart+lccolor
        SCATTER MEMVAR MEMO
        INSERT INTO rcylupc FROM MEMVAR
        *:B605121,1 HIA 11/25/2001 [Begin]
        SELECT RCYLUPC
        REPLACE RCYLUPC.Desc       WITH STYLE.Desc       ,;
                RCYLUPC.Scale      WITH STYLE.Scale      ,;
                RCYLUPC.PriceA     WITH STYLE.PriceA     ,;
                RCYLUPC.Start      WITH STYLE.Start      ,;
                RCYLUPC.nSugRetPri WITH STYLE.nSugRetPri ,;
                RCYLUPC.Fabric     WITH STYLE.Fabric     ,;
                RCYLUPC.Content1   WITH STYLE.Content1   ,;
                RCYLUPC.TotCost    WITH STYLE.TotCost    
                
        SELECT styleupc        
        *:B605121,1 HIA 11/25/2001 [End]
        =gfTraceKey('STYLEUPC',lcMajor+lcSepart+lccolor,'D')
        *B804054,1 KAM comment blank statement because blank issue append blank 
        *B804054,1 so pointer in file will move in file[start]
        *BLANK
        *B804054,1 KAM [end]
        DELETE
      ENDSCAN
      *B603797,1 (End)
    ENDIF
	*4)--Delete all records in the style dyelot file all dyelots
	*----and all warehouse records for the current style color.
	IF SEEK(lcMajor+lcSepart+lccolor,'StyDye')
  	  SELECT stydye
      *E300803,1 SWK 05/25/98 Call TraceKey global function.
      *-- Call TraceKey global function.
      *BLANK REST FOR (Style = lcMajor+lcSepart+lccolor)
      *YMA
	  *BLANK REST FOR (Style=lcMajor+lcSepart+lccolor) AND ;
	                 gfTraceKey('STYDYE',lcMajor+lcSepart+lccolor,'D')
	   *MAN              
       *BLANK REST FOR (Style=lcMajor+lcSepart+lccolor) 
       BLANK REST FOR (style+cwarecode+dyelot=lcMajor+lcSepart+lccolor) 
       
	   =gfTraceKey('STYDYE',lcMajor+lcSepart+lccolor,'D')	                 
	  *E300803,1(End)               
	  =SEEK(SPACE(19))
	  *MAN
	  *DELETE REST WHILE STYLE = SPACE(19)
	  DELETE REST WHILE style+cwarecode+dyelot = SPACE(19)
	ENDIF

    *5)--Delete the style foreign prices. 
	IF SEEK(lcMajor+lcSepart+lccolor,'STYPRICE')
  	  SELECT STYPRICE
      *E300803,1 SWK 05/25/98 Call TraceKey global function.
      *-- Call TraceKey global function.
	  *BLANK REST FOR (Style=lcMajor+lcSepart+lccolor)
	  *YMA
	  *BLANK REST FOR (Style=lcMajor+lcSepart+lccolor) AND ;
	             gfTraceKey('STYPRICE',lcMajor+lcSepart+lccolor,'D')
       BLANK REST FOR (Style=lcMajor+lcSepart+lccolor) 
	   =gfTraceKey('STYPRICE',lcMajor+lcSepart+lccolor,'D')	             
	  *E300803,1(End)           
	  =SEEK(SPACE(19))
	  DELETE REST WHILE STYLE = SPACE(19)
	ENDIF

    *6)--Delete Style History.
    SELECT ICSTYHST
    IF SEEK(lcMajor+lcSepart+lccolor)
      *E300803,1 SWK 05/25/98 Call TraceKey global function.
      *-- Call TraceKey global function.
      *DELETE REST WHILE Style=lcMajor+lcSepart+lccolor 
      *YMa
      *DELETE REST WHILE Style=lcMajor+lcSepart+lccolor ;
             FOR gfTraceKey('ICSTYHST',lcMajor+lcSepart+lccolor,'D')
       DELETE REST WHILE Style=lcMajor+lcSepart+lccolor 
       =gfTraceKey('ICSTYHST',lcMajor+lcSepart+lccolor,'D')
       
      *E300803,1(End)       
    ENDIF
  
	*7)--Delete the style record in the main style file.
	SELECT STYLE
	IF SEEK(lcMajor)
      lnFrstRec  = RECNO()
      lnLastRec  = RECNO() 
  	  SCAN REST WHILE Style = lcMajor
        lnFrstRec  = MIN(RECNO(),lnFrstRec)
        lnLastRec  = MAX(RECNO(),lnLastRec)
  	  ENDSCAN
  	  IF SEEK(lcMajor+lcSepart+lccolor)
        lnRecToDel = RECNO()
        IF lnFrstRec = lnRecToDel AND lnLastRec <> lnFrstRec
          GO lnLastRec
          SCATTER MEMVAR MEMO
          GO lnFrstRec
          GATHER MEMVAR MEMO
          GO lnLastRec
        ENDIF
        =RLOCK()
        *E300803,1 SWK 05/25/98 Call TraceKey global function.
        *-- Call TraceKey global function.
    	=gfTraceKey('STYLE',lcMajor+lcSepart+lccolor,'D')
        *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
        IF ASCAN(laEvntTrig , PADR('STYLE_D',10)) <> 0
          =gfDoTriger('ICSTYLE',PADR('STYLE_D',10))
        ENDIF
	    *C200597,1  TMI [End  ] 
    	*E300803,1(End)
	    BLANK
	    DELETE
        UNLOCK
  	  ENDIF
	ENDIF

  
    SELECT (lcColorFil)
  ENDSCAN
ENDFOR

IF llOpnShst
  USE IN ICSTYHST
ENDIF

*C200235,1 ABD - If customer Cathy Daniel delete style from Aria27 
*C200235,1 ABD - delete the style from the Old System. [Begin]
IF ASCAN(laEvntTrig , PADR('DELFROLD',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('DELFROLD',10))
ENDIF      
*C200235,1 ABD - [End]


*--If all colors was deleted for this style so delete the notepad also.
IF !SEEK(lcMajor,'STYLE')
  *--Delete Bom.
  SELECT BOM
  SET ORDER TO TAG Bom
  =SEEK(lcMajor)
  LOCATE REST WHILE cItmMajor=lcMajor FOR !lmaterial
  IF FOUND()
    *E300803,1 SWK 05/25/98 Call TraceKey global function.
    *-- Call TraceKey global function.
    *DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial
    *YMA
    *DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial AND ;
           gfTraceKey('BOM',lcMajor,'D')
    DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial AND ;
           gfTraceKey('BOM',EVAL(KEY()),'D')
           
    *E300803,1(End)       
  ENDIF  
  *--Delete style notepad.
  *E301077,7 Open a notepad file.
  =gfOpenFile(gcDataDir+'NotePad','NotePad','SH')

  IF SEEK('F'+lcMajor,'NotePad')
	SELECT notepad
    *E300803,1 SWK 05/25/98 Call TraceKey global function.
    *-- Call TraceKey global function.
	=gfTraceKey('NOTEPAD','F'+lcMajor,'D')
	*E300803,1(End)
	BLANK
	=SEEK(SPACE(1))
	DELETE
  ENDIF
ENDIF
WAIT CLEAR
RETURN
*:*************************************************************
*! Name     : lpSavScr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose  : Save/Update P/O.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpSavScr

*E300803,1 SWK 05/25/98 Save the screen data to the ladata array
laData[75] =IIF(llInvSty,.T.,.F.)
laData[80] =llDetCost
IF !llMulCurr
  laData[27] = gcbasecurr
  laData[28] = gcbasecurr
ENDIF
*E300803,1 (End)

*B122006,1 MHM 03/30/2004 in case of EX. Size check llCupdate[Start]
IF !EMPTY(laUsrFields[1,1]) 
  llCupdate = .T.
ENDIF
*B122006,1 [End]

lnAlias=SELECT()
IF !glUpdated AND !llCupdate AND laScrMode[3]
  *--Nothing to Save.
  =gfModalGen('TRM42013B42001','DIALOG')
  RETURN
ENDIF

*B120791,1 MHM 12/30/2003 Define variable to check array[Start]
*--lnChanged ===>1 initial value for copying user defined array offirst color to another array
*--lnChanged ===>2 user defined field array not like data in Style file
*--lnChanged ===>3 case to go out of checking cases

lnChanged = 1
*B120791,1 MHM 12/30/2003 [End]
*B123627,1 MHM 07/29/2004 Define variable to check Color and array Changed[Start]
lcFstClor=''
DIMENSION laChgUsrFd[ALEN(laUsrFields,1)]
STORE .F. TO laChgUsrFd
*B123627,1 MHM 07/29/2004 [End]

*B606680,1 ABD - Define new variables and needed fields. [Begin]
IF llCopySt
  PRIVATE lcNMjrFlds, laNMjrFlds
  DIMENSION laNMjrFlds[1,34]
  lcNMjrFlds = "STYLE     ,STATUS    ,DESC      ,DESC1     ,NMCOST1   ,NMCOST2   ,"+;
               "NMCOST3   ,NMCOST4   ,NMCOST5   ,NICOST1   ,NICOST2   ,NICOST3   ,"+;
               "NICOST4   ,NICOST5   ,NPRCOST2  ,NPRCOST3  ,NPRCOST4  ,NPRCOST5  ,"+;
               "TOTCOST   ,AVE_COST  ,NSTKVAL   ,PRICEA    ,PRICEB    ,PRICEC    ,"+;
               "MARKA     ,MARKB     ,MARKC     ,NSUGRETPRI,NMARKRET  ,GROS_PRICE,"+;
               "DISC_PCNT ,CPRICECUR ,CDUTYCUR"
ENDIF
*B606680,1 ABD - [End]

STORE .T. TO llCSave,llCUpdate,llUpdated,llShow
IF EMPTY(laData[6])
  *--You have to enter a scale code. Cannot update.
  =gfModalGen('TRM42014B42001','DIALOG')	
  STORE .F. TO llShow,llCSave
  _CUROBJ = OBJNUM(laData[6])
  RETURN
ENDIF

*--If the multiple warehouse feature is available.
IF llMultiWh AND llAllColors
  *-In add mode for all colors & the default warehouse was empty.
  IF EMPTY(laData[71]) .AND. (laScrMode[4] .OR. llacsdefwr)
	*--You have to set a default warehouse for this style.
    =gfModalGen('TRM42015B42001','DIALOG')	
    STORE .F. TO llShow,llCSave
    _CUROBJ = OBJNUM(pbwhouse)
	RETURN
  ENDIF
ENDIF

*C102452,1 HBG 21/11/2001 If user didn't select Department and Group , cann't save [Begin]
IF llGMATrig AND (EMPTY(laData[85]) OR EMPTY(laData[10]))
  =gfModalGen('TRM00000B00000','DIALOG',.F.,.F.,'You have to enter a Department and Group. Cannot update.')	
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF
*C102452,1 [End]

*--No Non Majors found in structure.
*-- HDM B602296,1 12/01/1198 ignore adding record in case style structure has ESS as nonmajor [START]
*IF llNoNMjrs AND !llCopySt
IF llNoNMjrs AND !llCopySt AND !llMScale
*-- HDM B602296,1 12/01/1198 ignore adding record in case style structure has ESS as nonmajor [END]
  SELECT (lccolorfil)
  APPEND BLANK
  GATHER FROM laData FIELDS &lcScFields MEMO
  REPLACE cStatus WITH IIF(laScrMode[4],'A','S')
ENDIF

GO TOP IN (lccolorfil)
IF EOF(lccolorfil)
  *--No non majors was entered. Cannot update.
  =gfModalGen('TRM42016B42001','DIALOG')	
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF

*--Not to save the style records if there is no warehouses entered.
GO TOP IN (lcwarefil)
IF llMultiwh AND EOF(lcwarefil)
  *--No warehouses was entered. Cannot update.
  =gfModalGen('TRM42017B42001','DIALOG')	
  STORE .F. TO llShow,llCSave
  RETURN
ENDIF


IF laScrMode[3]    && Edit Mode

  *B038753,1 WSH If Cost Method is Standard call program to update Inv. Journal [Start]
  SELECT STYLE
  SET ORDER TO STYLE
  =SEEK(lcMajor+lcNMKey)

  IF lcCstMth = 'S' AND !Style.lDetCost
    STORE .T. TO llContinSave
  
    *B128128,1 HBG 11/20/2005 Calculate the cost according to the base currancy [End]
    *DO (gcAppHome+'ICUPDSV') WITH '0001', lcMajor+lcNMKey, lcColorFil
    For lnI =1 To 5
      lcI = STR(lnI,1)
      laCostFld[lnI,2] = lnCostVal&lcI
    ENDFOR
    DO (gcAppHome+'ICUPDSV') WITH '0001', lcMajor+lcNMKey, lcColorFil , .F. , laCostFld
    *B128128,1 [End]

  
    IF !llContinSave
      STORE .F. TO llShow, llCSave
      RETURN
    ENDIF
  ENDIF
  *B038753,1 WSH [End]
  
  WAIT 'Updating Style: '+lcMajor+' Information ...' WINDOW NOWAIT
  SELECT STYLE
  SET ORDER TO STYLE
  =SEEK(lcMajor+lcNMKey)
  
  *-- E301023,1 HDM[START] Change the variable name because of Conflict with ARIA3.PRG-ARIABROW()
  *  lnSavRec=RECNO()
  lnSvStyRec=RECNO()
  *-- E301023,1 HDM[ END ] Change the variable name because of Conflict with ARIA3.PRG-ARIABROW()
  
  *** Store the average cost for the first color of that style
  *** to be used in filling the AVE_COST field for the new style/color
  *** in modify mode. If the default cost method is "AVERAGE" and the
  *** the style is not manufactured.
  llFrst = .T.
  lnNewAvgCst = STYLE.totcost
  *B603434,1 SSH 05/02/00 Get the changed data in string (Start)
  PRIVATE lcChangStr
  lcChangStr = ''
  lnStrtCut = 0
  DIMENSION laDataT[1]
  laDataT = ''
  
  
  FOR lnChInd = 1 TO ALEN(laData)
    lcStrToCom = SUBSTR(lcScFields,lnStrtCut+1,AT(',',lcScFields,1)-1)
    *B803786,1 ABD Fix Bug that when changing the discount to N/A from any 
    *B803786,1 ABD Other selection does not work. Upon saving, the prior value 
    *B803786,1 ABD That was in this field before the change gets saved. [Begin]
    *lcChangStr = lcChangStr + IIF(EVAL(lcStrToCom)<> IIF(lnChInd = 72,STR(laData[lnChInd],1),laData[lnChInd]),lcStrToCom+',','')    
    *IF EVAL(lcStrToCom)<> IIF(lnChInd = 72,STR(laData[lnChInd],1),laData[lnChInd])
    
    *B604965,1 ABD - Fix problem in the Planning Qty when Company use Use extended size scale
    *B604965,1 ABD - The planning field calculate all planning Qty for all color when add new color,
    *B604965,1 ABD - We will not update the Planning Qty in case use Use extended size scale. [Begin]
    IF !llMScale .OR. !llallcolors .OR. (llallcolors .AND. ATC('PLAN',Upper(lcStrToCom)) = 0))
      *B604965,1 ABD - [End]
      
      lcChangStr = lcChangStr + IIF(IIF(lnChInd = 72,STR(laData[lnChInd],1),laData[lnChInd]) <> EVAL(lcStrToCom),lcStrToCom+',','')
      IF IIF(lnChInd = 72,STR(laData[lnChInd],1),laData[lnChInd]) <> EVAL(lcStrToCom)
        *B803786,1 ABD [End]
        laDataT[ALEN(laDataT)] = laData[lnChInd]
        DIMENSION laDataT[ALEN(laDataT)+1]
      ENDIF
      *B605469,1 HBG 03/18/2002 Move this line out of if condition to fix bug of editing 
      *B605469,1                Special/Costing fields on style does not save change[Begin]
      *lnStrtCut  = AT(',',lcScFields,lnChInd)
      *B605469,1 [End]
      *B604965,1 ABD - End For IF Statment. [Begin]
    ENDIF 
    *B605469,1 HBG 03/18/2002 Move this line out of if condition to fix bug of editing 
    *B605469,1                Special/Costing fields on style does not save change[Begin]
    lnStrtCut  = AT(',',lcScFields,lnChInd)
    *B605469,1 [End]

    *B604965,1 ABD - [End]

  ENDFOR
  *B603434,1 SSH 05/02/00 End
  
  *C200120,1 KHM 06/12/2000 (Begin) Adding the follwoing custom process
  *C200120,1                in order to update the IcDeptHd and IcDeptDt files.
  IF ASCAN(laEvntTrig , PADR('UPDDEPFL',10)) <> 0
    =gfDoTriger('ICSTYLE',PADR('UPDDEPFL',10))
  ENDIF
  *C200120,1 KHM 06/12/2000 (End)
  
  *C102452,1 HBG 21/11/2001 Save Department and Group, and flag to save other fields in the screen[Begin]
  lcStyle = STYLE
  SCAN WHILE SUBSTR(Style,1,lnstylewid) = lcMajor
    IF llGMATrig
      REPLACE Dept       WITH laData[85]  ,;
              cstygroup  WITH laData[10] 
     *B124451,1 MHM 10/28/2004 Fix Bug of Custom Classfication for GMA did not coped in case of Copy styles[Start]
      *llSvUsrFld = .T.
      IF llCopySt        
        llSvUsrFld = .F.
      ELSE
        llSvUsrFld = .T.
      ENDIF
     *B124451,1 MHM [End]
      
    ENDIF
  ENDSCAN
  =SEEK(lcStyle)
  *C102452,1 [End]

  *B123627,1 MHM 07/29/2004 Get First Color[Start]
  =SEEK(lcMajor) 
  lcFstClor = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  *B124482,1 MHM 09/14/2004 Seek to last style color[Start]
  =SEEK(lcStyle)
  *B124482,1 MHM 09/14/2004 [End]
  
  *B123627,1 MHM 07/29/2004 [End]
    
  SCAN WHILE Style = lcMajor+lcNMKey
    =SEEK(STYLE.Style,lcColorFil) 
    lcColor = SUBSTR(Style,lnStyleWid+2,lnColorWid)
    =lfDibCods(lcMajor+lcSepart+lcColor)
    *lcLdscrip = IIF(laData[4]<>Desc1 AND llFrst AND &lcColorFil..cStatus = 'A',laData[4],&lccolorfil..Desc1)
    lcLdscrip = &lccolorfil..Desc1
    *--B801907,1 HDM Update llFrst According To llAlwsFrst value[start]
    *llFrst = .F.
    IF !llAlwsFrst
      llFrst = .F.
    ENDIF
    

    *--B801907,1 HDM Update llFrst According To llAlwsFrst value[end]
    *B604016,1 MHM 11/14/2000 [START]
    IF !EMPTY(laUsrFields[1,1]) 

      *B120791,1 MHM 12/30/2003 Cases of replacing values in array [Start]
      *FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
      *  =lfSvUsrFld()
      *ENDFOR
      IF llAllColors
         IF lnChanged = 1
           FOR lnCountArr = 1 TO ALEN(laUsrFields,1)
             lcUsrFld = "&lccolorfil.."+laUsrFields[lnCountArr,1]
             *B122006,1 MHM 03/30/2004 Change alltrim to work only in case of CHR only [Start]
             *IF ALLTRIM(laUsrFields[lnCountArr,6])<> ALLTRIM(&lcUsrFld)
             lcLchek = IIF(TYPE('laUsrFields[lnCountArr,6]') = "C",ALLTRIM(laUsrFields[lnCountArr,6]),laUsrFields[lnCountArr,6])
             lcRchek = IIF(TYPE('&lcUsrFld') = "C",ALLTRIM(&lcUsrFld),&lcUsrFld)
             IF !(lcLchek == lcRchek)
             *B122006,1  [End]
               *B123627,1 MHM 07/29/2004 If First color check if changed to update for all color[Start]
               IF lcFstClor =lcColor 
                 laChgUsrFd[lnCountArr] = .T.
               ENDIF
               *B123627,1 MHM 07/29/2004 [End]
               lnChanged = 2
             ENDIF
           ENDFOR
         ENDIF
         IF lnChanged = 2
           FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
             *B123627,1 MHM 07/29/2004 update for only Changed Colors [Start]
             *=lfSvUsrFld()
             IF laChgUsrFd[lnUsrField]           
               =lfSvUsrFld()
             ENDIF  
             *B123627,1 MHM 07/29/2004 [Start]
           ENDFOR
         ENDIF
         IF lnChanged = 2
          lnChanged = 2
         ELSE
           lnChanged = 3
         ENDIF  
       ELSE 
         FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
           =lfSvUsrFld()
         ENDFOR
       ENDIF
       *B120791,1 MHM 12/30/2003 Cases of replacing values in array [End]

    ENDIF  
    *B604016,1 MHM 11/14/2000 [END]

    lnOldAvCost = STYLE.Ave_cost
    *--Hold Old Style costs.
    SCATTER FIELDS nICost1,nICost2,nICost3,nICost4,nICost5,GROS_PRICE TO laOldICost
    =RLOCK()
    *B603434,1 SSH 05/02/00 Commented Out to gath from new Fields (Start)
    *GATHER FROM laData FIELDS &lcScFields MEMO
    IF !EMPTY(lcChangStr)
      IF SUBSTR(lcChangStr,LEN(lcChangStr),1) = ','
        lcChangStr = SUBSTR(lcChangStr,1,LEN(lcChangStr)-1)
      ENDIF
      GATHER FROM laDataT FIELDS &lcChangStr MEMO
    ENDIF
    *B603434,1 SSH 05/02/00 End
    REPLACE Style WITH lcMajor+lcSepart+lcColor,;
            Scale WITH IIF(llMScale,SUBSTR(Style,laSegInfo[ALEN(laSegInfo,1),4],3),Scale),;
            Desc  WITH &lccolorfil..Desc,;
            Desc1 WITH &lccolorfil..Desc1
    IF STYLE.Totstk <> 0 OR STYLE.Totret <> 0
 	  REPLACE Ave_cost WITH lnOldAvCos
    ENDIF
    IF STYLE.Make
      *B603434,1 SSH 05/02/00 Update changed variable only (Start)
      *REPLACE nMcost1  WITH lncostval1,nMcost2 WITH lncostval2,nMcost3 WITH lncostval3,nMcost4 WITH lncostval4,nMcost5 WITH lncostval5,;
               nIcost1  WITH 0,nIcost2 WITH 0,nIcost3 WITH 0,nIcost4 WITH 0,nIcost5 WITH 0,GROS_PRICE WITH 0
      *REPLACE nprcost2 WITH lnCostPrc2, nprcost3 WITH lnCostPrc3,nprcost4 WITH lnCostPrc4,nprcost5 WITH lnCostPrc5
      FOR lnMInd = 1 TO 5
        lcInd = STR(lnMInd,1)
        IF "nMcost"+lcInd $ lcChangStr
          REPLACE nMcost&lcInd WITH lncostval&lcInd
          IF lnMInd > 1
            REPLACE nprcost&lcInd WITH lnCostPrc&lcInd
          ENDIF
        ENDIF
        REPLACE nIcost&lcInd WITH 0
      ENDFOR
      *B603434,1 SSH 05/02/00 Update changed variable only (End)
    ELSE
      REPLACE nMcost1 WITH 0,nMcost2 WITH 0,nMcost3 WITH 0,nMcost4 WITH 0,nMcost5 WITH 0
      *B603140,1 (Begin)  If the imported style uses detail costing don't change cost for any color as we
      *B603140,1          edit them in Edit mode within style screen.
      *REPLACE nICost1    WITH IIF(laOldICost[1]<>&lccolorfil..nICost1 AND &lccolorfil..nICost1<>laData[45],&lccolorfil..nICost1,IIF(!llMulcurr,lncostval1,laData[45])),;
              nICost2    WITH IIF(laOldICost[2]<>&lccolorfil..nICost2 AND &lccolorfil..nICost2<>laData[46],&lccolorfil..nICost2,IIF(!llMulcurr,lncostval2,laData[46])),;
              nICost3    WITH IIF(laOldICost[3]<>&lccolorfil..nICost3 AND &lccolorfil..nICost3<>laData[47],&lccolorfil..nICost3,IIF(!llMulcurr,lncostval3,laData[47])),;
              nICost4    WITH IIF(laOldICost[4]<>&lccolorfil..nICost4 AND &lccolorfil..nICost4<>laData[48],&lccolorfil..nICost4,IIF(!llMulcurr,lncostval4,laData[48])),;
              nICost5    WITH IIF(laOldICost[5]<>&lccolorfil..nICost5 AND &lccolorfil..nICost5<>laData[49],&lccolorfil..nICost5,IIF(!llMulcurr,lncostval5,laData[49])),;
              GROS_PRICE WITH IIF(laOldICost[6]<>&lccolorfil..GROS_PRICE AND &lccolorfil..GROS_PRICE<>laData[83],&lccolorfil..GROS_PRICE,IIF(!llMulcurr,lncostval0,laData[83]))
      REPLACE nICost1    WITH IIF((laOldICost[1]<>&lccolorfil..nICost1 AND &lccolorfil..nICost1<>laData[45]) OR llDetCost,&lccolorfil..nICost1,IIF(!llMulcurr,lncostval1,laData[45])),;
              nICost2    WITH IIF((laOldICost[2]<>&lccolorfil..nICost2 AND &lccolorfil..nICost2<>laData[46]) OR llDetCost,&lccolorfil..nICost2,IIF(!llMulcurr,lncostval2,laData[46])),;
              nICost3    WITH IIF((laOldICost[3]<>&lccolorfil..nICost3 AND &lccolorfil..nICost3<>laData[47]) OR llDetCost,&lccolorfil..nICost3,IIF(!llMulcurr,lncostval3,laData[47])),;
              nICost4    WITH IIF((laOldICost[4]<>&lccolorfil..nICost4 AND &lccolorfil..nICost4<>laData[48]) OR llDetCost,&lccolorfil..nICost4,IIF(!llMulcurr,lncostval4,laData[48])),;
              nICost5    WITH IIF((laOldICost[5]<>&lccolorfil..nICost5 AND &lccolorfil..nICost5<>laData[49]) OR llDetCost,&lccolorfil..nICost5,IIF(!llMulcurr,lncostval5,laData[49]))
              *--Separate this condition as the statement is too long to compile.
              REPLACE GROS_PRICE WITH IIF((laOldICost[6]<>&lccolorfil..GROS_PRICE AND &lccolorfil..GROS_PRICE<>laData[83]) OR llDetCost,&lccolorfil..GROS_PRICE,IIF(!llMulcurr,lncostval0,laData[83]))
      *B603140,1 (End)
      *B603434,1 SSH 05/02/00 Update changed variable only (Start)
      *REPLACE nprcost2  WITH IIF(nprcost2=0,lnCostPrc2,nprcost2),;
              nprcost3  WITH IIF(nprcost3=0,lnCostPrc3,nprcost3),;
              nprcost4  WITH IIF(nprcost4=0,lnCostPrc4,nprcost4),;
              nprcost5  WITH IIF(nprcost5=0,lnCostPrc5,nprcost5),;
              DISC_PCNT WITH IIF(DISC_PCNT=0,lnCostPrc0,DISC_PCNT)
      REPLACE nprcost2  WITH IIF(nprcost2=0 ,IIF('nPrCost2'  $ lcChangStr,lnCostPrc2,nprcost2),nprcost2),;
              nprcost3  WITH IIF(nprcost3=0 ,IIF('nPrCost3'  $ lcChangStr,lnCostPrc3,nprcost3),nprcost3),;
              nprcost4  WITH IIF(nprcost4=0 ,IIF('nPrCost4'  $ lcChangStr,lnCostPrc4,nprcost4),nprcost4),;
              nprcost5  WITH IIF(nprcost5=0 ,IIF('nPrCost5'  $ lcChangStr,lnCostPrc5,nprcost5),nprcost5),;
              DISC_PCNT WITH IIF(DISC_PCNT=0,IIF('Disc_Pcnt' $ lcChangStr,lnCostPrc0,DISC_PCNT),DISC_PCNT)
      *B603434,1 SSH 05/02/00 Update changed variable only (End)
*      IF ! llMulcurr
*        REPLACE nIcost1 WITH lncostval1,nIcost2 WITH lncostval2,nIcost3 WITH lncostval3,nIcost4 WITH lncostval4,nIcost5 WITH lncostval5
*      ENDIF
*      REPLACE nprcost2 WITH lnCostPrc2, nprcost3 WITH lnCostPrc3,nprcost4 WITH lnCostPrc4,nprcost5 WITH lnCostPrc5
    ENDIF
    *-- HDM Fix Bug Of Not saving Style Prices in All Colors if They'r 0[Start]
    *B802177,1 Remove the comment of this replace portion.
    IF llAllColors   &&AND SEEK(STYLE.Style,lccolorfil)
      REPLACE PriceA     WITH &lccolorfil..PriceA,;
              PriceB     WITH &lccolorfil..PriceB,;
              PriceC     WITH &lccolorfil..PriceC,;
              nSugRetPri WITH &lccolorfil..nSugRetPri,;
              MarkA      WITH &lccolorfil..MarkA,;
              MarkB      WITH &lccolorfil..MarkB,;
              MarkC      WITH &lccolorfil..MarkC,;
              nMarkRet   WITH &lccolorfil..nMarkRet
      *B604192,1 AME [Start] Replace totcost. 
      IF llMScale .AND. ASCAN(laEvntTrig , PADR('LLCOSTUPDT',10)) <> 0
        = gfDoTriger('ICSTYLE',PADR('LLCOSTUPDT',10))
      ENDIF
      *B604192,1 End[End] 
      *B130339,1  TMI [Start] Do not update the nICost1 , ... , nICost5 by the cost entered in style screen if all colors selected
      IF ASCAN(laEvntTrig,'DNYUPCLR') <> 0
        =gfDoTriger('ICSTYLE','DNYUPCLR')
      ENDIF
      *B130339,1  TMI [End  ] 
    ENDIF
    

    *-- HDM Fix Bug Of Not saving Style Prices in All Colors if They'r 0[End]
  
    *REPLACE mod_action WITH 'MODIFIED' ,;
            mod_date   WITH gdSysDate  ,;
            mod_time   WITH TIME()     ,;
            mod_userid WITH gcUser_id

    UNLOCK
    *E300803,1 SWK 05/25/98 Call TraceKey global function.
    *--Call TraceKey global function.
    =gfTraceKey('STYLE',lcMajor+lcSepart+lcColor,'M')
    *C200597,1  TMI [Start] Call a customized tracekey function for Bong Hwa
    IF ASCAN(laEvntTrig , PADR('STYLE_M',10)) <> 0
      =gfDoTriger('ICSTYLE',PADR('STYLE_M',10))
    ENDIF
    *C200597,1  TMI [End  ] 
    *E300803,1 (End)   

    *--HDM[START]
    = lfSaveBom(lcMajor,lcNMKey,lccolor)
    *--HDM[END]

  ENDSCAN
  *E301525,1 HS Add these lines to mark any "Selection Codes" that contain the edited style(s)
  *E301525,1    as "To Be Transmitted", if the Inter-Company EDI module is installed and
  *E301525,1    setup and the "System Type" system setup is set to "Back Office"
  
  *-- if the Inter-Company EDI module is installed and setup and the "System Type"
  *-- system setup is set to "Back Office"
  IF "NC" $ gcCmpModules .AND. gfGetMemVar('M_SYSTYPE') = "B"
    PRIVATE lcCatgSty
   
    *-- Open EDI Files
    IF !USED('EDICatgH')
      laOpFile[24] =gfOpenFile(gcDataDir + 'EDICatgH' , gcDataDir + 'Account' , 'SH')
    ENDIF
    IF !USED('EDICatgD')
      laOpFile[25] =gfOpenFile(gcDataDir + 'EDICatgD' , gcDataDir + 'Account' , 'SH')
    ENDIF
    IF !USED('EDITRANS')
      laOpFile[26] =gfOpenFile(gcDataDir + 'EDITRANS' , gcDataDir + 'TYPEKEY' , 'SH')
    ENDIF
    
    *-- If the user was editing the style major
    IF lcNonMjr = REPLICATE("*" , LEN(lcNonMjr))
      lcCatgSty = lcMajor
    ELSE    && Else, If the user was editing a certain style
      lcCatgSty = lcMajor + lcSepart + lcNonMjr
    ENDIF
    
    *-- Mark any "Selection Codes" that contain the edited style(s) as "To Be Transmitted"
    SELECT EDICatgH
    SCAN FOR cEDIStatus = "A"
      IF SEEK(Type + cPartner + cSelcCode + lcCatgSty , "EDICatgD")
        SELECT EDICatgD
        LOCATE FOR cEDIDStat = "A";
          REST;
         WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type + EDICatgH.cPartner +;
                                                     EDICatgH.cSelcCode + lcCatgSty
        
        IF FOUND()
          REPLACE lTransmit WITH .T.;
             REST;
              FOR cEDIDStat = "A";
            WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type + EDICatgH.cPartner +;
                                                        EDICatgH.cSelcCode + lcCatgSty
          
          SELECT EDITrans
          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
          *IF SEEK("832" + PADR(EDICatgH.cSelcCode , 20) + EDICatgH.Type + EDICatgH.cPartner)
          IF SEEK("832" + PADR(EDICatgH.cSelcCode , 40) + EDICatgH.Type + EDICatgH.cPartner)
          *E037853,1 [End]
            IF cStatus <> "N"
              REPLACE cStatus WITH "N"
            ENDIF
          ELSE
            APPEND BLANK
            REPLACE cEDITrnTyp WITH "832"              ,;
                    Key        WITH EDICatgH.cSelcCode ,;
                    Type       WITH EDICatgH.Type      ,;
                    cPartner   WITH EDICatgH.cPartner  ,;
                    cStatus    WITH "N"
          ENDIF
        
          SELECT EDICatgH
          REPLACE lTransmit WITH .T.
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  *E301525,1 HS Add these lines to mark any "Selection Codes" that contain [End]
  
ELSE  && Add Mode
  lcStyKey=lcMajor
  IF !gfKeyExist('Style',@lcStyKey,lnStyleWid)
    STORE .F. TO llShow,llCSave
    RETURN
  ENDIF
  *C200120,1 KHM 06/12/2000 (Begin) Adding the follwoing custom process
  *C200120,1                in order to update the IcDeptHd and IcDeptDt files.
  IF ASCAN(laEvntTrig , PADR('UPDDEPFL',10)) <> 0
    =gfDoTriger('ICSTYLE',PADR('UPDDEPFL',10))
  ENDIF
  *C200120,1 KHM 06/12/2000 (End)
ENDIF

*C200307,1 AMH Save changes in STYCDNOT file for customer Collage [Start]
IF ASCAN(laEvntTrig , PADR('SAVSTYNT',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('SAVSTYNT',10))
ENDIF
*C200307,1 AMH [End]

*C038291,1  TMI [Start] Save measurments to the file "STYSPEC" for customer Eng10\Vijay
IF ASCAN(laEvntTrig , PADR('SVMSRMNT',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('SVMSRMNT',10))
ENDIF
*C038291,1  TMI [End  ] 
*---------------------------------------------------------
WAIT 'Adding new Non Majors to this Style .....' WINDOW NOWAIT

llOpnShst = !USED('ICSTYHST')
IF llOpnShst
  =gfOpenFile(gcDataDir+'ICSTYHST','Styhst','SH')
ENDIF
SELECT (lccolorfil)
*B604203,1 KAM 08/03/2001 stopping add first forign price to new color[start]
*B803097,1 (Begin) Assign the first color foiegn price to each new color added.
*IF laScrMode[3]    && Edit Mode
*  LOCATE FOR cstatus = 'A'
*  IF FOUND()
*    SELECT (lctmpstypr)
*    LOCATE
*    SCATTER MEMVAR MEMO
*  ENDIF
*  SELECT (lccolorfil)
*ENDIF  
*B803097,1 (End)
IF llMScale
  STORE 0 TO lnExtScPos,lnLenExtnd 
  =lfGetSclPo()
  lnLenExtnd = 3 - lnScaleLen                && Extended Length.
ENDIF  
*B604203,1 KAM [end]

*120205,1   TMI [Start] Send a mail to employees to notify them with added style/color(s)
IF ASCAN(laEvntTrig , PADR('SENDMAIL',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('SENDMAIL',10))
ENDIF
*120205,1   TMI [End  ] 

SCAN ALL FOR cstatus = 'A'

  *B606680,1 ABD - Scatter needed field to local array. [Begin]
  IF llCopySt
    SCATTER FIELDS &lcNMjrFlds MEMO To laNMjrFlds
    *B122822,1 NNA 07/15/2004 (Begin) Store Zero To The Stock Value if we Created Style From Another
    STORE 0 TO laNMjrFlds[21]
    *B122822,1 NNA (End)
  ENDIF
  *B606680,1 ABD - [End]

  lcColor=SUBSTR(Style,lnStyleWid+2,lnColorWid)
  =lfDibCods(lcMajor+lcSepart+lcColor)
  *B604203,1 KAM take all forign price to new color(s) [start]
  *B803097,1 (Begin) Assign the first color foiegn price to each new color added.
  *IF laScrMode[3]    && Edit Mode
  *  INSERT INTO STYPRICE FROM MEMVAR    
  *  REPLACE STYPRICE.Style WITH &lcColorFil..Style
  *ENDIF
  *B803097,1 (End)
  IF laScrMode[3]    && Edit Mode
    SELECT (lctmpstypr)
    LOCATE FOR cStatus = 'S'
    lcFrstStyl=STYLE
    lcExpTmp  ="STYLE=lcFrstStyl"
    IF llMsCALE
      lcExpTmp ="SUBSTR(STYLE,1,lnExtScPos-1)= SUBSTR(lcFrstStyl,1,lnExtScPos-1) AND SUBSTR(STYLE,lnExtScPos,3 ) = SUBSTR(&lcColorFil..Style,lnExtScPos,3 ) "       
     *lcExpTmp ="SUBSTR(STYLE,1,lnExtScPos-1)= SUBSTR(lcFrstStyl,1,lnExtScPos-1) AND SUBSTR(STYLE,lnExtScPos,lnLenExtnd ) = SUBSTR(&lcColorFil..Style,lnExtScPos,lnLenExtnd ) "
    ENDIF
    SCAN FOR &lcExpTmp
    * SCAN FOR STYLE=lcFrstStyl
      SCATTER MEMVAR MEMO
      m.Style = &lcColorFil..Style
      INSERT INTO STYPRICE FROM MEMVAR    
    ENDSCAN  
    SELECT (lccolorfil)
  ENDIF 
  *B604203,1 KAM [end]
  *--Calculate the costing fields and avarage cost when
  *-adding colors to an existing style.

  *--HDM Also we have to update the fabric file in case "Imported Styles"[start]
  *  IF ( laData[21] OR (!laData[21] AND llimpcost) ) AND SEEK(lcMajor,'BOM')
  *--HDM Also we have to update the fabric file in case "Imported Styles"[end]

  *:B602872,1 Comment out and move this part down.
  *--HDM[START]
  *    = lfSaveBom(lcMajor,lcNMKey,lccolor)
  *--HDM[END]

  *Calculate cost from bom only if detail costing.
  *IF SEEK(lcMajor,'BOM')
  IF SEEK(lcMajor,'BOM') AND laData[80]

    *--Check if the color segment exist and get the segno.
    lnClrPnt=0

    FOR I=1 TO ALEN(laSegInfo,1)
      IF laSegInfo[I,1]='N' AND laSegInfo[I,3]='C'
        lnClrPnt=I
        EXIT
      ENDIF
    ENDFOR
    *--HDM B602642,1 Stop Calling Calc Cost Function In Case of ESS[start]
    IF !llMScale
	  =lfCalcCost(lcColor,laData[6])
	ENDIF
	*--HDM B602642,1 Stop Calling Calc Cost Function In Case of ESS[end]
  ENDIF
  SELECT STYLE
  SET DELETE OFF
  IF SEEK(SPACE(19)) .AND. DELETED()
    RECALL
    =RLOCK()
    BLANK
  ELSE
    APPEND BLANK
    =RLOCK()
  ENDIF
  SET DELETE ON
  GATHER FROM laData FIELDS &lcScFields MEMO
  *B606680,1 ABD - Gather needed field from the current file and not from the fields on the screen.[Begin]
  IF llCopySt
    GATHER FROM laNMjrFlds FIELDS &lcNMjrFlds MEMO
  ENDIF
  *B606680,1 ABD - [End]
  
  *B604965,1 ABD - Fix problem in the Planning Qty when Company use Use extended size scale
  *B604965,1 ABD - The planning field calculate all planning Qty for all color when add new color,
  *B604965,1 ABD - We will not update the Planning Qty in case use Use extended size scale. [Begin]
  IF llMScale
    *-- Update the Plan filds with Qty in the Tmp File.
    REPLACE Plan1   WITH &lccolorfil..Plan1   ,;
            Plan2   WITH &lccolorfil..Plan2   ,;
            Plan3   WITH &lccolorfil..Plan3   ,;
            Plan4   WITH &lccolorfil..Plan4   ,;
            Plan5   WITH &lccolorfil..Plan5   ,;
            Plan6   WITH &lccolorfil..Plan6   ,;
            Plan7   WITH &lccolorfil..Plan7   ,;
            Plan8   WITH &lccolorfil..Plan8   ,;
            TotPlan WITH &lccolorfil..TotPlan
  ENDIF
  *B604965,1 ABD - [End]
  
  REPLACE Style      WITH lcMajor+lcSepart+lcColor,;
          cStyMajor  WITH lcMajor,;
          Ave_cost   WITH IIF((laScrMode[3] AND !Make AND !llDetCost),lnNewAvgcst,Totcost),;
          Desc       WITH &lccolorfil..Desc,;
          Desc1      WITH &lccolorfil..Desc1,; 
          Scale      WITH IIF(llMScale,SUBSTR(Style,laSegInfo[ALEN(laSegInfo,1),4],3),Scale),;
          PriceA     WITH &lccolorfil..PriceA,;
          PriceB     WITH &lccolorfil..PriceB,;
          PriceC     WITH &lccolorfil..PriceC,;
          nSugRetPri WITH &lccolorfil..nSugRetPri,;
          MarkA      WITH &lccolorfil..MarkA,;
          MarkB      WITH &lccolorfil..MarkB,;
          MarkC      WITH &lccolorfil..MarkC,;
          nMarkRet   WITH &lccolorfil..nMarkRet,;
          dAdd_date  WITH gdSysDate   ,;
          cAdd_time  WITH TIME()      ,;
          cAdd_user  WITH gcUser_id
  
  *B607888,1 TMI [Start] update with the correct tax break when adding a new color  
  IF llMScale 
    REPLACE NTAXBREAK WITH &lcColorFil..NTAXBREAK  
  ENDIF
  *B607888,1 TMI [End  ] 
  
  *B604192,1 AME [Start] Replace totcost. 
  IF llMScale .AND. ASCAN(laEvntTrig , PADR('LLCOSTUPDT',10)) <> 0
    = gfDoTriger('ICSTYLE',PADR('LLCOSTUPDT',10))
  ENDIF
  *B604192,1 End[End] 
  
  IF !Make
    REPLACE nICost1    WITH IIF(&lccolorfil..nICost1=0,IIF(!llMulcurr,lncostval1,laData[45]),&lccolorfil..nICost1),;
            nICost2    WITH IIF(&lccolorfil..nICost2=0,IIF(!llMulcurr,lncostval2,laData[46]),&lccolorfil..nICost2),;
            nICost3    WITH IIF(&lccolorfil..nICost3=0,IIF(!llMulcurr,lncostval3,laData[47]),&lccolorfil..nICost3),;
            nICost4    WITH IIF(&lccolorfil..nICost4=0,IIF(!llMulcurr,lncostval4,laData[48]),&lccolorfil..nICost4),;
            nICost5    WITH IIF(&lccolorfil..nICost5=0,IIF(!llMulcurr,lncostval5,laData[49]),&lccolorfil..nICost5),;
            GROS_PRICE WITH IIF(&lccolorfil..GROS_PRICE=0,IIF(!llMulcurr,lncostval0,laData[83]),&lccolorfil..GROS_PRICE),;
            DISC_PCNT  WITH IIF(&lccolorfil..DISC_PCNT=0,IIF(!llMulcurr,lnCostPrc0,laData[84]),&lccolorfil..DISC_PCNT)

  ELSE
    REPLACE nMCost1  WITH IIF(&lccolorfil..nMCost1=0,lncostval1,&lccolorfil..nMCost1),;
            nMCost2  WITH IIF(&lccolorfil..nMCost2=0,lncostval2,&lccolorfil..nMCost2),;
            nMCost3  WITH IIF(&lccolorfil..nMCost3=0,lncostval3,&lccolorfil..nMCost3),;
            nMCost4  WITH IIF(&lccolorfil..nMCost4=0,lncostval4,&lccolorfil..nMCost4),;
            nMCost5  WITH IIF(&lccolorfil..nMCost5=0,lncostval5,&lccolorfil..nMCost5)

  ENDIF
  REPLACE nprcost2  WITH IIF(nprcost2=0,lnCostPrc2,nprcost2),;
          nprcost3  WITH IIF(nprcost3=0,lnCostPrc3,nprcost3),;
          nprcost4  WITH IIF(nprcost4=0,lnCostPrc4,nprcost4),;
          nprcost5  WITH IIF(nprcost5=0,lnCostPrc5,nprcost5)
  *--HDM
  IF llCopySt
    REPLACE plan1    WITH &lccolorfil..plan1, plan2  WITH &lccolorfil..plan2,;
            plan3    WITH &lccolorfil..plan3, plan4  WITH &lccolorfil..plan4,;
            plan5    WITH &lccolorfil..plan5, plan6  WITH &lccolorfil..plan6,;
            plan7    WITH &lccolorfil..plan7, plan8  WITH &lccolorfil..plan8,;
            TotPlan  WITH &lccolorfil..TotPlan
  ENDIF
  *--HDM

  *C122981,1 NNA 07/18/2004 (Begin) Blank the Color Cost if We add a new Color to a Style for Customer[ERI02]
  IF ASCAN(laEvntTrig,PADR('CLRCOST',10))<>0 
    =gfDoTriger(lcProgName,PADR('CLRCOST',10))
  ENDIF
  *C122981,1 NNA (End)

 
  *C102452,1 HBG 21/11/2001 Save Department and Group, and flag to save other fields in the screen[Begin]
  IF llGMATrig
    REPLACE Dept       WITH laData[85]  ,;
            cstygroup  WITH laData[10] 
    *B124451,1 MHM 10/28/2004 Fix Bug of Custom Classfication for GMA did not coped in case of Copy styles[Start]
    *llSvUsrFld = .T.
    IF llCopySt        
      llSvUsrFld = .F.
    ELSE
      llSvUsrFld = .T.
    ENDIF
     *B124451,1 MHM [End]
  ENDIF
  *C102452,1 [End]

  *B604016,1 MHM 11/14/2000 [START]
  IF !EMPTY(laUsrFields[1,1]) 
    
    *B120791,1 MHM 12/30/2003 Cases of replacing values in array [Start]
    *FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
    *  =lfSvUsrFld()
    *ENDFOR
    IF llCopySt
       IF lnChanged = 1
         FOR lnCountArr = 1 TO ALEN(laUsrFields,1)
           lcUsrFld = "&lccolorfil.."+laUsrFields[lnCountArr,1]
           *B122006,1 MHM 03/30/2004 Change alltrim to work only in case of CHR only [Start]
           *IF ALLTRIM(laUsrFields[lnCountArr,6])<> ALLTRIM(&lcUsrFld)
           lcLchek = IIF(TYPE('laUsrFields[lnCountArr,6]') = "C",ALLTRIM(laUsrFields[lnCountArr,6]),laUsrFields[lnCountArr,6])
           lcRchek = IIF(TYPE('&lcUsrFld') = "C",ALLTRIM(&lcUsrFld),&lcUsrFld)
           IF !(lcLchek == lcRchek)
           *B122006,1  [End]
             lnChanged = 2
           ENDIF
         ENDFOR
       ENDIF
       IF lnChanged = 2
         FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
          =lfSvUsrFld()
         ENDFOR
       ELSE
         =SEEK(STYLE.STYLE,lccolorfil)
         FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
           lcUsrFld = "&lccolorfil.."+laUsrFields[lnUsrField,1]
           lcStyFld = "Style."+laUsrFields[lnUsrField,1]
           REPLACE &lcStyFld WITH  &lcUsrFld
         ENDFOR  
         FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
           lcUsrFld = "&lccolorfil.."+laUsrFields[lnUsrField,1]
           laUsrFields[lnUsrField,6] = &lcUsrFld
         ENDFOR  
       ENDIF
       IF lnChanged = 2
         lnChanged = 2
       ELSE
         lnChanged = 3
       ENDIF  
    ELSE 
      FOR lnUsrField = 1 TO ALEN(laUsrFields,1)
        =lfSvUsrFld()
      ENDFOR
    ENDIF
    *B120791,1 MHM 12/30/2003 Cases of replacing values in array [End]
  ENDIF  
  *B604016,1 MHM 11/14/2000 [END]
  UNLOCK
  *E300803,1 SWK 05/25/98
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',lcMajor+lcSepart+lcColor,'A')
  *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
  IF ASCAN(laEvntTrig , PADR('STYLE_A',10)) <> 0
    =gfDoTriger('ICSTYLE',PADR('STYLE_A',10))
  ENDIF
  *C200597,1  TMI [End  ] 
  *E300803,1 (End)

  *:B602872,1 Moved to be after style file update.
  *--HDM[START]
    = lfSaveBom(lcMajor,lcNMKey,lccolor)
  *--HDM[END]

  SELECT ICSTYHST
  INSERT INTO ICSTYHST (Style,cFisFYear) VALUES (STYLE.Style,STR(VAL(gcCurrYear)-1,4))

  *E300803,1 SWK 05/25/98
  *--Call TraceKey global function.
  =gfTraceKey('ICSTYHST',STYLE.Style+STR(VAL(gcCurrYear)-1,4),'A')
  *E300803,1 (End)

  INSERT INTO ICSTYHST (Style,cFisFYear) VALUES (STYLE.Style,gcCurrYear)

  *E300803,1 SWK 05/25/98
  *--Call TraceKey global function.
  =gfTraceKey('ICSTYHST',STYLE.Style+gcCurrYear,'A')
  *E300803,1 (End)

  INSERT INTO ICSTYHST (Style,cFisFYear) VALUES (STYLE.Style,STR(VAL(gcCurrYear)+1,4))

  *E300803,1 SWK 05/25/98
  *--Call TraceKey global function.
  =gfTraceKey('ICSTYHST',STYLE.Style+STR(VAL(gcCurrYear)+1,4),'A')
  *E300803,1 (End)
  
  *C200171 TMI [Start] Generate UPC in styleupc.dbf     
  IF llNCInstl .AND. ASCAN(laEvntTrig , 'GNUPCCLR  ') <> 0   
    =gfDoTriger('ICSTYLE','GNUPCCLR  ')
  ENDIF
  *C200171 TMI [End  ]
  
ENDSCAN


IF llOpnShst
  USE IN ICSTYHST
ENDIF


*- Call this function to save the available warehouses.
=lfSaveWare()


*E301586,1 ABD - Delete the imported style from the PDMstyle file.
*E301586,1 ABD - Also we will change status to be complete. [Begin]
IF llImprtSty
  *-- Delete the seletced item from the PDMMATRL File.
  lnAlias = SELECT (0)
  SELECT PDMSTYLE
  lcOldOrder = ORDER()
  SET ORDER TO STYLE
  SEEK lcMajor
*--- SSH
  *SCAN REST WHILE LEFT(Style,lnStyleWid) = lcMajor
  SCAN FOR LEFT(Style,lnStyleWid) = lcMajor
*--- SSH
    BLANK
    DELETE
  ENDSCAN
  SET ORDER TO &lcOldOrder 
  
  *-- Mark the style in the log file as an imported Style.
  IF SEEK('PROTOTYPE'+lcMajor,"PDMLOG")
    SELECT PDMLOG
    REPLACE cImpstatus With 'C'
  ENDIF
  SELECT (lnAlias)
  llImprtSty = .F.
ENDIF
*E301586,1 ABD - [End]


*--Update custom fields.
*C200076,1 (Begin) Save custom fields.
*B604016,1 MHM 11/14/2000 [start]
*B803629,1 MHM 08/29/2000 [start]
*IF !EMPTY(laUsrFields[1,1])
*IF !EMPTY(laUsrFields[1,1]) .AND. ASCAN(laEvntTrig , PADR('STY_SAV',10)) <> 0
*B803629,1 MHM 08/29/2000 [end]
*  = gfDoTriger('ICSTYLE','STY_SAV')
*ENDIF
*B604016,1 MHM 11/14/2000 [end]
*C200076,1 End.
*--Save style foreign prices.
SELECT (lctmpstypr)
SET ORDER TO TAG (lctmpstypr)
SET DELETE OFF
SEEK lcMajor+lcNMKey
SCAN WHILE Style=lcMajor+lcNMKey FOR cStatus<>'S'
  lcFPStat=cStatus
  SCATTER MEMVAR 
  SELECT (lcColorFil)
  SEEK lcMajor+lcNMKey
  *B603561,1 (Begin) Deleted records exist in lcColorFil which will be browsed and updated in STYPRICE
  *B603561,1         by accident because of SET DELETE OFF. So we will exclude these deleted records to prevent them from be added to styprice file.
  *SCAN WHILE Style=lcMajor+lcNMKey
  SCAN WHILE Style=lcMajor+lcNMKey FOR !DELETED()
  *B603561,1 (Begin)
    SELECT STYPRICE
    DO CASE
      CASE lcFPStat='A' 
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Style WITH &lcColorFil..Style
        *E300803,1 SWK 05/25/98
        *--Call TraceKey global function.
        =gfTraceKey('STYPRICE',STYPRICE.STYLE+STYPRICE.CCURRCODE,'A')
        *E300803,1 (End)
        
      CASE lcFPStat $ 'MD'     
        IF SEEK(&lcColorFil..Style+&lcTmpStyPr..cCurrCode)    
          IF lcFPStat = 'D'  
           *E300803,1 SWK 05/25/98
           *--Call TraceKey global function.
           =gfTraceKey('STYPRICE',STYPRICE.STYLE+STYPRICE.CCURRCODE,'D')
            *E300803,1 (End)
            BLANK
            DELETE
          ELSE
            GATHER MEMVAR
            REPLACE Style WITH &lcColorFil..Style
           *E300803,1 SWK (End)
           *--Call TraceKey global function.
           =gfTraceKey('STYPRICE',STYPRICE.STYLE+STYPRICE.CCURRCODE,'M')
            *E300803,1 (End)
          ENDIF
        ENDIF          
    ENDCASE       
  ENDSCAN 
ENDSCAN
SET DELETE ON

SELECT STYDYE
SET RELATION TO

*--Update the segment value file if there is a free segment in the major.

FOR lnCnt=1 TO lnMajSeg
  IF laSegInfo[lnCnt,3]='F'
    SELECT ICSEGVAL
    IF !SEEK(laSegInfo[lnCnt,2]+SUBSTR(lcMajor,laSegInfo[lnCnt,4],laSegInfo[lnCnt,5]))
      APPEND BLANK
      REPLACE cISegNo   WITH laSegInfo[lnCnt,2],;
              cISegVal  WITH SUBSTR(lcMajor,laSegInfo[lnCnt,4],laSegInfo[lnCnt,5]),;
              cISgValSd WITH laData[3],; 
              cISgValLd WITH laData[4],; 
              cAdd_User WITH gcUser_id,;
              cAdd_Time WITH TIME()   ,;
              dAdd_Date WITH gdSysDate
       *E300803,1 SWK 05/25/98
       *--Call TraceKey global function.
       =gfTraceKey('ICSEGVAL',ICSEGVAL.cisegno+ICSEGVAL.cisegval,'A')
       *E300803,1 (End)
    ENDIF
  ENDIF
ENDFOR
*--B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
*--B602753,1 HDM 04/07/1999        Will Call it

*--Would you like to edit style notes?','\<Yes;\<No.
*IF laScrMode[4] AND gfModalGen('TRM42018B42002','DIALOG')=1
*  =lfvnotes()
*ENDIF
*--B602753,1 HDM 04/07/1999[End]
SELECT STYLE
IF laScrMode[3]
  *-- E301023,1 HDM[START] Change the variable name because of Conflict with ARIA3.PRG-ARIABROW()
  *GOTO lnSavRec
  GOTO lnSvStyRec
  
  *C102564,1 BWA 04/24/2002 Add 3 fields to let know if the style has change or not.[START]
  IF ASCAN(laEvntTrig , PADR('CHNGEFCT',10)) <> 0
    =gfDoTriger('ICSTYLE',PADR('CHNGEFCT',10))
  ENDIF
  *C102564,1 BWA 04/24/2002.[END]

  *-- E301023,1 HDM[END] Change the variable name because of Conflict with ARIA3.PRG-ARIABROW()
ENDIF
*B604115,4 KAM 22/02/2001 assign false to llAllClrs[start]
llAllClrs = .F.

*C200519,1 TMI [Start] A function created for GMA with name "EXPCTOLD" to save data to CSTPKINF file

*C200235,1 ABD - If customer Cathy Daniel add or edit style to Aria27 
*C200235,1 ABD - update the Old System. [Begin]
IF ASCAN(laEvntTrig , PADR('EXPCTOLD',10)) <> 0
  =gfDoTriger('ICSTYLE',PADR('EXPCTOLD',10))
ENDIF      
*C200235,1 ABD - [End]
*C200519,1 TMI [End  ] 

*B604115,4 KAM [end]
SELECT(lnAlias)
WAIT CLEAR
RETURN

*!*************************************************************
*! Name      : lfSaveWare
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Save the new added warehouses, And delete the
*!             deleted the warehouses to/from the StyDye master file.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfsaveware

DECLARE lacolor[1]
SELECT SUBSTR(Style,lnStyleWid+2,lnColorWid) FROM (lccolorfil) INTO ARRAY lacolor
lnstyrec = RECNO("STYLE")
IF llmultiwh
  SELECT (lcwarefil)
  GO TOP
  SCAN
    DO CASE
      CASE cstatus = 'A'
  		FOR lncount = 1 TO ALEN(lacolor,1)
          
          *--HDM B602386,1 We have to relace in stydye File ass well[start]
  	  	  *lcNMPart=IIF(llNoNMjrs ,'',PADR(lacolor[lnCount],18-lnStyleWid))
          *--HDM B602386,1 We have to relace in stydye File ass well[end]
          
          lcNMPart=IIF(llNoNMjrs AND lnNonSeg = 0 ,'',PADR(lacolor[lnCount],18-lnStyleWid))
          *C102045,1 ABD -Add new location for selected color or Color-scale only if we
          *C102045,1 ABD -have color in Non Major Filed [Begin]
          IF !EMPTY(lcNMPart) .AND. SUBSTR(lcNonMjr,1,1) <> '*'
            =SEEK(lcMajor+lcSepart+lcNonMjr,"STYLE")
    		SELECT (lcwarefil)
            REPLACE ave_cost WITH style.ave_cost,;
        	        Style    WITH lcMajor+lcSepart+lcNonMjr
            SCATTER TO larecarr
            SELECT stydye
  	        APPEND BLANK
  		    GATHER FROM larecarr
    	    REPLACE dAdd_date WITH gdSysDate  ,;
  	  	       	    cAdd_time WITH TIME()  ,;
   				    cAdd_user WITH gcUser_id
            *--Call TraceKey global function.
            =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'A')
            EXIT      
          ELSE
            *C102045,1 ABD [End]
     	    =SEEK(lcMajor+lcSepart+lcNMPart,"STYLE")
  		    SELECT (lcwarefil)
   		    REPLACE ave_cost WITH style.ave_cost,;
        	  	    Style    WITH lcMajor+lcSepart+lcNMPart
            SCATTER TO larecarr
            SELECT stydye
  	        APPEND BLANK
  		    GATHER FROM larecarr
    	    REPLACE dAdd_date WITH gdSysDate  ,;
  	  	     	    cAdd_time WITH TIME()  ,;
   				    cAdd_user WITH gcUser_id
            *E300803,1 SWK 05/25/98
            *--Call TraceKey global function.
            =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'A')
            *E300803,1 (End)
          *C102045,1 ABD - End if for if statment. [Begin]
          ENDIF
          *C102045,1 ABD- [End]
   	    ENDFOR

        *C200171 TMI [Start] Generate UPC's for new assigned WH.
        IF llNCInstl .AND. ASCAN(laEvntTrig,'GNUPCWH   ') <> 0
          *-- Defining parameters       
          lcWhCode   = &lcWareFil..CWARECODE
          lcSty      = &lcWareFil..STYLE
          llFrmAdWre = .F.
          =gfDoTriger('ICSTYLE','GNUPCWH   ')
        ENDIF
        *C200171 TMI [End  ]
   	    
	  CASE cstatus = 'M'
 	    FOR lncount = 1 TO ALEN(lacolor,1)
  	  	  lcNMPart=IIF(llNoNMjrs AND lnNonSeg=0 ,'',PADR(lacolor[lnCount],18-lnStyleWid))
          *C102045,1 ABD -Add new location for selected color or Color-scale only if we
          *C102045,1 ABD -have color in Non Major Filed [Begin]
          IF !EMPTY(lcNMPart) .AND. SUBSTR(lcNonMjr,1,1) <> '*'
            SELECT (lcwarefil)
            REPLACE Style WITH lcMajor+lcSepart+lcNonMjr
            *B606503,1 (Begin) Only scatter modifyed fields.
            *SCATTER TO larecarr
            SCATTER FIELDS Style,Desc,cWareCode,gl_link,cStatus,cdisccode TO laRecArr
            *B606503,1 (End) Only scatter modifyed fields.
            SELECT stydye
            IF SEEK (lcMajor+lcSepart+lcNonMjr+&lcwarefil..cwarecode+SPACE(10))
  		      =RLOCK()
  		      *B606503,1 (Begin) Only Gather modifyed fields.
  		      *GATHER FROM larecarr
  		      GATHER FIELDS Style,Desc,cWareCode,gl_link,cdisccode FROM larecarr
  		      *B606503,1 (End) Only Gather modified fields.
              *--Call TraceKey global function.
              =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
		    ELSE
		      APPEND BLANK
		      GATHER FROM larecarr
		      REPLACE dAdd_date WITH gdSysDate  ,;
  	   	              cAdd_time WITH TIME()  ,;
       		          cAdd_user WITH gcUser_id
              *--Call TraceKey global function.
              =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'A')
            ENDIF
            EXIT
          ELSE
           *C102045,1 ABD [End]
  		    SELECT (lcwarefil)
  		    REPLACE Style WITH lcMajor+lcSepart+lcNMPart
            *B606503,1 (Begin) Only scatter modifyed fields.
            *SCATTER TO larecarr
            SCATTER FIELDS Style,Desc,cWareCode,gl_link,cStatus,cdisccode TO laRecArr
            *B606503,1 (End) Only gahter modified fields.
  		    SELECT stydye
  		    IF SEEK (lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode+SPACE(10))
  		      =RLOCK()
  		      *B606503,1 (Begin) Only scatter modifyed fields.
  		      *GATHER FROM larecarr
  		      GATHER FIELDS Style,Desc,cWareCode,gl_link,cdisccode FROM larecarr
  		      *B606503,1 (End) Only scatter modifyed fields.
  		      *REPLACE mod_action WITH 'MODIFIED' ,;
  			   	       mod_date   WITH gdSysDate     ,;
    	        	   mod_time   WITH TIME()     ,;
   				       mod_userid WITH gcUser_id
	          UNLOCK
              *E300803,1 SWK 05/25/98
              *--Call TraceKey global function.
              =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
              *E300803,1 (End)
		    ELSE
		      APPEND BLANK
		      GATHER FROM larecarr
		      REPLACE dAdd_date WITH gdSysDate  ,;
  	   	       	      cAdd_time WITH TIME()  ,;
       		          cAdd_user WITH gcUser_id
              *E300803,1 SWK 05/25/98
              *--Call TraceKey global function.
              =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'A')
              *E300803,1 (End)
		    ENDIF
          *C102045,1 ABD - End if for if statment. [Begin]
		  ENDIF  
          *C102045,1 ABD [End]

	    ENDFOR
	  CASE cstatus = 'D'
        FOR lncount = 1 TO ALEN(lacolor,1)
  	  	  lcNMPart=IIF(llNoNMjrs AND lnNonSeg=0,'',PADR(lacolor[lnCount],18-lnStyleWid))
          *C102045,1 ABD -Add new location for selected color or Color-scale only if we
          *C102045,1 ABD -have color in Non Major Filed [Begin]
          IF !EMPTY(lcNMPart) .AND. SUBSTR(lcNonMjr,1,1) <> '*'
   	        SELECT STYDYE
  		    IF SEEK (lcMajor+lcSepart+lcNonMjr+&lcwarefil..cwarecode+SPACE(10))
  		      SCATTER FIELDS stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8 TO lawarstk
  		      lnwrtotstk = totstk
  		      
  		      *B607286,1 ABD - Remark the next line and Blank the Stydye recored for 
  		      *B607286,1 ABD - Warehouse before deleted. [Begin]
   		      *DELETE REST WHILE Style+cWarecode = lcMajor+lcSepart+lcNonMjr+&lcwarefil..cwarecode FOR;
              *gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'D')
              SCAN REST WHILE SEEK(lcMajor+lcSepart+lcNonMjr+&lcwarefil..cwarecode) FOR;
                gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'D')
                BLANK
                DELETE
              ENDSCAN  
              *B607286,1 ABD - [End]

  	          SELECT STYLE
  	          IF SEEK(lcMajor+lcSepart+lcNonMjr)
  			    =RLOCK ()
  			    REPLACE stk1   WITH stk1   - lawarstk[1] ,;
  			            stk2   WITH stk2   - lawarstk[2] ,;
  					    stk3   WITH stk3   - lawarstk[3] ,;
  					    stk4   WITH stk4   - lawarstk[4] ,;
  					    stk5   WITH stk5   - lawarstk[5] ,;
  					    stk6   WITH stk6   - lawarstk[6] ,;
  					    stk7   WITH stk7   - lawarstk[7] ,;
  					    stk8   WITH stk8   - lawarstk[8] ,;
  					    totstk WITH totstk - lnwrtotstk
	            UNLOCK
  		      ENDIF
              =gfTraceKey('STYLE',STYLE.Style,'M')
              *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
              IF ASCAN(laEvntTrig , PADR('STYLE',10)) <> 0
                =gfDoTriger('ICSTYLE',PADR('STYLE',10))
              ENDIF
              *C200597,1  TMI [End  ] 
  		    ENDIF
            EXIT
          ELSE
            *C102045,1 ABD [End]
 	        SELECT stydye
  		    IF SEEK (lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode+SPACE(10))
  		      SCATTER FIELDS stk1,stk2,stk3,stk4,stk5,stk6,stk7,stk8 TO lawarstk
  		      lnwrtotstk = totstk
              *E300803,1 SWK 05/25/98 Add the gfTraceKey function
   		      *DELETE REST WHILE Style+cWarecode = lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode
   		      DELETE REST WHILE Style+cWarecode = lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode FOR;
              gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'D')
              *E300803,1 (End)

  	          SELECT STYLE
  	          IF SEEK(lcMajor+lcSepart+lcNMPart)
  			    =RLOCK ()
  			    REPLACE stk1   WITH stk1   - lawarstk[1] ,;
  			            stk2   WITH stk2   - lawarstk[2] ,;
  					    stk3   WITH stk3   - lawarstk[3] ,;
  					    stk4   WITH stk4   - lawarstk[4] ,;
  					    stk5   WITH stk5   - lawarstk[5] ,;
  					    stk6   WITH stk6   - lawarstk[6] ,;
  					    stk7   WITH stk7   - lawarstk[7] ,;
  					    stk8   WITH stk8   - lawarstk[8] ,;
  					    totstk WITH totstk - lnwrtotstk
	            UNLOCK
  		      ENDIF
  		      *E300803,1 SWK 05/25/98 Call the gfTraceKey function
              =gfTraceKey('STYLE',STYLE.Style,'M')
              *C200597,1  TMI [Start] Call a customized tracekey function for Bong Wha
              IF ASCAN(laEvntTrig , PADR('STYLE',10)) <> 0
                =gfDoTriger('ICSTYLE',PADR('STYLE',10))
              ENDIF
              *C200597,1  TMI [End  ] 
              *E300803,1 (End)
  		    ENDIF
          *C102045,1 ABD END for If Statment. [Begin]
  		  ENDIF
          *C102045,1 ABD [END]
 	    ENDFOR
	  CASE cstatus = 'S'
	    *B604323,1 ASH 03/26/2002 (Begin) Don't add the missing warehouse if not all colors.
	    IF !llAllColors
	      =SEEK(lcMajor+lcSepart+lcNonMjr+&lcwarefil..cwarecode+SPACE(10),"STYDYE")
	      SELECT StyDye
       	  IF STYDYE.Style = &lcwarefil..Style  
             REPLACE cDiscCode WITH &lcwarefil..cdisccode
       	  ENDIF
       	ELSE
       	*B604323,1 ASH 03/26/2002 (End)
          FOR lncount = 1 TO ALEN(lacolor,1)
  	  	    lcNMPart=IIF(llNoNMjrs AND lnNonSeg=0 ,'',PADR(lacolor[lnCount],18-lnStyleWid))
            IF !SEEK(lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode+SPACE(10),"STYDYE")
	  	      =SEEK(lcMajor+lcSepart+lcNMPart,"STYLE")
		      SELECT (lcwarefil)
              REPLACE ave_cost WITH style.ave_cost,;
      	              Style    WITH lcMajor+lcSepart+lcNMPart
    		  SCATTER TO larecarr
 		      SELECT stydye
 	          APPEND BLANK
	  	      GATHER FROM larecarr
  		      REPLACE dAdd_date WITH gdSysDate  ,;
		   	          cAdd_time WITH TIME()  ,;
     		          cAdd_user WITH gcUser_id
  		      *E300803,1 SWK 05/25/98 Call the gfTraceKey function
              =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'A')
              *E300803,1 (End)
	        ELSE
              *C102045,1 ABD add CdiscCode for the selected color only. [Begin]
              SELECT STYDYE
              IF SUBSTR(lcNonMjr,1,1) = '*'
          	    REPLACE cDiscCode WITH &lcwarefil..cdisccode
          	  ELSE
          	    IF STYDYE.Style = &lcwarefil..Style  
                  REPLACE cDiscCode WITH &lcwarefil..cdisccode
          	    ENDIF
              ENDIF
              *C102045,1 ABD [End]
              *** Update the warehouse records with the new cost
              =gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
              IF !SEEK(lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode,'STYINVJL')
  	  		    =SEEK(lcMajor+lcSepart+lcNMPart,"STYLE")
                SELECT STYDYE
                IF SEEK (lcMajor+lcSepart+lcNMPart+&lcwarefil..cwarecode+SPACE(10))
          	      REPLACE Ave_cost WITH style.Totcost
     		      *E300803,1 SWK 05/25/98 Call the gfTraceKey function
                  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'M')
                  *E300803,1 (End)
                ENDIF 
		      ENDIF
		    ENDIF
            *C200171 TMI [Start] Generate UPC's for new assigned WH.
            IF llNCInstl .AND. ASCAN(laEvntTrig,'GNUPCWH   ') <> 0
              *-- Defining parameters     
              lcWhCode   = &lcWareFil..CWARECODE
              lcSty      = &lcWareFil..STYLE
              llFrmAdWre = .F.
              =gfDoTriger('ICSTYLE','GNUPCWH   ')
            ENDIF 
            *C200171 TMI [End  ]
	      ENDFOR
	    ENDIF
    ENDCASE
    IF lnstyrec > 0 .AND. lnstyrec <= RECCOUNT("STYLE")
	  GO lnstyrec IN STYLE
    ENDIF
    SELECT (lcwarefil)
  ENDSCAN
ELSE
  IF laScrMode[4]
	FOR lncount = 1 TO ALEN(lacolor,1)
  	  lcNMPart=IIF(llNoNMjrs AND lnNonSeg=0,'',PADR(lacolor[lnCount],18-lnStyleWid))
	  =SEEK(lcMajor+lcSepart+lcNMPart,"STYLE")
      SELECT STYDYE
	  APPEND BLANK
      *C102045,1 ABD add CdiscCode in stydye file in case not multi warehouse. [Begin]
  	  *REPLACE Style     WITH lcMajor+lcSepart+lcNMPart,;
              cWareCode WITH laData[71],;
              Ave_Cost  WITH laData[2] ,;
              gl_link   WITH laData[19],;
 		      dAdd_date WITH gdSysDate ,;
  	  	      cAdd_time WITH TIME(),;
    	      cAdd_user WITH gcUser_id
  	  REPLACE Style     WITH lcMajor+lcSepart+lcNMPart,;
              cWareCode WITH laData[71]               ,;
              Ave_Cost  WITH laData[2]                ,;
              gl_link   WITH laData[19]               ,;
              cDiscCode WITH laData[78]               ,;
 		      dAdd_date WITH gdSysDate                ,;
  	  	      cAdd_time WITH TIME()                   ,;
    	      cAdd_user WITH gcUser_id
      *C102045,1 ABD [End]
      *E300803,1 SWK 05/25/98 Call the gfTraceKey function
      =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'A')
      *E300803,1 (End)
 	ENDFOR
  ELSE
    FOR lncount = 1 TO ALEN(lacolor,1)
  	  lcNMPart=IIF(llNoNMjrs AND lnNonSeg=0,'',PADR(lacolor[lnCount],18-lnStyleWid))
      SELECT STYDYE
	  IF !SEEK(lcMajor+lcSepart+lcNMPart+laData[71]+SPACE(10))
        APPEND BLANK
    	REPLACE Style     WITH lcMajor+lcSepart+lcNMPart,;
                cWareCode WITH laData[71]
        *E300803,1 SWK 05/25/98 Call the gfTraceKey function
        =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'A')
        *E300803,1 (End)
      ENDIF
	  =RLOCK()
      *C102045,1 ABD add CdiscCode in stydye file in case not multi warehouse. [Begin]	  
      *REPLACE Ave_Cost   WITH laData[2],;
              gl_link    WITH laData[19]
       REPLACE Ave_Cost  WITH laData[2] ,;
              gl_link    WITH laData[19],;
              cDiscCode  WITH laData[78]
       *C102045,1 ABD [End]
    	      *mod_action WITH 'MODIFIED',;
		      *mod_date   WITH gdSysDate ,;
		      *mod_time   WITH TIME()    ,;
   		  	  *mod_userid WITH gcUser_id
      UNLOCK
	  *E300803,1 SWK 05/25/98 Call the gfTraceKey function
      =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'M')
      *E300803,1 (End)
    ENDFOR
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfCalcCost
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to calculate the costing fileds.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcPColor -> Color (Non major)
*!                      lcPScale -> Scale code.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfCalcCost
PARA lcPColor,lcPScale

IF llMScale
  lnSepMins = lfvGtSclSp()
  lcPColor  = SUBSTR(lcPColor,1,lnColorWid-lnSepMins)
ENDIF
STORE 0.00 TO lncostval1,lncostval2,lncostval3,lncostval4,lncostval5
lnSclNo =IIF(SEEK('S'+lcPScale,'SCALE'),SCALE.Cnt,8)
lnNMJlen=LEN(lcMajor+lcSepart+lcPColor)
SELECT BOM
SCAN REST WHILE BOM.cItmMajor = lcMajor ;
     FOR LIKE(SUBSTR(STRTRAN(cItmMask,'*','?'),1,lnNMJlen),lcMajor+lcSepart+lcPColor) AND;
                (lcPScale $ mSizes .OR.EMPTY(mSizes) )
  lccurvar = 'lnCostVal' + Typ
  lnNoOfSizes = OCCURS(MLINE(mSizes,ATCLINE(lcPScale,mSizes)),',')
  lnNoOfSizes = lnNoOfSizes + IIF(lnNoOfSizes=0,lnSclNo,1)
  DO CASE
    CASE llMAInstld AND ( bom.ccatgtyp = 'F' OR (bom.ccatgtyp = 'T' AND bom.trim_invt) )       && FABRIC
      lcCmpClr=IIF(lnClrPnt<>0,;
              SUBSTR(cItmMask,laSegInfo[lnClrPnt,4],laSegInfo[lnClrPnt,5]),"")
	  IF AT('*',lcCmpClr)<>0  AND BOM.IClr = '******'
		IF !SEEK(SUBSTR(BOM.Item,1,7)+lcPColor,'FABRIC')
		  *--Fabric/Color ' +item)+'/'+color+' is required for costing.
		  *--It will be added to the material file,
          *--Please update the style cost sheet after adding this color.
          =gfModalGen('TRM42019B42001','DIALOG',ALLTRIM(bom.item)+'|'+ALLTRIM(lcPColor))
		  SELECT fabric
		  SEEK ALLTRIM(bom.item)
		  *--HDM B602742,1 04/19/1999[Start] Fixing bug of (Variable nmCost1 not found)
		  *SCATTER TO laFabricRec
		  SCATTER MEMVAR MEMO
		  APPEND BLANK
		  *GATHER FROM lafabricrec
		  GATHER MEMVAR MEMO
		  REPLACE Color     WITH lcPColor,;
      			  onhand    WITH 0,;
			   	  onorder   WITH 0,;
				  usage     WITH 0,;
        	      dAdd_date WITH gdSysDate,;
		          cAdd_time WITH TIME()   ,;
     		      cAdd_user WITH gcUser_id

          *--HDM B602608,1 Not all Fabric Fields Should be replaced[start]
          REPLACE nStkVal   WITH 0,;
                  ReOrder   WITH 0 ,;
                  nMatWIP   WITH 0
          *-- Get The Fabric Real Cost values [Start]
          *lnFCostVal = M.NMCOST1 + M.NMCOST2 + M.NMCOST3 + M.NMCOST4 + M.NMCOST5
          lnFCostVal = M.NMCOST1 + M.NMCOST2 + M.NMCOST3 + M.NMCOST4
          *--HDM B602742,1 04/19/1999[End]

          REPLACE NFAVE_COST  WITH lnFCostVal,;
                  NAVECSTBUY WITH lnFCostVal
          *-- Get The Fabric Real Cost values [End]

          *--HDM B602608,1 Not all Fabric Fields Should be replaced[END]
          *E300803,1 SWK 05/25/98 Call the gfTraceKey function
          *--Call TraceKey global function.
          =gfTraceKey('FABRIC',PADR(bom.item,7)+lcPColor,'A')
          *E300803,1(End)
		  SELECT BOM
	    ENDIF
		&lccurvar = &lccurvar+ROUND(bom.nBomTotqty*fabric.costbuy/fabric.conv,3)*lnNoOfSizes
	  ELSE
		IF SEEK(SUBSTR(bom.item,1,7)+bom.iclr,'FABRIC')
		  &lccurvar = &lccurvar+ROUND(bom.nBomTotqty*fabric.costbuy/fabric.conv,3)*lnNoOfSizes
		ENDIF
	  ENDIF

	CASE (ccatgtyp = 'T' AND !trim_invt) OR ccatgtyp $ 'MDP'
  	  &lccurvar = &lccurvar+bom.totcost*lnNoOfSizes

	CASE ccatgtyp = 'S'  && STYLE COMPONENT
      lcCmpClr=IIF(lnClrPnt<>0,;
              SUBSTR(Item,laSegInfo[lnClrPnt,4],laSegInfo[lnClrPnt,5]),"")

	  IF lnClrPnt<>0 OR AT('*',lcCmpClr)=0 
        lcSTToSeek=BOM.Item
        FOR I=1 TO LEN(lcIMjrPt)
          IF SUBSTR(lcSTToSeek,I,1)='*'
            lcSTToSeek=SUBSTR(lcSTToSeek,1,I-1)+SUBSTR(lcMajor+lcSepart+lcPColor,I,1)+SUBSTR(lcSTToSeek,I+1,LEN(lcIMjrPt)-I)
          ENDIF
        ENDFOR

		lnRcSv=IIF(!EOF('STYLE'),RECNO('STYLE'),0)
		IF SEEK(lcSTToSeek,'STYLE')
		  &lccurvar = &lccurvar+ROUND(bom.nBomTotqty*style.totcost,3)*lnNoOfSizes
		ENDIF
        IF lnRcSv<>0
          GOTO lnRcSv IN STYLE
	    ENDIF
	  ENDIF   

  ENDCASE
ENDSCAN

FOR lnCount = 1 TO 5
  lcCount = STR(lnCount,1)
  lncostval&lcCount = ( lncostval&lcCount / lnSclNo )
ENDFOR

lnCostPrc2=IIF(lnCostval1=0,0,(lnCostVal2/lnCostVal1)*100)
lnCostPrc3=IIF(lnCostval1=0,0,(lnCostVal3/lnCostVal1)*100)
lnCostPrc4=IIF(lnCostval1=0,0,(lnCostVal4/lnCostVal1)*100)
lnCostPrc5=IIF(lnCostval1=0,0,(lnCostVal5/lnCostVal1)*100)

*--Update the TotCost and AveCost.
laData[57] = lncostval1 + lncostval2 + lncostval3 + lncostval4 + lncostval5
laData[2]  = laData[57]

RETURN

*!*************************************************************
*! Name      : lfvNotes
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function for push button < Notes >.
*!*************************************************************
*! Calls     : NOTEPAD
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvnotes

lnAlias=SELECT()
*E301077,7 Open a notepad file.
=gfOpenFile(gcDataDir+'NotePad','NotePad','SH')
=NotePad('F',lcMajor)
IF SEEK('F'+lcMajor,'NOTEPAD')
  glUpdated = .T.
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*!  Function : lfvPackBrw
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to browse packs.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvpackbrw

lnAlias=SELECT()
lcpackkey = 'P'+lcMajor+lcNMKey
laOpFile[18]=gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLNST','SH')

SELECT SPCK_LIN
IF !SEEK( lcpackkey)
  *--No Packs found for this "+IIF(llallcolors,"Style","Style\Color")+"."
  =gfModalGen('TRM42020B42001','DIALOG',IIF(llallcolors,"Style","Style\Color"))
ELSE
  lcwindtitl = 'Style Packs'
  PRIVATE lcBrFields
  lcbrfields = "Account :8 :H='Acct#',PACK_ID :20 :H='Pack id',"+;
               "Qty1 :H='Size 1' :P='99999',"+;
               "Qty2 :H='Size 2' :P='99999',"+;
               "Qty3 :H='Size 3' :P='99999',"+;
               "Qty4 :H='Size 4' :P='99999',"+;
               "Qty5 :H='Size 5' :P='99999',"+;
               "Qty6 :H='Size 6' :P='99999',"+;
               "Qty7 :H='Size 7' :P='99999',"+;
               "Qty8 :H='Size 8' :P='99999',"+;                                                                                                         
               "TOTQTY:H='TotQty':P='9999999'"

  =ARIABROW([lcpackkey],'Style Packs',7.4, gnbrhscol1, 21, gnbrhscol2,'',;
		'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ENDIF

IF USED('spck_lin') AND laOpFile[18]
  USE IN spck_lin
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*!  Function : lfvObjLnk
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Object link ,Style picture.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvObjLnk

lnAlias=SELECT()
DO GetObj WITH 'S',laData[1]
SELECT(lnalias)
RETURN



*:*************************************************************

*##1#-#-# First folder functions. ( G E N E R A L )

*:*************************************************************
*!*************************************************************
*! Name      : lfvScale
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate scale field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvscale

lnAlias=SELECT()
laData[6]=IIF(llbrowse,' ',laData[6])
*E301209,4 Read scale depend on setup scale length.
*lcOldValue = IIF(TYPE('lcOldValue')='C',lcOldValue,SPACE(1))
lcOldValue = IIF(TYPE('lcOldValue')='C',lcOldValue,SPACE(lnScaleLen))
*E301209,4 End.
IF llMScale AND lnNonSeg>1 AND laData[6] <> lcOldValue 
  SELECT (lcColorFil)
  GO TOP
  IF !EOF()
    *--You have to remove all non majors before changing scale.
    =gfModalGen('TRM42076B42001','DIALOG')  
    laData[6] = lcoldvalue
    SELECT(lnAlias)
    RETURN
  ENDIF
ENDIF

*E301209,4 Read scale depend on setup scale length.
*lcScle = IIF(llMScale,SUBSTR(laData[6],1,1),laData[6])
lcScle = IIF(llMScale,SUBSTR(laData[6],1,lnScaleLen),laData[6])
*E301209,4 End.
=gfscalbrow(@lcScle,llMScale)
laData[6]=lcScle
llbrowse = .F.

*B604057,1 AAN[Start].
PRIVATE lcAlias
PRIVATE lcMsgTit        && Var. that hold the type of message, (Order, Inventory or Wip)
PRIVATE llNoUpdate      && Var. to know if we allow update the scale or not
lcAlias = ALIAS()
*If screen is in edit mode and scale not equal old value
IF laData[6] <> lcOldValue AND laScrMode[3]   
  lcStyle = Style.Style
  lcNonMajor = Style.CStyMajor
  *if a user select a specific color
  IF !llAllcolors
    lcseekExp = "STYLE.STYLE = lcStyle"
  ELSE
    lcseekExp = "STYLE.CStyMajor = lcNonMajor"
  ENDIF
  lnStyRecNo = RECNO('Style')
  llNoUpdate = .F.
  SELECT Style
  SCAN REST WHILE &lcSeekExp
   *If there are any transactions on this scale
   *B604057,4 AAN[Start] add check of shiped qty and also negative stock
   *IF Style.TotOrd>0 OR Style.TotStk>0 OR Style.TotWip>0 
    IF Style.TotOrd>0 OR Style.TotStk<>0 OR Style.TotWip>0 OR Style.TotShp>0
      lcMsgTit = IIF(Style.TotOrd>0,"order",IIF(Style.TotStk<>0,"inventory",IIF(Style.TotWIP>0,"WIP",IIF(Style.TotShp>0,"Shiped",""))))
      =gfModalGen('TRM42221B42000','DIALOG',lcMsgTit)  
      laData[6] = lcoldvalue
      llNoUpdate = .T.
      EXIT
    ENDIF
  ENDSCAN
  SELECT (lcAlias)
  IF BETWEEN(lnStyRecNo,1,RECCOUNT('STYLE'))
    GOTO lnStyRecNo IN Style
    IF llNoUpdate
      RETURN
    ENDIF
  ENDIF
ENDIF            
*B604057,1 AAN[End].


IF EMPTY(laData[6]) OR laData[6] = lcoldvalue
  laData[6] = lcoldvalue
  SELECT(lnalias)
  RETURN
ENDIF

llCupdate = (laData[6] <> lcOldValue)
IF llMScale
  *E301209,4 Read scale depend on setup scale length.
  *SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(lcScle,1,1) INTO ARRAY laExtSz
  SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(lcScle,1,lnScaleLen) INTO ARRAY laExtSz
  *E301209,4 End.

  IF laScrMode[4] AND lnNonSeg=1
    SELECT (lcColorFil)
    DELETE ALL 
    FOR I=1 TO ALEN(laExtSz)
      APPEND BLANK
      GATHER FROM laData FIELDS &lcScFields MEMO    
      REPLACE Style   WITH lcMajor+lcSepart+laExtSz[I],;
              cStatus WITH 'A'
    ENDFOR
  ENDIF
ENDIF
IF llAllScales
  lnCntlScl = 1
  *B602846,1 Read Ex scl description.
  *lcsize1 = ' Extended '
  *lcsize2 = 'Size Scale'
  *E301209,4 Read scale depend on setup scale length.
  *=SEEK(SUBSTR(laData[6],1,1),'ScaleHd')
  =SEEK(SUBSTR(laData[6],1,lnScaleLen),'ScaleHd')
  *E301209,4 End.
  lcsize1=SUBSTR(ScaleHd.CSCALEDES,1,10)
  lcsize2=SUBSTR(ScaleHd.CSCALEDES,11,30)
  *B602846,1 End.
  *=SEEK ('S'+laData[6],'SCALE')
  *E301209,4 Read scale depend on setup scale length.
  *=SEEK('S'+SUBSTR(laData[6],1,1),'SCALE')
  =SEEK('S'+SUBSTR(laData[6],1,lnScaleLen),'SCALE')
  *E301209,4 End.
  STORE PADL(ALLTRIM(scale.sz1),5) TO lcSz1
  STORE PADL(ALLTRIM(scale.sz2),5) TO lcSz2
  STORE PADL(ALLTRIM(scale.sz3),5) TO lcSz3
  STORE PADL(ALLTRIM(scale.sz4),5) TO lcSz4
  STORE PADL(ALLTRIM(scale.sz5),5) TO lcSz5
  STORE PADL(ALLTRIM(scale.sz6),5) TO lcSz6
  STORE PADL(ALLTRIM(scale.sz7),5) TO lcSz7
  STORE PADL(ALLTRIM(scale.sz8),5) TO lcSz8
  
  lcSclDesc='Size:'+SCALE.cScl_Desc
ELSE
  IF llMScale
    =SEEK ('S'+SUBSTR(lcNonMjr,lnColorWid-2,3),'SCALE')
  ELSE
    =SEEK ('S'+laData[6],'SCALE')  
  ENDIF
  STORE PADL(ALLTRIM(scale.sz1),5) TO lcsize1,lcSz1
  STORE PADL(ALLTRIM(scale.sz2),5) TO lcsize2,lcSz2
  STORE PADL(ALLTRIM(scale.sz3),5) TO lcsize3,lcSz3
  STORE PADL(ALLTRIM(scale.sz4),5) TO lcsize4,lcSz4
  STORE PADL(ALLTRIM(scale.sz5),5) TO lcsize5,lcSz5
  STORE PADL(ALLTRIM(scale.sz6),5) TO lcsize6,lcSz6
  STORE PADL(ALLTRIM(scale.sz7),5) TO lcsize7,lcSz7
  STORE PADL(ALLTRIM(scale.sz8),5) TO lcsize8,lcSz8
  lnScaleCnt=scale.cnt
  FOR I=1 TO lnScaleCnt
    lcI=STR(I,1)
    DIME lascale[I,2]
    lascale[I,1] = SCALE.Sz&lcI
    lascale[I,2] = I
  ENDFOR
  *--HDM B602647,1 We should not restore lnTBrk to 1 [Start]
  *lnTBrk=1
  *-- only if the country code is not England restore the variable
  IF gcContCode <> lcEngland
    lnTBrk=1
  *B603875,1 KHM 09/04/2000 (Begin) If the company was England and the
  *B603875,1                scale was changed then default lnTBrk to 1.
  ELSE  
    IF (laScrMode[3] OR laScrMode[4]) AND laData[6] <> lcOldValue
      lnTBrk = 1
    ENDIF
  *B603875,1 KHM 09/04/2000 (End)    
  ENDIF
  *--HDM B602647,1 We should not restore lnTBrk to 1 [END]
  laData[7]  = SPACE(1)
  laData[82] = SPACE(1)           && HDM
  STORE 0 TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
  STORE 0 TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x
  IF !laScrMode[2]
    STORE 0 TO laData[31],laData[32],laData[33],laData[34],laData[35],laData[36],laData[37],laData[38],laData[39]
  ENDIF
ENDIF
SELECT(lnalias)
SHOW GETS WINDOW (lcWinCh1) ONLY
=lfRefresh(lcWinCh1)
RETURN


*!*************************************************************
*! Name      : lfvPrepak
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate prepack field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvprepak

PRIVATE lcBrFields,lcFile_Ttl , lcPrpk
STORE '' TO lcPrpk
lnAlias = SELECT()

IF llbrowse OR !EMPTY(laData[7]) OR !EMPTY(laData[82])
  *-- HDM E301023,1 [START]
  IF lcPrepack = 'D'
    lcPrpk = IIF(lnPuPrepak = 1,laData[7],laData[82])
  ELSE
    IF lcPrepack = 'S'
      lcPrpk = laData[7]
    ENDIF
  ENDIF
  *-- HDM E301023,1 [END]

  = gfprepbrow(laData[6],@lcPrpk)

  IF !EMPTY(lcPrpk)
    DO CASE
      CASE lcPrepack = 'D'
        IF lnPuPrepak = 2                && Case User selected buy else make it normal bahaviour
          laData[82] = lcPrpk
          SHOW GET laData[7]  DISABLE
          SHOW GET laData[82]
        ELSE                             && SELL WAS THE CHOICE
          IF lnPuPrepak = 1
            laData[7] = lcPrpk
            SHOW GET laData[82] DISABLE
            SHOW GET laData[7]
          ENDIF
        ENDIF
      CASE  lcPrepack = 'S'       && Case we have the same prepack for buy and sell
        laData[82] = lcPrpk
        laData[7]  = lcPrpk
        SHOW GET laData[7]
    ENDCASE
    *-- HDM E301023,1 [end]
  *--HDM B602334,1[Start]
  ELSE
    laData[7] = lcPrpk
    *RETURN
  *--HDM B602334,1[end]
  ENDIF
ENDIF

*B804219,1 (Begin)
IF laScrMode[3] .AND. !EMPTY(lcPrpk) 
  *llCatRtrn =.F.
  IF !lfEnblPln()
    laData[7] = lcoldvalue
  ENDIF
ENDIF
*B804219,1 (End)

lcCurrGet = IIF(lcPrepack = 'D',IIF(lnPuPrepak=1,'laData[7]','laData[82]'),'laData[7]')
IF EMPTY(lcPrpk)
  STORE 0 TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
  STORE 0 TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x
ELSE
  lcoldval=IIF(TYPE('lcoldvalue')='C' , lcoldvalue,' ')
  *IF &lcCurrGet <> lcoldval .AND. !EMPTY(&lcCurrGet)
 	STORE  scale.pp1 TO lnprpak1,lnPrPak1x
    STORE  scale.pp2 TO lnprpak2,lnPrPak2x
  	STORE  scale.pp3 TO lnprpak3,lnPrPak3x
	STORE  scale.pp4 TO lnprpak4,lnPrPak4x
  	STORE  scale.pp5 TO lnprpak5,lnPrPak5x
	STORE  scale.pp6 TO lnprpak6,lnPrPak6x
  	STORE  scale.pp7 TO lnprpak7,lnPrPak7x
	STORE  scale.pp8 TO lnprpak8,lnPrPak8x
  *ENDIF
ENDIF

llbrowse = .F.
* SHOW GET laData[7] && HDM
SELECT(lnalias)

=lfRefresh(lcWinCh1)
RETURN

*B804219,1 (Begin)
*!*************************************************************
*! Name      : lfvTotPln
*! Developer : Adel 
*! Date      : 05/30/2001
*! Purpose   : Valid function to validate total paln qty field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example            : =lfvTotPln()
*!*************************************************************
FUNCTION lfvTotPln
IF laData[39] <> lcOldValue 
  IF !lfEnblPln()
    laData[39]=lcOldValue
  ENDIF
ENDIF
*B804219,1 (End)

*!*************************************************************
*! Name      : lfvVldPlan
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate amounts for plan fields.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcObjectNam --> Object name
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example            : =lfvVldPlan('Plan1')
*!*************************************************************
FUNCTION lfvvldplan
PARAMETERS lcobjectnam

IF EVALUATE(lcobjectnam) < 0
	*--Negative values are not allowed.
  =gfModalGen('TRM42000B42001','DIALOG')
	&lcobjectnam = lcoldvalue
	_CUROBJ      = _CUROBJ
	RETURN
ENDIF
laData[39]=laData[31]+laData[32]+laData[33]+laData[34]+laData[35]+laData[36]+laData[37]+laData[38]
SHOW GET laData[39]
RETURN

*!*************************************************************
*! Name      : lfvDomestic
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate domestic field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvdomestic

llOldMake  = laData[21]
laData[21] = (lnDomestic = 1)
IF llOldMake=laData[21]
  RETURN .F.
ENDIF

lnAlias   = SELECT()
llAbrtChg = .F.
DO WHILE .T.
  IF !llAllColors
  	*--Mixed costing methods is not allowed for different non majors of one style.
  	* Changed by ASH on 05/07/2002 for B#605866
  	*--You cannot change the domestic setting on the Non-Major level.

    =gfModalGen('TRM42022B42001','DIALOG')
    llAbrtChg = .T.
    EXIT
  ENDIF

  IF !llOldMake AND !llDetCost AND laData[45]<>0
    *--You have to zero all the foreign costs first, Can not change to domestic.
    =gfModalGen('TRM42021B42001','DIALOG')
    llAbrtChg = .T.
    EXIT
  ENDIF

  IF !laScrMode[4]
    IF ! llOldMake AND !lldetcost
      IF &lctpstyle..TotWip <> 0
        *--Quantity on Work in prosess, Cannot change to domestic.
        =gfModalGen('QRM42084B42001','DIALOG')
        llAbrtChg = .T.
        EXIT
      ENDIF
    ELSE
      *B605866,1 ASH 04/21/2002 (Begin) Don't chang the domestic if the style has WIP.
      *IF SEEK(lcMajor,'BOM')
      IF SEEK(lcMajor,'BOM') OR &lctpstyle..TotWip <> 0    
      *B605866,1 ASH 04/21/2002 (End)
	    SELECT BOM
    	IF ! llAllColors
	      LOCATE REST WHILE cItmMajor=lcMajor ;
	                    FOR LIKE(STRTRAN(LEFT(cItmMask,LEN(lcMajor+lcNMKey)),'*','?'),lcMajor+lcNMKey)
        ENDIF
	    IF FOUND()
     	  *--This style has a cost sheet, Therefore you cannot update the domestic.'
          =gfModalGen('TRM42023B42001','DIALOG','')
          llAbrtChg = .T.
          EXIT
        *B605866,1 ASH 04/21/2002 (Begin) Don't chang the domestic if the style has WIP.
        ELSE
          IF &lctpstyle..TotWip <> 0        
            =gfModalGen('QRM42084B42001','DIALOG')
            llAbrtChg = .T.
            EXIT
          ENDIF
	    ENDIF
	    *B605866,1 ASH 04/21/2002 (End)
      ENDIF
    ENDIF
  ENDIF
  EXIT
ENDDO

SELECT(lnalias)
IF llAbrtChg
  lnDomestic = lcOldValue
  laData[21] = llOldMake
  SHOW GET lnDomestic
  RETURN .F.
ENDIF


IF laData[21]
  laData[14] = 0.000
  laData[15] = 1.0000
ELSE
  *lldye = .F.
  *laData[23]='N'
ENDIF

*lcCostlbl1 = PROPER(ALLTRIM(IIF(laData[21],lcmslbl1,lcislbl1)))
lccostlbl1 = IIF(laData[21],lcmslbl1,lcislbl1)
lccostlbl2 = IIF(laData[21],lcmslbl2,lcislbl2)
lccostlbl3 = IIF(laData[21],lcmslbl3,lcislbl3)
lccostlbl4 = IIF(laData[21],lcmslbl4,lcislbl4)
lccostlbl5 = IIF(laData[21],lcmslbl5,lcislbl5)

IF laScrMode[3] AND STYLE.Make<>laData[21]
  llcupdate=.T.
ENDIF
IF laScrMode[4]
  llDetCost = IIF((laData[21] AND llMFCost) OR (!laData[21] AND llImpCost) ,.T.,.F.)  
ENDIF
SHOW GETS WINDOW (lcWinCh1) ONLY
=lfRefresh(lcWinCh1)
SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
=lfRefresh(lcWinCh5)
RETURN .T.


*!*************************************************************
*! Name    : lfvDyeFlg
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose : Valid function Dyelot flag.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvDyeFlg

*E300954,1 Allow dyelots for imported styles
IF laScrMode[3]
*IF laScrMode[3] AND laData[21]
*E300954,1(End)
 lcTpFile=IIF(!llStyMode,lctpstydye,lctpstyle)  
 IF &lcTpFile..TotWip<>0 OR &lcTpFile..TotStk<>0
   *--You cannot change the dyelot since you have quantity in WIP or Stock.
   =gfModalGen('TRM42102B42001','DIALOG')
   lldye = lcOldValue
   SHOW GET lldye
   RETURN
 ENDIF
ENDIF
laData[23]=IIF(lldye,'Y','N')
RETURN


*!*************************************************************
*! Name      : lfvFabric
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate fabric field.
*!*************************************************************
*! Calls     : FABROW
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvfabric

*--HDM B602700,1 03/25/1999[Start] Fix Bug of fabric browse button doesn't work
*--                          we have to check llbrowse Value first before return
laData[11]=IIF(llbrowse,' ',laData[11])
*--HDM B602700,1 03/25/1999[End]
IF laData[11] = lcOldValue
  RETURN
ENDIF
lnalias=SELECT()
IF EMPTY(laData[11]) AND !llbrowse
    *E300954,1 SWK 08/09/1998 Refuse to change the 
    *E300954,1                primary fabric if it is used in the style cost sheet
    IF !EMPTY(lcoldvalue)
      IF SEEK(lcMajor,'BOM')
        SELECT BOM
        SCAN WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = lcMajor
          IF BOM.ITEM = lcoldvalue
            =gfModalGen('QRM42148B42001','DIALOG')
            laData[11] = lcoldvalue
            SELECT(lnalias)
            RETURN
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    *E300954,1(End)
	lcFabric   = SPACE(20)
*	laData[23] = 'N'
ELSE
  lcFab=laData[11]
	IF (llbrowse OR !SEEK(lcFab,'FABRIC')) AND fabrow(@lcFab,'*')
	ENDIF
  laData[11]=lcFab
	IF EMPTY(laData[11])
		laData[11] = lcoldvalue
	ELSE
      *E300954,1 SWK 08/09/1998 Refuse to change the 
      *E300954,1                primary fabric if it is used in the style cost sheet
      IF !EMPTY(lcoldvalue)
        IF SEEK(lcMajor,'BOM')
          SELECT BOM
          SCAN WHILE CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR = lcMajor
            IF BOM.ITEM = lcoldvalue
              =gfModalGen('QRM42148B42001','DIALOG')
              laData[11] = lcoldvalue
              SELECT(lnalias)
              RETURN
            ENDIF
          ENDSCAN
        ENDIF
      ENDIF
      *E300954,1(End)
	  lcFabric = Fabric.Desc
*		laData[23] = IIF(!EMPTY(laData[11]) AND SEEK(laData[11],'Fabric'),fabric.cDye_flg,'N')
	ENDIF
ENDIF
SHOW GET lcFabric
IF !EMPTY(laData[11]) AND laScrMode[2]
	SHOW GET pbfabric ENABLE
ELSE
	SHOW GET pbfabric DISABLE
ENDIF
llbrowse = .F.
SELECT(lnalias)
RETURN .T.


*!*************************************************************
*! Name      : lfvSeeFab
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function see fabric details.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvseefab
PRIVATE lnAlias,lcFab,lcClr
lnAlias=SELECT()
SELECT STYLE
lcFab = Fabric
IF AT('*',lcNonMjr)<>0
  lcClr="" 
ELSE
  SELECT BOM
  =SEEK(lcMajor)
  LOCATE REST WHILE cItmMajor=lcMajor ;
                  FOR LIKE(STRTRAN(cItmMask,'*','?'),PADR(lcMajor+lcNMKey,19))
  IF FOUND()

    *B128198,1 NNA 06/14/2005 (Begin) check that the color field not empty to get the fabric/color
    *IF BOM.ICLR <> '******'
    IF BOM.ICLR <> '******' AND !EMPTY(BOM.ICLR)
    *B128198,1 NNA (End)

      lcClr=BOM.IClr
    ELSE
      llFnd=.F.
      FOR I=1 TO ALEN(laSegInfo)
        IF laSegInfo[I,3] = 'C'
          llFnd=.T.
          lcClr=SUBSTR(lcMajor+lcNMKey,laSegInfo[I,4],laSegInfo[I,5])    
          EXIT
        ENDIF
      ENDFOR
      IF !llFnd
        lcClr="" 
      ENDIF    
    ENDIF
  ELSE
    lcClr="" 
  ENDIF
ENDIF

IF !SEEK(lcFab+lcClr,'Fabric')
  *--Fabric/Color : xxxx/xxxx does not exist.
  =gfModalGen('TRM42024B42001','DIALOG',lcFab+'|'+lcClr)
ELSE
  lcFabInq="'"+FABRIC.Fabric+"','"+FABRIC.Color+"'"
  DO gpDoProg WITH 'AWRMAMATRL',.F.,'MA',lcFabInq
ENDIF
SELECT(lnAlias)
RETURN


*!*************************************************************
*! Name      : lfDefWH()
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function for default warehouse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfDefWH

IF lnDefWhs=lcOldValue
  RETURN
ENDIF

lnAlias=SELECT()

laData[71]=laDefWhs[lnDefWhs,1]
llfound    = .F.
SELECT (lcwarefil)
IF !EMPTY(laData[71])
	*-- Check the existance of the new added record.
    LOCATE FOR cWareCode = laData[71] .AND. cstatus <> 'D'
	IF ! FOUND()
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    IF gfModalGen('TRM42025B42006','DIALOG',lcMajor+'|'+laData[71]) = 1
  	  glUpdated = .T.
	  APPEND BLANK
      *C102045,1 ABD add the cDiscCode to the new location that add to style. [Begin]
      *REPLACE STYLE     WITH lcMajor ,;
               dyelot    WITH SPACE(10) ,;
               cwarecode WITH laData[71],;
               gl_link   WITH 'DEFDEF',;
               cstatus   WITH 'A' 
      *B605958,1 ASH 06/02/2002 (Begin) Replace the gl_link with the data from screen not 'DEFDEF'
      *REPLACE STYLE     WITH lcMajor   ,;
               dyelot    WITH SPACE(10) ,;
               cwarecode WITH laData[71],;
               gl_link   WITH 'DEFDEF'  ,;
               cstatus   WITH 'A'       ,; 
               cDiscCode WITH laData[78]
      REPLACE STYLE     WITH lcMajor   ,;
              dyelot    WITH SPACE(10) ,;
              cwarecode WITH laData[71],;
              gl_link   WITH laData[19],;
              cstatus   WITH 'A'       ,; 
              cDiscCode WITH laData[78]                 
      *B605958,1 ASH 06/02/2002 (End)
      *C102045,1 ABD [End]
    ELSE
      lnDefWhs=lcOldValue    
      laData[71]=IIF(lnDefWhs<>0,laDefWhs[lnDefWhs,1],' ')
    ENDIF
		SHOW GET lnDefWhs ENABLE
  ENDIF
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate price fields.
*!*************************************************************
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcObjChr -> To determine which price field
*!                      llExtPCll-> .T. for not update file.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example            : =lfvPrice('A'.T.)
*!*************************************************************
FUNCTION lfvprice
PARAMETERS lcobjchr,llExtPCll


*--For Retail price.
IF lcobjchr='R'
  *B802387,1 TAK 06/30/1999 (Begin) Moved the if statement inside the above if statement
  *B802387,1                        to Fix the bug of not saving the price levels in the edit mode
  IF !llExtPCll AND laData[60] = lcOldValue
    RETURN
  ENDIF
  *B802387,1 TAK 06/30/1999 (End)

  IF !llExtPCll AND llAllColors AND !EOF(lcColorFil) AND lcOldValue<>0 AND laData[60]<>lcOldValue
    *--This price will be the default price for all color,Do you wish to continue? y/n
    IF gfModalGen('TRM42145B42002','DIALOG',+'price'+'|'+lcNMjrTl)=2
      laData[60] = lcoldvalue
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF

  IF laData[60] = 0
    laData[81] = 0.00
    IF (laScrMode[3] OR laScrMode[4])
      lcpdspmode = IIF(laData[57]<>0 AND !llExtPCll,'ENABLE','DISABLE')
      SHOW GET laData[81] &lcpdspmode
    ELSE
      SHOW GET laData[81] DISABLE    
    ENDIF
  ELSE
	IF laData[60] < 0
      *--Negative values are not allowed.
      =gfModalGen('TRM42000B42001','DIALOG')
	  laData[60] = IIF(!llExtPCll,lcoldvalue,0)
	  _CUROBJ = _CUROBJ
	ELSE
  	  IF laData[57] <> 0 
        lnSubPrice = ROUND(laData[60],2)
        lnSubTCost = ROUND(laData[57],2)
        lnRotSub   = IIF(llStyMark,lnSubTCost,lnSubPrice)
        *B603713,1 AMH [Start] let ladata[81] bounded by max and min value of its field
	    *laData[81] = ((lnSubPrice-lnSubTCost) / lnRotSub )*100
	    laData[81] = MAX(MIN(((lnSubPrice-lnSubTCost) / lnRotSub )*100,999999),-99999)
        *B603713,1 AMH [End]
	  ENDIF
	  SHOW GET laData[81] DISABLE
	ENDIF
  ENDIF
  IF !llExtPCll
    lnAlias=SELECT()
    SELECT (lcColorFil)  
    lnSvRc=IIF(!EOF(),RECNO(),0)
    SEEK lcMajor+lcNMKey
    REPLACE REST WHILE Style=lcMajor+lcNMKey ;
            nSugRetPri WITH laData[60],;
            NMarkRet   WITH laData[81]
    IF lnSvRc<>0
      GOTO lnSvRc
    ENDIF
    SELECT(lnalias)
  ENDIF  
  RETURN
ENDIF


*--For Price A,B,C.
lnObjCout  = IIF(lcobjchr='B',1,IIF(lcobjchr='C',2,0))
lcPriceObj = 'laData[24+lnObjCout]'
lcMarkpObj = 'laData[54+lnObjCout]'
lnPriceVal = EVALUATE(lcPriceObj)

*B802387,1 TAK 06/30/1999 (Begin) Changed the if statement to Fix the bug 
*B802387,1                of not saving the price levels in the edit mode
*IF !llExtPCll AND llAllColors AND !EOF(lcColorFil) AND lcOldValue<>0 AND lnPriceVal<>lcOldValue
IF !llExtPCll AND llAllColors AND !EOF(lcColorFil) AND lnPriceVal<>lcOldValue
*B802387,1 TAK 06/30/1999 (End)

  *--This price will be the default price for all color,Do you wish to continue? y/n
  IF gfModalGen('TRM42145B42002','DIALOG',+'price'+'|'+lcNMjrTl)=2
    &lcPriceObj = lcoldvalue
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
ENDIF

*B123215,1 MHM 07/06/2004 fix bug that changing primary fabric overwrites price A[Start]
IF TYPE('lcOldValue') = 'N' AND lnPriceVal = lcOldValue
  llExtPCll = .T.
ENDIF
*B123215,1 MHM 07/06/2004 [End]

IF lnPriceVal = 0
	&lcMarkpObj = 0.00
    IF (laScrMode[3] OR laScrMode[4])
      lcpdspmode = IIF(laData[57]<>0 AND &lcPriceObj=0 AND !llExtPCll,'ENABLE','DISABLE')
      SHOW GET &lcmarkpobj &lcpdspmode
    ELSE
      SHOW GET &lcmarkpobj DISABLE    
    ENDIF
ELSE
	IF lnpriceval < 0
 	 *--Negative values are not allowed.
      =gfModalGen('TRM42000B42001','DIALOG')
	  &lcPriceObj = IIF(!llExtPCll,lcoldvalue,0)
	  _CUROBJ = _CUROBJ
	ELSE
  	  IF laData[57] <> 0 
        lnSubPrice = ROUND(lnPriceVal,2)
        lnSubTCost = ROUND(laData[57],2)
        lnRotSub   = IIF(llStyMark,lnSubTCost,lnSubPrice)
        *B603713,1 AMH [Start] let lcmarkpobj bounded by max and min value of its field
	    *&lcmarkpobj = ((lnSubPrice-lnSubTCost) / lnRotSub )*100
	    &lcmarkpobj = MAX(MIN(((lnSubPrice-lnSubTCost) / lnRotSub )*100,999999),-99999)
	    *B603713,1 AMH [END]
	  ENDIF
	  SHOW GET &lcMarkpObj DISABLE
	ENDIF
ENDIF

IF !llExtPCll
  lnAlias=SELECT()
  SELECT (lcColorFil)  
  lnSvRc=IIF(!EOF(),RECNO(),0)
  SEEK lcMajor+lcNMKey
  REPLACE REST WHILE Style=lcMajor+lcNMKey ;
          Price&lcobjchr WITH &lcPriceObj ,;
          Mark&lcobjchr  WITH &lcMarkpobj
  IF lnSvRc<>0
    GOTO lnSvRc
  ENDIF
  SELECT(lnalias)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfvMarkup
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate markup fields.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcObjChr -> To determine which markup field
*!                      llExtPCll-> .T. for not update file.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvmarkup('A',.T.)
*!*************************************************************
FUNCTION lfvmarkup
PARAMETERS lcobjchr,llExtPCll

*--For Retail price.
IF lcobjchr='R'
  IF laData[81] <> 0

    IF !llExtPCll AND llAllColors AND !EOF(lcColorFil) AND lcOldValue<>0 AND laData[81]<>lcOldValue
      *--This price will be the default price for all color,Do you wish to continue? y/n
      IF gfModalGen('TRM42145B42002','DIALOG',+'price'+'|'+lcNMjrTl)=2
        laData[81] = lcoldvalue
        _CUROBJ = _CUROBJ
        RETURN
      ENDIF
    ENDIF

    IF laData[81] < 0
      *--Negative values are not allowed.
       =gfModalGen('TRM42000B42001','DIALOG')
       laData[81] = IIF(!llExtPCll,lcoldvalue,0)
      _CUROBJ    = _CUROBJ
    ELSE
	  laData[60]=IIF(llStyMark,(laData[57]*(laData[81]/100))+laData[57],;
	    (laData[57])/(1-(laData[81]/100)))
	  SHOW GET laData[60]
      IF laData[60]<>0
        SHOW GET laData[81] DISABLE
      ENDIF
	ENDIF
  ENDIF

  IF !llExtPCll
    lnAlias=SELECT()
    SELECT (lcColorFil)  
    lnSvRc=IIF(!EOF(),RECNO(),0)
    SEEK lcMajor+lcNMKey
    REPLACE REST WHILE Style=lcMajor+lcNMKey ;
            nSugRetPri WITH laData[60],;
            NMarkRet   WITH laData[81]
    IF lnSvRc<>0
      GOTO lnSvRc
    ENDIF
    SELECT(lnalias)
  ENDIF
  RETURN
ENDIF

  *--For Price A,B,C.
  lnObjCout  = IIF(lcobjchr='B',1,IIF(lcobjchr='C',2,0))
  *B802556,1 (Start) ALB 08/30/1999 Prevented the MarkUp to have value > 99
  *B802556,1                        if MarkUp method is Bottom Up
  IF laData[54+lnObjCout] > 99 .AND. !llStyMark
    = gfModalGen('TRM42200B00000','DIALOG')
    laData[54+lnObjCout]=lcoldvalue
    SHOW GET laData[54+lnObjCout]
    _CUROBJ = _CUROBJ
    RETURN
  ENDIF
  *B802556,1 (End)
  lcPriceObj = 'laData[24+lnObjCout]'
  lcMarkpObj = 'laData[54+lnObjCout]'
  lnmarkpval = EVALUATE(lcmarkpobj)

IF lnmarkpval <> 0
  IF !llExtPCll AND llAllColors AND !EOF(lcColorFil) AND lcOldValue<>0 AND lnmarkpval<>lcOldValue
    *--This price will be the default price for all color,Do you wish to continue? y/n
    IF gfModalGen('TRM42145B42002','DIALOG',+'price'+'|'+lcNMjrTl)=2
      &lnmarkpval = lcoldvalue
      _CUROBJ = _CUROBJ
      RETURN
    ENDIF
  ENDIF

  IF lnmarkpval < 0
	*--Negative values are not allowed.
    =gfModalGen('TRM42000B42001','DIALOG')
	&lcmarkpobj = IIF(!llExtPCll,lcoldvalue,0)
	_CUROBJ     = _CUROBJ
  ELSE

	&lcpriceobj=IIF(llStyMark,(laData[57]*(lnmarkpval/100))+laData[57],;
		(laData[57])/(1-(lnmarkpval/100)))
	SHOW GET &lcpriceobj
    IF &lcPriceObj<>0
     SHOW GET &lcMarkpObj DISABLE
   ENDIF
  ENDIF
ENDIF 

IF !llExtPCll
  lnAlias=SELECT()
  SELECT (lcColorFil)  
  lnSvRc=IIF(!EOF(),RECNO(),0)
  SEEK lcMajor+lcNMKey
  REPLACE REST WHILE Style=lcMajor+lcNMKey ;
          Price&lcobjchr WITH &lcPriceObj ,;
          Mark&lcobjchr  WITH &lcMarkpobj
  IF lnSvRc<>0
    GOTO lnSvRc
  ENDIF
  SELECT(lnalias)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvForeign
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate foreign price field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvforeign

IF laData[14] < 0.00
	*--Foreign price should be greater than or equal zero.
  =gfModalGen('TRM42026B42001','DIALOG')
	laData[14] = lcoldvalue
	_CUROBJ = _CUROBJ
ENDIF

IF !llDetCost AND lncostval1=0
  lncostval1 = IIF( laData[15]=0, 0, (laData[14] / laData[15]) )
  SHOW GET lncostval1
  =lfvCost('1')
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvExchang
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate exchange rate field.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvexchang

IF laData[15] < 0.00
	*--Exchange rate should be greater than zero.
  =gfModalGen('TRM42027B42001','DIALOG')
	laData[15] = lcoldvalue
	_CUROBJ = _CUROBJ
ENDIF

IF ! llDetCost AND lncostval1=0
  lncostval1 = IIF( laData[15]=0, 0, (laData[14] / laData[15]) )
  SHOW GET lncostval1
  =lfvCost('1')
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvFrnPrc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function foreign price push button.
*!*************************************************************
*! Call : lfDispBrow(),lfwBrows(),lfvClose(),lfvProceed(),
*!        lfvRemove(),lfvModify(),lfvInsert(),lfvCrCod2().
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvfrnprc
PRIVATE lcBrFields

lnAlias=SELECT()
lcBrowTitl = 'Prices'
lcCurrCode = SPACE(5)
llInsert   = .F.
STORE 0.0 TO lnPriceA,lnPriceB,lnPriceC,lnRetPric

SELECT (lcTmpStyPr)
GO TOP
SET ORDER TO TAG lctmppr1
lcDisMode=IIF(EOF(),'DISABLE','ENABLE')
lnFpRecNo = 1
lcFrnPrTtl = "Foreign price"
PUSH KEY
ON KEY
=lfTrapFp()
DO (gcScrDir+gcWinAppl+"\FRNPRC.SPX")
ON KEY
POP KEY
SELECT (lcTmpStyPr)
SET ORDER TO TAG (lcTmpStyPr)
SELECT(lnAlias)
RETURN

*****************
FUNCTION lfTrapFp
ON KEY LABEL TAB        DO lpFPtab
ON KEY LABEL backtab    DO lpFPshifttab
ON KEY LABEL esc        DO lpFPesc
*****************
PROCEDURE lpFPesc
ACTIVATE WINDOW FRNPRC1
_CUROBJ = OBJNUM(pbFPclose)
KEYBOARD CHR(17)+CHR(13)
*****************
PROCEDURE lpFPtab
IF laScrMode[2]
	IF WONTOP(lcBrowTitl)
		_CUROBJ=OBJNUM(pbFPclose)
		ACTIVATE WINDOW FRNPRC1
	ELSE
		ACTIVATE WINDOW (lcBrowTitl)
	ENDIF
ELSE
	IF WONTOP(lcBrowTitl)
		_CUROBJ=OBJNUM(pbfpInsert)
		ACTIVATE WINDOW FRNPRC1
	ELSE
		IF _CUROBJ = OBJNUM(pbFPclose)
  		ACTIVATE WINDOW (lcBrowTitl)
		ELSE
			_CUROBJ= _CUROBJ + 1
		ENDIF
	ENDIF
ENDIF
RETURN
**********************
PROCEDURE lpFPshifttab
IF laScrMode[2]
	IF WONTOP(lcBrowTitl)
		_CUROBJ=OBJNUM(pbFPclose)
		ACTIVATE WINDOW FRNPRC1
	ELSE
		ACTIVATE WINDOW (lcBrowTitl)
	ENDIF
ELSE
	IF WONTOP(lcBrowTitl)
		_CUROBJ=OBJNUM(pbFPclose)
		ACTIVATE WINDOW FRNPRC1
	ELSE
		IF _CUROBJ = OBJNUM(pbfpInsert)
		ACTIVATE WINDOW (lcBrowTitl)
		ELSE
			_CUROBJ= _CUROBJ - 1
		ENDIF
	ENDIF
ENDIF
RETURN




*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : create style forign price browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfDispBrow

lnFPRecNo  = RECNO(lcTmpStyPr)
lcBrowTitl = 'Prices'

lcBrFields = "lcMarker=IIF(lnFPRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
             "cCurrCode :R :H='Currency Code':20,"+;
             "PriceA    :R :H='Price A':15,"+;
  	         "PriceB    :R :H='Price B':15,"+;
             "PriceC    :R :H='Price C':15,"+;
             "nSugRetPri :R :H='Retail Price' :15"
SET ORDER TO TAG lctmppr1
BROWSE FIELDS &lcBrFields;
       WINDOW    FRNPRC0;
       WHEN lfwBrows();
       IN WINDOW FRNPRC;
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       NOEDIT;
       SAVE;
       TITLE lcBrowTitl

*!*************************************************************
*! Name      : lfwBrows
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Refresh record pointer.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfwBrows

lnFPRecNo  = RECNO(lcTmpStyPr)
SHOW WINDOW (lcBrowTitl) REFRESH

*!*************************************************************
*! Name      : lfvclose
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Validation on push button close.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvClose

ACTIVATE WINDOW FRNPRC1
_CUROBJ = OBJNUM(pbfpClose)
KEYBOARD '{ENTER}'


*!*************************************************************
*! Name      : lfvProceed
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : validation on push button proceed.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvProceed

SELECT (lcTmpStyPr)
lcStyle=lcMajor+lcNMKey

IF llInsert
  SET ORDER TO TAG (lcTmpStyPr)
  =SEEK(lcStyle)
  LOCATE REST WHILE Style=lcStyle FOR cCurrCode=lcCurrCode
  IF FOUND()
    *--Style :'+xxxxx prices are already defined in '+lcCurrCode,'\!\<Modify;\?\<Cancel.
    lnChoice=gfModalGen('TRM42028B42004','DIALOG',ALLTRIM(lcStyle)+'|'+lcCurrCode)
    IF lnChoice = 1
      llInsert=.F.
    ELSE
      SET ORDER TO TAG lctmppr1
      _CUROBJ = OBJNUM(lcCurrCode)
      RETURN
    ENDIF  
  ENDIF
  SET ORDER TO TAG lctmppr1
ENDIF  

IF llInsert
  lcStatus = 'A'
  ** Ash: A bug fixed with the release. 01/29/2002
  SET ORDER TO TAG (lcTmpStyPr)
  ** Ash: End
  SET DELE OFF
  =SEEK(lcStyle)
  LOCATE REST WHILE Style=lcStyle FOR cCurrCode=lcCurrCode
  IF FOUND()
    RECALL
  ELSE
    APPEND BLANK
  ENDIF
  SET DELE ON
ELSE
  IF cStatus = 'S'
    lcStatus = 'M'
  ELSE
    IF cStatus = 'A'
      lcStatus = 'A'
    ENDIF
  ENDIF    
ENDIF  
REPLACE Style     WITH lcStyle,;
        cCurrCode WITH lcCurrCode,;
        PriceA    WITH lnPriceA,;
        PriceB    WITH lnPriceB,;
        PriceC    WITH lnPriceC,;
        nSugRetPri WITH lnRetPric,;
        cStatus   WITH lcStatus
          
llInsert =.F.
glUpdated=.T. 
** Ash: A bug fixed with the release. 01/29/2002
SET ORDER TO TAG lctmppr1
** Ash End
lnFPRecNo  = RECNO(lcTmpStyPr)
SHOW WINDOW (lcBrowTitl) REFRESH
CLEAR READ

*!*************************************************************
*! Name      : lfvRemove
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : validation on push button remove.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvRemove

lcStyle=lcMajor+lcNMKey
lcCurry=cCurrCode
*--Are you sure you want to remove this record','\<Yes;\?\<No'
IF gfModalGen('TRM42029B42002','DIALOG')=1
  SET ORDER TO TAG (lcTmpStyPr)
  =SEEK(lcStyle)
  REPLACE REST cStatus WITH IIF(cStatus = 'A','S','D') ;
          WHILE Style=lcStyle FOR cCurrCode=lcCurry
  =SEEK(lcStyle)
  DELETE REST WHILE Style=lcStyle FOR cCurrCode=lcCurry
  SET ORDER TO TAG lctmppr1
  glUpdated=.T.    
  GO TOP
  IF EOF()
    SHOW GET pbfpModify DISABLE
    SHOW GET pbfpRemove DISABLE
  ELSE
    lnFPRecNo  = RECNO(lcTmpStyPr)  
  ENDIF
  SHOW WINDOW (lcBrowTitl) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvModify
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : validation on push button modify.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvModify

lcObjDip1 = 'DISABLE'
lcObjDip2 = 'ENABLE'
lcWindTitl="  Modify Record "
GOTO lnFpRecNo
lcCurrCode = cCurrCode 
lnPriceA   = PriceA
lnPriceB   = PriceB
lnPriceC   = PriceC
lnRetPric  = nSugRetPri
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\FRNPRCA.SPX")
POP KEY
SHOW WINDOW (lcBrowTitl) REFRESH
RETURN


*!*************************************************************
*! Name      : lfvInsert
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : validation on push button insert.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvInsert

lcObjDip1 = 'ENABLE'
lcObjDip2 = 'DISABLE'
lcWindTitl =" Add a new record "

llInsert   = .T.
lcCurrCode = SPACE(5)
STORE 0 TO lnPriceA, lnPriceB, lnPriceC ,lnRetPric
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\FRNPRCA.SPX")
POP KEY
lnSvRc=IIF(EOF(),0,RECNO())
GO TOP
IF EOF()
  SHOW GET pbfpModify DISABLE
  SHOW GET pbfpRemove DISABLE
ELSE
  SHOW GET pbfpModify ENABLE
  SHOW GET pbfpRemove ENABLE
  IF lnSvRc<>0
    GOTO lnSvRc
  ENDIF
ENDIF

lnFpRecNo  = RECNO(lcTmpStyPr)  
SHOW WINDOW (lcBrowTitl) REFRESH
llInsert = .F.



*!*************************************************************
*! Name      : lfvCrCod2
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Curr Code validation.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvCrCod2


IF !EMPTY(lcCurrCode)
  *-- HDM[START]
  IF !laOpFile[2]
    *--HDM B602447,1 Check if not opened[END]
    laOpFile[2] = gfOpenFile(gcSysHome+'SycCurr' ,'cCurrCode','SH')
  ENDIF
  IF !laOpFile[17]
    laOpFile[17] = gfOpenFile(gcSysHome+'SycExch','Currency','SH')
  ENDIF
  *-- HDM[END]

  =gfcurrbrow(@lcCurrCode)
  IF "?" $ lcCurrCode OR EMPTY(lcCurrCode)
    lcCurrCode = SPACE(5)
    _CUROBJ = OBJNUM(lcCurrCode)
  ENDIF
ENDIF  

SHOW GET ibCurrBrow ENABLE
SHOW GET lcCurrCode ENABLE

IF lcCurrCode=gcBaseCurr
  *--This is the base currency, Please retry.
  =gfModalGen('TRM42082B42001','DIALOG')
  lcCurrCode = SPACE(5)
  _CUROBJ = OBJNUM(lcCurrCode)
ENDIF

IF EMPTY(lcCurrCode)
  SHOW GET lnPriceA   DISABLE
  SHOW GET lnPriceB   DISABLE
  SHOW GET lnPriceC   DISABLE
  SHOW GET lnRetPric  DISABLE
  SHOW GET pbfpProceed DISABLE
ELSE
  SHOW GET ibCurrBrow DISABLE
  SHOW GET lcCurrCode DISABLE
  SHOW GET lnPriceA   ENABLE
  SHOW GET lnPriceB   ENABLE
  SHOW GET lnPriceC   ENABLE
  SHOW GET lnRetPric  ENABLE
  SHOW GET pbfpProceed ENABLE
ENDIF

RETURN

*!*************************************************************
*! Name      : lfvQualty
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function Quality popup.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvQualty

IF lnGrade=lcOldValue
  RETURN
ENDIF
laData[72]=STR(lnGrade,1)
J=0
FOR I=1 TO 3
  IF lnGrade<>I
    J=J+1
    laDamQlt[J,1]=laStyGrd[I,1]
    laDamQlt[J,2]=laStyGrd[I,2]
  ENDIF
ENDFOR
lnDamQlt = 1
STORE '' TO lcRetSty,laData[70],laData[73]
RETURN



*:*************************************************************

*##2#-#-# Second folder functions. ( C U T  &  S O L D )

*:*************************************************************
*!*************************************************************
*! Name      : lfvWHouse
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function for push button < Warehouse >.
*!*************************************************************
*! Calls     : lfTrapKy
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvwhouse

lnAlias=SELECT()
lcbrow_ttl = lcMjrTtl+':'+ALLTRIM(lcMajor)+IIF(llallcolors,'',lcNMjrTl+':'+ALLTRIM(lcNonMjr))+' Locations'
lcwindtitl  ='Locations'

IF (laScrMode[3] or laScrMode[4])
  lcRestoreWh =IIF(lcSysType='P',lawareh[1,1],'All   ')
ELSE
  lcRestoreWh =lcwarecode
ENDIF
SELECT (lcwarefil)
GO TOP
SET RELATION TO &lcwarefil..cwarecode INTO warehous ADDITIVE
PUSH KEY
ON KEY
=lftrapky()
*E300889,1 TAK If Point of sale setup view only the current location.
*E300889,1 Save the current mode.
IF lcSysType='P'
  lnScMode  = ASCAN(laScrMode,.T.)
  laScrMode = .F.
  laScrMode[2] = .T.
ENDIF
*E300889,1 End.
lcDefWare  = laData[71]
lcGlWareHs = 'DEFDEF'

*C102045,1 ABD define the new Popup for Cdiscode in the location screen. [Begin]
= gfwCodePop ( @laNwCodinf, "CDISCCODE"  ,"L" )
lcDisCode = &lcwarefil..cDiscCode
lnWarhDisc = ASCAN('laWarhDisc',lcDisCode)
lnWarhDisc = IIF(lnWarhDisc=0,1,ASUBSCRIPT(laWarhDisc,lnWarhDisc,1))
*C102045,1 ABD [End]

*PRIVATE lcwarecode
SHOW GETS WINDOW (lcWinCh2) DISABLE ONLY
DO (gcScrDir+gcWinAppl+"\WAREHS.SPX")
ON KEY
POP KEY
*E300889,1 Restore the current mode.
IF lcSysType='P'
  laScrMode = .F.
  laScrMode[lnScMode] = .T.
ENDIF
*E300889,1 End.
SELECT (lcwarefil)
SET RELATION TO
*-This to refresh the style/color default warehouse after closing the warehouse screen.
SHOW GET laData[71]

IF (laScrMode[3] OR laScrMode[4] )
  *-Initialize the array.
  DECLARE lawareh[1,2]
  lawareh[1,1]  = IIF(lcSysType = 'P',laDefWhs[1,1],'All   ')
  lawareh[1,2]  = 1
  *-- HDM B602631,1 Assign The Default warehouse case POS[Start]
  *lcwarecode    = lawareh[1,1]
  lcWareCode = IIF(lcSysType='P',lawareh[1,1],'All   ')
  *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
  *-- HDM B602631,1 Assign The Default warehouse case POS[End]
  i=1
  SCAN FOR cStatus<>'D'
    IF ASCAN(lawareh,cwarecode)=0
      i=i+1
      DECLARE lawareh[I,2]
      lawareh[I,1]  = cwarecode
      lawareh[I,2]  = i
    ENDIF
  ENDSCAN
  IF llmultiloc
    
    =lfvviewloc()
  ELSE
    llstymode = IIF(lcSysType='P',.F.,.T.)   && Style Mode (Style/warehouse level).
    SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
    =lfRefresh(lcWinCh2)
  ENDIF 
ENDIF
lcWareCode=lcRestoreWh
SHOW GET lcwarecode
=lfActFolder()
SELECT(lnAlias)
RETURN



*!*************************************************************
*! Name      : lfActBrow
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to activate warehouse window.
*!*************************************************************
*! Calls     : lfwWareBrs
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfactbrow

SELECT (lcwarefil)
lcwbrfields = "cWareCode :H= 'Location'  :20 :R," +;
          	 "lcWareDesc=Warehous.cDesc:H= 'Description' :50:R" +;
             IIF(lllink_gl,',GL_Link:H="GL Link" :10 :R','')

BROWSE FIELDS &lcwbrfields;
	LOCK 0   ;
	NOAPPEND ;
	NOCLEAR  ;
	NODELETE ;
	NOEDIT   ;
	NOMENU   ;
	NOWAIT   ;
	SAVE     ;
	WHEN lfwwarebrs()   ;
	FOR  cstatus <> 'D' ;
	TITLE lcbrow_ttl    ;
	WINDOW warehs_0 IN WINDOW warehs

=lfwwarebrs()
IF laScrMode[2]
	lcddspmode = 'DISABLE'
	SHOW GET cbsetdef DISABLE
ELSE
	LOCATE FOR &lcwarefil..cstatus <> 'D'
	IF FOUND()
		lcddspmode = 'ENABLE'
		IF llallcolors
			IF laScrMode[4] .OR. llacsdefwr
				IF cbsetdef
					SHOW GET cbsetdef DISABLE
				ELSE
					SHOW GET cbsetdef ENABLE
				ENDIF
			ELSE
				SHOW GET cbsetdef DISABLE
			ENDIF
		ELSE
			IF cbsetdef
				SHOW GET cbsetdef DISABLE
			ELSE
				SHOW GET cbsetdef ENABLE
			ENDIF
		ENDIF
	ELSE
		lcddspmode = 'DISABLE'
		SHOW GET cbsetdef DISABLE
	ENDIF
ENDIF
SHOW GET lcglwarehs &lcddspmode
SHOW GET ibwargllnk &lcddspmode
SHOW GET pbdelwar   &lcddspmode
*C102045,1 ABD show get the new popup[Begin]
SHOW GET lnWarhDisc &lcddspmode
*C102045,1 ABD [End]

RETURN

*!*************************************************************
*! Name      : lfwWareBrs
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : When warehouse browse function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfwwarebrs

cbsetdef   = IIF(&lcwarefil..cwarecode = lcdefware,.T.,.F.)
IF laScrMode[2]
	SHOW GET cbsetdef DISABLE
ELSE
	IF llallcolors
		IF laScrmode[4] .OR. llacsdefwr
			IF !cbsetdef
				SHOW GET cbsetdef ENABLE
			ELSE
				SHOW GET cbsetdef DISABLE
			ENDIF
		ELSE
			SHOW GET cbsetdef DISABLE
		ENDIF
	ELSE
		IF cbsetdef
			SHOW GET cbsetdef DISABLE
		ELSE
			SHOW GET cbsetdef ENABLE
		ENDIF
	ENDIF
ENDIF

lcglwarehs = &lcwarefil..gl_link
=lfRefresh('WAREHS_1')
SHOW GET lcglwarehs

*C102045,1 ABD show get for the Popup. [Begin]
lcDisCode = &lcwarefil..cDiscCode
lnWarhDisc = ASCAN('laWarhDisc',lcDisCode)
lnWarhDisc = IIF(lnWarhDisc=0,1,ASUBSCRIPT(laWarhDisc,lnWarhDisc,1))
SHOW GET lnWarhDisc
*C102045,1 ABD [End]

RETURN


*!*************************************************************
*! Name      : lfvNewWar
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to assign a warehouse to the style\color.
*!*************************************************************
*! Calls     : gfBrowWare
*!             lfwWareBrs
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvnewwar

*B603670,1 (Begin) Only Finished goods inventory locations can be selected.
*lcwarecod = gfbrowware(.T.)
lcwarecod  = gfbrowware(.T.,.F.,.F.,.F.,.F.,'S')
*B603670,1 (End)
SELECT (lcwarefil)
IF EMPTY(lcwarecod)
  RETURN
ENDIF
*-- Check the existance of the new added record.
LOCATE FOR cwarecode = lcwarecod
IF FOUND() AND cstatus <> 'D'
  *--This warehouse is already assigned to this style.
  =gfModalGen('TRM42030B42001','DIALOG')
  RETURN
ENDIF
IF FOUND() AND cstatus = 'D'
  REPLACE cStatus WITH 'A'
ELSE
  llCUpdate=.T.
  *B604323,1 ASH 03/26/2002 (Begin) Fix bug of not assigning another location to the same style.
  APPEND BLANK
  *B604323,1 ASH 03/26/2002 (End)
  *C102045,1 ABD add default discode. [Begin]
  *REPLACE STYLE     WITH lcMajor  ,;
           Dyelot    WITH SPACE(10) ,;
       	   cwarecode WITH lcwarecod ,;
           Desc      WITH Style.Desc,;
           cStatus   WITH 'A'       ,;
           ave_cost  WITH style.Ave_cost

  REPLACE STYLE     WITH lcMajor          ,;
          Dyelot    WITH SPACE(10)        ,;
          cWareCode WITH lcwarecod        ,;
          Desc      WITH Style.Desc       ,;
          cStatus   WITH 'A'              ,;
   		  ave_cost  WITH style.Ave_cost   ,;
   		  cDiscCode WITH lcDefCod
  *C102045,1 ABD [End]
ENDIF

glUpdated = .T.
SHOW GET lcglwarehs ENABLE
SHOW GET ibwargllnk ENABLE
SHOW GET pbdelwar   ENABLE
IF llallcolors
	IF laScrMode[4] .OR. llacsdefwr
		IF !cbsetdef
			SHOW GET cbsetdef ENABLE
		ELSE
			SHOW GET cbsetdef DISABLE
		ENDIF
	ELSE
		SHOW GET cbsetdef DISABLE
	ENDIF
ELSE
	IF cbsetdef
		SHOW GET cbsetdef DISABLE
	ELSE
		SHOW GET cbsetdef ENABLE
	ENDIF
ENDIF
SELECT (lcwarefil)
GO TOP
ACTIVATE WINDOW (lcbrow_ttl) IN WINDOW warehs
SHOW WINDOW (lcbrow_ttl) REFRESH
=lfwwarebrs()
_CUROBJ = OBJNUM(lcglwarehs)
RETURN

*!*************************************************************
*! Name      : lfvDelWar
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to delete a warehouse from style\color.
*!*************************************************************
*! Calls     : lfwWareBrs
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvdelwar

SELECT (lcwarefil)
IF cStatus = 'D'
  LOCATE FOR cStatus <> 'D'
ENDIF  
IF !EOF()
  IF !cbsetdef
	*--Are you sure you want to remove (cwarecode) warehouse ?" ,'\<Yes;\!\<No'.
    lcchoice = gfModalGen('TRM42031B42002','DIALOG',ALLTRIM(cwarecode))
	IF lcchoice = 1
	  *-- Check if there is any stock or ordered quantities in
	  *-- this warehouse, for this style\color.
	  IF llallcolors
	    *B607286,1 ABD - Call function to calculate the qty for every field. [Begin]
		*lntotstk = &lcwarefil..totstk
		*lntotord = &lcwarefil..totord
        *lnTotWip = &lcwarefil..totWip
		lntotstk = lfCalculte(&lcwarefil..cWareCode,'totstk')
		lntotord = lfCalculte(&lcwarefil..cWareCode,'totord')
        lnTotWip = lfCalculte(&lcwarefil..cWareCode,'totWip')
        *B607286,1 ABD - [End]
	  ELSE
		IF &lcwarefil..cstatus <> 'A'
		  SELECT STYDYE
		  =SEEK(lcMajor+lcNMKey)
		  LOCATE REST WHILE Style=lcMajor+lcNMKey ;
		          FOR cWareCode=&lcwarefil..cWarecode AND EMPTY(Dyelot)
		  IF FOUND()
  		    lntotstk = stydye.totstk
		    lntotord = stydye.totord
            lnTotWip = stydye.totwip
          ELSE
		    lntotstk = 00
		    lntotord = 00
            lnTotWip = 00
  		    lcchoice = IIF(&lcwarefil..cstatus = 'A', 1, 0)
          ENDIF
          SELECT (lcwarefil)
		ELSE
		  lntotstk = 00
		  lntotord = 00
          lnTotWip = 00
		  lcchoice = IIF(&lcwarefil..cstatus = 'A', 1, 0)
		ENDIF
	  ENDIF

 	 IF lntotord > 0
	   *--The style :XXXX has on order quantity at this warehouse. Cannot remove.
       =gfModalGen('TRM42032B42001','DIALOG',lcMajor+IIF(llallcolors,'',lcSepart+lcNonMjr))
	   RETURN
	 ENDIF

	 IF lntotstk > 0
	   IF lllink_gl
	     *--The style :XXXX has an inventory, Please adjust the existing inventory before removing the Style from the warehouse.
         =gfModalGen('TRM42034B42001','DIALOG',lcMajor+IIF(llallcolors,'',lcSepart+lcNonMjr))
		 RETURN
	   ELSE
	     *--The style :xxxx has an inventory, Are you sure you want to remove the style from this warehouse.','\<Yes;\!\<No'
         lcchoice=gfModalGen('TRM42036B42002','DIALOG',lcMajor+IIF(llallcolors,'',lcSepart+lcNonMjr))
	   ENDIF
	 ENDIF

     IF lnTotWip > 0
        *--Quantity in WIP. Please cancel all WIP first.
        =gfModalGen('QRM42011B42001','DIALOG')
     ENDIF

	 IF lcchoice = 1
	   SELECT (lcwarefil)
       REPLACE cstatus WITH 'D'       
       llCUpdate = .T.
	   glUpdated = .T.
	   GO TOP
	   LOCATE FOR cstatus <> 'D'
	   IF !FOUND()
		 SHOW GET lcglwarehs DISABLE
		 SHOW GET ibwargllnk DISABLE
		 SHOW GET pbdelwar   DISABLE
		 SHOW GET cbsetdef   DISABLE
	   ENDIF
	   SHOW WINDOW (lcbrow_ttl) REFRESH
	   =lfwwarebrs()
	  ENDIF
	ENDIF
  ELSE
	*--You cannot remove a default warehouse.
    =gfModalGen('TRM42038B42001','DIALOG')
  ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvGLWareHs
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to validate the gl link code for warehouses.
*!*************************************************************
*! Calls     : gfGLBrowse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvglwarehs

IF !llbrowse AND lcglwarehs = lcoldvalue
  RETURN
ENDIF  

lnAlias=SELECT()
IF llbrowse OR !EMPTY(lcglwarehs)
  =gfglbrowse('03',@lcglwarehs,' ',0)
  llbrowse = .F.
  IF EMPTY(lcglwarehs)
	lcglwarehs = lcoldvalue
    RETURN
  ENDIF
ENDIF

SELECT (lcwarefil)
REPLACE gl_link WITH lcglwarehs ,;
   		cstatus WITH IIF (cstatus='A','A','M')
SHOW WINDOW (lcbrow_ttl) REFRESH
glUpdated = .T.
llCUpdate = .T.
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfvSetDef
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function for the check box [ ] Set as default
*!           : in the warehouse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvsetdef

IF cbsetdef
  IF !EMPTY(lcdefware) .AND. !(lcdefware = &lcwarefil..cwarecode)
    *--This style already has a default warehouse.  Do you want to set XXXX as a default?
    IF gfModalGen('TRM42039B42002','DIALOG',&lcwarefil..cwarecode) = 1
  	  lcdefware = &lcwarefil..cwarecode
      =lfRefresh('WAREHS_1')
	ELSE
  	  cbsetdef = .F.
	  SHOW GET cbsetdef
	ENDIF
  ELSE
	lcdefware = &lcwarefil..cwarecode
    =lfRefresh('WAREHS_1')
  ENDIF
ENDIF
IF cbsetdef
	SHOW GET cbsetdef DISABLE
ELSE
	SHOW GET cbsetdef ENABLE
ENDIF
laData[71]=lcDefWare
lnDefWhs=ASUBSCRIPT(laDefWhs,ASCAN(laDefWhs,laData[71]),1)
glUpdated = .T.
RETURN

*!*************************************************************
*! Name      : lfTrapKy
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Key traps necessary for browse.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lftrapky

ON KEY LABEL ctrl+W     lndummy = 1
ON KEY LABEL ctrl+q     lndummy = 1
ON KEY LABEL ctrl+home  lndummy = 1
ON KEY LABEL ctrl+END   lndummy = 1
ON KEY LABEL TAB        DO lptab
ON KEY LABEL backtab    DO lpshifttab
ON KEY LABEL esc        DO lpesc
ON KEY LABEL ctrl+enter DO lpesc
RETURN

*!*************************************************************
*! Name      : lpEsc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Esc key trapping procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpesc

ON KEY
ACTIVATE WINDOW warehs_1
_CUROBJ = IIF(laScrMode[2],OBJNUM(pbokwar),OBJNUM(pbclosewar))
KEYBOARD CHR(17)+CHR(13)

*!*************************************************************
*! Name      : lpTab
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Tab key trapping procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lptab

IF laScrMode[2]
	IF WONTOP(lcbrow_ttl)
		_CUROBJ=OBJNUM(pbokwar)
		ACTIVATE WINDOW warehs_1
	ELSE
		ACTIVATE WINDOW (lcbrow_ttl) IN WINDOW warehs
	ENDIF
ELSE
	IF WONTOP(lcbrow_ttl)
		_CUROBJ=IIF(lllink_gl,OBJNUM(lcglwarehs),OBJNUM(pbnewwar))
		ACTIVATE WINDOW warehs_1
	ELSE
		IF _CUROBJ = OBJNUM(pbclosewar)
			ACTIVATE WINDOW (lcbrow_ttl) IN WINDOW warehs
		ELSE
			_CUROBJ= _CUROBJ + 1
		ENDIF
	ENDIF
ENDIF
RETURN

*!*************************************************************
*! Name      : lpShiftTab
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Shift Tab key trapping procedure.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
PROCEDURE lpshifttab

IF laScrMode[2]
	IF WONTOP(lcbrow_ttl)
		_CUROBJ=OBJNUM(pbokwar)
		ACTIVATE WINDOW warehs_1
	ELSE
		ACTIVATE WINDOW (lcbrow_ttl) IN WINDOW warehs
	ENDIF
ELSE
	IF WONTOP(lcbrow_ttl)
		_CUROBJ=OBJNUM(pbclosewar)
		ACTIVATE WINDOW warehs_1
	ELSE
		IF _CUROBJ = IIF(lllink_gl,OBJNUM(lcglwarehs),OBJNUM(pbnewwar))
			ACTIVATE WINDOW (lcbrow_ttl) IN WINDOW warehs
		ELSE
			_CUROBJ= _CUROBJ - 1
		ENDIF
	ENDIF
ENDIF
RETURN




*:*************************************************************
*: Name      : lfvViewWH
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Display warehouses for current style/color.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : llFromInfo -> Called from getinfo.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvviewwh
PARA llFromInfo

SELECT stydye
=SEEK(lcMajor+lcNMKey)
lccondtn='Style=lcMajor+lcNMKey'
IF FOUND()
  *-Initialize the array.
  DECLARE lawareh[1,2]
  lawareh[1,1]  = IIF(lcSysType = 'P',laDefWhs[1,1],'All   ')
  lawareh[1,2]  = 1
  *E300889,1 Remove all from warehous list if pos.
  *-- HDM B602631,1
  IF lcSysType<>'P'
    I = 1
    lnCurrntWh = 1
    SCAN WHILE &lccondtn
    	IF ASCAN(lawareh,cwarecode)=0
        i=i+1
        DECLARE lawareh[I,2]
        lawareh[I,1]  = cwarecode
        lawareh[I,2]  = i
        IF !llStyMode AND lawareh[I,1]=lcWareCode
          lnCurrntWh = I
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  
  *-- HDM B602631,1 Assign The Default warehouse case POS[Start]
  *lcwarecode = lawareh[lnCurrntWh,1]
  *lcWareCode = IIF(lcSysType='P',laData[71],'All   ')
  *-- HDM B602631,1
  *lcWareCode = IIF(lcSysType='P',lawareh[lnCurrntWh,1],'All   ')
  lcWareCode  = IIF(lcSysType='P',lawareh[1,1],IIF(!llStyMode,lcWareCode,'All   '))
  *-- HDM B602631,1 Assign The Default warehouse case POS[End]
  SHOW GET lcwarecode ENABLE
  IF !llFromInfo AND llmultiloc
    
    =lfvviewloc()
  ENDIF 
ENDIF
SHOW GET lcwarecode 
SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
=lfRefresh(lcWinCh2)
RETURN


*:*************************************************************
*! Name      : lfvViewLoc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Display locations for current style/color/warehouse.
*:*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvviewloc

PRIVATE lnlocatno, lnstcol

llstymode = IIF(lcSysType='P',.F.,IIF( ALLTRIM(lcWareCode) = 'All' ,.T.,.F.))
IF ALLTRIM(lcwarecode) <> 'All' OR !llmultiwh
  IF !llallcolors AND AT('*',lcNonMjr)=0
    *E301077,7 Add to open whsloc file.
    =gfOpenFile(gcDataDir+'whsloc','WhsLocSt','SH')
    IF !SEEK(PADR(lcMajor+lcSepart+lcNonMjr,19)+SPACE(6)+IIF(llmultiwh,lcwarecode,laData[71]))
      DECLARE lalocation[1]
	  lalocation[1] = 'N/A'
	  lnloc = 'N/A'
	ELSE
 	  lalocation[1] = 'View Locations'
	  lnloc='View Locations'
	  i=1
	  SCAN REST WHILE Style+Color+cWareCode=;
      	  PADR(lcMajor+lcSepart+lcNonMjr,19)+SPACE(6)+IIF(llmultiwh,lcwarecode,laData[71])
		i=i+1
		DECLARE lalocation[I]
		lalocation[I]  = clocation
  	  ENDSCAN
	ENDIF
  ELSE
    DECLARE lalocation[1]
    lalocation[1] = 'N/A'
	lnloc = 'N/A'
  ENDIF
ELSE
  DECLARE lalocation[1]
  lalocation[1] = 'N/A'
  lnloc = 'N/A'
ENDIF
SHOW GET lnloc

*--Calculate the optional line.
=lfGetOptn()


*E300803,1 SWK 05/25/98 Call fumction to calculate totals
=lfTotPrePak()
*E300803,1 (End)


*--Get cut & sold information.
=lfGetTmp()
=lfRefresh(lcWinCh2)
RETURN



*!*************************************************************
*! Name      : lfvDyelots
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function see dyelots.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvdyelots

PRIVATE lcBrFields,lnAlias
lnAlias=SELECT()

*-- Case of specific color all warehouses.-------------------------
lcexkey  = lcMajor+lcNMKey
lcwcondt = 'Style = lcExKey'
lcfcondt = '!EMPTY(Dyelot)'
DO CASE
CASE ! llallcolors AND   llstymode
	lcmesgx  = 'Style :'+lcMajor+lcSepart+lcNonMjr
    lcFlds   = IIF(llmultiwh,"cWareCode:R :H='Warehouse',","")
	*-- Case of specific color and specific warehouses.----------------
CASE ! llallcolors AND ! llstymode
	lcmesgx  = 'Style\Warehouse :'+lcMajor+lcSepart+lcNonMjr+'\'+ALLTRIM(lcwarecode)
	lcfcondt = lcfcondt +' .AND. cWareCode = lcWareCode'
    lcFlds   = ""
	*-- Case of all color all warehouses.------------------------------
CASE  llallcolors AND  llstymode
	lcmesgx  = 'Style :'+lcMajor
    lcFlds   = IIF(llmultiwh,"cWareCode:R :H='Warehouse',","")
	*-- Case of all color and specific warehouses.---------------------
CASE  llallcolors AND !llstymode
	lcmesgx  = 'Style\Warehouse '+lcMajor+'\'+ALLTRIM(lcwarecode)
	lcfcondt = lcfcondt +' .AND. cWareCode = lcWareCode'
    lcFlds   = ""
ENDCASE

SELECT STYDYE
llfound = SEEK( lcexkey )
IF llfound
  LOCATE REST WHILE &lcwcondt FOR &lcfcondt
  llfound=FOUND()
ENDIF

IF ! llfound
  *--No Dyelots found for (lcmesgx).
  =gfModalGen('TRM42053B42001','DIALOG',lcmesgx)
ELSE
  lcBrFields = lcFlds + "DYELOT:R :H='Dyelot #':15,"+;
               "STK1  :R :H='Stk1':6,"+;	         	           	         
               "STK2  :R :H='Stk2':6,"+;
               "STK3  :R :H='Stk3':6,"+;
	           "STK4  :R :H='Stk4':6,"+;
               "STK5  :R :H='Stk5':6,"+;
	           "STK6  :R :H='Stk6':6,"+;
               "STK7  :R :H='Stk7':6,"+;
               "STK8  :R :H='Stk8':6,"+;
               "TOTSTK:R :H='TotStk':7,"+;
               "ALO1  :R :H='Alo1':6,"+;
               "ALO2  :R :H='Alo2':6,"+;
               "ALO3  :R :H='Alo3':6,"+;
               "ALO4  :R :H='Alo4':6,"+;
               "ALO5  :R :H='Alo5':6,"+;
               "ALO6  :R :H='Alo6':6,"+;
               "ALO7  :R :H='Alo7':6,"+;
               "ALO8  :R :H='Alo8':6,"+;
               "TOTALO:R :H='TotAlo':7"

  =Ariabrow([lcExKey FOR !EMPTY(Dyelot) .AND. ;
	IIF(llstymode,.T.,cwarecode=lcwarecode) rest],'Dyelots for '+lcmesgx,;
	gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ENDIF
SELECT (lnAlias)
RETURN

*!*************************************************************
*! Name      : lfCutSld
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Get the Col/Row cut & sold object to Dispaly.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnDspSz -> Size no. 1->8 or 9 for Total.
*!                      lcObjFlg-> controls Up/Down objects.
*!*************************************************************
*! Note : lcObjFlg -> '1'   PLANE           .F. <- llDown
*!                    '1'   IN-TRANSIT      .T.
*!                    '2'   IN-TRANSIT      .F.
*!                    '2'   ALOCATED        .T.
*!                    '3'   WORK ORDER      .F.
*!                    '3'   INVENTORY       .T.
*!                    '4'   INVENTORY       .F.
*!                    '4'   AVAILABLE       .T.
*!*************************************************************
*! Returns            : lnDispx -> Cut & Sold object to Display.
*!*************************************************************
*! Example   : =lfCutSld('1','2') -> 1ST SIZE FOR INTRANSIT.
*!*************************************************************
FUNCTION lfCutSld
PARA lnDspSz,lcObjFlg
PRIVATE lnAli
lnDispx = ' '

*C200077,1 Changed the way of calling the lfCutSld.
IF llDown  && Show Down information.

  *--1) Read intransit qty or Optional WIP.
  IF lcObjFlg='1'
    IF !laData[21]
      lnDispx = IIF(lnDspSz='9',lnTInTrn,lnInTrn&lnDspSz)
    ELSE
      lnDispx = IIF(lcPCAoptn='P',lnCHold&lnDspSz,;
                IIF(lcPCAoptn='C',lnCOpen&lnDspSz,;
                IIF(lcPCAoptn='A',lnCActl&lnDspSz,' ')))
    ENDIF
  ENDIF
  
  *--2) Read Alocated qty.
  IF lcObjFlg='2'
    lnDispx = IIF(lnDspSz='9',lnTotAlo,ALO&lnDspSz)
  ENDIF 

  *--3) Read Total inventory or On-shelf inventory qty.
  IF lcObjFlg='3'
    IF llOnShelf
      lnDispx = IIF(lnDspSz='9',lnCTTotStk,Stk&lnDspSz)-lnOnShf&lnDspSz
    ELSE
      lnDispx = IIF(lnDspSz='9',lnCTTotStk,Stk&lnDspSz)
    ENDIF
  ENDIF

  *--4) Read Alocated qty.
  IF lcObjFlg='4'
    lnDispx = IIF(lnDspSz='9',lnCTTotStk+lnTotWip,STK&lnDspSz+WIP&lnDspSz)
  ENDIF 

ELSE       &&  (!llDown)  Show UP information (default).

  *--1) Read Plan Qty.
  IF lcObjFlg='1'
    lnDispx = lnTPlan&lnDspSz
  ENDIF

  *--2) Read intransit qty or Optional WIP.
  IF lcObjFlg='2'
    IF !laData[21]
      lnDispx = IIF(lnDspSz='9',lnTInTrn,lnInTrn&lnDspSz)
    ELSE
      lnDispx = IIF(lcPCAoptn='P',lnCHold&lnDspSz,;
                IIF(lcPCAoptn='C',lnCOpen&lnDspSz,;
                IIF(lcPCAoptn='A',lnCActl&lnDspSz,' ')))
    ENDIF
  ENDIF

  *--3) Read Work Order Qty.
  IF lcObjFlg='3'
    lnDispx = IIF(lnDspSz='9',lnTotWO,nWO&lnDspSz)
  ENDIF 

  *--4) Read Total inventory or On-shelf inventory qty.
  IF lcObjFlg='4'
    IF llOnShelf
      lnDispx = IIF(lnDspSz='9',lnCTTotStk,Stk&lnDspSz)-lnOnShf&lnDspSz
    ELSE
      lnDispx = IIF(lnDspSz='9',lnCTTotStk,Stk&lnDspSz)
    ENDIF
  ENDIF
  
ENDIF

RETURN lnDispx


*IF (llDown AND lcObjFlg='1') OR (!llDown AND lcObjFlg='2')
*  IF !laData[21]
*    IF lnDspSz='9'
*      lnDispx = lnTInTrn
*    ELSE
*      lnDispx = lnInTrn&lnDspSz
*    ENDIF
*  ELSE
*    DO CASE
*      CASE lnBarNo=1
*        lnDispx = lnCHold&lnDspSz
*      CASE lnBarNo=2
*        lnDispx = lnCOpen&lnDspSz
*      CASE lnBarNo=3
*        lnDispx = lnCActl&lnDspSz
*      OTHE
*        lnDispx=' '
*    ENDCASE
*  ENDIF
*ENDIF

*B602831,1 Read the plane from variables insted of calculate it.
*IF !llDown AND lcObjFlg='1'
*  lnDispx = lnTPlan&lnDspSz
*ENDIF
*B602831,1 End.

*IF llDown AND lcObjFlg='2'
*  IF lnDspSz='9'
*    lnDispx = TotAlo
*  ELSE
*    lnDispx = ALO&lnDspSz
*  ENDIF 
*ENDIF

*--HDM B602340,1 Incorrecr Plan Quantity[Start]
*IF !llDown AND !llObjFlg
*    lnDsNoSz=INT(VAL(lnDspSz))
*    lnDispx =laData[30+lnDsNoSz]
*ENDIF

*B602831,1 Comment out since we don't need to calculate plane at this stage.
*IF !llDown AND !llObjFlg
*  *-- If Style selected as all colors we have to Sum Plan Qty in Cut & Sold Folder
*  *-- For all colors
*  IF llAllColors AND !laScrMode[4]
*    lnAli = SELECT(0)
*    SELECT STYLE
*    lnRecNo = RECNO()
*    lnDsNoSz=INT(VAL(lnDspSz))
*    *--If The calculated Qty not the Total Qty sum PLANx
*    *--Else Sum TotPlan
*    IF lnDsNoSz <> 9
*      *--HDM B602392,1 Redundency in plan values if Extended Size scale[start]
*      *SUM TO lnDispx PLAN&lnDspSz FOR CSTYMAJOR = PADR(lcMajor,19)
*      *--Check For ESS
*      IF llMScale
*        *--IF company uses ESS make sure the calculations takes in consideration the scale
*        *--For each Major-Scale
*        SUM TO lnDispx PLAN&lnDspSz FOR CSTYMAJOR + SCALE = PADR(lcMajor,19) + laExtSz[lnCntlScl]
*      ELSE
*        SUM TO lnDispx PLAN&lnDspSz FOR CSTYMAJOR = PADR(lcMajor,19)
*      ENDIF
*      *--HDM B602392,1 Redundency in plan values if Extended Size scale[End]
*    ELSE
*      SUM TO lnDispx TOTPLAN FOR CSTYMAJOR = PADR(lcMajor,19)
*    ENDIF
*    GOTO (lnRecNo) IN Style
*    SELECT (lnAli)
*  ELSE
*    lnDsNoSz=INT(VAL(lnDspSz))
*    lnDispx =laData[30+lnDsNoSz]
*  ENDIF
*ENDIF
*B602831,1 End.
*--HDM B602340,1 Incorrecr Plan Quantity[End]
*C200077,1 End.
*RETURN lnDispx


*!*************************************************************
*! Name      : lfExtScl
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Previus next extended scale scrolling.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfExtScl
PARAMETER lcNPScl
PRIVATE lnAlias

lnAlias = SELECT()
DO CASE
  CASE lcNPScl='N' 
    lnCntlScl = lnCntlScl + IIF(lnCntlScl=ALEN(laExtSz),0,1)
  CASE lcNPScl='P'
    lnCntlScl = lnCntlScl - IIF(lnCntlScl=1,0,1)
ENDCASE
IF lnCntlScl = 1
  SHOW GET pbPrvsScl DISABLE
ELSE
  SHOW GET pbPrvsScl ENABLE
ENDIF     
IF lnCntlScl=ALEN(laExtSz) OR SUBSTR(lcNonMjr,lnColorWid-2,3)<>'***'
  SHOW GET pbNxtScl DISABLE
ELSE
  SHOW GET pbNxtScl ENABLE    
ENDIF

= SEEK ('S'+laExtSz[lnCntlScl],'SCALE')

STORE PADL(ALLTRIM(scale.sz1),5) TO lcSz1
STORE PADL(ALLTRIM(scale.sz2),5) TO lcSz2
STORE PADL(ALLTRIM(scale.sz3),5) TO lcSz3
STORE PADL(ALLTRIM(scale.sz4),5) TO lcSz4
STORE PADL(ALLTRIM(scale.sz5),5) TO lcSz5
STORE PADL(ALLTRIM(scale.sz6),5) TO lcSz6
STORE PADL(ALLTRIM(scale.sz7),5) TO lcSz7
STORE PADL(ALLTRIM(scale.sz8),5) TO lcSz8
lcSclDesc='Size:'+SCALE.cScl_Desc
*E300803,1 SWK 05/25/98 Call function to calculate the totals
=lfTotPrePak()
*E300803,1 (End)

*--Calculate the optional line.
=lfGetOptn()

*--Get cut & sold information.
=lfGetTmp()

=lfRefresh(lcWinCh2)
RETURN

*!*************************************************************
*!  Function : lfvSUp and lfvSDn.
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : functions to control the cut & sold expresions.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*************** UP ** Dn **************************************
* Prepack        1    xx 
* Plane          2    xx 
* W. O.          3    xx
* W.I.P.         4     1
* InTransit      5     2     
* Tot.Invt.      6     3
* Unal.Inv.      7     4 
* Alo.Invt.      8     5 
* Tot.Aval.      9     6
* Opn.Ords.     10     7
* O.T.S.        11     8
* Shipped       xx     9 
* Ret.Auth.     xx    10 
* Returns       xx    11 
**************************************************************
FUNCTION lfvSUp
PARA llNoVw

llDown=.F.
lcNoVwSt=IIF(llNoVw,'DISABLE','ENABLE')
SHOW GET pbScrl,1 PROMPT lcCSDn
SHOW GET pbScrl ENABLE

lcCExp1 = 'Prepack'
lcCExp2 = 'Plan'
lcCExp3 = 'W.O.'
lcCExp4 = 'W.I.P.'
SHOW GET pbPbExp1,1 DISABLE PROMPT lcCExp1
SHOW GET pbPbExp2,1 DISABLE PROMPT lcCExp2
SHOW GET pbPbExp3,1 &lcNoVwSt PROMPT lcCExp3
SHOW GET pbPbExp4,1 &lcNoVwSt PROMPT lcCExp4
IF !laData[21]
  lcCExp5 = 'InTransit'
  SHOW GET pbPbExp5,1 &lcNoVwSt PROMPT lcCExp5
ELSE
  lcCExp5 = IIF(lcPCAoptn='P','Proposed',;
                IIF(lcPCAoptn='C','Cut',;
                IIF(lcPCAoptn='A','Actual','-')))
  IF lcCExp5='-'
    SHOW GET pbPbExp5,1 DISABLE PROMPT lcCExp5 
  ELSE
    SHOW GET pbPbExp5,1 &lcNoVwSt PROMPT lcCExp5 
  ENDIF
ENDIF
*C200077,1 Switch the buttom title.
lcCExp6 = IIF(llOnShelf,'On-Shelf','Tot. Invt.')
SHOW GET pbPbExp6,1 &lcNoVwSt PROMPT lcCExp6
*C200077,1 End.
lcCExp7 = 'Unal.Invt.'
lcCExp8 = 'Allo.Invt.'
lcCExp9 = 'Tot. Avail'
*B603032,1 Start.
lcCExp10= 'Opn.Orders'
lcCExp10= 'Sls.Orders'
*B603032,1 End.
lcCExp11= 'O.T.S.'
SHOW GET pbPbExp7,1 DISABLE   PROMPT lcCExp7
SHOW GET pbPbExp8,1 &lcNoVwSt PROMPT lcCExp8
SHOW GET pbPbExp9,1 DISABLE   PROMPT lcCExp9
SHOW GET pbPbEx10,1 &lcNoVwSt PROMPT lcCExp10
SHOW GET pbPbEx11,1 &lcNoVwSt PROMPT lcCExp11
*B602855,1 added in case of folder 2 only.
*IF ! laScrMode[1]
IF ! laScrMode[1] AND lnactfolder = 2
  SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
  =lfRefresh(lcWinCh2)
ENDIF
RETURN
**********************************************
FUNCTION lfvSDn
PARA llNoVw

lcNoVwSt=IIF(llNoVw,'DISABLE','ENABLE')
llDown=.T.
SHOW GET pbScrl,1 PROMPT lcCSUp
SHOW GET pbScrl ENABLE
lcCExp1 = 'W.I.P.'
SHOW GET pbPbExp1,1 &lcNoVwSt PROMPT lcCExp1
IF !laData[21]
  lcCExp2 = 'InTransit'
  SHOW GET pbPbExp2,1 ENABLE PROMPT lcCExp2
ELSE
  lcCExp2 = IIF(lcPCAoptn='P','Proposed',;
                IIF(lcPCAoptn='C','Cut',;
                IIF(lcPCAoptn='A','Actual','-')))
  IF lcCExp2='-'
    SHOW GET pbPbExp2,1 DISABLE PROMPT lcCExp2 
  ELSE
    SHOW GET pbPbExp2,1 &lcNoVwSt PROMPT lcCExp2 
  ENDIF
ENDIF
*C200077,1 Switch the buttom title.
lcCExp3 = IIF(llOnShelf,'On-Shelf','Tot. Invt.')
SHOW GET pbPbExp3,1 &lcNoVwSt PROMPT lcCExp3
*C200077,1 End.

lcCExp4 = 'Unal.Invt.'
lcCExp5 = 'Allo.Invt.'
lcCExp6 = 'Tot. Avail'
*B603032,1 Start.
*lcCExp7 = 'Opn.Orders'
lcCExp7 = 'Sls.Orders'
*B603032,1 End.
lcCExp8 = 'O.T.S.'
lcCExp9 = 'Shipped'
lcCExp10= 'Ret.Authr.'
lcCExp11= 'Returns'

SHOW GET pbPbExp4,1 DISABLE   PROMPT lcCExp4
SHOW GET pbPbExp5,1 &lcNoVwSt PROMPT lcCExp5
SHOW GET pbPbExp6,1 DISABLE   PROMPT lcCExp6
SHOW GET pbPbExp7,1 &lcNoVwSt PROMPT lcCExp7
SHOW GET pbPbExp8,1 &lcNoVwSt PROMPT lcCExp8
SHOW GET pbPbExp9,1 &lcNoVwSt PROMPT lcCExp9
SHOW GET pbPbEx10,1 &lcNoVwSt PROMPT lcCExp10
SHOW GET pbPbEx11,1 &lcNoVwSt PROMPT lcCExp11
*B602855,1 added in case of folder 2 only.
*IF ! laScrMode[1]
IF ! laScrMode[1] AND lnactfolder = 2
  SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
  =lfRefresh(lcWinCh2)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetOptn
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Calculate optioanl Cut,propose and Actual.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfGetOptn
PRIVATE lnalias

lnalias=SELECT(0)
*--- B602631,1 HDM    03/11/1999 Error In Opening CT Files [Done By Hesham][Start]
IF llMFInstld
  laOpFile[15]= gfOpenFile(gcDataDir+'CuttktH','CuttktH' ,'SH')
  laOpFile[16]= gfOpenFile(gcDataDir+'CuttktL','CuttktLs','SH')
ENDIF
*B602831,1 Added variable for plan insted of calculate it each time lnTPlan.
STORE 0 TO lnTPlan1,lnTPlan2,lnTPlan3,lnTPlan4,lnTPlan5,lnTPlan6,lnTPlan7,lnTPlan8
*--- B602631,1 HDM    03/11/1999 Error In Opening CT Files [Done By Hesham][End]
SELECT STYLE
lnStSavRc=IIF(!EOF(),RECNO(),0)
IF laData[21]

  STORE 0 TO lnCHold1,lnCHold2,lnCHold3,lnCHold4,lnCHold5,lnCHold6,lnCHold7,lnCHold8,lnCHold9
  STORE 0 TO lnCOpen1,lnCOpen2,lnCOpen3,lnCOpen4,lnCOpen5,lnCOpen6,lnCOpen7,lnCOpen8,lnCOpen9
  STORE 0 TO lnCActl1,lnCActl2,lnCActl3,lnCActl4,lnCActl5,lnCActl6,lnCActl7,lnCActl8,lnCActl9
  *--Other scales qty.
  STORE 0 TO lnZCHold1,lnZCHold2,lnZCHold3,lnZCHold4,lnZCHold5,lnZCHold6,lnZCHold7,lnZCHold8,lnZCHold9
  STORE 0 TO lnZCOpen1,lnZCOpen2,lnZCOpen3,lnZCOpen4,lnZCOpen5,lnZCOpen6,lnZCOpen7,lnZCOpen8,lnZCOpen9
  STORE 0 TO lnZCActl1,lnZCActl2,lnZCActl3,lnZCActl4,lnZCActl5,lnZCActl6,lnZCActl7,lnZCActl8,lnZCActl9
  IF !llMFInstld
    RETURN
  ENDIF
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
  SET RELATION TO Style INTO Style ADDI

  lcXKEY =lcMajor+lcNMKey
  lcWhile=IIF(llAllColors,"Style=lcMajor","Style=lcMajor+lcNMKey")
  lcFor    = IIF(llstymode,".T.","CUTTKTL.cWareCode=lcWareCode")
  IF llMScale AND llAllscales
    lcSzExpr = "STYLE.Scale=laExtSz[lnCntlScl]"
  ELSE
    lcSzExpr = ".T."
  ENDIF
  IF SEEK(lcXKEY)
    SCAN WHILE &lcWhile FOR Cuttkth.Status $ 'HOA' AND &lcFor
      IF &lcSzExpr
        DO CASE  
          CASE Cuttkth.Status = 'H'
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnCHold&Z=MAX(lnCHold&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnCHold9 =lnCHold1+lnCHold2+lnCHold3+lnCHold4+lnCHold5+lnCHold6+lnCHold7+lnCHold8
          CASE Cuttkth.Status = 'O'
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnCOpen&Z=MAX(lnCOpen&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnCOpen9 =lnCOpen1+lnCOpen2+lnCOpen3+lnCOpen4+lnCOpen5+lnCOpen6+lnCOpen7+lnCOpen8
          CASE Cuttkth.Status = 'A'            
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnCActl&Z=MAX(lnCActl&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnCActl9 =lnCActl1+lnCActl2+lnCActl3+lnCActl4+lnCActl5+lnCActl6+lnCActl7+lnCActl8
        ENDCASE
      ELSE
        DO CASE  
          CASE Cuttkth.Status = 'H'
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnZCHold&Z=MAX(lnZCHold&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnZCHold9 =lnZCHold1+lnZCHold2+lnZCHold3+lnZCHold4+lnZCHold5+lnZCHold6+lnZCHold7+lnZCHold8
          CASE Cuttkth.Status = 'O'
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnZCOpen&Z=MAX(lnZCOpen&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnZCOpen9 =lnZCOpen1+lnZCOpen2+lnZCOpen3+lnZCOpen4+lnZCOpen5+lnZCOpen6+lnZCOpen7+lnZCOpen8
          CASE Cuttkth.Status = 'A'            
            FOR I=1 TO 8 
              Z=STR(I,1) 
              lnZCActl&Z=MAX(lnZCActl&Z+(Qty&Z*IIF(TranCd='1',1,-1)),0)
            ENDFOR 
            lnZCActl9 =lnZCActl1+lnZCActl2+lnZCActl3+lnZCActl4+lnZCActl5+lnZCActl6+lnZCActl7+lnZCActl8
        ENDCASE
      ENDIF
    ENDSCAN
    *--Totals for all external sizes.
    lnCHold9 = lnCHold9 + lnZCHold9
    lnCOpen9 = lnCOpen9 + lnZCOpen9
    lnCActl9 = lnCActl9 + lnZCActl9
  ENDIF
  SELECT CUTTKTL
  SET RELATION TO
ELSE
  STORE 0 TO lnInTrn1,lnInTrn2,lnInTrn3,lnInTrn4,lnInTrn5,lnInTrn6,lnInTrn7,lnInTrn8
  IF !llPOInstld
    RETURN
  ENDIF
  
  lcSqlScl  = IIF(llAllScales,laExtSz[lnCntlScl],"")
  lcSqlWare = IIF(llMultiwh AND !llStyMode,lcWareCode,"")
  IF !llStyMode  
    SELECT STYDYE
    SET RELATION TO Style INTO Style
    IF SEEK(lcMajor+lcNMKey)
      lnSavRc=RECNO()
      SCAN WHILE Style=lcMajor+lcNMKey ;
           FOR cWareCode = lcSqlWare AND STYLE.Scale = lcSqlScl
        lnInTrn1=lnInTrn1+InTrans1
        lnInTrn2=lnInTrn2+InTrans2
        lnInTrn3=lnInTrn3+InTrans3
        lnInTrn4=lnInTrn4+InTrans4
        lnInTrn5=lnInTrn5+InTrans5
        lnInTrn6=lnInTrn6+InTrans6                                
        lnInTrn7=lnInTrn7+InTrans7
        lnInTrn8=lnInTrn8+InTrans8        
*       lnTInTrn=lnTInTrn+TotInTrn

      ENDSCAN
      SELECT STYDYE
      GOTO lnSavRc
    ENDIF 
    SET RELATION TO
  ELSE
    SELECT STYLE
    IF SEEK(lcMajor+lcNMKey)    
      lnSavRc=RECNO()
      SUM InTrans1,InTrans2,InTrans3,InTrans4,InTrans5,InTrans6,InTrans7,InTrans8 ;
          REST WHILE Style = lcMajor+lcNMKey FOR Scale = lcSqlScl ;
          TO lnInTrn1,lnInTrn2,lnInTrn3,lnInTrn4,lnInTrn5,lnInTrn6,lnInTrn7,lnInTrn8
      GOTO lnSavRc
    ENDIF
  ENDIF
ENDIF

*B609027,1 TMI 10/07/2009 [Start] Open ordline file when SO module is installed in the correct place
IF llSOInstld
  IF !laOpFile[4]
    laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
  ENDIF
ENDIF
*B609027,1 TMI 05/26/2009 [End  ] 

*C200077,1 Calculate On-Shelf inventory.
STORE 0 TO lnOnShf1,lnOnShf2,lnOnShf3,lnOnShf4,lnOnShf5,lnOnShf6,lnOnShf7,lnOnShf8,lnOnShf9
IF llALInstld
  IF !laOpFile[5]
    laOpFile[5] = gfOpenFile(gcDataDir+'Piktkt','Piktkt','SH')
  ENDIF
  *B608872,1 TMI 05/26/2009 [Start] Comment this line , the ordline file should be opened when SO is installed
  *IF !laOpFile[4]
  *  laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
  *ENDIF
  *B608872,1 TMI 05/26/2009 [End  ] 
  lcSqlScl  = IIF(llAllScales,laExtSz[lnCntlScl],"")
  lcSqlWare = IIF(llMultiwh AND !llStyMode,lcWareCode,"")
  SELECT ORDLINE
  *B802425,1 Start.
  lnOrdLrc = IIF(EOF(),0,RECNO())
  *B802425,1 End.
  SET RELATION TO PIKTKT INTO PIKTKT ADDI
  IF SEEK(lcMajor+lcNMKey)
   SUM Pik1,Pik2,Pik3,Pik4,Pik5,Pik6,Pik7,Pik8,TotPik ;
      REST WHILE Style = lcMajor+lcNMKey ;
      FOR !EMPTY(Piktkt) AND PIKTKT.Status='P' AND ;
          PIKTKT.cWareCode=lcSqlWare AND Scale=IIF(llMScale,lcSqlScl,'') ;
      TO lnOnShf1,lnOnShf2,lnOnShf3,lnOnShf4,lnOnShf5,lnOnShf6,lnOnShf7,lnOnShf8,lnOnShf9
  ENDIF
  SELECT ORDLINE
  *B802425,1 Start.
  IF lnOrdLrc<>0
    GOTO lnOrdLrc
  ENDIF  
  *B802425,1 End.
  SET RELATION TO
ENDIF
*C200077,1 End.

*B609027,1 TMI 10/04/2009 [start] comment this code and move it before the "IF llALInstld" statement just above
**B608872,1 TMI 05/26/2009 [Start] Open ordline file when SO module is installed
*IF llSOInstld
*  IF !laOpFile[4]
*    laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
*  ENDIF
*ENDIF
**B608872,1 TMI 05/26/2009 [End  ] 
*B609027,1 TMI 10/04/2009 [ end ] comment this code and move it before the "IF llALInstld" statement just above


SELECT STYLE
*B602831,1 Calculate the plance once.
IF SEEK(lcMajor+lcNMKey)    
   SUM Plan1,Plan2,Plan3,Plan4,Plan5,Plan6,Plan7,Plan8 ;
      REST WHILE Style = lcMajor+lcNMKey FOR Scale = IIF(llMScale,lcSqlScl,'') ;
      TO lnTPlan1,lnTPlan2,lnTPlan3,lnTPlan4,lnTPlan5,lnTPlan6,lnTPlan7,lnTPlan8
ENDIF

IF lnStSavRc<>0
  GOTO lnStSavRc
ENDIF
SELECT(lnalias)

*!**********************************************************************
*! Name      : lfCnrolEx()
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to control different progrom veiw mode cases.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lccurfile->File to filter.
*!                      lcCTStat ->C/t or P/o available statuses.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfcnrolex
PARA lccurfile,lcCTStat

STORE " " TO lcexkey,lcmesgx,lcwcondt,lcfcondt
SELECT (lccurfile)
IF lccurfile='PIKTKT'
	SELECT ordline
ENDIF

lcExKey  = lcMajor+lcNMKey
*B606580,1 ASH 10/29/2002 (Begin) Seek for the selected scale.
IF ! llallcolors AND llMScale AND llAllScales
  lcExKey = lcExKey + laExtSz[lnCntlScl]
ENDIF
*B606580,1 ASH 10/29/2002 (End)
lcWcondt = "Style = lcExKey"
lcFcondt = ".T."    

DO CASE
  *-- Case of specific color all warehouses.-------------------------
  CASE ! llallcolors AND   llstymode
    lcmesgx  = 'Style :'+lcMajor+lcSepart+lcNonMjr
    DO CASE
      CASE lccurfile='ORDLINE'
	  	lcfcondt = "cOrdType='O' AND !(OrdHdr.Status $ 'CXB' .OR. TotQty <= 0)"
  	  CASE lccurfile='PIKTKT'
	  	lcfcondt = 'Picked'
  	  CASE lccurfile='CUTTKTL' AND !EMPTY(lcCTStat)
	    lcfcondt = "CUTTKTH.Status $ lcCTStat "
	  
	  
	  *B603256,1 KHM 11/01/1999 (Begin) Adding !laData[21] to this case
	  *B603256,1                which is only for the purchased styles.
  	  *CASE lccurfile='POSLN' AND EMPTY(lcCTStat)
  	  CASE lccurfile='POSLN' AND EMPTY(lcCTStat) AND !laData[21]
  	  *B603256,1 KHM 11/01/1999 (End)
  	    *B122043,1 ASH 04/19/2004 (Begin) Don't display the BID P/O in the Work order browse.
	    *lcfcondt = "cStyType<>'C'"
	    lcfcondt = "POSHDR.Status <> 'B' AND cStyType<>'C'"
	    *B122043,1 ASH 04/19/2004 (End)
  	  
	  *B603256,1 KHM 11/01/1999 (Begin) Adding !laData[21] to this case
	  *B603256,1                which is only for the purchased styles.
  	  *CASE lccurfile='POSLN' AND !EMPTY(lcCTStat)
  	  CASE lccurfile='POSLN' AND !EMPTY(lcCTStat) AND !laData[21]
  	  *B603256,1 KHM 11/01/1999 (End)

	  *B603859,1 MHM 08/29/2000 [Begin] 
	    *lcfcondt = "POSHDR.Status $ 'HO' AND cStyType<>'C'"
 	    lcfcondt = "POSHDR.Status $ 'HOA' AND cStyType<>'C'"
 	  *B603859,1 MHM 08/29/2000 [End] 

 	  *B603256,1 KHM 11/01/1999 (Begin) Adding the following case to
 	  *B603256,1                handle the style that has dye and adornment orders.
 	  CASE lccurfile='POSLN' AND laData[21]
        lcfcondt = "IIF(!EMPTY(lcCTStat),POSHDR.Status $ lcCTStat AND cStyType $ 'DA',cStyType = 'D')"
	  *B603256,1 KHM 11/01/1999 (End)
	  
      *--HDM[START]
	  CASE lccurfile='SHPMTHDR' && AND !EMPTY(lcCTStat)
	    SELECT POSLN
	    SET ORDER TO TAG Poslns
	    
	    *B603407,1 NAD 01/25/2000 (Begin) Fixed the bug variable CSTYTYQE not found
	    *lcfcondt = "TRANCD$'36' .AND. CSTYTYQE$'PN' .AND. TOTQTY > 0"
	    lcfcondt = "TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0"
	    *B603407,1 NAD 01/25/2000 (End)
	  *--HDM[END]
        *B604357,1 TMI [Start] Calculate InTransit Qty ( Issued Interlocation - Recieved )
        =lfInTrns()
        *B604357,1 TMI [End  ] 

  	  *B607006,1 KHM 07/16/2003 (Begin) Add the checking of RA status.
  	  CASE lccurfile='RALINE'
        lcfcondt = 'RETAUTH.Status <> "X"'
	  *B607006,1 KHM 07/16/2003 (End)
    ENDCASE

  *-- Case of specific color and specific warehouses.----------------
  CASE ! llallcolors AND ! llstymode
	lcmesgx  = 'Style\Warehouse :'+lcMajor+lcSepart+lcNonMjr+'\'+ALLTRIM(lcwarecode)
	DO CASE
   	  CASE lccurfile='CUTTKTL'
        IF !EMPTY(lcCTStat)
	      lcfcondt = "CUTTKTL.cWareCode=lcWareCode AND CUTTKTH.Status $ lcCTStat "
        ELSE
	      lcfcondt = "CUTTKTL.cWareCode=lcWareCode"
        ENDIF 
  	  CASE lccurfile='POSLN'
	  *B603256,1 KHM 11/01/1999 (Begin) Changing the following IF command	  
	  *B603256,1                to DO CASE to handle the two case of the
	  *B603256,1                imported style and the case of styles that 
	  *B603256,1                have dye and adornment orders.
        *IF !EMPTY(lcCTStat)
        *  lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HO' AND cStyType<>'C'"
        *ELSE
	    *  lcfcondt = "POSLN.cWareCode=lcWareCode AND cStyType<>'C'"
        *ENDIF
        DO CASE
          CASE !EMPTY(lcCTStat) AND !laData[21]
          *B603859,1 MHM 08/29/2000 [Begin]
            *lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HO' AND cStyType<>'C'"
            lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HOA' AND cStyType<>'C'"
          *B603859,1 MHM 08/29/2000[End]  
          CASE EMPTY(lcCTStat) AND !laData[21]
            *B122043,1 ASH 04/19/2004 (Begin) Don't display the BID P/O in the Work order browse.
            *lcfcondt = "POSLN.cWareCode=lcWareCode AND cStyType<>'C'"
            lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status <> 'B' AND cStyType<>'C'"
            *B122043,1 ASH 04/19/2004 (End)
          CASE laData[21]
            lcfcondt = "POSLN.cWareCode=lcWareCode AND "+;
            " IIF(!EMPTY(lcCTStat),POSHDR.Status $ lcCTStat AND PosLn.cStyType $ 'DA',PosLn.cStyType = 'D')"
        ENDCASE
        *B603256,1 KHM 11/01/1999 (End)
  	  CASE lccurfile='ORDLINE'
  	    *B604864,1 HBG 09/03/2001 Check for warehouse of the order line not the header [Begin]
	   	*lcfcondt = "cOrdType='O' AND !(OrdHdr.Status$'CXB' OR TotQty<=0) AND OrdHdr.cWareCode=lcWareCode"
	   	lcfcondt = "cOrdType='O' AND !(OrdHdr.Status$'CXB' OR TotQty<=0) AND OrdLine.cWareCode=lcWareCode"
	   	*B604864,1 [End]
  	  CASE lccurfile='PIKTKT'
	  	*B602948,1 Start.
	  	*lcfcondt = 'Picked AND PIKTKT.cWareCode=lcWareCode'
        lcfcondt = "IIF(ORDLINE.PIKTKT='*',ORDLINE.cWareCode=lcWareCode,PIKTKT.cWareCode=lcWareCode)"
	  	*B602948,1 End.
  	  CASE lccurfile='INVLINE'
	  	lcfcondt = 'INVHDR.cWareCode=lcWareCode'
  	  CASE lccurfile='RETLINE'
	  	lcfcondt = 'RETHDR.cWareCode = lcWareCode'
   	  CASE lccurfile='RALINE'
		*B607006,1 KHM 07/16/2003 (Begin) Add the status to the condition.
		*lcfcondt = 'RETAUTH.cWareCode = lcWareCode'
		lcfcondt = 'RETAUTH.cWareCode = lcWareCode AND RETAUTH.Status <> "X"'
		*B607006,1 KHM 07/16/2003 (End)
  	  CASE lccurfile='STYINVJL'
		lcfcondt = 'STYINVJL.cWarecode = lcWareCode'
	  *--HDM[START]
	  CASE lccurfile='SHPMTHDR' && AND !EMPTY(lcCTStat)
	    SELECT POSLN
	    SET ORDER TO TAG Poslns
	    lcfcondt = "TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0"
	  *--HDM[END]
        *B604357,1 TMI [Start]  Calculate InTransit Qty ( Issued Interlocation - Recieved ) for a single WH
        lcfcondt = lcfcondt + '.AND. CWARECODE = lcWareCode'
        =lfInTrns()
        *B604357,1 TMI [End  ] 

	ENDCASE

  *-- Case of all color all warehouses.------------------------------
  CASE  llallcolors AND  llstymode
	lcmesgx  = 'Style :'+lcMajor
	DO CASE
  	  CASE lccurfile='ORDLINE'
		  lcfcondt = "cOrdType='O' AND !(OrdHdr.Status $ 'CXB' .OR. TotQty <= 0)"
	  CASE lccurfile='PIKTKT'
  		  lcfcondt = 'Picked'
  	  CASE lccurfile='CUTTKTL' AND !EMPTY(lcCTStat)
	      lcfcondt = "CUTTKTH.Status $ lcCTStat "

	  *B603256,1 KHM 11/01/1999 (Begin) Adding !laData[21] to this case
	  *B603256,1                which is only for the purchased styles.
  	  *CASE lccurfile='POSLN' AND EMPTY(lcCTStat)
  	  CASE lccurfile='POSLN' AND EMPTY(lcCTStat) AND !laData[21]
  	  *B603256,1 KHM 11/01/1999 (End)
  	    *B122043,1 ASH 04/19/2004 (Begin) Don't display the BID P/O in the Work order browse.
	    *lcfcondt = "cStyType<>'C'"
	    lcfcondt = "POSHDR.Status <> 'B' AND cStyType<>'C'"
	    *B122043,1 ASH 04/19/2004 (End)
	      
	  *B603256,1 KHM 11/01/1999 (Begin) Adding !laData[21] to this case
	  *B603256,1                which is only for the purchased styles.
  	  *CASE lccurfile='POSLN' AND !EMPTY(lcCTStat)
  	  CASE lccurfile='POSLN' AND !EMPTY(lcCTStat) AND !laData[21]
  	  *B603256,1 KHM 11/01/1999 (End)
        *B603859,1 MHM 08/29/2000 [Begin]
        *lcfcondt = "POSHDR.Status $ 'HO' AND cStyType<>'C'"
        lcfcondt = "POSHDR.Status $ 'HOA' AND cStyType<>'C'"
        *B603859,1 MHM 08/29/2000 [Begin]
 	  *B603256,1 KHM 11/01/1999 (Begin) Adding the following case to
 	  *B603256,1                handle the style that has dye and adornment orders.
 	  CASE lccurfile='POSLN' AND laData[21]
	    lcfcondt = "IIF(!EMPTY(lcCTStat),POSHDR.Status $ lcCTStat AND cStyType $ 'DA',cStyType = 'D')"
  	  *B603256,1 KHM 11/01/1999 (End)
	      
	  *--HDM[START]
	  CASE lccurfile='SHPMTHDR' && AND !EMPTY(lcCTStat)
	    lcfcondt = "TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0"
	    SELECT POSLN
	    SET ORDER TO TAG Poslns
	  *--HDM[END]
        *B604357,1 TMI [Start]  Calculate InTransit Qty ( Issued Interlocation - Recieved )
        =lfInTrns()
        *B604357,1 TMI [End  ] 

  	  *B607006,1 KHM 07/16/2003 (Begin) Add the checking of RA status.
  	  CASE lccurfile='RALINE'
        lcfcondt = 'RETAUTH.Status <> "X"'
	  *B607006,1 KHM 07/16/2003 (End)

	ENDCASE

  *-- Case of all color and specific warehouses.---------------------
  CASE  llallcolors AND !llstymode
	lcmesgx  = 'Style\Warehouse :'+lcMajor+'\'+ALLTRIM(lcwarecode)
	lcfcondt = 'cWareCode = lcWareCode'
	DO CASE
  	  CASE lccurfile='CUTTKTL'
          IF !EMPTY(lcCTStat)
	        lcfcondt = "CUTTKTL.cWareCode=lcWareCode AND CUTTKTH.Status $ lcCTStat "
          ELSE
	        lcfcondt = "CUTTKTL.cWareCode=lcWareCode"
          ENDIF 
  	  CASE lccurfile='POSLN'
	      *B603256,1 KHM 11/01/1999 (Begin) Changing the following IF command	  
          *B603256,1                to DO CASE to handle the two case of the
	      *B603256,1                imported style and the case of styles that 
    	  *B603256,1                have dye and adornment orders.
          *IF !EMPTY(lcCTStat)
	      *  lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HO' AND cStyType<>'C'"
          *ELSE
	      *  lcfcondt = "POSLN.cWareCode=lcWareCode AND cStyType<>'C'"
          *ENDIF 
          DO CASE
            CASE !EMPTY(lcCTStat) AND !laData[21]
            *B603859,1 MHM 08/29/2000 [Begin]
              *lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HO' AND cStyType<>'C'"
              lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status $ 'HOA' AND cStyType<>'C'"
            *B603859,1 MHM 08/29/2000 [Begin]
            CASE EMPTY(lcCTStat) AND !laData[21]
              *B122043,1 ASH 04/19/2004 (Begin) Don't display the BID P/O in the Work order browse.
              *lcfcondt = "POSLN.cWareCode=lcWareCode AND cStyType<>'C'"
              lcfcondt = "POSLN.cWareCode=lcWareCode AND POSHDR.Status <> 'B' AND cStyType<>'C'"
              *B122043,1 ASH 04/19/2004 (End)
            CASE laData[21] 
              lcfcondt = "POSLN.cWareCode=lcWareCode  AND "+;
              " IIF(!EMPTY(lcCTStat),POSHDR.Status $ lcCTStat AND cStyType $ 'DA',cStyType = 'D')"
          ENDCASE
          *B603256,1 KHM 11/01/1999 (End)
	  CASE lccurfile='ORDLINE'
	      *B804468,1 HBG 10/18/2001 Check for warehouse of the order line not the header [Begin]
		  *lcfcondt = "cOrdType='O' AND !(OrdHdr.Status$'CXB' OR TotQty<=0) AND OrdHdr.cWareCode=lcWareCode"
		  lcfcondt = "cOrdType='O' AND !(OrdHdr.Status$'CXB' OR TotQty<=0) AND ORDLINE.cWareCode=lcWareCode"
		  *B804468,1 [End]
  	  CASE lccurfile='PIKTKT'
	  	*B602948,1 Start.
	  	*lcfcondt = 'Picked AND PIKTKT.cWareCode=lcWareCode'
        lcfcondt = "IIF(ORDLINE.PIKTKT='*',ORDLINE.cWareCode=lcWareCode,PIKTKT.cWareCode=lcWareCode)"
	  	*B602948,1 End.

  	  CASE lccurfile='INVLINE'
	  	  lcfcondt = 'INVHDR.cWareCode=lcWareCode'
  	  CASE lccurfile='RETLINE'
	  	  lcfcondt = 'RETHDR.cWareCode = lcWareCode'

  	  CASE lccurfile='RALINE'
   	  	  *B607006,1 KHM 07/16/2003 (Begin) Add the status to the condition.
	  	  *lcfcondt = 'RETAUTH.cWareCode = lcWareCode'
	  	  lcfcondt = 'RETAUTH.cWareCode = lcWareCode AND RETAUTH.Status <> "X"'
	  	  *B607006,1 KHM 07/16/2003 (End)
	  	  
	  *--HDM[START]
	  CASE lccurfile='SHPMTHDR' && AND !EMPTY(lcCTStat)
	    lcfcondt = "TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0"
	    SELECT POSLN
	    SET ORDER TO TAG Poslns
	  *--HDM[END]
        *B604357,1 TMI [Start]  Calculate InTransit Qty ( Issued Interlocation - Recieved ) for a single WH
        lcfcondt = lcfcondt + '.AND. CWARECODE = lcWareCode'
        =lfInTrns()
        *B604357,1 TMI [End  ] 

    ENDCASE
ENDCASE

llfound = SEEK( lcexkey )
IF llfound AND lcfcondt <> '.T.'
	LOCATE REST WHILE &lcwcondt FOR &lcfcondt
	llfound=FOUND()
ENDIF

IF ! llfound
  SET RELATION TO
ENDIF
RETURN llfound
*!**********************************************************************
*! Name      : lfvWip
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Browse the Cutting Ticket or Purcharse Order Lines.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCTStat ->C/t or P/o available statuses.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************

FUNCTION lfvwip
PARA lcCTStat
PRIVATE lcBrFields

*B603711,1 (Begin) If PS is installed let him inquire.
*IF ( laData[21] AND !llMFInstld ) OR ( !laData[21] AND !llPOInstld )
IF (OCCURS('PS',gcCmpModules)=0) AND (( laData[21] AND !llMFInstld ) OR ( !laData[21] AND !llPOInstld ))
*B603711,1 (End)
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG',IIF(laData[21],'Manufacturing','Style Purchase Order'))
  RETURN
ENDIF
lnAlias=SELECT()
IF laData[21]

  *B603256,1 KHM 11/01/1999 (Begin) Opening the PosHdr & PosLn if they
  *B603256,1                were not opend because they might be used to 
  *B603256,1                get the dye and adornment orders.
  IF !laOpFile[6]
    laOpFile[6] = gfOpenFile(gcDataDir+'POShdr','POSHdr'  ,'SH')
  ENDIF
  IF !laOpFile[7]
    laOpFile[7] = gfOpenFile(gcDataDir+'POSLN' ,'POSLNS'  ,'SH')
  ENDIF
  SELECT POSLN
  SET RELATION TO cStyType+Po INTO POSHDR
  *B603256,1 KHM 11/01/1999 (End)
  lcCTStat=IIF(lcCTStat='BAR',IIF(lnBarNo=1,'H',IIF(lnBarNo=2,'O','A')),lcCTStat)
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH 

  *B603256,1 KHM 11/01/1999 (Begin) Checking for the CutTkts and the
  *B603256,1                dye and adornment orders.
  *IF ! lfcnrolex('CUTTKTL',lcCTStat)    
  llDyeOrd = lfcnrolex('POSLN',lcCTStat)
  IF llDyeOrd
    lcwcondt1 = lcwcondt
    lcfcondt1 = lcfcondt
  ENDIF
  llCutTkt = lfcnrolex('CUTTKTL',lcCTStat)  
  IF !llCutTkt AND !llDyeOrd
  *B603256,1 KHM 11/01/1999 (End)
  
    DO CASE
      CASE lcCTStat=='H'
        lcStBar='Hold '
      CASE lcCTStat=='O'
        lcStBar='Open '
      CASE lcCTStat=='A'
        lcStBar='Actualize '
      OTHERWISE
        lcStBar=''
    ENDCASE
    *--'No '+lcStBar+'cutting tickets found for '+lcmesgx+'.'
    =gfModalGen('TRM42040B42001','DIALOG',lcStBar+'|'+lcmesgx)
    SELECT(lnAlias)
	RETURN
  ENDIF

  xcuttkt  = cuttkt
  xstatus  = cuttkth.status

  *B603256,1 KHM 11/01/1999 (Begin) Adding cType field to tell if the
  *B603256,1                transaction is a CutTkt,Dye order or Adorment order.
  *CREATE CURSOR (lcCursName) (cuttkt C(6),STATUS C(1),entered D,COMPLETE D,;
		bud N(7),rec N(7), dam N(7), can N(7),;
		STYLE C(19) , Dyelot C(10))
  CREATE CURSOR (lcCursName) (cuttkt C(6),STATUS C(1),entered D,COMPLETE D,;
		bud N(7),rec N(7), dam N(7), can N(7),;
		STYLE C(19) , Dyelot C(10),cType C(1))
  *B603256,1 KHM 11/01/1999 (End)
  
  *--HDM E301134,1 Allow PO Browse to display dyelots[Start]
  *INDEX ON Cuttkt+style TAG (lccursname) OF (lccursname)
  INDEX ON Cuttkt+style + Dyelot TAG (lccursname) OF (lccursname)
  *--HDM E301134,1 Allow PO Browse to display dyelots[End]
  IF llMScale
    lccursn1 = gfTempName()
    *B603256,1 KHM 11/01/1999 (Begin) Adding cType field to tell if the
    *B603256,1                transaction is a CutTkt,Dye order or Adorment order.
    *CREATE CURSOR (lccursn1) (cuttkt C(6),STATUS C(1),entered D,COMPLETE D,;
		  bud N(7),rec N(7), dam N(7), can N(7),;
    		STYLE C(19) , Dyelot C(10))
    CREATE CURSOR (lccursn1) (cuttkt C(6),STATUS C(1),entered D,COMPLETE D,;
		  bud N(7),rec N(7), dam N(7), can N(7),;
    		STYLE C(19) , Dyelot C(10),cType C(1))
    *B603256,1 KHM 11/01/1999 (End)
    		
    *--HDM E301134,1 Allow PO Browse to display dyelots[Start]
    *INDEX ON Cuttkt+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) TAG (lccursn1) OF (lccursn1)
    INDEX ON Cuttkt+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) + Dyelot TAG (lccursn1) OF (lccursn1)
    *--HDM E301134,1 Allow PO Browse to display dyelots[End]
  ENDIF

  *B603256,1 KHM 11/01/1999 (Begin) Adding the checking of llCuttkt to tell
  *B603256,1                if there are any cuttkts for the selected style.
  IF llCutTkt
  *B603256,1 KHM 11/01/1999 (End)
    SELECT cuttktl
    lcoldcttkt = SPACE(1)
    lcoldcttst = SPACE(1)
    SCAN WHILE &lcwcondt FOR &lcfcondt
  	  *IF Cuttkt <> lcoldcttkt
	    lcoldcttkt = Cuttkt
        IF !SEEK(lcoldcttkt+ CutTktl.STYLE + CutTktl.Dyelot , lccursname)
    	  =SEEK(cuttkt,'CUTTKTH')
          *B603256,1 KHM 11/01/1999 (Begin) Changing the insert command by
          *B603256,1                adding cType field.
	      *INSERT INTO (lccursname) (cuttkt,STATUS,entered,COMPLETE,STYLE,Dyelot);
		    		VALUES (cuttkth.cuttkt, cuttkth.status,;
			    	cuttkth.entered, cuttkth.complete,;
				    cuttktl.style ,cuttktl.Dyelot)
	      INSERT INTO (lccursname) (cuttkt,STATUS,entered,COMPLETE,STYLE,Dyelot,cType);
		    		VALUES (cuttkth.cuttkt, cuttkth.status,;
			    	cuttkth.entered, cuttkth.complete,;
				    cuttktl.style ,cuttktl.Dyelot,'C')
          *B603256,1 KHM 11/01/1999 (End)
        ENDIF
	  *ENDIF

      IF llMScale AND Cuttkt+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) <> lcoldcttst
	    lcoldcttst = Cuttkt+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) + Dyelot
        IF !SEEK(lcoldcttst,lccursn1)
    	  =SEEK(cuttkt,'CUTTKTH')
            *B603256,1 KHM 11/01/1999 (Begin) Changing the insert command by
            *B603256,1                adding cType field.
	    	*INSERT INTO (lccursn1) (cuttkt,STATUS,entered,COMPLETE,STYLE,Dyelot);
		    		VALUES (cuttkth.cuttkt, cuttkth.status,;
			    	cuttkth.entered, cuttkth.complete,;
				    cuttktl.style,Dyelot)
	    	INSERT INTO (lccursn1) (cuttkt,STATUS,entered,COMPLETE,STYLE,Dyelot,cType);
		    		VALUES (cuttkth.cuttkt, cuttkth.status,;
			    	cuttkth.entered, cuttkth.complete,;
				    cuttktl.style,Dyelot,'C')
           *B603256,1 KHM 11/01/1999 (End)
        ENDIF
  	  ENDIF

      IF SEEK(Cuttkt+Style+Dyelot,lccursname)
		 DO CASE
      		CASE trancd='1'
		    	REPLACE &lccursname..bud WITH &lccursname..bud + cuttktl.totqty
    		CASE trancd='2'
	    		REPLACE &lccursname..rec WITH &lccursname..rec + cuttktl.totqty
    		CASE trancd='3'
	    		REPLACE &lccursname..dam WITH &lccursname..dam + cuttktl.totqty
    		CASE trancd='4'
	    		REPLACE &lccursname..can WITH &lccursname..can + cuttktl.totqty
		  ENDCASE
      ENDIF
      IF llMScale AND SEEK(Cuttkt+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)+Dyelot,lccursn1)
		 DO CASE
      		CASE trancd='1'
		    	REPLACE &lccursn1..bud WITH &lccursn1..bud + cuttktl.totqty
    		CASE trancd='2'
	    		REPLACE &lccursn1..rec WITH &lccursn1..rec + cuttktl.totqty
    		CASE trancd='3'
	    		REPLACE &lccursn1..dam WITH &lccursn1..dam + cuttktl.totqty
    		CASE trancd='4'
	    		REPLACE &lccursn1..can WITH &lccursn1..can + cuttktl.totqty
		  ENDCASE
      ENDIF 
	ENDSCAN
  ENDIF	

  *B603256,1 KHM 11/01/1999 (Begin) Adding the following code to check if
  *B603256,1                there are any dye or adornment orders then
  *B603256,1                get them all.
  IF llDyeOrd
    =lfGetDyOrd()
  ENDIF
  *B603256,1 KHM 11/01/1999 (End)
  
	SELECT (lccursname)
	GO TOP
	*B603256,1 KHM 11/01/1999 (Begin) Changing the header of the browse	
	*lcCTbrowtl = 'Cutting Tickets'
	lcCTbrowtl = IIF(llCutTkt AND llDyeOrd,'Cutting Tickets/Work Orders',;
	             IIF(llCutTkt,'Cutting Tickets','Work Orders'))
	*B603256,1 KHM 11/01/1999 (End)
	
	*B603256,1 KHM 11/01/1999 (Begin) Changing the browse
	*lcbrfields = "CUTTKT    :H='Cuttkt',"+;
		"STATUS    :H='S',"+;
		"ENTERED   :H='Entered',"+;
		"COMPLETE  :H='Complete',"+;
		"Dyelot    :H='Dyelot',"+;
		"BUD    :7 :H='Budget',"+;
		"REC    :7 :H='Recvd.',"+;
		"DAM    :7 :H='Dmgd.' ,"+;
		"CAN = IIF(Status = 'X', MAX((BUD-(REC+DAM)),0) ,CAN)"+;
		":7 :H='Cncld' :P='9999999',"+;
		"OPN = IIF(Status $ 'XC', 0, MAX((BUD-(REC+DAM+CAN)),0))"+;
		":7 :H='Open' :P='9999999'"
		
	lcbrfields = "lcPoType=IIF(cType='C','Cut Ticket',IIF(cType='D','Dye Order','Adornment Order')):H='Type         ',"+;
	    "CUTTKT    :H='Mfg. Orders',"+;
		"STATUS    :H='S',"+;
		"ENTERED   :H='Entered',"+;
		"COMPLETE  :H='Complete',"+;
		"Dyelot    :H='Dyelot',"+;
		"BUD    :7 :H='Budget',"+;
		"REC    :7 :H='Recvd.',"+;
		"DAM    :7 :H='Dmgd.' ,"+;
		"CAN = IIF(Status = 'X', MAX((BUD-(REC+DAM)),0) ,CAN)"+;
		":7 :H='Cncld' :P='9999999',"+;
		"OPN = IIF(Status $ 'XC', 0, MAX((BUD-(REC+DAM+CAN)),0))"+;
		":7 :H='Open' :P='9999999'"
    *B603256,1 KHM 11/01/1999 (End)
    llInquiry=.F.
	=ARIABROW('',lcCTbrowtl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'lfWipInfo')
    SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
	=lfvViewLoc()
	IF llInquiry
      lcCTInq="'"+&lccursname..Cuttkt+"'"
      DO gpDoProg WITH 'AWRMFCUTKT',.F.,'MF',lcCTInq
	ENDIF
    SELECT CUTTKTL
    SET RELATION TO

************************* P/O *********************************************
ELSE  && Make No.....
  *E301077,7 OPEN PO Files.
  IF !laOpFile[6]
    laOpFile[6] = gfOpenFile(gcDataDir+'POShdr','POSHdr'  ,'SH')
  ENDIF
  IF !laOpFile[7]
    laOpFile[7] = gfOpenFile(gcDataDir+'POSLN' ,'POSLNS'  ,'SH')
  ENDIF

  SELECT POSLN
  SET RELATION TO cStyType+Po INTO POSHDR

  IF ! lfcnrolex('POSLN',lcCTStat)
	*--No Work in process found for '+lcmesgx+'.'
    =gfModalGen('TRM42041B42001','DIALOG',IIF(EMPTY(lcCTStat),'Work Orders','Work in process')+'|'+lcmesgx)
    SELECT POSLN
	SET RELATION TO
    SELECT(lnAlias)
	RETURN
  ENDIF

  WAIT 'Selecting Purchase Orders...' WINDOW NOWAIT
  lcPObrowtl = 'Purchase Orders'
  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty and save it in the temp. file, 
  *B605797,1                to fix the problem of wrong computing the open qty in case of over receive.
  *CREATE CURSOR (lccursname) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),;
		                      STYLE C(19) , Dyelot C(10))
  *B606743,1 WAB (Start) browse the wIP dpend on lineNO
  *CREATE CURSOR (lccursname) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),Opn N(7),;
          		              rec1 N(7), dam1 N(7), can1 N(7),Opn1 N(7),;
		                      STYLE C(19) , Dyelot C(10))
  **B605797,1 ASH 04/09/2002 (End)		                      
  **--HDM E301134,1 Allow PO Browse to display dyelots[start]
  **INDEX ON cPoType+Po TAG (lccursname) OF (lccursname)
  **INDEX ON cPoType+Po + STYLE + Dyelot TAG (lccursname) OF (lccursname)
  *--HDM E301134,1 Allow PO Browse to display dyelots[start]

  CREATE CURSOR (lccursname) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),Opn N(7),;
          		              rec1 N(7), dam1 N(7), can1 N(7),Opn1 N(7),;
		                      STYLE C(19) , Dyelot C(10) ,lineNo N(6))		                      
  INDEX ON cPoType+Po + STYLE + STR(lineno,6) TAG (lccursname) OF (lccursname)
  *B606743,1 WAB (End)
  IF llMScale
    lccursn1 = gfTempName()
    *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty and save it in the temp. file, 
    *B605797,1                to fix the problem of wrong computing the open qty in case of over receive.
    *CREATE CURSOR (lccursn1) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),;
		                      STYLE C(19) , Dyelot C(10))
    *B606743,1 WAB (Start) browse the wIP dpend on lineNO		                      
    *CREATE CURSOR (lccursn1) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),Opn N(7),;
		                      STYLE C(19) , Dyelot C(10))
    *INDEX ON cPoType+Po+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) + Dyelot TAG (lccursn1) OF (lccursn1)
    CREATE CURSOR (lccursn1) (cPoType C(1),po C(6),vendor C(8),vname C(30),COMPLETE D,;
                        	  price N(9,2),STATUS C(1),available D,;
          		              ord N(7),rec N(7), dam N(7), can N(7),Opn N(7),;
		                      STYLE C(19) , Dyelot C(10) ,lineNo N(6))
    INDEX ON cPoType+Po+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) + STR(lineno,6) TAG (lccursn1) OF (lccursn1)
    *B606743,1 WAB (End) 
  ENDIF

  SELECT POSLN
  lcoldpo = SPACE(1)
  lcoldcttst = SPACE(1)
  *B605797,1 ASH 04/09/2002 (Begin) A flag to detect if the data computed for All Warehouses or a specific warehouse.
  llWare = .F.
  IF 'POSLN.cWareCode=lcWareCode AND' $ lcfcondt
    lcfcondt = STRTRAN(lcfcondt ,'POSLN.cWareCode=lcWareCode AND','')
    llWare = .T.
  ENDIF
  *B605797,1 ASH 04/09/2002 (End)
  =SEEK(lcExKey)
  SCAN REST WHILE &lcwcondt FOR &lcfcondt
    *--HDM E301134,1 Allow PO Browse to display dyelots[start]
  	*IF cStyType+Po <> lcoldpo
    *--HDM E301134,1 Allow PO Browse to display dyelots[End]
    lcOldPo = cStyType+Po
    *B606743,1 WAB (Start)
    *IF !SEEK(cStyType+Po+Style+Dyelot,lccursname) AND SEEK(lcoldpo,'POSHDR')
    IF !SEEK(cStyType+Po+Style+STR(lineno,6),lccursname) AND SEEK(lcoldpo,'POSHDR')
    *B606743,1 WAB (End)
      *B605797,1 ASH 04/09/2002 (Begin) A variable to hold the open qty's.
      FOR I = 1 TO 8
        Z = STR(I,1)
        lnPoOpen&Z = 0
        lnPoW&Z    = 0
      ENDFOR
      *B605797,1 ASH 04/09/2002 (End)
      *B606743,1 WAB (Start)
      *INSERT INTO (lccursname) (cPoType,po,vendor,Status,Complete,available,price,STATUS,STYLE,DYELOT);
           VALUES (poshdr.cStyType,poshdr.po,poshdr.vendor,poshdr.status,poshdr.complete,poshdr.available,;
		       	  posln.ncost1,poshdr.status,posln.style , POSLN.DYELOT)
      INSERT INTO (lccursname) (cPoType,po,vendor,Status,Complete,available,price,STATUS,STYLE,DYELOT,lineno);
           VALUES (poshdr.cStyType,poshdr.po,poshdr.vendor,poshdr.status,poshdr.complete,poshdr.available,;
		       	  posln.ncost1,poshdr.status,posln.style , POSLN.DYELOT , POSLN.lineno)
      *B606743,1 WAB (End)
    ENDIF 
    *--HDM E301134,1 Allow PO Browse to display dyelots[Start]
	*ENDIF
    *--HDM E301134,1 Allow PO Browse to display dyelots[End]
    IF llMScale AND cStyType+Po+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3) <> lcoldcttst
      lcoldcttst = cStyType+Po+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)
      IF !SEEK(lcoldcttst,lccursn1) AND SEEK(cStyType+Po,'POSHDR')
  	    INSERT INTO (lccursn1) (cPoType,po,vendor,Status,Complete,available,price,STATUS,STYLE);
          VALUES (poshdr.cStyType,poshdr.po,poshdr.vendor,poshdr.status,poshdr.complete,poshdr.available,;
		       	  posln.ncost1,poshdr.status,posln.style)
      ENDIF
  	ENDIF
  	*B606743,1 WAB (Start)
    *IF SEEK(cStyType+Po+Style+Dyelot,lccursname)
    IF SEEK(cStyType+Po+Style+STR(lineno,6),lccursname)
    *B606743,1 WAB (End)
      DO CASE
	    CASE trancd='1'
          *B606743,1 WAB (Start)
	      REPLACE &lccursname..Dyelot WITH posln.Dyelot
          *B606743,1 WAB (End)

	   	  REPLACE &lccursname..ord WITH &lccursname..ord + posln.totqty
          *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          FOR I = 1 TO 8
 		    Z = STR(I,1)
            lnPoOpen&Z = lnPoOpen&Z + PosLn.Qty&Z
            IF llWare AND lcWareCode = Posln.cWareCode
              lnPoW&Z    = lnPoW&Z    + PosLn.Qty&Z
            ENDIF
          ENDFOR
         *B605797,1 ASH 04/09/2002 (End)
 		CASE trancd='2'
	   	  REPLACE &lccursname..rec WITH &lccursname..rec + posln.totqty
	   	  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          FOR I = 1 TO 8
 		    Z = STR(I,1)
            lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
            IF llWare AND lcWareCode = Posln.cWareCode
              lnPoW&Z    = MAX(lnPoW&Z-PosLn.Qty&Z,0)
            ENDIF
          ENDFOR
          IF llWare AND lcWareCode = Posln.cWareCode
            REPLACE &lccursname..rec1 WITH &lccursname..rec1 + posln.totqty
          ENDIF
          
          *B605797,1 ASH 04/09/2002 (End)
	   	  
 	    CASE trancd='4'
	      REPLACE &lccursname..dam WITH &lccursname..dam + posln.totqty
	      *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          FOR I = 1 TO 8
 		    Z = STR(I,1)
            lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
            IF llWare AND lcWareCode = Posln.cWareCode
              lnPoW&Z    = MAX(lnPoW&Z-PosLn.Qty&Z,0)
            ENDIF
          ENDFOR
          IF llWare AND lcWareCode = Posln.cWareCode
            REPLACE &lccursname..dam1 WITH &lccursname..dam1 + posln.totqty          
          ENDIF
          *B605797,1 ASH 04/09/2002 (End)
	      
	    CASE trancd='5'
		  REPLACE &lccursname..can WITH &lccursname..can + posln.totqty
		  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          FOR I = 1 TO 8
 		    Z = STR(I,1)
            lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
            IF llWare AND lcWareCode = Posln.cWareCode
              lnPoW&Z    = MAX(lnPoW&Z-PosLn.Qty&Z,0)
            ENDIF
          ENDFOR
          IF llWare AND lcWareCode = Posln.cWareCode
            REPLACE &lccursname..can1 WITH &lccursname..can1 + posln.totqty
          ENDIF
          *B605797,1 ASH 04/09/2002 (End)
		  
      ENDCASE
      *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
      REPLACE &lcCursName..Opn  WITH IIF(&lccursname..Status $ 'XC',0,lnPoOpen1+lnPoOpen2+lnPoOpen3+lnPoOpen4+lnPoOpen5+lnPoOpen6+lnPoOpen7+lnPoOpen8)
      REPLACE &lcCursName..Opn1 WITH IIF(&lccursname..Status $ 'XC',0,lnPoW1+lnPoW2+lnPoW3+lnPoW4+lnPoW5+lnPoW6+lnPoW7+lnPoW8)
      *B605797,1 ASH 04/09/2002 (End)
    ENDIF 
    IF llMScale AND SEEK(cStyType+Po+SUBSTR(Style,1,lnStyleWid)+SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3),lccursn1)
      DO CASE
	    CASE trancd='1'
	   	  REPLACE &lccursn1..ord WITH &lccursn1..ord + posln.totqty
	   	  *B606534,1 ASH 10/14/2002 (Begin) Comment the following lines, as the opn qty computed before.
	   	  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          *FOR I = 1 TO 8
 		  *  Z = STR(I,1)
          *  lnPoOpen&Z = lnPoOpen&Z + PosLn.Qty&Z
          *ENDFOR
          *B605797,1 ASH 04/09/2002 (End)
	   	  *B606534,1 ASH 10/14/2002 (End)
 		CASE trancd='2'
	   	  REPLACE &lccursn1..rec WITH &lccursn1..rec + posln.totqty
	   	  *B606534,1 ASH 10/14/2002 (Begin) Comment the following lines, as the opn qty computed before.
	   	  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          *FOR I = 1 TO 8
 		  *  Z = STR(I,1)
          *  lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
          *ENDFOR
          *B605797,1 ASH 04/09/2002 (End)
	   	  *B606534,1 ASH 10/14/2002 (End)
	   	  
 	    CASE trancd='4'
	      REPLACE &lccursn1..dam WITH &lccursn1..dam + posln.totqty
	      *B606534,1 ASH 10/14/2002 (Begin) Comment the following lines, as the opn qty computed before.
	      *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          *FOR I = 1 TO 8
 		  *  Z = STR(I,1)
          *  lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
          *ENDFOR
          *B605797,1 ASH 04/09/2002 (End)
          *B606534,1 ASH 10/14/2002 (End)
	      
	    CASE trancd='5'
		  REPLACE &lccursn1..can WITH &lccursn1..can + posln.totqty
		  *B606534,1 ASH 10/14/2002 (Begin) Comment the following lines, as the opn qty computed before.
		  *B605797,1 ASH 04/09/2002 (Begin) Compute the open qty for each PO.
          *FOR I = 1 TO 8
 		  *  Z = STR(I,1)
          *  lnPoOpen&Z = MAX(lnPoOpen&Z-PosLn.Qty&Z,0)
          *ENDFOR
		  *B605797,1 ASH 04/09/2002 (End)
		  *B606534,1 ASH 10/14/2002 (End)
 	  ENDCASE
    ENDIF 
  ENDSCAN
  WAIT CLEAR
  SELECT (lccursname)
  GO TOP
  *SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
  
  *lcbrfields = "lcPotyp = IIF(cPoType ='R','Return P/O',IIF(cPoType ='N','Inter Loc. P/O','Purchase order')) :17 :H='P/O Type',"+;
               "PO :7:H='P/O #',"+;
               "STATUS :2:H='S',"+;
			   "VENDOR :11:H='Vendor',"+;
			   "COMPLETE :8 :H='Complete',"+;
			   "AVAILABLE :8 :H='Available',"+;
			   "Dyelot  :H='Dyelot',"+;
			   "ORD :7 :H='Ordered' :P='9999999',"+;
			   "REC :7 :H='Recvd.' :P='9999999',"+;
			   "DAM :7 :H='Dmgd.' :P='9999999',"+;
			   "CAN = IIF(Status $ 'XC', MAX((ORD-(REC+DAM+CAN)),0),CAN) :7 :H='Cncld' :P='9999999',"+;
			   "OPN = IIF(Status $ 'XC', 0, MAX((ORD-(REC+DAM+CAN)),0)) :7 :H='Open' :P='9999999'"

  *B603684,1 AME[Start]
  *lcbrfields = "lcPotyp = IIF(&lccursname..cPoType ='R','Return P/O',IIF(&lccursname..cPoType ='N','Inter Loc. P/O','Purchase order')) :17 :H='P/O Type',"+;
               "&lccursname..PO :7:H='P/O #',"+;
               "&lccursname..STATUS :2:H='S',"+;
			   "&lccursname..VENDOR :11:H='Vendor',"+;
			   "&lccursname..COMPLETE :8 :H='Complete',"+;
			   "&lccursname..AVAILABLE :8 :H='Available',"+;
			   "&lccursname..Dyelot  :H='Dyelot',"+;
			   "&lccursname..ORD :7 :H='Ordered' :P='9999999',"+;
			   "&lccursname..REC :7 :H='Recvd.' :P='9999999',"+;
			   "&lccursname..DAM :7 :H='Dmgd.' :P='9999999',"+;
			   "nCAN = IIF(&lccursname..Status $ 'XC', MAX((&lccursname..ORD-(&lccursname..REC+&lccursname..DAM+&lccursname..CAN)),0),&lccursname..CAN) :7 :H='Cncld' :P='9999999',"+;
			   "nOPN = IIF(&lccursname..Status $ 'XC', 0, MAX((&lccursname..ORD-(&lccursname..REC+&lccursname..DAM+&lccursname..CAN)),0)) :7 :H='Open' :P='9999999'"
  *B605797,1 ASH 04/09/2002 (Begin) Display the open qty from the temp file instead of computing it online.
  IF !llWare
    lcbrfields = "lcPotyp = IIF(&lccursname..cPoType ='R','Return P/O',IIF(&lccursname..cPoType ='N','Inter Loc. P/O','Purchase order')) :17 :H='P/O Type',"+;
                 "&lccursname..PO :7:H='P/O #',"+;
                 "&lccursname..STATUS :2:H='S',"+;
		         "&lccursname..VENDOR :11:H='Vendor',"+;
			     "&lccursname..COMPLETE :8 :H='Complete',"+;
   			     "&lccursname..AVAILABLE :8 :H='Available',"+;
			     "&lccursname..Dyelot  :H='Dyelot',"+;
			     "&lccursname..ORD :7 :H='Ordered' :P='9999999',"+;
			     "&lccursname..REC :7 :H='Recvd.' :P='9999999',"+;
			     "&lccursname..DAM :7 :H='Dmgd.' :P='9999999',"+;
			     "nCAN = IIF(&lccursname..Status $ 'XC', MAX((&lccursname..ORD-(&lccursname..REC+&lccursname..DAM)),0),&lccursname..CAN) :7 :H='Cncld' :P='9999999',"+;
			     "&lccursname..Opn :7 :H='Open' :P='9999999'"
  ELSE
    lcbrfields = "lcPotyp = IIF(&lccursname..cPoType ='R','Return P/O',IIF(&lccursname..cPoType ='N','Inter Loc. P/O','Purchase order')) :17 :H='P/O Type',"+;
                 "&lccursname..PO :7:H='P/O #',"+;
                 "&lccursname..STATUS :2:H='S',"+;
		         "&lccursname..VENDOR :11:H='Vendor',"+;
			     "&lccursname..COMPLETE :8 :H='Complete',"+;
   			     "&lccursname..AVAILABLE :8 :H='Available',"+;
			     "&lccursname..Dyelot  :H='Dyelot',"+;
			     "&lccursname..ORD :7 :H='Ordered' :P='9999999',"+;
			     "&lccursname..REC :7 :H='Recvd.' :P='9999999',"+;
			     "&lccursname..DAM :7 :H='Dmgd.' :P='9999999',"+;
			     "nCAN = IIF(&lccursname..Status $ 'XC', MAX((&lccursname..ORD-(&lccursname..REC+&lccursname..DAM)),0),&lccursname..CAN) :7 :H='Cncld' :P='9999999',"+;
			     "&lccursname..Opn :7 :H='Open' :P='9999999',"+;
			     "&lccursname..REC1 :7 :H='Recvd./WH' :P='9999999',"+;
			     "&lccursname..DAM1 :7 :H='Dmgd./WH' :P='9999999',"+;
			     "nCAN1 = IIF(&lccursname..Status $ 'XC', MAX((&lccursname..ORD-(&lccursname..REC1+&lccursname..DAM1)),0),&lccursname..CAN1) :7 :H='Cncld/WH' :P='9999999'"
			     *"&lccursname..OPN1 :7 :H='Open/WH' :P='9999999'"
   
  ENDIF			   

			   
  *B605797,1 ASH 04/09/2002 (End)
  *B603684,1 AME[End]
  llInquiry=.F.
  *--HDM B602701,1 03/30/1999[Start] Bug Of Changing the active cursor so in-correct information appears in
  *                     WO & WIP Browse
  *SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
  *--HDM B602701,1 03/30/1999[End]
  = ARIABROW('',lcPObrowtl,gnbrhsrow1,gnbrhscol1,gnbrhsrow2,gnbrhscol2,'lfWipInfo')
  SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
  =lfvViewLoc()
  *SHOW GETS
  IF llInquiry
  	lcPOInq=+"'"+&lccursname..cPOType+"','"+&lccursname..Po+"'"
    DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcPOInq
  ENDIF

ENDIF
SELECT(lnAlias)
RETURN


*!**********************************************************************
*! Name      : lfWipInfo
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid of P/O browse select button.
*!             Valid of Cuttkts browse select button.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfWipInfo

lcTmpWip = gftempname()
lcTmpWCur = IIF(llMScale,lccursn1,lccursname)
SELECT (lcTmpWCur)

IF laData[21]

  *B603256,1 KHM 11/01/1999 (Begin) Adding this check in order to 
  *B603256,1                distinguish between the CutTkts and the Dye 
  *B603256,1                and Adornment orders
  IF &lccursname..cType = 'C'
  *B603256,1 KHM 11/01/1999 (End)
    =SEEK(&lccursname..CUTTKT,'CUTTKTH')
    IF !USED(lcTmpWip)
      USE (gcDataDir+'CUTTKTL') AGAIN ALIAS (lcTmpWip) ORDER TAG CUTTKTLS IN 0
    ENDIF
    IF llMScale
      SELECT (lcTmpWCur)
      SET FILTER TO Cuttkt=CUTTKTH.Cuttkt
    ENDIF

  *B603256,1 KHM 11/01/1999 (Begin) Adding the else to handle the dye
  *B603256,1                and adornment orders.
  ELSE  
    =SEEK(&lccursname..cType+&lccursname..CuttKt,'POSHDR')
    IF !USED(lcTmpWip)
        USE (gcDataDir+'POSLN') AGAIN ALIAS (lcTmpWip) ORDER TAG POSLNS IN 0
    ENDIF

    IF llMScale
      SELECT (lcTmpWCur)
      SET FILTER TO cType =POSHDR.cStyType AND Cuttkt=POSHDR.Po
    ENDIF
  ENDIF
  *B603256,1 KHM 11/01/1999 (End)
ELSE
  =SEEK(&lccursname..cPOType+&lccursname..PO,'POSHDR')
  IF !USED(lcTmpWip)
    USE (gcDataDir+'POSLN') AGAIN ALIAS (lcTmpWip) ORDER TAG poslns IN 0
  ENDIF
  IF llMScale
    SELECT (lcTmpWCur)
    SET FILTER TO cPOType =POSHDR.cStyType AND PO =POSHDR.Po
  ENDIF
ENDIF

lnStyRec = IIF(!EOF('STYLE'),RECNO('STYLE'),0)
DIMENSION xbud1(8),xrec1(8),xdam1(8),xcan1(8),xint1(8)  && Totals by size.
STORE 0 TO xbud1,xrec1,xdam1,xcan1,xint1,xtot1
STORE '' TO lcWSz1,lcWSz2,lcWSz3,lcWSz4,lcWSz5,lcWSz6,lcWSz7,lcWSz8

=lfWipCalc(' ')

IF laData[21]
  *B603256,1 KHM 11/01/1999 (Begin) Adding the following variable
  *B603256,1                to do not display the CutTkt button in 
  *B603256,1                case of Dye and Adornment order.
  llDispButt = IIF(&lccursname..cType $ 'AD',.F.,.T.)  
  *B603256,1 KHM 11/01/1999 (End)

  DO (gcScrDir+gcWinAppl+"\ICWipQM.SPX")  
ELSE
  *B603711,2 AMH 08/20/2000 do not display the Purchase Order button in 
  *B603711,2                case of PO not Installed [Start]
  llDispButt = IIF(OCCURS('PO',gcCmpModules)=0,.F.,.T.)  
  *B603711,2 AMH 08/20/2000 [End  ]
  DO (gcScrDir+gcWinAppl+"\ICWipQI.SPX")
ENDIF

IF llInquiry
  llBrowTmnat = .T.
ENDIF

IF USED(lcTmpWip)
  SELECT (lcTmpWip)
  USE
ENDIF
IF lnStyRec<>0
  GOTO lnStyRec IN STYLE
ENDIF
SELECT (lccursname)
RETURN


*!**********************************************************************
*! Name      : lfWipCalc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Calculate lines Wip.
*!**********************************************************************
*---rere
FUNCTION lfWipCalc
PARA LCPP

SELECT (lcTmpWCur)

*--B802473,1 [Start] we shouldn't change record pointer in the cursor
*IF EMPTY(LCPP)
*  GO TOP
*ENDIF
*--B802473,1 [End]

IF LCPP='P' AND !BOF()
  SKIP -1
ENDIF
IF LCPP='N' AND !EOF()
  SKIP
  IF EOF()
    SKIP -1
  ENDIF  
ENDIF
*--
*-- B602372,1 Incorrect WO WIP Qty (Qty are for first color only)[start]
lcExKey  = lcMajor+lcNMKey
LcExpWc = "Style = lcExKey"
*-- B602372,1 Incorrect WO WIP Qty (Qty are for first color only)[End]
IF llMScale  
  IF laData[21]

    *B603256,1 KHM 11/01/1999 (Begin) Adding the following IF to handle both cases the CutTkt 'C'
    *B603256,1                case and the Dye and Adornment orders case.
    *lcExpFc = "Cuttkt =CUTTKTH.Cuttkt AND SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)=SUBSTR(&lcTmpWCur..Style,lnStyleWid+2+lnColorWid-3,3)"
    IF &lccursname..cType = 'C'        
      lcExpFc = "Cuttkt =CUTTKTH.Cuttkt AND SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)=SUBSTR(&lcTmpWCur..Style,lnStyleWid+2+lnColorWid-3,3)"
    ELSE
      lcExpFc = "cStyType+Po = POSHDR.cStyType+POSHDR.PO AND SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)=SUBSTR(&lcTmpWCur..Style,lnStyleWid+2+lnColorWid-3,3)"
    ENDIF
    *B603256,1 KHM 11/01/1999 (End)
    
  ELSE
    lcExpFc = "cStyType+Po = POSHDR.cStyType+POSHDR.PO AND SUBSTR(Style,lnStyleWid+2+lnColorWid-3,3)=SUBSTR(&lcTmpWCur..Style,lnStyleWid+2+lnColorWid-3,3)"
 ENDIF
ELSE
  IF laData[21]
    
    *-- B602372,1 Incorrect WO WIP Qty (Qty are for first color only)[start]
    *    lcExKey = &lcTmpWCur..Style+CUTTKTH.CUTTKT
    *    LcExpWc = "Style+Cuttkt = lcExKey"
    
    *B603256,1 KHM 11/01/1999 (Begin) Adding the following IF to handle both cases the CutTkt
    *B603256,1                case and the Dye and Adornment orders.
    *lcExpFc = "Cuttkt =CUTTKTH.Cuttkt"
    IF cType = 'C'    
      lcExpFc = "Cuttkt =CUTTKTH.Cuttkt"
    ELSE
      lcExpFc = "PO =PosHdr.PO"
    ENDIF
    *B603256,1 KHM 11/01/1999 (End)
    
    *-- B602372,1 Incorrect WO WIP Qty (Qty are for first color only)[end]

  ELSE
    lcExKey=&lccursname..Style+&lccursname..cPOType+&lccursname..PO
    LcExpWc = "Style+cStyType+Po = lcExKey"
    lcExpFc = ".T."
 ENDIF
ENDIF
lcExpFr = IIF(!llStyMode,"cWareCode=lcWareCode",".T.")

IF llMScale
  =SEEK('S'+SUBSTR(&lcTmpWCur..Style,lnStyleWid+2+lnColorWid-3,3),'SCALE')  
ELSE
  =SEEK(&lcTmpWCur..Style,'STYLE')
  =SEEK('S'+STYLE.Scale,'SCALE')
ENDIF
STORE PADL(ALLTRIM(scale.sz1),5) TO lcWSz1
STORE PADL(ALLTRIM(scale.sz2),5) TO lcWSz2
STORE PADL(ALLTRIM(scale.sz3),5) TO lcWSz3
STORE PADL(ALLTRIM(scale.sz4),5) TO lcWSz4
STORE PADL(ALLTRIM(scale.sz5),5) TO lcWSz5
STORE PADL(ALLTRIM(scale.sz6),5) TO lcWSz6
STORE PADL(ALLTRIM(scale.sz7),5) TO lcWSz7
STORE PADL(ALLTRIM(scale.sz8),5) TO lcWSz8

DIMENSION xbud1(8),xrec1(8),xdam1(8),xcan1(8),xint1(8)  && Totals by size.
STORE 0 TO xbud1,xrec1,xdam1,xcan1,xint1,xtot1

SELECT (lcTmpWip)
SEEK lcExKey
SCAN REST WHILE &LcExpWc FOR &lcExpFc AND &lcExpFr
  DO CASE
    *-- Ordered -------------------
	CASE trancd = '1'
      FOR I=1 TO 8
		z = STR(I,1)
		xbud1(I) = xbud1(I) + qty&z
      ENDFOR
    *-- Received ------------------    
	CASE trancd = '2' 
      FOR I=1 TO 8
		z = STR(I,1)
		xrec1(I) = xrec1(I) + qty&z
      ENDFOR
    *--Imported In-Transit --------
	CASE trancd $ '36' AND !laData[21]
      FOR I=1 TO 8
		z = STR(I,1)
		xint1(I) = xint1(I) + qty&z
      ENDFOR
    *-- Damaged ---------------------

	*B603256,1 KHM 11/01/1999 (Begin) Changing the case to handle the Dye & Adornment orders.
	*CASE (trancd ='4' AND !laData[21]) OR;
	     (trancd ='3' AND laData[21] )
	CASE (trancd ='4' AND !laData[21]) OR;
	     (trancd ='3' AND laData[21] AND &lcTmpWCur..cType = 'C') OR;
	     (TranCd = '4' AND laData[21] AND &lcTmpWCur..cType $ 'AD')
    *B603256,1 KHM 11/01/1999 (End)
    
      FOR I=1 TO 8
		z = STR(I,1)
		xdam1(I) = xdam1(I) + qty&z
      ENDFOR
    *-- Cancelled ---------------------

	*B603256,1 KHM 11/01/1999 (Begin) Changing the case to handle the Dye & Adornment orders.
	*CASE (trancd ='5' AND !laData[21]) OR;
	     (trancd ='4' AND laData[21] )
	CASE (trancd ='5' AND !laData[21]) OR;
	     (trancd ='4' AND laData[21] AND &lcTmpWCur..cType = 'C') OR;
	     (TranCd = '5' AND laData[21] AND &lcTmpWCur..cType $ 'AD')
    *B603256,1 KHM 11/01/1999 (End)
      FOR I=1 TO 8
		z = STR(I,1)
		xcan1(I) = xcan1(I) + qty&z
      ENDFOR

  ENDCASE
ENDSCAN

*B603256,1 KHM 11/01/1999 (Begin) Changing the IF command to handle the Dye and Adornment orders.
*IF IIF(laData[21],CUTTKTH.status<>'X',POSHDR.status<>'X')
IF IIF(laData[21],IIF(&lcTmpWCur..cType = 'C',CUTTKTH.status<>'X',POSHDR.status<>'X'),POSHDR.status<>'X')
*B603256,1 KHM 11/01/1999 (End)
  FOR I=1 TO 8
    xtot1=xtot1+ MAX((xbud1(I) - xrec1(I) - xdam1(I) - xcan1(I)),0)
  ENDFOR
ENDIF

IF laData[21]  
  =lfRefresh('ICWipQM')
ELSE
  =lfRefresh('ICWipQI')
ENDIF
RETURN


*!**********************************************************************
*! Name      : lfvInvtJl
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to browse style journal transactions.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvinvtjl
PRIVATE lcbrfields ,lnAlias,lcFile_Ttl

lnAlias=SELECT()
=gfOpenFile(gcDataDir+'STYINVJL','STYINVJL','SH')
IF ! lfcnrolex('STYINVJL')
  *--No inventory transactions was done for '+lcmesgx+'.
  =gfModalGen('TRM42042B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF
lcfile_ttl = 'Inventory Journal'
*E300803,1 SWK 05/25/98  Change the header of the browse to get the sizes desc.
*lcbrfields = "TrnType=lfTrnType() :25 :H='Transaction  '," +;
	IIF(llmultiwh AND llstymode,"cWareCode :10 :H='Warehouse',","")+;
	IIF(llDyelot,"cDyelot :12 :H='Dyelot',","")+;
	"dTrDate :H='Trn.Date'," +;
	"cTrCode :H='Tr.Num.',"+;
	"Adj1=IIF((cIRType='I' AND cTrType$'29'),-nStk1,nStk1) :P='999999',"+;
	"Adj2=IIF((cIRType='I' AND cTrType$'29'),-nStk2,nStk2) :P='999999',"+;
	"Adj3=IIF((cIRType='I' AND cTrType$'29'),-nStk3,nStk3) :P='999999',"+;
	"Adj4=IIF((cIRType='I' AND cTrType$'29'),-nStk4,nStk4) :P='999999',"+;
	"Adj5=IIF((cIRType='I' AND cTrType$'29'),-nStk5,nStk5) :P='999999',"+;
	"Adj6=IIF((cIRType='I' AND cTrType$'29'),-nStk6,nStk6) :P='999999',"+;
	"Adj7=IIF((cIRType='I' AND cTrType$'29'),-nStk7,nStk7) :P='999999',"+;
	"Adj8=IIF((cIRType='I' AND cTrType$'29'),-nStk8,nStk8) :P='999999',"+;
	"TAdj=IIF((cIRType='I' AND cTrType$'29'),-nTotStk,nTotStk) :H='Tot.Adj.':P='99999999',"+;
	"nCost   :H='Cost' :P='99999.99',"+;
	"Reference :H='Reference'"

  *E301228,1 Added a new fields LineNo,Prev. stk,Prev. stk value and dont display unneeded sizes.
  lcBrFields = "TrnType=lfTrnType() :25 :H='Transaction  ',"
  IF llMultiWh AND llStyMode
    lcBrFields = lcBrFields + "cWareCode :10 :H='Warehouse',"
  ENDIF
  IF llDyelot
    lcBrFields = lcBrFields + "cDyelot :12 :H='Dyelot',"
  ENDIF
  lcBrFields = lcBrFields + ;
               "dTrDate :H='Trn.Date',"+;
               "lcTrCd = IIF(EMPTY(cTrCode),cSession,cTrCode) :H='Tr.Num.',"
  FOR lnI=1 TO 8
    lcI=STR(lnI,1)
    IF !EMPTY(lcSz&lcI)
      *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
      *lcBrFields = lcBrFields + "nStk"+lcI+" :H=PADL(lcSz"+lcI+",5) :P='999999',"
      lcBrFields = lcBrFields + "nStk"+lcI+" :H=PADL(lcSz"+lcI+",5) :P='9999999',"
      *E301800,1 BWA 01/21/2002 (End)
    ENDIF
  ENDFOR
  IF !EMPTY(lcSz2)
    *E301800,1 BWA 01/21/2002 (Begin) Increase nStk1,nStk2....,nStk8 in STYINVJL file to be 7 instead of 6.
    *lcBrFields = lcBrFields + "nTotStk :H='Total ':P='99999999',"
    lcBrFields = lcBrFields + "nTotStk :H='Total ':P='999999999',"
    *E301800,1 BWA 01/21/2002 (End)
  ENDIF
  *B602963,1 Display any cost fields with costing access flag. 
  IF llCostPrv
    lcBrFields = lcBrFields + "nCost   :H='Cost'  :P='99999.99',"
  ENDIF
  lcBrFields = lcBrFields + "Reference :H='Reference',"
  IF llCostPrv
    lcBrFields = lcBrFields + "nStkVal  :H='Stk. Val.',"
  ENDIF
  lcBrFields = lcBrFields + "nPrvsQty :H='Prev. Stk',"
  IF llCostPrv
    lcBrFields = lcBrFields + "nPrvsVal :H='Prev. Stk Val.',"
  ENDIF
  lcBrFields = lcBrFields + "LineNo   :H='Line No.'"
  *B602963,1 End.
  *E301228,1 End.

*E300803,1 (End)
SELECT STYINVJL
*B603032,1 Start.
*lcOldFl    = SET("FILTER")
*lcCurKeyEx = KEY()
*SET FILTER TO &lcCurKeyEx. = lcExKey AND ((llStyMode OR cWareCode=lcWareCode) AND !(cTrType$'29' AND nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0))
*  =ARIABROW('',lcfile_ttl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
*	'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
IF ! llStyMode
  =ARIABROW([lcExKey FOR cWareCode=lcWareCode REST],lcfile_ttl,;
            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
            'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ELSE
  =ARIABROW([lcExKey REST],lcfile_ttl,;
            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
            'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ENDIF
*IF !EMPTY(lcOldFl)
*  SET FILTER TO &lcOldFl.
*ENDIF  
*B603032,1 End.
SELECT(lnAlias)
RETURN
*!**********************************************************************
*! Name      : lfTrnType
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function return the transaction title descreption.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lftrntype

PRIVATE lctrntyp
DO CASE
  *E301248,1 AHM Added to display a new type 'I' in journal brow as an ajustment.
  *CASE ctrtype = '1'
  CASE ctrtype $ '1I'
	lctrntyp  ='Adjustment        '
  *E301248,1 End.
  CASE ctrtype='2'
	lctrntyp  ='Physical Inventory'
  CASE ctrtype='3'
	lctrntyp  ='Invoice           '
  CASE ctrtype='4'
	lctrntyp  ='Void Invoice      '
  CASE ctrtype='5'
	lctrntyp  ='Receive C/T       '
  CASE ctrtype='6'
    IF laData[21]
      lctrntyp  =IIF(cIRType='I','Issue Adornment order','Receive Mfg. Order')
    ELSE
      lctrntyp = IIF(cIRType='I','Issue','Receive')+' P/O       '
    ENDIF
  CASE ctrtype='7'
	lctrntyp  ='Return Merchandise'
  CASE ctrtype='8'
	lctrntyp  ='Void Credit Memo  '
  CASE ctrtype='9'
	lctrntyp  ='Inventory Markdown'
ENDCASE
RETURN lctrntyp



*!*************************************+********************************
*! Name      : lfvAllo
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Browse the pick tikets.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvallo
PRIVATE lcBrFields,lcFile_Ttl
IF !llALInstld
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Allocation')
  RETURN
ENDIF

lnAlias=SELECT()

*E301077,7 open orders files.
IF !laOpFile[3]
  laOpFile[3] = gfOpenFile(gcDataDir+'OrdHdr' ,'OrdHdr'  ,'SH')
ENDIF
IF !laOpFile[4]
  laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
ENDIF
IF !laOpFile[21]
  laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF
IF !laOpFile[5]
  laOpFile[5] = gfOpenFile(gcDataDir+'Piktkt','Piktkt','SH')
ENDIF

SELECT ordline
SET RELATION TO cOrdType+Order INTO ORDHDR
SET RELATION TO PIKTKT INTO PIKTKT ADDI

IF ! lfcnrolex('PIKTKT')
  *--No picking tickets was done for '+lcmesgx+'.'
  =gfModalGen('TRM42043B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF
SET RELATION TO IIF(EMPTY(STORE),'M'+account,'S'+account+STORE) INTO customer ADDITIVE

SELECT ORDLINE
=AFIELDS(laFeilds)
CREATE CURSOR (lccursname) From ARRAY laFeilds
INDEX ON STYLE+STR(LINENO,6) TAG StyTmpTG

SELECT ORDLINE
*B602948,1 Change to see order warehouse insted of piktkt warehouse if alocated only.
SCAN WHILE STYLE=lcExKey FOR Picked .AND.;
 IIF(llStyMode,.T.,IIF(PIKTKT='*',cWareCode=lcWareCode,PIKTKT.cWareCode=lcWareCode))
  SCATTER TO MEMVAR
  SELECT (lccursname)   
  APPEND BLANK
  GATHER FROM MEMVAR
  REPLACE DESC1 WITH CUSTOMER.STNAME,Flag WITH 'Z'
ENDSCAN
SET RELATION TO

SELECT (lcCursName)
GO TOP
lcfile_ttl = 'Open Pick Tickets'
llPickTkt = .T.
*-- B802136,1 HDM[Start] Adjust Browse fields to include Dyelot Field Too
*lcbrfields = "PIKTKT   :H='P.Tkt#' :P='999999'," +;
		         "PIKDATE  :H='Pik.Date',"           +;
		         "ORDER    :H='Order'  :P='999999'," +;
		         "COMPLETE :H='Complete',"           +;
		         "ACCOUNT  :H='Acct#'  :P='99999',"  +;
		         "DESC1    :H='Name':35,"+;
		         "TOTPIK   :H='Pik.Qty':P='9999999'"
lcbrfields = "PIKTKT   :H='P.Tkt#' :P='999999',"     +;
		         "PIKDATE  :H='Pik.Date',"           +;
		         "DYELOT   :H='Dyelot',"             +;
		         "ORDER    :H='Order'  :P='999999'," +;
		         "COMPLETE :H='Complete',"           +;
		         "ACCOUNT  :H='Acct#'  :P='99999',"  +;
		         "DESC1    :H='Name':35,"            +;
		         "TOTPIK   :H='Pik.Qty':P='9999999'"

*-- B802136,1 HDM[End]

llInquiry=.F.
=ARIABROW([FOR Flag='Z'],lcfile_ttl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
IF llInquiry
  *B602948,1 Start.
  IF &lcCursName..PIKTKT='*'
    *--'The piktkt does not generated yet , unable to see piktkt.'
    =gfModalGen('TRM42185B42000','DIALOG')
  ELSE
  *B602948,1 End.
    lcPTInq="'"+&lccursname..PIKTKT+"'"
    *--HDM B802155,1 04/18/1999 Call Picking ticket program[Start]
    *DO gpDoProg WITH 'AWRICSTYPTK',.F.,'IC',lcPTInq
    DO gpDoProg WITH 'AWRALPKTKT',.F.,'AL',lcPTInq
    *--HDM B802155,1 04/18/1999 Call Picking ticket program[End]
  ENDIF
ENDIF

SELECT ordline
SET RELATION TO
SELECT(lnAlias)
RETURN



*!**********************************************************************
*! Name      : lfvOpen
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Browse the open order lines.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvopen
PRIVATE lcBrFields
IF !llSOInstld
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Sales Order')
  RETURN
ENDIF  
lnAlias=SELECT()

*E301077,7 open orders files.
IF !laOpFile[3]
  laOpFile[3] = gfOpenFile(gcDataDir+'OrdHdr' ,'OrdHdr'  ,'SH')
ENDIF
IF !laOpFile[4]
  laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
ENDIF
IF !laOpFile[21]
  laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF


SELECT ordline
SET RELATION TO cOrdType+Order INTO ordhdr

IF ! lfcnrolex('ORDLINE')
  *--No open order lines found for '+lcmesgx+'.'
  =gfModalGen('TRM42044B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF

SELECT ordline
SET RELATION TO IIF(EMPTY(STORE),'M'+account,'S'+account+STORE) INTO customer ADDI

*B603196,1 ABD 10/18/1999 Fixed the bug In Cut & Sold enquiry in Sytle screen 
*B603196,1                Sales order Browse shows the header completion date for each 
*B603196,1                order line instead of complete date per line 1 & we change 
*B603196,1                the ORDHDR.COMPLETE to ORDLINE.COMPLETE. [ begin]
*lcbrfields = "cOrdType :1  :H='T',"  +;
             "ORDER   :8  :H='Order',"  +;
		     "ORDHDR.STATUS :2 :H='St',"            +;
	         "ACCOUNT :7  :H='Account',"+;
		     "STORE   :10 :H='Store',"  +;
		     "CUSTOMER.STNAME  :35 :H='Name',"      +;
		     "ORDHDR.COMPLETE  :8  :H='Complete'  ,"+;
		     "TOTQTY  :6  :H='Ord.Qty' :P='999999',"+;
		     "PRICE   :8  :P='9999.99' :H='Price' ,"+;
		     "PIKTKT  :6  :H='Piktkt',"             +;
             "lcStat=ALLTRIM(CUSTOMER.cAddress4) :H='State',"+;
             "lcZip =SUBSTR(CUSTOMER.cAddress5,1,5) :H='Zip',"+;
             "lcTel =ORDHDR.Phone :H='Phone' :P=gfPhoneTem()"
*B603284,1 Remove the Type column from the SO Browse
*lcbrfields = "cOrdType :1  :H='T',"  +

*C101705,1 ARH 01/11/2000 (Begin) Added the start date to the browse
*lcbrfields = "ORDER   :8  :H='Order',"  +;
		     "ORDHDR.STATUS :2 :H='St',"            +;
	         "ACCOUNT :7  :H='Account',"+;
		     "STORE   :10 :H='Store',"  +;
		     "CUSTOMER.STNAME  :35 :H='Name',"      +;
		     "COMPLETE  :8  :H='Complete'  ,"+;
		     "TOTQTY  :6  :H='Ord.Qty' :P='999999',"+;
		     "PRICE   :8  :P='9999.99' :H='Price' ,"+;
		     "PIKTKT  :6  :H='Piktkt',"             +;
             "lcStat=ALLTRIM(CUSTOMER.cAddress4) :H='State',"+;
             "lcZip =SUBSTR(CUSTOMER.cAddress5,1,5) :H='Zip',"+;
             "lcTel =ORDHDR.Phone :H='Phone' :P=gfPhoneTem()"

lcbrfields = "ORDER   :8  :H='Order',"  +;
		     "ORDHDR.STATUS :2 :H='St',"            +;
	         "ACCOUNT :7  :H='Account',"+;
		     "STORE   :10 :H='Store',"  +;
		     "CUSTOMER.STNAME  :35 :H='Name',"      +;
		     "OrdHdr.Start :8  :H='Start'  ,"+;
		     "COMPLETE  :8  :H='Complete'  ,"+;
		     "TOTQTY  :6  :H='Ord.Qty' :P='999999',"+;
		     "PRICE   :8  :P='9999.99' :H='Price' ,"+;
		     "PIKTKT  :6  :H='Piktkt',"             +;
             "lcStat=ALLTRIM(CUSTOMER.cAddress4) :H='State',"+;
             "lcZip =SUBSTR(CUSTOMER.cAddress5,1,5) :H='Zip',"+;
             "lcTel =ORDHDR.Phone :H='Phone' :P=gfPhoneTem()"
*C101705,1 ARH 01/11/2000 (End)

*B603196,1 [ END ]
lcORbrowtl= 'Orders'
llPickTkt = .F. 
llInquiry = .F.
*B604864,1 HBG 09/03/2001 Browse for warehouse of the order line not the header [Begin]
*=ARIABROW([lcExKey FOR !(OrdHdr.Status $ 'CX' .OR. TotQty <= 0) .AND. ;
         	IIF(llstymode,.T.,OrdHdr.cwarecode=lcwarecode) rest],;
            	lcORbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
*B607310,1 KHM 07/08/2003 (Begin) Get the sales orders with cOrdType = 'O'.
*=ARIABROW([lcExKey FOR !(OrdHdr.Status $ 'CX' .OR. TotQty <= 0) .AND. ;
         	IIF(llstymode,.T.,ORDLINE.cwarecode=lcwarecode) rest],;
            	lcORbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')            	
=ARIABROW([lcExKey FOR !(OrdHdr.Status $ 'CX' .OR. TotQty <= 0) .AND. cOrdType = 'O' .AND. ;
         	IIF(llstymode,.T.,ORDLINE.cwarecode=lcwarecode) rest],;
            	lcORbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')            	
*B607310,1 KHM 07/08/2003 (End)

*B604864,1 [End]
SELECT ordline
SET RELATION TO

IF llInquiry
  lcOrdInq="'"+ORDLINE.cOrdType+"','"+ORDLINE.Order+"'"
  DO gpDoProg WITH 'AWRSOORD',.F.,'SO',lcOrdInq
ENDIF

SELECT(lnAlias)
RETURN



*!**********************************************************************
*! Name      : lfvOTS
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to show the open to sell.
*!             Same as Stock Availabilty report.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvots
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function cause.
*E500304,1 WAB- START
*B603711,1 (Begin) As gfOTSDisp is set to RETURN if (PO or MF or SO) not installed, we will add
*B603711,1         any one of them MANUALLY to gcCnpModules variable - if PS is installed- to 
*B603711,1         force the function to show OTS and remove it from the variable.
*=gfOTSDisp(lcMajor+lcNMKey,lcWareCode,llStyMode)
IF (OCCURS('PS',gcCmpModules)<>0)
  lcAddedMod = IIF(OCCURS('PO',gcCmpModules)=0,",PO",;
               IIF(OCCURS('SO',gcCmpModules)=0,",SO",;
               IIF(OCCURS('MF',gcCmpModules)=0,",MF","")))
  gcCmpModules = gcCmpModules + lcAddedMod
ENDIF  
=gfOTSDisp(lcMajor+lcNMKey,lcWareCode,llStyMode)
IF (OCCURS('PS',gcCmpModules)<>0) AND !EMPTY(lcAddedMod)
  gcCmpModules = STRTRAN(gcCmpModules,lcAddedMod,'')
ENDIF
*B603711,1 (End)
RETURN
*PRIVATE lcBrFields,lnAlias,lcFile_Ttl
*IF !llPOInstld AND !llMFInstld AND !llSOInstld
*  RETURN
*ENDIF
*lnAlias=SELECT()
*SELECT Style
**--Checking existance of OTC information.
*llotcinfo=.F.
*FOR I=1 TO 8
*  Z=STR(I,1)
*  IF Stk&z<>0 OR Wip&z<>0 OR Ord&z<>0
*	llotcinfo=.T.
*	EXIT
*  ENDIF
*ENDFOR
*IF ! llotcinfo
*  *--No Open to sell information found.
*  =gfModalGen('TRM42045B42001','DIALOG')
*  RETURN
*ENDIF
*E301077,7 open orders files.
*IF llSOInstld
*  IF !laOpFile[3]
*    laOpFile[3] = gfOpenFile(gcDataDir+'OrdHdr' ,'OrdHdr'  ,'SH')
*  ENDIF
*  IF !laOpFile[4]
*    laOpFile[4] = gfOpenFile(gcDataDir+'OrdLine','OrdLines','SH')
*  ENDIF
*ENDIF
**E301077,7 OPEN PO Files.
*IF llPOInstld 
*  IF !laOpFile[6]
*    laOpFile[6] = gfOpenFile(gcDataDir+'POShdr','POSHdr'  ,'SH')
*  ENDIF
*  IF !laOpFile[7]
*    laOpFile[7] = gfOpenFile(gcDataDir+'POSLN' ,'POSLNS'  ,'SH')
*  ENDIF
*  IF !laOpFile[8] 
*    laOpFile[8] = gfOpenFile(gcDataDir+'Shpmthdr','Shpmthdr','SH')
*  ENDIF
*ENDIF
*SELECT (lnAlias)
*--Save current pointers since this function change it.
*lnCrSav1=IIF(!EOF(),RECNO(),0)
*lnCrSav2=IIF(!EOF('STYLE'),RECNO('STYLE'),0)
*lnCrSav3=IIF(!EOF('STYDYE'),RECNO('STYDYE'),0)
*WAIT WINDOW 'Colecting Open to Sell information...' NOWAIT
*CREATE CURSOR (lctmpots) (Style C(19),SZCnt C(1) ,Size C(5) ,;
*                          nqty1 N(6),nqty2 N(6),nqty3 N(6),nqty4 N(6),;
*                          nqty5 N(6),nqty6 N(6),nqty7 N(6),nqty8 N(6),;
*                          nqty9 N(6),nqty10 N(6),nqty11 N(6),nqty12 N(6))
*SELECT IIF(llstymode,'STYLE','STYDYE')
*lcStyle  = lcMajor
*lcColor  = lcNMKey
*lcexkey  = lcstyle + lccolor
*lcwcondt = 'Style = lcExKey'
*lcfcondt = IIF(!llStyMode,'cWareCode = lcWareCode','.T.')
*=SEEK( lcexkey )
*SCAN WHILE &lcwcondt FOR &lcfcondt
*	IF ! llStyMode
*	  =SEEK(STYDYE.Style,'STYLE')
*	ENDIF
*	lcColor = SUBSTR(Style,lnStyleWid+1,lnColorWid+1)
*	=SEEK('S'+Style.Scale,'SCALE')
*	SELECT (lctmpots)
*	FOR I=1 TO scale.cnt
*		z=STR(I,1)
*		APPEND BLANK
*		REPLACE Style WITH lcStyle+lcColor,;
*                SZCnt WITH Z,;
*			    Size  WITH scale.sz&z
*	ENDFOR
*ENDSCAN
*SELECT (lctmpots)
*INDEX ON Style+SZCnt TAG (lctmpots) OF (lctmpots)
**-- Initialize the necessary global variables.
*ldToday = gdSysDate
*lcThisM = ALLTRIM(STR(MONTH(ldToday)))
*lcThisY = ALLTRIM(STR(YEAR(ldToday)))
*lcThisD = ALLTRIM(STR(DAY(ldToday)))
*--No of dayes in the month.
*DIME laNoOfDays[12]
*laNoOfDays = '31'
*STORE '30' TO laNoOfDays[4],laNoOfDays[6],laNoOfDays[9],laNoOfDays[11]
**--Build periods from -To dates up to 10 periods.
*DIME laDtPeriod[10,3]   
*laDtPeriod = {}
*lcPrdMonth = lcThisM
*lnPrdMonth = INT(VAL(lcThisM))
*lcPrdYear  = lcThisY
**--Read date format.
*llEngDate = (ALLTRIM(gcContCode) = lcEngland .OR. SET('DATE')='BRITISH' )
**-- HDM B602128,1 11/30/1998 [Start] 
**-- Start Calculating the OTS From The Current OTS Period
**-- Meaning if we R in the middle of a month and OTS period Weekly we will ignore
**-- the the weeks before the current week and calculate the prev. OTS as 'current'
*lnPrdNum = 10
*IF lcOtsPrd='E' AND VAL(lcThisD)<=15  
*  lnPrdNum = 11
*ENDIF
*IF lcOtsPrd='W' 
*  DO CASE
*    CASE BETWEEN(VAL(lcThisD),8,15)
*      lnPrdNum = 11
*    CASE BETWEEN(VAL(lcThisD),16,22)
*      lnPrdNum = 12
*    CASE BETWEEN(VAL(lcThisD),22,31)
*      lnPrdNum = 13
*  ENDCASE
*ENDIF
*DIME laDtPeriod[lnPrdNum,3]   
*laDtPeriod = {}
**-- HDM B602128,1 11/30/1998 [end]
*FOR I=1 TO lnPrdNum
*  lcPrdCnt = ALLTRIM(STR(I,2))
*  DO CASE
*    CASE lcOtsPrd='E'
*     IF lcPrdCnt $ '1-3-5-7-9-11'  &&First part of month.
*       IF llEngDate
*         laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)
*         laDtPeriod[I,2] = CTOD('15/'+lcPrdMonth+'/'+lcPrdYear)
*         laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 01-15'
*       ELSE
*         laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)  
*         laDtPeriod[I,2] = CTOD(lcPrdMonth+'/15/'+lcPrdYear)
*         laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 01-15'
*       ENDIF
*     ELSE
*       IF llEngDate
*         laDtPeriod[I,1] = CTOD('16/'+lcPrdMonth+'/'+lcPrdYear)  
*         laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*         laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)
*       ELSE
*         laDtPeriod[I,1] = CTOD(lcPrdMonth+'/16/'+lcPrdYear)
*         laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*         laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)
*       ENDIF
*       laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 16-'+ALLTRIM(laNoOfDays[lnPrdMonth])
*       ldNewDate  = (laDtPeriod[I,2]+1)
*       lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate)))
*       lnPrdMonth = INT(VAL(lcPrdMonth))
*       lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))
*     ENDIF
*    CASE lcOtsPrd='W'
*      DO CASE
*        CASE lcPrdCnt $ '1-5-9-13'  &&First week.
*         IF llEngDate
*           laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)  
*           laDtPeriod[I,2] = CTOD('07/'+lcPrdMonth+'/'+lcPrdYear)
*         ELSE
*           laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)  
*           laDtPeriod[I,2] = CTOD(lcPrdMonth+'/07/'+lcPrdYear)
*         ENDIF
*         laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 01-07'
*        CASE lcPrdCnt $ '2-6-10'
*          IF llEngDate
*            laDtPeriod[I,1] = CTOD('08/'+lcPrdMonth+'/'+lcPrdYear)  
*            laDtPeriod[I,2] = CTOD('15/'+lcPrdMonth+'/'+lcPrdYear)
*          ELSE
*            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/08/'+lcPrdYear)  
*            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/15/'+lcPrdYear)
*          ENDIF
*          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 08-15'
*        CASE lcPrdCnt $ '3-7-11'
*          IF llEngDate
*            laDtPeriod[I,1] = CTOD('16/'+lcPrdMonth+'/'+lcPrdYear)  
*            laDtPeriod[I,2] = CTOD('22/'+lcPrdMonth+'/'+lcPrdYear)
*          ELSE
*            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/16/'+lcPrdYear)  
*            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/22/'+lcPrdYear)
*          ENDIF
*          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 16-22'
*        CASE lcPrdCnt $ '4-8-12'
*          IF llEngDate
*            laDtPeriod[I,1] = CTOD('23/'+lcPrdMonth+'/'+lcPrdYear)            
*            laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*            laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)
*          ELSE
*            laDtPeriod[I,1] = CTOD(lcPrdMonth+'/23/'+lcPrdYear)  
*            laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*            laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)
*          ENDIF 
*          laDtPeriod[I,3] = SUBSTR(CMONTH(laDtPeriod[I,1]),1,3)+'. 23-'+ALLTRIM(laNoOfDays[lnPrdMonth])
*          ldNewDate  = (laDtPeriod[I,2]+1)
*          lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate)))
*          lnPrdMonth = INT(VAL(lcPrdMonth))
*          lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))
*      ENDCASE
*    CASE lcOtsPrd='M'
*      IF llEngDate
*        laDtPeriod[I,1] = CTOD('01/'+lcPrdMonth+'/'+lcPrdYear)  
*        laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*        laDtPeriod[I,2] = CTOD(ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdMonth+'/'+lcPrdYear)
*      ELSE
*        laDtPeriod[I,1] = CTOD(lcPrdMonth+'/01/'+lcPrdYear)  
*        laNoOfDays[2]   = IIF(MOD(YEAR(laDtPeriod[I,1]),4)=0 ,'29' ,'28' )
*        laDtPeriod[I,2] = CTOD(lcPrdMonth+'/'+ALLTRIM(laNoOfDays[lnPrdMonth])+'/'+lcPrdYear)
*      ENDIF
*      laDtPeriod[I,3] = PADR(CMONTH(laDtPeriod[I,1]),10)
*      ldNewDate  = (laDtPeriod[I,2]+1)
*      lcPrdMonth = ALLTRIM(STR(MONTH(ldNewDate)))
*      lnPrdMonth = INT(VAL(lcPrdMonth))
*      lcPrdYear  = ALLTRIM(STR(YEAR(ldNewDate)))
*  ENDCASE
* *--Browse fields hedding.
*ENDFOR
**-- HDM B602128,1 11/30/1998 [Start]
*=ACOPY(laDtPeriod,laDtTmpPrd)
*LNREMVCOL=0   
*FOR I=1 TO ALEN(laDtTmpPrd,1)
*  IF gdSysDate>laDtTmpPrd[I,1] AND gdSysDate>laDtTmpPrd[I,2]
*    *--REMOVE
*    LNREMVCOL=I   
*    LOOP
*  ENDIF
*ENDFOR
*=ACOPY(LADTTMPPRD,LADTPERIOD,(LNREMVCOL*3)+1)
*FOR I=1 TO ALEN(laDtPeriod,1)
*  lcPrdCnt = ALLTRIM(STR(I,2))
*  IF I=1
*    laDtPeriod[I,1] = {01/01/01}
*    laDtPeriod[I,3] = 'Current'
*  ENDIF
*  *--Browse fields hedding.
*  lcMn&lcPrdCnt = laDtPeriod[I,3]
*ENDFOR
**-- HDM B602128,1 11/30/1998 [end] 
** --- Filling the main array.
*SELECT STYLE
*=SEEK( lcexkey )
*lnSize_cnt = IIF(SEEK('S'+Style.Scale,'SCALE'), Scale.cnt , 1)
*DIMENSION laOTCInfo[lnSize_cnt,12]
*laOTCInfo = 00
*SELECT IIF(llstymode,'STYLE','STYDYE')
*=SEEK( lcexkey )
*SCAN WHILE &lcwcondt FOR &lcfcondt
*  lcPStyle  = Style
*  laOTCInfo = 0.00
*  FOR  lnSz_no = 1 TO lnSize_cnt
*    lcSz_no = STR(lnSz_no,1) 
*	laOTCInfo[lnSz_no,01] = STK&lcSz_no
*	laOTCInfo[lnSz_no,12] = ORD&lcSz_no + SHP&lcSz_no
*  ENDFOR
*  *--According to "MAKE" field call procedure.
*  IF llMFInstld AND laData[21]
*    DO lpOTSmYes WITH lcPStyle
*  ENDIF
*  IF llPOInstld AND !laData[21]
*    DO lpOTSmNo  WITH lcPStyle
*  ENDIF
*  *-- To subtract the proper quantities in the "ORDLINE" file .
*  IF llSOInstld
*    DO lpOrdQSub WITH lcPStyle
*  ENDIF
*  *--To post the negative values to the nearst positive value.
*  DO lppstnvvlu
*  *--Replace the OTS Quantities.
*  DO lpotslins WITH lcPStyle
*ENDSCAN
*IF lnCrSav2<>0
*  GOTO lnCrSav2 IN STYLE
*ENDIF
*IF lnCrSav3<>0
*  GOTO lnCrSav3 IN STYDYE
*ENDIF
*SELECT(lnAlias)
*IF lnCrSav1<>0
*  GOTO lnCrSav1
*ENDIF
*SELECT (lcTmpOTS)
*SUM nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12 TO ;
*    lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12
*INSERT INTO (lcTmpOTS) (STYLE,SZCnt,nQty1,nQty2,nQty3,nQty4,nQty5,nQty6,nQty7,nQty8,nQty9,nQty10,nQty11,nQty12) ;
*	 VALUES (REPL(CHR(255),LEN(ALLTRIM(lcMajor))),'W',lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9,lnQty10,lnQty11,lnQty12)
*GOTO TOP
*lcBrFields = IIF(llAllColors,"lcSty=SUBSTR(Style,lnStyleWid+2,lnColorWid) :H=lcNMjrTl,","")+;
*             "Size ," +;
*         	 "nQty1  :H=lcMn1  :P='999999',"+;
*        	 "nQty2  :H=lcMn2  :P='999999',"+;
*         	 "nQty3  :H=lcMn3  :P='999999',"+;
*         	 "nQty4  :H=lcMn4  :P='999999',"+;
*        	 "nQty5  :H=lcMn5  :P='999999',"+;
*        	 "nQty6  :H=lcMn6  :P='999999',"+;
*        	 "nQty7  :H=lcMn7  :P='999999',"+;
*        	 "nQty8  :H=lcMn8  :P='999999',"+;
*        	 "nQty9  :H=lcMn9  :P='999999',"+;
*        	 "nQty10 :H='FUTURE' :P='999999',"+;
*        	 "nQty11 :H='TOTAL AVAIL' :P='999999',"+;
*        	 "nQty12 :H='TOT. SLD YTD' :P='999999'"
*WAIT CLEAR
*=ARIABROW('','Style Open To Sell for multiple periods',gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'',;
*         	'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
*SELECT(lnAlias)
*RETURN
*E500304,1 WAB- END
*:******************************************************************
*! Name      : lpOTSmno
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Procedure to compute the OTS quantites for the imported
*!        styles (MAKE : NO)
*! -- Remember that there is a relation between
*! -- "POSLN" -> "POSHDR"  to check on the "COMPLETE" and the "AVAILABLE "
*! -- dates in case of the transaction code 1,2,4,5
*! -- "POSLN" -> "SHPMTHDR" to check on the "ETA" date in case of
*! -- the transaction code 3.
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lpotsmno
*PARA lcOtsKey
*SELECT POSLN
*SEEK lcOtsKey
*SCAN WHILE Style = lcOtsKey ;
*     FOR cStyType<>'C' AND (llStymode OR cWareCode=lcWareCode) AND TranCd<>'6'
*  =SEEK(POSLN.cStyType+POSLN.PO,'POSHDR')
*  IF !(POSHDR.Status $ 'OH')
*    LOOP
*  ENDIF  
*  lcTrSign = IIF(Trancd='1',1,-1) * IIF(cStyType $ 'PN',1,-1)
*  ldTranDate = IIF (EMPTY(poshdr.available),poshdr.complete ,poshdr.available)
*  *-- To accumulate the quantity in process.
*  *B603144,1 Add a new parameter [Start]
*  *DO acum_ots WITH 'POSLN' , ldTranDate , lcTrSign
*  DO acum_ots WITH 'POSLN' , ldTranDate , lcTrSign , .T.
*  *B603144,1 Add a new parameter [End..]
*  IF Trancd ='3'
*    =SEEK(POSLN.ShipNo,'shpmthdr')
*    *B603144,1 Add a new parameter [Start]
*	*DO acum_ots WITH 'POSLN' , shpmthdr.eta , 1 
*	DO acum_ots WITH 'POSLN' , shpmthdr.eta , 1 , .T.	
*	*B603144,1 Add a new parameter [End..]
*  ENDIF
*ENDSCAN
*RETURN
*E500304,1 WAB- END

*:******************************************************************
*! PROG : lpOTSmYes
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : Procedure to compute the OTS quantites for the
*!        manufactured styles (MAKE : YES)
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lpotsmyes
*PARA lcOtsKey
*SELECT CUTTKTL
*SEEK lcOtsKey
*SCAN WHILE Style = lcOtsKey FOR (llStymode OR cWareCode=lcWareCode)
*  =SEEK(Cuttkt,'CUTTKTH')
*  IF CUTTKTH.Status $ 'OAH'
*    *--To accumulate the quantity in process.
*    *B603144,1 Add a new parameter [Start]
*    *DO acum_ots WITH 'CUTTKTL' , CUTTKTH.complete , IIF(Trancd='1',1,-1)
*    DO acum_ots WITH 'CUTTKTL' , CUTTKTH.complete , IIF(Trancd='1',1,-1) , .T.
*    *B603144,1 Add a new parameter [End..]
*  ENDIF
*ENDSCAN
*RETURN
*E500304,1 WAB- END

*:******************************************************************
*! PROG : lpOrdQSub
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : Procedure to substract the order line quantities from the
*!        OTS quantities .
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lpordqsub
*PARA lcOtsKey
*SELECT ORDLINE
*SEEK lcOtsKey
*SCAN WHILE Style = lcOtsKey FOR cOrdType='O'
*  =SEEK('O'+ORDLINE.Order,'ORDHDR')
*  IF ORDHDR.Status$'OH' AND (llStymode OR ORDHDR.cWareCode=lcWareCode)
*	* -- To accumulate the quantity in process .
*	*- Use the start ship date instead of the completion date.
*	*B603144,1 Add a new parameter [Start]
*	*DO acum_ots WITH 'ORDLINE' , start , -1 
*	DO acum_ots WITH 'ORDLINE' , start , -1 , .F.
*	*B603144,1 Add a new parameter [End..]
*  ENDIF
*ENDSCAN
*RETURN
*E500304,1 WAB- END

*:******************************************************************
*! PROG : ACUM_OTS
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : Procedure to accumulate the OTS quantities .
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  :
*!        1) lcMFile  -> To hold the file name in process .
*!        2) ldTrDate -> To hold the date in process .
*!        3) lnSnType -> To deside whether to add or subtract the
*!                          quantity in process to or from the balance .
*!
*!Due to B603144,1 Add a new parameter to this function which will indicate                 
*!                 if this funtion was called to accumalats O.T.S Qty
*!                 or Subtract the order Qty .
*!        4) llAcumOTS  
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE Acum_ots
**B603144,1 Add a new parameter and a new private variable [Start]
**PARAMETERS lcMFile , ldTrDate , lnSnType
*PARAMETERS lcMFile , ldTrDate , lnSnType , llAcumOTS
*PRIVATE lnX
*lnX=IIF(llAcumOTS,2,1)
**-- The columns of the array laOTCInfo are initialized
**-- By the stock Qty and this function is going to accumalats the O.T.S QTY
**-- Or Subtract the order Qty form each column , But now we need to exclude
**-- The first column of this array (i.e laOTCInfo[1,x]) form this process
**-- Because this column carry the current O.T.S Qty .    
**B603144,1 Add a new parameter and a new private variable [End..]
*SELECT (lcMFile)
*IF BETWEEN(ldTrDate,laDtPeriod[1,1],laDtPeriod[10,2])
*  FOR  lnSz_no = 1 TO lnSize_cnt
*    lcSz_no = STR(lnSz_no,1) 
*    * -- Search for the right period to process .
*    *B603144,1 If this function will Accumalat O.T.S start from second column [Start]
*    *FOR I=1 TO 10
*    FOR I=lnX TO 10
*   	*B603144,1 If this function will Accumalat O.T.S start from second column [Start]
*   	  IF BETWEEN(ldTrDate,laDtPeriod[I,1],laDtPeriod[I,2])
*        laOTCInfo[lnSz_no,I]=laOTCInfo[lnSz_no,I] + ( Qty&lcSz_no * lnSnType )
*		EXIT
*	  ENDIF
*	ENDFOR
*  ENDFOR
*ELSE
*  IF ldTrDate > laDtPeriod[10,2]
*    FOR lnSz_no = 1 TO lnSize_cnt
*      lcSz_no = STR(lnSz_no,1) 
*      laOTCInfo[lnSz_no,10]=laOTCInfo[lnSz_no,10] + ( Qty&lcSz_no * lnSnType )
*	ENDFOR
*  ELSE
*    *B603144,1 This block of code will never be accessible any more
*    *          Because       laDtPeriod[I,1] = {01/01/01} 
*    FOR lnSz_no = 1 TO lnSize_cnt
*      lcSz_no = STR(lnSz_no,1) 
*      laOTCInfo[lnSz_no,1]=laOTCInfo[lnSz_no,1] + ( Qty&lcSz_no * lnSnType )
*	ENDFOR
*  ENDIF
*ENDIF
*RETURN
*E500304,1 WAB- END

*:******************************************************************
*! PROG : lpPstNvVlu
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : Procedure to post the negative values in any period to the
*!        nearst positive value of any next period.
*!        - We do this by looping the two dimensional array "laOTCInfo"
*!          in case of the report format is by size , otherwise we will
*!          loop just the last row of the same array (which holds the
*!          total OTS quantities for this color) ,to do the following :
*!          - If there is a negative value , we will make it 0 and subtract
*!            it from the nearst next period has a POSITIVE value.
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lppstnvvlu
*xon_hand = 0
*FOR lnSz_no = 1 TO lnSize_cnt
*  FOR I=1 TO 10
*	IF laOTCInfo[lnSz_no,I] < 0    && -VE NUMBER
*  	  xon_hand = laOTCInfo [lnSz_no,I]
*	  laOTCInfo[lnSz_no,I] = 0
*	  IF I = 1
*		DO lppostbf WITH 2,10                  && GO_BACKWORD   ->
*	  ELSE
*		DO lppostbf WITH I-1,1                 && GO_BACKWORD   <-
*		IF (xon_hand < 0) AND (I < 10)
*	  	  DO lppostbf WITH I+1,10              && GO_FOREWORD   ->
*		ENDIF
* 	  ENDIF
*	ENDIF   &&-> -VE NUMBER
*  ENDFOR
*  *--To put the remainning -ve quantity in the last period.
*  IF xon_hand < 0
*	laOTCInfo[lnSz_no,10] = xon_hand
*  ENDIF
*  xon_hand = 0
*ENDFOR
*RETURN
*E500304,1 WAB- END
*:******************************************************************
*! PROG : lpPostBf
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : To do the posting either backword or forword.
*! NOTE : Called from lpPstNvVlu
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnIniPer,lnEndPer->Initial and end periods.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lppostbf
*PARAMETERS lnIniPer ,lnEndPer
*lcFloop=IIF(lnIniPer>lnEndPer,'lnIniPer TO lnEndPer STEP -1',;
*                              'lnIniPer TO lnEndPer')
*FOR I = &lcFloop
*  xon_hand =  xon_hand + laOTCInfo[lnSz_no,I]
*  IF xon_hand <= 0
*	laOTCInfo[lnSz_no,I] = 0
*  ELSE
*	laOTCInfo[lnSz_no,I] = xon_hand
*	xon_hand = 0
*	EXIT
*  ENDIF
*ENDFOR
*RETURN
*E500304,1 WAB- END

*:******************************************************************
*! PROG : lpOTSLins
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! DESC : Replace the OTS Quantities.
*:******************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcOtsKey->OTS key like style as ex.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : Do ..
*!*************************************************************
*E500304,1 WAB -Call The O.T.S global function and comment all lines 
*E500304,1 WAB- of the O.T.S local function and all calling procedure.
*E500304,1 WAB- START
*PROCEDURE lpotslins
*PARA lcOtsKey
**-- Loop to accumulate the the OTS lines and total to the 11th col.
**-- in the array .
*FOR lnSz_no = 1 TO lnSize_cnt
*  FOR I = 1 TO 10
*	laOTCInfo[lnSz_no,11] = laOTCInfo[lnSz_no,11]+laOTCInfo[lnSz_no,I]
*  ENDFOR
*ENDFOR
**--Start Update.
**--OTS line for each [color\size].
*FOR lnSz_no = 1 TO lnSize_cnt
*  lcSz_no = STR(lnSz_no,1) 
*  SELECT (lctmpots)
*  =SEEK(lcOtsKey+lcSz_no)
*  FOR I = 1 TO 12
*	Z=ALLTRIM(STR(I,2))
*	REPLACE nQty&Z WITH laOTCInfo[lnSz_no,I]
*  ENDFOR
*ENDFOR
*RETURN
*E500304,1 WAB- END

*!**********************************************************************
*! Name      : lfvShipped
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Browse the invoicesd and shipped qty.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvshipped

PRIVATE lcBrFields

*B603711,1 (Begin) If PS is installed let him inquire. 
*IF !llARInstld
IF (OCCURS('PS',gcCmpModules)=0) AND !llARInstld
*B603711,1 (End)
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Accounts Receivable')
  RETURN
ENDIF

lnAlias=SELECT()

*E301077,7 open AR files.
IF !laOpFile[9]
  laOpFile[9] = gfOpenFile(gcDataDir+'InvHdr'  ,'InvHdr'  ,'SH')
ENDIF
IF !laOpFile[10]
  laOpFile[10]= gfOpenFile(gcDataDir+'InvLine' ,'InvLines','SH')
ENDIF
IF !laOpFile[21]
  laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF


SELECT invline
SET RELATION TO invoice INTO invhdr

IF ! lfcnrolex('INVLINE')
  *--No invoices was done for '+lcmesgx+'.'
  =gfModalGen('TRM42046B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF

SELECT invline
SET RELATION TO IIF(EMPTY(STORE) ,'M'+account,'S'+account+STORE ) INTO customer ADDI

lcIVbrowtl = 'Invoices'
lcbrfields = "INVOICE :8  :H='Invoice'," +;
             "INVHDR.Status  :8  :H='Status'," +;
             "INVDATE :8  :H='Inv.Date',"+;
             "ACCOUNT :7  :H='Acct#',"   +;
   	         "STORE   :10 :H='Store',"   +;
   		     "NAME = SUBSTR(CUSTOMER.STNAME,1,22) :H='Name',"+;
		     "ORDER   :8           :H='Order',"+;
		     "TOTQTY  :H='Tot Qty' :P='999999', "+;
		     "PRICE   :H='Price'   :P='99999.99',"+;
             "lcSRp   = INVHDR.Rep1  :5 :H='Sales Rep.',"+;
             "lcTerms = INVHDR.cTermCode :8 :H='Term',"+;
             "lcTel =CUSTOMER.Phone1 :H='Phone' :P=gfPhoneTem() ,"+;
             "lcSt=ALLTRIM(CUSTOMER.cAddress42) :H='State',"+;
             "lcZp=ALLTRIM(CUSTOMER.cAddress52) :H='Zip'"

llInquiry=.F.
=ARIABROW([lcExKey FOR IIF(llStyMode,.T.,InvHdr.cWareCode=lcWareCode)],;
	lcIVbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
IF llInquiry
  lcInv="'"+INVLINE.Invoice+"'"
  DO gpDoProg WITH 'AWRARDINV',.F.,'AR',lcInv
ENDIF

SELECT invline
SET RELATION TO
SELECT(lnAlias)
RETURN




*!**********************************************************************
*! Name      : lfvRA
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Browse the R/a's.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvRA
PRIVATE lcBrFields

IF !llRMInstld
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Return Merchandise')
  RETURN
ENDIF

lnAlias=SELECT()
*E301077,7 OPEN RA Files.
IF ! laOpFile[13]
  laOpFile[13]= gfOpenFile(gcDataDir+'RetAuth' ,'RetAuth' ,'SH')
ENDIF
IF ! laOpFile[14]
  laOpFile[14]= gfOpenFile(gcDataDir+'RALine'  ,'RALines' ,'SH')
ENDIF
IF !laOpFile[21]
  laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF


SELECT Raline
SET RELATION TO 'M'+account INTO customer,rano INTO retauth

IF ! lfcnrolex('RALINE')
  *--No return authorization was done for '+lcmesgx+'.
  =gfModalGen('TRM42047B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF

lcRTbrowtl = 'Return Authorization'
lcbrfields = "Rano :8         :H='R/A #',"+;
       	  	 "RETAUTH.RADATE  :8  :H='Date',"+;
             "ACCOUNT         :7  :H='Account',"+;
             "CUSTOMER.STNAME :18 :H='Name',"+;
           	 "lcRReson=gfCodDes(reason,'REASON') :H='Reason':R:14,"+;
             "TOTQTY :H='Tot Qty' :P= '999999',"+;
             "PRICE  :H='Price'   :P= '99999.99',"+;
             "AMOUNT :H='Amount'  :P= '9999999.99'"

llInquiry=.F.
*B607006,1 KHM 07/16/2003 (Begin) Add the checking of RA status.
*=ARIABROW([lcExKey FOR IIF(llStyMode,.T.,RetAuth.cWareCode=lcWareCode)],;
	lcRTbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
=ARIABROW([lcExKey FOR RetAuth.Status <> "X" AND IIF(llStyMode,.T.,RetAuth.cWareCode=lcWareCode)],;
	lcRTbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
*B607006,1 KHM 07/16/2003 (End)

IF llInquiry
  lcRaNo= "'"+RALINE.RANO+"'"
  DO gpDoProg WITH 'AWRRMRTATH',.F.,'RM',lcRaNo
ENDIF

SELECT raline
SET RELATION TO

SELECT(lnAlias)
RETURN


*!**********************************************************************
*! Name      : lfvReturns
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Browse the returns.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvReturns
PRIVATE lcBrFields

*B603711,1 (Begin) If PS is installed let him inquire.
*IF !llRMInstld
IF (OCCURS('PS',gcCmpModules)=0) AND !llRMInstld 
*B603711,1 (End)
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Return Merchandise')
  RETURN
ENDIF

lnAlias=SELECT()
*E301077,7 Open returns files.
IF ! laOpFile[11]
  laOpFile[11]= gfOpenFile(gcDataDir+'RETHdr'  ,'RETHdr'  ,'SH')
ENDIF
IF ! laOpFile[12]
  laOpFile[12]= gfOpenFile(gcDataDir+'RETLine' ,'RETLines','SH')
ENDIF
IF !laOpFile[21]
  laOpFile[21] = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF


SELECT retline
SET RELATION TO 'M'+account INTO customer,crmemo INTO rethdr

llPickTkt = .F. 
IF ! lfcnrolex('RETLINE')
  *--No returns was done for '+lcmesgx+'.
  =gfModalGen('TRM42048B42001','DIALOG',lcmesgx)
  SELECT(lnAlias)
  RETURN
ENDIF

lcREbrowtl = 'Returns'
*B802807,1 (Begin) Add 'Status' field to the returns browse to show the status of returned credit memo
*lcbrfields = "CRMEMO          :8  :H='Memo#',"+;
		"RETHDR.CRDATE   :8  :H='Date',"+;
		"ACCOUNT         :7  :H='Account',"+;
		"CUSTOMER.STNAME :18 :H='Name',"+;
     	"lcRReson=gfCodDes(reason,'REASON') :H='Reason':R:14,"+;
		"TOTQTY :H='Tot Qty' :P= '999999',"+;
		"PRICE  :H='Price'   :P= '99999.99',"+;
		"AMOUNT :H='Amount'  :P= '9999999.99'"
lcbrfields = "CRMEMO          :8  :H='Memo#',"+;
        "RETHDR.STATUS        :8  :H='Status',"+;
		"RETHDR.CRDATE   :8  :H='Date',"+;
		"ACCOUNT         :7  :H='Account',"+;
		"CUSTOMER.STNAME :18 :H='Name',"+;
     	"lcRReson=gfCodDes(reason,'REASON') :H='Reason':R:14,"+;
		"TOTQTY :H='Tot Qty' :P= '999999',"+;
		"PRICE  :H='Price'   :P= '99999.99',"+;
		"AMOUNT :H='Amount'  :P= '9999999.99'"
*B802807,1 (End)
llInquiry=.F.
*B802558,1 ALB(Start) 08/26/1999 Fixed Bug of Browsing both Received & Canceled quantites when
*B802558,1                       Inquiring for Returned quantites (Received)
*=ARIABROW([lcExKey FOR IIF(llStyMode,.T.,RetHdr.cWareCode=lcWareCode)],;
	lcREbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')

=ARIABROW([lcExKey FOR cRet_Trncd='2' AND IIF(llStyMode,.T.,RetHdr.cWareCode=lcWareCode)],;
	lcREbrowtl,gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2, 'ICSTYQTY')
*B802558,1 ALB(End) 
IF llInquiry
  lcCrMemo= "'"+RETLINE.CRMEMO+"'"
  DO gpDoProg WITH 'AWRRMCRMEM',.F.,'RM',lcCrMemo
ENDIF

SELECT retline
SET RELATION TO

SELECT(lnAlias)
RETURN



*!**********************************************************************
*! Name      : ICSTYQTY
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function of browses select button.
*!**********************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION ICSTYQTY

lcWkAls=ALIAS()
DO CASE
  CASE lcWkAls='ORDLINE' AND !llPickTkt
    lcWindTitl = 'Order #: ' + Order
    lcPrgTtl ='Order'

  CASE llPickTkt
    IF !SEEK(piktkt,'PIKTKT')
     	*--No picking ticket line found for this order line.'
      =gfModalGen('TRM42049B42001','DIALOG')
     	RETURN
    ENDIF
    llDispTick = .F.
    lcWindTitl = 'Picking ticket # : ' + piktkt
    lcPrgTtl='Pick ticket'

  CASE lcWkAls='INVLINE'
    lcWindTitl = 'Invoice #: ' + Invoice
    lcPrgTtl ='Invoice'

  CASE lcWkAls='RALINE'
    lcWindTitl = 'R/A # : ' + RaNo
    lcPrgTtl='Return Auth.'

  CASE lcWkAls='RETLINE'
    lcWindTitl = 'Credit Memo # : ' + CrMemo
    lcPrgTtl='Credit Memo'
    *B603711,2 AMH 08/20/2000 do not display the Credit Memo button in 
    *B603711,2                case of RM not Installed [Start]
    llExCalled = IIF(OCCURS('RM',gcCmpModules)=0,.T.,.F.)  
    *B603711,2 AMH 08/20/2000 [End  ]

ENDCASE

lnStyRec = IIF(!EOF('STYLE'),RECNO('STYLE'),0)
=SEEK(Style,'STYLE')
=SEEK('S'+STYLE.Scale,'SCALE')
STORE PADL(ALLTRIM(scale.sz1),5) TO lcWSz1
STORE PADL(ALLTRIM(scale.sz2),5) TO lcWSz2
STORE PADL(ALLTRIM(scale.sz3),5) TO lcWSz3
STORE PADL(ALLTRIM(scale.sz4),5) TO lcWSz4
STORE PADL(ALLTRIM(scale.sz5),5) TO lcWSz5
STORE PADL(ALLTRIM(scale.sz6),5) TO lcWSz6
STORE PADL(ALLTRIM(scale.sz7),5) TO lcWSz7
STORE PADL(ALLTRIM(scale.sz8),5) TO lcWSz8

llInquiry=.F.
DO (gcScrDir+gcWinAppl+"\ICSTYQTY.SPX")
IF llInquiry
  llBrowTmnat = .T.
ENDIF

IF lnStyRec<>0
  GOTO lnStyRec IN STYLE
ENDIF
RETURN








*:*************************************************************

*##3#-#-# Thered folder functions. ( S P E C I A L )

*:*************************************************************
*!*************************************************************
*! Name      : lfvLinkCod
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate gl link code field.
*!*************************************************************
*! Calls     : gfGLBrowse 6chr
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvlinkcod

lnAlias=SELECT()
IF llbrowse OR !EMPTY(laData[19])
  lcGllnk6   = laData[19]
  lcLinkDesc = SPACE(30)
  =gfGLbrowse('03',@lcGllnk6,@lcLinkDesc,0)
  IF !EMPTY(lcGllnk6)
    laData[19] = lcGllnk6
  ELSE
    lcLinkDesc = SPACE(30)  
  ENDIF
ENDIF
laData[19]  =IIF(EMPTY(laData[19]),'DEFDEF',laData[19])
lcLinkDesc = IIF(EMPTY(laData[19]),'',lcLinkDesc)

*B605958,1 ASH 06/02/2002 (Begin) Replace the gl_link code with the right data.
SELECT (lcwarefil)
IF !EOF()
  REPLACE gl_link WITH laData[19] 
ENDIF
*B605958,1 ASH 06/02/2002 (End)
SHOW GET laData[19]
SHOW GET lcLinkDesc
llbrowse = .F.
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfvLink3Cod
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate sales gl link field.
*!*************************************************************
*! Calls     : gfGLBrowse 3chr
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvlink3cod

lnAlias=SELECT()
IF llbrowse OR !EMPTY(laData[68])
	lcSlinkcde = SPACE(30)
    lcGllnk3 = laData[68]
	=gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
	IF !EMPTY(lcGllnk3)
	laData[68] = lcGllnk3
	ELSE
  	lcSlinkcde = SPACE(30)	
	ENDIF
ENDIF
laData[68]  =IIF(EMPTY(laData[68]),'DEF',laData[68])
lcSlinkcde = IIF(EMPTY(laData[68]),'',lcSlinkcde)
SHOW GET laData[68]
SHOW GET lcSlinkcde
llbrowse = .F.
SELECT(lnalias)
RETURN


*!*************************************************************
*! Name      : lfvVen
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate Supplier  code.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvVen

lnAlias=SELECT()
lcVenFile=gcDataDir+'APVendor.dbf'
IF FILE(lcVenFile)
  =gfOpenFile(gcDataDir+'APVENDOR','Vencode','SH')
  IF llBrowse OR (!EMPTY(laData[66]) AND !SEEK(laData[66],'APVendor'))
    lcSupplier=laData[66]
    *-- E301268,1 HDM [Start] filter vendor browse by sup. type
    *=gfApVnBrow(@lcSupplier)
    =gfApVnBrow(@lcSupplier,.F.,'S')
    *-- E301268,1 HDM [End]
    laData[66]=lcSupplier
  ENDIF
  SELECT STYLE
ENDIF
llbrowse = .F.
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfvRetSty
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid function to validate return style.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnDqlty -> Quality no.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvRetSty
PARA lnDqlty

IF !llBrowse AND !EMPTY(SUBSTR(lcRetSty,1,1)) AND;
                 lcRetSty=IIF(EMPTY(lcOldValue),SPACE(19),lcOldValue)
  RETURN
ENDIF  
lnAlias=SELECT()
SELECT STYLE
lnSavRc=IIF(EOF(),0,RECNO())
SET ORDER TO TAG StyQlty
lcGrdKey=laDamQlt[lnDqlty,2]
IF llBrowse OR (!EMPTY(lcRetSty) AND !SEEK(lcGrdKey+lcRetSty,'STYLE'))
  SET FILTER TO cStyGrade=lcGrdKey
  GO TOP
  IF EOF()
    *--No styles found for this quality. 
    =gfModalGen('TRM42087B42000','DIALOG')
    lcRetSty = SPACE(19)
  ELSE
    lcRetSty = gfStyBrw('I','','',.F.)  
    SELECT (lcColorFil)
    =lfActClrBr()
  ENDIF 
  SELECT STYLE
  SET FILTER TO 
ENDIF
SET ORDER TO TAG Style

laData[IIF(lnDqlty=1,70,73)]=lcRetSty
llbrowse = .F.
IF !EMPTY(lcRetSty) AND STYLE.Scale<>laData[6]
  *--The selected style has a different size scale.
  =gfModalGen('TRM42089B42000','DIALOG')
ENDIF
IF lnSavRc<>0
  GOTO lnSavRc
ENDIF
SELECT(lnalias)
RETURN






*:*************************************************************

*##4#-#-# Forth folder functions. ( C O L O R S )

*:*************************************************************
*!*************************************************************
*! Name      : lfActClrBr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to activate browse window.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfactclrbr

lnAlias = SELECT()
SELECT (lccolorfil)
lnBrRecNo = RECNO()
IF !llallcolors
  =SEEK(lcMajor+lcSepart+lcNonMjr)
  lnbrrecno = RECNO()
  GO TOP
ENDIF
*--HDM E301023,1[Start]
*lcBrD1fld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ':W=.F.,"+;
*            "lcNMjrDisc = gfCodDes(lcNonMjr,'COLOR'):H='HH2', "+;
*            "lcSegm=SUBSTR(Style,lnStyleWid+2,lnColorWid) :15 :R :H='Code',"+;
*          	"Desc  :H='Description':R:25,"+;
*         	"Desc1 :H='Long Description':R:40,"+;
*            "PriceA :R :H='Price A',"+;
*           "PriceB :R :H='Price B',"+;
*            "PriceC :R :H='Price C',"+;
*            "nSugRetPri :R :H='Retail Price',"+;
*            "MarkA  :R :H='MarkUp A',"+;
*            "MarkB  :R :H='MarkUp B',"+;
*            "MarkC  :R :H='MarkUp C',"+;
*            "nMarkRet :R :H='Ret.MarkUp' "

*--HDM B602700,1 03/25/1999[Start] Change NonMajor browse structure in case of ESS
IF llMScale
  lcBrD1fld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ',"+;
              "lcSegm = SUBSTR(Style,lnStyleWid+2,lnColorWid-3) :10 :R :H=SUBSTR(lcNMjrTl,1,lnColorWid-4) ,"+;
              "lcScale= SUBSTR(Style,lnStyleWid+2+lnColorWid-3) :5 :R :H=SUBSTR(ALLTRIM(lcNMjrTl),lnColorWid-3)  ,"+;
              "lcNMjrDisc2 = gfCodDes(SUBSTR(Style,lnStyleWid+2,lnColorWid-3),'COLOR'):H = ALLTRIM(lcNMjrTl) + ' Short Description' :W=.F., "+;
              "lcClrLName  = lfGetRlt() :H= ALLTRIM(lcNMjrTl) + ' Long Description',"+;
              "Desc  :H='Description':R:25,"+;
              "Desc1 :H='Long Description':R:40,"+;
              "PriceA :R :H='Price A',"+;
              "PriceB :R :H='Price B',"+;
              "PriceC :R :H='Price C',ORD1   :R ,ORD2   :R ,ORD3   :R ,ORD4   :R ,ORD5   :R ,"

ELSE
  lcBrD1fld = "cMarker=IIF(lnBrRecNo=RECNO(),'>',' '):1:H=' ',"+;
              "lcSegm = SUBSTR(Style,lnStyleWid+2,lnColorWid) :15 :R :H=lcNMjrTl ,"+;
              "lcNMjrDisc2 = gfCodDes(SUBSTR(Style,lnStyleWid+2,lnColorWid),'COLOR'):H = ALLTRIM(lcNMjrTl) + ' Short Description' :W=.F., "+;
              "lcClrLName  = lfGetRlt() :H= ALLTRIM(lcNMjrTl) + ' Long Description',"+;
              "Desc  :H='Description':R:25,"+;
              "Desc1 :H='Long Description':R:40,"+;
              "PriceA :R :H='Price A',"+;
              "PriceB :R :H='Price B',"+;
              "PriceC :R :H='Price C',ORD1   :R ,ORD2   :R ,ORD3   :R ,ORD4   :R ,ORD5   :R ,"

ENDIF
*--HDM B602700,1 03/25/1999[End] 
lcBrD1fld=lcBrD1fld + "ORD6   :R ,"+;
            "ORD7   :R ,"+;
            "ORD8   :R ,"+;
            "WIP1   :R ,"+;
            "WIP2   :R ,"+;
            "WIP3   :R ,"+;
            "WIP4   :R ,"+;
            "WIP5   :R ,"+;
            "WIP6   :R ,"+;
            "WIP7   :R ,"+;
            "WIP8   :R ,"+;
            "TOTWIP :R ,"+;
            "STK1   :R ,"+;
            "STK2   :R ,"+;
            "STK3   :R ,"+;
            "STK4   :R ,"+;
            "STK5   :R ,"+;
            "STK6   :R ,"+;
            "STK7   :R ,"+;
            "STK8   :R ,"+;
            "TOTSTK :R ,"+;
            "ALO1   :R ,"+;
            "ALO2   :R ,"+;
            "ALO3   :R ,"+;
            "ALO4   :R ,"+;
            "ALO5   :R ,"+;
            "ALO6   :R ,"+;
            "ALO7   :R ,"+;
            "ALO8   :R ,"+;
            "TOTALO :R ,"+;
            "SHP1   :R ,"+;
            "SHP2   :R ,"+;
            "SHP3   :R ,"+;
            "SHP4   :R ,"+;
            "SHP5   :R ,"+;
            "SHP6   :R ,"+;
            "SHP7   :R ,"+;
            "SHP8   :R ,"+;
            "TOTSHP :R ,"+;
            "RET1   :R ,"+;
            "RET2   :R ,"+;
            "RET3   :R ,"+;
            "RET4   :R ,"+;
            "RET5   :R ,"+;
            "RET6   :R ,"+;
            "RET7   :R ,"+;
            "RET8   :R ,"+;
            "TOTRET :R ,"+;
            "RA1    :R ,"+;
            "RA2    :R ,"+;
            "RA3    :R ,"+;
            "RA4    :R ,"+;
            "RA5    :R ,"+;
            "RA6    :R ,"+;
            "RA7    :R ,"+;
            "RA8    :R ,"+;
            "TOTRA  :R ,"
lcBrD1fld = lcBrD1fld +"INTRANS1   :R ,"+;
                       "INTRANS2   :R ,"+;
                       "INTRANS3   :R ,"+;
                       "INTRANS4   :R ,"+;
                       "INTRANS5   :R ,"+;
                       "INTRANS6   :R ,"+;
                       "INTRANS7   :R ,"+;
                       "INTRANS8   :R ,"+;
                       "TOTINTRN   :R ,"+;
                       "NWO1       :R ,"+;
                       "NWO2       :R ,"+;
                       "NWO3       :R ,"+;
                       "NWO4       :R ,"+;
                       "NWO5       :R ,"+;
                       "NWO6       :R ,"+;
                       "NWO7       :R ,"+;
                       "NWO8       :R ,"+;
                       "NTOTWO     :R ,"+;
            "nSugRetPri :R :H='Retail Price'"
IF llCostPrv
  lcBrD1fld = lcBrD1fld + ",MarkA  :R :H='MarkUp A',"+;
                          " MarkB  :R :H='MarkUp B',"+;
                          " MarkC  :R :H='MarkUp C',"+;
                          " nMarkRet :R :H='Ret.MarkUp' "
ENDIF
*--HDM E301023,1[End]
*--HDM B602700,1 03/25/1999[Start] Change NonMajor browse structure in case of ESS
IF llMScale
  *E301049,1 (Begin) Freeze the color short description filed also.
  *BROWSE FIELDS &lcBrD1fld ;
         NOAPPEND ;
	     NOCLEAR  ;
  	     NODELETE ;
         NOMENU   ;
         NOWAIT   ;
         SAVE     ;
         PARTITION 23;
         TITLE lcBrowcTtl ;
         WINDOW (lcWinC41) IN WINDOW (lcWinCh4);
         WHEN lfwBrow() ;
         VALID :F lfvBrows()
  BROWSE FIELDS &lcBrD1fld ;
         NOAPPEND ;
	     NOCLEAR  ;
  	     NODELETE ;
         NOMENU   ;
         NOWAIT   ;
         SAVE     ;
         PARTITION 53;
         TITLE lcBrowcTtl ;
         WINDOW (lcWinC41) IN WINDOW (lcWinCh4);
         WHEN lfwBrow() ;
         VALID :F lfvBrows()
  *E301049,1 (End)         

ELSE
  *E301049,1 (Begin) Freeze the color short description filed also.
  *BROWSE FIELDS &lcBrD1fld ;
         NOAPPEND ;
	     NOCLEAR  ;
  	     NODELETE ;
         NOMENU   ;
         NOWAIT   ;
         SAVE     ;
         PARTITION 20;
         TITLE lcBrowcTtl ;
         WINDOW (lcWinC41) IN WINDOW (lcWinCh4);
         WHEN lfwBrow() ;
         VALID :F lfvBrows()
  BROWSE FIELDS &lcBrD1fld ;
         NOAPPEND ;
	     NOCLEAR  ;
  	     NODELETE ;
         NOMENU   ;
         NOWAIT   ;
         SAVE     ;
         PARTITION  50;         
         TITLE lcBrowcTtl ;
         WINDOW (lcWinC41) IN WINDOW (lcWinCh4);
         WHEN lfwBrow() ;
         VALID :F lfvBrows()
 *E301049,1 (End)
ENDIF
*--HDM B602700,1 03/25/1999[End]
*-Check if there is any added color in the color file.
IF laScrMode[3] AND ! llCUpdate
  SELECT (lcColorFil) 
  lnSvRc=IIF(EOF(),0,RECNO())
  LOCATE FOR cstatus <> 'S'
  llCUpdate = FOUND()
  IF lnSvRc<>0
    GOTO lnSvRc
  ENDIF
ENDIF
SELECT(lnalias)

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : When Browse function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfwBrow
glFromBrow = .T.
lnbrrecno = RECNO()
SHOW WINDOW (lcbrowcttl) REFRESH


*!*************************************************************
*! Name      : lfvBrows
*! Purpose   : Valid Browse function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() <> ALLTRIM(lcBrowcTtl)
  = gfStopBrow() 
ELSE
  glFromBrow = .T.
ENDIF  


*!*************************************************************
*! Name      : lfvNewClr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Add/Remove non majors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvNewClr

IF llMScale AND EMPTY(laData[6])
  *--You have to edit Scale first.
  =gfModalGen('TRM42079B42001','DIALOG')
  RETURN
ENDIF

DO (gcAppHome+gcWinAppl+"\ICNMjr.FXP")

SELECT (lccolorfil)
GO TOP
IF !EOF()
  SHOW GET pbEdtPrc ENABLE
  IF !llDetCost AND !laData[21]
    SHOW GET pbEdtCost ENABLE
  ENDIF
  *--HDM B801907,1[START] Update Description Fields
  laData[4] = &lccolorfil..Desc1
  SHOW GET laData[4]
  laData[3] = &lccolorfil..Desc
  SHOW GET laData[3]
  
  *--HDM B801907,1[END]
ENDIF
SHOW WINDOW (lcbrowcttl) REFRESH
_CUROBJ = OBJNUM(pbnewclr)
RETURN


*!*************************************************************
*! Name      : lfvEdtPrc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Edit prices for Non Majors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvEdtPrc

*C200155,1 ABD Add new triger to add priceA and retail price to sizes in case 
*C200155,1 ABD company is extended size scale for specific customer.
IF llMScale .AND. ASCAN(laEvntTrig , PADR('LLUPDAT_PR',10)) <> 0
  = gfDoTriger('ICSTYLE',PADR('LLUPDAT_PR',10))
  RETURN
ENDIF
*C200155,1 ABD [End]


*--Save the main style prices.
lnNMPrA=laData[24]
lnNMPrB=laData[25]
lnNMPrC=laData[26]
lnNMPrR=laData[60]
lnNMMkA=laData[54]
lnNMMkB=laData[55]
lnNMMkC=laData[56]
lnNMMkR=laData[81]
SELECT (lccolorfil)
laData[24]=PriceA
laData[25]=PriceB
laData[26]=PriceC
laData[60]=nSugRetPri
laData[54]=MarkA
laData[55]=MarkB
laData[56]=MarkC
laData[81]=nMarkRet
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ICNMjrB.SPX")
ON KEY
POP KEY
SELECT (lccolorfil)
*--Restore the style prices.
laData[24]=lnNMPrA
laData[25]=lnNMPrB
laData[26]=lnNMPrC
laData[60]=lnNMPrR
laData[54]=lnNMMkA
laData[55]=lnNMMkB
laData[56]=lnNMMkC
laData[81]=lnNMMkR
SHOW WINDOW (lcbrowcttl) REFRESH
_CUROBJ = OBJNUM(pbEdtPrc)
RETURN


*!*************************************************************
*! Name      : lfUpdPrc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Update prices for Non Majors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfUpdPrc

SELECT (lccolorfil)
REPLACE PriceA WITH laData[24],;
        PriceB WITH laData[25],;
        PriceC WITH laData[26],;
        nSugRetPri WITH laData[60],;
        MarkA  WITH laData[54],;
        MarkB  WITH laData[55],;
        MarkC  WITH laData[56],;
        nMarkRet WITH laData[81]
RETURN


*!*************************************************************
*! Name      : lfValdClr
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid color in mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lnOption-> option no. in mover.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfValdClr
PARAMETERS lnOption
DO CASE
  CASE lnOption=3
  IF ASCAN('laOldTrAr',ALLTRIM(laTarget[lsTarget])) > 0
	*--You cannot remove this non major because it has not been entered in this session, Use delete option instead.
    =gfModalGen('TRM42050B42001','DIALOG')
    RETURN .F.
  ENDIF
  CASE lnOption=4 AND laScrMode[3]
	*--You cannot remove the non major that has not been entered in this session, Use delete option instead.'
    =gfModalGen('TRM42051B42001','DIALOG')
    RETURN .F.
ENDCASE  









*:*************************************************************

*##5#-#-# Fifth folder functions. ( C O S T I N G )

*:*************************************************************
*!*************************************************************
*! Name      : lfvCurrncy
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Validate the currency code. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCurrency->Currency code.
*!                      lcCurMth  ->Currency method.
*!                      llGetCRt  ->Ability browse.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvCurrncy
PARA lcCurrency,lcCurMth,llGetCRt
PRIVATE llOldMsgSt

STORE '' TO llOldMsgSt
lnAlias=SELECT()
*E301077,7 open currency file.
*--HDM B602447,1 Check if not opened[START]
*IF laOpFile[2]
IF !laOpFile[2]
  *--HDM B602447,1 Check if not opened[END]
  laOpFile[2] = gfOpenFile(gcSysHome+'SycCurr' ,'cCurrCode','SH')
ENDIF
IF !laOpFile[17]
  laOpFile[17] = gfOpenFile(gcSysHome+'SycExch','Currency','SH')
ENDIF

IF !llGetCRt
  IF !EMPTY(lcCurrency) AND lcCurrency=lcOldValue
    RETURN
  ENDIF  
  =gfcurrbrow(@lcCurrency)
ENDIF
*IF llDispMsg .AND. SYS(18)='PBEDTCOST'
*  llOldMsgSt = llDispMsg
*  llDispMsg = .F.
*ENDIF

lcCurrency=IIF(!EMPTY(lcCurrency),lcCurrency,gcBaseCurr)
IF lcCurMth=1
  llFound    = SEEK(lcCurrency,'SycCurr')
  lcFrnSmbl1 = IIF(llFound,SycCurr.cCurrSmbl,'')
  IF lcCurrency<>gcBaseCurr  
    =gfChkRate('lnCurrUnt1',lcCurrency,gdSysDate,.f.,gcAct_comp,.f.)
    lcPMethod = gfGetExSin(@lcUnPMeth,lcCurrency)
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcUnPMeth = IIF(EMPTY(lcUnPMeth),'/',lcUnPMeth)
    llFound    = SEEK(gcBaseCurr+lcCurrency,'SycExch')
    lnPrExRate = IIF(llFound,sycexch.nExRate,0)
    ldRateDt1  = IIF(llFound,sycexch.dRateDate,gdSysDate)
  ELSE
    lnPrExRate = 1
  ENDIF
  lnCurrUnt1 = IIF(lnCurrUnt1=0,1,lnCurrUnt1)
  laData[27] = lcCurrency
  =lfvEquCost('1',laData[45],lnPrExRate,lnCurrUnt1,.T.)

ELSE
  llFound = SEEK(lcCurrency,'SycCurr')
  STORE IIF(llFound,SycCurr.cCurrSmbl,lcBaseSmbl) TO lcFrnSmbl2,lcFrnSmbl3,lcFrnSmbl4,lcFrnSmbl5
  IF lcCurrency<>gcBaseCurr  
    =gfChkRate('lnCurrUnt2',lcCurrency,gdSysDate,.f.,gcAct_comp,.f.)
    lcDMethod  = gfGetExSin(@lcUnDMeth,lcCurrency)
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcUnDMeth = IIF(EMPTY(lcUnDMeth),'/',lcUnDMeth)
    llFound    = SEEK(gcBaseCurr+lcCurrency,'SycExch')
    lnChExRate = IIF(llFound,sycexch.nExRate,0)
    ldRateDt2  = IIF(llFound,sycexch.dRateDate,gdSysDate)
  ELSE
    lnChExRate = 1
  ENDIF
  lnCurrUnt2 = IIF(lnCurrUnt2=0,1,lnCurrUnt2)
  laData[28] = lcCurrency
  =lfvEquCost('2',laData[46],lnChExRate,lnCurrUnt2,.T.)
  =lfvEquCost('3',laData[47],lnChExRate,lnCurrUnt2,.T.)
  =lfvEquCost('4',laData[48],lnChExRate,lnCurrUnt2,.T.)
  =lfvEquCost('5',laData[49],lnChExRate,lnCurrUnt2,.T.)    

ENDIF

DO CASE
  CASE lnPrExRate=0
    lcRateTxt1='No valid rate on '+DTOC(ldRateDt1)
  CASE lnPrExRate=1
    lcRateTxt1='Rate'
  CASE lnPrExRate<>1 OR lnPrExRate<>0
    lcRateTxt1='Rate on '+DTOC(ldRateDt1)
  OTHE
    lcRateTxt1=''
ENDCASE
DO CASE
  CASE lnChExRate=0
    lcRateTxt2='No valid rate on '+DTOC(ldRateDt2)
  CASE lnChExRate=1
    lcRateTxt2='Rate'
  CASE lnChExRate<>1 OR lnChExRate<>0
    lcRateTxt2='Rate on '+DTOC(ldRateDt2)
  OTHE
    lcRateTxt2=''
ENDCASE


IF !llGetCRt
  =lfvPrice('A',.T.)
  =lfvPrice('B',.T.)
  =lfvPrice('C',.T.)  
  =lfvPrice('R',.T.)  
ENDIF

SELECT(lnalias)
SHOW GET laData[27]
SHOW GET laData[28]  
SHOW GET lnPrExRate
SHOW GET lnChExRate
IF !laData[21] AND llMulCurr AND !llImpCost
  IF laData[27] = IIF(lcIType2 $ 'MD',laData[28],gcBaseCurr)   
    SHOW GET lnFCostPr2 ENABLE
  ELSE
    lnFCostPr2=0
    SHOW GET lnFCostPr2 DISABLE
  ENDIF
  IF laData[27] = IIF(lcIType3 $ 'MD',laData[28],gcBaseCurr)   
    SHOW GET lnFCostPr3 ENABLE
  ELSE
    lnFCostPr3=0
    SHOW GET lnFCostPr3 DISABLE
  ENDIF
  IF laData[27] = IIF(lcIType4 $ 'MD',laData[28],gcBaseCurr)   
    SHOW GET lnFCostPr4 ENABLE
  ELSE
    lnFCostPr4=0
    SHOW GET lnFCostPr4 DISABLE
  ENDIF
  IF laData[27] = IIF(lcIType5 $ 'MD',laData[28],gcBaseCurr)   
    SHOW GET lnFCostPr5 ENABLE
  ELSE
    lnFCostPr5=0
    SHOW GET lnFCostPr5 DISABLE
  ENDIF
ENDIF
IF !llLineCost
  =lfRefresh(lcWinCh5)
ENDIF

*IF TYPE(llOldMsgSt) = 'L'
*  llDispMsg = llOldMsgSt
*ENDIF

RETURN

*!*************************************************************
*! Name      : lfvEquCost
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Get eguevalent cost by pathing the foreign.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCstNo  -> Cost no.
*!                      lnFrnCost-> Forign cost.
*!                      lnCurRate-> Rate.
*!                      lnCurUnt -> Units.
*!                      llCalcMrk-> Calculate Mrkup.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt,llCalcMrk

lnAlias=SELECT()
lcCstType = lcIType&lcCstNo
IF lcCstType $ 'PMD' AND ( lnCurRate<>1 OR lnCurUnt<>1 )
  lcSign1   = IIF(lcCstType='P',lcPMethod,lcDMethod)
  lcSign2   = IIF(lcCstType='P',lcUnPMeth,lcUnDMeth)
  lnEquCost = lnFrnCost &lcSign1 lnCurRate &lcSign2 lnCurUnt
ELSE
  lnEquCost = lnFrnCost
ENDIF

*B606719,4 ASH 02/05/2003 (Begin) Round the cost elements.
lnEquCost = ROUND(lnEquCost,2)
*B606719,4 ABD 02/05/2003 (End)
lnCostVal&lcCstNo = lnEquCost
IF llDispMsg .AND.  lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. gcContCode = lcEgypt AND ;
                  laData[45] <> 0 .AND. !laScrMode[2] .AND. EVAL(SYS(18)) > 0 AND SYS(18) <> UPPER('pbEdtCost') AND;
                  (lnCostVal&lcCstNo > 0 OR lnCostPrc&lcCstNo > 0)
  IF gfModalGen("QRM42151B42012","DIALOG",lcCostLbl&lcCstNo) = 2
    llOnGross = .F.
  ELSE
    llOnGross = .T.
  ENDIF
ENDIF
IF lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. llOnGross
  IF lnCostVal0 = 0
    =lfvGPrice('F')
  ENDIF
  lnCostPrc&lcCstNo = IIF(lncostval0=0,0,(lnEquCost/lnCostVal0)*100)
ELSE
  lnCostPrc&lcCstNo = IIF(lncostval1=0,0,(lnEquCost/lnCostVal1)*100)
ENDIF
SHOW GET lnCostVal&lcCstNo
SHOW GET lnCostPrc&lcCstNo

IF lcCstNo<> '1' AND laData[45]<>0 AND ;
  laData[27] = IIF(lcIType&lcCstNo $ 'MD',laData[28],gcBaseCurr)   
  lnCstNo = VAL(lcCstNo)
  IF lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. llOnGross
    lnFCostPr&lcCstNo = (laData[44+lnCstNo] *100 / laData[83])
  ELSE
    lnFCostPr&lcCstNo = (laData[44+lnCstNo] *100 / laData[45])
  ENDIF
  SHOW GET lnFCostPr&lcCstNo
ENDIF

IF lcCstNo='1'
  FOR I = 2 TO 5
    lcXno = STR(I,1)
    IF laData[45] = 0
      lnFCostPr&lcXno = 0 
    ELSE
      IF laData[27] = IIF(lcIType&lcXno $ 'MD',laData[28],gcBaseCurr)
        IF lcIType&lcXno $ 'D'
        
          IF STYLE.NPRCOST&lcXno = STYLE.NICOST&lcXno *100 / STYLE.GROS_PRICE
             lnFCostPr&lcXno = (laData[44+I] *100 / laData[83])
             llOnGross = .T.
             SHOW GET lnFCostPr&lcXno 
             LOOP
          ENDIF
        ENDIF
        lnFCostPr&lcXno = (laData[44+I] *100 / laData[45] )
      ENDIF
    ENDIF 
    SHOW GET lnFCostPr&lcXno 
  ENDFOR
ENDIF
*B606719,4 ASH 02/05/2003 (Begin) Round the average cost field.
*laData[57]=lnCostVal1+lnCostVal2+lnCostVal3+lnCostVal4+lnCostVal5
laData[57]=ROUND(lnCostVal1+lnCostVal2+lnCostVal3+lnCostVal4+lnCostVal5,2)
*B606719,4 ASH 02/05/2003 (End)
IF STYLE.TotStk = 0 AND STYLE.TotRet = 0
  laData[2] = laData[57]
ENDIF
*--Save the main style prices.
lnNMPrA=laData[24]
lnNMPrB=laData[25]
lnNMPrC=laData[26]
lnNMPrR=laData[60]
lnNMMkA=laData[54]
lnNMMkB=laData[55]
lnNMMkC=laData[56]
lnNMMkR=laData[81]

IF laScrMode[4] AND !EOF(lcColorFil)
  STORE 0 TO laData[54],laData[55],laData[56]

  SELECT (lcColorFil)  
  lnRcNo=RECNO()
  SCAN
    laData[24]=PriceA
    laData[25]=PriceB
    laData[26]=PriceC
    laData[60]=nSugRetPri
    laData[54]=MarkA
    laData[55]=MarkB
    laData[56]=MarkC
    laData[81]=nMarkRet

    =lfvPrice('A',.T.)
    =lfvPrice('B',.T.)
    =lfvPrice('C',.T.)  
    =lfvPrice('R',.T.)  
    
    SELECT (lcColorFil)  
    REPLACE MarkA WITH laData[54],;
            MarkB WITH laData[55],;
            MarkC WITH laData[56],;
            nMarkRet WITH laData[81]

  ENDSCAN
  GOTO lnRcNo
ENDIF
*--Restore the style prices.
laData[24]=lnNMPrA
laData[25]=lnNMPrB
laData[26]=lnNMPrC
laData[60]=lnNMPrR
laData[54]=lnNMMkA
laData[55]=lnNMMkB
laData[56]=lnNMMkC
laData[81]=lnNMMkR

IF !llCalcMrk
  =lfvPrice('A',.T.)
  =lfvPrice('B',.T.)
  =lfvPrice('C',.T.)  
  =lfvPrice('R',.T.)  

ENDIF

*--HDM We Have to Replace in the temp. file[START]
lnPrevTbl = SELECT(0)
SELECT(lcColorFil)
IF laScrMode[3] or laScrMode[4]
  IF !llAllColors && Case Selected Color REPLACE JUST FOR IT

    REPLACE nICost1    WITH IIF(!llMulcurr,lncostval1,laData[45]),;
            nICost2    WITH IIF(!llMulcurr,lncostval2,laData[46]),;
            nICost3    WITH IIF(!llMulcurr,lncostval3,laData[47]),;
            nICost4    WITH IIF(!llMulcurr,lncostval4,laData[48]),;
            nICost5    WITH IIF(!llMulcurr,lncostval5,laData[49]),;
            Gros_Price WITH IIF(!llMulcurr,lncostval0,laData[83]),;
            Disc_Pcnt  WITH IIF(!llMulcurr,lnCostPrc0,laData[84])

    REPLACE  NPRCOST2   WITH IIF(llMulCurr,lnFCostPr2,lnCostprc2);
             NPRCOST3   WITH IIF(llMulCurr,lnFCostPr3,lnCostprc3);
             NPRCOST4   WITH IIF(llMulCurr,lnFCostPr4,lnCostprc4);
             NPRCOST5   WITH IIF(llMulCurr,lnFCostPr5,lnCostprc5)
  ELSE

    IF llAllColors .AND. !llPreColor && Case all colors selected and not per color update all colors

      REPLACE ALL nICost1 WITH IIF(!llMulcurr,lncostval1,laData[45]),;
              nICost2    WITH IIF(!llMulcurr,lncostval2,laData[46]),;
              nICost3    WITH IIF(!llMulcurr,lncostval3,laData[47]),;
              nICost4    WITH IIF(!llMulcurr,lncostval4,laData[48]),;
              nICost5    WITH IIF(!llMulcurr,lncostval5,laData[49]),;
              Gros_Price WITH IIF(!llMulcurr,lncostval0,laData[83]),;
              Disc_Pcnt  WITH IIF(!llMulcurr,lnCostPrc0,laData[84])

     REPLACE ALL NPRCOST2   WITH IIF(llMulCurr,lnFCostPr2,lnCostprc2);
                 NPRCOST3   WITH IIF(llMulCurr,lnFCostPr3,lnCostprc3);
                 NPRCOST4   WITH IIF(llMulCurr,lnFCostPr4,lnCostprc4);
                 NPRCOST5   WITH IIF(llMulCurr,lnFCostPr5,lnCostprc5)
    ENDIF
  ENDIF
ENDIF
SELECT (lnPrevTbl)
*--HDM We Have to Replace in the temp. file[END]

SELECT(lnalias)
IF !llLineCost
  =lfRefresh(lcWinCh5)
ENDIF
RETURN


*!*************************************************************
*! Name      : lfvCost
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid costs. Used only in case of not imported
*!           : and not multi currency imported style.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCstNo->Cost no. as character.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvCost
PARAMETERS lcCstNo

lnAlias=SELECT()
*E300803,1 SWK 05/25/98 Add a check that the cost cannot be less than zero
IF lnCostVal&lcCstNo < 0
  lnCostVal&lcCstNo = lcOldValue 
  =gfModalGen('TRM42000B42001','DIALOG')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*E300803,1 (End)

IF lcCstNo='1'
  FOR I = 2 TO 5
    lcXno = STR(I,1)
    IF lcIType&lcXno $ 'D'
      IF STYLE.NPRCOST&lcXno = STYLE.NICOST&lcXno *100 / STYLE.GROS_PRICE
        lnCostVal&lcXno = (lnCostPrc&lcXno * lncostval0 )/100
      ELSE
        lnCostVal&lcXno = (lnCostPrc&lcXno * lncostval1 )/100
      ENDIF
    ELSE
      lnCostVal&lcXno = (lnCostPrc&lcXno * lncostval1 )/100
    ENDIF
    SHOW GET lnCostVal&lcXno
  ENDFOR

ELSE
  IF lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. gcContCode = lcEgypt .AND.;
     lncostval1 > 0 .AND. lncostval0 > 0 .AND. EVAL(SYS(18)) > 0 && .AND. llDispMsg
     
    IF gfModalGen("QRM42151B42012","DIALOG",lcCostLbl&lcCstNo) = 2
      lnCostPrc&lcCstNo = IIF(lncostval1=0,0,(lnCostVal&lcCstNo/lnCostVal1)*100)
    ELSE
      lnCostPrc&lcCstNo = IIF(lncostval0=0,0,(lnCostVal&lcCstNo/lnCostVal0)*100)
    ENDIF
  ELSE
    lnCostPrc&lcCstNo = IIF(lncostval1=0,0,(lnCostVal&lcCstNo/lnCostVal1)*100)
  ENDIF
ENDIF
SHOW GET lnCostVal&lcCstNo
SHOW GET lnCostPrc&lcCstNo

laData[45]=lnCostVal1
laData[46]=lnCostVal2
laData[47]=lnCostVal3
laData[48]=lnCostVal4
laData[49]=lnCostVal5

*E300803,1 SWK 05/25/98 Calculate the laData array in case of not usind detail costing
IF !llDetCost 
  laData[40]=lnCostVal1
  laData[41]=lnCostVal2
  laData[42]=lnCostVal3
  laData[43]=lnCostVal4
  laData[44]=lnCostVal5
ENDIF
*E300803,1 (End)
*--Update the TotCost and AveCost.
*--HDM B602812,1 04/18/1999 [Start] Fixing bug of incorrect totCost Saving in Style File
laData[57] = lncostval1 + lncostval2 + lncostval3 + lncostval4 + lncostval5
*--HDM B602812,1 04/18/1999 [End]

IF laScrMode[4] OR (laScrMode[3] AND STYLE.Totstk = 0 AND STYLE.Totret = 0)
  laData[57] = lncostval1 + lncostval2 + lncostval3 + lncostval4 + lncostval5
*  IF llAvgCost    && New Instructions From OR ==>HDM
    laData[2]  = laData[57]
*  ENDIF           && New Instructions From OR ==>HDM
  IF laData[57] <> 0
    IF laData[24]<>0
      *B603713,1 AMH [Start] let ladata[54] bounded by max and min value of its field
      *laData[54] = 100 * (ROUND(laData[24],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[24],2)) 
      laData[54] = MAX(MIN(100 * (ROUND(laData[24],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[24],2)),999999),-99999)
      *B603713,1 AMH [END]
    ENDIF
    IF laData[25]<>0
      *B603713,1 AMH [Start] let ladata[55] bounded by max and min value of its field
      *laData[55] = 100 * (ROUND(laData[25],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[25],2))
      laData[55] = MAX(MIN(100 * (ROUND(laData[25],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[25],2)),999999),-99999)
      *B603713,1 AMH [END]
    ENDIF
    IF laData[26]<>0
      *B603713,1 AMH [Start] let ladata[56] bounded by max and min value of its field
      *laData[56] = 100 * (ROUND(laData[26],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[26],2))
      laData[56] = MAX(MIN(100 * (ROUND(laData[26],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[26],2)),999999),-99999)
      *B603713,1 AMH [END]
    ENDIF
    IF laData[60]<>0
      *B603713,1 AMH [Start] let ladata[81] bounded by max and min value of its field
      *laData[56] = 100 * (ROUND(laData[60],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[60],2))
      laData[81] = MAX(MIN(100 * (ROUND(laData[60],2)-ROUND(laData[57],2)) / ;
                    IIF(llStyMark,ROUND(laData[57],2),ROUND(laData[60],2)),999999),-99999)
      *B603713,1 AMH [END]
    ENDIF
  ENDIF
ENDIF

*--HDM We Have to Replace in the temp. file[START]
*lnPrevTbl = SELECT(0)
*SELECT(lcColorFil)
*IF llAllColors
*  REPLACE ALL GROS_PRICE WITH lnCostVal0;
*              DISC_PCNT  WITH lnCostPrc0;
*              NICOST1    WITH lnCostVal1;
*              NICOST2    WITH lnCostVal2;
*              NICOST3    WITH lnCostVal3;
*              NICOST4    WITH lnCostVal4;
*              NICOST5    WITH lnCostVal5
*ELSE
*  REPLACE  GROS_PRICE WITH lnCostVal0;
*           DISC_PCNT  WITH lnCostPrc0;
*           NICOST1    WITH lnCostVal1;
*           NICOST2    WITH lnCostVal2;
*           NICOST3    WITH lnCostVal3;
*           NICOST4    WITH lnCostVal4;
*           NICOST5    WITH lnCostVal5
*ENDIF
*SELECT (lnPrevTbl)
*--HDM We Have to Replace in the temp. file[END]

*B120221,1 ABD - update the cost fields at the temp file in case copy and not multi curr. [Begin]

*B038753,1 WSH Copy cost info to temp file in all cases [Start]
*IF llCopySt .AND. !llMulcurr
*B038753,1 WSH [End]

  *-- Save old File & recored.
  PRIVATE lnOld_Rec , lnPrevTbl
  lnPrevTbl = SELECT(0)
  SELECT(lcColorFil)
  IF llAllColors
    lnOld_Rec = RECNO()
    REPLACE ALL GROS_PRICE WITH lnCostVal0;
                DISC_PCNT  WITH lnCostPrc0;
                NICOST1    WITH lnCostVal1;
                NICOST2    WITH lnCostVal2;
                NICOST3    WITH lnCostVal3;
                NICOST4    WITH lnCostVal4;
                NICOST5    WITH lnCostVal5

    *B038753,1 WSH Copy Percentage quantities to temp file [Start]
    REPLACE ALL NPRCOST2   WITH lnCostprc2;
                NPRCOST3   WITH lnCostprc3;
                NPRCOST4   WITH lnCostprc4;
                NPRCOST5   WITH lnCostprc5
    *GOTO lnOld_Rec
    IF BETWEEN(lnOld_Rec,1,RECCOUNT()) 
       GOTO lnOld_Rec
    ENDIF    
    *B038753,1 WSH [End]
    
  ELSE
    *B038753,1 WSH Set the record pointer to the selected style in case of specific color [Start]
    SEEK lcMajor + lcNMkey
    *B038753,1 WSH  [End]

    REPLACE  GROS_PRICE WITH lnCostVal0;
             DISC_PCNT  WITH lnCostPrc0;
             NICOST1    WITH lnCostVal1;
             NICOST2    WITH lnCostVal2;
             NICOST3    WITH lnCostVal3;
             NICOST4    WITH lnCostVal4;
             NICOST5    WITH lnCostVal5

    *B038753,1 WSH Copy Percentage quantities to temp file [Start]
    REPLACE  NPRCOST2   WITH lnCostprc2;
             NPRCOST3   WITH lnCostprc3;
             NPRCOST4   WITH lnCostprc4;
             NPRCOST5   WITH lnCostprc5
    *B038753,1 WSH [End]

  ENDIF
  SELECT (lnPrevTbl)

*B038753,1 WSH Copy cost info to temp file in all cases [Start]
*ENDIF
*B038753,1 WSH [End]

*B120221,1 ABD - [End]


SELECT(lnalias)
IF !llLineCost
  =lfRefresh(lcWinCh5)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvCostPrc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid costs prec. Used only in case of not imported
*!           : and not multi currency imported style.   
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcCstNo->Cost no. as character.
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvCostPrc
PARAMETERS lcCstNo

*E300803,1 SWK 05/25/98 Add a check that the % cannot be less than zero
IF lnCostPrc&lcCstNo < 0
  lnCostPrc&lcCstNo = lcOldVAlue
  =gfModalGen('TRM42000B42001','DIALOG')
  _CUROBJ = _CUROBJ
  RETURN
ENDIF
*E300803,1 (End)
IF lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. gcContCode = lcEgypt .AND.;
                        lncostval1 > 0 .AND. lncostval0 > 0 .AND. EVAL(SYS(18)) > 0
  
  IF gfModalGen("QRM42151B42012","DIALOG",lcCostLbl&lcCstNo) = 2
    lnCostVal&lcCstNo = (lnCostPrc&lcCstNo * lncostval1 )/100
  ELSE
    lnCostVal&lcCstNo = (lnCostPrc&lcCstNo * lncostval0 )/100
  ENDIF
ELSE
  lnCostVal&lcCstNo = (lnCostPrc&lcCstNo * lncostval1 )/100
ENDIF
SHOW GET lnCostVal&lcCstNo
SHOW GET lnCostPrc&lcCstNo

IF !llLineCost
  =lfRefresh(lcWinCh5)
ENDIF

*!*************************************************************
*! Name      : lfvFCostPrc
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Valid foriegn costs prec.
*!*************************************************************
FUNCTION lfvFCostPrc
PARAMETERS lnCstNo

lcCstNo = STR(lnCstNo,1)
IF llDispMsg AND lnDomestic = 2 .AND. lcIType&lcCstNo = 'D' .AND. gcContCode = lcEgypt .AND.;
                        laData[45] > 0 .AND. laData[83] > 0 .AND. EVAL(SYS(18)) > 0

  IF gfModalGen("QRM42151B42012","DIALOG",lcCostLbl&lcCstNo) = 2
    laData[44+lnCstNo] = (lnFCostPr&lcCstNo * laData[45])/100
    llOnGross = .F.
    llDispMsg = .F.
  ELSE
    laData[44+lnCstNo] = (lnFCostPr&lcCstNo * laData[83])/100
    llOnGross = .T.
    llDispMsg = .F.
  ENDIF
ELSE
  laData[44+lnCstNo] = (lnFCostPr&lcCstNo * laData[45])/100  
ENDIF
SHOW GET laData[44+lnCstNo]
SHOW GET lnFCostPr&lcCstNo

=lfvEquCost(lcCstNo,laData[44+lnCstNo],lnChExRate,lnCurrUnt2)

*!*************************************************************
*! Name      : lfvCosting
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Browe costing information.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvCosting

lnAlias=SELECT()
IF llDetCost AND SEEK(lcMajor,'BOM')
  *--Save variable that may be changed in costing proc.
  lnStyRec=RECNO('STYLE')
  DO (gcAppHome+"Costing.FXP") WITH lcMajor+lcSepart+lcNonMjr,'BOM',IIF(laData[21],'M','I')
  GOTO lnStyRec IN Style
ELSE
  *--No cost sheet information available to compute costing.'
  =gfModalGen('TRM42052B42001','DIALOG')
ENDIF
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfToTPW.
*! Developer : Samah Wilson Kirrollos (SWK)
*! Date      : 05/25/98
*! Purpose   : Get the total WIP for all sizes
*!*************************************************************
*! Calls     : E300803,1 SWK 05/25/98
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfToTPW()
*!*************************************************************
FUNCTION lfToTPW
PRIVATE lnAlias,lnStyRec

lnAlias    = SELECT()
lnTotWip   = 0
lnTotPrPak = 0
lnTotWO    = 0
lnCTTotStk = 0
lnTotAlo   = 0
lnTotOrd   = 0
lnToTShp   = 0
lnTOTRA    = 0
lnTOTRET   = 0
*B606741,1 RAE [start]
lnTotUnAll = 0
*B606741,1 RAE [end]
*B602831,1 initialize the total plane.
IF llStyMode
  lnTPlan9 = 0
ENDIF

STORE 0 TO lnInTrn1,lnInTrn2,lnInTrn3,lnInTrn4,lnInTrn5,lnInTrn6,lnInTrn7,lnInTrn8,lnTInTrn
IF llAllScales OR (llAllColors AND !llMScale)
  laData[39] = 0
  IF llstyMode
    SELECT STYLE
    lcWarExp = ".T."
  ELSE
    SELECT STYDYE
    *B606859,1 ASH 02/16/2003 (Begin) Fix the bug of wrong calculating total inv. in case of using dyelots.
    *lcWarExp = "cWarecode = lcWarecode"
    lcWarExp = "cWarecode = lcWarecode AND EMPTY(DYELOT)"
    *B606859,1 ASH 02/16/2003 (End)
  ENDIF  
  lnStyRec = IIF(!EOF(),RECNO(),0)
*B602831,1 Change FOR to WHILE in the following Scan command.   
  =SEEK(lcMajor+ALLTRIM(lcNMKey))
* SCAN FOR SUBSTR(Style,1,lnStyleWid+len(lcNMKey))+IIF(llStyMode,'',cWarecode) = lcMajor+ALLTRIM(lcNMKey)+IIF(llStyMode,'',lcWarecode)
  *B605442,1 ASH 01/29/2002 (Begin) Scan only for the color entered by the user.
  ** if the required color is '11' and the next color is '113', the program is scanning for both colors not only '11'
  *SCAN WHILE Style=lcMajor+ALLTRIM(lcNMKey) FOR &lcWarExp
  SCAN WHILE Style=lcMajor+lcNMKey FOR &lcWarExp
  *B605442,1 ASH 01/29/2002 (End)
*B602831,1 End.
    =SEEK('P'+STYLE.SCALE+STYLE.PREPAK,'SCALE')
    lnTotPrPak = lnTotPrPak + SCALE.PPTOT
    lnTotWip   = lnTotWip   + TOTWIP
    *--HDM B602340,1 InCorrect Plan QTY in General Folder[Start]
    *laData[39] = laData[39] + STYLE.TOTPLAN
    laData[39] = STYLE.TOTPLAN
    *--HDM B602340,1 InCorrect Plan QTY in General Folder[end]
    lnTotWO    = lnTotWO    + nTotWo        
    lnCTTotStk = lnCTTotStk + TotStk 
    lnTotAlo   = lnTotAlo   + TotAlo
    lnTotOrd   = lnTotOrd   + TotOrd
    lnToTShp   = lnToTShp   + TotShp
    lnTOTRA    = lnTOTRA    + TOTRA
    lnTOTRET   = lnTOTRET   + TOTRET
    lnTInTrn   =lnTInTrn    + TotInTrn

    *B606741,1 RAE Unallocated QTY total is not calculated properly. [start]
    IF !llTotAvlbl
      lnTotUnAll = lnTotUnAll + MAX((TotStk - TotAlo),0)
    ELSE
      lnTotUnAll = lnTotUnAll + MAX((TotWip + TotStk - TotAlo),0)
    ENDIF
    *B606741,1 RAE [end]
    
    *B602831,1 Calculate the total plane.
    IF llStyMode
      lnTPlan9   = lnTPlan9   + STYLE.TOTPLAN
    ENDIF

    *--HDM Fixing Bug =>What if we don't have an E.S.S. => INTRANSIT will not be read[start]
    IF SUBSTR(STYLE,LEN(ALLTRIM(STYLE))-2,3) = laExtSz[lnCntlScl]
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        lnInTrn&lcCount = lnInTrn&lcCount + InTrans&lcCount
      ENDFOR
    ELSE
      IF llAllColors AND !llMScale
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          lnInTrn&lcCount = lnInTrn&lcCount + InTrans&lcCount
        ENDFOR
      ENDIF
    ENDIF
    
    *--HDM Fixing Bug =>What if we don't have an E.S.S. => INTRANSIT will not be read[end]
  ENDSCAN
  IF lnstyrec <> 0
    GOTO lnStyRec
  ENDIF  
ELSE
  IF !llStyMode
    =SEEK(STYLE.STYLE+lcWarecode,'STYDYE')
  ENDIF  
  =SEEK('P'+STYLE.SCALE+STYLE.PREPAK,'SCALE')
  lnTotPrPak = lnTotPrPak + SCALE.PPTOT
  lnTotWip   = lnTotWip   + IIF(llStyMode,STYLE.TOTWIP,STYDYE.TOTWIP)
  lnTotWO    = lnTotWO    + IIF(llStyMode,STYLE.nTotWo,STYDYE.nTotWo)
  lnCTTotStk = lnCTTotStk + IIF(llStyMode,STYLE.TotStk,STYDYE.TotStk)
  lnTotAlo   = lnTotAlo   + IIF(llStyMode,STYLE.TotAlo,STYDYE.TotAlo)
  lnTotOrd   = lnTotOrd   + IIF(llStyMode,STYLE.TotOrd,STYDYE.TotOrd)
  lnToTShp   = lnToTShp   + IIF(llStyMode,STYLE.TotShp,STYDYE.TotShp)
  lnTOTRA    = lnTOTRA    + IIF(llStyMode,STYLE.TOTRA,STYDYE.TOTRA)
  lnTOTRET   = lnTOTRET   + IIF(llStyMode,STYLE.TOTRET,STYDYE.TOTRET)

  *B606741,1 RAE Unallocated QTY total is not calculated properly. [start]
  IF llStyMode
    lnTotUnAll = lnTotUnAll + IIF(!llTotAvlbl,MAX((STYLE.TotStk-STYLE.TotAlo),0),MAX((STYLE.TotWip+STYLE.TotStk-STYLE.TotAlo),0))
  ELSE
    lnTotUnAll = lnTotUnAll + IIF(!llTotAvlbl,MAX((STYDYE.TotStk-STYDYE.TotAlo),0),MAX((STYDYE.TotWip+STYDYE.TotStk-STYDYE.TotAlo),0))
  ENDIF
  *B606741,1 RAE [end]
  
  *B602831,1 Calculate the total plane.
  IF llStyMode
    lnTPlan9   = lnTPlan9   + STYLE.TOTPLAN
  ENDIF

  FOR lnCount = 1 TO 8
    lcCount = STR(lnCount,1)
    lnInTrn&lcCount = lnInTrn&lcCount + IIF(llStyMode,STYLE.InTrans&lcCount,STYDYE.InTrans&lcCount)
  ENDFOR
  lnTInTrn   =lnTInTrn+IIF(llStyMode,STYLE.TotInTrn,STYDYE.TotInTrn)
ENDIF  
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfTotPrePak.
*! Developer : Samah Wilson Kirrollos (SWK)
*! Date      : 05/25/98
*! Purpose   : Get the total prepack for all sizes
*!*************************************************************
*! Calls     : E300803,1 SWK 05/25/98
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfTotPrePak()
*!*************************************************************
FUNCTION lfTotPrePak
PRIVATE llPrepak,lnAlias

llPrepak = .F.
lnAlias  = SELECT()

STORE 0 TO lnprpak1,lnPrPak1x,lnprpak2,lnPrPak2x,lnprpak3,lnPrPak3x,;
           lnprpak4,lnPrPak4x,lnprpak5,lnPrPak5x,lnprpak6,lnPrPak6x,;
           lnprpak7,lnPrPak7x,lnprpak8,lnPrPak8x

=SEEK('P'+laData[6]+laData[7],'SCALE')
IF llAllScales
  STORE 0 TO laData[31],laData[32],laData[33],laData[34],laData[35],laData[36],;
             laData[37],laData[38]
  *STORE 0 TO lnInTrn1,lnInTrn2,lnInTrn3,lnInTrn4,lnInTrn5,lnInTrn6,lnInTrn7,lnInTrn8,lnTInTrn
  lcSqlScl  = IIF(llAllScales,laExtSz[lnCntlScl],"")
  SELECT STYLE
  lnStyRec  = RECNO('STYLE')
  *B602831,1 Change FOR to WHILE in the following Scan command.   
  =SEEK(lcMajor+lcNMKey)
  *SCAN FOR SUBSTR(Style,1,lnStyleWid+len(lcNMKey)) = lcMajor+ALLTRIM(lcNMKey) AND SCALE = lcSqlScl
  SCAN WHILE Style = lcMajor+lcNMKey FOR SCALE = lcSqlScl
  *B602831,1 End.
    llPrepak = !EMPTY(STYLE.PREPAK)
    lcPrePak = STYLE.PREPAK
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laData[30+lnCount] = laData[30+lnCount] + Plan&lcCount
      *lnInTrn&lcCount    = lnInTrn&lcCount    + InTrans&lcCount
    ENDFOR
    IF llPrepak
      =SEEK('P'+lcSqlScl+lcPrePak,'SCALE')
      lnprpak1  = lnprpak1  + scale.pp1
      lnprpak1x = lnprpak1x + scale.pp1
      lnprpak2  = lnprpak2  + scale.pp2
      lnprpak2x = lnprpak2x + scale.pp2
      lnprpak3  = lnprpak3  + scale.pp3
      lnprpak3x = lnprpak3x + scale.pp3
      lnprpak4  = lnprpak4  + scale.pp4
      lnprpak4x = lnprpak4x + scale.pp4
      lnprpak5  = lnprpak5  + scale.pp5
      lnprpak5x = lnprpak5x + scale.pp5
      lnprpak6  = lnprpak6  + scale.pp6
      lnprpak6x = lnprpak6x + scale.pp6
      lnprpak7  = lnprpak7  + scale.pp7
      lnprpak7x = lnprpak7x + scale.pp7
      lnprpak8  = lnprpak8  + scale.pp8
      lnprpak8x = lnprpak8x + scale.pp8
    ENDIF
  ENDSCAN
  *IF llStyMode 
   * SELECT STYLE
    *IF lnstyrec > 0 .AND. lnstyrec <= RECCOUNT("STYLE")
     * GOTO lnStyRec
    *ENDIF  
  *ELSE
   * SELECT STYDYE  
    *=SEEK(STYLE.STYLE+lcWarecode,'STYDYE')
  *ENDIF
  *SCAN FOR SUBSTR(Style,1,lnStyleWid+len(lcNMKey))+IIF(llStyMode,'',cWarecode) = lcMajor+ALLTRIM(lcNMKey)+IIF(llStyMode,'',lcWarecode)  
   * FOR lnCount = 1 TO 8
    *  lnInTrn&lcCount    = lnInTrn&lcCount    + InTrans&lcCount
    *ENDFOR
    *lnTInTrn=lnTInTrn+TotInTrn
  *ENDSCAN
  *SELECT STYLE
  IF lnstyrec > 0 .AND. lnstyrec <= RECCOUNT("STYLE")
    GOTO lnStyRec
  ENDIF  
ENDIF
IF !llAllScales
  STORE  scale.pp1 TO lnprpak1,lnPrPak1x
  STORE  scale.pp2 TO lnprpak2,lnPrPak2x
  STORE  scale.pp3 TO lnprpak3,lnPrPak3x
  STORE  scale.pp4 TO lnprpak4,lnPrPak4x
  STORE  scale.pp5 TO lnprpak5,lnPrPak5x
  STORE  scale.pp6 TO lnprpak6,lnPrPak6x
  STORE  scale.pp7 TO lnprpak7,lnPrPak7x
  STORE  scale.pp8 TO lnprpak8,lnPrPak8x
ENDIF 
=lfToTPW()
SELECT(lnAlias)
*!*************************************************************
*! Name      : lfvInvSty.
*! Developer : Samah Wilson Kirrollos (SWK)
*! Date      : 05/25/98
*! Purpose   : Validate the detail costing button
*!*************************************************************
*! Calls     : E300803,1 SWK 05/25/98
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvInvSty()
*!*************************************************************
FUNCTION lfvInvSty

IF laScrMode[3] AND laData[75]
 IF &lctpstyle..TotStk<>0
   *--You cannot change the dyelot since you have quantity in Stock.
   =gfModalGen('TRM42139B42001','DIALOG','Stock')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotWip<>0 
   *--You cannot change the dyelot since you have quantity in WIP.
   =gfModalGen('TRM42139B42001','DIALOG','WIP')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotOrd<>0 
   *--You cannot change the dyelot since you have quantity in Order.
   =gfModalGen('TRM42139B42001','DIALOG','Order')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotAlo<>0 
   *--You cannot change the dyelot since you have quantity in Alocation.
   =gfModalGen('TRM42139B42001','DIALOG','Alocation')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotShp<>0 
   *--You cannot change the dyelot since you have quantity in Shipment.
   =gfModalGen('TRM42139B42001','DIALOG','Shipment')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotRet<>0 
   *--You cannot change the dyelot since you have quantity in Return.
   =gfModalGen('TRM42139B42001','DIALOG','Return')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..TotRa<>0 
   *--You cannot change the dyelot since you have quantity in Ra.
   =gfModalGen('TRM42139B42001','DIALOG','Ra')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
 IF &lctpstyle..nTotWo<>0 
   *--You cannot change the dyelot since you have quantity in Wo.
   =gfModalGen('TRM42139B42001','DIALOG','Wo')
   llInvSty = lcOldValue
   SHOW GET llInvSty
   RETURN
 ENDIF
ENDIF
RETURN
*!*************************************************************
*! Name      : lfvQty.
*! Developer : Samah Wilson Kirrollos (SWK)
*! Date      : 05/25/98
*! Purpose   : Validate the at qtys
*!*************************************************************
*! Calls     : E300803,1 SWK 05/25/98
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvQty()
*!*************************************************************
FUNCTION lfvQty
PARAMETER lcQtyField
PRIVATE   lcQtyField 

IF EVALUATE(lcQtyField) < 0
  *--Negative values are not allowed.
  =gfModalGen('TRM42000B42001','DIALOG')
  &lcQtyField = lcOldValue
ENDIF
*!*************************************************************
*! Name      : lfvDtCst.
*! Developer : Samah Wilson Kirrollos (SWK)
*! Date      : 05/25/98
*! Purpose   : Validate the detail costing button
*!*************************************************************
*! Calls     : E300803,1 SWK 05/25/98
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvDtCst()
*!*************************************************************
FUNCTION lfvDtCst

IF !llDetCost
  *-- Check if there is a cost sheet
  IF SEEK(lcMajor,'BOM')
    =gfModalGen('TRM42142B42001','DIALOG')
    llDetCost = .T.
    SHOW GET llDetCost
    RETURN
  ELSE
    IF lnDomestic = 1 AND llMFCost
      FOR lnCount = 1 to 5
        lcCount = STR(lnCount,1)
        IF lcMtype&lcCount $ 'FST'
          *-- HDM B602667,1[Start] The User has the option to mark as not detail costing
          *--               even the setup said Yes
          *llDetCost = .T.
          *-- HDM B602667,1[End]
          SHOW GET llDetCost
          RETURN
        ENDIF
      ENDFOR
    ENDIF
    IF lnDomestic = 2 AND llIMPCost
      FOR lnCount = 1 to 5
        lcCount = STR(lnCount,1)
        IF lcItype&lcCount $ 'FST'
          *-- HDM B602667,1[Start] The User has the option to mark as not detail costing
          *--               even the setup said Yes
          *llDetCost = .T.
          *-- HDM B602667,1[End]
          SHOW GET llDetCost
          RETURN
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvEdtCost
*! Developer : Timour A. K.
*! Date      : 06/02/97
*! Purpose   : Function to Edit Costs for Non Majors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvEdtCost()
*!*************************************************************
FUNCTION lfvEdtCost

IF !(!llDetCost AND !laData[21])
  RETURN
ENDIF

STORE .T. TO llPreColor
SELECT (lccolorfil)

*--Save main costing screen variabels.
*--HDM
lnFCSVal0  = laData[83]
lnECSVal0  = lnCostVal0
lnECPVal0  = laData[84]
lnFCPVal0  = lnCostPrc0
*--HDM
lnFCSVal1 = laData[45]
lnFCSVal2 = laData[46]
lnFCSVal3 = laData[47]
lnFCSVal4 = laData[48]
lnFCSVal5 = laData[49]
lnECSVal1 = lnCostVal1
lnECSVal2 = lnCostVal2
lnECSVal3 = lnCostVal3
lnECSVal4 = lnCostVal4
lnECSVal5 = lnCostVal5
lnECPVal2 = lnCostPrc2
lnECPVal3 = lnCostPrc3
lnECPVal4 = lnCostPrc4
lnECPVal5 = lnCostPrc5
lnFCPVal2 = lnFCostPr2
lnFCPVal3 = lnFCostPr3
lnFCPVal4 = lnFCostPr4
lnFCPVal5 = lnFCostPr5

*--Take it from the current record.
IF llMulcurr
  *--HDM
  laData[83] = Gros_Price
  laData[84] = DISC_PCNT
  *--HDM

  laData[45] = nICost1
  laData[46] = nICost2
  laData[47] = nICost3
  laData[48] = nICost4
  laData[49] = nICost5
  =lfvCurrncy(laData[27],1,.T.)
  =lfvCurrncy(laData[28],2,.T.)
  =lfvGPrice('F')

ELSE
  *--HDM
  lnCostVal0 = Gros_Price
  lnCostPrc0 = DISC_PCNT
  *--HDM
    
  lnCostVal1 = nICost1
  lnCostVal2 = nICost2 
  lnCostVal3 = nICost3 
  lnCostVal4 = nICost4 
  lnCostVal5 = nICost5 
  lnCostPrc2 = nprcost2
  lnCostPrc3 = nprcost3
  lnCostPrc4 = nprcost4
  lnCostPrc5 = nprcost5
  =lfvGPrice('B')
  SHOW GET lnCostval0  ENABLE
  SHOW GET lnCostPrc0  ENABLE

ENDIF

llLineCost = .T.

*B604192,1 AME  Add new triger to add TOTCOST to sizes in case company is 
*B604192,1 AME  extended size scale for specific customer.
IF llMScale .AND. ASCAN(laEvntTrig , PADR('LLUPDAT_CS',10)) <> 0
  = gfDoTriger('ICSTYLE',PADR('LLUPDAT_CS',10))
  RETURN
ENDIF
*B604192,1 AME [End]

SHOW GETS WINDOW (lcWinCh5) DISABLE ONLY
PUSH KEY
ON KEY
*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
*DO (gcScrDir+gcWinAppl+"\ICNMCst.SPX")
IF laScrMode[3] AND lcCstMth = 'S' AND !llRsonCode
  *--You must have Adjustment Reason Codes to edit costing .
  =gfModalGen('INM42264B42001','DIALOG')
ELSE
  DO (gcScrDir+gcWinAppl+"\ICNMCst.SPX")
ENDIF
*B126312,1 EIH 03/13/2005 [End]
ON KEY
POP KEY

llLineCost = .F.
SELECT (lccolorfil)
lnTotCost = lncostval1 + lncostval2 + lncostval3 + lncostval4 + lncostval5

REPLACE nICost1    WITH IIF(!llMulcurr,lncostval1,laData[45]),;
        nICost2    WITH IIF(!llMulcurr,lncostval2,laData[46]),;
        nICost3    WITH IIF(!llMulcurr,lncostval3,laData[47]),;
        nICost4    WITH IIF(!llMulcurr,lncostval4,laData[48]),;
        nICost5    WITH IIF(!llMulcurr,lncostval5,laData[49]),;
        Gros_Price WITH IIF(!llMulcurr,lncostval0,laData[83]),;
        TOTCost    WITH lnTotCost

REPLACE nprcost2  WITH lnCostPrc2,;
        nprcost3  WITH lnCostPrc3,;
        nprcost4  WITH lnCostPrc4,;
        nprcost5  WITH lnCostPrc5,;
        DISC_PCNT WITH lnCostPrc0
IF (STYLE.Totstk <> 0 OR STYLE.Totret <> 0 AND laScrMode[3]) OR laScrMode[4]
  REPLACE Ave_cost WITH lnTotCost
ENDIF
*--Restore main costing screen variabels.
laData[83] = lnFCSVal0
lnCostVal0 = lnECSVal0
laData[84] = lnECPVal0
lnCostPrc0 = lnFCPVal0

laData[45] = lnFCSVal1
laData[46] = lnFCSVal2
laData[47] = lnFCSVal3
laData[48] = lnFCSVal4
laData[49] = lnFCSVal5

lnCostVal1 = lnECSVal1 
lnCostVal2 = lnECSVal2 
lnCostVal3 = lnECSVal3 
lnCostVal4 = lnECSVal4 
lnCostVal5 = lnECSVal5 
lnCostPrc2 = lnECPVal2 
lnCostPrc3 = lnECPVal3 
lnCostPrc4 = lnECPVal4 
lnCostPrc5 = lnECPVal5 
lnFCostPr2 = lnFCPVal2 
lnFCostPr3 = lnFCPVal3 
lnFCostPr4 = lnFCPVal4 
lnFCostPr5 = lnFCPVal5 
llReadCTmp = .T.
SHOW WINDOW (lcbrowcttl) REFRESH
_CUROBJ = OBJNUM(pbEdtCost)

*B607394,1 ABD - Save the old recored no. (Save the pointer) [Begin]
lnOldRecNo = RECNO()
*B607394,1 ABD - [END]

=lfChngFolder(lnActFolder)

*B607394,1 ABD - [Begin]
IF BETWEEN(lnOldRecNo,1,RECCOUNT(lccolorfil))
  GOTO lnOldRecNo IN (lccolorfil)
ENDIF
*B607394,1 ABD - [END]

RETURN
*---------------
*!*************************************************************
*! Name      : lfvPreType
*! Developer : HOSSAM EL ETREBY
*! Date      : 01/13/1998
*! Purpose   : Validate Prepack Type
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPreType()
*!*************************************************************

FUNCTION lfvPreType

IF llMScale
  RETURN .F.
ENDIF
IF EMPTY(STYLE.PREPAK)
  lnPuPrepak = 2
ELSE
  SHOW GET laData[7] DISABLE
  lnPuPrepak = 1
ENDIF

ACTIVATE POPUP lnPuPrepak
*------------------------------------------------
*!*************************************************************
*! Name      : lfvPuPrepk
*! Developer : HOSSAM EL ETREBY
*! Date      : 01/13/1998
*! Purpose   : Valid Function for Prepack Popup
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvPuPrepk()
*!*************************************************************

FUNCTION lfvPuPrepk

*--Restor old Values

*IF lcPrepack='D'
* SHOW GET laData[82] DISABLE
* SHOW GET laData[7] ENABLE
*ENDIF

IF lnPuPrepak = 1
  IF laScrMode[4] = .F. .AND. laScrMode[3] = .F.
    laData[7] = STYLE.PREPAK
  ENDIF
  SHOW GET laData[82] DISABLE
  SHOW GET laData[7]  ENABLE
  lnPrevtab = SELECT(0)
  SELECT SCALE
  SEEK 'P' + STYLE.SCALE + STYLE.PREPAK
  SELECT (lnPrevtab)
ELSE
  IF laScrMode[4] = .F. .AND. laScrMode[3] = .F.
    laData[82] = STYLE.CBUYPREPK
  ENDIF
  SHOW GET laData[7]  DISABLE
  SHOW GET laData[82] ENABLE
  lnPrevtab = SELECT(0)
  SELECT SCALE
  SEEK 'P' + STYLE.SCALE + STYLE.CBUYPREPK
  SELECT (lnPrevtab)
ENDIF

=lfvprepak()
*--
DEACTIVATE POPUP lnPuPrepak
SHOW GET PBPrepack
_CUROBJ = IIF(lnPuPrepak=1,OBJNUM(laData[7]),OBJNUM(laData[82]))

*------------------------------------------------
*!*************************************************************
*! Name      : lfvGPrice
*! Developer : HOSSAM EL ETREBY
*! Date      : 01/13/1998
*! Purpose   : Valid Function for Cost Fields
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcCostFld = 'F' => Forign Currency
*!                      lcCostFld = 'B' => Base Currency
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvGPrice('F')
*!*************************************************************

FUNCTION lfvGPrice
PARAMETERS lcCostFld
*--HDM B602143,1 Discount Shouldn't be 100 % [start]
IF lcCostFld = 'B'
  IF lnCostPrc0 > 99
    =gfModalGen('QRM42157B00000','DIALOG')
    *WAIT WINDOW "Discount Shouldn't exceed 99 %"
    lnCostPrc0 = 0
    _CUROBJ = OBJNUM(lnCostPrc0)
    RETURN
  ENDIF
ELSE
  IF laData[84] > 99
    =gfModalGen('QRM42157B00000','DIALOG')
    laData[84] = 0
    _CUROBJ = OBJNUM(laData[84])
    RETURN
  ENDIF
  *B603722,1 AMH 07/11/2000 Fix Numeric overflow message (Start)
  IF laData[84]<0
    =gfModalGen('QRM42217B00000','DIALOG')
    laData[84] = 0
    _CUROBJ = OBJNUM(laData[84])
    RETURN
  ENDIF
  *B603722,1 AMH 07/11/2000 Fix Numeric overflow message (End)
ENDIF
*--HDM B602143,1 Discount Shouldn't be 100 % [end]

*--- laData[83] --> G.Price
*--- laData[84] --> Disc
*--- laData[45] --> P.Price
DO CASE

  CASE lcCostFld = 'F'
    *-- If we edit P.Price and it differs from G.Price and G.Price >0
    IF SYS(18) = UPPER("laData(45)") .AND. laData[45] <> laData[83] .AND. laData[83] > 0
      *--Calculate the Disc field.
      laData[84] = ((laData[83] - laData[45]) / laData[83]) * 100
    ELSE
      IF laData[83] > 0
        laData[45] = laData[83] - (laData[83] * (laData[84] / 100))
      ELSE
        *B603604,1 (Begin) If the user edits the Gross price and zeros it out, we must zero out 
        *B603604,1         P.Price and Discount fields.
        *IF laData[83] = 0 .AND. laData[45] > 0
        IF SYS(18) = UPPER("laData(83)") AND laData[83] = 0
          STORE 0 TO laData[45],laData[84]
        ENDIF
        *--Don't calculate G.Price via P.Price and Discount if we edit it itself.
        IF SYS(18) <> UPPER("laData(83)") AND laData[83] = 0 .AND. laData[45] > 0
        *B603604,1 (End)
          laData[83] = laData[45] / (1-(laData[84] / 100))
        ELSE
          IF laData[83] <> laData[45] .AND. laData[84] > 0
            laData[84] = (laData[45] / laData[83]) * 100
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    *--HDM B602938,1 Discount Shouldn't be 100 % [start]
    *lnCostVal0 = lnCostVal1 / (1-(laData[84] / 100)) &&Restore P.Price for basic cost
    IF laData[84] = 100
      =lfvGPrice('F')
      laData[84]=0
    ELSE
      *lnCostVal0 = lnCostVal1 / (1-(laData[84] / 100)) &&Restore P.Price for basic cost
      *B603722,1 AMH 07/11/2000 Fix Numeric overflow message (Start)
      IF laData[84]<0
        =lfvGPrice('F')
        laData[84]=0
      ELSE
        lnCostVal0 = lnCostVal1 / (1-(laData[84] / 100)) &&Restore P.Price for basic cost
      ENDIF
      *B603722,1 AMH 07/11/2000 Fix Numeric overflow message (End)
    ENDIF
    *--HDM B602938,1 [end]

    lnCostPrc0 = laData[84]
    SHOW GET lnCostPrc0
    SHOW GET lnCostVal0

    *B038753,1 WSH Validate Purchase Priece to update temp color file [Start]
    =lfvEquCost('1',laData[45],lnPrExRate,lnCurrUnt1)
    *B038753,1 WSH [End]

  CASE lcCostFld = 'B'
    * laData[45] ==> P.Price  (lnCostVal1)
    * laData[83] ==> Gross    (lnCostVal0)
    * laData[84] ==> Discount (lnCostPrc0)
    IF SYS(18) = UPPER("lnCostVal1") .AND. lnCostVal1 <> lnCostVal0 .AND. lnCostVal0 > 0
      lnCostPrc0 = ((lnCostVal0 - lnCostVal1) / lnCostVal0) * 100
    ELSE
      IF lnCostVal0 > 0
        lnCostVal1 = lnCostVal0 - (lnCostVal0 * (lnCostPrc0 / 100))
      ELSE
        IF lnCostVal0 = 0 .AND. lnCostVal1 > 0
          lnCostVal0 = lnCostVal1 / (1-(lnCostPrc0 / 100))
        ELSE
          IF lnCostVal0 <> lnCostVal1 .AND. lnCostPrc0 > 0
            lnCostPrc0 = (lnCostVal1 / lnCostVal0) * 100
          ENDIF
        ENDIF
      ENDIF
    ENDIF
*    lnCostVal0 = lnCostVal1 / (1-(laData[84] / 100)) &&Restore P.Price for basic cost
    laData[84] = lnCostPrc0
    laData[83] = lnCostVal0
    laData[45] = lnCostVal1
    
    SHOW GET lnCostPrc0
    SHOW GET lnCostVal0
    SHOW GET lnCostVal1

    *B038753,1 WSH Validate Purchase Priece to update temp color file [Start]
    =lfvCost('1')
    *B038753,1 WSH [End]

ENDCASE
SHOW GET laData[84]
SHOW GET laData[83]
SHOW GET laData[45]
SHOW GET lnCostPrc0
SHOW GET lnCostVal0

*------------------------------------------------
*!*************************************************************
*! Name      : lfGetRlt
*! Developer : HOSSAM EL ETREBY
*! Date      : 01/13/1998
*! Purpose   : To Get The Current Color Long Name In The 
*!                                Non-Major Folder Browse
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : Long Color Description If Found
*!*************************************************************
*! Example   : =lfGetRlt()
*!*************************************************************
FUNCTION lfGetRlt
PRIVATE lnClrPos,lnPrevLen,lnLoop

STORE 0 TO lnPrevLen,lnClrPos

*--Look For The Color Segment in laSegInfo=>3

*--Loop To Find The Correct Place
FOR lnLoop = 1 TO ALEN(laSegInfo,1)
  IF laSegInfo[lnLoop,3] = 'C'
    lnClrPos = lnLoop
    EXIT
  ENDIF
ENDFOR
IF lnClrPos > 0
  *--Loop to calculate the prev. segments length
  FOR lnLoop = 1 TO lnClrPos - 1
    lnPrevLen  = lnPrevLen + laSegInfo[lnLoop , 5]
  ENDFOR
  *--
  lnClrLen = laSegInfo[lnClrPos,5]
  lcClrSeg = SUBSTR(STYLE , lnPrevLen + lnClrPos , lnClrLen)

  LLNTHING = gfRltFld(lcClrSeg , @laClrLname , 'COLOR')
  IF !EMPTY(lcClrLName)
    RETURN lcClrLName
  ELSE
    RETURN ''
  ENDIF
ELSE
  RETURN ''
ENDIF
*---------------------------------------------------------------
*!*************************************************************
*! Name      : lfvInTrs
*! Developer : HOSSAM EL ETREBY
*! Date      : 10/28/1998
*! Purpose   : Valid Function For InTransit Button 'Cut & Sold' Folder
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvInTrs()
*!*************************************************************
FUNCTION lfvInTrs
PRIVATE lnPrevTbl,lcPrvTag , lcPosLnTag , lcBrFields
DIMENSION laSelected[4]

*B603711,1 (Begin) If PS is installed let him inquire.
*IF !laData[21] AND !llPOInstld
IF (OCCURS('PS',gcCmpModules)=0) AND !laData[21] AND !llPOInstld
*B603711,1 (End)
  *--XXX module is not installed, Cannot proceed.
  =gfModalGen('QRM42083B42001','DIALOG','Style Purchase Order')
  RETURN
ENDIF
*E301077,7 OPEN PO Files.
IF !laOpFile[6]
  laOpFile[6] = gfOpenFile(gcDataDir+'POShdr','POSHdr'  ,'SH')
ENDIF
IF !laOpFile[7]
  laOpFile[7] = gfOpenFile(gcDataDir+'POSLN' ,'POSLNS'  ,'SH')
ENDIF
IF !laOpFile[8] 
  laOpFile[8] = gfOpenFile(gcDataDir+'Shpmthdr','Shpmthdr','SH')
ENDIF

IF ! lfcnrolex('SHPMTHDR')
  *--No No Transit were founf for '+lcmesgx+'.'
  =gfModalGen('TRM42154B42001','DIALOG',lcmesgx)
  RETURN
ENDIF

WAIT WINDOW 'CALCULATING INTRANSIT' NOWAIT

*--SET RELATION FROM SHPMNTHDR TO POSLN ACCORDING TO POSLN.SHIPNO
lnPrevTbl = SELECT(0)
lcPrvTag  = TAG()
*--Preparing Work Area [Start]

*-- Set the Correct order to POSLN[start]
SELECT POSLN
lcPosLnTag = TAG()                      && Get the Prev. Tag
SET ORDER TO TAG Poshrec

*B603085,1 WAB - the relation must be from posln to shpmthdr
*B603085,1 WAB - START

**-- E301091,1 HDM[start]
*SELECT ShpmtHdr
*SET RELATION TO Shpmthdr.shipno INTO Posln ADDITIVE
*SELECT PosLn
*-- E301091,1 HDM[end]

*B604357,1 TMI [Start] Set the needed relation to Shpmthdr file 
*SELECT PosLn
*SET RELATION TO POSLN.shipno INTO Shpmthdr ADDITIVE
* *B603085,1 WAB - END
SELECT (lcTmpPosLn)
SET ORDER TO Poshrec
SET RELATION TO shipno INTO Shpmthdr ADDITIVE
*B604357,1 TMI [End  ] 

*--Preparing Work Area [end]
lcFile_Ttl = 'InTransit'


*B802469,2 WAB - Delete Date,price Fields  from browse fileds
*B802469,2 WAB - START
*lcBrFields = "lcTranTyp = IIF(TRANCD='3' , 'Shipment' , 'InterLocation') :H = 'Tran. Type' ,"+;
*             "lcTranNo  = IIF(TRANCD='3' , SHIPNO ,                  PO) :H = 'Tran#' ,DATE,"+;
*             "PRICE,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,lcShpETA=IIF(TRANCD='3' ,Shpmthdr.ETA,''):10 :H='E.T.A.',"

lcBrFields = "lcTranTyp = IIF(TRANCD='3' , 'Shipment' , 'InterLocation') :H = 'Tran. Type' ,"+;
             "lcTranNo  = IIF(TRANCD='3' , SHIPNO ,                  PO) :H = 'Tran#' ,"+;
             "QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,lcShpETA=IIF(TRANCD='3' ,Shpmthdr.ETA,''):10 :H='E.T.A.',"

*B802469,2 WAB - END             
             
*-- E301091,1 HDM Added these lines[start]
lcBrFields = lcBrFields + "lcShpStatus=IIF(TRANCD='3' ,Shpmthdr.STATUS,'') :H='Status' ,lcShpEnt=IIF(TRANCD='3',Shpmthdr.ENTERED,''):10:H='Entered',"+;
             "lsShpAir  = IIF(TRANCD='3' ,Shpmthdr.AIRWAYB,''):15 :H='Airway Bill',lcVessel  = IIF(TRANCD='3' ,Shpmthdr.CVESSEL,'')  :H='Vessel',"+;
             "lcCartons = IIF(TRANCD='3' ,Shpmthdr.CARTONS,'')  :H='Cartons',lcRecv = IIF(TRANCD='3' ,Shpmthdr.RECV_STK,''):H='Received',"+;
             "lcDam     = IIF(TRANCD='3' ,Shpmthdr.RECV_DAM,'') :H='Damage', lcCan  = IIF(TRANCD='3' ,Shpmthdr.RECV_CAN,''):H='Canceled',"+;
             "lcRef     = IIF(TRANCD='3' ,Shpmthdr.REFERENCE,''):H='Reference',"

*-- E301091,1 HDM Added these lines[end]
lcBrFields =lcBrFields + "ORD1,ORD2,ORD3,ORD4,ORD5,ORD6,ORD7,ORD8,TOTORD,"
lcBrFields=lcBrFields+;
           "ACCOUNT,STORE,CWARECODE :H='Warehouse'"
   
*--ARIABROW Calling [start]
*B603711,2 AMH 08/20/2000 do not display the Select button in case of PO not Installed [Start]
*llWasSel = AriaBrow([FOR TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0 .AND. STYLE =lcExKey ], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
*            gnBrHSRow2, gnBrHSCol2,"","","SHIPNO ,PO,TRANCD,CSTYTYPE","laSelected")
IF OCCURS('PO',gcCmpModules)=0
  llWasSel = AriaBrow([FOR TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0 .AND. STYLE =lcExKey ], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,"",'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok',"SHIPNO ,PO,TRANCD,CSTYTYPE","laSelected")
ELSE
  llWasSel = AriaBrow([FOR TRANCD$'36' .AND. CSTYTYPE$'PN' .AND. TOTQTY > 0 .AND. STYLE =lcExKey ], lcFile_Ttl, gnBrHSRow1, gnBrHSCol1,;
              gnBrHSRow2, gnBrHSCol2,"","","SHIPNO ,PO,TRANCD,CSTYTYPE","laSelected")
ENDIF
*B603711,2 AMH 08/20/2000 [End  ]
*--ARIABROW Calling [end]

*--If There Is Selected Value[start]
*--Check IF TRANCD = '3' ==> Run Shipment Program
*--ELSE                  ==> Run Postyle  Program
IF llWasSel
  DO CASE
    CASE laSelected[3] = '3'
      lcPOInq = +"'"+laSelected[1]+"'"
      DO gpDoProg WITH 'AWRPOSHP',.F.,'PO',lcPOInq

    CASE laSelected[3] = '6'
      lcPOInq=+"'"+laSelected[4]+"','"+laSelected[2]+"'"
      DO gpDoProg WITH 'AWRPOSTYLE',.F.,'PO',lcPOInq
      
  ENDCASE
ENDIF
*--If There Is Selected Value[end]

*-- HDM B802473,1 [Start] Distroy the relation between posln and Shpmthdr
SELECT PosLn
SET RELATION OFF INTO Shpmthdr
*-- HDM B802473,1 [End]

*--Restor environment[start]
*B604357,1 TMI [Start] release releation 
SELECT (lcTmpPosLn)
SET RELATION OFF INTO Shpmthdr
*B604357,1 TMI [End  ] 
SELECT Shpmthdr
SELECT POSLN
SET ORDER TO TAG &lcPosLnTag
SELECT (lnPrevTbl)
SET ORDER TO TAG &lcPrvTag
*--Restor environment[end]

RETURN
*--------------------------------------------------------------------------------
*!*********************************************************************
*! Name               : lfSaveBom
*! Developer          : HOSSAM EL ETREBY
*! Date               : 11/01/1998
*! Purpose            : Function to save costing elements in BOM File
*!*********************************************************************
*! Calls              : 
*!*********************************************************************
*! Passed Parameters  : lcNmajrKey , lcNmjrKey
*!*********************************************************************
*! Returns            : ............
*!*********************************************************************
*! Called From        : lpSavScr
*!*********************************************************************
*! Example            : =lfSaveBom()
*!*********************************************************************

FUNCTION lfSaveBom
PARAMETER lcMajrKey , lcNmjrKey , lcColorKey
PRIVATE lcSeekKey , lnPrevTBL , lcPrevTag

STORE .F. TO llFInBOM
lcPrevTag = TAG()
lnPrevTBL = SELECT(0)

lcSeekKey = PADR(lcMajrKey + lcSepart + lcColorKey , 19)

IF !laData[80]
  *-- Call function to add MFGCODE if not found in CODES file
  *-- MFGCODE Will be added as (*1 *2 *3 *4 *5)
  =lfAddMgfCd()
  SELECT BOM
  SET ORDER TO TAG BOM
  FOR lnCount = 1 TO 5
    lcCount = STR(lnCount,1)

      IF !SEEK(lcMajor+SPACE(19-LEN(lcMajor))+lcCount+lcSeekKey)
      *--HDM B602143,1 Dublication in saving costing in BOM File[end]
      *B802574,1 Start.
      * IF STYLE.nICost&lcCount > 0
        IF IIF(!laData[21],STYLE.nICost&lcCount,STYLE.nMCost&lcCount) > 0
      *B802574,1 End.
          APPEND BLANK
          llFInBOM = .T.
        ENDIF
      ELSE
        llFInBOM = .T.
      ENDIF

    IF llFInBOM = .T.
      *B802574,1 added IIF(!laData[21] in updating of untcost.
      REPLACE cItmMajor  WITH lcMajor,;
              Typ        WITH lcCount,;
              cItmMask   WITH lcmajrKey+lcSepart+lcColorKey,;
              MFGCODE    WITH PADR('*'+lcCount,LEN(MfgCode)),;
              UOM        WITH 'EAC',;
              UntCost    WITH IIF(!laData[21],STYLE.nICost&lcCount,STYLE.nMCost&lcCount),;
              nBomTotQty WITH 1,;
              TotCost    WITH UntCost,;
              cCatgTyp   WITH IIF(!laData[21],lcIType&lcCount,lcMType&lcCount)

      *--Call TraceKey global function.
      =gfTraceKey('BOM',CITMMAJOR+TYP+CITMMASK+MFGCODE+ITEM+ICLR,'M')
    ENDIF
    *B123029,1 NNA 06/07/2004 (Begin) If details Costing [No] and the cost Element Becomes Zero
    *B123029,1 NNA             we Delete the Record [BOM file] To not Show in the style cost sheet
    IF !LLDETCOST .AND. UNTCOST = 0
      DELETE
    ENDIF
    *B123029,1 NNA (End)

  ENDFOR
ENDIF

SELECT(lnPrevTBL)
SET ORDER TO TAG (lcPrevTag)
*----------------------------------------------------------------------------------
FUNCTION lfAddMgfCd
PRIVATE lnPrevTbl

*--HDM B206323,1 Modify Related Fields to 'Y' Instead of 'N'[start]
lnPrevTbl = SELECT(0)
lcPrevTag = TAG()

SELECT CODES
lcCodesTag = TAG()
SET ORDER TO TAG Ccode_no

FOR lnCdCount = 1 TO 5
  lcCdCount = STR(lnCdCount)
  lcCdCount = ALLTRIM(lcCdCount)
  *E300789,1 Hesham (Start)
  *IF !SEEK(PADR(GCACT_COMP,2)+PADR('MFGCODE',10)+PADR('*'+ALLTRIM(LCCDCOUNT),6))
  IF !SEEK('N'+PADR('MFGCODE',10)+PADR('*'+ALLTRIM(LCCDCOUNT),6))
  *E300789,1 Hesham (End)
  *--First Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'N'                       ,;
            CRLTD_NAM   WITH ''

    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'N'                       ,;
            CRLTD_NAM   WITH ''

   *E300789,1 Hesham (End)
  *--Second Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'CCONTCODE'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'CCONTCODE'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

  *E300789,1 Hesham (End)
  *--Third Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'CCONTNAME'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''
  
      REPLACE CDEFCODE  WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'CCONTNAME'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

  *E300789,1 Hesham (End)
  *--FORTH Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'COPERSEQ'                ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'COPERSEQ'                ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

  *E300789,1 Hesham (END) 
  *--Fifth Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'GLACCOUNT'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'GLACCOUNT'               ,;
            CRLTD_TYP   WITH 'C'                       ,;
            CRLTD_VLU   WITH ''

  *E300789,1 Hesham (END)
  *--Sixth Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LEADTIME'                ,;
            CRLTD_TYP   WITH 'N'                       ,;
            CRLTD_VLU   WITH '0'


    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LEADTIME'                ,;
            CRLTD_TYP   WITH 'N'                       ,;
            CRLTD_VLU   WITH '0'

  *E300789,1 Hesham (END)
  *--SEVENTH Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LINHOUSE'                ,;
            CRLTD_TYP   WITH 'L'                       ,;
            CRLTD_VLU   WITH 'F'

    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LINHOUSE'                ,;
            CRLTD_TYP   WITH 'L'                       ,;
            CRLTD_VLU   WITH 'F'

  *E300789,1 Hesham (END)
  *--Eighth Record for new code
    APPEND BLANK
    *E300789,1 Hesham (Start)
    *REPLACE CCOMP_ID    WITH gcAct_Comp                ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LMFGOPR'                 ,;
            CRLTD_TYP   WITH 'L'                       ,;
            CRLTD_VLU   WITH 'F'
  
  
    REPLACE CDEFCODE    WITH 'N'                       ,;
            CFLD_NAME   WITH 'MFGCODE'                 ,;
            CCODE_NO    WITH '*'+ ALLTRIM(lcCdCount)   ,;
            CDISCREP    WITH lcISlbl&lcCdCount         ,;
            LRLTFIELDS  WITH .F.                       ,;
            CRLTFIELD   WITH 'Y'                       ,;
            CRLTD_NAM   WITH 'LMFGOPR'                 ,;
            CRLTD_TYP   WITH 'L'                       ,;
            CRLTD_VLU   WITH 'F'
  
  *E300789,1 Hesham (END)
  ENDIF
ENDFOR
*--HDM B206323,1 Modify Related Fields to 'Y' Instead of 'N'[end]
*--Restore old environment
SET ORDER TO TAG (lcCodesTag)
SELECT (lnPrevTbl)
SET ORDER TO TAG (lcPrevTag)
RETURN



*!*********************************************************************
*! Name               : lfvLngDsc
*! Developer          : HOSSAM EL ETREBY
*! Date               : 02/22/1999
*! Purpose            : Valid Function for long description field
*!*********************************************************************
*! Calls              : 
*!*********************************************************************
*! Passed Parameters  : None
*!*********************************************************************
*! Returns            : ............
*!*********************************************************************
*! Called From        : laData[4] Validation method
*!*********************************************************************
*! Example            : =lfvLngDsc()
*!*********************************************************************
*! This Function due to B801907,1
*!*********************************************************************

FUNCTION lfvLngDsc
PRIVATE lcColor , lnPrevAli ,lcExStat , lcMsg , lcMsgParam

IF laData[4] = lcOldValue
  RETURN
ENDIF

lnPrevAli = SELECT(0)
lcExStat  = SET('EXACT')
SET EXACT ON

*B802618,1 Start.
*IF laScrMode[3]
IF laScrMode[3]  OR (laScrMode[4] AND llCopySt )
*B802618,1 End.
  SELECT(lcColorFil)
  *B039133,1 NNA 03/17/2005 (Begin) Seek for the Style
  =SEEK(STYLE.STYLE)
  *B039133,1 NNA (End)
  lcColor   = SUBSTR(Style,lnStyleWid+2,lnColorWid)
  lcClrDesc = gfCodDes(lcColor,'COLOR')
  lcClrDesc = ALLTRIM(&lcColorFil..DESC) + " " + ALLTRIM(lcClrDesc)

  IF ALLTRIM(lcClrDesc) <> ALLTRIM(lcOldValue)
    *--This description will be the default for all\this color,Do you wish to continue? y/n
    *B607367,1 KHM 06/29/2003 (Begin) Displaying the message in case of all colors only.
    *B607367,1                and changing the message.
    *lcMsgParam = IIF(llAllColors,'all ' + lcNMjrTl , 'this ' + lcNMjrTl)
    IF llAllColors
      lcMsgParam = ALLTRIM(lcNMjrTl)+"(s)"
    *B607367,1 KHM 06/29/2003 (End)
      IF gfModalGen('TRM42145B42002','DIALOG',+'description'+'|'+lcMsgParam)=2
        laData[4] = lcOldValue
        SHOW GET laData[4]
        SELECT(lnPrevAli)
        SET EXACT &lcExStat
        RETURN
      ENDIF
    *B607367,1 KHM 06/29/2003 (Begin) 
    ENDIF
    *B607367,1 KHM 06/29/2003 (End)    
    SET EXACT OFF
    REPLACE ALL FOR Style=lcMajor+lcNMKey ;
            Desc1 WITH laData[4]
    llAlwsFrst = .T.
    *B603123,1 (Begin) True it.
    llAllClrs = .T.
    *B603123,1 (End)
    SET EXACT ON
  ELSE
    IF llAllColors
      lcMsg = 'all '+ lcNMjrTl
      IF gfModalGen('QRM42159B42002' , 'DIALOG' , lcMsg) = 1
        *B603123,1 (Begin) Set exact off.
        *REPLACE REST WHILE Style=lcMajor+lcNMKey ;
                Desc1 WITH laData[4]
        SET EXACT OFF
        REPLACE REST WHILE Style=lcMajor+lcNMKey ;
                Desc1 WITH laData[4]
        SET EXACT ON
        *--True it.
        llAllClrs = .T.
        *B603123,1 (End)
        llAlwsFrst = .T.
      ELSE
        laData[4] = lcOldValue
        SHOW GET laData[4]
        SELECT(lnPrevAli)
        SET EXACT &lcExStat
        RETURN
      ENDIF
    ELSE
      lcMsg = lcMajor + lcNMKey
      IF gfModalGen('QRM42159B42002' , 'DIALOG' , lcMsg) = 1
        REPLACE Desc1 WITH laData[4]
        llAlwsFrst = .T.
      ELSE
        laData[4] = lcOldValue
        SHOW GET laData[4]
        SELECT(lnPrevAli)
        SET EXACT &lcExStat
        RETURN
      ENDIF
    ENDIF
    
  ENDIF
ENDIF

SELECT(lnPrevAli)
SET EXACT &lcExStat

********************************************************************************************

*E301152,1 Hesham (Start)
FUNCTION lfInfoGet
PRIVATE llPrepak
llPrepak = .F.

IF EMPTY(lcFolGets)
  SELECT STYLE
  SET ORDER TO TAG STYLE

  lcNonMjr = IIF(llallcolors,STRTRAN(lcNMjrPt,'X','*'),lcNonMjr)
  IF laScrMode[2] AND AT('*',lcNonMjr)=0 AND SUBSTR(Style,lnStyleWid+2,lnColorWid)<>lcNonMjr
    lcNonMjr=' '
  ENDIF
  lcNonMjr = IIF(EMPTY(lcNonMjr),SUBSTR(Style,lnStyleWid+2,lnColorWid),lcNonMjr)
  lcNMKey  = lcSepart+IIF(AT('*',lcNonMjr)<>0,SUBSTR(lcNonMjr,1,AT('*',lcNonMjr)-1),lcNonMjr)
  IF !llCopySt
    SEEK lcMajor+lcNMKey
  ENDIF
  SCATTER FIELDS &lcScFields MEMO TO laData
  laData[1] = lcMajor

  SHOW GET lcMajor  DISABLE
  SHOW GET lcNonMjr DISABLE
  llAllScales =(llMScale AND SUBSTR(lcNonMjr,lnColorWid-2,3)='***')
  laData[20]  = IIF(EMPTY(laData[20]),'A',laData[20])
  lnstatus    = IIF(laData[20]='A',1,IIF(laData[20]='H',2,3))
  lndomestic  = IIF(laData[21],1,2)
  lnGrade     = IIF(EMPTY(laData[72]),1,INT(VAL(laData[72])))
  lldye       = IIF((EMPTY(laData[23]) OR laData[23]='N'),.F.,.T.)
  *B602855,1
  *B802091,1 Read the following fields.
  lnPuTaxabl  = IIF(laScrMode[4] AND !llCopySt,IIF(llDefTax,1,2),IIF(laData[30],1,2))
  lncommiss   = IIF(laData[22] OR (laScrMode[4] AND !llCopySt),1,2)
  llInvSty    = IIF(laScrMode[4],.T.,IIF(!laData[75],.F.,.T.))
  *B602855,1 End.
  laData[30] = (lnPuTaxabl=1) 
  
  IF !EMPTY(laData[6])
     IF llMScale
       *--HDM B802112,1[Start] Fixing Bug of In-correct scales sorting
      *SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,1) INTO ARRAY laExtSz
      *E301209,4 Read scale depend on setup scale length.
      *SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,1) ORDER BY 1 INTO ARRAY laExtSz
      SELECT Scale FROM SCALE WHERE Type+Scale='S'+SUBSTR(laData[6],1,lnScaleLen) ORDER BY 1 INTO ARRAY laExtSz
      *E301209,4 End.
      *--HDM B802112,1[End]
     ENDIF
     IF !llAllScales
       IF llMScale
          =SEEK ('S'+SUBSTR(lcNonMjr,lnColorWid-2,3),'SCALE')
       ELSE
         =SEEK ('S'+laData[6],'SCALE')
     ENDIF
     STORE PADL(ALLTRIM(scale.sz1),5) TO lcsize1,lcSz1
     STORE PADL(ALLTRIM(scale.sz2),5) TO lcsize2,lcSz2
     STORE PADL(ALLTRIM(scale.sz3),5) TO lcsize3,lcSz3
     STORE PADL(ALLTRIM(scale.sz4),5) TO lcsize4,lcSz4
     STORE PADL(ALLTRIM(scale.sz5),5) TO lcsize5,lcSz5
     STORE PADL(ALLTRIM(scale.sz6),5) TO lcsize6,lcSz6
     STORE PADL(ALLTRIM(scale.sz7),5) TO lcsize7,lcSz7
     STORE PADL(ALLTRIM(scale.sz8),5) TO lcsize8,lcSz8
     lcSclDesc='Size:'+SCALE.cScl_Desc
     lnScaleCnt=scale.cnt
      FOR I=1 TO lnScaleCnt
        lcI=STR(I,1)
        DIME lascale[I,2]
        lascale[I,1] = SCALE.Sz&lcI
        lascale[I,2] = I
      ENDFOR
      lnTBrk=laData[64]
      *E300803,1 SWK 05/25/98 comments next lines
      *=SEEK('P'+laData[6]+laData[7],'SCALE')
      *E300803,1 (End)
    ELSE
      lnCntlScl = 1
      STORE ' ' TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8
      *B602846,1 Read Ex scl description.
      *lcsize1 = ' Extended '
      *lcsize2 = 'Size Scale'
      *E301209,4 Read scale depend on setup scale length.
      *=SEEK(SUBSTR(laData[6],1,1),'ScaleHd')
      =SEEK(SUBSTR(laData[6],1,lnScaleLen),'ScaleHd')
      *E301209,4 End.      
      lcsize1=SUBSTR(ScaleHd.CSCALEDES,1,10)
      lcsize2=SUBSTR(ScaleHd.CSCALEDES,11,30)
      *B602846,1 End.
      *E301209,4 Read scale depend on setup scale length.
      *=SEEK('S'+SUBSTR(laData[6],1,1),'SCALE')
      =SEEK('S'+SUBSTR(laData[6],1,lnScaleLen),'SCALE')
      *E301209,4 End.
      STORE PADL(ALLTRIM(scale.sz1),5) TO lcSz1
      STORE PADL(ALLTRIM(scale.sz2),5) TO lcSz2
      STORE PADL(ALLTRIM(scale.sz3),5) TO lcSz3
      STORE PADL(ALLTRIM(scale.sz4),5) TO lcSz4
      STORE PADL(ALLTRIM(scale.sz5),5) TO lcSz5
      STORE PADL(ALLTRIM(scale.sz6),5) TO lcSz6
      STORE PADL(ALLTRIM(scale.sz7),5) TO lcSz7
      STORE PADL(ALLTRIM(scale.sz8),5) TO lcSz8
      lcSclDesc='Size:'+SCALE.cScl_Desc
     ENDIF
  ELSE
    STORE '' TO lcsize1,lcsize2,lcsize3,lcsize4,lcsize5,lcsize6,lcsize7,lcsize8
    STORE '' TO lcSz1,lcSz2,lcSz3,lcSz4,lcSz5,lcSz6,lcSz7,lcSz8
    STORE 0  TO lnprpak1,lnprpak2,lnprpak3,lnprpak4,lnprpak5,lnprpak6,lnprpak7,lnprpak8
    STORE 0  TO lnPrPak1x,lnprpak2x,lnprpak3x,lnprpak4x,lnprpak5x,lnprpak6x,lnprpak7x,lnprpak8x
  ENDIF
  IF laData[21] 

    lnCostVal1=laData[40]
    lnCostVal2=laData[41]
    
    lnCostVal3=laData[42]
    lnCostVal4=laData[43]
    lnCostVal5=laData[44]
  ELSE
    IF !llMulCurr
      lnCostVal1=laData[45]
      lnCostVal2=laData[46]
      lnCostVal3=laData[47]
      lnCostVal4=laData[48]
      lnCostVal5=laData[49]
    *B802556,1 ALB (Start)08/30/1999 Fixed Bug of having 0 MarkUp if !MultiCurrency
    ELSE
      =lfvCurrncy(laData[27],1,.T.)
      =lfvCurrncy(laData[28],2,.T.)
    ENDIF
    *B802556,1 (End)
  ENDIF
  lnTotCst   = lnCostVal1+lnCostVal2+lnCostVal3+lnCostVal4+lnCostVal5
  lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[24],2))
  *B603713,1 AMH [Start] let ladata[54] bounded by max and min value of its field
  *laData[54] = IIF(laData[24]=0 OR lnTotCst=0 ,0,((ROUND(laData[24],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
  laData[54] = MAX(MIN(IIF(laData[24]=0 OR lnTotCst=0 ,0,;
                          ((ROUND(laData[24],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
  *B603713,1 AMH [END]
  lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[25],2))
  *B603713,1 AMH [Start] let ladata[55] bounded by max and min value of its field
  *laData[55] = IIF(laData[25]=0 OR lnTotCst=0 ,0,((ROUND(laData[25],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
  laData[55] = MAX(MIN(IIF(laData[25]=0 OR lnTotCst=0 ,0,;
                          ((ROUND(laData[25],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
  *B603713,1 AMH [END]
  lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[26],2))
  *B603713,1 AMH [Start] let ladata[56] bounded by max and min value of its field
  *laData[56] = IIF(laData[26]=0 OR lnTotCst=0 ,0,((ROUND(laData[26],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
  laData[56] = MAX(MIN(IIF(laData[26]=0 OR lnTotCst=0 ,0,;
                          ((ROUND(laData[26],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
  *B603713,1 AMH [END]
  lnRotSub   = IIF(llStyMark,ROUND(lnTotCst,2),ROUND(laData[60],2))
  *B603713,1 AMH [Start] let ladata[81] bounded by max and min value of its field
  *laData[81] = IIF(laData[60]=0 OR lnTotCst=0 ,0,((ROUND(laData[60],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 )
  laData[81] = MAX(MIN(IIF(laData[60]=0 OR lnTotCst=0 ,0,;
                          ((ROUND(laData[60],2)-ROUND(lnTotCst,2)) / lnRotSub )*100 ),999999),-99999)
  *B603713,1 AMH [END]
  laData[2]  = IIF(laScrMode[4],laData[57],laData[2])
  
ENDIF

DO CASE
  CASE lnActFolder = 1 AND !('1' $ lcFolGets)
    lcFolGets = lcFolGets+'1'

    *E300803,1 SWK 05/25/98 Variable used for check if inventory style or not
    llInvSty    = IIF(laScrMode[4],.T.,IIF(!laData[75],.F.,.T.))
    *E300803,1 (End)


    *E300803,1 SWK 05/25/98 Variable used for check if using detail costing
    IF laScrMode[4] AND !llCopySt
      llDetCost = IIF((laData[21] AND llMFCost) OR (!laData[21] AND llImpCost) ,.T.,.F.)  
    ELSE
      llDetCost = laData[80]
    ENDIF
    *E300803,1 (End)
    *B804271,1 ASH 07/11/2001 (Begin) Don't seek in Fabric file unless the MA module not installed.
    IF llMAInstld
    *B804271,1 ASH 07/11/2001 (End)
      lcfabric    = IIF(EMPTY(laData[11]),SPACE(20),IIF(SEEK(laData[11],'FABRIC'),fabric.desc,'**'+gcerrortxt+'**'))
    *B804271,1 ASH 07/11/2001 (Begin) If MA module not installed.
    ELSE
      lcFabric    = SPACE(20)
    ENDIF
    *B804271,1 ASH 07/11/2001 (End)
    
    *laData[23] = IIF(!EMPTY(laData[11]) AND SEEK(laData[11],'Fabric'),fabric.cDye_flg,'N')
    laData[72]  = lnGrade
    laData[22]  = (lncommiss=1)
    laData[23]  =IIF(lldye,'Y','N')
    *E300803,1 SWK 05/25/98 get the value of inventory style
    laData[75]  =IIF(llInvSty,.T.,.F.)
    *E300803,1 (End)
    laData[19]  =IIF(EMPTY(laData[19]),'DEFDEF',laData[19])
    laData[68]  =IIF(EMPTY(laData[68]),'DEF',laData[68])
    J=0
    FOR I=1 TO 3
      IF lnGrade<>I
        J=J+1
        laDamQlt[J,1]=laStyGrd[I,1]
        laDamQlt[J,2]=laStyGrd[I,2]
      ENDIF
    ENDFOR
    = gfwCodePop ( @laCodInfo, "SEASON"   ,"T" )
    = gfwCodePop ( @laCodInfo, "CDIVISION","T" )
    = gfwCodePop ( @laCodInfo, "CSTYGROUP","T" )
    = gfwCodePop ( @laCodInfo, "ROYALTY"  ,"T" )  
    = gfwCodePop ( @laCodInfo, "CDISCCODE"  ,"T" )
    = gfwCodePop ( @laCodInfo, "CPURCODE"  ,"T" )
    *--HDM
    *B802854,1 SAM (Start) comment these lines and moved them after endcase
    *B802854,1     cause non major discription updated only if folder 1(general) is active
    *lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
    *lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
    *=lfRefresh(lcWinCh0)
    *B802854 - END
    IF llPrepak .AND. lcPrepack <> 'N'
    *--HDM
      *IF laScrMode[4]
        ladata[7] = &lcColorFil..PREPAK
      *ELSE
      *  ladata[7] = STYLE.PREPAK
    *  ENDIF
    ENDIF
    *=lfRefresh(lcWinCh0)
    *--HDM
    IF !EMPTY(laData[6])
      *E300803,1 SWK 05/25/98 Call the function to calculate totals
      =lfTotPrePak()
      *E300803,1 (End)
    ENDIF
    IF !llmultiwh
      *--Get the default warehouse.
      SELECT WareHous
      GO TOP IN WareHous
      laData[71]=WareHous.cWareCode
    ELSE
      *B605451,1 ASH 02/25/2002 (Begin) Display a message if the style default location not found in warehouse file.
      IF ASCAN(laDefWhs,laData[71]) = 0
        =gfModalGen('QRM42226B42001','DIALOG')
        laData[71] = ''
      ENDIF
      *B605451,1 ASH 02/25/2002 (End)
    
      IF lcSysType = 'P'
        lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,1)
      ELSE
        lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,;
                      ASUBSCRIPT(laDefWhs,ASCAN(laDefWhs,laData[71]),1) )
      ENDIF
    ENDIF
  CASE lnActFolder = 2 AND !('2' $ lcFolGets)

    lcFolGets = lcFolGets+'2'  
    STORE 1 TO lnloc

    IF !llmultiwh
      *--Get the default warehouse.
      SELECT WareHous
      GO TOP IN WareHous
      laData[71]=WareHous.cWareCode
      IF llmultiloc
        =lfvviewloc()
      *E301152,1 Hesham (Start)   
      ELSE
        *--Calculate the optional line.
        =lfGetOptn()
        *E300803,1 SWK 05/25/98 Call the function to get the totals
        =lfTotPrePak()
        *E300803,1 (End)
        *--Get cut & sold information.
        =lfGetTmp()
      *E301152,1 Hesham (End)  
      ENDIF 
    ELSE
      IF lcSysType = 'P'
        lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,1)
      ELSE
        lnDefWhs=IIF((laScrMode[4] AND !llCopySt) OR EMPTY(laData[71]) , 0 ,;
                      ASUBSCRIPT(laDefWhs,ASCAN(laDefWhs,laData[71]),1) )
      ENDIF
      IF llmultiloc &&.AND. laScrMode[2]
         =lfvviewloc()
      *E301152,1 Hesham (Start)   
      ELSE
        *--Calculate the optional line.
        =lfGetOptn()
        *E300803,1 SWK 05/25/98 Call the function to get the totals
        =lfTotPrePak()
        *E300803,1 (End)
        *--Get cut & sold information.
        =lfGetTmp()
      *E301152,1 Hesham (End)  
      ENDIF 

    ENDIF
    *E301152,1 Hesham (Start)
    *--Calculate the optional line.
    *=lfGetOptn()
    *E300803,1 SWK 05/25/98 Call the function to get the totals
    *=lfTotPrePak()
    *E300803,1 (End)
    *--Get cut & sold information.
    *=lfGetTmp()
    *E301152,1 Hesham (End)
    *--Create warehouses temp.
    IF llMultiwh
      SELECT (lcwarefil)
      GO TOP
      IF EOF() OR (Style<>lcMajor+lcNMKey)
        SELECT STYDYE
        SEEK lcMajor+lcNMKey
        SCAN WHILE Style=lcMajor+lcNMKey FOR EMPTY(dyelot)
          SELECT (lcwarefil)
          APPEND BLANK
         *C102045,1 ABD Add cDiscCode for the temp file when created. [Begin]
          *REPLACE Style     WITH lcMajor+lcNMKey,;
                  Desc      WITH laData[3],; 
                  cWareCode WITH STYDYE.cWareCode,;
                  gl_link   WITH STYDYE.Gl_Link,;
                  cStatus   WITH 'S'

          REPLACE Style     WITH lcMajor+lcNMKey  ,;
                  Desc      WITH laData[3]        ,; 
                  cWareCode WITH STYDYE.cWareCode ,;
                  gl_link   WITH STYDYE.Gl_Link   ,;
                  cStatus   WITH 'S'              ,;
                  cdisccode WITH  STYDYE.cDiscCode
        *C102045,1 ABD [End]
        ENDSCAN
        SELECT (lcwarefil)
        INDEX ON cwarecode TAG cwarecode Unique
        SET ORDER TO       TAG cwarecode
      ENDIF
    ENDIF
    =lfvViewWh(.T.)

 CASE lnActFolder = 3 AND !('3' $ lcFolGets)
 *   WAIT '3' WINDOW
    lcFolGets = lcFolGets+'3'  
    STORE 1 TO lnUpc
    lnDamQlt = 1
    lcRetSty=IIF(lnDamQlt=1,laData[70],laData[73])
  
    GO TOP IN GL_LINK
    IF !EOF('GL_LINK')
      lcGllnk6   = laData[19]
      IF llLink_GL AND !EMPTY(lcGllnk6)
        =gfGLbrowse('03',@lcGllnk6,@lcLinkDesc,0)
        laData[19] = lcGllnk6
      ENDIF
      lcGllnk3  = laData[68]
      IF llLink_GL AND !EMPTY(lcGllnk3)
        =gfGLbrowse('02',@lcGllnk3,@lcSlinkcde,2)
        laData[68]=lcGllnk3
      ENDIF  
      lcLinkDesc = IIF(EMPTY(laData[19]),'',lcLinkDesc)
      lcSlinkcde = IIF(EMPTY(laData[68]),'',lcSlinkcde)
    ENDIF
    IF (ALLTRIM(gcContCode) = lcEngland)
      = gfwCodePop ( @laCodInfo, "CTAXCODE","T" )  
      DIME laTermAry[1,2]
      laTermAry[1,1] = 'NTAXRATE'
      laTermAry[1,2] = 'lnTaxPr'  
      =gfRltFld(laData[63],@laTermAry, 'CTAXCODE')
    ENDIF
    IF llUpc 
      laOpFile[19]= gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
      SELECT STYLEUPC
      *IF ! llAllColors AND SEEK(laData[1]) 
      IF ! llAllColors AND SEEK(lcMajor + lcNMKey)
        I = 0
        *--HDM B802112,1[Start] Change Scan Exp. To seek the whole style 
        *--                     as laData[1] containes major name only
        *SCAN WHILE STYLE=laData[1]
        *B602831,1 Change FOR to WHILE in the following Scan command.   
        *SCAN FOR STYLE=lcMajor + lcNMKey
        SCAN WHILE STYLE=lcMajor + lcNMKey
        *B602831,1 End.
        *--HDM B802112,1[End]

       I=I+1   
       DECLARE laUPCNum[I]
       *--HDM Fixing Bug of wrong variable assigning in case of ESS[Start]
       *z=STR(I,1)
       Z=STYLEUPC.SIZE
       
       *lcSz='lcSize'+Z
       IF llAllScales
         *E301209,4 Read scale depend on setup scale length.
         *=SEEK('S'+ SUBSTR(ALLTRIM(STYLE),LEN(ALLTRIM(STYLEUPC.STYLE))-2),'SCALE')
         =SEEK('S'+ SUBSTR(ALLTRIM(STYLE),LEN(ALLTRIM(STYLEUPC.STYLE))-IIF(lnScaleLen=1,2,1)),'SCALE')
         *E301209,4 End.
       ELSE
         =SEEK('S'+ laData[6],'SCALE')
       ENDIF
       lcSz = 'SCALE.SZ&Z'
       *lcSz=IIF(llAllScales,'lcSz'+Z,'lcSize'+Z)
       *--HDM Fixing Bug of wrong variable assigning in case of ESS[End]

           *lcSiz=ALLTRIM(EVAL(lcSz))+SPACE(5-LEN(ALLTRIM(EVAL(lcSz))))
           lcSiz=ALLTRIM(EVAL(lcSz))+SPACE(5-LEN(ALLTRIM(EVAL(lcSz))))
           laUPCNum[I]=lcSiz+'=>'+cupcnum1+cupcnum2+cupcnum3
        ENDSCAN
      ELSE
        DECLARE laUPCNum[1]
        laUPCNum[1]='N/A'
      ENDIF
    ENDIF
    lncommiss   = IIF(laData[22] OR (laScrMode[4] AND !llCopySt),1,2)    
  CASE (lnActFolder = 4 AND !('4' $ lcFolGets)) OR (lnActFolder = 5 AND !('5' $ lcFolGets))
  *  WAIT '45' WINDOW  
        lcFolGets  = lcFolGets+'45'

        *lccostlbl1 = PROPER(ALLTRIM(IIF(laData[21],lcmslbl1,lcislbl1)))
        lccostlbl1 = IIF(laData[21],lcmslbl1,lcislbl1)
        lccostlbl2 = IIF(laData[21],lcmslbl2,lcislbl2)
        lccostlbl3 = IIF(laData[21],lcmslbl3,lcislbl3)
        lccostlbl4 = IIF(laData[21],lcmslbl4,lcislbl4)
        lccostlbl5 = IIF(laData[21],lcmslbl5,lcislbl5)

        IF laData[21] 
          lnCostPrc2 = IIF(lnCostval1=0,0,(lnCostVal2/lnCostVal1)*100)
          lnCostPrc3 = IIF(lnCostval1=0,0,(lnCostVal3/lnCostVal1)*100)
          lnCostPrc4 = IIF(lnCostval1=0,0,(lnCostVal4/lnCostVal1)*100)
          lnCostPrc5 = IIF(lnCostval1=0,0,(lnCostVal5/lnCostVal1)*100)        
        ELSE
          IF llMulCurr
            STORE 1 TO lnPrExRate,lnChExRate,lnCurrUnt1,lnCurrUnt2
            STORE SET('CURRENCY',1) TO lcfrnsmbl1,lcfrnsmbl2,lcfrnsmbl3,lcfrnsmbl4,lcfrnsmbl5,lcBaseSmbl
  
            =lfvCurrncy(laData[27],1,.T.)
            =lfvCurrncy(laData[28],2,.T.)
            *--E301023,1 HDM [START] Get The Actual Data For The Gross Price & Discount
            *-- If We use Multy Currency We Have To Validate The Values
              =lfvGPrice('F')
            *--E301023,1 HDM [END]

          ELSE
            FOR I = 2 TO 5

              lcXno = STR(I,1)
              IF lcIType&lcXno $ 'D'
        
                IF STYLE.NPRCOST&lcXno = STYLE.NICOST&lcXno *100 / STYLE.GROS_PRICE
                  IF lnCostVal0 = 0
                    lnCostVal0 = STYLE.GROS_PRICE
                  ENDIF
                  lnCostPrc&lcXno=IIF(lnCostval0=0,0,(lnCostVal&lcXno/lnCostVal0)*100)
                ELSE
                   lnCostPrc&lcXno=IIF(lnCostval1=0,0,(lnCostVal&lcXno/lnCostVal1)*100)
                ENDIF
              ELSE
                lnCostPrc&lcXno=IIF(lnCostval1=0,0,(lnCostVal&lcXno/lnCostVal1)*100)
              ENDIF
              SHOW GET lnCostPrc&lcXno
            ENDFOR

            *--E301023,1 HDM [START] Get The Actual Data For The Gross Price & Discount
            *-- If We use Multy Currency We Have To Validate The Values
            lnCostPrc0 = laData[84]
            lnCostVal0 = laData[83]
            *--E301023,1 HDM [END]
          ENDIF
        ENDIF

    *-Create Style Non MAJORS temp file.
    SELECT (lcColorFil)
    GO TOP
    IF EOF() OR Style<>lcMajor
      IF llMScale AND laScrMode[4] AND lnNonSeg=1 AND ALEN(laExtSz)>=1 AND !EMPTY(laExtSz[1])
        SELECT (lcColorFil)
        FOR I=1 TO ALEN(laExtSz)
          APPEND BLANK
          GATHER FROM laData FIELDS &lcScFields MEMO    
          REPLACE Style   WITH lcMajor+lcSepart+laExtSz[I],;
                  cStatus WITH 'A'
        ENDFOR
      ELSE
        SELECT * , 'S' AS cstatus FROM STYLE ;
            WHERE STYLE = lcMajor ;
          	INTO DBF (gcWorkDir+lccolorfil)
      ENDIF
      INDEX ON Style TAG colororder
      SET ORDER TO   TAG colororder
    ENDIF
    =lfActClrBr()           
ENDCASE
*B802854,1 SAM -  non major discription updated only if user 1(general) active
*B802854,1        after endcase to update if any folder is active     
lcCodGet  = IIF(llMScale AND !llallcolors,SUBSTR(lcNonMjr,1,LEN(lcNonMjr)-3),lcNonMjr)
lcNMjrDisc = gfCodDes(lcCodGet,'COLOR')
=lfRefresh(lcWinCh0)
*B802854 - END

*E301152,1 Hesham (End)


FUNCTION lfShowGet

=gpCtrlShow(lcLoclShow,llDoLocal)
=LFREFRESH(lcfolder)
*=LFREFRESH(lafoldwinds[lnActFolder,2])
RETURN .F.


*!**********************************************************************
*! Name      : lfGetDyOrd
*! Developer : Khalid Mohi El-Din
*! Date      : 11/01/1999
*! Purpose   : To get all the dye and adornment orders.
*!**********************************************************************
*! Example   : =lflfGetDyOrd()
*!*************************************************************
*! *B603256,1 KHM 11/01/1999
*!*************************************************************
FUNCTION lfGetDyOrd
 
STORE SPACE(1) TO lcOldPo,lcoldPoSt

SELECT PosLn 
DO WHILE &lcwcondt1
  lcOldPo    = Po
  lcStyNo    = Style
  lcPoType   = cStyType
  lcVendor   = Vendor
  lcPoStatus = PoShdr.Status
  ldComplete = PoSHdr.Complete
  ldEntered  = PoSHdr.Entered
  STORE 0 TO lnOpen,lnReceive,lnCancel,lnDamage
  SCAN REST WHILE &lcwcondt1 AND PO = lcOldPo FOR &lcfcondt1 AND TranCd <> '6'
    IF !EMPTY(lcCtStat)
      lnOpen    = lnOpen + IIF(TranCd $ '1',TotQty,-TotQty)
    ELSE
      lnOpen  = IIF(TranCd $ '1',lnOpen + TotQty, lnOpen)
    ENDIF
    lnReceive = IIF(TranCd = '2', lnReceive + TotQty, lnReceive)
    lnDamage  = IIF(TranCd = '4', lnDamage  + TotQty, lnDamage)
    lnCancel  = IIF(TranCd = '5', lnCancel  + TotQty, lnCancel)
  ENDSCAN
  IF !(lnOpen > 0)
    LOOP
  ENDIF
  IF !SEEK(lcOldPo + lcStyNo , lccursname)
   	INSERT INTO (lccursname) (cuttkt,STATUS,entered,COMPLETE,STYLE,cType);
	                 VALUES  (lcOldPo, lcPoStatus,ldEntered,;
	                          ldComplete,lcStyNo,lcPoType)
	                          
  ENDIF
  IF llMScale AND lcOldPo+SUBSTR(lcStyNo,1,lnStyleWid)+SUBSTR(lcStyNo,lnStyleWid+2+lnColorWid-3,3) <> lcoldPoSt
    lcoldPoSt = lcOldPo+SUBSTR(lcStyNo,1,lnStyleWid)+SUBSTR(lcStyNo,lnStyleWid+2+lnColorWid-3,3)
    IF !SEEK(lcoldPoSt,lccursn1)
      INSERT INTO (lccursn1) (cuttkt,STATUS,entered,COMPLETE,STYLE,cType);
		       		  VALUES (lcOldPo,lcPoStatus,ldEntered,;
		       		          ldComplete,lcStyNo,lcPoType)
    ENDIF
  ENDIF
  
  IF SEEK(lcOldPo+lcStyNo,lccursname)
  	REPLACE &lccursname..bud WITH &lccursname..bud + lnOpen,;
	       &lccursname..rec  WITH lnReceive,;
	       &lccursname..dam  WITH lnDamage,;
 	       &lccursname..can  WITH &lccursname..can + lnCancel
  ENDIF
  IF llMScale AND SEEK(lcOldPo+SUBSTR(lcStyNo,1,lnStyleWid)+SUBSTR(lcStyNo,lnStyleWid+2+lnColorWid-3,3),lccursn1)
  	REPLACE &lccursn1..bud WITH &lccursn1..bud + lnOpen  ,;
 	        &lccursn1..rec WITH &lccursn1..rec + lnReceive,;
   		    &lccursn1..dam WITH &lccursn1..dam + lnDamage,;
   		    &lccursn1..can WITH &lccursn1..can + lnCancel
  ENDIF 
ENDDO

*!*************************************************************
*! Name      : lfUnAloInv
*! Developer : WAB - WALID A. WAHAB
*! Date      : 11/25/1999
*! Purpose   : calclulate the TOtAL Unallocated inventory or the 
*!             Unallocated inventory by size 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  lcSize -> size No
*!						    lcSize = '9'  get total Unallocated inventory
*!							lcSize =('1'-->'8' ) get by size
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfUnAloInv(lcSize)
*!*************************************************************
**E500304,1 WAB 
*!*************************************************************
FUNCTION lfUnAloInv
PARAMETER lcSize
PRIVATE lnTotal,lnAlias
*lnAlias = SELECT()
*SELECT (IIF(!llStyMode,lctpstydye,lctpstyle))
*------------- this function is to calculate the Unallocated inventory depend 
*------------- on setting in IC module  ( Unallocated inventory = STQ - ALO ) or
*------------- (Unallocated inventory = WIP + STQ - ALO )
*--lcSize  --> hold '9' if the function is called to get total Unallocated inventory
*--			   hold ('1'->'8') called to get Unallocated inventory by size
*--lnTotal --> hold the availble qty
*=SEEK(lcMajor+lcNMKey+lcWareCode+SPACE(10))
lcSize = ALLTRIM(lcSize)
IF !llTotAvlbl 		&& the Unallocated inventory = stq - alocated
  IF lcSize = '9'			&& the total Unallocated inventory
  
    *B803136,1 BWA 22/03/2000 Fix the bug in Calculating the total Unallocated inventory field. [START]
    *lnTotal = MAX((TotStk - TotALO),0)
    lnTotal = 0
    FOR lnSize = 1 TO 8
      lcSize = ALLTRIM(STR(lnSize))
      lnTotal = lnTotal + MAX((Stk&lcSize - Alo&lcSize) , 0 )
    ENDFOR
    *B803136,1 [END]
    
  ELSE						&& by size
    lnTotal = MAX((Stk&lcSize - Alo&lcSize) , 0 )
  ENDIF
ELSE					   && Unallocated inventory = wip + stq - alocated
  IF lcSize = '9'		   && the total Unallocated inventory
  
    *B803136,1 BWA 22/03/2000 Fix the bug in Calculating the total Unallocated inventory field.[START]
    *lnTotal = TotWIP + TotStk - TotALO
    lnTotal = 0
    FOR lnSize = 1 TO 8
      lcSize = ALLTRIM(STR(lnSize))
      lnTotal = lnTotal + MAX((WIP&lcSize + Stk&lcSize - Alo&lcSize) , 0 )
    ENDFOR
    *B803136,1 [END]
    
  ELSE					   && by size
    lnTotal = WIP&lcSize + Stk&lcSize - Alo&lcSize
  ENDIF
ENDIF
RETURN(lnTotal)





*******************************************************************************************
* FUNC: lfUPCCheck
* DESC: This function checks if there's any UPC records for the current Style/Color
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/04/2000
* Refer To : B603797
*******************************************************************************************
FUNCTION lfUPCCheck

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT EDICATGH
SCAN
  lcScanExp = type+cpartner+cselccode+&lcColorFil..Style
  IF SEEK(lcScanExp,'EDICATGD')
    SELECT EDICATGD
    SCAN REST WHILE type+cpartner+cselccode+style+size = lcScanExp FOR cEdidStat <> 'D'
      *--Don't delete it.
      SELECT (lnAlias)
      RETURN .F.
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lnAlias)
*!*************************************************************
*! Name      : lfvDescCod
*! Developer : Abdou ElGendi _ (ABD)
*! Date      : 11/27/2000
*! Purpose   : Update the temp file with new dicCode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDescCod()
*!*************************************************************
*C#102045,1
*!*************************************************************
*
FUNCTION lfvDescCod
Private lnalias
lnAlias = SELECT(0)

SELECT (lcwarefil)
REPLACE  cdisccode WITH laWarhDisc[lnWarhDisc,2]

*- Update the style file with the new warehouse with new discound code.
IF &lcwarefil..cWareCode = laData[71]
  laData[78] = laWarhDisc[lnWarhDisc,2]
  = gfwCodePop ( @laNwCodinf, "CDISCCODE"  ,"L" )
  lnDisc = ASCAN('laDisc',laData[78])
  lnDisc = IIF(lnDisc=0,1,ASUBSCRIPT(laDisc,lnDisc,1))
ENDIF
glUpdated = .T.
SELECT(lnAlias)

*-- End OF lfvDescCod
*!*************************************************************
*! Name      : lfvDiscont
*! Developer : Abdou ElGendi _ (ABD)
*! Date      : 11/27/2000
*! Purpose   : Update the temp file with new dicCode.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDiscont()
*!*************************************************************
*C#102045,1
*!*************************************************************
*
FUNCTION lfvDiscont
PRIVATE lasource,latarget , lcDisType
DIMENSION lasource[1],latarget[1]
STORE '' TO lasource, latarget , lcDisType
*-- return if we didn't made any change.
IF lnDisc = lcOldValue
  RETURN
ENDIF
IF llMultiwh
  lcDiscCode = laDisc[lnDisc,2]

  *-- Get the disecound related filed to now which 
  *-- type whole Sale Or Retail sale Or Both.
  DECLARE laDisType[1,2]

  laDisType[1,1] = 'CCOSTAFECT'
  laDisType[1,2] = 'lcDisType'
  =IIF(EMPTY(lcDiscCode),.T.,gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE'))

  IF ALLTRIM(lcDisType) = 'R'
    *-- Fill array the source array with wearhouse.
    SELECT Warehous
    GOTO TOP
    I = 1 
    SCAN REST WHILE !EOF()
      DIMENSION lasource[I]
      lasource [I] = cwarecode
      I = I +1
    ENDSCAN

    =gfMover(@lasource, @latarget,"Select Warehouse",.T.)
  
    *-- Cheak if target array is empty.
    *- You must enter at least one warehouse.  
    IF EMPTY(latarget)
      *-- Text Message : - No locations entered. Cannot update.
      *-- Text No      : - 42017.
      *-- Text Button  : - Add  - Ignore.
      *-- Button No    : -42002
      = gfModalGen('QRM42017B42000','DIALOG')
      laData[78] = laDisc[lcOldValue,2]
      = gfwCodePop ( @laNwCodinf, "CDISCCODE"  ,"L" )
      lnDisc = ASCAN('laDisc',laData[78])
      lnDisc = IIF(lnDisc=0,1,ASUBSCRIPT(laDisc,lnDisc,1))
      RETURN
    ELSE
      FOR I = 1 TO ALEN(latarget,1)
        SELECT(lcWareFil)
        IF SEEK (latarget[I])
          REPLACE CdiscCode WITH laDisc[lnDisc,2]
        ELSE
          *-- Text Message : - Style: XXX is not assigned to location: XXX.
          *-- Text No      : - 42056.
          *-- Text Button  : - \<Add;\<Ignore
          *-- Button No    : -42018
          IF gfModalGen('QRM42056B42018','ALERT',lcMajor+"|"+latarget[I]) = 1
            glUpdated = .T.
            APPEND BLANK
            *B605958,1 ASH 06/02/2002 (Begin) Replace the gl_link with the data from screen not 'DEFDEF'
            *REPLACE STYLE     WITH lcMajor      ,;
                    dyelot    WITH SPACE(10)    ,;
                    cwarecode WITH latarget[I]  ,;
                    gl_link   WITH 'DEFDEF'     ,;
                    cstatus   WITH 'A'          ,;
                   cDiscCode WITH laDisc[lnDisc,2]
             REPLACE STYLE     WITH lcMajor      ,;
                     dyelot    WITH SPACE(10)    ,;
                     cwarecode WITH latarget[I]  ,;
                     gl_link   WITH laData[19]   ,;
                     cstatus   WITH 'A'          ,;
                     cDiscCode WITH laDisc[lnDisc,2]                   
             *B605958,1 ASH 06/02/2002 (End)                     
          ENDIF
        ENDIF  
      ENDFOR
    ENDIF
  ELSE
    SELECT(lcWareFil)
    GOTO TOP
    SCAN
      IF &lcwarefil..cWareCode = laData[71]
        REPLACE CdiscCode WITH laDisc[lnDisc,2]
      ENDIF  
    ENDSCAN
  ENDIF
ENDIF  
laData[78] = laDisc[lnDisc,2]
*-- select warehouse file & update filed cdiscCode
*-- End Of lfvDiscont
*!*************************************************************
*!*************************************************************
*! Name      : lfGetSclPo
*! Developer : Abd Elrahim Koura -KAM
*! Date      : 14/02/2001
*! Purpose   : get scale position
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSclPo()
*!*************************************************************
*!*B604203,1 KAM
*!*************************************************************
FUNCTION lfGetSclPo
DECLARE laItemSeg[1]
lcOldSelect=select()
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnExtScPos = IIF(lnExtScPos=0,laItemSeg[lnCount,4],lnExtScPos)
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetSclPo


*!******************************************************************
*! Name      : lfENBLPLN
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/28/2001
*! Purpose   : Enable the total plan field.
*!******************************************************************
*B804219,1 ADEL 05/28/2001 
*!******************************************************************
FUNCTION lfENBLPLN
PRIVATE llPlnRtrn
*--If all colors and not empty the prepack.
llPlnRtrn = .T.
IF !llAllColors AND !EMPTY(laData[7]) AND lcPrePack <> 'N'
  *--Enable the total plan field.
  llPlanSt = .T.
  *--Re-calculate the total plan upon selected prepqack
  llPlnRtrn = lfPLanClc()
ELSE
  llPlanSt = .F.
ENDIF  
RETURN llPlnRtrn
*!******************************************************************
*! Name      : lfPLanClc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 05/28/2001
*! Purpose   : Re-calculate the total plan upon selected prepqack
*!******************************************************************
*B804219,1 ADEL 05/28/2001 
*!******************************************************************
FUNCTION lfPLanClc
=SEEK("P"+laData[6]+laData[7],'Scale')
lnTotPre = scale.pp1+scale.pp2+scale.pp3+scale.pp4+scale.pp5+scale.pp6+scale.pp7+scale.pp8
PRIVATE lnScaleCnt

IF MOD(laData[39]/lnTotPre,1) <>0
  =gfModalGen('TRM32030B00000','ALERT',STR(Scale.PPTot,4))
* =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Total qty is not evenly divisible by the total prepack qty.")
  RETURN .F.
ENDIF
IF laData[39] <> 0
  lnPaln = 30
  FOR I=1 TO 8
    lcI=STR(I,1)
    IF scale.pp&lcI <> 0
      laData[lnPaln+I] = (laData[39] * scale.pp&lcI) / lnTotPre
      SHOW GET laData[lnPaln+I]
    ENDIF
  ENDFOR
ELSE
  lnPaln = 30
  FOR I=1 TO 8
    laData[lnPaln+I] = 0
    SHOW GET laData[lnPaln+I]
  ENDFOR
ENDIF
llcupdate=.T.
RETURN .T.
*!*************************************************************
*! Name      : lfObsColor
*! Developer : Abdou ElGendi _ (ABD)
*! Date      : 11/14/2001
*! Purpose   : Function to Check for the copyied colors if
*! Purpose   : status is obsolete for one or more color.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfObsColor()
*!*************************************************************
*C102466,1 
*!*************************************************************
*
FUNCTION lfObsColor

Private lnAlias , laColorObs , lcColorObs , llColorObs ;
        llAllClrOs , llOneClrOs , llNewClor

DECLARE laColorObs[1,2] ,laObsColor[1]
STORE '' TO lcColorObs
laColorObs[1,1] = 'LLOBSOLETE'
laColorObs[1,2] = 'llColorObs'
STORE .T. TO  llAllClrOs 
STORE .F. TO  llOneClrOs,laObsColor,llNewClor

lnAlias = SELECT(0)
SELECT STYLE
= SEEK(lcfrmstyle)

SCAN REST WHILE LEFT(Style,lnStyleWid) = lcfrmstyle
  *-- Store Emty to the next Variable.
  STORE .F. TO llColorObs
  *-- Get the color from the style file.
  lcColorObs = SUBSTR(STYLE,lnstylewid+2,lncolorwid)
  = gfRltFld(lcColorObs , @laColorObs, 'COLOR'))
  *-- check for this color if Obsolete.
  llAllClrOs = IIF(!llAllClrOs,llAllClrOs,llColorObs)
  llOneClrOs = IIF(llOneClrOs,llOneClrOs,llColorObs)
  IF llColorObs
    DECLARE laObsColor [ALEN(laObsColor)+IIF(!llNewClor,0,1)]
    laObsColor[ALEN(laObsColor)] = lcColorObs
    llNewClor = .T.
  ENDIF
ENDSCAN

DO CASE
  *-- Check if all color are obsolete yes , don't let the user to
  *-- Copy this style.
  CASE llAllClrOs
    *- Message Text   :- All Style colors are Obsolete, Can not copy this style.
    *- Message Number :- 42224.
    *- button message :- OK.
    *- button Number  :- 42001
    = gfModalGen('INM42224B42001','DIALOG','style'+'|'+'copy this style.')
    lcfrmstyle = ''
    _CUROBJ = OBJNUM(lcfrmstyle)

  *-- Check if at least one color was obsolete , let the user know that
  *-- By a message and don't copy this color for the new style.
  CASE llOneClrOs
    *- Message Text   :- One or more of the style colors will not be 
    *- Message Text   :- copied because their statuses are obsolete.
    *- Message Number :- 42225
    *- button message :- OK.
    *- button Number  :- 000000
    = gfModalGen('INM42225B000000','DIALOG','style')
  SELECT (lnAlias)
ENDCASE

RETURN

*-- End OF lfObsColor
*C102466,1 

*!*************************************************************
*! Name      : lfvStyClas
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Valid function of Style Classification menu
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvStyClas()
*!*************************************************************
*!C102452,1
FUNCTION lfvStyClas

lnAlias=SELECT()

= gfOpenFile(gcDataDir+'ICDEPTHD','Depthd','SH')
= gfOpenFile(gcDataDir+'ICCLSSTP','ICCLSSTP','SH')

lcDepDesc = ''
lcGrpDesc = ''
=lfFilPopUp()

IF EMPTY(laData[85])
  STORE 1   TO lnDept,lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
  STORE ' ' TO laData[10],laData[86],laData[87],laData[88],laData[89],laData[90]
  lcDepDesc = ' '
  lcGrpDesc = ' '
ENDIF

lcDptStat  = IIF(laScrMode[2],'DISABLE','ENABLE')

lcWindTitl = 'Style Classification'
DO (gcScrDir+gcWinAppl+"\ICSTYCLS.SPX")
ON KEY
POP KEY
SHOW GET lnGrp
SELECT(lnalias)
RETURN

*!*************************************************************
*! Name      : lfFilPopUp
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Update Popups in Style classification screen
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfFilPopUp()
*!*************************************************************
*!C102452,1
FUNCTION lfFilPopUp

*-Style Group array.
laGropInfo[1,01] = "CSTYGROUP"
laGropInfo[1,02] = "laGroup"
laGropInfo[1,03] = "lnGroup"
laGropInfo[1,04] = ""
laGropInfo[1,05] = .F.            
laGropInfo[1,06] = .T.            
laGropInfo[1,07] = "ICCLSSTP"     
laGropInfo[1,08] = "ICCLSSTP"     
laGropInfo[1,09] = "laData[85]"
laGropInfo[1,10] = "CSTYGROUP"  

*-Style Category array.
laCatgInfo[1,01] = "CSTYCATG"
laCatgInfo[1,02] = "laCatgry"
laCatgInfo[1,03] = "lnCatgry"
laCatgInfo[1,04] = ""
laCatgInfo[1,05] = .F.            
laCatgInfo[1,06] = .T.            
laCatgInfo[1,07] = "ICCLSSTP"     
laCatgInfo[1,08] = "ICCLSSTP"     
laCatgInfo[1,09] = "laData[85]+laData[10]"
laCatgInfo[1,10] = "CSTYCATG"  

*-Style Class array.
laClasInfo[1,01] = "CSTYCLASS"
laClasInfo[1,02] = "laClass"
laClasInfo[1,03] = "lnClass"
laClasInfo[1,04] = ""
laClasInfo[1,05] = .F.            
laClasInfo[1,06] = .T.            
laClasInfo[1,07] = "ICCLSSTP"     
laClasInfo[1,08] = "ICCLSSTP"     
laClasInfo[1,09] = "laData[85]+laData[10]+laData[86]"
laClasInfo[1,10] = "CSTYCLASS"  

*-Style Sub Class 1 array.
laCls1Info[1,01] = "CSTYSBCLAS"
laCls1Info[1,02] = "laSClas1"
laCls1Info[1,03] = "lnSClas1"
laCls1Info[1,04] = ""
laCls1Info[1,05] = .F.            
laCls1Info[1,06] = .T.            
laCls1Info[1,07] = "ICCLSSTP"     
laCls1Info[1,08] = "ICCLSSTP"     
laCls1Info[1,09] = "laData[85]+laData[10]+laData[86]+laData[87]"
laCls1Info[1,10] = "CSTYSBCLAS"  

*-Style Sub Class 2 array.
laCls2Info[1,01] = "CSTYSBCLS2"
laCls2Info[1,02] = "laSClas2"
laCls2Info[1,03] = "lnSClas2"
laCls2Info[1,04] = ""
laCls2Info[1,05] = .F.            
laCls2Info[1,06] = .T.            
laCls2Info[1,07] = "ICCLSSTP"     
laCls2Info[1,08] = "ICCLSSTP"     
laCls2Info[1,09] = "laData[85]+laData[10]+laData[86]+laData[87]+laData[88]"
laCls2Info[1,10] = "CSTYSBCLS2"  

*-Style Sub Class 3 array.
laCls3Info[1,01] = "CSTYSBCLS3"
laCls3Info[1,02] = "laSClas3"
laCls3Info[1,03] = "lnSClas3"
laCls3Info[1,04] = ""
laCls3Info[1,05] = .F.            
laCls3Info[1,06] = .T.            
laCls3Info[1,07] = "ICCLSSTP"     
laCls3Info[1,08] = "ICCLSSTP"     
laCls3Info[1,09] = "laData[85]+laData[10]+laData[86]+laData[87]+laData[88]+laData[89]"
laCls3Info[1,10] = "CSTYSBCLS3"  

DIMENSION laDept[1]
laDept[1] = 'N/A'
SELECT ICDEPTHD
SET ORDER TO TAG Depthd
LOCATE 
SCAN 
  lnFound = ASCAN(laDept,ICDEPTHD.Dept)
  IF lnFound = 0
    lnI = ALEN(laDept,1)+1
    DIMENSION laDept[lnI]
    laDept[lnI] = ICDEPTHD.Dept
  ENDIF  
ENDSCAN

*B607253,1 HBG 12/05/2003 Get the Group from ICDEPTHD file not from ICCLSSTP ftile [Begin]
SELECT ICDEPTHD
SET ORDER TO TAG Depthd
=SEEK(laData[85])
SCAN REST WHILE dept+cstygroup = laData[85]
  lnFound = ASCAN(laGroup,ICDEPTHD.cstygroup)
  IF lnFound = 0
    lnI = ALEN(laGroup,1)+1
    DIMENSION laGroup[lnI,2]
    laGroup[lnI,1] = gfCodDes(ICDEPTHD.cstygroup,'CSTYGROUP')
    laGroup[lnI,2] = ICDEPTHD.cstygroup
  ENDIF  
ENDSCAN
*B607253,1 [End]

DO CASE 
  CASE laScrMode[2] OR laScrMode[3] OR !EMPTY(lcfrmstyle)  && If view mode or edit mode
       
    lnFound = ASCAN(laDept,laData[85])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laDept,lnFound,1)
      STORE lnFound TO lnDept
    ELSE  
      STORE 1 TO lnDept
    ENDIF  
    IF SEEK(laDept[lnDept],'ICDEPTHD')
      lcDepDesc = ICDEPTHD.cdeptdesc
    ELSE
      lcDepDesc = ''  
    ENDIF
    
    *B607253,1 HBG 12/05/2003 No need for this code because we Get the Group from ICDEPTHD file directly[Begin]
    *= gfwCodePop(@laGropInfo, "CSTYGROUP","L")
    *B607253,1 [End]
    lnFound = ASCAN(laGroup,laData[10])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laGroup,lnFound,1)
      STORE lnFound TO lnGroup
    ELSE  
      STORE 1 TO lnGroup
    ENDIF  
    lcGrpDesc = IIF(laGroup[lnGroup,1]= 'N/A',' ',laGroup[lnGroup,1])
    
    = gfwCodePop(@laCatgInfo, "CSTYCATG","L")
    lnFound = ASCAN(laCatgry,laData[86])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laCatgry,lnFound,1)
      STORE lnFound TO lnCatgry
    ELSE  
      STORE 1 TO lnCatgry
    ENDIF  

    = gfwCodePop(@laClasInfo, "CSTYCLASS","L")
    lnFound = ASCAN(laClass,laData[87])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laClass,lnFound,1)
      STORE lnFound TO lnClass
    ELSE  
      STORE 1 TO lnClass
    ENDIF  

    = gfwCodePop(@laCls1Info, "CSTYSBCLAS","L")
    lnFound = ASCAN(laSClas1,laData[88])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSClas1,lnFound,1)
      STORE lnFound TO lnSClas1
    ELSE  
      STORE 1 TO lnSClas1
    ENDIF  

    = gfwCodePop(@laCls2Info, "CSTYSBCLS2","L")   
    lnFound = ASCAN(laSClas2,laData[89])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSClas2,lnFound,1)
      STORE lnFound TO lnSClas2
    ELSE  
      STORE 1 TO lnSClas2
    ENDIF  

    = gfwCodePop(@laCls3Info, "CSTYSBCLS3","L")  
    lnFound = ASCAN(laSClas3,laData[90])
    IF lnFound > 0
      lnFound = ASUBSCRIPT(laSClas3,lnFound,1)
      STORE lnFound TO lnSClas3
    ELSE  
      STORE 1 TO lnSClas3
    ENDIF  
    
    
  CASE laScrMode[4]      && If add mode
    IF lnDept = 1
      = gfwCodePop(@laGropInfo,"CSTYGROUP","N")
      = gfwCodePop(@laCatgInfo,"CSTYCATG","N")
      = gfwCodePop(@laClasInfo,"CSTYCLASS","N")
      = gfwCodePop(@laCls1Info,"CSTYSBCLAS","N")
      = gfwCodePop(@laCls2Info,"CSTYSBCLS2","N")   
      = gfwCodePop(@laCls3Info,"CSTYSBCLS3","N")       
    ELSE
      IF SEEK(laDept[lnDept],'ICDEPTHD')
        lcDepDesc = ICDEPTHD.cdeptdesc
      ELSE
        lcDepDesc = ''  
      ENDIF   
      lcGrpDesc = laGroup[lnGroup,1]  
    ENDIF
ENDCASE    

*!*************************************************************
*! Name      : lfEnablPop
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Enable and disable objects on Style classification screen
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfEnablPop()
*!*************************************************************
*!C102452,1 
FUNCTION lfEnablPop
PARAMETERS lnCurrent,lnNext

DO CASE 
  CASE lnNext = 6
    IF lcOldValue <> lnCurrent
      STORE 1 TO lnGroup,lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
      STORE ' ' TO laGroup[lnGroup,2],laCatgry[lnCatgry,2],laClass[lnClass,2],;
                   laSClas1[lnSClas1,2],laSClas2[lnSClas2,2],laSClas3[lnSClas3,2]

      IF SEEK(laData[85],'ICDEPTHD')
        lcDepDesc = ICDEPTHD.cdeptdesc
      ELSE
        lcDepDesc = ''  
      ENDIF

      SHOW GET lcDepDesc

      SHOW GET lnGroup
      SHOW GET lnCatgry
      SHOW GET lnClass
      SHOW GET lnSClas1
      SHOW GET lnSClas2
      SHOW GET lnSClas3
      =lfRefresh()    
    ENDIF
  CASE lnNext = 5
    IF lcOldValue <> lnCurrent
      STORE 1 TO lnCatgry,lnClass,lnSClas1,lnSClas2,lnSClas3
      STORE ' ' TO laCatgry[lnCatgry,2],laClass[lnClass,2],laSClas1[lnSClas1,2],;
                   laSClas2[lnSClas2,2],laSClas3[lnSClas3,2]    

      lcGrpDesc = laGroup[lnGroup,1]

      SHOW GET lcGrpDesc
    
      laGrp[lnGrp,1]  = laGroup[lnGroup,1]
      laGrp[lnGrp,2]  = laGroup[lnGroup,2]
    
      laUsrFields[8,6]  = ' '
      laUsrFields[9,6]  = ' '
      laUsrFields[10,6] = ' '
      laUsrFields[11,6] = ' '
      laUsrFields[12,6] = ' '
    
      SHOW GET lnGrp
      SHOW GET lnCatgry
      SHOW GET lnClass
      SHOW GET lnSClas1
      SHOW GET lnSClas2
      SHOW GET lnSClas3
      =lfRefresh()    
    ENDIF  
  CASE lnNext = 4
    IF lcOldValue <> lnCurrent  
      laUsrFields[8,6] = laCatgry[lnCatgry,2]
      STORE 1 TO lnClass,lnSClas1,lnSClas2,lnSClas3
      STORE ' ' TO laClass[lnClass,2],laSClas1[lnSClas1,2],laSClas2[lnSClas2,2],;
                   laSClas3[lnSClas3,2]    
    
      laUsrFields[9,6]  = ' '
      laUsrFields[10,6] = ' '
      laUsrFields[11,6] = ' '
      laUsrFields[12,6] = ' '

      SHOW GET lnClass
      SHOW GET lnSClas1
      SHOW GET lnSClas2
      SHOW GET lnSClas3
 
    ENDIF  
  
  CASE lnNext = 3
    IF lcOldValue <> lnCurrent    
      laUsrFields[9,6] = laClass[lnClass,2]
      STORE 1 TO lnSClas1,lnSClas2,lnSClas3
      STORE ' ' TO laSClas1[lnSClas1,2],laSClas2[lnSClas2,2],laSClas3[lnSClas3,2]    

      laUsrFields[10,6] = ' '
      laUsrFields[11,6] = ' '
      laUsrFields[12,6] = ' '
  
      SHOW GET lnSClas1
      SHOW GET lnSClas2
      SHOW GET lnSClas3
    ENDIF

  CASE lnNext = 2
    IF lcOldValue <> lnCurrent        
      laUsrFields[10,6] = laSClas1[lnSClas1,2]
      STORE 1 TO lnSClas2,lnSClas3
      STORE ' ' TO laSClas2[lnSClas2,2],laSClas3[lnSClas3,2]        

      laUsrFields[11,6] = ' '
      laUsrFields[12,6] = ' '
 
      SHOW GET lnSClas2
      SHOW GET lnSClas3
    ENDIF       
  CASE lnNext = 1
    IF lcOldValue <> lnCurrent            
      laUsrFields[11,6] = laSClas2[lnSClas2,2]  
      STORE 1 TO lnSClas3
      STORE ' ' TO laSClas3[lnSClas3,2]            
 
      laUsrFields[12,6] = ' '
 
      SHOW GET lnSClas3
    ENDIF        
  CASE lnNext = 0
    laUsrFields[12,6] = laSClas3[lnSClas3,2]  
    
ENDCASE 


*!*************************************************************
*! Name      : lfGetGrp
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of group popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetGrp()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetGrp

DIMENSION laGroup[1,2]
laGroup[1,1] = 'N/A'
laGroup[1,2] = ' '

*B607253,1 HBG 12/05/2003 Get the Group from ICDEPTHD file not from ICCLSSTP ftile [Begin]
*SELECT ICCLSSTP
*SET ORDER TO TAG ICCLSSTP
*IF SEEK(laDept[lnDept])
*  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
*                  laDept[lnDept]
*    lnFound = ASCAN(laGroup,ICCLSSTP.cstygroup)
*    IF lnFound = 0
*      lnI = ALEN(laGroup,1)+1
*      DIMENSION laGroup[lnI,2]
*      laGroup[lnI,1] = gfCodDes(ICCLSSTP.cstygroup,'CSTYGROUP')
*      laGroup[lnI,2] = ICCLSSTP.cstygroup
*    ENDIF  
*  ENDSCAN
*ENDIF  
SELECT ICDEPTHD
SET ORDER TO TAG Depthd
=SEEK(laDept[lnDept])
SCAN REST WHILE dept+cstygroup = laDept[lnDept]
  lnFound = ASCAN(laGroup,ICDEPTHD.cstygroup)
  IF lnFound = 0
    lnI = ALEN(laGroup,1)+1
    DIMENSION laGroup[lnI,2]
    laGroup[lnI,1] = gfCodDes(ICDEPTHD.cstygroup,'CSTYGROUP')
    laGroup[lnI,2] = ICDEPTHD.cstygroup
  ENDIF  
ENDSCAN
*B607253,1 [End]

lnFound = ASCAN(laGroup,laData[10])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laGroup,lnFound,1)
  STORE lnFound TO lnGroup
ELSE  
  STORE 1 TO lnGroup
ENDIF  
lcGrpDesc = IIF(laGroup[lnGroup,1]= 'N/A',' ',laGroup[lnGroup,1])

SHOW GET lnGroup

*!*************************************************************
*! Name      : lfGetCatg
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Category popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetCatg()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetCatg

DIMENSION laCatgry[1,2]
laCatgry[1,1] = 'N/A'
laCatgry[1,2] = ' '

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(laDept[lnDept]+laGroup[lnGroup,2])
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  laDept[lnDept]+laGroup[lnGroup,2]
    lnFound = ASCAN(laCatgry,ICCLSSTP.cstycatg)
    IF lnFound = 0
      lnI = ALEN(laCatgry,1)+1
      DIMENSION laCatgry[lnI,2]
      laCatgry[lnI,1] = gfCodDes(ICCLSSTP.cstycatg,'CSTYCATG')
      laCatgry[lnI,2] = ICCLSSTP.cstycatg
    ENDIF  
  ENDSCAN
ENDIF  

lnFound = ASCAN(laCatgry,laData[86])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laCatgry,lnFound,1)
  STORE lnFound TO lnCatgry
ELSE  
  STORE 1 TO lnCatgry
ENDIF  

SHOW GET lnCatgry

*!*************************************************************
*! Name      : lfGetClas
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Class popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetClas

DIMENSION laClass[1,2]
laClass[1,1] = 'N/A'
laClass[1,2] = ' '

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2])
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]
    lnFound = ASCAN(laClass,ICCLSSTP.cstyclass)
    IF lnFound = 0
      lnI = ALEN(laClass,1)+1
      DIMENSION laClass[lnI,2]
      laClass[lnI,1] = gfCodDes(ICCLSSTP.cstyclass,'CSTYCLASS')
      laClass[lnI,2] = ICCLSSTP.cstyclass
    ENDIF  
  ENDSCAN
ENDIF  

lnFound = ASCAN(laClass,laData[87])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laClass,lnFound,1)
  STORE lnFound TO lnClass
ELSE  
  STORE 1 TO lnClass
ENDIF  

SHOW GET lnClass

*!*************************************************************
*! Name      : lfGetClas1
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Sub Class 1 popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas1()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetClas1

DIMENSION laSClas1[1,2]
laSClas1[1,1] = 'N/A'
laSClas1[1,2] = ' '

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2])
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2]
    lnFound = ASCAN(laSClas1,ICCLSSTP.cstysbclas)
    IF lnFound = 0
      lnI = ALEN(laSClas1,1)+1
      DIMENSION laSClas1[lnI,2]
      laSClas1[lnI,1] = gfCodDes(ICCLSSTP.cstysbclas,'CSTYSBCLAS')
      laSClas1[lnI,2] = ICCLSSTP.cstysbclas
    ENDIF  
  ENDSCAN
ENDIF  

lnFound = ASCAN(laSClas1,laData[88])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laSClas1,lnFound,1)
  STORE lnFound TO lnSClas1
ELSE  
  STORE 1 TO lnSClas1
ENDIF  

SHOW GET lnSClas1

*!*************************************************************
*! Name      : lfGetClas2
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Sub Class 2 popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas2()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetClas2

DIMENSION laSClas2[1,2]
laSClas2[1,1] = 'N/A'
laSClas2[1,2] = ' '

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2]+;
        laSClas1[lnSClas1,2])
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2]+;
                  laSClas1[lnSClas1,2]
    lnFound = ASCAN(laSClas2,ICCLSSTP.cstysbcls2)
    IF lnFound = 0
      lnI = ALEN(laSClas2,1)+1
      DIMENSION laSClas2[lnI,2]
      laSClas2[lnI,1] = gfCodDes(ICCLSSTP.cstysbcls2,'CSTYSBCLS2')
      laSClas2[lnI,2] = ICCLSSTP.cstysbcls2
    ENDIF  
  ENDSCAN
ENDIF  

lnFound = ASCAN(laSClas2,laData[89])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laSClas2,lnFound,1)
  STORE lnFound TO lnSClas2
ELSE  
  STORE 1 TO lnSClas2
ENDIF  

SHOW GET lnSClas2

*!*************************************************************
*! Name      : lfGetClas3
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Sub Class 3 popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas3()
*!*************************************************************
*!C102452,1 
FUNCTION lfGetClas3

DIMENSION laSClas3[1,2]
laSClas3[1,1] = 'N/A'
laSClas3[1,2] = ' '

SELECT ICCLSSTP
SET ORDER TO TAG ICCLSSTP
IF SEEK(laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2]+;
        laSClas1[lnSClas1,2]+laSClas2[lnSClas2,2])
  SCAN REST WHILE dept+cstygroup+cstycatg+cstyclass+cstysbclas+cstysbcls2+cstysbcls3 =;
                  laDept[lnDept]+laGroup[lnGroup,2]+laCatgry[lnCatgry,2]+laClass[lnClass,2]+;
                  laSClas1[lnSClas1,2]+laSClas2[lnSClas2,2]
    lnFound = ASCAN(laSClas3,ICCLSSTP.cstysbcls3)
    IF lnFound = 0
      lnI = ALEN(laSClas3,1)+1
      DIMENSION laSClas3[lnI,2]
      laSClas3[lnI,1] = gfCodDes(ICCLSSTP.cstysbcls3,'CSTYSBCLS3')
      laSClas3[lnI,2] = ICCLSSTP.cstysbcls3
    ENDIF  
  ENDSCAN
ENDIF  

lnFound = ASCAN(laSClas3,laData[90])
IF lnFound > 0
  lnFound = ASUBSCRIPT(laSClas3,lnFound,1)
  STORE lnFound TO lnSClas3
ELSE  
  STORE 1 TO lnSClas3
ENDIF  

SHOW GET lnSClas3


*!*************************************************************
*! Name      : lfGetClas3
*! Developer : HEND GHANEM (HBG)
*! Date      : 25/10/2001
*! Purpose   : Get data of Sub Class 3 popup
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfGetClas3()
*!*************************************************************
*!C102452,1 
FUNCTION lfSavUserF

IF llSvUsrFld
  lcStyle = SUBSTR(STYLE.STYLE,1,lnstylewid)
  lcAlias = ALIAS()
  SELECT STYLE
  lcOrder = ORDER()
  SET ORDER TO STYLE
  IF SEEK(lcStyle)
    SCAN REST WHILE SUBSTR(STYLE,1,lnstylewid) = lcStyle
      REPLACE cstycatg   WITH laCatgry[lnCatgry,2],;
              cstyclass  WITH laClass[lnClass,2]  ,;
              cstysbclas WITH laSClas1[lnSClas1,2],;
              cstysbcls2 WITH laSClas2[lnSClas2,2],;
              cstysbcls3 WITH laSClas3[lnSClas3,2]
    ENDSCAN 
    =SEEK(lcStyle)
  ENDIF
  SET ORDER TO &lcOrder
  SELECT (lcAlias)
  llSvUsrFld = .F.
ENDIF  

*!*************************************************************
*! Name      : lfvSkubrw
*! Developer : Ashraf Sherif (ASH)
*! Date      : 02/12/2002
*! Purpose   : Browse SKU's related to the selected style.
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfvSkubrw()
*!*************************************************************
*!C102528,1 

Function lfvSkubrw

lnAlias=SELECT()
lcPackKey = 'S'+lcMajor+lcNMKey
laOpFile[18]=gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLNST','SH')

SELECT SPCK_LIN
IF !SEEK( lcPackKey)
  *--No Skus found for this "+IIF(llallcolors,"Style","Style\Color")+"."
  =gfModalGen('TRM42041B42001','DIALOG','Skus'+'|'+IIF(llAllColors,"Style","Style\Color"))
ELSE
  lcWindTitl = 'Style Skus'
  PRIVATE lcBrFields
  lcBrfields = "ACCOUNT:H='Acct#'," +;
                "x=IIF(qty1=1,Scale.Sz1,IIF(qty2=1,Scale.Sz2,IIF(qty3=1,;
                 Scale.Sz3,IIF(qty4=1,Scale.Sz4,IIF(qty5=1,Scale.Sz5,;
                 IIF(qty6=1,Scale.Sz6,IIF(qty7=1,Scale.Sz7,IIF(qty8=1,;
                 Scale.Sz8,'')))))))) :H='Sizes',"+;    
                 "PACK_ID :16 :H='Sku #'"
 
  =ARIABROW([lcPackKey],'Style Skus',7.4, gnbrhscol1, 21, gnbrhscol2,'',;
		'Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ENDIF

IF USED('Spck_Lin') AND laOpFile[18]
  USE IN Spck_Lin
ENDIF
SELECT(lnAlias)
RETURN


*:**************************************************************************
*:* Name        : lfInTrns                                        *B604357,1
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/29/2002
*:* Purpose     : Calculate InTransit related to reciving
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfInTrns()
*:***************************************************************************
FUNCTION lfInTrns

IF !FILE(gcWorkDir+lcTmpPosln+'.DBF')
  COPY STRUCTURE TO (gcWorkDir+lcTmpPosln) WITH CDX
ENDIF
IF !USED(lcTmpPosln)
  =gfOpenFile(gcWorkDir+lcTmpPosln,'POSLNS','EX')
ENDIF

SELECT (lcTmpPosln)
ZAP
SET ORDER TO POSLNS

SELECT POSLN
=SEEK(lcexkey)
SCAN REST WHILE &lcwcondt FOR &lcfcondt
  SCATTER MEMVAR 
  INSERT INTO (lcTmpPosln) FROM MEMVAR
ENDSCAN
SELECT (lcTmpPosln)
GO TOP
*B608056,1 MHM 04/22/2007 Fix a bug that deducting recived QTY from Intranset [Start]
*                         in case of interlocation PO
*SCAN
*  IF SEEK(STYLE+CSTYTYPE+PO+STR(LINENO,6)+'2','POSLN') AND TRANCD = '6'
*    SELECT POSLN
*    SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = ;
*                    &lcTmpPosln..STYLE+&lcTmpPosln..CSTYTYPE+;
*                    &lcTmpPosln..PO+STR(&lcTmpPosln..LINENO,6)+'2'
*      SELECT (lcTmpPosln)
*      REPLACE QTY1 WITH MAX(QTY1-POSLN.QTY1,0),;
*              QTY2 WITH MAX(QTY2-POSLN.QTY2,0),;
*              QTY3 WITH MAX(QTY3-POSLN.QTY3,0),;
*              QTY4 WITH MAX(QTY4-POSLN.QTY4,0),;
*              QTY5 WITH MAX(QTY5-POSLN.QTY5,0),;
*              QTY6 WITH MAX(QTY6-POSLN.QTY6,0),;
*              QTY7 WITH MAX(QTY7-POSLN.QTY7,0),;
*              QTY8 WITH MAX(QTY8-POSLN.QTY8,0),;
*              TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
*    ENDSCAN
*  ENDIF
*ENDSCAN
*B608056,1 MHM 04/22/2007 [END]

*-- end of lfInTrns.
*!*************************************************************
*! Name      : lfCalculte
*! Developer : Abdou ElGendi _ (ABD)
*! Date      : 06/30/2003
*! Purpose   : Function to calculate the neede fields.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfCalculte()
*!*************************************************************
*! B607286,1
*!*************************************************************
* B607286,1 [Begin]
FUNCTION lfCalculte
Parameter lcWare_Cod , lcCalctFld
Private lnalias , lnFild_Qty
lnFild_Qty = 0

lnAlias = SELECT(0)

SELECT STYDYE
IF SEEK(lcMajor+lcnmkey)
  SCAN REST WHILE Style=lcMajor+lcNMKey FOR EMPTY(dyelot) .AND. cWareCode = lcWare_Cod
    lnFild_Qty = lnFild_Qty + &lcCalctFld
  ENDSCAN
ENDIF

SELECT (lnAlias)

RETURN lnFild_Qty

*-- End Of lfCalculte
* B607286,1 [End]


*B126312,1 EIH 03/13/2005 No edit costing in style screen when cost method 'S' and no adjustment reasons in codes.[Begin]
*!*************************************************************
*! Name      : lpEditCost
*! Developer : Ehab Ismail Hamed _ (EIH)
*! Date      : 03/13/2005
*! Purpose   : No edit costing in style screen when cost method 'S' and no adjustment reasons in codes .
*!*************************************************************
*! Calls     : gfModalGen()
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpEditCost()
*!*************************************************************
*! B126312,1
*!*************************************************************
PROCEDURE lpEditCost
IF laScrMode[3] AND lcCstMth = 'S' AND !llRsonCode AND lnActFolder = 5
  *--You must have Adjustment Reason Codes to edit costing .
  =gfModalGen('INM42264B42001','DIALOG')
  SHOW GET laData[2] DISABLE
  SHOW GET laData[27] DISABLE
  SHOW GET lnPrExRate DISABLE
  SHOW GET laData[83] DISABLE
  SHOW GET laData[45] DISABLE
  SHOW GET laData[46] DISABLE
  SHOW GET laData[47] DISABLE
  SHOW GET laData[48] DISABLE
  SHOW GET laData[49] DISABLE
  SHOW GET laData[28] DISABLE
  SHOW GET lnChExRate DISABLE
  SHOW GET laData[84] DISABLE
  SHOW GET lnFCostPr2 DISABLE
  SHOW GET lnFCostPr3 DISABLE
  SHOW GET lnFCostPr4 DISABLE
  SHOW GET lnFCostPr5 DISABLE
  SHOW GET laData[65] DISABLE
  SHOW GET laData[66] DISABLE
  SHOW GET lnCostVal0 DISABLE
  SHOW GET lnCostVal1 DISABLE
  SHOW GET lnCostVal2 DISABLE
  SHOW GET lnCostVal3 DISABLE
  SHOW GET lnCostVal4 DISABLE
  SHOW GET lnCostVal5 DISABLE
  SHOW GET lnCostPrc0 DISABLE
  SHOW GET lnCostPrc2 DISABLE
  SHOW GET lnCostPrc3 DISABLE
  SHOW GET lnCostPrc4 DISABLE
  SHOW GET lnCostPrc5 DISABLE
ENDIF
*B126312,1 EIH 03/13/2005 [End]
*!*************************************************************