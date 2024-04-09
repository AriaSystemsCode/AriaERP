using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Aria.Data.BusinessObject;
using BunnyBear;
using Aria.Data;
using Aria.Environment;
using Aria.DataTypes;
using System.Diagnostics;


public partial class OrderSummaryControl : System.Web.UI.UserControl
{

    LinesDataTableController lines;

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        Session["SummaryControl"] = this;

        SaveBtn.Attributes.Add("onclick", "Loading(' Saving...');");
        RemoveBtn.Attributes.Add("onclick", "Loading(' Deleting...');");
    }

    protected void Page_Load(object sender, EventArgs e)
    {

        lines = ((OrderDataTableController)Session["Order"]).lines;

        Update();
    }

    public void Update()
    {
        if (lines.select().Rows.Count > 0)
        {
            LinesGrid.DataSource = lines.select();
            LinesGrid.Columns[0].Visible = false;
            LinesGrid.Columns[1].Visible = true;
        }
        else
        {
            LinesDataTableController tempLines = new LinesDataTableController();
            DataTable tempTable = tempLines.select();
            tempTable.Rows.Add(tempTable.NewRow());
            LinesGrid.DataSource = tempTable;
            LinesGrid.Columns[0].Visible = true;
            LinesGrid.Columns[1].Visible = false;
        }
        UpdateTotals();
        LinesGrid.DataBind();
    }


    protected void RemoveBtn_Click(object sender, EventArgs e)
    {
        bool linesSelected = false;

        for (int i = LinesGrid.Rows.Count - 1; i >= 0; i--)
        {
            GridViewRow row = LinesGrid.Rows[i];
            CheckBox cb = (CheckBox)row.FindControl("LineSelectorChkBx");
            if (cb != null && cb.Checked)
            {
                linesSelected = true;
                lines.delete(i);
            }
        }

        if (linesSelected)
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);
            Update();
        }
        else
        {
            ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('No lines selected!')", true);
        }
    }

    public static bool isFlaged(DataTable criteriaTable, DataTable table, string prefix, Hashtable orgColumnMap)
    {
        Hashtable columnMap = new Hashtable();

        foreach(DictionaryEntry entry in orgColumnMap)
        {
            columnMap[entry.Value] = entry.Key;
        }
        
         
        Hashtable result = new Hashtable();
        
        for (int i = 0; i < criteriaTable.Rows.Count; i++)
        {
            string fullName = criteriaTable.Rows[i]["Crittyp"].ToString().Trim();
            if (fullName.StartsWith(prefix + "."))
            {
                string[] sArray = fullName.Split(".".ToCharArray());
                string targetColName = sArray[1];

                for (int j = 0; j < table.Rows.Count; j++)
                {
                    bool compare = false;

                    if (table.Rows[j][columnMap[targetColName.Trim().ToLower()].ToString()] != DBNull.Value)
                    {
                        if (table.Columns[columnMap[targetColName.Trim().ToLower()].ToString()].DataType.ToString() == typeof(string).ToString())
                        {
                            string value1 = table.Rows[j][columnMap[targetColName.Trim().ToLower()].ToString()].ToString().Trim();
                            string value2 = criteriaTable.Rows[i]["CritVal"].ToString().Trim();
                            compare = value1 == value2;
                        }
                        else if (table.Columns[columnMap[targetColName.Trim().ToLower()].ToString()].DataType.ToString() == typeof(DateTime).ToString())
                        {
                            DateTime value1 = Convert.ToDateTime(table.Rows[j][columnMap[targetColName.Trim().ToLower()].ToString()].ToString().Trim());
                            string[] range = criteriaTable.Rows[i]["CritVal"].ToString().Trim().Split('|');
                            DateTime valueFrom = Convert.ToDateTime(range[0]);
                            DateTime valueTo = Convert.ToDateTime(range[1]);
                            compare = value1.Date >= valueFrom.Date && value1.Date <= valueTo.Date;
                        }
                        else
                        {
                            double value1 = Convert.ToDouble(table.Rows[j][columnMap[targetColName.Trim().ToLower()].ToString()].ToString().Trim());
                            double value2 = Convert.ToDouble((criteriaTable.Rows[i]["CritVal"].ToString().Trim()));
                            compare = value1 < value2;
                        }
                    }

                    if (result.Contains(targetColName))
                    {
                        if (compare)
                        {
                            result[targetColName] = true;
                        }
                    }
                    else
                    {
                        result.Add(targetColName, compare);
                    }
                }
            }
        }

        bool falg = true;
        foreach(object item in result.Values)
        {
            falg &= (bool)item;
        }

        return falg;
    }

    public bool CheckPO()
    {
        Session["PODoublicated"] = false;
        if (((OrderDataTableController)Session["Order"]).header.Rows[0]["CustomerPO"].ToString().TrimEnd().Length > 0)
        {
            AriaDbCommand command = new AriaDbCommand("", new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data);
            command.CommandText = "SELECT ORDER As SalesOrderNumber FROM ORDHDR WHERE ACCOUNT+UPPER(CUSTPO)+CORDTYPE+ORDER = '" +
                                    Session["AccountID"].ToString().PadRight(5) +
                                        (((OrderDataTableController)Session["Order"]).header.Rows[0]["CustomerPO"]).ToString().PadRight(15) + "'";

            AriaDataProvider provider = new AriaDataProvider();
            DataTable poOrder = provider.GetDataTable(command);
            if (poOrder.Rows.Count > 0 &&
                (((OrderDataTableController)Session["Order"]).header.Rows[0]["SalesOrderNumber"]).ToString().TrimEnd() !=
                    poOrder.Rows[0]["SalesOrderNumber"].ToString().TrimEnd())
            {
                Session["PODoublicated"] = true;
            }
        }
        return true;
    }


    protected void SaveBtn_Click(object sender, EventArgs e)
    {
        try
        {

            Control CheckSalesRepInformationControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("SalesRepsInformationControl1");
            CheckSalesRepInformationControl1.GetType().GetMethod("SaveFields").Invoke(CheckSalesRepInformationControl1, null);

            if (((OrderDataTableController)Session["Order"]).header.Rows[0]["SalesRep2"] != null && ((OrderDataTableController)Session["Order"]).header.Rows[0]["SalesRep2"].ToString().Trim() == "*")
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please select sales rep.!');", true);
                return;
            }

            Control CheckGeneralControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("GeneralControl1");
            CheckGeneralControl1.GetType().GetMethod("SaveFields").Invoke(CheckGeneralControl1, null);

            if (Convert.ToDateTime(((OrderDataTableController)Session["Order"]).header.Rows[0]["CompleteDate"]) < Convert.ToDateTime(((OrderDataTableController)Session["Order"]).header.Rows[0]["StartDate"]))
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Complete date is less than start date!');", true);
                return;
            }

            if (((OrderDataTableController)Session["Order"]).lines.select().Rows.Count == 0)
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('No added lines!')", true);
                return;
            }

            if (Session["Saving"] != null)
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Please wait!')", true);
                return;
            }

            AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

            string sequence = "";

            if (Session["PageMode"].ToString().Equals("Add") || Session["PageMode"].ToString().Equals("Copy"))
            {
                Application.Lock();
                try
                {
                    sequence = adapter.GetNewSequence("", "", "99", "CRMORDER  ");
                }
                catch (Exception ex)
                {
                    Application.UnLock();
                    string errorMessage = "Error: " + ex.Message;
                    if (ex.InnerException != null)
                    {
                        errorMessage += "\n\r Error Detail:" + ex.InnerException.Message;
                    }
                    errorMessage = errorMessage.Replace("'", "");

                    ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);

                    EventLog.WriteEntry("Aria.CRM.SaveGetSequence", errorMessage + ":" + ex.StackTrace, EventLogEntryType.Information); 
                }
                Application.UnLock();

                if (Session["LocationCode"] != null)
                {
                    ((OrderDataTableController)Session["Order"]).header.Rows[0]["LocationCode"] = Session["LocationCode"];
                }
                else
                {
                    ((OrderDataTableController)Session["Order"]).header.Rows[0]["LocationCode"] = "";
                }

                string defaultTermCode = adapter.GetDefaultCode("", "", "99", "CTERMCODE");
                if (defaultTermCode != null)
                {
                    ((OrderDataTableController)Session["Order"]).header.Rows[0]["PaymentTermsCode"] = defaultTermCode;
                }
            }
            else
            {
                sequence = ((OrderDataTableController)Session["Order"]).header.Rows[0]["SalesOrderNumber"].ToString();
            }

            Session["sequence"] = sequence;

            AriaDataProvider dataProvider = new AriaDataProvider();

            DataTable orderHeader = ((OrderDataTableController)Session["Order"]).header;
            orderHeader.Rows[0]["LastLineNumber"] = Session["LastLineNumber"];
            //orderHeader.Columns.Remove("CustomerName");
            //orderHeader.Columns.Remove("SalesRep1Name");
            //orderHeader.Columns.Remove("SalesRep2Name");
            orderHeader.Rows[0]["Enteredviaweb"] = true;
            orderHeader.Rows[0]["SalesOrderNumber"] = sequence;
            orderHeader.Rows[0]["CustomerCode"] = Session["AccountID"];

            orderHeader.Rows[0]["BulkOrder"] = "N";
            orderHeader.Rows[0]["ReOrder"] = "N";
            orderHeader.Rows[0]["Status"] = "B";

            if (Session["StoreID"].ToString() == "Multi")
            {
                orderHeader.Rows[0]["Multi"] = "Y";
            }
            else
            {
                orderHeader.Rows[0]["Multi"] = "N";
            }

            Control AddressOrderControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("AddressOrderControl1");
            Hashtable hashtable = (Hashtable)AddressOrderControl1.GetType().GetMethod("SaveFields").Invoke(AddressOrderControl1, null);

            Control OrderInfoControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("OrderInfoControl1");
            hashtable = (Hashtable)OrderInfoControl1.GetType().GetMethod("SaveFields").Invoke(OrderInfoControl1, null);

            Control ContactInfromationControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("ContactInfromationControl1");
            hashtable = (Hashtable)ContactInfromationControl1.GetType().GetMethod("SaveFields").Invoke(ContactInfromationControl1, null);


            Control GeneralControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("GeneralControl1");
            hashtable = (Hashtable)GeneralControl1.GetType().GetMethod("SaveFields").Invoke(GeneralControl1, null);

            Control SalesRepsInformationControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("SalesRepsInformationControl1");
            hashtable = (Hashtable)SalesRepsInformationControl1.GetType().GetMethod("SaveFields").Invoke(SalesRepsInformationControl1, null);

            Control NoteControl1 = Parent.Parent.Parent.Parent.Parent.FindControl("NoteControl1");
            hashtable = (Hashtable)NoteControl1.GetType().GetMethod("SaveFields").Invoke(NoteControl1, null);

            if (Session["PageMode"].ToString().Equals("Add"))
            {
                orderHeader.Rows[0]["Insurance"] = ((DataRow)Session["ActiveCustomerMainAccount"])["Insurance"];
                orderHeader.Rows[0]["GLLinkCode"] = "DEFDEF";
                orderHeader.Rows[0]["GLSalesLinkCode"] = "DEF";
                orderHeader.Rows[0]["Discount"] = ((DataRow)Session["ActiveCustomerMainAccount"])["Discount"];
                orderHeader.Rows[0]["Priority"] = ((DataRow)Session["ActiveCustomerMainAccount"])["Priority"];
                orderHeader.Rows[0]["FactorCode"] = ((DataRow)Session["ActiveCustomerMainAccount"])["FactorCode"];

                if (Session["StoreID"].ToString() == "Multi" || Session["StoreID"].ToString() == "Main")
                {
                    orderHeader.Rows[0]["StoreNumber"] = "";
                }
                else
                {
                    orderHeader.Rows[0]["StoreNumber"] = Session["StoreID"];
                }

                orderHeader.Rows[0]["CurrencyCode"] = "USD";
                orderHeader.Rows[0]["ExchangeRate"] = 1.0;
                orderHeader.Rows[0]["CurrencyUnit"] = 1;
            }

            orderHeader.Rows[0]["BookedQuantity"] = 0;
            orderHeader.Rows[0]["BookedAmount"] = 0;
            orderHeader.Rows[0]["POOpenedQuantity"] = 0;
            orderHeader.Rows[0]["OpenAmount"] = 0;

            DataTable lines = ((OrderDataTableController)Session["Order"]).lines.select();

            for (int i = 0; i < lines.Rows.Count; i++)
            {
                if (lines.Rows[i].RowState != DataRowState.Deleted)
                {
                    orderHeader.Rows[0]["BookedQuantity"] = Convert.ToInt32(orderHeader.Rows[0]["BookedQuantity"]) + Convert.ToInt32(lines.Rows[i]["TotalBookedquantity"]);
                    orderHeader.Rows[0]["BookedAmount"] = Convert.ToDouble(orderHeader.Rows[0]["BookedAmount"]) + Convert.ToDouble(lines.Rows[i]["TotalBookedquantity"]) * Convert.ToDouble(lines.Rows[i]["GrossPrice"]) * (1 - Convert.ToDouble(lines.Rows[i]["DiscountPercent"]) / 100);

                    orderHeader.Rows[0]["POOpenedQuantity"] = Convert.ToInt32(orderHeader.Rows[0]["POOpenedQuantity"]) + Convert.ToInt32(lines.Rows[i]["TotalBookedquantity"]);
                    orderHeader.Rows[0]["OpenAmount"] = Convert.ToDouble(orderHeader.Rows[0]["OpenAmount"]) + Convert.ToDouble(lines.Rows[i]["TotalBookedquantity"]) * Convert.ToDouble(lines.Rows[i]["GrossPrice"]) * (1 - Convert.ToDouble(lines.Rows[i]["DiscountPercent"]) / 100);
                }
            }

            if (Session["PageMode"].ToString().Equals("Add") || Session["PageMode"].ToString().Equals("Copy"))
            {
                DataTable criteriaTable = null;
                try
                {
                    criteriaTable = dataProvider.GetDataTable(new AriaDbCommand("SELECT * FROM ORDFLGS", new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data));
                }
                catch
                {
                }

                if (criteriaTable != null && criteriaTable.Rows.Count > 0 && isFlaged(criteriaTable, orderHeader, "ORDHDR", adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderHeader")) &&
                    isFlaged(criteriaTable, ((OrderDataTableController)Session["Order"]).lines.select(), "ORDLINE", adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderHeader")) &&
                    isFlaged(criteriaTable, ((OrderDataTableController)Session["Order"]).lines.select(), "STYLE", adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderHeader")))
                {
                    orderHeader.Rows[0]["SalesOrderType"] = "T";
                }
                else
                {
                    orderHeader.Rows[0]["SalesOrderType"] = "O";
                }
            }

            DataTable ordlines = ((OrderDataTableController)Session["Order"]).lines.select();
            //ordlines.Columns.Remove("ColorDescription");
            //ordlines.Columns.Remove("StyleDescription");
            //ordlines.Columns.Remove("Check");
            //ordlines.Columns.Remove("amount");
            //ordlines.Columns.Remove("StyleGroups");
            //ordlines.Columns.Remove("PiecesPerCase");
            //ordlines.Columns.Remove("NumberOfCases");

            for (int i = 0; i < ordlines.Rows.Count; i++)
            {
                    if (ordlines.Rows[i].RowState != DataRowState.Deleted)
                {
                    ordlines.Rows[i]["SalesOrderNumber"] = sequence;

                    ordlines.Rows[i]["SalesOrderType"] = orderHeader.Rows[0]["SalesOrderType"];
                    ordlines.Rows[i]["SeasonCode"] = orderHeader.Rows[0]["SeasonCode"];
                    ordlines.Rows[i]["CustomerCode"] = orderHeader.Rows[0]["CustomerCode"];
                    ordlines.Rows[i]["CompleteDate"] = orderHeader.Rows[0]["CompleteDate"];

                    AriaDataObjectPointer stylePointer = new AriaDataObjectPointer();
                    stylePointer.AddKeyField("Style", ordlines.Rows[i]["Style"].ToString().Trim());
                    ordlines.Rows[i]["ScaleCode"] = adapter.GetBusinessObject("", "", "99", "CRM.Style", stylePointer).Rows[0]["ScaleCode"];
                    ordlines.Rows[i]["LocationCode"] = ((OrderDataTableController)Session["Order"]).header.Rows[0]["LocationCode"];
                }
                else
                {
                    ordlines.Rows.InsertAt(ordlines.NewRow(), i + 1);
                    for (int j = 0; j < ordlines.Columns.Count; j++)
                    {
                        ordlines.Rows[i + 1][j] = ordlines.Rows[i][j, DataRowVersion.Original];
                    }

                    ordlines.Rows[i + 1].AcceptChanges();

                    ordlines.Rows.RemoveAt(i);

                    ordlines.Rows[i]["SalesOrderNumber"] = sequence;
                    ordlines.Rows[i].Delete();
                }
            }

            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable profilesNamesDataTable = businessObject.GetProfileCodes("", "", "99", "SO", "ST");
            int size = profilesNamesDataTable.Rows.Count;

            DataTable profile = ((OrderDataTableController)Session["Order"]).linesProfiles.select();
            for (int i = 0; i < profile.Rows.Count; i++)
            {
                if (profile.Rows[i].RowState != DataRowState.Deleted)
                {
                    profile.Rows[i]["objectkey"] = orderHeader.Rows[0]["SalesOrderType"] + sequence + Convert.ToString(Convert.ToInt32(i / size) + 1).PadLeft(6);
                }
                else
                {
                    profile.Rows.InsertAt(profile.NewRow(), i + 1);
                    for (int j = 0; j < profile.Columns.Count; j++)
                    {
                        profile.Rows[i + 1][j] = profile.Rows[i][j, DataRowVersion.Original];
                    }

                    profile.Rows[i + 1].AcceptChanges();

                    profile.Rows.RemoveAt(i);

                    profile.Rows[i]["objectkey"] = orderHeader.Rows[0]["SalesOrderType"] + sequence + Convert.ToString(Convert.ToInt32(i / size) + 1).PadLeft(6);
                    profile.Rows[i].Delete();
                }
            }

            Hashtable profileMap = new Hashtable();
            profileMap["type"] = "cpro_type";
            profileMap["objectkey"] = "ckey";
            profileMap["code"] = "cpro_code";
            profileMap["value"] = "cpro_value";


            DataTable designs = ((OrderDataTableController)Session["Order"]).linesDesigns.designsTable;
            //designs.Columns.Remove("StylePositionDescription");
            //designs.Columns.Remove("ArtworDesignCategoryDescription");
            //designs.Columns.Remove("ArtworkDesignTypeDescription");
            //designs.Columns.Remove("ArtworDesignCategory");
            //designs.Columns.Remove("ArtworkDesignType");
            //designs.Columns.Remove("DesignName");
            //designs.Columns.Remove("NameDropId");
            //designs.Columns.Remove("NameDropTextBottom");
            //designs.Columns.Remove("NameDropTextLine3");
            //designs.Columns.Remove("NameDropTextLine4");
            //designs.Columns.Remove("NameDropTextTop");
            //designs.Columns.Remove("StylePosition");

            for (int i = 0; i < ordlines.Rows.Count; i++)
            {
                DataRow[] rows = ((OrderDataTableController)Session["Order"]).linesDesigns.GetDesignsForLine(Convert.ToInt32(ordlines.Rows[i].RowState == DataRowState.Deleted ? ordlines.Rows[i]["LineNumber", DataRowVersion.Original] : ordlines.Rows[i]["LineNumber"]));

                for (int j = 0; j < rows.Length; j++)
                {
                    if (rows[j].RowState != DataRowState.Deleted)
                    {
                        rows[j]["SalesOrderNumber"] = sequence;
                        rows[j]["LineNumber"] = j + 1;
                    }
                    else
                    {
                        rows[j].Table.Rows.InsertAt(rows[j].Table.NewRow(), j + 1);
                        for (int k = 0; k < rows[j].Table.Columns.Count; k++)
                        {
                            profile.Rows[j + 1][k] = profile.Rows[j][k, DataRowVersion.Original];
                        }

                        profile.Rows[j + 1].AcceptChanges();

                        rows[j].Table.Rows.RemoveAt(j);

                        rows[j]["SalesOrderNumber"] = sequence;
                        rows[j]["LineNumber"] = j + 1;
                        rows[j].Delete();
                    }
                }
            }

            dataProvider.UpdateDataTable(new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data, "ordhdr", orderHeader, new string[] { "SalesOrderType", "SalesOrderNumber" }, "CORDTYPE+ORDER = '@SalesOrderType@SalesOrderNumber'",
                                            adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderHeader"));

            dataProvider.UpdateDataTable(new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data, "ordline", ordlines, new string[] { "SalesOrderType", "SalesOrderNumber", "LineNumber" }, "CORDTYPE+ORDER+STR(LINENO,6) = '@SalesOrderType@SalesOrderNumber' + STR(@LineNumber,6)",
                                            adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderLine"));

            dataProvider.UpdateDataTable(new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data, "profvalu", profile, new string[] { "Type", "ObjectKey", "Code" }, "CPRO_TYPE+CKEY+CPRO_CODE = '@Type@ObjectKey@Code' ",
                                            profileMap);

            dataProvider.UpdateDataTable(new AriaDbConnection("", "99"), AriaDatabaseTypes.Aria27Data, "ORDDSGN", designs, new string[] { "SalesOrderNumber", "SalesOrderLineNo", "LineNumber" }, "ORDER+CORDLINE+STR(LINENO,6) = '@SalesOrderNumber@SalesOrderLineNo' + STR(@LineNumber, 6)",
                                            adapter.GetBusinessObjectFieldsMap("", "", "CRM.OrderDesign"));

            Session["AccountID"] = null;

            this.Response.Redirect("OrderSave.aspx");
        }
        catch (Exception ex)
        {
            Session["Saving"] = null;
            string errorMessage = "Error: " + ex.Message;
            if (ex.InnerException != null)
            {
                errorMessage += "\n\r Error Detail:" + ex.InnerException.Message;
            }
            errorMessage = errorMessage.Replace("'", "");

            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);

            EventLog.WriteEntry("Aria.CRM.Save", errorMessage + ":" + ex.StackTrace, EventLogEntryType.Information); 

        }
    }


    protected void LinesGrid_RowCommand(object sender, GridViewCommandEventArgs e)
    {
        // call from navigation links
        if(e.CommandName == "Page")
        return;

        int rowIndex = Int32.Parse(e.CommandArgument.ToString());
        rowIndex = lines.GetNotDeletedRowNo(rowIndex);

        DataRow row = lines.getLineDataRow(rowIndex);

        string style = row["Style"].ToString();        
        //string styleMajor = style.Substring(0,12).Trim();
        //string color = style.Substring(13).Trim();
        Control containerControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1");
        containerControl.Visible = true;

        Control templateControl = Parent.Parent.Parent.Parent.Parent.FindControl("TemplateControl1");
        templateControl.Visible = false;

        Control styleControl = containerControl.FindControl("Styles1");
        Object[] parameters = new Object[3];
        parameters[0] = style;
        //parameters[1] = color;
        parameters[1] = row["StoreNumber"].ToString();
        parameters[2] = row["CustomerPO"].ToString();

        styleControl.GetType().GetMethod("FillStyleControls").Invoke(styleControl, parameters);

        Control DescriptionControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1").FindControl("Description1");
        parameters = new Object[13];
        parameters[0]= row["BookedQuantity1"].ToString();
        parameters[1] = row["BookedQuantity2"].ToString();
        parameters[2] = row["BookedQuantity3"].ToString();
        parameters[3] = row["BookedQuantity4"].ToString();
        parameters[4] = row["BookedQuantity5"].ToString();
        parameters[5] = row["BookedQuantity6"].ToString();
        parameters[6] = row["BookedQuantity7"].ToString();
        parameters[7] = row["BookedQuantity8"].ToString();

        parameters[8] =  Convert.ToDouble(row["DiscountPercent"]);
        parameters[9] = row["Group"].ToString();
        parameters[10] = Convert.ToDouble(row["GrossPrice"]);
        parameters[11] = Convert.ToDouble(row["Price"]);
        parameters[12] = row["Description1"].ToString();

        DescriptionControl.GetType().GetMethod("FillDescriptionControls").Invoke(DescriptionControl, parameters);

        Control ProfileControl = Parent.Parent.Parent.Parent.Parent.FindControl("OrderDetailsContainerControl1").FindControl("Profiles1");
        parameters = new Object[1];

        int lineNumber = Convert.ToInt32(lines.select().Rows[rowIndex]["LineNumber"]);
        parameters[0] = lineNumber;

        ProfileControl.GetType().GetMethod("FillProfileControlsFromSummary").Invoke(ProfileControl, parameters);

        ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();

        ((OrderDataTableController)Session["Order"]).tempDesigns.Insert(
                            ((OrderDataTableController)Session["Order"]).linesDesigns.GetDesignsForLine(lineNumber), 0);

        Session["LineState"] = lineNumber.ToString();
        ((AjaxControlToolkit.Accordion)Parent.Parent.Parent.Parent.Parent).SelectedIndex = 1;

        // Enhance Profile Performance
        ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete()", true);
        // Enhance Profile Performance
    }

    protected void UpdateTotals()
    {
        DataTable table = lines.select();
        int total1 = 0;
        int total2 = 0;
        double total3 = 0;
        for (int index = 0; index < table.Rows.Count; index++)
        {
            if (table.Rows[index].RowState != DataRowState.Deleted)
            {
                total1 += Convert.ToInt32(table.Rows[index]["TotalBookedquantity"]);
                total2 += Convert.ToInt32(table.Rows[index]["TotalQuantity"]);
                total3 += Convert.ToDouble(table.Rows[index]["Amount"]);
            }
        }
        LinesGrid.Columns[6].FooterText = total1.ToString();
        LinesGrid.Columns[7].FooterText = total2.ToString();
        LinesGrid.Columns[13].FooterText = total3.ToString("C");
    }

    [System.Web.Services.WebMethodAttribute(), System.Web.Script.Services.ScriptMethodAttribute()]
    public static string GetDynamicContent(string contextKey)
    {
        return default(string);
    }

    protected void LinesGrid_PageIndexChanging(object sender, GridViewPageEventArgs e)
    {
        LinesGrid.DataSource = ((OrderDataTableController)Session["Order"]).lines.select();
        LinesGrid.PageSize = 8;
        LinesGrid.PageIndex = e.NewPageIndex;
        LinesGrid.DataBind();
    }
    protected void LinesGrid_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
}
