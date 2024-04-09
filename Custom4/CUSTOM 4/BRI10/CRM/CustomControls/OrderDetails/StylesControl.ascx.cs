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
using System.Collections.Generic;
using Aria.Data.BusinessObject;
using Aria.DataTypes;
using System.Reflection;
using System.Diagnostics;

public partial class StylesControl : System.Web.UI.UserControl
{

    private DescriptionControl descriptionControl;
    private ProfilesControl profilesControl;
    private Control orderSummaryControl;

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        StyleCode.Attributes.Add("onchange", "Loading(' Loading...');");
        StoreCode.Attributes.Add("onchange", "Loading(' Loading...');");
        ColorCode.Attributes.Add("onchange", "Loading(' Loading...');");
        ScaleCode.Attributes.Add("onchange", "Loading(' Loading...');");
        UpdateLineBtn.Attributes.Add("onclick", "Loading(' Updating...');");

        Session["POCode"] = POCode;
        Session["POCodeLabel4"] = Label4;

        if (!(bool)Session["UseExtenedScale"])
        {
            Label11.Visible = false;
            ScaleCode.Visible = false;
            StyleCode.Width = 97;
            ColorCode.Width = 108;
            StoreCode.Width = 128;
        }
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
        if (!Page.IsPostBack)
            Session.Add("LineState", "New");
        descriptionControl = (DescriptionControl)Parent.FindControl("Description1");
        profilesControl = (ProfilesControl)Parent.FindControl("Profiles1");      
        this.displayParentsTypes(Parent);

        string storeName = Session["StoreName"].ToString().Trim();
        string storeID = Session["StoreID"].ToString().Trim();
        
        if (storeName.Equals("Main"))
        {
            this.StoreCode.Text = "";
            this.StoreCode.Enabled = false;
            this.Label3.Enabled = false;
        }
        else if (storeName.Equals("Multi"))
        {
            this.StoreCode.Enabled = true;
            this.Label3.Enabled = true;
        }
        else
        {
            this.StoreCode.Text = Session["StoreID"].ToString();
            this.StoreCode.Enabled = false;
            this.Label3.Enabled = false;
        }
    }

    protected void UpdateLineBtn_Click(object sender, EventArgs e)
    {
        EventLog.WriteEntry("Aria.CRM.UpdateLine", "xx", EventLogEntryType.Information); 

        try
        {
            if (StyleCode.Text.Trim() == "*")
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please select style!');", true);
                return;
            }

            if (StoreCode.Text.Trim() == "*")
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please select store!');", true);
                return;
            }

            if (StyleCode.Text.Length > 0)
            {
                if (Session["AddingLine"] != null)
                {
                    ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Please wait!')", true);
                    ClearStyleControls();
                    return;
                }

                Session["AddingLine"] = "on";

                StyleCode.Enabled = true;
                string styleCode = this.GetStyleCode();

                int[] qty = descriptionControl.GetAllQtyValues();

                if (Session["StoreID"].ToString() == "Multi" && StoreCode.Text.Trim() == "")
                {
                    ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('Store assigned!')", true);
                    Session["AddingLine"] = null;
                    return;
                }

                if ((qty[0] + qty[1] + qty[2] + qty[3] + qty[4] + qty[5] + qty[6] + qty[7]) == 0)
                {
                    ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('No quantity assigned!')", true);
                    Session["AddingLine"] = null;
                    return;
                }

                if (!profilesControl.IsValid())
                {
                    ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('Please assign all profiles values!')", true);
                    Session["AddingLine"] = null;
                    return;
                }

                double grossPrice = descriptionControl.GetPriceValue();
                double disc = descriptionControl.GetDiscValue();
                string description = descriptionControl.GetStyleDescriptionValue();
                
                string group = descriptionControl.GetGroupValue();

                int LineNumber = 0;

                LinesDataTableController linesController = ((OrderDataTableController)Session["Order"]).lines;
                if (Session["LineState"].ToString().Equals("New"))
                {
                    linesController.insert(styleCode, qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                        qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                        grossPrice, disc, Convert.ToDateTime(((TextBox)Session["StartShipDateControl"]).Text), group, this.StoreCode.Text, this.POCode.Text, description);

                    LineNumber = Convert.ToInt32(Session["LastLineNumber"]);
                }
                else
                {
                    LineNumber = Convert.ToInt32(Session["LineState"]);

                    linesController.Update(LineNumber, styleCode, qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                        qty[0], qty[1], qty[2], qty[3], qty[4], qty[5], qty[6], qty[7],
                        grossPrice, disc, Convert.ToDateTime(((TextBox)Session["StartShipDateControl"]).Text), group, this.StoreCode.Text, this.POCode.Text, description);

                }

                string[] values = profilesControl.GetProfilesValues();
                ProfilesSummaryController profilesSummaryController = ((OrderDataTableController)Session["Order"]).linesProfiles;
                LinesDataTableController linesControler = ((OrderDataTableController)Session["Order"]).lines;

                if (Session["LineState"].ToString().Equals("New"))
                {
                    profilesSummaryController.insert(LineNumber, values);
                }
                else
                {
                    profilesSummaryController.update(LineNumber, values);
                }

                linesController.CalculatePackingInfo(LineNumber);

                DesignDataTableController temp = ((OrderDataTableController)Session["Order"]).tempDesigns;

                if (temp.designsTable.GetChanges() == null || temp.designsTable.GetChanges().Rows.Count > 0)
                {
                    if (Session["LineState"].ToString().Equals("New"))
                    {
                        ((OrderDataTableController)Session["Order"]).linesDesigns.Insert(temp.designsTable.Select(), LineNumber);
                    }
                    else
                    {
                        ((OrderDataTableController)Session["Order"]).linesDesigns.Update(temp.designsTable.Select(), LineNumber);
                    }
                }

                orderSummaryControl.GetType().GetMethod("Update").Invoke(orderSummaryControl, null);

                ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();

                ClearStyleControls();

                profilesControl.SetInitValues();

                Session["LineState"] = "New";

                ScriptManager.GetCurrent(this.Page).SetFocus(StyleCode.ClientID);
            }
            else
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();alert('No style selected!')", true);
                ScriptManager.GetCurrent(this.Page).SetFocus(StyleCode.ClientID);
            }

            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        }
        catch (Exception ex)
        {
            Session["AddingLine"] = null;
            string errorMessage = "Error: " + ex.Message;
            if (ex.InnerException != null)
            {
                errorMessage += "\n\r Error Detail:" + ex.InnerException.Message;
            }
            errorMessage = errorMessage.Replace("'", "");

            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);

            EventLog.WriteEntry("Aria.CRM.UpdateLine", errorMessage + ":" + ex.StackTrace, EventLogEntryType.Information); 


            
        }
    }

    private void displayParentsTypes(System.Web.UI.Control parent)
    {

        if (orderSummaryControl == null)
        {
            orderSummaryControl = parent.FindControl("OrderSummary1");
        }
        else
        {
            return;
        }
        displayParentsTypes(parent.Parent);
    }

    public void AssignStyle(string style)
    {
        StyleCode.Text = style.Trim();
        StyleCode_Selected(null, null);
    }
    
    protected void StyleCode_Selected(object sender, EventArgs e)
    {
        if (StyleCode.Text.Trim().Length > 0)
        {
            Session["AddingLine"] = null;

            StyleCode.Text = StyleCode.Text.TrimEnd().ToUpper();

            string styleValue = StyleCode.Text;
            string colorValue = ColorCode.SelectedValue;
            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            string[] arr = StyleCode.Text.Trim().Split("|".ToCharArray());
            StyleCode.Text = arr[0].TrimEnd().ToUpper();

            pointer.AddKeyField("Style", StyleCode.Text);
            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);

            if ((bool)Session["UseExtenedScale"])
            {
                if (styleRecords.Rows.Count > 0 && StyleCode.Text == styleRecords.Rows[0]["Style"].ToString().Substring(0, 8).TrimEnd())
                {
                    StyleCode.Text = styleRecords.Rows[0]["Style"].ToString().Substring(0, 8);

                    InitColor();
                }
                else
                {
                    ClearStyleControls();
                }
            }
            else
            {
                if (styleRecords.Rows.Count > 0 && StyleCode.Text == styleRecords.Rows[0]["Style"].ToString().Substring(0, 12).TrimEnd())
                {
                    StyleCode.Text = styleRecords.Rows[0]["Style"].ToString().Substring(0, 12);

                    InitColor();
                }
                else
                {
                    ClearStyleControls();
                }
            }
        }

        if (StyleCode.Text.Trim().Length == 0)
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('Invalid Style ID!');", true);
        }
        else
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        }

        ScriptManager.GetCurrent(this.Page).SetFocus(StyleCode.ClientID);
    }

    protected void ResetBtn_Click(object sender, EventArgs e)
    {
        ((OrderDataTableController)Session["Order"]).tempDesigns = new DesignDataTableController();

        ClearStyleControls();

        profilesControl.SetInitValues();

        Session["LineState"] = "New";
        Session["TemplateState"] = "New";

        ScriptManager.GetCurrent(this.Page).SetFocus(StyleCode.ClientID);
    }

    protected void ColorCode_TextChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        
        //T20080825.0002  [Begin]
        //InitScale();
        //T20080825.0002  [End]

        string styleValue = StyleCode.Text;
        string colorValue = ColorCode.SelectedValue;
        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();


        if ((bool)Session["UseExtenedScale"])
        {
            //T20080825.0002  [Begin]
            InitScale();
            //T20080825.0002  [End]
            pointer.AddKeyField("Style", styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.ToString().PadRight(3));
        }
        else
        {
            pointer.AddKeyField("Style", styleValue.PadRight(12) + "-" + colorValue.PadRight(6));
        }

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();

        DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);


        object[] parameters = new object[1];
        parameters[0] = styleRecords;
        descriptionControl.InitScalesTable(styleRecords);

        ScriptManager.GetCurrent(this.Page).SetFocus(ColorCode.ClientID);
    }

    protected void ScaleCode_SelectedIndexChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);

        string styleValue = StyleCode.Text;
        string colorValue = ColorCode.SelectedValue;
        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();


        if ((bool)Session["UseExtenedScale"])
        {
            pointer.AddKeyField("Style", styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.ToString().PadRight(3));
        }
        else
        {
            pointer.AddKeyField("Style", styleValue.PadRight(12) + "-" + colorValue.PadRight(6));
        }

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();

        DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);


        object[] parameters = new object[1];
        parameters[0] = styleRecords;
        descriptionControl.InitScalesTable(styleRecords);

        ScriptManager.GetCurrent(this.Page).SetFocus(ColorCode.ClientID);
    }

    private void InitScale()
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = StyleCode.Text.PadRight(8) + "-" + ColorCode.SelectedValue.ToString().PadRight(6);
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Style"); 

        conditionList.Items.Add(condition);

        DataTable xssss = businessObject.GetBusinessObjects("", "", "99", "CRM.Style.Color.Scale", conditionList);

        ScaleCode.DataSource = xssss;
        ScaleCode.DataTextField = "Description";
        ScaleCode.DataValueField = "Scale";
        ScaleCode.DataBind();
    }

    private void InitColor()
    {
        bool useExtenedScale = (bool)Session["UseExtenedScale"];

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;



        if ((bool)Session["UseExtenedScale"])
        {
            ((AriaStandardDataType)condition.RightHandSide).Value = StyleCode.Text.PadRight(8); // User Input
        }
        else
        {
            ((AriaStandardDataType)condition.RightHandSide).Value = StyleCode.Text.PadRight(12); // User Input
        }

        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Style"); // Field
        conditionList.Items.Add(condition);

        DataTable xssss = businessObject.GetBusinessObjects("", "", "99", "CRM.Style.Color", conditionList);

        ColorCode.DataSource = xssss;
        ColorCode.DataTextField = "Description";
        ColorCode.DataValueField = "Color";
        ColorCode.DataBind();

        if (useExtenedScale)
        {
            InitScale();
            
            string styleValue = StyleCode.Text;
            string colorValue = ColorCode.SelectedValue;
            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            pointer.AddKeyField("Style", styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.PadRight(3));
            businessObject = new AriaBusinessObjectAdapter();
            DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);


            descriptionControl.InitScalesTable(styleRecords);

            profilesControl.SetDefaults(styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.PadRight(3));
        }
        else
        {
            string styleValue = StyleCode.Text;
            string colorValue = ColorCode.SelectedValue;
            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            pointer.AddKeyField("Style", styleValue.PadRight(12) + "-" + colorValue.PadRight(6));
            businessObject = new AriaBusinessObjectAdapter();
            DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);

            descriptionControl.InitScalesTable(styleRecords);

            profilesControl.SetDefaults(styleValue.PadRight(12) + "-" + colorValue.PadRight(6));
        }
    }

    public void ClearStyleControls()
    {
        StyleBtn.Enabled = true;
        StyleCode.Text = "";
        StyleCode.Enabled = true;
        ColorCode.DataSource = null;
        ColorCode.Items.Clear();
        ColorCode.DataBind();

        ScaleCode.DataSource = null;
        ScaleCode.Items.Clear();
        ScaleCode.DataBind();
        
        descriptionControl.ClearScalesTable();
        Session["LineState"] = "New";
        Session["TemplateState"] = "New";

        if (Session["MultiPOMain"] == null || ((CheckBox)Session["MultiPOMain"]).Checked)
        {
            POCode.Text = "";
        }

        StoreCode.Text = "";
    }

    private int ConvertToInt(string value)
    {
        try
        {
            return Int32.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
        }
    }

    private double ConvertToDouble(string value)
    {
        try
        {
            return double.Parse(value);
        }
        catch (Exception e)
        {
            return 0;
        }
    }

    public void FillStyleControls(string style, string storeNumber, string customerPO)
    {
        string styleValue = "";
        string colorValue = "";
        string scaleValue = "";

        if ((bool)Session["UseExtenedScale"])
        {
            styleValue = style.Substring(0, 8);
            colorValue = style.Substring(9, 6);
            scaleValue = style.Substring(16).PadRight(3);
        }
        else
        {
            styleValue = style.Substring(0, 12);
            colorValue = style.Substring(13).PadRight(6);
            scaleValue = "";
        }

        Session["AddingLine"] = null;

        StoreCode.Text = storeNumber;
        POCode.Text = customerPO;

        StyleCode.Text = styleValue;
        InitColor();
        ColorCode.SelectedValue = colorValue.PadRight(6, ' ');

        if ((bool)Session["UseExtenedScale"])
        {
            InitScale();
            ScaleCode.SelectedValue = scaleValue.PadRight(3, ' ');
        }

        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();

        if ((bool)Session["UseExtenedScale"])
        {
            pointer.AddKeyField("Style", styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.ToString().PadRight(3));
        }
        else
        {
            pointer.AddKeyField("Style", styleValue.PadRight(12) + "-" + colorValue.PadRight(6));
        }

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);

        descriptionControl.InitScalesTable(styleRecords);

        profilesControl.SetDefaults(style);
    }

    public string GetColorDescription()
    {
        return this.ColorCode.SelectedValue;
    }

    public string GetStyleCode()
    {
        string styleValue = StyleCode.Text;
        string colorValue = ColorCode.SelectedValue;
        if (styleValue.Trim() == "*")
        {
            return "*";
        }
        else
        {
            if ((bool)Session["UseExtenedScale"])
            {
                return styleValue.PadRight(8) + "-" + colorValue.PadRight(6) + "-" + ScaleCode.SelectedValue.ToString().PadRight(3);
            }
            else
            {
                return styleValue.PadRight(12) + "-" + colorValue.PadRight(6);
            }

            
        }
    }

    protected void StyleBtn_Click(object sender, EventArgs e)
    {
        StyleCode.Text = "*";
        ScriptManager.GetCurrent(this.Page).SetFocus(StyleCode.ClientID);
    }
    
    
    protected void StoreCode_TextChanged(object sender, EventArgs e)
    {        
        string[] strArray = StoreCode.Text.Split('|');
        if (strArray.Length == 2)
        {
            StoreCode.Text = strArray[0].TrimEnd().ToUpper();
        }

        AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
        AriaDataObjectPointer pointer = new AriaDataObjectPointer();

        pointer.AddKeyField("Account", Session["AccountID"].ToString().ToUpper().PadRight(5));
        pointer.AddKeyField("Store", StoreCode.Text.ToUpper().PadRight(8));

        DataTable result = provider.GetBusinessObject("", "", "99", "CRM.Store", pointer);
        if (result.Rows.Count == 0 || StoreCode.Text != result.Rows[0]["StoreNumber"].ToString().TrimEnd())
        {
            StoreCode.Text = "";
        }

        if (StoreCode.Text.Trim().Length == 0)
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('Invalid Store ID!');", true);
        }
        else
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        }

        ScriptManager.GetCurrent(this.Page).SetFocus(StoreCode.ClientID);
    }

    protected void Label3_Click(object sender, EventArgs e)
    {
        StoreCode.Text = "*";
        ScriptManager.GetCurrent(this.Page).SetFocus(StoreCode.ClientID);
    }

    public string GetStyleControlClientID()
    {
        return StyleCode.ClientID;
    }
}
