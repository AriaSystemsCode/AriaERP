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
using Aria.DataTypes;
using System.Diagnostics;

public partial class CustomerSelection : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        Session["AccountID"] = AccountNoTextBox.Text;
        AccountNoTextBox.Attributes.Add("onchange", "Loading(' Loading...')");
        NameTextBox.Attributes.Add("onchange", "Loading(' Loading...')");
        CityTextBox.Attributes.Add("onchange", "Loading(' Loading...')");
        ZipCodeTextBox.Attributes.Add("onchange", "Loading(' Loading...')");
        txtPhone.Attributes.Add("onchange", "Loading(' Loading...')");
        StoreTextBox.Attributes.Add("onchange", "Loading(' Loading...')");
        SelectBtn.Attributes.Add("onclick", "Loading(' New Order...')");
        cboStoreType.Attributes.Add("onchange", "Loading(' Loading...')");

        Session["RepID"] = Request.QueryString["RepID"] == null? "" : Request.QueryString["RepID"];
        //Session["LocationCode"] = Request.QueryString["WareHous"] == null ? "" : Request.QueryString["WareHous"];


        //
    }

    private void ClearAll()
    {
        AccountNoTextBox.Text = "";
        NameTextBox.Text = "";
        CityTextBox.Text = "";
        ZipCodeTextBox.Text = "";
        txtPhone.Text = "";
        StoreTextBox.Text = "";
        cboStoreType.SelectedIndex = 0;
        cboStoreType.Items[1].Enabled = false;
        cboStoreType.Items[2].Enabled = false;
        StoreTextBox.Enabled = false;
        Session["AccountID"] = "";
        Session["StoreID"] = "";
        Session["ActiveCustomer"] = null;
    }

    protected override void OnLoad(EventArgs e)
    {  
        base.OnLoad(e);

        ScriptManager.GetCurrent(this).SetFocus(AcctLinkBtn.ClientID);
    }

    private void GetAccount(string Field, string TextValue)
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = TextValue.Trim().ToUpper();
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", Field);

        conditionList.Items.Add(condition);

        DataTable customers;

        customers = businessObject.GetBusinessObjects("", "", "99", "CRM.Customer", conditionList);

        if (customers.Rows.Count == 0 || TextValue.TrimEnd().Length == 0 ||
            customers.Rows[0][Field].ToString().TrimEnd().ToUpper() != TextValue.TrimEnd().ToUpper())
        {
            ClearAll();
            return;
        }
        else
        {
            AccountNoTextBox.Text = customers.Rows[0]["Account"].ToString().Trim();
            NameTextBox.Text = customers.Rows[0]["Name"].ToString().Trim();
            CityTextBox.Text = customers.Rows[0]["City"].ToString().Trim();
            ZipCodeTextBox.Text = customers.Rows[0]["ZipCode"].ToString().Trim();
            txtPhone.Text = customers.Rows[0]["Phone"].ToString().Trim();
            if (customers.Rows[0]["Store"].ToString().Trim().ToLower() == "***")
            {
                cboStoreType.Items[0].Enabled = true;
                cboStoreType.Items[1].Enabled = true;
                cboStoreType.Items[2].Enabled = true;
            }
            else
            {
                cboStoreType.Items[0].Enabled = true;
                cboStoreType.Items[1].Enabled = false;
                cboStoreType.Items[2].Enabled = false;
            }
            cboStoreType.SelectedIndex = 0;
            StoreTextBox.Enabled = false;
            StoreTextBox.Text = "";
            Session["AccountID"] = customers.Rows[0]["Account"].ToString().TrimEnd();
        }
    }


    private void GetAccountStore(string Field, string TextValue)
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = TextValue.Trim().ToUpper();
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", Field);

        conditionList.Items.Add(condition);

        DataTable customers;

        AriaCondition condition1 = new AriaCondition();
        condition1.RightHandSide = new AriaStandardDataType();
        condition1.LeftHandSide = new AriaStandardDataType();
        condition1.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition1.RightHandSide).Value = Session["AccountID"].ToString().PadRight(5).ToUpper();
        ((AriaStandardDataType)condition1.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Account");

        conditionList.Items.Add(condition1);

        customers = businessObject.GetBusinessObjects("", "", "99", "CRM.Customer.Store", conditionList);

        if (customers.Rows.Count > 0 && customers.Rows[0][Field].ToString().TrimEnd().ToUpper() == TextValue.TrimEnd().ToUpper())
        {
            AccountNoTextBox.Text = customers.Rows[0]["Account"].ToString().Trim();
            NameTextBox.Text = customers.Rows[0]["Name"].ToString().Trim();
            CityTextBox.Text = customers.Rows[0]["City"].ToString().Trim();
            ZipCodeTextBox.Text = customers.Rows[0]["ZipCode"].ToString().Trim();
            txtPhone.Text = customers.Rows[0]["Phone"].ToString().Trim();
            cboStoreType.SelectedIndex = 2;
            StoreTextBox.Text = customers.Rows[0]["Store"].ToString().TrimEnd();
            StoreTextBox.Enabled = true;
            Session["AccountID"] = customers.Rows[0]["Account"].ToString().TrimEnd();
            Session["StoreID"] = customers.Rows[0]["Store"].ToString().TrimEnd();
        }
        else
        {
            ClearAll();
            return;
        }
    }

    protected void AccountNoTextBox_TextChanged(object sender, EventArgs e)
    {
        string[] strArray = AccountNoTextBox.Text.Split('|');

        if (strArray.Length == 6)
        {
            AccountNoTextBox.Text = strArray[0].Trim();
            this.GetAccount("Account", AccountNoTextBox.Text.Trim());
        }
        else
        {
            this.GetAccount("Account", AccountNoTextBox.Text.Trim());
        }

        if (AccountNoTextBox.Text.Trim().Length == 0)
        {
            ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Invalid Account!')", true);
        }

        ScriptManager.GetCurrent(this).SetFocus(NameLinkBtn.ClientID);
    }

    protected void ValidateValue(string name, TextBox textBox, string nextControlID)
    {
        string[] strArray = textBox.Text.Split('|');
        if (strArray.Length == 6)
        {
            textBox.Text = strArray[2].Trim();
        }

        if (Session["AccountID"] == null || Session["AccountID"].ToString().TrimEnd() == "")
        {
            string oldAccount = AccountNoTextBox.Text;

            if (strArray.Length == 6)
            {
                this.GetAccount("Account", strArray[0].Trim());
            }
            else
            {
                this.GetAccount(name, textBox.Text.Trim());
            }

            if (AccountNoTextBox.Text.Trim().Length == 0)
            {
                GetAccount("Account", oldAccount);
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Invalid " + name + "!')", true);
            }
        }
        else
        {
            string oldAccount = AccountNoTextBox.Text;
            string oldStoreType = cboStoreType.Text;
            string oldStore = StoreTextBox.Text;

            if (strArray.Length == 6)
            {
                this.GetAccountStore("Store", strArray[1].Trim());
            }
            else
            {
                this.GetAccountStore(name, NameTextBox.Text.Trim());
            }
            if (AccountNoTextBox.Text.Trim().Length == 0)
            {
                if (oldStoreType == "Single")
                {
                    Session["AccountID"] = oldAccount;
                    GetAccountStore("Store", oldStore);
                }
                else
                {
                    GetAccount("Account", oldAccount);
                }
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Invalid Name!')", true);
            }
        }

        ScriptManager.GetCurrent(this).SetFocus(nextControlID);
    }
    
    protected void NameTextBox_TextChanged(object sender, EventArgs e)
    {
        ValidateValue("Name", NameTextBox, CityLinkBtn.ClientID);
    }

    protected void CityTextBox_TextChanged(object sender, EventArgs e)
    {
        ValidateValue("City", CityTextBox, ZipCodeLinkBtn.ClientID);
    }

    protected void ZipCodeTextBox_TextChanged(object sender, EventArgs e)
    {
        ValidateValue("ZipCode", ZipCodeTextBox, Phone.ClientID);
    }


    protected void txtPhone_TextChanged(object sender, EventArgs e)
    {
        ValidateValue("Phone", txtPhone, StoreLinkBtn.ClientID);
    }

    protected void cboStoreType_SelectedIndexChanged(object sender, EventArgs e)
    {
        int oldValue = cboStoreType.SelectedIndex;
        if (cboStoreType.SelectedIndex == 0 || cboStoreType.SelectedIndex == 1)
        {
            this.GetAccount("Account", AccountNoTextBox.Text);
        }

        cboStoreType.SelectedIndex = oldValue;
        StoreTextBox.Enabled = cboStoreType.SelectedIndex == 2;
    }

    protected void StoreTextBox_TextChanged(object sender, EventArgs e)
    {
        StoreTextBox.Text = StoreTextBox.Text.TrimEnd().ToUpper();

        if (StoreTextBox.Text.Trim().Length == 0)
        {
            GetAccount("Account", AccountNoTextBox.Text);
        }
        else
        {
            string[] strArray = StoreTextBox.Text.Split('|');
            if (strArray.Length == 5)
            {
                StoreTextBox.Text = strArray[0].TrimEnd();
            }

            string oldAccount = AccountNoTextBox.Text;
            string oldStoreType = cboStoreType.Text;
            string oldStore = StoreTextBox.Text;

            this.GetAccountStore("Store", StoreTextBox.Text);

            if (AccountNoTextBox.Text.Trim().Length == 0)
            {
                if (oldStoreType == "Single")
                {
                    Session["AccountID"] = oldAccount;
                    GetAccountStore("Store", oldStore);
                }
                else
                {
                    GetAccount("Account", oldAccount);
                }
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Invalid Store!')", true);
            }


            ScriptManager.GetCurrent(this).SetFocus(CityLinkBtn.ClientID);
        }
    }

    protected void AcctLinkBtn_Click(object sender, EventArgs e)
    {
        AccountNoTextBox.Text = "*";
        ScriptManager.GetCurrent(this).SetFocus(AccountNoTextBox.ClientID);
    }

    protected void NameLinkBtn_Click(object sender, EventArgs e)
    {
        NameTextBox.Text = "*";
        ScriptManager.GetCurrent(this).SetFocus(NameTextBox.ClientID);
    }

    protected void CityLinkBtn_Click(object sender, EventArgs e)
    {
        CityTextBox.Text = "*";
        ScriptManager.GetCurrent(this).SetFocus(CityTextBox.ClientID);
    }

    protected void ZipCodeLinkBtn_Click(object sender, EventArgs e)
    {
        ZipCodeTextBox.Text = "*";
        ScriptManager.GetCurrent(this).SetFocus(ZipCodeTextBox.ClientID);
    }
    protected void Phone_Click(object sender, EventArgs e)
    {
        txtPhone.Text = "*";
        ScriptManager.GetCurrent(this).SetFocus(txtPhone.ClientID);
    }

    protected void StoreLinkBtn_Click(object sender, EventArgs e)
    {
        if (cboStoreType.Text == "Single")
        {
            StoreTextBox.Text = "*";
            ScriptManager.GetCurrent(this).SetFocus(StoreTextBox.ClientID);
        }
    }

    protected void SelectBtn_Click(object sender, EventArgs e)
    {
        try
        {
            if (AccountNoTextBox.Text.Trim() == "*")
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Please select account!');", true);
                return;
            }

            if (StoreTextBox.Text.Trim() == "*" || (StoreTextBox.Text.Trim().Length == 0 && cboStoreType.SelectedIndex == 2) )
            {
                ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Please select store!');", true);
                return;
            }

            if (AccountNoTextBox.Text.Trim().Length > 0)
            {
                if (Session["SelectCustomer"] != null)
                {
                    ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "alert('Please wait!');", true);
                    return;
                }

                Session["SelectCustomer"] = "on";

                Session.Add("AccountID", AccountNoTextBox.Text);

                if (cboStoreType.SelectedIndex == 0)
                {
                    Session.Add("StoreID", "Main");
                    Session.Add("StoreName", "Main");
                }
                else if (cboStoreType.SelectedIndex == 1)
                {
                    Session.Add("StoreID", "Multi");
                    Session.Add("StoreName", "Multi");
                }
                else
                {
                    Session.Add("StoreID", StoreTextBox.Text);
                }

                AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
                AriaDataObjectPointer pointer = new AriaDataObjectPointer();
                pointer.AddKeyField("Account", AccountNoTextBox.Text.ToString().PadRight(5));

                if (cboStoreType.SelectedIndex == 2)
                {
                    DataTable mainAccountResult = provider.GetBusinessObject("", "", "99", "CRM.Customer", pointer);
                    Session["ActiveCustomerMainAccount"] = mainAccountResult.Rows[0];
                    
                    
                    pointer.AddKeyField("Store", StoreTextBox.Text.PadRight(8));
                    DataTable result = provider.GetBusinessObject("", "", "99", "CRM.Store", pointer);
                    Session["ActiveCustomer"] = result.Rows[0];
                    Session.Add("StoreName", ((DataRow)Session["ActiveCustomer"])["BillToName"]);
                }
                else
                {
                    DataTable result = provider.GetBusinessObject("", "", "99", "CRM.Customer", pointer);
                    Session["ActiveCustomer"] = result.Rows[0];

                    Session["ActiveCustomerMainAccount"] = Session["ActiveCustomer"];
                }

                Session.Add("ActiveCustomerGetAddressInfo", "Yes");
                Session.Add("ActiveCustomerGetContactInfo", "Yes");
                Session.Add("ActiveCustomerGetGeneralInfo", "Yes");
                Session.Add("ActiveCustomerGetNoteInfo", "Yes");
                Session.Add("ActiveCustomerGetOrderInfo", "Yes");
                Session.Add("ActiveCustomerGetSalesRepresentativeInfo", "Yes");

                Session["TemplateState"] = null;
                Session["LastLineNumber"] = 0;
                Session["TemplateLastLineNumber"] = 0;
                Session["LineState"] = "New";
                Session["sequence"] = null;
                Session["PageMode"] = "Add";

                Session["Saving"] = null;
                Session["AddingLine"] = null;

                Session["Order"] = new OrderDataTableController();
                ((OrderDataTableController)Session["Order"]).Add();

                Response.Redirect("PreOrder.asp?CustomerID=" + AccountNoTextBox.Text);
                //Response.Redirect("Order.aspx");
            }
            else
            {
                ScriptManager.RegisterClientScriptBlock(this, typeof(string), "", "alert('Please select an account.');", true);
            }
        }
        catch (Exception ex)
        {
            Session["SelectCustomer"] = null;
            string errorMessage = "Error: " + ex.Message;
            if (ex.InnerException != null)
            {
                errorMessage += "\n\r Error Detail:" + ex.InnerException.Message;
            }
            errorMessage = errorMessage.Replace("'", "");

            ScriptManager.RegisterClientScriptBlock(this, typeof(string), "", "LoadingComplete(); alert('" + errorMessage + "')", true);

            EventLog.WriteEntry("Aria.CRM.SelectCustomer", errorMessage + ":" + ex.StackTrace, EventLogEntryType.Information); 

        }
    }

    protected void btnReset_Click(object sender, EventArgs e)
    {
        ClearAll();
    }
}

