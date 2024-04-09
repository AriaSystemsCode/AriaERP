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
using Aria.Data;

public partial class General : System.Web.UI.UserControl
{
    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);


        Session["StartShipDateControl"] = StartDate;

        EnteredDate.Attributes.Add("onfocusout", "return IsValidDate(this)");
        StartDate.Attributes.Add("onfocusout", "return IsValidDate(this)");
        CompleteDate.Attributes.Add("onfocusout", "return IsValidDateOrEmpty(this)");

        EnteredDate.Attributes.Add("onfocus", "SaveLastValue(this.value)");
        StartDate.Attributes.Add("onfocus", "SaveLastValue(this.value)");
        CompleteDate.Attributes.Add("onfocus", "SaveLastValue(this.value)");

        StartDate.Attributes.Add("onchange", "Loading(' Updating...');");

        EnteredDate.MaxLength = 10;
        StartDate.MaxLength = 10;
        CompleteDate.MaxLength = 10;

        Session["CustomerPO1"] = CustomerPO1;
        Session["CustomerPO1Label1"] = Label1;

        CustomerPO1.Attributes.Add("onchange", "Loading(' Updating...');");
    }
    
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["ActiveCustomerGetGeneralInfo"].ToString() == "Yes")
        {
            if (Session["PageMode"].ToString().Equals("Add"))
            {
                EnteredDate.Text = DateTime.Now.Date.ToShortDateString();
                StartDate.Text = DateTime.Now.Date.ToShortDateString();
            }

            AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();

            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            pointer.AddKeyField("ApplicationID", "SO");
            pointer.AddKeyField("Setting", "M_COMPDATE".PadRight(10));

            DataTable setup = adapter.GetBusinessObject("Aria", "", "99", "CRM.Setup", pointer);

            if (setup.Rows.Count > 0)
            {
                CompleteDate.Text = Convert.ToDateTime(DateTime.Now.Date).AddDays(Int32.Parse(setup.Rows[0]["DefaultData"].ToString())).ToShortDateString();
            }
            else
            {
                CompleteDate.Text = DateTime.Now.ToShortDateString();
            }
            
            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetGeneralInfo"] = "No";
        }
    }

    private object GetDate(string value)
    {
        if (value.Trim() == "")
        {
            return DBNull.Value ;
        }
        else
        {
            string[] parse = value.Split("/".ToCharArray());

            return new DateTime(Convert.ToInt32(parse[2]), Convert.ToInt32(parse[0]), Convert.ToInt32(parse[1]));
        }
    }

    private void LoadFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();

        this.CustomerPO1.Text = datatable.Rows[0]["CustomerPO"].ToString();


        this.EnteredDate.Text = Convert.ToDateTime(datatable.Rows[0]["EnteredDate"]).ToString("MM/dd/yyyy");
        this.StartDate.Text = Convert.ToDateTime(datatable.Rows[0]["StartDate"]).ToString("MM/dd/yyyy");
        if (Convert.ToDateTime(datatable.Rows[0]["CompleteDate"]) > (new DateTime(1900, 1, 1)))
        {
            this.CompleteDate.Text = Convert.ToDateTime(datatable.Rows[0]["CompleteDate"]).ToString("MM/dd/yyyy");
        }
        else
        {
            this.CompleteDate.Text = "";
        }

        this.SeasonCode.SelectedValue = datatable.Rows[0]["SeasonCode"].ToString();
        this.Division.SelectedValue = datatable.Rows[0]["Division"].ToString();
    }

    public void SaveFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];
        r["CustomerPO"] = this.CustomerPO1.Text;
        r["EnteredDate"] = GetDate(this.EnteredDate.Text);
        r["StartDate"] = GetDate(this.StartDate.Text);
        r["CompleteDate"] = GetDate(this.CompleteDate.Text);

        r["SeasonCode"] = this.SeasonCode.SelectedValue;
        r["Division"] = this.Division.SelectedValue;
    }

    protected void Timer1_Tick(object sender, EventArgs e)
    {
        Timer1.Enabled = false;
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);
    }
    
    protected void CustomerPO1_TextChanged(object sender, EventArgs e)
    {
        if (CustomerPO1.Text.TrimEnd().Length > 0)
        {
            AriaDbCommand command = new AriaDbCommand("Select order FROM ORDHDR WHERE ACCOUNT+UPPER(CUSTPO)+CORDTYPE+ORDER = + '" + Session["AccountID"].ToString().PadRight(5) + "@custpo'", new AriaDbConnection("", "99"), Aria.Environment.AriaDatabaseTypes.Aria27Data);
            command.Parameters.Add(new AriaDbParameter("custpo", CustomerPO1.Text.PadRight(15).ToUpper()));
            DataTable result = command.GetDataTable();

            if (result.Rows.Count > 0 &&
               result.Rows[0]["order"].ToString().TrimEnd() !=
               ((OrderDataTableController)Session["Order"]).header.Rows[0]["SalesOrderNumber"].ToString())
            {
                ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete(); if(confirm('Customer PO# has been entered before. Do you want to reenter it again?')) {document.getElementById('" + CustomerPO1.ClientID + "').value= ''; document.getElementById('" + CustomerPO1.ClientID + "').focus(); }", true);
                ScriptManager.GetCurrent(this.Page).SetFocus(CustomerPO1.ClientID);
            }
            else
            {
                ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
                ScriptManager.GetCurrent(this.Page).SetFocus(EnteredDate.ClientID);
            }
        }
        else
        {
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
            ScriptManager.GetCurrent(this.Page).SetFocus(EnteredDate.ClientID);
        }
        

        ((OrderDataTableController)Session["Order"]).lines.UpdatePO(CustomerPO1.Text);
        ((TextBox)Session["POCode"]).Text = ((TextBox)Session["CustomerPO1"]).Text;

        ((OrderDataTableController)Session["Order"]).lines.UpdateShipStartDate(Convert.ToDateTime(StartDate.Text));
        ((OrderDataTableController)Session["Order"]).templateLines.UpdateShipStartDate(Convert.ToDateTime(StartDate.Text));

        Session["SummaryControl"].GetType().GetMethod("Update").Invoke(Session["SummaryControl"], null);
        Session["TemplateControl"].GetType().GetMethod("Update").Invoke(Session["TemplateControl"], null);
    }
    
    protected void StartDate_TextChanged(object sender, EventArgs e)
    {
        ((OrderDataTableController)Session["Order"]).lines.UpdateShipStartDate(Convert.ToDateTime(StartDate.Text));
        ((OrderDataTableController)Session["Order"]).templateLines.UpdateShipStartDate(Convert.ToDateTime(StartDate.Text));
        
        Session["SummaryControl"].GetType().GetMethod("Update").Invoke(Session["SummaryControl"], null);
        Session["TemplateControl"].GetType().GetMethod("Update").Invoke(Session["TemplateControl"], null);
        
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete()", true);
    }
}
