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


public partial class Order : System.Web.UI.Page
{
    private AriaBusinessObjectAdapter dictionary;

    protected override void OnPreInit(EventArgs e)
    {
        base.OnPreInit(e);

        if (Session["AccountID"] == null || Session["AccountID"] == "")
        {
            this.Response.Redirect("CustomerSelection.aspx");
        }
    }

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        dictionary = new AriaBusinessObjectAdapter();
        PageValidationController validator = new PageValidationController(dictionary, Page);
        validator.ApplyValidationRules();
        if (!Page.IsPostBack)
            Accordion1.SelectedIndex = 0;

        if (Session["StoreID"].ToString() == "Multi")
        {
            CheckBox1.Checked = true;
            CheckBox2.Enabled = true;
        }
        else
        {
            CheckBox1.Checked = false;
            CheckBox2.Enabled = false;
        }

        Session["MultiPOMain"] = CheckBox2;

        CheckBox2.Attributes.Add("onclick", "Loading(' Updating...');");
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        string AccountID = Session["AccountID"].ToString();
        if (Label2 != null)
        {
            Label2.Text = AccountID.TrimEnd() + " - " + Session["StoreID"].ToString().TrimEnd();
            Label5.Text = AccountID.TrimEnd() + " - " + Session["StoreID"].ToString().TrimEnd();
            Label8.Text = AccountID.TrimEnd() + " - " + Session["StoreID"].ToString().TrimEnd();
        }

        Session["SelectCustomer"] = null;
        //Utility.EnableAllControlsInsideControl(this.Page, false);
    }



    protected void CheckBox2_CheckedChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Page, typeof(string), "", "LoadingComplete();", true);
        
        if (((CheckBox)Session["MultiPOHeader"]).Checked != CheckBox2.Checked)
            ((CheckBox)Session["MultiPOHeader"]).Checked = CheckBox2.Checked;

        ((TextBox)Session["CustomerPO1"]).Enabled = !CheckBox2.Checked;
        ((Label)Session["CustomerPO1Label1"]).Enabled = !CheckBox2.Checked;

        ((TextBox)Session["POCode"]).Enabled = CheckBox2.Checked;
        ((Label)Session["POCodeLabel4"]).Enabled = CheckBox2.Checked;

        if (CheckBox2.Checked)
        {
            ((TextBox)Session["CustomerPO1"]).Text = "";
            ((TextBox)Session["POCode"]).Text = "";
            ((OrderDataTableController)Session["Order"]).lines.UpdatePO("");
        }
        else
        {
            ((TextBox)Session["POCode"]).Text = ((TextBox)Session["CustomerPO1"]).Text;
            ((OrderDataTableController)Session["Order"]).lines.UpdatePO(((TextBox)Session["CustomerPO1"]).Text);
        }
    }

    protected void OkButton_Click(object sender, EventArgs e)
    {
        string username = this.txtUserName.Text.Trim();
        object password = (object)this.txtPassword.Text.Trim();

        AriaBusinessObjectAdapter object1 = new AriaBusinessObjectAdapter();
        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
        pointer.AddKeyField("UserID", username);

        DataTable y = object1.GetSystemObject("Aria", "", "99", "CRM.USER", pointer);
        if (y.Rows.Count == 0)
        {
            return;
        }
        frameworkfox.nativefoxClass x = new frameworkfox.nativefoxClass();
        string passwordhash = (string)x.gethashcode(ref password);
        bool superVisor = y.Rows[0][""].ToString().Trim().ToLower().Contains("supervisor");
        bool correctPassword = y.Rows[0][""].ToString().Trim().ToUpper().Equals(passwordhash);
        if (superVisor && correctPassword)
        {
            Utility.EnableAllControlsInsideControl(this.Page, true);
        }
    }
}
