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

public partial class SalesRepsInformationControl : System.Web.UI.UserControl
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["ActiveCustomerGetSalesRepresentativeInfo"].ToString() == "Yes")
        {
            if (Session["PageMode"].ToString().Equals("Add"))
            {
                SalesRep1.Text = ((DataRow)Session["ActiveCustomer"])["SalesRepresentative"].ToString();
                SalesRep1_TextChanged(null, null);
                SalesrepCommission1.Text = ((DataRow)Session["ActiveCustomer"])["Commissionforrep1"].ToString();
                if (SalesrepCommission1.Text == "0") SalesrepCommission1.Text = "";

                SalesRep2.Text = ((DataRow)Session["ActiveCustomer"])["SalesRep2"].ToString();
                SalesRep2_TextChanged(null, null);
                SalesrepCommission2.Text = ((DataRow)Session["ActiveCustomer"])["salesrepCommission2"].ToString();
                if (SalesrepCommission2.Text == "0") SalesrepCommission2.Text = "";

                Session["ActiveCustomerGetSalesRepresentativeInfo"] = "No";
            }

            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetSalesRepresentativeInfo"] = "No";

            SalesrepCommission1.Visible = Session["RepID"].ToString().Trim() == SalesRep1.Text.Trim();
            SalesrepCommission2.Visible = Session["RepID"].ToString().Trim() == SalesRep2.Text.Trim();

            SalesRep1.Attributes.Add("onchange", "Loading(' Loading...')");
            SalesRep2.Attributes.Add("onchange", "Loading(' Loading...')");

            TextBox1.Visible = !SalesrepCommission1.Visible;
            TextBox2.Visible = !SalesrepCommission2.Visible;
        }
    }

    protected void SalesRep1_TextChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);

        string dele = "|";
        string[] strArray = SalesRep1.Text.Split(dele.ToCharArray());
        SalesRep1.Text = strArray[0].Trim(); 
        
        AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
        AriaDataObjectPointer pointer = new AriaDataObjectPointer();
        pointer.AddKeyField("SalesRepCode", SalesRep1.Text.Trim().PadRight(3));
        DataTable result = provider.GetBusinessObject("", "", "99", "CRM.SalesRep", pointer);
        if (result.Rows.Count > 0 && result.Rows[0]["SalesRepCode"].ToString().TrimEnd().ToUpper() == SalesRep1.Text.TrimEnd().ToUpper())
        {
            string name = result.Rows[0]["Name"].ToString().Trim();
            SalesRep1Name.Text = name;
            if (((DataRow)Session["ActiveCustomer"])["SalesRepresentative"].ToString().TrimEnd() == SalesRep1Name.ToString().TrimEnd())
            {
                SalesrepCommission1.Text = ((DataRow)Session["ActiveCustomer"])["Commissionforrep1"].ToString().TrimEnd();
            }
            else
            {
                SalesrepCommission1.Text = result.Rows[0]["Commissionforrep1"].ToString().Trim();
            }
        }
        else
        {
            SalesRep1Name.Text = "";
            SalesrepCommission1.Text = "";
        }

        SalesrepCommission1.Visible = Session["RepID"].ToString().Trim() == SalesRep1.Text.Trim();
        TextBox1.Visible = !SalesrepCommission1.Visible;

        ScriptManager.GetCurrent(this.Page).SetFocus(SalesRep1.ClientID);
    }

    protected void SalesRep2_TextChanged(object sender, EventArgs e)
    {
        string dele = "|";
        string[] strArray = SalesRep2.Text.Split(dele.ToCharArray());
        SalesRep2.Text = strArray[0].TrimEnd().ToUpper();
        
        AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
        AriaDataObjectPointer pointer = new AriaDataObjectPointer();
        pointer.AddKeyField("SalesRepCode", SalesRep2.Text.Trim().PadRight(3));
        DataTable result = provider.GetBusinessObject("", "", "99", "CRM.SalesRep", pointer);
        if (result.Rows.Count > 0 && result.Rows[0]["SalesRepCode"].ToString().TrimEnd() == SalesRep2.Text)
        {
            string name = result.Rows[0]["Name"].ToString().Trim();
            SalesRep2Name.Text = name;
            
            if (((DataRow)Session["ActiveCustomer"])["SalesRep2"].ToString().TrimEnd() == SalesRep2Name.ToString().TrimEnd())
            {
                SalesrepCommission1.Text = ((DataRow)Session["ActiveCustomer"])["salesrepCommission2"].ToString().TrimEnd();
            }
            else
            {
                SalesrepCommission2.Text = result.Rows[0]["Commissionforrep1"].ToString().Trim();
            }
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);
        }
        else
        {
            SalesRep2.Text = "";
            SalesRep2Name.Text = "";
            SalesrepCommission2.Text = "";
            ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();alert('Invalid Sales Rep!')", true);
        }

        SalesrepCommission2.Visible = Session["RepID"].ToString().Trim() == SalesRep2.Text.Trim();
        TextBox2.Visible = !SalesrepCommission2.Visible;

        ScriptManager.GetCurrent(this.Page).SetFocus(SalesRep2.ClientID);
    }

    private void LoadFields()
    {
        OrderDataTableController orderController = ((OrderDataTableController)Session["Order"]);
        DataTable datatable = orderController.GetOrderHeaderData();
        this.SalesRep1.Text = datatable.Rows[0]["SalesRep1"].ToString();
        this.SalesRep2.Text = datatable.Rows[0]["SalesRep2"].ToString();
        this.SalesRep1Name.Text = datatable.Rows[0]["SalesRep1Name"].ToString();
        this.SalesRep2Name.Text = datatable.Rows[0]["SalesRep2Name"].ToString();
    }

    public void SaveFields()
    {
        OrderDataTableController orderController = ((OrderDataTableController)Session["Order"]);
        DataTable datatable = orderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];
        r["SalesRep1"] = this.SalesRep1.Text;
        r["SalesRep2"] = this.SalesRep2.Text;
    }

    protected void btnSalesRep2_Click(object sender, EventArgs e)
    {
        SalesRep2.Text = "*";
        ScriptManager.GetCurrent(this.Page).SetFocus(SalesRep2.ClientID);
    }
}
