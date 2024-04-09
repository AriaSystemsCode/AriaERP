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

public partial class AddressOrderControl : System.Web.UI.UserControl
{
    OrderDataTableController orderHeaderController;

    protected override void OnInit(EventArgs e)
    {
        base.OnInit(e);

        this.LoadStore();

        Session["MultiPOHeader"] = MultiPO1;

        MultiPO1.Attributes.Add("onclick", "Loading(' Updating...');");
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["ActiveCustomerGetAddressInfo"].ToString() == "Yes")
        {
            string store = Session["StoreID"].ToString().Trim();
            if (store.Equals("Main"))
            {
                AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
                AriaDataObjectPointer pointer = new AriaDataObjectPointer();
                pointer.AddKeyField("Account", Session["AccountID"].ToString().PadRight(5));
                DataTable result = provider.GetBusinessObject("", "", "99", "CRM.Customer", pointer);

                Address12.Text = result.Rows[0]["Address12"].ToString();
                Address22.Text = result.Rows[0]["Address22"].ToString();
                Address32.Text = result.Rows[0]["Address32"].ToString().Trim() + " - " + result.Rows[0]["Address42"].ToString().Trim() + " - " + result.Rows[0]["Address52"].ToString().Trim();

                if (Session["PageMode"].ToString().Equals("Add"))
                {
                    ShiptoAddress1.Text = result.Rows[0]["ShiptoAddress1"].ToString();
                    ShiptoAddress2.Text = result.Rows[0]["ShiptoAddress2"].ToString();
                    ShiptoAddress3.Text = result.Rows[0]["ShiptoAddress3"].ToString().Trim() + " - " + result.Rows[0]["ShiptoAddress4"].ToString().Trim() + " - " + result.Rows[0]["ShiptoAddress5"].ToString().Trim();

                    ShipToDropDownList.SelectedIndex = 0;
                    ShipToDropDownList.Enabled = true;
                    ShipToDropDownList_SelectedIndexChanged(null, null);
                }
            }
            else if (store.Equals("Multi"))
            {
                Address12.Text = "";
                Address22.Text = "";
                Address32.Text = "";

                if (Session["PageMode"].ToString().Equals("Add"))
                {
                    ShiptoAddress1.Text = "";
                    ShiptoAddress2.Text = "";
                    ShiptoAddress3.Text = "";

                    ShipToDropDownList.SelectedIndex = 0;
                    ShipToDropDownList.Enabled = false;
                    ShipToDropDownList_SelectedIndexChanged(null, null);
                }
            }
            else
            {
                AriaBusinessObjectAdapter provider = new AriaBusinessObjectAdapter();
                AriaDataObjectPointer pointer = new AriaDataObjectPointer();
                pointer.AddKeyField("Account", Session["AccountID"].ToString().PadRight(5));
                pointer.AddKeyField("Store", Session["StoreID"].ToString().PadRight(8));
                DataTable result = provider.GetBusinessObject("", "", "99", "CRM.Store", pointer);

                Address12.Text = result.Rows[0]["Address12"].ToString();
                Address22.Text = result.Rows[0]["Address22"].ToString();
                Address32.Text = result.Rows[0]["Address32"].ToString().Trim() + " - " + result.Rows[0]["Address42"].ToString().Trim() + " - " + result.Rows[0]["Address52"].ToString().Trim();

                if (Session["PageMode"].ToString().Equals("Add"))
                {
                    ShiptoAddress1.Text = result.Rows[0]["ShiptoAddress1"].ToString();
                    ShiptoAddress2.Text = result.Rows[0]["ShiptoAddress2"].ToString();
                    ShiptoAddress3.Text = result.Rows[0]["ShiptoAddress3"].ToString().Trim() + " - " + result.Rows[0]["ShiptoAddress4"].ToString().Trim() + " - " + result.Rows[0]["ShiptoAddress5"].ToString().Trim();

                    ShipToDropDownList.SelectedIndex = 0;
                    ShipToDropDownList.Enabled = true;
                    ShipToDropDownList_SelectedIndexChanged(null, null);
                }
            }

            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetAddressInfo"] = "No";
        }

        ((TextBox)Session["CustomerPO1"]).Enabled = !MultiPO1.Checked;
        ((Label)Session["CustomerPO1Label1"]).Enabled = !MultiPO1.Checked;

        ((TextBox)Session["POCode"]).Enabled = MultiPO1.Checked;
        ((Label)Session["POCodeLabel4"]).Enabled = MultiPO1.Checked;
    }

    protected void ShipToDropDownList_SelectedIndexChanged(object sender, EventArgs e)
    {
        if (ShipToDropDownList.SelectedIndex == 1)
        {
            ShiptoAddress1.Enabled = true;
            ShiptoAddress1.Text = ShiptoAddress1.Text.Trim();
            ShiptoAddress2.Enabled = true;
            ShiptoAddress2.Text = ShiptoAddress2.Text.Trim();
            ShiptoAddress3.Enabled = true;
            ShiptoAddress3.Text = ShiptoAddress3.Text.Trim();
        }
        
        if (ShipToDropDownList.SelectedIndex == 0)
        {
            ShiptoAddress1.Enabled = false;
            ShiptoAddress2.Enabled = false;
            ShiptoAddress3.Enabled = false;
        }

        ScriptManager.GetCurrent(this.Page).SetFocus(ShipToDropDownList.ClientID);
    }

    private void LoadStore()
    {
        string store = Session["StoreID"].ToString().Trim();
        if (store.Equals("Main"))
        {
            this.StoreID1.Text = "";
            this.StoreID1.Enabled = false;
            this.MultiShipTo1.Enabled = false;
            this.MultiPO1.Enabled = false;

        }
        else if (store.Equals("Multi"))
        {
            this.MultiShipTo1.Checked = true;
            this.MultiPO1.Enabled = false;
            this.StoreID1.Text = "";
            this.StoreID1.Enabled = false;
            this.MultiShipTo1.Enabled = false;
            this.MultiPO1.Enabled = true;
        }
        else
        {
            this.StoreID1.Text = store;
            this.StoreID1.Enabled = false;
            this.MultiShipTo1.Enabled = false;
            this.MultiPO1.Enabled = false;
        }
    }

    protected void MultiPO1_CheckedChanged(object sender, EventArgs e)
    {
        ScriptManager.RegisterClientScriptBlock(this.Parent, typeof(string), "", "LoadingComplete();", true);

        if (((CheckBox)Session["MultiPOMain"]).Checked != MultiPO1.Checked)
            ((CheckBox)Session["MultiPOMain"]).Checked = MultiPO1.Checked;

        ((TextBox)Session["CustomerPO1"]).Enabled = !MultiPO1.Checked;
        ((Label)Session["CustomerPO1Label1"]).Enabled = !MultiPO1.Checked;

        ((TextBox)Session["POCode"]).Enabled = MultiPO1.Checked;
        ((Label)Session["POCodeLabel4"]).Enabled = MultiPO1.Checked;

        if (MultiPO1.Checked)
        {
            ((TextBox)Session["CustomerPO1"]).Text = "";
            ((TextBox)Session["POCode"]).Text = ((TextBox)Session["CustomerPO1"]).Text;
            ((OrderDataTableController)Session["Order"]).lines.UpdatePO(((TextBox)Session["CustomerPO1"]).Text);
        }
        else
        {
            ((TextBox)Session["POCode"]).Text = ((TextBox)Session["CustomerPO1"]).Text;
            ((OrderDataTableController)Session["Order"]).lines.UpdatePO(((TextBox)Session["CustomerPO1"]).Text);
        }
    }

    public void SaveFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];

        r["ShiptoAddress1"] = this.ShiptoAddress1.Text;
        r["ShiptoAddress2"] = this.ShiptoAddress2.Text;

        string[] Address = this.ShiptoAddress3.Text.Split(new char[] { '-' });
        r["ShiptoAddress3"] = Address[0];
        if (Address.Length > 1) r["ShiptoAddress4"] = Address[1];
        if (Address.Length > 2) r["ShiptoAddress5"] = Address[2];

        datatable.Rows[0]["multi"] = (this.MultiShipTo1.Checked ? "Y" : "N");
        datatable.Rows[0]["multiPO"] = this.MultiPO1.Checked;
        datatable.Rows[0]["alternateshipto"] = this.ShipToDropDownList.SelectedIndex > 0;
    }

    private void LoadFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
        DataTable datatable = orderHeaderController.GetOrderHeaderData();

        this.ShiptoAddress1.Text = datatable.Rows[0]["ShiptoAddress1"].ToString();
        this.ShiptoAddress2.Text = datatable.Rows[0]["ShiptoAddress2"].ToString();
        this.ShiptoAddress3.Text = datatable.Rows[0]["ShiptoAddress3"].ToString().Trim() + " - " + datatable.Rows[0]["ShiptoAddress4"].ToString().Trim() + " - " + datatable.Rows[0]["ShiptoAddress5"].ToString().Trim();

        this.MultiPO1.Checked = datatable.Rows[0]["multi"].ToString().Equals("Y");
        this.MultiShipTo1.Checked =  Convert.ToBoolean(datatable.Rows[0]["multiPO"]);

        if (Convert.ToBoolean(datatable.Rows[0]["alternateshipto"]))
            this.ShipToDropDownList.SelectedIndex = 1;
        else
            this.ShipToDropDownList.SelectedIndex = 0;
    }
}
