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

public partial class OrderInfoControl : System.Web.UI.UserControl
{
    OrderDataTableController orderHeaderController;

    protected void Page_Load(object sender, EventArgs e)
    {
        AriaBusinessObjectAdapter adapter = new AriaBusinessObjectAdapter();


        if (Session["ActiveCustomerGetOrderInfo"].ToString() == "Yes")
        {
            if (Session["PageMode"].ToString().Equals("Add"))
            {
                try
                {
                    if (((DataRow)Session["ActiveCustomerMainAccount"])["SpecialInstructions"].ToString().Trim().Length == 0)
                    {
                        this.SpecialInstructions.SelectedValue = adapter.GetDefaultCode("", "", "", "SPCINST");
                    }
                    else
                    {
                        this.SpecialInstructions.SelectedValue = ((DataRow)Session["ActiveCustomerMainAccount"])["SpecialInstructions"].ToString();
                    }
                }
                catch (Exception ex)
                {
                }
            }

            if (Session["PageMode"].ToString().Equals("Modify") || Session["PageMode"].ToString().Equals("Copy"))
            {
                LoadFields();
            }

            Session["ActiveCustomerGetOrderInfo"] = "No";
        }

    }

    private void LoadFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];

        DataTable datatable = orderHeaderController.GetOrderHeaderData();


        this.OrderCategory.SelectedValue = datatable.Rows[0]["OrderCategory"].ToString();
        this.ShipVia.SelectedValue = datatable.Rows[0]["ShipVia"].ToString();
        this.SpecialInstructions.SelectedValue = datatable.Rows[0]["SpecialInstructions"].ToString();

        this.SwapOrder.Checked = (bool)datatable.Rows[0]["SwapOrder"];
        this.FixtureInclude.Checked = (bool)datatable.Rows[0]["FixtureInclude"];
        this.DiscountedPrice.Checked = (bool)datatable.Rows[0]["DiscountedPrice"];
        this.GuaranteedSales.Checked = (bool)datatable.Rows[0]["GuaranteedSales"];
        this.SampleOrder.Checked = (bool)datatable.Rows[0]["SampleOrder"];
    }

    public void SaveFields()
    {
        OrderDataTableController orderHeaderController = (OrderDataTableController)Session["Order"];
            
        DataTable datatable = orderHeaderController.GetOrderHeaderData();
        DataRow r = datatable.Rows[0];

        datatable.Rows[0]["OrderCategory"] = OrderCategory.SelectedValue;
        datatable.Rows[0]["ShipVia"] = this.ShipVia.SelectedValue;
        datatable.Rows[0]["SpecialInstructions"] = this.SpecialInstructions.SelectedValue;

        datatable.Rows[0]["SwapOrder"] = SwapOrder.Checked;
        datatable.Rows[0]["FixtureInclude"] = FixtureInclude.Checked;
        datatable.Rows[0]["DiscountedPrice"] = DiscountedPrice.Checked;
        datatable.Rows[0]["GuaranteedSales"] = GuaranteedSales.Checked;
        datatable.Rows[0]["SampleOrder"] = SampleOrder.Checked;
    }
}
