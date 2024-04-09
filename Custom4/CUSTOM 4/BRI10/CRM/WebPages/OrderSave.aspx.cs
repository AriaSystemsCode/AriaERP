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

public partial class WebPages_OrderSave : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["Sequence"] == null)
        {
            this.Response.Redirect("CustomerSelection.aspx");
        }
        else
        {
            if (Session["PageMode"].ToString().Equals("Modify"))
            {
                OrderNumber.Text = "Your order has been updated successfully.";
            }
            else
            {
                OrderNumber.Text = "Your order has been saved with temporary order # " + Session["Sequence"].ToString().Trim() + " pending customer service approval.";
            }
            Session["Saving"] = null;
        }
    }
    protected void btnAddNewOrder_Click(object sender, EventArgs e)
    {
        this.Response.Redirect("CustomerSelection.aspx");
    }
}
