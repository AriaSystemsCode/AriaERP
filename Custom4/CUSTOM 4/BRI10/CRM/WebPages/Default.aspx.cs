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

public partial class WebPages_Default : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        try
        {
            TextBox1.Text = HttpContext.Current.User.Identity.Name.ToString();
        }
        catch (Exception ex)
        {
            TextBox1.Text = ex.Message;
            try
            {
                TextBox2.Text = ex.InnerException.Message;
            }
            catch (Exception ex1)
            {
            }
        }
    }

    protected void Button1_Click1(object sender, EventArgs e)
    {
        try
        {
            TextBox3.Text = "0";
            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            TextBox3.Text = "1";
            AriaConditionList conditionList = new AriaConditionList();
            TextBox3.Text = "2";

            AriaCondition condition = new AriaCondition();
            TextBox3.Text = "3";

            condition.RightHandSide = new AriaStandardDataType();
            TextBox3.Text = "4";

            condition.LeftHandSide = new AriaStandardDataType();
            TextBox3.Text = "5";

            condition.Operator = AriaConditionOperators.GreaterOrEqual;
            TextBox3.Text = "6";

            ((AriaStandardDataType)condition.RightHandSide).Value = "W";
            TextBox3.Text = "7";

            ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Account");
            TextBox3.Text = "8";

            conditionList.Items.Add(condition);
            TextBox3.Text = "9";


            DataTable customers = businessObject.GetBusinessObjects("", "", "99", "CRM.Customer", conditionList);
            TextBox3.Text = "10";

        }
        catch (Exception ex)
        {
            TextBox1.Text = ex.Message;
            try
            {
                TextBox2.Text = ex.InnerException.Message;
            }
            catch (Exception ex1)
            {
            }
        }
    }
    protected void Button2_Click(object sender, EventArgs e)
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = "W".Trim().ToUpper();
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Account");

        conditionList.Items.Add(condition);


        DataTable customers = businessObject.GetBusinessObjects("", "", "99", "CRM.Customer", conditionList);
        customers.Rows[0].Delete();

        GridView1.DataSource = customers;
        GridView1.DataBind();


    }
}
