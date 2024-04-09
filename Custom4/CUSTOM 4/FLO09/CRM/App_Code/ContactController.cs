using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using Aria.Data.BusinessObject;
using Aria.DataTypes;

/// <summary>
/// Summary description for ContactController
/// </summary>
public class ContactController
{
    public ContactController()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    public DataTable GetContacts()
    {
        string AccountID = HttpContext.Current.Session["AccountID"].ToString().PadRight(8);
        if (HttpContext.Current.Session["StoreID"].ToString().TrimEnd() != "Main" && HttpContext.Current.Session["StoreID"].ToString().TrimEnd() != "Multi")
        {
            AccountID += HttpContext.Current.Session["StoreID"].ToString().PadRight(8);
        }
        else
        {
            AccountID += "".PadRight(8);
        }
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();
        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = AccountID; // User Input
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Customer"); // Field
        conditionList.Items.Add(condition);
        return businessObject.GetBusinessObjects("", "", "99", "CRM.Contact", conditionList);
    }
}
