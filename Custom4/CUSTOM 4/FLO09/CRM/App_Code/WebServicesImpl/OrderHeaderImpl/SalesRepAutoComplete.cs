using System;
using System.Web;
using System.Collections;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Collections.Generic;
using Aria.Data.BusinessObject;
using Aria.DataTypes;
using System.Data;


/// <summary>
/// Summary description for SalesRepAutoComplete
/// </summary>
[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
[System.Web.Script.Services.ScriptService]
public class SalesRepAutoComplete : System.Web.Services.WebService
{

    public SalesRepAutoComplete()
    {

        //Uncomment the following line if using designed components 
        //InitializeComponent(); 
    }

    [WebMethod(EnableSession = true)]
    public string[] GetCompletionList(string prefixText, int count)
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();
        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = prefixText.ToUpper(); // User Input
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "SalesRepCode"); // Field

        conditionList.Items.Add(condition);

        DataTable xssss = businessObject.GetBusinessObjects("", "", "99", "CRM.SalesRep", conditionList);

        int size = xssss.Rows.Count;

        List<string> r = new List<string>(size);

        for (int i = 0; i < size && i < 256; i++)
        {

            string s = xssss.Rows[i]["SalesRepCode"].ToString().Trim() + "\t" + "|" + xssss.Rows[i]["Name"].ToString().ToLower().Trim();
            r.Add(s);
        }

        if (size == 1 && xssss.Rows[0]["SalesRepCode"].ToString().TrimEnd() == prefixText.TrimEnd())
        {
            return null;
        }

        if (size > 256)
        {
            r.Add("The number of matched records excced than maximum, please type more characters");
        }

        //string[] array = r.ToArray();
        //System.Windows.Forms.MessageBox.Show("finish "+array.Length);
        return r.ToArray();
    }

}

