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
/// Summary description for CustomerNameAutoComplete
/// </summary>
[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
[System.Web.Script.Services.ScriptService]
public class StoreAutoComplete : System.Web.Services.WebService
{

    public StoreAutoComplete()
    {

        //Uncomment the following line if using designed components 
        //InitializeComponent(); 
    }

    [WebMethod(EnableSession = true)]
    public string[] GetCompletionList(string prefixText, int count)
    {

        AriaBusinessObjectAdapter object1 = new AriaBusinessObjectAdapter();

        Aria.DataTypes.AriaConditionList pointer = new Aria.DataTypes.AriaConditionList();
        Aria.DataTypes.AriaCondition condition = new Aria.DataTypes.AriaCondition();
        condition.LeftHandSide = new Aria.DataTypes.AriaStandardDataType();
        condition.LeftHandSide.AddPropertyDataPathEntry("Value", "Account");
        condition.RightHandSide = new Aria.DataTypes.AriaStandardDataType();
        if (prefixText.Trim() == "*")
            prefixText = "";
        prefixText = prefixText.Trim().ToUpper();
        ((AriaStandardDataType)condition.RightHandSide).Value = prefixText;
        pointer.Items.Add(condition);

        AriaCondition condition1 = new AriaCondition();
        condition1.RightHandSide = new AriaStandardDataType();
        condition1.LeftHandSide = new AriaStandardDataType();
        condition1.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition1.RightHandSide).Value = Session["AccountID"].ToString().ToUpper();
        pointer.Items.Add(condition1);

        DataTable storesDT = object1.GetBusinessObjects("Aria", "", "99", "CRM.Customer.Store", pointer);


        int size = storesDT.Rows.Count;

        List<string> r = new List<string>(size);

        for (int i = 0; i < size && i < 256; i++)
        {
            Object[] rowItems = storesDT.Rows[i].ItemArray;
            string s = Formatter.FormateString(0, rowItems[0].ToString().Trim());

            for (int j = 1; j < rowItems.GetLength(0); j++)
            {
                s += " | " + Formatter.FormateString(j, rowItems[j].ToString().Trim().ToLower());
            }
            r.Add(s);
        }

        if (size == 1 && storesDT.Rows[0]["Store"].ToString().TrimEnd() == prefixText.TrimEnd())
        {
            return null;
        }

        if (size > 256)
        {
            r.Add("The number of matched records excced than maximum, please type more characters");
        }

        if (size == 0)
        {
            r.Add("No matched records!");
        }

        return r.ToArray();
    }

    private string FormateString(int index, string p)
    {
        switch (index)
        {
            case 0:
                p = p.PadRight(5);
                p += "\t";
                break;
            case 1:
                p = p.PadRight(8);
                p += "\t";
                break;
            case 2:
                if (p.Length <= 7)
                {
                    p = p.PadRight(30);
                    p += "\t\t";
                }
                else
                {
                    p = p.PadRight(30);
                    p += "\t";
                }
                break;
            case 3:
                p = p.PadRight(20);
                p += "\t";
                break;
            case 4:
                p = p.PadRight(9);
                p += "\t";
                break;
            case 5:
                p = p.PadRight(16);
                break;
        }



        return p;
    }
    private string Pad(string str, int size)
    {
        for (int i = 0; i < str.Length; i++)
            str += " ";
        return str;
    }
}

