using System;
using System.Web;
using System.Collections;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Collections.Generic;
using Aria.Data.BusinessObject;
using Aria.DataTypes;
using System.Data;
using System.Text;


/// <summary>
/// Summary description for CustomerAccAutoComplete
/// </summary>
[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
[System.Web.Script.Services.ScriptService]
public class CustomerAccAutoComplete : System.Web.Services.WebService {

    public CustomerAccAutoComplete () {

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
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Account"); // Field

       conditionList.Items.Add(condition);

        DataTable xssss = businessObject.GetBusinessObjects("", "", "99", "CRM.Customer", conditionList);

        int size = xssss.Rows.Count;

        List<string> r = new List<string>(size);

        for (int i = 0; i < size && i < 256; i++)
        {
            Object[] rowItems = xssss.Rows[i].ItemArray;
            string s = Formatter.FormateString(0, rowItems[0].ToString().Trim());

            for (int j = 1; j < rowItems.GetLength(0); j++)
            {
                s += " | " + Formatter.FormateString(j, rowItems[j].ToString().Trim().ToLower());                
            }
            r.Add(s);
        }

        if (size == 1 && xssss.Rows[0]["Account"].ToString().TrimEnd() == prefixText.TrimEnd())
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

        //string[] array = r.ToArray();
        //System.Windows.Forms.MessageBox.Show("finish "+array.Length);

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
                if (p.Length <= 8)
                {
                    p = p.PadRight(30);
                    p += "\t\t";
                }
                else if(p.Length <=13)
                {
                    p = p.PadRight(40);
                    p += "\t";
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
                if(p.Length < 10)
                    p += "\t";
                break;
            case 5:
                p = p.PadRight(16);
                break;
        }

        

        return p;
    }

    private string Pad(string str,int size){
        StringBuilder builder = new StringBuilder(str,size);
        for (int i = builder.Length; i < builder.MaxCapacity; i++)
        {
            builder[i] = ' ';
        }

        return builder.ToString();
    }

}
