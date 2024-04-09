using System;
using System.Web;
using System.Collections;
using System.Web.Services;
using System.Web.Services.Protocols;
using System.Data;
using Aria.Data.BusinessObject;
using System.Web.Services.Protocols;
using System.Web.Script.Services;
using Aria.Data.BusinessObject;
using Aria.DataTypes;


/// <summary>
/// Summary description for Profile
/// </summary>
[WebService(Namespace = "http://tempuri.org/")]
[WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
[ScriptService]
public class Profile : System.Web.Services.WebService
{
    public Profile()
    {
    }

    [WebMethod]

    public string GetProfile(int ProfileNo)
    {
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable profilesNamesDataTable = businessObject.GetProfileCodes("", "", "99", "SO", "ST"); //

        if (ProfileNo < profilesNamesDataTable.Rows.Count)
        {
            DataTable profileListDataTable = businessObject.GetProfileList("", "", "99", profilesNamesDataTable.Rows[ProfileNo]["Code"].ToString());

            string result = "";
            for (int index = 0; index < profileListDataTable.Rows.Count; index++)
            {
                if (index > 0)
                {
                    result += "|";
                }

                result += profileListDataTable.Rows[index]["Value"].ToString().TrimEnd();
            }

            return result;
        }
        else
        {
            return "";
        }
    }


    [WebMethod(true)]
    public string CheckStyle(string styleCode)
    {
        if (styleCode.Trim().Length > 0)
        {
            Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();
            string[] arr = styleCode.Split("|".ToCharArray());
            styleCode = arr[0].TrimEnd().ToUpper();

            pointer.AddKeyField("Style", styleCode);
            AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
            DataTable styleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Style", pointer);

            if ((bool)Session["UseExtenedScale"])
            {
                if (styleRecords.Rows.Count > 0 && styleCode == styleRecords.Rows[0]["Style"].ToString().Substring(0, 8).TrimEnd())
                {
                    return "Yes|" + styleRecords.Rows[0]["Description1"].ToString().TrimEnd() + "|" + 
                            styleRecords.Rows[0]["PriceA"].ToString() + "|" + styleRecords.Rows[0]["PriceA"].ToString() + "|0" +
                            "|" + styleRecords.Rows[0]["OTS1"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS2"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS3"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS4"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS5"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS6"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS7"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS8"].ToString();
                }
                else
                {
                    return "No";
                }
            }
            else
            {
                if (styleRecords.Rows.Count > 0 && styleCode == styleRecords.Rows[0]["Style"].ToString().Substring(0, 12).TrimEnd())
                {
                    return "Yes|" + styleRecords.Rows[0]["Description1"].ToString().TrimEnd() + "|" +
                        styleRecords.Rows[0]["PriceA"].ToString() + "|" + styleRecords.Rows[0]["PriceA"].ToString() + "|0" +
                            "|" + styleRecords.Rows[0]["OTS1"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS2"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS3"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS4"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS5"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS6"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS7"].ToString() +
                            "|" + styleRecords.Rows[0]["OTS8"].ToString();
                }
                else
                {
                    return "No";
                }
            }
        }

        return "No";
    }

    [WebMethod(true)]
    public string GetStyleColors(string styleCode)
    {
        styleCode = styleCode.TrimEnd();

        bool useExtenedScale = (bool)Session["UseExtenedScale"];

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;

        if ((bool)Session["UseExtenedScale"])
        {
            ((AriaStandardDataType)condition.RightHandSide).Value = styleCode.PadRight(8); // User Input
        }
        else
        {
            ((AriaStandardDataType)condition.RightHandSide).Value = styleCode.PadRight(12); // User Input
        }

        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Style"); // Field
        conditionList.Items.Add(condition);

        DataTable resultTable = businessObject.GetBusinessObjects("", "", "99", "CRM.Style.Color", conditionList);

        string result = "";
        for (int index = 0; index < resultTable.Rows.Count; index++)
        {
            if (index > 0)
            {
                result += "|";
            }

            result += resultTable.Rows[index]["Color"].ToString().TrimEnd() + "|" + resultTable.Rows[index]["Description"].ToString().TrimEnd();
        }

        return result;
    }

    [WebMethod(true)]
    public string GetStyleColorSizes(string styleCode, string colorCode)
    {
        styleCode = styleCode.TrimEnd();
        colorCode = colorCode.TrimEnd();

        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        AriaConditionList conditionList = new AriaConditionList();

        AriaCondition condition = new AriaCondition();
        condition.RightHandSide = new AriaStandardDataType();
        condition.LeftHandSide = new AriaStandardDataType();
        condition.Operator = AriaConditionOperators.GreaterOrEqual;
        ((AriaStandardDataType)condition.RightHandSide).Value = styleCode.PadRight(8) + "-" + colorCode.ToString().PadRight(6);
        ((AriaStandardDataType)condition.LeftHandSide).PropertyDataPathDictionary.Add("Value", "Style");

        conditionList.Items.Add(condition);

        DataTable resultTable = businessObject.GetBusinessObjects("", "", "99", "CRM.Style.Color.Scale", conditionList);

        string result = "";
        for (int index = 0; index < resultTable.Rows.Count; index++)
        {
            if (index > 0)
            {
                result += "|";
            }

            result += resultTable.Rows[index]["Scale"].ToString().TrimEnd() + "|" + resultTable.Rows[index]["Description"].ToString().TrimEnd();
        }

        return result;
    }

    [WebMethod(true)]
    public string GetStyleScaleSizes(string scaleCode)
    {
        scaleCode = scaleCode.TrimEnd();

        Aria.DataTypes.AriaDataObjectPointer pointer = new Aria.DataTypes.AriaDataObjectPointer();

        pointer.AddKeyField("Scale", scaleCode.Trim().PadRight(3));
        AriaBusinessObjectAdapter businessObject = new AriaBusinessObjectAdapter();
        DataTable scaleRecords = businessObject.GetBusinessObject("", "", "99", "CRM.Scale", pointer);

        int NumberOfSizes = Convert.ToInt32(scaleRecords.Rows[0]["Numberofsizes"].ToString());

        string result = "";
        for (int index = 0; index < NumberOfSizes; index++)
        {
            if (index > 0)
            {
                result += "|";
            }

            result += scaleRecords.Rows[0]["Size" + (index + 1)].ToString().Trim().TrimEnd();
        }

        return result;
    }

    //[WebMethod(true)]
    //public void insert(string style, int qty1, int qty2, int qty3, int qty4, int qty5, int qty6, int qty7, int qty8,
    //                    int openQty1, int openQty2, int openQty3, int openQty4, int openQty5, int openQty6, int openQty7, int openQty8,
    //                        double grossPrice, double discount, DateTime shipDate, string group, string storeID, string customerPO, string description)
    //{
    //            LinesDataTableController linesController = ((OrderDataTableController)Session["Order"]).lines;
    //            linesController.insert(style, qty1, qty2, qty3, qty4, qty5, qty6, qty7, qty8,
    //                                    openQty1, openQty2, openQty3, openQty4, openQty5, openQty6, openQty7, openQty8,
    //                                        grossPrice, discount, shipDate, group, storeID, customerPO, description);
    //}


    [WebMethod(true)]
    public void insertOrderLine(string style, string color, string size)
    {
        
        LinesDataTableController linesController = ((OrderDataTableController)Session["Order"]).lines;
        linesController.insert(style.TrimEnd().PadRight(8) + "-" + color.TrimEnd().PadRight(6) + "-" + size.TrimEnd().PadRight(3), 0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, DateTime.Now.Date, "", "", "", style);
    }
}