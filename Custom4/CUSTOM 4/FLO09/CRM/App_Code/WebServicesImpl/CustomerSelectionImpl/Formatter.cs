using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

/// <summary>
/// Summary description for Formatter
/// </summary>
public class Formatter
{
    public Formatter()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    public static string FormateString(int index, string p)
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
                else if (p.Length <= 13)
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
                if (p.Length < 10)
                    p += "\t";
                break;
            case 5:
                p = p.PadRight(16);
                break;
        }



        return p;
    }
}
