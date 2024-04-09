using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public class Utility
{   
    public static void EnableAllControlsInsideControl(Control control,bool enabled)
    {
        TraverseInsideControls(control, enabled);
    }

    private static void TraverseInsideControls(Control control, bool enabled)
    {
        int size = control.Controls.Count;
        if (size == 0)
        {
            Enable_DisableControl(control, enabled);
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                TraverseInsideControls(control.Controls[i], enabled);
            }
        }
    }

    private static void Enable_DisableControl(Control control, bool enabled)
    {
        if (control is DropDownList)
        {
            DropDownList dl = (DropDownList)control;
            dl.Enabled = enabled;
        }
        else if (control is TextBox)
        {
            TextBox tb = (TextBox)control;
            tb.ReadOnly = !enabled;
        }
        else if (control is CheckBox)
        {
            CheckBox cb = (CheckBox)control;
            cb.Enabled = enabled;
        }
    }
}
