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
using Aria.Xml;
using Aria.DataTypes.Settings;
using Aria.DataTypes.ObjectDictionary;
using System.Collections;
/// <summary>
/// Summary description for PageValidationController
/// </summary>
public class PageValidationController
{
    private AriaBusinessObjectAdapter schema;
    private Page page;
    public PageValidationController(AriaBusinessObjectAdapter schema, Page page)
    {
        this.schema = schema;
        this.page = page;
    }

    public void ApplyValidationRules()
    {
        ArrayList properties = schema.GetBusinessObjectSchema("", "", "CRM.OrderHeader");
        for (int index = 0; index < properties.Count; index++)
        {
            ParseRow((AriaObjectProperty)properties[index]);
        }     

    }


    private void ParseRow(AriaObjectProperty property)
    {
        string propertyName = property.PropertyName;
        string propertyType = property.PropertyType.ToString();
        AriaXmlSerializer xml = new AriaXmlSerializer();
        string propertySettings = xml.ConvertToXml(property.PropertySettings);
        TraverseInsideControls(page, propertyName, propertyType, propertySettings);
    }

    private void TraverseInsideControls(Control control, string propertyName, string propertyType, string propertySettings)
    {
        int size = control.Controls.Count;
        if (size == 0)
        {
            if (control.ID != null && control.ID.Equals(propertyName))
            {
                FillControl(control, propertyName, propertyType, propertySettings);
            }
        }
        else
        {
            for (int i = 0; i < size; i++)
            {
                TraverseInsideControls(control.Controls[i], propertyName, propertyType, propertySettings);
            }
        }
    }



    private void FillControl(Control control, string propertyName, string propertyType, string propertySettings)
    {
        if (control is DropDownList)
        {
            AriaXmlSerializer xmlUtil = new AriaXmlSerializer();
            AriaFieldSettings settings = (AriaFieldSettings)xmlUtil.ConvertFromXml(propertySettings);
            DropDownList list = (DropDownList)control;
            string code = settings.Code;            
            DataTable table = schema.GetCode("", "", "99", code);
            string defaultValue = schema.GetDefaultCode("", "", "99", code);
        //    System.Windows.Forms.MessageBox.Show(control.ID + " " +defaultValue);
            
            list.DataSource = table;
            list.DataTextField = "Description";
            list.DataValueField = "Code";
            list.DataBind();
            list.SelectedValue = defaultValue;
        }
        else if (control is TextBox)
        {
            if (propertyType.Equals("Field"))
            {
                AriaXmlSerializer xmlUtil = new AriaXmlSerializer();
                AriaFieldSettings settings = (AriaFieldSettings)xmlUtil.ConvertFromXml(propertySettings);
                TextBox textBox = (TextBox)control;
                if (settings.DataType == Aria.DataTypes.AriaStandardDataTypes.String)
                {
                    textBox.MaxLength = settings.Width;
                }
                //    System.Windows.Forms.MessageBox.Show(textBox.ID + " MaxLength= " + textBox.MaxLength);

            }
            else if (propertyType.Equals("RelatedField"))
            {

                AriaXmlSerializer xmlUtil = new AriaXmlSerializer();
                AriaRelatedFieldSettings settings = (AriaRelatedFieldSettings)xmlUtil.ConvertFromXml(propertySettings);
                TextBox textBox = (TextBox)control;
                textBox.MaxLength = settings.Width;
                // System.Windows.Forms.MessageBox.Show(textBox.ID + " MaxLength= " + textBox.MaxLength);
            }
        }
        else if (control is CheckBox)
        {
            //   System.Windows.Forms.MessageBox.Show(control.ID + " is CheckBox");
        }
    }

}
