//using System;
//using System.Globalization;
//using System.Web.UI.WebControls;
//using DevExpress.Web;
////using DevExpress.ExpressApp;
//using DevExpress.ExpressApp.Web;
//using DevExpress.ExpressApp.Editors;
//using DevExpress.ExpressApp.Web.Editors.ASPx;
//using DevExpress.ExpressApp.Model;
//using Aria5SystemAdmin.Module.BusinessObjects;
//using DevExpress.Utils;
////... 
//namespace Aria5SystemAdmin.Module
//{
//    [PropertyEditor(typeof(String), false)]
//    public class CustomStringEditor : ASPxPropertyEditor
//    {
//        ASPxComboBox ASPxComboBoxControl = null;
//        ASPxDateEdit ASPxDateEditControl = null;
//        ASPxSpinEdit ASPxSpinEditControl = null;
//        ASPxCheckBox ASPxCheckBoxControl = null;
//        ASPxTextBox ASPxTextEditControl = null;
//        ASPxImagePropertyEditor ASPxImagePropertyEditorControl = null;
//        ASPxListEdit ASPxListEditControl = null;
//        ASPxColorPropertyEditor ASPxColorPropertyEditorControl = null;
//        ASPxDecimalPropertyEditor ASPxDecimalPropertyEditorControl = null;

//        public CustomStringEditor(Type objectType, IModelMemberViewItem info)
//            : base(objectType, info) { }

//        protected override WebControl CreateEditModeControlCore()
//        {

//            string ColumnType = "";
//            int VisibleNumber = 0;
//            string PropertyValueOver = "";
//            int width = 0;
//            int decimalPlaces = 0;
//            String [] ValidValues = null;
//            String Format = "";

//            try
//            {
//                if (this.View == null)
//                {
//                    ASPxGridView GridView1 = ((ASPxGridView)((DevExpress.ExpressApp.Web.TableEx)this.Control).NamingContainer.Parent.NamingContainer);
//                    VisibleNumber = GridView1.EditingRowVisibleIndex;
//                    ColumnType = GridView1.GetRowValues(VisibleNumber, "DataType").ToString().ToUpper().TrimEnd()  ;
//                    PropertyValueOver = GridView1.GetRowValues(VisibleNumber, "Value").ToString();

//                    width = int.Parse(GridView1.GetRowValues(VisibleNumber, "Width").ToString() );
//                    decimalPlaces = int.Parse(GridView1.GetRowValues(VisibleNumber, "DecimalPlaces").ToString());

//                    if (GridView1.GetRowValues(VisibleNumber, "Validvalues") != null)
//                    { ValidValues = GridView1.GetRowValues(VisibleNumber, "Validvalues").ToString().Split(','); };

//                    if (GridView1.GetRowValues(VisibleNumber, "Format") != null)
//                    { Format = GridView1.GetRowValues(VisibleNumber, "Format").ToString(); };

                    
//                }
//                else
//                {
//                    ColumnType = ((Setup)this.CurrentObject).DataType.ToString().ToUpper().TrimEnd();
//                    PropertyValueOver = this.PropertyValue.ToString();

//                    width = int.Parse(((Setup)this.CurrentObject).Width.ToString());
//                    decimalPlaces = int.Parse(((Setup)this.CurrentObject).DecimalPlaces.ToString());

//                    if (((Setup)this.CurrentObject).Validvalues != null)
//                    {ValidValues = ((Setup)this.CurrentObject).Validvalues.ToString().Split(',') ;};

//                    if (((Setup)this.CurrentObject).Format != null)
//                    { Format = ((Setup)this.CurrentObject).Format.ToString().TrimEnd(); };
//                }


//            }
//            catch (Exception ex)
//            { };

//            switch (ColumnType)
//            {
//                #region VALIDENTRIES
//                case "VALIDENTRIES":

//                    ASPxComboBoxControl = RenderHelper.CreateASPxComboBox();
//                    ASPxComboBoxControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    if (ValidValues != null)
//                    {
//                        for (int intValid = 0; intValid < ValidValues.Length; intValid++)
//                        {
//                            ((ASPxComboBox)ASPxComboBoxControl).Items.Add(ValidValues[intValid].ToString().TrimEnd());

//                        };
//                    };


//                    return ASPxComboBoxControl;
//                    break;
//                #endregion

//                #region DATE
//                case "DATE":

//                    ASPxDateEditControl = RenderHelper.CreateASPxDateEdit();
//                    ASPxDateEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    ASPxDateEditControl.MaxDate = DateTime.Parse("01/01/2100");
//                    //ASPxDateEdit1.DisplayFormatString = "yyyy/MM/dd";
//                    //ASPxDateEdit1.Value = this.PropertyValue.ToString();
//                    ASPxDateEditControl.Date = DateTime.Parse(PropertyValueOver);
//                    if (string.IsNullOrEmpty(Format) == false)
//                    {
//                        ASPxDateEditControl.DisplayFormatString = Format;
                        
//                    };


//                    return ASPxDateEditControl;
//                    break;
//                #endregion

//                #region NUMERIC
//                case "NUMERIC":
//                    ASPxSpinEditControl = RenderHelper.CreateASPxSpinEdit();
//                    ASPxSpinEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    ASPxSpinEditControl.MaxValue = 100000000;
//                    ASPxSpinEditControl.MinValue = 0;
//                    ASPxSpinEditControl.MaxLength = width;
//                    ASPxSpinEditControl.DecimalPlaces = decimalPlaces;
//                    if (string.IsNullOrEmpty(Format) == false)
//                    {
//                        ASPxSpinEditControl.DisplayFormatString = Format;
                        
//                    };
//                    return ASPxSpinEditControl;
//                    break;

//                #endregion

//                #region NUMBER
//                case "INT":
//                    ASPxSpinEditControl = RenderHelper.CreateASPxSpinEdit();
//                    ASPxSpinEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    ASPxSpinEditControl.MaxValue = 100000000;
//                    ASPxSpinEditControl.MinValue = 0;
//                    ASPxSpinEditControl.MaxLength = width;
//                    ASPxSpinEditControl.DecimalPlaces = decimalPlaces;
//                    if (string.IsNullOrEmpty(Format) == false)
//                    {
//                        ASPxSpinEditControl.DisplayFormatString = Format;

//                    };
//                    return ASPxSpinEditControl;
//                    break;

//                #endregion

//                #region LOGIC
//                case "LOGIC":
//                    ASPxCheckBoxControl = RenderHelper.CreateASPxCheckBox();
//                    ASPxCheckBoxControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    ASPxCheckBoxControl.Text = "";
//                    ASPxCheckBoxControl.Checked = (PropertyValueOver.ToUpper().TrimEnd() == "TRUE") ? true : false;
//                    ASPxCheckBoxControl.ValueChecked = "True";
//                    ASPxCheckBoxControl.ValueUnchecked = "False";
//                    ASPxCheckBoxControl.ValueType = typeof(String);
//                    return ASPxCheckBoxControl;

//                    break;

//                #endregion

//                #region Text
//                default:

//                    ASPxTextEditControl = RenderHelper.CreateASPxTextBox();
//                    ASPxTextEditControl.ValueChanged += new EventHandler(ExtendedEditValueChangedHandler);
//                    ASPxTextEditControl.MaxLength = width;
//                    if (string.IsNullOrEmpty(Format) == false)
//                    {
//                        ASPxTextEditControl.DisplayFormatString = Format;
//                        ASPxTextEditControl.MaskSettings.Mask = Format;
//                    };

//                    return ASPxTextEditControl;
//                #endregion
//            }
//        }


//        public override void BreakLinksToControl(bool unwireEventsOnly)
//        {
//            if (ASPxComboBoxControl != null)
//            {
//                ASPxComboBoxControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }
//            if (ASPxDateEditControl != null)
//            {
//                ASPxDateEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxSpinEditControl != null)
//            {
//                ASPxSpinEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxCheckBoxControl != null)
//            {
//                ASPxCheckBoxControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxTextEditControl != null)
//            {
//                ASPxTextEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxImagePropertyEditorControl != null)
//            {
//                ASPxImagePropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxListEditControl != null)
//            {
//                ASPxListEditControl.ValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxColorPropertyEditorControl != null)
//            {
//                ASPxColorPropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            if (ASPxDecimalPropertyEditorControl != null)
//            {
//                ASPxDecimalPropertyEditorControl.ControlValueChanged -= new EventHandler(ExtendedEditValueChangedHandler);
//            }

//            base.BreakLinksToControl(unwireEventsOnly);

//        }

//        protected override object GetControlValueCore()
//        {
//            if (this.View == null)
//            {
//                return base.GetControlValueCore().ToString();
//            }
//            else
//            { return base.GetControlValueCore(); }

//        }

//        public void ExtendedEditValueChangedHandler(object source, EventArgs e)
//        {
//            if (this.View != null)
//            {
//                if (ASPxComboBoxControl != null)
//                {
//                    base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }
//                if (ASPxDateEditControl != null)
//                {
//                    this.PropertyValue = ((ASPxDateEdit)source).Date.ToString();
//                    return;
//                }

//                if (ASPxSpinEditControl != null)
//                {
//                    this.PropertyValue = ((ASPxSpinEdit)source).Value.ToString();
//                    return;
//                }

//                if (ASPxCheckBoxControl != null)
//                {
//                    this.PropertyValue = ((ASPxCheckBox)source).Value.ToString();
//                    return;
//                }

//                if (ASPxTextEditControl != null)
//                {
//                    base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }

//                if (ASPxImagePropertyEditorControl != null)
//                {
//                    this.PropertyValue = ASPxImagePropertyEditorControl.ControlValue.ToString();
//                    //base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }

//                if (ASPxListEditControl != null)
//                {
//                    this.PropertyValue = ASPxListEditControl.Value.ToString();
//                    //base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }

//                if (ASPxColorPropertyEditorControl != null)
//                {
//                    this.PropertyValue = ASPxColorPropertyEditorControl.ControlValue.ToString();
//                    //base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }

//                if (ASPxDecimalPropertyEditorControl != null)
//                {
//                    this.PropertyValue = ASPxDecimalPropertyEditorControl.ControlValue.ToString();
//                    //base.ExtendedEditValueChangedHandler(source, e);
//                    return;
//                }

//            }

//            base.ExtendedEditValueChangedHandler(source, e);


//        }

//    }
//}