using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Aria.DataTypes.ObjectDictionary;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Collections;
using Aria.Data.BusinessObject;


namespace Aria.Utilities.ObjectDictionary
{
    public partial class SelectObject : Form
    {
        public SelectObject()
        {
            InitializeComponent();
        }
        private string clientId;
        private void SelectObject_Load(object sender, EventArgs e)
        {
            AriaObjectDictionaryDBCentric dictionary = new AriaObjectDictionaryDBCentric();
            ArrayList Objects = dictionary.LoadAriaObjects(new Aria.Data.AriaDbConnection("", ""), clientId);
            for (int index = 0; index < Objects.Count; index++)
            {
                cboObjects.Items.Add(((AriaObject)Objects[index]).ObjectName); ;
            }
            cboObjects.SelectedIndex = 0;
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {

        }

        private void button1_Click(object sender, EventArgs e)
        {
            Aria.DataTypes.AriaDataObjectPointer xx = new Aria.DataTypes.AriaDataObjectPointer();
            xx.AddKeyField("Style", "A1-TU1  -ARMYDP-1IN");
            AriaBusinessObjectAdapter x = new AriaBusinessObjectAdapter();
            x.GetBusinessObject("", "", "", cboObjects.Text, xx, clientId);
        }
    }
}