using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace TestRB
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            TarekTeks.AriaRequestHandler rq = new TarekTeks.AriaRequestHandler();
            rq.info = textBox1.Text;
            textBox2.Text = rq.server;
            rq.loopOverRequests(this.textBox2);
        }

        private void button2_Click(object sender, EventArgs e)
        {
            Aria.DataTypes.ObjectDictionary.AriaArgumentList ariaArgumentList = new Aria.DataTypes.ObjectDictionary.AriaArgumentList();
            Aria.DataTypes.ObjectDictionary.AriaArgument ariaArgument = new Aria.DataTypes.ObjectDictionary.AriaArgument();
            
            Aria.DataTypes.Settings.AriaDataObjectPointerSettings ariaDataObjectPointerSettings = new Aria.DataTypes.Settings.AriaDataObjectPointerSettings();

            ariaArgument.Settings = ariaDataObjectPointerSettings;
            ariaDataObjectPointerSettings.DataObjectName = "Aria4XP.Invoice";
            //ariaDataObjectPointerSettings.DataObjectRevision = "000001";

            
            Aria.DataTypes.AriaDataObjectPointer ariaDataObjectPointer = new Aria.DataTypes.AriaDataObjectPointer();
            Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric ariaObjectDictionaryDBCentric = new Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric();

            Aria.Data.AriaDbConnection ariaDbConnection = new Aria.Data.AriaDbConnection();
            ariaDbConnection.CustomerName = "Aria";
            ariaDbConnection.CompanyName = "55";

            Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter ariaParameterSubstituter = new Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter();
            ariaParameterSubstituter.ClientID = "HEN02";
            ariaDataObjectPointer.AddKeyField("INVOICE", "010000");
            ariaArgument.ParameterName = "Pointer"; 
            ariaArgument.Value = ariaDataObjectPointer;
            ariaArgumentList.Add(ariaArgument);

            ariaParameterSubstituter.ArgumentList = ariaArgumentList;

            ariaParameterSubstituter.Connection = ariaDbConnection;

            ariaParameterSubstituter.BuildArgumentsDataProvider();
            string cOriginalString = ariaParameterSubstituter.GetSubstitutedText("<Account>", ariaParameterSubstituter.ClientID);
        }
    }
}
