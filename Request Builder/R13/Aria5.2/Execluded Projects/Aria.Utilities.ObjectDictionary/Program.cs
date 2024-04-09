using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Collections;
using Aria.Data.BusinessObject;
using System.Data;

namespace Aria.Utilities.ObjectDictionary
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            //Aria.EnterpriseServices.ObjectDictionary.AriaObjectDataPathsExplorer x = new Aria.EnterpriseServices.ObjectDictionary.AriaObjectDataPathsExplorer();
            //object x1 = x.GetDataPathsTree("Aria4XP.SalesOrderHeader");

            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new ObjectDictionary());
        }
    }
}