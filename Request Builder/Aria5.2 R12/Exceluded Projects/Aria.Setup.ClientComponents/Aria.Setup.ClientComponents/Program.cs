using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Data;

namespace Aria.Setup.ClientComponents
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new Form1());
            //DataTable table = new DataTable("AriaAssemblyNames");
            //table.Columns.Add("dll_short_name");
            //table.Columns.Add("dll_long_name");

            //DataRow row = table.NewRow();
            //row[0] = "OBJDIC.dll";
            //row[1] = "Aria.ObjectDictionary.UI.dll";
            //table.Rows.Add(row);
            //table.WriteXml("c:\\ASM.xml");
        }
    }
}