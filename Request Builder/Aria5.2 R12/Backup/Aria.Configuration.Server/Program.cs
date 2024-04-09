using System;
using System.Collections.Generic;
using System.Windows.Forms;

namespace Aria.Configuration.Server
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            //try
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new ConfigureServerForm());
            }
            //catch (Exception ex)
            {
                //MessageBox.Show(ex.Message);
                //MessageBox.Show(ex.GetBaseException().Message);
                //MessageBox.Show(ex.StackTrace);
            }
        }
    }
}