using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace Aria.Utilities.Dictionary
{
    static class Program
    {
        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            if (MessageBox.Show("This program has to run under request machine.", "Confirmation", MessageBoxButtons.OKCancel, MessageBoxIcon.Warning) == DialogResult.OK)
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                    Application.Run(new ObjectDictionaryViewer());

            }
        }
    }
}
