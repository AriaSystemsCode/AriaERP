using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utilities.ObjectDictionaryBuilder
{
    /// <summary>
    /// This is windows form to enable used from build Object dictionary
    /// </summary>
    public partial class AriaObjectDictionaryBuilderUI : Form
    {   private string  clientId;
        public AriaObjectDictionaryBuilderUI()
        {
            InitializeComponent();
        }

        private void convertToolStripMenuItem_Click(object sender, EventArgs e)
        {
            AriaObjectDictionaryBuilder builder = new AriaObjectDictionaryBuilder();
            builder.BuildDataObjects(clientId);
        }
    }
}