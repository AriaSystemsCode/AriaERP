using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.Xml;

namespace Aria.Configuration.Server.Controls
{
    public partial class ComControl : UserControl
    {
        private AriaConfigurationStatusTypes _status;
        public AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }
 
        private ComComponentsControl comComponentsControl = null;

        private ComGeneralSettingsControl comGeneralSettingsControl = null;
        
        private ListViewItem _ownItem = null;

        private ToolStripProgressBar _progressBar;

        public ComControl(ListViewItem ownItem, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownItem = ownItem;

            _progressBar = progressBar;

            comGeneralSettingsControl = new ComGeneralSettingsControl(_ownItem, _progressBar, button3);

            comComponentsControl = new ComComponentsControl(_ownItem, _progressBar);

            button2_Click(button2, new EventArgs());
        }
       
        private void button2_Click(object sender, EventArgs e)
        {
            this.panel1.Controls.Clear();

            this.comGeneralSettingsControl.Size = new Size(this.panel1.Size.Width, this.panel1.Size.Height);

            this.comGeneralSettingsControl.RefreshControl();

            this.panel1.Controls.Add(this.comGeneralSettingsControl);            
        }

        private void button3_Click(object sender, EventArgs e)
        {
            this.panel1.Controls.Clear();

            this.comComponentsControl.Size = new Size(this.panel1.Size.Width, this.panel1.Size.Height);

            this.comComponentsControl.RefreshControl();

            this.panel1.Controls.Add(this.comComponentsControl);            
        }
    }
}