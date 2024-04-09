using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace Aria.Configuration.Server.Controls
{
    public partial class EnvironmentVariablesControl : UserControl
    {
        private AriaConfigurationStatusTypes _status;
        public AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }
 
        private string _ariaServerConfigurationSettingsFilePath = "";
        
        public EnvironmentVariablesControl()
        {
            InitializeComponent();

            this.Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));
        }

        public void InitControlData()
        {
            this.textBox2.Text = Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine);

            if (this.textBox2.Text == null)
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;
            }
            else if (!File.Exists(this.textBox2.Text))
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;
            }
            else
            {
                _status = AriaConfigurationStatusTypes.Configured;
            }

            this.textBox1.Text = "ARIA_SERVER_CONFIGURATION_PATH";

            this.textBox13.Text = this.textBox2.Text;

            _ariaServerConfigurationSettingsFilePath = this.textBox2.Text;
        }

        private void button5_Click(object sender, EventArgs e)
        {
            if (!File.Exists(this.textBox13.Text))
            {
                MessageBox.Show("The specified file not exists");

                return;
            }

            EnvironmentVariableTarget environmentVariableTarget;

            if (this.radioButton2.Checked == true)
            {
                environmentVariableTarget = EnvironmentVariableTarget.Machine;
            }
            else
            {
                environmentVariableTarget = EnvironmentVariableTarget.User;
            }

            Environment.SetEnvironmentVariable(this.textBox1.Text, this.textBox13.Text, environmentVariableTarget);

            InitControlData();

            MessageBox.Show("Environment saved successfuly");
        }

        private void button1_Click(object sender, EventArgs e)
        {
            DialogResult dialogResult = this.openFileDialog1.ShowDialog();

            if (dialogResult == DialogResult.OK || dialogResult == DialogResult.Yes)
            {
                this.textBox13.Text = this.openFileDialog1.FileName;
            }
        }

        private void textBox13_TextChanged(object sender, EventArgs e)
        {
            if (_ariaServerConfigurationSettingsFilePath != textBox13.Text)
            {
                this.button5.Enabled = true;
            }
            else
            {
                this.button5.Enabled = false;
            }
        }

        private void textBox13_Leave(object sender, EventArgs e)
        {
            if (!File.Exists(textBox13.Text))
            {
                MessageBox.Show("The specified file not exists");

                textBox13.Text = _ariaServerConfigurationSettingsFilePath;
            }
        }

        private void button2_Click(object sender, EventArgs e)
        {
            try
            {
                StreamReader streamReader = File.OpenText(this.textBox2.Text);

                TextEditorForm textEditorForm = new TextEditorForm(streamReader.ReadToEnd());

                textEditorForm.ShowDialog();
            }
            catch (DirectoryNotFoundException ex)
            {
                MessageBox.Show(ex.Message);
            }
            catch (FileNotFoundException ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void button3_Click(object sender, EventArgs e)
        {
            try
            {
                File.Create(Application.StartupPath + @"\" + "configuration settings.xml");

                MessageBox.Show("Successfuly create new configuration settings file");
            }
            catch (Exception ex)
            {
                MessageBox.Show("Faild create new configuration settings file");
            }
        }
    }
}
