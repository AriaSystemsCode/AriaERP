using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using Microsoft.Win32;
using System.Xml;
using System.Net.Mail;
using System.Net;

namespace Aria.Configuration.Server.Controls
{
    public partial class EmailSettingsControl : UserControl
    {
        private AriaConfigurationStatusTypes _status;
        public AriaConfigurationStatusTypes Status
        {
            get { return _status; }
            set { _status = value; }
        }

        private string _smtpServer = "";

        private int _smtpPort = 0;

        private string _smtpUserName = "";

        private string _smtpUserPassword = "";

        private string _senderAddress = "";

        private string _senderName = "";

        private ListViewItem _ownItem = null;

        private ToolStripProgressBar _progressBar = null;

        //[START]Ahmed Maher -Date: 2-12-2009
        private bool _ssl = true;

        public bool Ssl
        {
            get { return _ssl; }
            set { _ssl = value; }
        }
        //[END]

        public EmailSettingsControl(ListViewItem ownItem, ToolStripProgressBar progressBar)
        {
            InitializeComponent();

            Anchor = ((AnchorStyles)((((AnchorStyles.Top | AnchorStyles.Bottom) | AnchorStyles.Left) | AnchorStyles.Right)));

            _ownItem = ownItem;

            _progressBar = progressBar;

            Init();
        }

        public void SetStatus()
        {
            _status = AriaConfigurationStatusTypes.Configured;

            errorProvider1.Clear();
            errorProvider2.Clear();
            errorProvider3.Clear();
            errorProvider4.Clear();
            errorProvider5.Clear();
            errorProvider6.Clear();
            errorProvider7.Clear();

            if (textBox9.Text == null || textBox9.Text == "")
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider1.SetError(numericUpDown1, "SMTP Server Not Set");
            }

            if (textBox3.Text == null || textBox3.Text == "")
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider2.SetError(textBox3, "SMTP User Name Not Set");
            }

            if (textBox4.Text == null || textBox4.Text == "")
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider3.SetError(textBox4, "SMTP User Password Not Set");
            }

            if (textBox11.Text == null || textBox11.Text == "" ||
                   textBox2.Text == null || textBox2.Text == "")
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider4.SetError(textBox2, "Sender Address Not Set");
            }

            if (textBox10.Text == null || textBox10.Text == "")
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider5.SetError(textBox10, "Sender Name Not Set");
            }

            if (TestSmtpConnection() == false)
            {
                _status = AriaConfigurationStatusTypes.NotConfigured;

                errorProvider6.SetError(button1, "SMTP Connection failed");
            }

            if (_status == AriaConfigurationStatusTypes.Configured)
            {
                _ownItem.ImageIndex = 0;
            }
            else if (_status == AriaConfigurationStatusTypes.NotConfigured)
            {
                _ownItem.ImageIndex = 1;
            }
            else if (_status == AriaConfigurationStatusTypes.Warning)
            {
                _ownItem.ImageIndex = 2;
            }
        }

        public void Init()
        {
            try
            {
                XmlDocument xmlDocument = new XmlDocument();

                xmlDocument.Load(Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "EmailSettings")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "SmtpServer")
                            {
                                _smtpServer = xmlNode.InnerText;

                                textBox9.Text = _smtpServer;
                            }
                            else if (xmlNode.Name == "SmtpPort")
                            {
                                _smtpPort = int.Parse(xmlNode.InnerText);

                                numericUpDown1.Value = _smtpPort;
                            }
                            else if (xmlNode.Name == "SmtpUserName")
                            {
                                _smtpUserName = xmlNode.InnerText;

                                textBox3.Text = _smtpUserName;
                            }
                            else if (xmlNode.Name == "SmtpUserPassword")
                            {
                                _smtpUserPassword = xmlNode.InnerText;

                                textBox4.Text = _smtpUserPassword;
                            }
                            else if (xmlNode.Name == "SenderAddress")
                            {
                                _senderAddress = xmlNode.InnerText;

                                string[] leftAndRight = _senderAddress.Split('@');

                                textBox11.Text = leftAndRight[0];

                                textBox2.Text = leftAndRight[1];
                            }
                            else if (xmlNode.Name == "SenderName")
                            {
                                _senderName = xmlNode.InnerText;

                                textBox10.Text = _senderName;
                            }
                            //[START]Ahmed Maher -Date: 2-12-2009 -Enable SSL
                            else if (xmlNode.Name == "SSL")
                            {
                                _ssl = Convert.ToBoolean(xmlNode.InnerText);

                                SSLcheckBox.Checked = Convert.ToBoolean(_ssl);
                            }
                            //[END]
                        }

                        break;
                    }
                }
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
            finally
            {
                SetStatus();
            }
        }

        public void Save()
        {
            try
            {
                _progressBar.Value = 0;

                _progressBar.Visible = true;

                //[START]Ahmed Maher -Date: 2-12-2009 -SSL Fix
                //_progressBar.Maximum = 6;
                _progressBar.Maximum = 7;
                //[END]

                XmlDocument xmlDocument = new XmlDocument();

                xmlDocument.Load(Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "EmailSettings")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "SmtpServer")
                            {
                                _smtpServer = xmlNode.InnerText = textBox9.Text;
                            }
                            else if (xmlNode.Name == "SmtpPort")
                            {
                                _smtpPort = (int)numericUpDown1.Value;

                                xmlNode.InnerText = _smtpPort.ToString();
                            }
                            else if (xmlNode.Name == "SmtpUserName")
                            {
                                _smtpUserName = xmlNode.InnerText = textBox3.Text;
                            }
                            else if (xmlNode.Name == "SmtpUserPassword")
                            {
                                _smtpUserPassword = xmlNode.InnerText = textBox4.Text;
                            }
                            else if (xmlNode.Name == "SenderAddress")
                            {
                                _senderAddress = xmlNode.InnerText = textBox11.Text + "@" + textBox2.Text;

                            }
                            else if (xmlNode.Name == "SenderName")
                            {
                                _senderName = xmlNode.InnerText = textBox10.Text;
                            }
                            //[START]Ahmed Maher -Date: 2-12-2009 -Enable SSL
                            else if (xmlNode.Name == "SSL")
                            {
                                _ssl = SSLcheckBox.Checked;
                                xmlNode.InnerText = SSLcheckBox.Checked.ToString();
                            }
                            //[END]

                            _progressBar.Value++;
                        }

                        break;
                    }
                }

                xmlDocument.Save(Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
            finally
            {
                _progressBar.Visible = false;

                button5.Enabled = false;

                SetStatus();
            }
        }

        private void button5_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void textBox9_TextChanged(object sender, EventArgs e)
        {
            //[START]Ahmed Maher -Date: 2-12-2009
            //if (_smtpServer != textBox9.Text ||
            //        _smtpPort != (int)numericUpDown1.Value ||
            //            _smtpUserName != textBox3.Text ||
            //                _smtpUserPassword != textBox4.Text ||
            //                    _senderAddress != textBox11.Text + "@" + textBox2.Text ||
            //                        _senderName != textBox10.Text)
            if (_smtpServer != textBox9.Text ||
                    _smtpPort != (int)numericUpDown1.Value ||
                        _smtpUserName != textBox3.Text ||
                            _smtpUserPassword != textBox4.Text ||
                                _senderAddress != textBox11.Text + "@" + textBox2.Text ||
                                    _senderName != textBox10.Text ||
                                        _ssl != SSLcheckBox.Checked)
            //[END]
            {
                button5.Enabled = true;
                errorProvider7.SetError(button5, "There exist new configurations you enterd not saved");
            }
            else
            {
                errorProvider7.Clear();
                button5.Enabled = false;
            }
        }

        private void button1_Click(object sender, EventArgs e)
        {
            SetStatus();

            if (errorProvider6.GetError(button1) != "")
            {
                MessageBox.Show("SMTP connection failed");
            }
            else
            {
                MessageBox.Show("Test SMTP connection succeeded");
            }
        }

        private bool TestSmtpConnection()
        {
            try
            {
                _progressBar.Value = 0;

                _progressBar.Visible = true;

                _progressBar.Maximum = 10;

                SmtpClient smtp = new SmtpClient();

                _progressBar.Value++;

                smtp.Port = (int)numericUpDown1.Value;

                _progressBar.Value++;

                smtp.Host = textBox9.Text;

                _progressBar.Value++;

                //[START]Ahmed Maher -Date: 2-12-2009
                //smtp.EnableSsl = true;
                //smtp.EnableSsl = SSLcheckBox.Checked;
                smtp.EnableSsl = _ssl;
                //[END]

                _progressBar.Value++;

                smtp.Credentials = new NetworkCredential(textBox3.Text, textBox4.Text);

                _progressBar.Value++;

                MailMessage mailMessage = new MailMessage();

                _progressBar.Value++;

                mailMessage.From = new MailAddress(textBox11.Text + "@" + textBox2.Text, textBox10.Text, Encoding.UTF8);

                _progressBar.Value++;

                mailMessage.To.Add(new MailAddress(mailMessage.From.Address, mailMessage.From.DisplayName, Encoding.UTF8));

                _progressBar.Value++;

                mailMessage.Body = "This mail for testing the SMTP sqlConnection";

                _progressBar.Value++;

                smtp.Send(mailMessage);

                _progressBar.Value++;

                return true;
            }
            catch
            {
                return false;
            }
            finally
            {
                _progressBar.Visible = false;
            }
        }

        //[START]Ahmed Maher -Date: 3-12-2009
        private void SSLcheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (_ssl != SSLcheckBox.Checked)
            {
                button5.Enabled = true;
                errorProvider7.SetError(button5, "There exist new configurations you enterd not saved");
            }
            else
            {
                errorProvider7.Clear();
                button5.Enabled = false;
            }
        }
        //[END]
    }
}
