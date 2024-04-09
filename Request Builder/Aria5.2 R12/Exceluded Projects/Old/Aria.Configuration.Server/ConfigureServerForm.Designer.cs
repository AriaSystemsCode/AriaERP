namespace Aria.Configuration.Server
{
    partial class ConfigureServerForm
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ConfigureServerForm));
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.panel2 = new System.Windows.Forms.Panel();
            this.button3 = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.panel1 = new System.Windows.Forms.Panel();
            this.button2 = new System.Windows.Forms.Button();
            this.panel4 = new System.Windows.Forms.Panel();
            this.buttonClients = new System.Windows.Forms.Button();
            this.buttonAria4XPSettings = new System.Windows.Forms.Button();
            this.buttonDatabaseSetup = new System.Windows.Forms.Button();
            this.buttonRequestHandlerService = new System.Windows.Forms.Button();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.toolStripProgressBar1 = new System.Windows.Forms.ToolStripProgressBar();
            this.panel3 = new System.Windows.Forms.Panel();
            this.label1 = new System.Windows.Forms.Label();
            this.panel1.SuspendLayout();
            this.panel4.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            this.panel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "");
            this.imageList1.Images.SetKeyName(1, "");
            this.imageList1.Images.SetKeyName(2, "Warning.bmp");
            this.imageList1.Images.SetKeyName(3, "Alert.bmp");
            // 
            // panel2
            // 
            this.panel2.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panel2.AutoScroll = true;
            this.panel2.BackColor = System.Drawing.Color.White;
            this.panel2.Location = new System.Drawing.Point(188, 57);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(546, 426);
            this.panel2.TabIndex = 2;
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(13, 8);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(89, 22);
            this.button3.TabIndex = 5;
            this.button3.Text = "Help";
            this.button3.UseVisualStyleBackColor = true;
            // 
            // button1
            // 
            this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.button1.Location = new System.Drawing.Point(446, 8);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(89, 22);
            this.button1.TabIndex = 3;
            this.button1.Text = "Exit";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // panel1
            // 
            this.panel1.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panel1.BackColor = System.Drawing.Color.White;
            this.panel1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.panel1.Controls.Add(this.button2);
            this.panel1.Controls.Add(this.button3);
            this.panel1.Controls.Add(this.button1);
            this.panel1.Location = new System.Drawing.Point(188, 484);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(546, 39);
            this.panel1.TabIndex = 6;
            // 
            // button2
            // 
            this.button2.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.button2.Location = new System.Drawing.Point(351, 8);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(89, 22);
            this.button2.TabIndex = 6;
            this.button2.Text = "Save All";
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Visible = false;
            // 
            // panel4
            // 
            this.panel4.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.panel4.BackColor = System.Drawing.Color.White;
            this.panel4.Controls.Add(this.buttonClients);
            this.panel4.Controls.Add(this.buttonAria4XPSettings);
            this.panel4.Controls.Add(this.buttonDatabaseSetup);
            this.panel4.Controls.Add(this.buttonRequestHandlerService);
            this.panel4.Controls.Add(this.statusStrip1);
            this.panel4.Location = new System.Drawing.Point(-1, 57);
            this.panel4.Name = "panel4";
            this.panel4.Size = new System.Drawing.Size(188, 466);
            this.panel4.TabIndex = 8;
            // 
            // buttonClients
            // 
            this.buttonClients.BackColor = System.Drawing.Color.White;
            this.buttonClients.Enabled = false;
            this.buttonClients.FlatAppearance.BorderSize = 0;
            this.buttonClients.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonClients.Image = ((System.Drawing.Image)(resources.GetObject("buttonClients.Image")));
            this.buttonClients.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonClients.Location = new System.Drawing.Point(4, 80);
            this.buttonClients.Name = "buttonClients";
            this.buttonClients.Size = new System.Drawing.Size(181, 35);
            this.buttonClients.TabIndex = 28;
            this.buttonClients.Text = "Clients";
            this.buttonClients.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonClients.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.buttonClients.UseVisualStyleBackColor = false;
            this.buttonClients.Click += new System.EventHandler(this.buttonClients_Click);
            // 
            // buttonAria4XPSettings
            // 
            this.buttonAria4XPSettings.BackColor = System.Drawing.Color.White;
            this.buttonAria4XPSettings.FlatAppearance.BorderSize = 0;
            this.buttonAria4XPSettings.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonAria4XPSettings.Image = global::Aria.Configuration.Server.Properties.Resources.Pass;
            this.buttonAria4XPSettings.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonAria4XPSettings.Location = new System.Drawing.Point(4, 118);
            this.buttonAria4XPSettings.Name = "buttonAria4XPSettings";
            this.buttonAria4XPSettings.Size = new System.Drawing.Size(181, 35);
            this.buttonAria4XPSettings.TabIndex = 27;
            this.buttonAria4XPSettings.Text = "  Aria 4 XP";
            this.buttonAria4XPSettings.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonAria4XPSettings.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.buttonAria4XPSettings.UseVisualStyleBackColor = false;
            this.buttonAria4XPSettings.Click += new System.EventHandler(this.buttonAria4XPSettings_Click);
            // 
            // buttonDatabaseSetup
            // 
            this.buttonDatabaseSetup.BackColor = System.Drawing.Color.White;
            this.buttonDatabaseSetup.FlatAppearance.BorderSize = 0;
            this.buttonDatabaseSetup.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonDatabaseSetup.Image = global::Aria.Configuration.Server.Properties.Resources.Pass;
            this.buttonDatabaseSetup.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonDatabaseSetup.Location = new System.Drawing.Point(4, 4);
            this.buttonDatabaseSetup.Name = "buttonDatabaseSetup";
            this.buttonDatabaseSetup.Size = new System.Drawing.Size(181, 35);
            this.buttonDatabaseSetup.TabIndex = 26;
            this.buttonDatabaseSetup.Text = "  Database Setup";
            this.buttonDatabaseSetup.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonDatabaseSetup.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.buttonDatabaseSetup.UseVisualStyleBackColor = false;
            this.buttonDatabaseSetup.Click += new System.EventHandler(this.buttonDatabaseSetup_Click);
            // 
            // buttonRequestHandlerService
            // 
            this.buttonRequestHandlerService.BackColor = System.Drawing.Color.White;
            this.buttonRequestHandlerService.FlatAppearance.BorderSize = 0;
            this.buttonRequestHandlerService.FlatStyle = System.Windows.Forms.FlatStyle.Flat;
            this.buttonRequestHandlerService.Image = ((System.Drawing.Image)(resources.GetObject("buttonRequestHandlerService.Image")));
            this.buttonRequestHandlerService.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonRequestHandlerService.Location = new System.Drawing.Point(4, 42);
            this.buttonRequestHandlerService.Name = "buttonRequestHandlerService";
            this.buttonRequestHandlerService.Size = new System.Drawing.Size(181, 35);
            this.buttonRequestHandlerService.TabIndex = 25;
            this.buttonRequestHandlerService.Text = "  Request Handler Service";
            this.buttonRequestHandlerService.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.buttonRequestHandlerService.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
            this.buttonRequestHandlerService.UseVisualStyleBackColor = false;
            this.buttonRequestHandlerService.Click += new System.EventHandler(this.buttonRequestHandlerService_Click);
            // 
            // statusStrip1
            // 
            this.statusStrip1.BackColor = System.Drawing.Color.White;
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripProgressBar1});
            this.statusStrip1.Location = new System.Drawing.Point(0, 444);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(188, 22);
            this.statusStrip1.SizingGrip = false;
            this.statusStrip1.TabIndex = 24;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // toolStripProgressBar1
            // 
            this.toolStripProgressBar1.Name = "toolStripProgressBar1";
            this.toolStripProgressBar1.Size = new System.Drawing.Size(185, 16);
            this.toolStripProgressBar1.Step = 1;
            this.toolStripProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous;
            this.toolStripProgressBar1.Visible = false;
            // 
            // panel3
            // 
            this.panel3.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panel3.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("panel3.BackgroundImage")));
            this.panel3.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.panel3.Controls.Add(this.label1);
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(735, 56);
            this.panel3.TabIndex = 7;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.BackColor = System.Drawing.Color.Transparent;
            this.label1.Font = new System.Drawing.Font("Tahoma", 14.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(12, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(460, 23);
            this.label1.TabIndex = 0;
            this.label1.Text = "Aria 5.0 Request Server Configuration Manager";
            // 
            // ConfigureServerForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.BackColor = System.Drawing.Color.Gainsboro;
            this.ClientSize = new System.Drawing.Size(735, 524);
            this.Controls.Add(this.panel4);
            this.Controls.Add(this.panel3);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.panel2);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "ConfigureServerForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Aria 5.0 Request Server Configuration Manager";
            this.panel1.ResumeLayout(false);
            this.panel4.ResumeLayout(false);
            this.panel4.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.panel3.ResumeLayout(false);
            this.panel3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.Button button3;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Panel panel4;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripProgressBar toolStripProgressBar1;
        private System.Windows.Forms.Button buttonRequestHandlerService;
        private System.Windows.Forms.Button buttonDatabaseSetup;
        private System.Windows.Forms.Button buttonAria4XPSettings;
        private System.Windows.Forms.Button buttonClients;
    }
}

