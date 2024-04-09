namespace Aria.Configuration.Server.Controls
{
    partial class Aria4XPSettings
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Aria4XPSettings));
            this.label1 = new System.Windows.Forms.Label();
            this.textBoxAria4XPSharedPath = new System.Windows.Forms.TextBox();
            this.buttonBrowse = new System.Windows.Forms.Button();
            this.labelDatabaseSetup = new System.Windows.Forms.Label();
            this.buttonSave = new System.Windows.Forms.Button();
            this.errorProviderInvalidFolder = new System.Windows.Forms.ErrorProvider(this.components);
            this.folderBrowserDialogAria4XPSharedPath = new System.Windows.Forms.FolderBrowserDialog();
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderInvalidFolder)).BeginInit();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(28, 45);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(116, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Aria 4 XP Shared Path:";
            // 
            // textBoxAria4XPSharedPath
            // 
            this.textBoxAria4XPSharedPath.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxAria4XPSharedPath.Location = new System.Drawing.Point(150, 42);
            this.textBoxAria4XPSharedPath.Name = "textBoxAria4XPSharedPath";
            this.textBoxAria4XPSharedPath.ReadOnly = true;
            this.textBoxAria4XPSharedPath.Size = new System.Drawing.Size(299, 20);
            this.textBoxAria4XPSharedPath.TabIndex = 4;
            this.textBoxAria4XPSharedPath.Click += new System.EventHandler(this.textBoxAria4XPSharedPath_TextChanged);
            // 
            // buttonBrowse
            // 
            this.buttonBrowse.BackgroundImage = ((System.Drawing.Image)(resources.GetObject("buttonBrowse.BackgroundImage")));
            this.buttonBrowse.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch;
            this.buttonBrowse.Location = new System.Drawing.Point(455, 42);
            this.buttonBrowse.Name = "buttonBrowse";
            this.buttonBrowse.Size = new System.Drawing.Size(34, 26);
            this.buttonBrowse.TabIndex = 5;
            this.buttonBrowse.UseVisualStyleBackColor = true;
            this.buttonBrowse.Click += new System.EventHandler(this.buttonBrowse_Click);
            // 
            // labelDatabaseSetup
            // 
            this.labelDatabaseSetup.AutoSize = true;
            this.labelDatabaseSetup.BackColor = System.Drawing.Color.Transparent;
            this.labelDatabaseSetup.Font = new System.Drawing.Font("Tahoma", 12.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.labelDatabaseSetup.Location = new System.Drawing.Point(18, 4);
            this.labelDatabaseSetup.Name = "labelDatabaseSetup";
            this.labelDatabaseSetup.Size = new System.Drawing.Size(154, 21);
            this.labelDatabaseSetup.TabIndex = 6;
            this.labelDatabaseSetup.Text = "Aria4XP Settings";
            // 
            // buttonSave
            // 
            this.buttonSave.Enabled = false;
            this.buttonSave.Location = new System.Drawing.Point(397, 390);
            this.buttonSave.Name = "buttonSave";
            this.buttonSave.Size = new System.Drawing.Size(103, 23);
            this.buttonSave.TabIndex = 9;
            this.buttonSave.Text = "Save";
            this.buttonSave.UseVisualStyleBackColor = true;
            this.buttonSave.Click += new System.EventHandler(this.buttonSave_Click);
            // 
            // errorProviderInvalidFolder
            // 
            this.errorProviderInvalidFolder.ContainerControl = this;
            // 
            // Aria4XPSettings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.buttonSave);
            this.Controls.Add(this.labelDatabaseSetup);
            this.Controls.Add(this.buttonBrowse);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBoxAria4XPSharedPath);
            this.Name = "Aria4XPSettings";
            this.Size = new System.Drawing.Size(519, 432);
            ((System.ComponentModel.ISupportInitialize)(this.errorProviderInvalidFolder)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button buttonBrowse;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox textBoxAria4XPSharedPath;
        private System.Windows.Forms.Label labelDatabaseSetup;
        private System.Windows.Forms.Button buttonSave;
        private System.Windows.Forms.ErrorProvider errorProviderInvalidFolder;
        private System.Windows.Forms.FolderBrowserDialog folderBrowserDialogAria4XPSharedPath;
    }
}
