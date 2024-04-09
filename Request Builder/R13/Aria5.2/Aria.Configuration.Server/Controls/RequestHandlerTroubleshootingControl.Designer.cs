namespace Aria.Configuration.Server.Controls
{
    partial class RequestHandlerTroubleshootingControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(RequestHandlerTroubleshootingControl));
            this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
            this.prbCheckProgress = new System.Windows.Forms.ProgressBar();
            this.lblAria4SharedPath = new System.Windows.Forms.Label();
            this.btnStartCheck = new System.Windows.Forms.Button();
            this.imglChecks = new System.Windows.Forms.ImageList(this.components);
            this.btnPathBrowse = new System.Windows.Forms.Button();
            this.txtAria4SharedPath = new System.Windows.Forms.TextBox();
            this.trvChecks = new System.Windows.Forms.TreeView();
            this.SuspendLayout();
            // 
            // openFileDialog1
            // 
            this.openFileDialog1.FileName = "openFileDialog1";
            // 
            // prbCheckProgress
            // 
            this.prbCheckProgress.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.prbCheckProgress.Location = new System.Drawing.Point(4, 393);
            this.prbCheckProgress.Name = "prbCheckProgress";
            this.prbCheckProgress.Size = new System.Drawing.Size(434, 23);
            this.prbCheckProgress.TabIndex = 12;
            // 
            // lblAria4SharedPath
            // 
            this.lblAria4SharedPath.AutoSize = true;
            this.lblAria4SharedPath.Location = new System.Drawing.Point(7, 9);
            this.lblAria4SharedPath.Name = "lblAria4SharedPath";
            this.lblAria4SharedPath.Size = new System.Drawing.Size(110, 13);
            this.lblAria4SharedPath.TabIndex = 9;
            this.lblAria4SharedPath.Text = "Aria4xp Shared Path :";
            // 
            // btnStartCheck
            // 
            this.btnStartCheck.Anchor = System.Windows.Forms.AnchorStyles.Bottom;
            this.btnStartCheck.Location = new System.Drawing.Point(448, 393);
            this.btnStartCheck.Name = "btnStartCheck";
            this.btnStartCheck.Size = new System.Drawing.Size(75, 23);
            this.btnStartCheck.TabIndex = 8;
            this.btnStartCheck.Text = "Start Check";
            this.btnStartCheck.UseVisualStyleBackColor = true;
            this.btnStartCheck.Click += new System.EventHandler(this.btnStartCheck_Click);
            // 
            // imglChecks
            // 
            this.imglChecks.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imglChecks.ImageStream")));
            this.imglChecks.TransparentColor = System.Drawing.Color.Transparent;
            this.imglChecks.Images.SetKeyName(0, "ProgressWarn.ico");
            this.imglChecks.Images.SetKeyName(1, "ProgressSuccess.ico");
            this.imglChecks.Images.SetKeyName(2, "ProgressError.ico");
            // 
            // btnPathBrowse
            // 
            this.btnPathBrowse.Location = new System.Drawing.Point(496, 3);
            this.btnPathBrowse.Name = "btnPathBrowse";
            this.btnPathBrowse.Size = new System.Drawing.Size(27, 23);
            this.btnPathBrowse.TabIndex = 11;
            this.btnPathBrowse.Text = "...";
            this.btnPathBrowse.UseVisualStyleBackColor = true;
            this.btnPathBrowse.Visible = false;
            this.btnPathBrowse.Click += new System.EventHandler(this.btnPathBrowse_Click);
            // 
            // txtAria4SharedPath
            // 
            this.txtAria4SharedPath.Location = new System.Drawing.Point(121, 5);
            this.txtAria4SharedPath.Name = "txtAria4SharedPath";
            this.txtAria4SharedPath.ReadOnly = true;
            this.txtAria4SharedPath.Size = new System.Drawing.Size(402, 20);
            this.txtAria4SharedPath.TabIndex = 10;
            this.txtAria4SharedPath.Leave += new System.EventHandler(this.txtAria4SharedPath_Leave);
            // 
            // trvChecks
            // 
            this.trvChecks.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.trvChecks.FullRowSelect = true;
            this.trvChecks.ImageIndex = 0;
            this.trvChecks.ImageList = this.imglChecks;
            this.trvChecks.Location = new System.Drawing.Point(4, 28);
            this.trvChecks.Name = "trvChecks";
            this.trvChecks.SelectedImageIndex = 0;
            this.trvChecks.ShowNodeToolTips = true;
            this.trvChecks.Size = new System.Drawing.Size(519, 357);
            this.trvChecks.TabIndex = 7;
            // 
            // RequestHandlerTroubleshootingControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.prbCheckProgress);
            this.Controls.Add(this.lblAria4SharedPath);
            this.Controls.Add(this.btnStartCheck);
            this.Controls.Add(this.btnPathBrowse);
            this.Controls.Add(this.txtAria4SharedPath);
            this.Controls.Add(this.trvChecks);
            this.Name = "RequestHandlerTroubleshootingControl";
            this.Size = new System.Drawing.Size(528, 426);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.OpenFileDialog openFileDialog1;
        private System.Windows.Forms.ProgressBar prbCheckProgress;
        private System.Windows.Forms.Label lblAria4SharedPath;
        private System.Windows.Forms.Button btnStartCheck;
        private System.Windows.Forms.ImageList imglChecks;
        private System.Windows.Forms.Button btnPathBrowse;
        private System.Windows.Forms.TextBox txtAria4SharedPath;
        private System.Windows.Forms.TreeView trvChecks;
    }
}
