namespace AriaObjectBrowser.UserControls
{
    partial class AriaThumbnailControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AriaThumbnailControl));
            this.filelabel = new System.Windows.Forms.Label();
            this.filePanel = new System.Windows.Forms.Panel();
            this.innerPanel = new System.Windows.Forms.Panel();
            this.iconPictureBox = new System.Windows.Forms.PictureBox();
            this.fileaxGdViewer = new AxGdViewer4S.AxGdViewer();
            this.filePanel.SuspendLayout();
            this.innerPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.iconPictureBox)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.fileaxGdViewer)).BeginInit();
            this.SuspendLayout();
            // 
            // filelabel
            // 
            this.filelabel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.filelabel.Location = new System.Drawing.Point(0, 182);
            this.filelabel.Name = "filelabel";
            this.filelabel.Size = new System.Drawing.Size(160, 18);
            this.filelabel.TabIndex = 1;
            this.filelabel.Text = "...";
            this.filelabel.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            this.filelabel.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            this.filelabel.Click += new System.EventHandler(this.filelabel_Click);
            // 
            // filePanel
            // 
            this.filePanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.filePanel.Controls.Add(this.innerPanel);
            this.filePanel.Location = new System.Drawing.Point(0, 0);
            this.filePanel.Name = "filePanel";
            this.filePanel.Size = new System.Drawing.Size(160, 175);
            this.filePanel.TabIndex = 2;
            this.filePanel.DoubleClick += new System.EventHandler(this.filePanel_DoubleClick);
            this.filePanel.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            this.filePanel.Click += new System.EventHandler(this.filePanel_Click);
            // 
            // innerPanel
            // 
            this.innerPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.innerPanel.BackColor = System.Drawing.Color.White;
            this.innerPanel.Controls.Add(this.iconPictureBox);
            this.innerPanel.Location = new System.Drawing.Point(4, 5);
            this.innerPanel.Name = "innerPanel";
            this.innerPanel.Size = new System.Drawing.Size(152, 166);
            this.innerPanel.TabIndex = 3;
            this.innerPanel.Visible = false;
            this.innerPanel.DoubleClick += new System.EventHandler(this.innerPanel_DoubleClick);
            this.innerPanel.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            this.innerPanel.Click += new System.EventHandler(this.innerPanel_Click);
            // 
            // iconPictureBox
            // 
            this.iconPictureBox.BackColor = System.Drawing.Color.White;
            this.iconPictureBox.Location = new System.Drawing.Point(33, 44);
            this.iconPictureBox.MinimumSize = new System.Drawing.Size(64, 64);
            this.iconPictureBox.Name = "iconPictureBox";
            this.iconPictureBox.Size = new System.Drawing.Size(64, 64);
            this.iconPictureBox.TabIndex = 1;
            this.iconPictureBox.TabStop = false;
            this.iconPictureBox.Visible = false;
            this.iconPictureBox.DoubleClick += new System.EventHandler(this.iconPictureBox_DoubleClick);
            this.iconPictureBox.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            this.iconPictureBox.Click += new System.EventHandler(this.iconPictureBox_Click);
            // 
            // fileaxGdViewer
            // 
            this.fileaxGdViewer.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fileaxGdViewer.Enabled = true;
            this.fileaxGdViewer.Location = new System.Drawing.Point(4, 5);
            this.fileaxGdViewer.Name = "fileaxGdViewer";
            this.fileaxGdViewer.OcxState = ((System.Windows.Forms.AxHost.State)(resources.GetObject("fileaxGdViewer.OcxState")));
            this.fileaxGdViewer.Size = new System.Drawing.Size(152, 166);
            this.fileaxGdViewer.TabIndex = 0;
            this.fileaxGdViewer.Visible = false;
            this.fileaxGdViewer.DblClickControl += new System.EventHandler(this.fileaxGdViewer_DblClickControl);
            this.fileaxGdViewer.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            // 
            // AriaThumbnailControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.filelabel);
            this.Controls.Add(this.fileaxGdViewer);
            this.Controls.Add(this.filePanel);
            this.Name = "AriaThumbnailControl";
            this.Size = new System.Drawing.Size(160, 200);
            this.DoubleClick += new System.EventHandler(this.AriaThumbnailControl_DoubleClick);
            this.PreviewKeyDown += new System.Windows.Forms.PreviewKeyDownEventHandler(this.AriaThumbnailControl_PreviewKeyDown);
            this.Click += new System.EventHandler(this.AriaThumbnailControl_Click);
            this.filePanel.ResumeLayout(false);
            this.innerPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.iconPictureBox)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.fileaxGdViewer)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Label filelabel;
        private System.Windows.Forms.Panel filePanel;
        private System.Windows.Forms.Panel innerPanel;
        public AxGdViewer4S.AxGdViewer fileaxGdViewer;
        public System.Windows.Forms.PictureBox iconPictureBox;

    }
}
