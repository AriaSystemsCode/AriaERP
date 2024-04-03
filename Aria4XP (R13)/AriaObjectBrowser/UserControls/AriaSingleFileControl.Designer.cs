namespace AriaObjectBrowser.UserControls
{
    partial class AriaSingleFileControl
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(AriaSingleFileControl));
            this.fileaxGdViewer = new AxGdViewer4S.AxGdViewer();
            this.innerPanel = new System.Windows.Forms.Panel();
            this.iconPictureBox = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)(this.fileaxGdViewer)).BeginInit();
            this.innerPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.iconPictureBox)).BeginInit();
            this.SuspendLayout();
            // 
            // fileaxGdViewer
            // 
            this.fileaxGdViewer.Enabled = true;
            this.fileaxGdViewer.Location = new System.Drawing.Point(-9, 13);
            this.fileaxGdViewer.Name = "fileaxGdViewer";
            this.fileaxGdViewer.OcxState = ((System.Windows.Forms.AxHost.State)(resources.GetObject("fileaxGdViewer.OcxState")));
            this.fileaxGdViewer.Size = new System.Drawing.Size(153, 87);
            this.fileaxGdViewer.TabIndex = 0;
            this.fileaxGdViewer.Visible = false;
            this.fileaxGdViewer.DblClickControl += new System.EventHandler(this.fileaxGdViewer_DblClickControl);
            // 
            // innerPanel
            // 
            this.innerPanel.BackColor = System.Drawing.Color.White;
            this.innerPanel.Controls.Add(this.iconPictureBox);
            this.innerPanel.Location = new System.Drawing.Point(1, 169);
            this.innerPanel.Name = "innerPanel";
            this.innerPanel.Size = new System.Drawing.Size(154, 139);
            this.innerPanel.TabIndex = 4;
            this.innerPanel.Visible = false;
            // 
            // iconPictureBox
            // 
            this.iconPictureBox.BackColor = System.Drawing.Color.White;
            this.iconPictureBox.Location = new System.Drawing.Point(56, 44);
            this.iconPictureBox.Name = "iconPictureBox";
            this.iconPictureBox.Size = new System.Drawing.Size(46, 46);
            this.iconPictureBox.TabIndex = 1;
            this.iconPictureBox.TabStop = false;
            this.iconPictureBox.Visible = false;
            this.iconPictureBox.DoubleClick += new System.EventHandler(this.iconPictureBox_DoubleClick);
            // 
            // AriaSingleFileControl
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.AutoScroll = true;
            this.BackColor = System.Drawing.Color.Transparent;
            this.Controls.Add(this.innerPanel);
            this.Controls.Add(this.fileaxGdViewer);
            this.Name = "AriaSingleFileControl";
            this.Size = new System.Drawing.Size(160, 381);
            ((System.ComponentModel.ISupportInitialize)(this.fileaxGdViewer)).EndInit();
            this.innerPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.iconPictureBox)).EndInit();
            this.ResumeLayout(false);

        }

        #endregion

        public AxGdViewer4S.AxGdViewer fileaxGdViewer;
        private System.Windows.Forms.Panel innerPanel;
        public System.Windows.Forms.PictureBox iconPictureBox;

    }
}
