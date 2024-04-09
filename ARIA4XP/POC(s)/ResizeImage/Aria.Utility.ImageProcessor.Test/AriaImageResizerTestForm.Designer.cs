namespace Aria.Utility.ImageProcessor.Test
{
    partial class AriaImageResizerTestForm
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
            this.labelFileName = new System.Windows.Forms.Label();
            this.textBoxFileName = new System.Windows.Forms.TextBox();
            this.buttonSelectFile = new System.Windows.Forms.Button();
            this.trackBarPercent = new System.Windows.Forms.TrackBar();
            this.labelPercent = new System.Windows.Forms.Label();
            this.panel1 = new System.Windows.Forms.Panel();
            this.pictureBoxFrom = new System.Windows.Forms.PictureBox();
            this.panel2 = new System.Windows.Forms.Panel();
            this.pictureBoxTo = new System.Windows.Forms.PictureBox();
            this.textBoxNewFileName = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.comboBoxSmoothingMode = new System.Windows.Forms.ComboBox();
            this.comboBoxInterpolationMode = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.comboBoxPixelOffsetMode = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.trackBarPercent)).BeginInit();
            this.panel1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBoxFrom)).BeginInit();
            this.panel2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBoxTo)).BeginInit();
            this.SuspendLayout();
            // 
            // labelFileName
            // 
            this.labelFileName.AutoSize = true;
            this.labelFileName.Location = new System.Drawing.Point(14, 21);
            this.labelFileName.Name = "labelFileName";
            this.labelFileName.Size = new System.Drawing.Size(57, 13);
            this.labelFileName.TabIndex = 0;
            this.labelFileName.Text = "File Name:";
            // 
            // textBoxFileName
            // 
            this.textBoxFileName.Location = new System.Drawing.Point(116, 16);
            this.textBoxFileName.Name = "textBoxFileName";
            this.textBoxFileName.Size = new System.Drawing.Size(489, 20);
            this.textBoxFileName.TabIndex = 1;
            this.textBoxFileName.Text = "C:\\Users\\mahmoud\\Desktop\\Page-1-Image-1.png";
            // 
            // buttonSelectFile
            // 
            this.buttonSelectFile.Location = new System.Drawing.Point(611, 17);
            this.buttonSelectFile.Name = "buttonSelectFile";
            this.buttonSelectFile.Size = new System.Drawing.Size(32, 21);
            this.buttonSelectFile.TabIndex = 2;
            this.buttonSelectFile.Text = "...";
            this.buttonSelectFile.UseVisualStyleBackColor = true;
            this.buttonSelectFile.Click += new System.EventHandler(this.buttonSelectFile_Click);
            // 
            // trackBarPercent
            // 
            this.trackBarPercent.Location = new System.Drawing.Point(20, 489);
            this.trackBarPercent.Maximum = 100;
            this.trackBarPercent.Minimum = 10;
            this.trackBarPercent.Name = "trackBarPercent";
            this.trackBarPercent.Size = new System.Drawing.Size(618, 45);
            this.trackBarPercent.TabIndex = 7;
            this.trackBarPercent.Value = 10;
            this.trackBarPercent.Scroll += new System.EventHandler(this.trackBarPercent_Scroll);
            // 
            // labelPercent
            // 
            this.labelPercent.AutoSize = true;
            this.labelPercent.Location = new System.Drawing.Point(20, 521);
            this.labelPercent.Name = "labelPercent";
            this.labelPercent.Size = new System.Drawing.Size(27, 13);
            this.labelPercent.TabIndex = 8;
            this.labelPercent.Text = "10%";
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.Controls.Add(this.pictureBoxFrom);
            this.panel1.Location = new System.Drawing.Point(49, 142);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(285, 344);
            this.panel1.TabIndex = 9;
            // 
            // pictureBoxFrom
            // 
            this.pictureBoxFrom.Location = new System.Drawing.Point(0, 0);
            this.pictureBoxFrom.Name = "pictureBoxFrom";
            this.pictureBoxFrom.Size = new System.Drawing.Size(285, 344);
            this.pictureBoxFrom.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.pictureBoxFrom.TabIndex = 7;
            this.pictureBoxFrom.TabStop = false;
            // 
            // panel2
            // 
            this.panel2.AutoScroll = true;
            this.panel2.Controls.Add(this.pictureBoxTo);
            this.panel2.Location = new System.Drawing.Point(349, 142);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(285, 344);
            this.panel2.TabIndex = 10;
            // 
            // pictureBoxTo
            // 
            this.pictureBoxTo.Location = new System.Drawing.Point(0, 0);
            this.pictureBoxTo.Name = "pictureBoxTo";
            this.pictureBoxTo.Size = new System.Drawing.Size(285, 344);
            this.pictureBoxTo.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.pictureBoxTo.TabIndex = 7;
            this.pictureBoxTo.TabStop = false;
            // 
            // textBoxNewFileName
            // 
            this.textBoxNewFileName.Location = new System.Drawing.Point(116, 37);
            this.textBoxNewFileName.Name = "textBoxNewFileName";
            this.textBoxNewFileName.Size = new System.Drawing.Size(488, 20);
            this.textBoxNewFileName.TabIndex = 13;
            this.textBoxNewFileName.Text = "C:\\Users\\mahmoud\\Desktop\\Page-1-Image-1.new.png";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(13, 42);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(82, 13);
            this.label1.TabIndex = 12;
            this.label1.Text = "New File Name:";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(14, 67);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(84, 13);
            this.label2.TabIndex = 14;
            this.label2.Text = "SmoothingMode";
            // 
            // comboBoxSmoothingMode
            // 
            this.comboBoxSmoothingMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxSmoothingMode.FormattingEnabled = true;
            this.comboBoxSmoothingMode.Location = new System.Drawing.Point(116, 63);
            this.comboBoxSmoothingMode.Name = "comboBoxSmoothingMode";
            this.comboBoxSmoothingMode.Size = new System.Drawing.Size(159, 21);
            this.comboBoxSmoothingMode.TabIndex = 15;
            this.comboBoxSmoothingMode.SelectedIndexChanged += new System.EventHandler(this.comboBoxSmoothingMode_SelectedIndexChanged);
            // 
            // comboBoxInterpolationMode
            // 
            this.comboBoxInterpolationMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxInterpolationMode.FormattingEnabled = true;
            this.comboBoxInterpolationMode.Location = new System.Drawing.Point(116, 83);
            this.comboBoxInterpolationMode.Name = "comboBoxInterpolationMode";
            this.comboBoxInterpolationMode.Size = new System.Drawing.Size(159, 21);
            this.comboBoxInterpolationMode.TabIndex = 17;
            this.comboBoxInterpolationMode.SelectedIndexChanged += new System.EventHandler(this.comboBoxInterpolationMode_SelectedIndexChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(14, 86);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(92, 13);
            this.label3.TabIndex = 16;
            this.label3.Text = "InterpolationMode";
            // 
            // comboBoxPixelOffsetMode
            // 
            this.comboBoxPixelOffsetMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBoxPixelOffsetMode.FormattingEnabled = true;
            this.comboBoxPixelOffsetMode.Location = new System.Drawing.Point(116, 101);
            this.comboBoxPixelOffsetMode.Name = "comboBoxPixelOffsetMode";
            this.comboBoxPixelOffsetMode.Size = new System.Drawing.Size(159, 21);
            this.comboBoxPixelOffsetMode.TabIndex = 19;
            this.comboBoxPixelOffsetMode.SelectedIndexChanged += new System.EventHandler(this.comboBoxPixelOffsetMode_SelectedIndexChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(17, 104);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(84, 13);
            this.label4.TabIndex = 18;
            this.label4.Text = "PixelOffsetMode";
            // 
            // AriaImageResizerTestForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(661, 539);
            this.Controls.Add(this.comboBoxPixelOffsetMode);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.comboBoxInterpolationMode);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.comboBoxSmoothingMode);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.textBoxNewFileName);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.panel2);
            this.Controls.Add(this.panel1);
            this.Controls.Add(this.labelPercent);
            this.Controls.Add(this.trackBarPercent);
            this.Controls.Add(this.buttonSelectFile);
            this.Controls.Add(this.textBoxFileName);
            this.Controls.Add(this.labelFileName);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "AriaImageResizerTestForm";
            this.Text = "Aria Image Resizer Test Form";
            this.Load += new System.EventHandler(this.AriaImageResizerTestForm_Load);
            ((System.ComponentModel.ISupportInitialize)(this.trackBarPercent)).EndInit();
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBoxFrom)).EndInit();
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBoxTo)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label labelFileName;
        private System.Windows.Forms.TextBox textBoxFileName;
        private System.Windows.Forms.Button buttonSelectFile;
        private System.Windows.Forms.TrackBar trackBarPercent;
        private System.Windows.Forms.Label labelPercent;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.PictureBox pictureBoxFrom;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.PictureBox pictureBoxTo;
        private System.Windows.Forms.TextBox textBoxNewFileName;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.ComboBox comboBoxSmoothingMode;
        private System.Windows.Forms.ComboBox comboBoxInterpolationMode;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox comboBoxPixelOffsetMode;
        private System.Windows.Forms.Label label4;
    }
}

