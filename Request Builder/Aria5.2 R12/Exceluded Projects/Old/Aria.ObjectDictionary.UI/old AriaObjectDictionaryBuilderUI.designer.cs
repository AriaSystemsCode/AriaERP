namespace Aria.Utilities.Aria40Converter
{
    partial class AriaObjectDictionaryBuilderUI
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
            this.menuStrip1 = new System.Windows.Forms.MenuStrip();
            this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.convertToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.showDictionaryToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.testToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.agentSalesOrderAllToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.pointerToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.menuStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // menuStrip1
            // 
            this.menuStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.testToolStripMenuItem});
            this.menuStrip1.Location = new System.Drawing.Point(0, 0);
            this.menuStrip1.Name = "menuStrip1";
            this.menuStrip1.Size = new System.Drawing.Size(480, 24);
            this.menuStrip1.TabIndex = 0;
            this.menuStrip1.Text = "menuStrip1";
            // 
            // fileToolStripMenuItem
            // 
            this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.convertToolStripMenuItem,
            this.showDictionaryToolStripMenuItem});
            this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
            this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
            this.fileToolStripMenuItem.Text = "File";
            // 
            // convertToolStripMenuItem
            // 
            this.convertToolStripMenuItem.Name = "convertToolStripMenuItem";
            this.convertToolStripMenuItem.Size = new System.Drawing.Size(160, 22);
            this.convertToolStripMenuItem.Text = "Convert";
            this.convertToolStripMenuItem.Click += new System.EventHandler(this.convertToolStripMenuItem_Click);
            // 
            // showDictionaryToolStripMenuItem
            // 
            this.showDictionaryToolStripMenuItem.Name = "showDictionaryToolStripMenuItem";
            this.showDictionaryToolStripMenuItem.Size = new System.Drawing.Size(160, 22);
            this.showDictionaryToolStripMenuItem.Text = "Show Dictionary";
            this.showDictionaryToolStripMenuItem.Click += new System.EventHandler(this.showDictionaryToolStripMenuItem_Click);
            // 
            // testToolStripMenuItem
            // 
            this.testToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.agentSalesOrderAllToolStripMenuItem,
            this.pointerToolStripMenuItem});
            this.testToolStripMenuItem.Name = "testToolStripMenuItem";
            this.testToolStripMenuItem.Size = new System.Drawing.Size(41, 20);
            this.testToolStripMenuItem.Text = "Test";
            this.testToolStripMenuItem.Visible = false;
            // 
            // agentSalesOrderAllToolStripMenuItem
            // 
            this.agentSalesOrderAllToolStripMenuItem.Name = "agentSalesOrderAllToolStripMenuItem";
            this.agentSalesOrderAllToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.agentSalesOrderAllToolStripMenuItem.Text = "Agent Sales Order All";
            this.agentSalesOrderAllToolStripMenuItem.Click += new System.EventHandler(this.agentSalesOrderAllToolStripMenuItem_Click);
            // 
            // pointerToolStripMenuItem
            // 
            this.pointerToolStripMenuItem.Name = "pointerToolStripMenuItem";
            this.pointerToolStripMenuItem.Size = new System.Drawing.Size(185, 22);
            this.pointerToolStripMenuItem.Text = "Pointer";
            this.pointerToolStripMenuItem.Click += new System.EventHandler(this.pointerToolStripMenuItem_Click);
            // 
            // AriaObjectDictionaryBuilderUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(480, 311);
            this.Controls.Add(this.menuStrip1);
            this.MainMenuStrip = this.menuStrip1;
            this.Name = "AriaObjectDictionaryBuilderUI";
            this.Text = "AriaObjectDictionaryBuilderUI";
            this.WindowState = System.Windows.Forms.FormWindowState.Maximized;
            this.Load += new System.EventHandler(this.AriaObjectDictionaryBuilderUI_Load);
            this.menuStrip1.ResumeLayout(false);
            this.menuStrip1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.MenuStrip menuStrip1;
        private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem convertToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem showDictionaryToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem testToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem agentSalesOrderAllToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem pointerToolStripMenuItem;

    }
}