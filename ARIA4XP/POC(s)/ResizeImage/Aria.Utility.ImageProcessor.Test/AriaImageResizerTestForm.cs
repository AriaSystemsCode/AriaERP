using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Text;
using System.Windows.Forms;

namespace Aria.Utility.ImageProcessor.Test
{
    public partial class AriaImageResizerTestForm : Form
    {
        public AriaImageResizerTestForm()
        {
            InitializeComponent();
        }

        private void buttonSelectFile_Click(object sender, EventArgs e)
        {
            OpenFileDialog openFileDialog1 = new OpenFileDialog();
            //openFileDialog1.Filter = "csv files (*.dbf)|*.dbf";

            //openFileDialog1.FilterIndex = 1;

            openFileDialog1.RestoreDirectory = true;

            openFileDialog1.FileName = "";

            if (openFileDialog1.ShowDialog() == DialogResult.OK)
            {

                textBoxFileName.Text = openFileDialog1.FileName;

                pictureBoxFrom.Image = new Bitmap(textBoxFileName.Text);
            }
        }

        private void trackBarPercent_Scroll(object sender, EventArgs e)
        {
            labelPercent.Text = Convert.ToString(trackBarPercent.Value) + "%";
            pictureBoxFrom.Image = new Bitmap(textBoxFileName.Text);

            AriaImageResizer resizer = new AriaImageResizer();

            SmoothingMode smoothingMode = SmoothingMode.None;
            if (comboBoxSmoothingMode.SelectedItem != null)
            {
                smoothingMode = (SmoothingMode)Enum.Parse(typeof(SmoothingMode), comboBoxSmoothingMode.SelectedItem.ToString().Split(')')[1]);
            }

            InterpolationMode interpolationMode = InterpolationMode.Default;
            if (comboBoxInterpolationMode.SelectedItem != null)
            {
                interpolationMode = (InterpolationMode)Enum.Parse(typeof(InterpolationMode), comboBoxInterpolationMode.SelectedItem.ToString().Split(')')[1]);
            }

            PixelOffsetMode pixelOffsetMode = PixelOffsetMode.None;
            if (comboBoxPixelOffsetMode.SelectedItem != null)
            {
                pixelOffsetMode = (PixelOffsetMode)Enum.Parse(typeof(PixelOffsetMode), comboBoxPixelOffsetMode.SelectedItem.ToString().Split(')')[1]);
            }

            pictureBoxTo.Image = resizer.Resize(textBoxFileName.Text, textBoxNewFileName.Text, smoothingMode, interpolationMode, pixelOffsetMode, pictureBoxFrom.Image.Width * Convert.ToInt32(trackBarPercent.Value) / 100, pictureBoxFrom.Image.Height * Convert.ToInt32(trackBarPercent.Value) / 100);
        }


        private void AriaImageResizerTestForm_Load(object sender, EventArgs e)
        {

            comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.AntiAlias).ToString() + ") " +  SmoothingMode.AntiAlias.ToString());
            comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.Default).ToString() + ") " + SmoothingMode.Default.ToString());
            comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.HighQuality).ToString() + ") " + SmoothingMode.HighQuality.ToString());
            comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.HighSpeed).ToString() + ") " + SmoothingMode.HighSpeed.ToString());
            //comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.Invalid).ToString() + ") " + SmoothingMode.Invalid.ToString());
            comboBoxSmoothingMode.Items.Add(Convert.ToInt32(SmoothingMode.None).ToString() + ") " + SmoothingMode.None.ToString());
            comboBoxSmoothingMode.SelectedIndex = 0;

            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.Bicubic).ToString() + ") " + InterpolationMode.Bicubic.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.Bilinear).ToString() + ") " + InterpolationMode.Bilinear.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.Default).ToString() + ") " + InterpolationMode.Default.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.High).ToString() + ") " + InterpolationMode.High.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.HighQualityBicubic).ToString() + ") " + InterpolationMode.HighQualityBicubic.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.HighQualityBilinear).ToString() + ") " + InterpolationMode.HighQualityBilinear.ToString());
            //comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.Invalid).ToString() + ") " + InterpolationMode.Invalid.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.Low).ToString() + ") " + InterpolationMode.Low.ToString());
            comboBoxInterpolationMode.Items.Add(Convert.ToInt32(InterpolationMode.NearestNeighbor).ToString() + ") " + InterpolationMode.NearestNeighbor.ToString());
            comboBoxInterpolationMode.SelectedIndex = 4;

            comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.Default).ToString() + ") " + PixelOffsetMode.Default.ToString());
            comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.Half).ToString() + ") " + PixelOffsetMode.Half.ToString());
            comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.HighQuality).ToString() + ") " + PixelOffsetMode.HighQuality.ToString());
            comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.HighSpeed).ToString() + ") " + PixelOffsetMode.HighSpeed.ToString());
            //comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.Invalid).ToString() + ") " + PixelOffsetMode.Invalid.ToString());
            comboBoxPixelOffsetMode.Items.Add(Convert.ToInt32(PixelOffsetMode.None).ToString() + ") " + PixelOffsetMode.None.ToString());
            comboBoxPixelOffsetMode.SelectedIndex = 2;

            trackBarPercent_Scroll(null, null);
        }

        private void buttonSave_Click(object sender, EventArgs e)
        {
            SaveFileDialog save = new SaveFileDialog();
            if (save.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                pictureBoxTo.Image.Save(save.FileName);  
            }
        }

        private void comboBoxSmoothingMode_SelectedIndexChanged(object sender, EventArgs e)
        {
            trackBarPercent_Scroll(null, null);

        }

        private void comboBoxInterpolationMode_SelectedIndexChanged(object sender, EventArgs e)
        {

            trackBarPercent_Scroll(null, null);
        }

        private void comboBoxPixelOffsetMode_SelectedIndexChanged(object sender, EventArgs e)
        {
            trackBarPercent_Scroll(null, null);

        }
    }
}
