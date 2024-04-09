using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Text;

namespace Aria.Utility.ImageProcessor
{
    public class AriaImageResizer
    {
        public Bitmap Resize(string imageFile, string imageNewFile, SmoothingMode smoothingMode,InterpolationMode interpolationMode, PixelOffsetMode pixelOffsetMode, int newWidth, int newHeight)
        {
            //try
            {
                Bitmap image = new Bitmap(imageFile);

                Bitmap newImage = new Bitmap(newWidth, Calculations(image.Width, image.Height, newWidth));

                using (Graphics gr = Graphics.FromImage(newImage))
                {

                    gr.SmoothingMode = smoothingMode;
                    gr.InterpolationMode = interpolationMode;
                    gr.PixelOffsetMode = pixelOffsetMode;

                    gr.DrawImage(image, new Rectangle(0, 0, newImage.Width, newImage.Height));

                    var myBrush = new SolidBrush(Color.FromArgb(70, 205, 205, 205));

                    double diagonal = Math.Sqrt(newImage.Width * newImage.Width + newImage.Height * newImage.Height);

                    Rectangle containerBox = new Rectangle();

                    containerBox.X = (int)(diagonal / 10);
                    //float messageLength = (float)(diagonal / message.Length * 1);
                    //containerBox.Y = -(int)(messageLength / 1.6);

                    //Font stringFont = new Font("verdana", messageLength);

                    //StringFormat sf = new StringFormat();

                    //float slope = (float)(Math.Atan2(newImage.Height, newImage.Width) * 180 / Math.PI);

                    //gr.RotateTransform(slope);
                    //gr.DrawString(message, stringFont, myBrush, containerBox, sf);
                    newImage.Save(imageNewFile);
                    return newImage;
                }
            }
            //catch (Exception exc)
            {
                //throw exc;
            }
        }

        public int Calculations(decimal w1, decimal h1, int newWidth)
        {
            decimal height = 0;
            decimal ratio = 0;


            if (newWidth < w1)
            {
                ratio = w1 / newWidth;
                height = h1 / ratio;

                return Convert.ToInt32(height);
            }

            if (w1 < newWidth)
            {
                ratio = newWidth / w1;
                height = h1 * ratio;
                return Convert.ToInt32(height);
            }

            if (w1 == newWidth)
            {
                ratio = newWidth / w1;
                height = h1 * ratio;
                return Convert.ToInt32(height);
            }

           return Convert.ToInt32(height);
        }

    }
}
