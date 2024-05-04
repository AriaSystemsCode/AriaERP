using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Aria5SystemAdmin.Module.BusinessObjects;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using DevExpress.Xpo;
using DevExpress.Data.Filtering;

namespace Aria5SystemAdmin.Module.Managers
{
    class HRManager
    {
        public HRJobPosition getlastrevision(HRJobPosition masterjobposition)
        {
            //if (masterjobposition.Master == true)
            //{
            //    if (masterjobposition.Revisions.Count > 0)
            //    {
            //      //  HRJobPosition lastversion = masterjobposition.Revisions.Where(x => x.RevisionNumber == masterjobposition.RevisionNumber).First();
            //        return lastversion;
            //    }
            //    else
            //    {
            //        return createfirstrevision(masterjobposition);
            //    }
            //}
            //else
            //{
               return null;
            //}
        }
        public HRJobPosition createfirstrevision(HRJobPosition masterjobposition)
        {

            HRJobPosition newversion = new HRJobPosition(masterjobposition.Session, masterjobposition);
            newversion.Activities.BaseAddRange(masterjobposition.Activities);
            newversion.EntityAttachments.BaseAddRange(masterjobposition.EntityAttachments);
            masterjobposition.RevisionNumber += 1;
            masterjobposition.Save();
            newversion.Save();
            masterjobposition.Session.CommitTransaction();
            return newversion;
        }

        /// <summary>
        /// Resize the image to the specified width and height.
        /// </summary>
        /// <param name="image">The image to resize.</param>
        /// <param name="width">The width to resize to.</param>
        /// <param name="height">The height to resize to.</param>
        /// <returns>The resized image.</returns>
        public Bitmap ResizeImage(Image image, int width, int height)
        {
            var destRect = new Rectangle(0, 0, width, height);
            var destImage = new Bitmap(width, height);

            destImage.SetResolution(image.HorizontalResolution, image.VerticalResolution);

            using (var graphics = Graphics.FromImage(destImage))
            {
                graphics.CompositingMode = CompositingMode.SourceCopy;
                graphics.CompositingQuality = CompositingQuality.HighQuality;
                graphics.InterpolationMode = InterpolationMode.HighQualityBicubic;
                graphics.SmoothingMode = SmoothingMode.HighQuality;
                graphics.PixelOffsetMode = PixelOffsetMode.HighQuality;

                using (var wrapMode = new ImageAttributes())
                {
                    wrapMode.SetWrapMode(WrapMode.TileFlipXY);
                    graphics.DrawImage(image, destRect, 0, 0, image.Width, image.Height, GraphicsUnit.Pixel, wrapMode);
                }
            }

            return destImage;
        }

        //ATA add the checklist menu 7/9/2017[start]

       
    }
}
