/******************************************************************************* 
 *  Copyright 2009 Amazon Services.
 *  Licensed under the Apache License, Version 2.0 (the "License"); 
 *  
 *  You may not use this file except in compliance with the License. 
 *  You may obtain a copy of the License at: http://aws.amazon.com/apache2.0
 *  This file is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 *  CONDITIONS OF ANY KIND, either express or implied. See the License for the 
 *  specific language governing permissions and limitations under the License.
 * ***************************************************************************** 
 * 
 *  Marketplace Web Service CSharp Library
 *  API Version: 2009-01-01
 *  Generated: Mon Mar 16 17:31:42 PDT 2009 
 * 
 */

using System;
using System.Xml.Serialization;
using System.Collections.Generic;
using System.Text;
using MarketplaceWebService.Attributes;


namespace MarketplaceWebService.Model
{
    [XmlTypeAttribute(Namespace = "http://mws.amazonaws.com/doc/2009-01-01/")]
    [XmlRootAttribute(Namespace = "http://mws.amazonaws.com/doc/2009-01-01/", IsNullable = false)]
    [MarketplaceWebServiceAttribute(RequestType = RequestType.DEFAULT, ResponseType = ResponseType.DEFAULT)]
    public class RequestReportRequest
    {
    
        private String marketplaceField;

        private String merchantField;

        private String reportTypeField;

        private DateTime? startDateField;

        private DateTime? endDateField;


        /// <summary>
        /// Gets and sets the Marketplace property.
        /// </summary>
        [XmlElementAttribute(ElementName = "Marketplace")]
        public String Marketplace
        {
            get { return this.marketplaceField ; }
            set { this.marketplaceField= value; }
        }



        /// <summary>
        /// Sets the Marketplace property
        /// </summary>
        /// <param name="marketplace">Marketplace property</param>
        /// <returns>this instance</returns>
        public RequestReportRequest WithMarketplace(String marketplace)
        {
            this.marketplaceField = marketplace;
            return this;
        }



        /// <summary>
        /// Checks if Marketplace property is set
        /// </summary>
        /// <returns>true if Marketplace property is set</returns>
        public Boolean IsSetMarketplace()
        {
            return  this.marketplaceField != null;

        }


        /// <summary>
        /// Gets and sets the Merchant property.
        /// </summary>
        [XmlElementAttribute(ElementName = "Merchant")]
        public String Merchant
        {
            get { return this.merchantField ; }
            set { this.merchantField= value; }
        }



        /// <summary>
        /// Sets the Merchant property
        /// </summary>
        /// <param name="merchant">Merchant property</param>
        /// <returns>this instance</returns>
        public RequestReportRequest WithMerchant(String merchant)
        {
            this.merchantField = merchant;
            return this;
        }



        /// <summary>
        /// Checks if Merchant property is set
        /// </summary>
        /// <returns>true if Merchant property is set</returns>
        public Boolean IsSetMerchant()
        {
            return  this.merchantField != null;

        }


        /// <summary>
        /// Gets and sets the ReportType property.
        /// </summary>
        [XmlElementAttribute(ElementName = "ReportType")]
        public String ReportType
        {
            get { return this.reportTypeField ; }
            set { this.reportTypeField= value; }
        }



        /// <summary>
        /// Sets the ReportType property
        /// </summary>
        /// <param name="reportType">ReportType property</param>
        /// <returns>this instance</returns>
        public RequestReportRequest WithReportType(String reportType)
        {
            this.reportTypeField = reportType;
            return this;
        }



        /// <summary>
        /// Checks if ReportType property is set
        /// </summary>
        /// <returns>true if ReportType property is set</returns>
        public Boolean IsSetReportType()
        {
            return  this.reportTypeField != null;

        }


        /// <summary>
        /// Gets and sets the StartDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "StartDate")]
        public DateTime StartDate
        {
            get { return this.startDateField.GetValueOrDefault() ; }
            set { this.startDateField= value; }
        }



        /// <summary>
        /// Sets the StartDate property
        /// </summary>
        /// <param name="startDate">StartDate property</param>
        /// <returns>this instance</returns>
        public RequestReportRequest WithStartDate(DateTime startDate)
        {
            this.startDateField = startDate;
            return this;
        }



        /// <summary>
        /// Checks if StartDate property is set
        /// </summary>
        /// <returns>true if StartDate property is set</returns>
        public Boolean IsSetStartDate()
        {
            return  this.startDateField.HasValue;

        }


        /// <summary>
        /// Gets and sets the EndDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "EndDate")]
        public DateTime EndDate
        {
            get { return this.endDateField.GetValueOrDefault() ; }
            set { this.endDateField= value; }
        }



        /// <summary>
        /// Sets the EndDate property
        /// </summary>
        /// <param name="endDate">EndDate property</param>
        /// <returns>this instance</returns>
        public RequestReportRequest WithEndDate(DateTime endDate)
        {
            this.endDateField = endDate;
            return this;
        }



        /// <summary>
        /// Checks if EndDate property is set
        /// </summary>
        /// <returns>true if EndDate property is set</returns>
        public Boolean IsSetEndDate()
        {
            return  this.endDateField.HasValue;

        }





    }

}
