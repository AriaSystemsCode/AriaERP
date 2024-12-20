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
    public class GetReportCountRequest
    {
    
        private String marketplaceField;

        private String merchantField;

        private  TypeList reportTypeListField;
        private Boolean? acknowledgedField;

        private DateTime? availableFromDateField;

        private DateTime? availableToDateField;


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
        public GetReportCountRequest WithMarketplace(String marketplace)
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
        public GetReportCountRequest WithMerchant(String merchant)
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
        /// Gets and sets the ReportTypeList property.
        /// </summary>
        [XmlElementAttribute(ElementName = "ReportTypeList")]
        public TypeList ReportTypeList
        {
            get { return this.reportTypeListField ; }
            set { this.reportTypeListField = value; }
        }



        /// <summary>
        /// Sets the ReportTypeList property
        /// </summary>
        /// <param name="reportTypeList">ReportTypeList property</param>
        /// <returns>this instance</returns>
        public GetReportCountRequest WithReportTypeList(TypeList reportTypeList)
        {
            this.reportTypeListField = reportTypeList;
            return this;
        }



        /// <summary>
        /// Checks if ReportTypeList property is set
        /// </summary>
        /// <returns>true if ReportTypeList property is set</returns>
        public Boolean IsSetReportTypeList()
        {
            return this.reportTypeListField != null;
        }




        /// <summary>
        /// Gets and sets the Acknowledged property.
        /// </summary>
        [XmlElementAttribute(ElementName = "Acknowledged")]
        public Boolean Acknowledged
        {
            get { return this.acknowledgedField.GetValueOrDefault() ; }
            set { this.acknowledgedField= value; }
        }



        /// <summary>
        /// Sets the Acknowledged property
        /// </summary>
        /// <param name="acknowledged">Acknowledged property</param>
        /// <returns>this instance</returns>
        public GetReportCountRequest WithAcknowledged(Boolean acknowledged)
        {
            this.acknowledgedField = acknowledged;
            return this;
        }



        /// <summary>
        /// Checks if Acknowledged property is set
        /// </summary>
        /// <returns>true if Acknowledged property is set</returns>
        public Boolean IsSetAcknowledged()
        {
            return  this.acknowledgedField.HasValue;

        }


        /// <summary>
        /// Gets and sets the AvailableFromDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "AvailableFromDate")]
        public DateTime AvailableFromDate
        {
            get { return this.availableFromDateField.GetValueOrDefault() ; }
            set { this.availableFromDateField= value; }
        }



        /// <summary>
        /// Sets the AvailableFromDate property
        /// </summary>
        /// <param name="availableFromDate">AvailableFromDate property</param>
        /// <returns>this instance</returns>
        public GetReportCountRequest WithAvailableFromDate(DateTime availableFromDate)
        {
            this.availableFromDateField = availableFromDate;
            return this;
        }



        /// <summary>
        /// Checks if AvailableFromDate property is set
        /// </summary>
        /// <returns>true if AvailableFromDate property is set</returns>
        public Boolean IsSetAvailableFromDate()
        {
            return  this.availableFromDateField.HasValue;

        }


        /// <summary>
        /// Gets and sets the AvailableToDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "AvailableToDate")]
        public DateTime AvailableToDate
        {
            get { return this.availableToDateField.GetValueOrDefault() ; }
            set { this.availableToDateField= value; }
        }



        /// <summary>
        /// Sets the AvailableToDate property
        /// </summary>
        /// <param name="availableToDate">AvailableToDate property</param>
        /// <returns>this instance</returns>
        public GetReportCountRequest WithAvailableToDate(DateTime availableToDate)
        {
            this.availableToDateField = availableToDate;
            return this;
        }



        /// <summary>
        /// Checks if AvailableToDate property is set
        /// </summary>
        /// <returns>true if AvailableToDate property is set</returns>
        public Boolean IsSetAvailableToDate()
        {
            return  this.availableToDateField.HasValue;

        }





    }

}
