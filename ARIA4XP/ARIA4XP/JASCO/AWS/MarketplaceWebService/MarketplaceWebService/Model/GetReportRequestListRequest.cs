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
    public class GetReportRequestListRequest
    {
    
        private String marketplaceField;

        private String merchantField;

        private  IdList reportRequestIdListField;
        private  TypeList reportTypeListField;
        private  StatusList reportProcessingStatusListField;
        private Decimal? maxCountField;

        private DateTime? requestedFromDateField;

        private DateTime? requestedToDateField;


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
        public GetReportRequestListRequest WithMarketplace(String marketplace)
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
        public GetReportRequestListRequest WithMerchant(String merchant)
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
        /// Gets and sets the ReportRequestIdList property.
        /// </summary>
        [XmlElementAttribute(ElementName = "ReportRequestIdList")]
        public IdList ReportRequestIdList
        {
            get { return this.reportRequestIdListField ; }
            set { this.reportRequestIdListField = value; }
        }



        /// <summary>
        /// Sets the ReportRequestIdList property
        /// </summary>
        /// <param name="reportRequestIdList">ReportRequestIdList property</param>
        /// <returns>this instance</returns>
        public GetReportRequestListRequest WithReportRequestIdList(IdList reportRequestIdList)
        {
            this.reportRequestIdListField = reportRequestIdList;
            return this;
        }



        /// <summary>
        /// Checks if ReportRequestIdList property is set
        /// </summary>
        /// <returns>true if ReportRequestIdList property is set</returns>
        public Boolean IsSetReportRequestIdList()
        {
            return this.reportRequestIdListField != null;
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
        public GetReportRequestListRequest WithReportTypeList(TypeList reportTypeList)
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
        /// Gets and sets the ReportProcessingStatusList property.
        /// </summary>
        [XmlElementAttribute(ElementName = "ReportProcessingStatusList")]
        public StatusList ReportProcessingStatusList
        {
            get { return this.reportProcessingStatusListField ; }
            set { this.reportProcessingStatusListField = value; }
        }



        /// <summary>
        /// Sets the ReportProcessingStatusList property
        /// </summary>
        /// <param name="reportProcessingStatusList">ReportProcessingStatusList property</param>
        /// <returns>this instance</returns>
        public GetReportRequestListRequest WithReportProcessingStatusList(StatusList reportProcessingStatusList)
        {
            this.reportProcessingStatusListField = reportProcessingStatusList;
            return this;
        }



        /// <summary>
        /// Checks if ReportProcessingStatusList property is set
        /// </summary>
        /// <returns>true if ReportProcessingStatusList property is set</returns>
        public Boolean IsSetReportProcessingStatusList()
        {
            return this.reportProcessingStatusListField != null;
        }




        /// <summary>
        /// Gets and sets the MaxCount property.
        /// </summary>
        [XmlElementAttribute(ElementName = "MaxCount")]
        public Decimal? MaxCount
        {
            get { return this.maxCountField ; }
            set { this.maxCountField= value; }
        }



        /// <summary>
        /// Sets the MaxCount property
        /// </summary>
        /// <param name="maxCount">MaxCount property</param>
        /// <returns>this instance</returns>
        public GetReportRequestListRequest WithMaxCount(Int32? maxCount)
        {
            this.maxCountField = maxCount;
            return this;
        }



        /// <summary>
        /// Checks if MaxCount property is set
        /// </summary>
        /// <returns>true if MaxCount property is set</returns>
        public Boolean IsSetMaxCount()
        {
            return this.maxCountField != null;

        }


        /// <summary>
        /// Gets and sets the RequestedFromDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "RequestedFromDate")]
        public DateTime RequestedFromDate
        {
            get { return this.requestedFromDateField.GetValueOrDefault() ; }
            set { this.requestedFromDateField= value; }
        }



        /// <summary>
        /// Sets the RequestedFromDate property
        /// </summary>
        /// <param name="requestedFromDate">RequestedFromDate property</param>
        /// <returns>this instance</returns>
        public GetReportRequestListRequest WithRequestedFromDate(DateTime requestedFromDate)
        {
            this.requestedFromDateField = requestedFromDate;
            return this;
        }



        /// <summary>
        /// Checks if RequestedFromDate property is set
        /// </summary>
        /// <returns>true if RequestedFromDate property is set</returns>
        public Boolean IsSetRequestedFromDate()
        {
            return  this.requestedFromDateField.HasValue;

        }


        /// <summary>
        /// Gets and sets the RequestedToDate property.
        /// </summary>
        [XmlElementAttribute(ElementName = "RequestedToDate")]
        public DateTime RequestedToDate
        {
            get { return this.requestedToDateField.GetValueOrDefault() ; }
            set { this.requestedToDateField= value; }
        }



        /// <summary>
        /// Sets the RequestedToDate property
        /// </summary>
        /// <param name="requestedToDate">RequestedToDate property</param>
        /// <returns>this instance</returns>
        public GetReportRequestListRequest WithRequestedToDate(DateTime requestedToDate)
        {
            this.requestedToDateField = requestedToDate;
            return this;
        }



        /// <summary>
        /// Checks if RequestedToDate property is set
        /// </summary>
        /// <returns>true if RequestedToDate property is set</returns>
        public Boolean IsSetRequestedToDate()
        {
            return  this.requestedToDateField.HasValue;

        }





    }

}
