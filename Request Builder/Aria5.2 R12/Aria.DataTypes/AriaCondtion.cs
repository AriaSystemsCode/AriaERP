using System;
using System.Data;
using System.Collections;
using Aria.Xml;


namespace Aria.DataTypes
{
    /// <remarks>
    /// Open Issues:
    /// 1-We need to take in consideration the case of between, contains and inList for the RightHandSide onject.
    /// </remarks>
    [Serializable, AriaSerializableAttribute]
    public class AriaCondtion : AriaDataType
    {
        private AriaStandardDataType _leftHandSide;
        /// <summary>
        /// Left hand side formula formated as below.
        /// </summary>
        /// <remarks>
        /// Formats:
        /// Expression:
        /// Field:
        /// ParameterName:
        /// Value:
        /// </remarks>
        public AriaStandardDataType LeftHandSide
        {
            get { return _leftHandSide; }
            set { _leftHandSide = value; }
        }

        private bool _isOperator;
        public bool IsOperator
        {
            get { return _isOperator; }
            set { _isOperator = value; }
        }

        private AriaConditionOperators _operator;
        public AriaConditionOperators Operator
        {
            get { return _operator; }
            set { _operator = value; }
        }

        private object _rightHandSide;
        public object RightHandSide
        {
            get { return _rightHandSide; }
            set { _rightHandSide = value; }
        }

        private ArrayList _leftSideParameterizedDataList = new ArrayList();
        public ArrayList LeftSideParameterizedDataList
        {
            get { return _leftSideParameterizedDataList; }
            set { _leftSideParameterizedDataList = value; }
        }

        private ArrayList _rightSideParameterizedDataList = new ArrayList();
        public ArrayList RightSideParameterizedDataList
        {
            get { return _rightSideParameterizedDataList; }
            set { _rightSideParameterizedDataList = value; }
        }
    }
}