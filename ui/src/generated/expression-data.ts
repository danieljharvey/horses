// tslint:disable
/**
 * 
 * No description provided (generated by Openapi Generator https://github.com/openapitools/openapi-generator)
 *
 * The version of the OpenAPI document: 
 * 
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 */


import { RuntimeData } from './runtime-data';
import { UnitTestData } from './unit-test-data';

/**
 * 
 * @export
 * @interface ExpressionData
 */
export interface ExpressionData {
    /**
     * 
     * @type {string}
     * @memberof ExpressionData
     */
    edHash: string;
    /**
     * 
     * @type {string}
     * @memberof ExpressionData
     */
    edPretty: string;
    /**
     * 
     * @type {string}
     * @memberof ExpressionData
     */
    edType: string;
    /**
     * 
     * @type {{ [key: string]: string; }}
     * @memberof ExpressionData
     */
    edBindings: { [key: string]: string; };
    /**
     * 
     * @type {{ [key: string]: string; }}
     * @memberof ExpressionData
     */
    edTypeBindings: { [key: string]: string; };
    /**
     * 
     * @type {Array<UnitTestData>}
     * @memberof ExpressionData
     */
    edUnitTests: Array<UnitTestData>;
    /**
     * 
     * @type {{ [key: string]: RuntimeData; }}
     * @memberof ExpressionData
     */
    edRuntimes: { [key: string]: RuntimeData; };
    /**
     * 
     * @type {string}
     * @memberof ExpressionData
     */
    edGraphviz: string;
}


