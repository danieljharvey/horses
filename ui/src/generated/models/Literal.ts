/* istanbul ignore file */
/* tslint:disable */
/* eslint-disable */

export type Literal = ({
    contents: number,
    tag: 'MyInt',
} | {
    contents: boolean,
    tag: 'MyBool',
} | {
    contents: string,
    tag: 'MyString',
});
