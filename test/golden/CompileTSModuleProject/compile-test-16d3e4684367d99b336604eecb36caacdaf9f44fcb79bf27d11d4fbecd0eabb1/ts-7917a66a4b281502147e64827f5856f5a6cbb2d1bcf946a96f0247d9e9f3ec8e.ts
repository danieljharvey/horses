
import * as Identity from "./ts-2c5faeafc9b8b62fba4403f13e4bd297971fac75717b879e587306687c64c9e9";




/* 
(Identity c) -> c
 */
export const main = <D>(a: Identity.Identity<D>) => { const { vars: [inner] } = a; return inner; }