
import * as Identity from "./ts-59414574e41342547fa6ce469d9d93dbc285b9572d609bf63a6b84710f782d00";




/* 
(Identity c) -> c
 */
export const main = <D>(a: Identity.Identity<D>) => { const { vars: [inner] } = a; return inner; }