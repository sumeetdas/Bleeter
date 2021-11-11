module String 

let replace (oldStr:string) (newStr:string) (message:string) = message.Replace(oldStr, newStr)
let contains (str:string) (message:string) = message.Contains(str)