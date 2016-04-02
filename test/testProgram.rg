// let x: Int = 0, y = false
// mut z = 555;

// let zzz: Bool = true, xxx = 0

// mut v: Bool = 0

// foo : (a: Int) -> (b: Int) -> Int {
//     let x: Int = a + b
//     let t: Int = b
//     foo 1 x // 1 2
//     foo t x // 1 2
//     return x
// }

// bar : (c: Int) -> Unit {
//     return 
// }

// zoo : (a: Int) -> (b: Int) -> Unit {
//     let z: Bool = a < -b || !(a >= b)
//     let t: Int = 5
//     zoo2 z
// }

// zoo2 : (f: Bool) -> Bool {
//     let x: Int = 5
//     x = x + 1
//     return f
// }

// testIf : (b: Bool) -> Int {
//     let x: Int = 5
//     if b {
//         x = 10
//     } else {
//         x = 5
//     }

//     if !b || b {
//         x = x + 1
//     } else {
//         x = x - 2
//         return x
//     }

//     return 1
// }

// testWhile : Unit {
//     let x: Int = 5

//     while x >= 10 {
//         x = x - 1
//         return
//     }

//     while x % 2 == 0 {
//         x = 3 * x / 2 + 1
//     }

//     return 
// }

main : Unit {
    let x: Int = 0
    
    scanf x

    x = x + 10
    
    printf x
}

/*
; ModuleID = 'my first module'

@.scanf_str = constant [3 x i8] c"%d\00"
@.printf_str = constant [4 x i8] c"%d\0A\00"

define void @main() {
entry:
  %x = alloca i32
  store i32 0, i32* %x
  %0 = call i32 (i8*, ...)* @scanf(i8* getelementptr inbounds ([3 x i8]* @.scanf_str, i32 0, i32 0), i32* %x)
  %x0 = load i32* %x
  %1 = add i32 %x0, 10
  store i32 %1, i32* %x
  %x1 = load i32* %x
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.printf_str, i32 0, i32 0), i32 %x1)
  ret void
}

declare i32 @scanf(i8*, ...)

declare i32 @printf(i8*, ...)
*/