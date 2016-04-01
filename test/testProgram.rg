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
//     // if b {
//     //     x = 10
//     // } else {
//     //     x = 5
//     // }

//     if !b || b {
//         x = x + 1
//     } else {
//         x = x - 2
//         return x
//     }
// }

testWhile : Unit {
    let x: Int = 5

    while x >= 10 {
        x = x - 1
        return
    }

    while x % 2 == 0 {
        x = 3 * x / 2 + 1
    }

    return x  
}

// main : Int {
//     foo 2 3 //2 3
//     return 1
// }

/*
; ModuleID = 'my first module'

define i32 @foo(i32 %a, i32 %b) {
entry:
  %aArg = alloca i32
  store i32 %a, i32* %aArg
  %bArg = alloca i32
  store i32 %b, i32* %bArg
  %x = alloca i32
  %a0 = load i32* %aArg
  %b0 = load i32* %bArg
  %0 = add i32 %a0, %b0
  store i32 %0, i32* %x
  %t = alloca i32
  %b1 = load i32* %bArg
  store i32 %b1, i32* %t
  %x0 = load i32* %x
  %1 = call i32 @foo(i32 1, i32 %x0)
  %t0 = load i32* %t
  %x1 = load i32* %x
  %2 = call i32 @foo(i32 %t0, i32 %x1)
  %x2 = load i32* %x
  ret i32 %x2
}

define void @bar(i32 %c) {
entry:
  %cArg = alloca i32
  store i32 %c, i32* %cArg
  ret void
}

define void @zoo(i32 %a, i32 %b) {
entry:
  %aArg = alloca i32
  store i32 %a, i32* %aArg
  %bArg = alloca i32
  store i32 %b, i32* %bArg
  %z = alloca i1
  %a0 = load i32* %aArg
  %b0 = load i32* %bArg
  %0 = sub i32 0, %b0
  %1 = icmp slt i32 %a0, %0
  %a1 = load i32* %aArg
  %b1 = load i32* %bArg
  %2 = icmp sge i32 %a1, %b1
  %3 = xor i1 %2, true
  %4 = or i1 %1, %3
  store i1 %4, i1* %z
  %t = alloca i32
  store i32 5, i32* %t
  %z0 = load i1* %z
  %5 = call i1 @zoo2(i1 %z0)
  ret void
}

define i1 @zoo2(i1 %f) {
entry:
  %fArg = alloca i1
  store i1 %f, i1* %fArg
  %x = alloca i32
  store i32 5, i32* %x
  %x0 = load i32* %x
  %0 = add i32 %x0, 1
  store i32 %0, i32* %x
  %f0 = load i1* %fArg
  ret i1 %f0
}

define i32 @testIf(i1 %b) {
entry:
  %bArg = alloca i1
  store i1 %b, i1* %bArg
  %x = alloca i32
  store i32 5, i32* %x
  %b0 = load i1* %bArg
  br i1 %b0, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  store i32 10, i32* %x
  br label %if.merge

if.else:                                          ; preds = %entry
  store i32 5, i32* %x
  br label %if.merge

if.merge:                                         ; preds = %if.else, %if.then
  %b1 = load i1* %bArg
  %0 = xor i1 %b1, true
  %b2 = load i1* %bArg
  %1 = or i1 %0, %b2
  br i1 %1, label %if.then0, label %if.else0

if.then0:                                         ; preds = %if.merge
  %x0 = load i32* %x
  %2 = add i32 %x0, 1
  store i32 %2, i32* %x
  br label %if.merge0

if.else0:                                         ; preds = %if.merge
  %x1 = load i32* %x
  %3 = sub i32 %x1, 2
  store i32 %3, i32* %x
  %x2 = load i32* %x
  br label %if.merge0

if.merge0:                                        ; preds = %if.else0, %if.then0
  %x3 = load i32* %x
  %4 = icmp sge i32 %x3, 10
  br i1 %4, label %while.loop, label %while.end

while.loop:                                       ; preds = %while.loop, %if.merge0
  %x4 = load i32* %x
  %5 = sub i32 %x4, 1
  store i32 %5, i32* %x
  %x5 = load i32* %x
  %6 = icmp sge i32 %x5, 10
  br i1 %6, label %while.loop, label %while.end

while.end:                                        ; preds = %while.loop, %if.merge0
  %x6 = load i32* %x
  %7 = srem i32 %x6, 2
  %8 = icmp eq i32 %7, 0
  br i1 %8, label %while.loop0, label %while.end0

while.loop0:                                      ; preds = %while.loop0, %while.end
  %x7 = load i32* %x
  %9 = mul i32 3, %x7
  %10 = sdiv i32 %9, 2
  %11 = add i32 %10, 1
  store i32 %11, i32* %x
  %x8 = load i32* %x
  %12 = srem i32 %x8, 2
  %13 = icmp eq i32 %12, 0
  br i1 %13, label %while.loop0, label %while.end0

while.end0:                                       ; preds = %while.loop0, %while.end
  %x9 = load i32* %x
  ret i32 %x9
}

define i32 @main() {
entry:
  %0 = call i32 @foo(i32 2, i32 3)
  ret i32 1
}
*/