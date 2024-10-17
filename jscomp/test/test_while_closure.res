/* Test when i is captured 
    {[
    ((fun _ -> j = i; (fun _ -> v := !v + j)) ())
    ]}

    babel does this way:
{[
var arr = []
var f = function(){
  for(let i = 0; i < 10 ; ++ i){
    arr [i] = ()=>{console.log(i)}
    
  }
}]}


{[
var arr = [];
var f = function f() {
  var _loop = function (i) {
    arr[i] = function () {
      console.log(i);
    };
  };

  for (var i = 0; i < 10; ++i) {
    _loop(i);
  }
};
]}    

This means inline is tricky in javascript, here we try to inline [_loop]?

    


 */

let v = ref(0)
let count = 10
let arr = Array.make(count, _ => ())
let f = () => {
  let n = ref(0)
  while n.contents < count {
    let j = n.contents
    arr[j] = _ => v := v.contents + j
    incr(n)
  }
}

let () = {
  f()
  Array.iter(x => x(), arr)
  print_endline(string_of_int(v.contents))
  assert (v.contents == 45)
}
