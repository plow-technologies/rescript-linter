/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

@get_index external getEnv: ('a, string) => option<string> = ""
let sys_getenv = s =>
  if (
    Js.typeof(%raw(`process`)) == "undefined" || %raw(`process.env`) == Caml_undefined_extern.empty
  ) {
    raise(Not_found)
  } else {
    switch getEnv(%raw(`process.env`), s) {
    | None => raise(Not_found)
    | Some(x) => x
    }
  }

/* https://nodejs.org/dist/latest-v12.x/docs/api/os.html#os_os_platform 
   The value is set at compile time. Possible values are 'aix', 'darwin','freebsd', 'linux', 'openbsd', 'sunos', and 'win32'.
   The return value is equivalent to process.platform. 
   NodeJS does not support Cygwin very well
*/
let os_type: unit => string = %raw(`function(_){
  if(typeof process !== 'undefined' && process.platform === 'win32'){
        return "Win32"    
  }
  else {
    return "Unix"
  }
}`)
/* TODO: improve [js_pass_scope] to avoid remove unused n here */

/* let initial_time = now ()  *. 0.001 */

type process
@send external uptime: (process, unit) => float = "uptime"
@send external exit: (process, int) => 'a = "exit"

let sys_time = () =>
  if (
    Js.typeof(%raw(`process`)) == "undefined" ||
      %raw(`process.uptime`) == Caml_undefined_extern.empty
  ) {
    -1.
  } else {
    uptime(%raw(`process`), ())
  }

/*
type spawnResult
external spawnSync : string -> spawnResult = "spawnSync" [@@bs.module "child_process"]

external readAs : spawnResult -> 
  < 
    status : int Js.null;
  > Js.t = 
  "%identity"
*/

let sys_getcwd: unit => string = %raw(`function(param){
    if (typeof process === "undefined" || process.cwd === undefined){
      return "/"  
    }
    return process.cwd()
  }`)

/* Called by {!Sys} in the toplevel, should never fail */
let sys_get_argv = (): (string, array<string>) =>
  if Js.typeof(%raw(`process`)) == "undefined" {
    ("", [""])
  } else {
    let argv = %raw(`process.argv`)
    if Js.testAny(argv) {
      ("", [""])
    } else {
      (Caml_array_extern.unsafe_get(argv, 0), argv)
    }
  }

/** {!Pervasives.sys_exit} */
let sys_exit: int => 'a = exit_code =>
  if Js.typeof(%raw(`process`)) != "undefined" {
    exit(%raw(`process`), exit_code)
  }

let sys_is_directory = _s => raise(Failure("sys_is_directory not implemented"))

/** Need polyfill to make cmdliner work 
    {!Sys.is_directory} or {!Sys.file_exists} {!Sys.command} 
*/
let sys_file_exists = _s => raise(Failure("sys_file_exists not implemented"))
