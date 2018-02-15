------
title: Calling Golang functions from Haskell
summary: Exploring Dynamic and Static linking
tags: haskell, golang, ffi, linking, static, dynamic
category: programming
id: haskell-golang-ffi
author: Saksham Sharma
------

Calling C from Haskell is easy. Calling Golang from C is easy too. Then why should calling Golang from Haskell be tough? Yet, I could not find a single document / blog on this topic. While doing this, I also explored the possibility domain of library linking in Haskell.

My latest project (in Haskell) required a cryptographic protocol (Verifiable Random Function), whose implementation I could only find in Golang and C. The Golang ones seemed better maintained, self-contained, and understandable. So, I took the easy way out, and wrote a binary using that source code, allowing me to call the binary from Haskell (taking care to marshall the input and output binary data as base64 strings). Of course, this was ugly, and I got a (mostly) satisfying solution working, which did not have to launch a separate process for this simple task.<!--more-->

## Types of linking
There are only a handful of ways to link to a library on Linux systems. The major ones are:

* `.so`: Shared Object files
* `.a`: Archive files

### Shared Objects
Shared Objects are dynamically linked libraries, much like `.dll` files in Windows. Their path is deduced from environment variables / linker arguments, and these are fetched when the binary is run. By default, if you run the binary from a particular directory, that directory is also searched to resolve the library, which is a relief.

These libraries are only loaded in memory once, even if there are `n` processes running which are linked against that `.so` file. This is super helpful if the library is something like `libc`, which is used by a huge number of programs.

### Archive Files
These are, unambiguously, archives of binaries. These contain library code which is meant to be linked into a binary during compile time, statically. This is helpful because this lets the binary be standalone to some level, and the user does not have to copy a file along with the binary everywhere. Of course, this has an overhead in terms of file size.

## State of FFI in Haskell and Golang
Unfortunately, I could not find any material on Foreign Function Interface between Haskell and Golang. What I did find was how to run C from Haskell, and how to run Golang from C. This led to the idea of using a C-compatible intermediate form to mediate between Haskell and Golang.

## Golang to C
### Compiling Golang to a C-style library
Go has an amazing support for such nifty things. [This is the link I followed to get this to work](https://stackoverflow.com/questions/32215509).

In short, you need to modify your source Golang file(s) as follows (almost-verbatim copied from the above linked answer on SO):

* The package needs to be called `main`.
* A `main` function must exist, albeit empty.
* There needs to be a `import "C"` statement at the top of the file which has functions to be exported.
* Each function that has to be exported, has to have a comment `//export <NameToExportAs>` *exactly* above its definition.

An example file looks like the following:
```go
package main

import "C"

//export Testing
func Testing(a int) int {
    return a+1
}
```

Now, we need to get a library from this which can be used later. The following commands work:
* Generate a shared object: `go build -buildmode=c-shared -o lib<LIBNAME>.so *.go`
* Generate an archive file: `go build -buildmode=c-archive -o lib<LIBNAME>.a *.go`

### Converting Golang types to C types
This step is necessary because Haskell does not know about Golang types. The above program is very simple, but what if you had to use strings?

A naive function:
```go
//export StrFxn
func StrFxn(input string) string {
    return "Hello " + input + " World"
}
```

This would generate the following few lines in the file `lib<LIBNAME>.h`:
```
...
typedef struct { const char *p; GoInt n; } GoString;
...
extern GoString StrFxn(GoString p0);
...
```

As you can see, there's a magical type GoString being used here, which we won't like later. So we modify the Go code as follows:

```go
import "C"

func strFxn(input string) string {
    return "Hello " + input + " World"
}

//export StrFxn
func StrFxn(cinput *C.char) *C.char {
    // C data needs to be manually managed in memory.
    // But we will do it from Haskell.
    input := C.GoString(cinput)
    return C.CString(strFxn(input))
}

func main() {}
```

Now the exported function would behave as if it is just a C function!

## Load C library from Haskell
This is pretty straitforward. Assuming that the linker finds your built library, you write a file like the following:
```haskell
{-# LANGUAGE ForeignFunctionInterface #-}
-- The above PRAGMA used to be required for FFI, but the code
-- seems to work without it on recent GHC versions.

import Foreign.C.String
import Foreign.Marshal.Alloc

-- | The call which brings the Foreign function into scope, with manually
--   declared type. This type is not checked at compile time.
--   ccall means C-calling-convention, which is usually what you want.
foreign import ccall "StrFxn" go_StrFxn :: CString -> IO CString

-- | Custom function to make CString-based FFI functions user-friendly.
runStrFxn :: (CString -> IO CString) -> String -> IO String
runStrFxn f input = do
  cinput <- newCString input
  coutput <- f cinput
  res <- peekCString coutput
  _ <- free cinput
  _ <- free coutput
  return res

-- | User friendly function for profit :)
strFxn :: String -> IO String
strFxn = runStrFxn go_StrFxn
```

This was pretty straighforward!

## Linking
The above part was pretty neat and simple. Alas, the linking part is not so. Since we're trying to link a custom library, it would be nice to be able to link it at compile time, and then forget about it. Yet, some constraints may make it difficult to do.

The following sections are also valid for linking C libraries.

### FFI in the Haskell Executable package: The easy scenario
I'll first begin with the easier solution. If the haskell code requiring the FFI is in your executable source files (and not the library files), you're in luck.

Let's assume that your Golang source (as well as the output library) is in `./golang` folder from the root of your Haskell project.

#### Statically Linking a Haskell executable with an Archive File
This is useful if you want your binary to be portable in terms of this Golang library at least.

Your Cabal file would now see the following sections in the section called `executable <executable-name>`:

```yaml
executables:
  ffi-exe:
  ...
  extra-lib-dirs:
      golang
  extra-libraries:
      <LIBNAME>
  ghc-options:
  - -threaded
  - -optl-static -optl-pthread
  - ....
```

Note that there might be many other `ghc-options` already.

#### Dynamically Linking a Haskell executable with a Shared Object File
Your Cabal file would now see the following sections in the section called `executable <executable-name>`:

```yaml
executables:
  ffi-exe:
  ...
  extra-lib-dirs:
      golang
  extra-libraries:
      <LIBNAME>
  ghc-options:
  - -threaded
  - -dynamic
  - ....
```

This will allow the code to compile, but running poses an issue.
```shell
$ stack exec ffi
/home/saksham/code/ffi/.stack-work/install/x86_64-linux/lts-10.5/8.2.2/bin/ffi-exe: error while loading shared libraries: libmytest.so: cannot open shared object file: No such file or directory
```

There is an extra step involved in this case. The generated binary somehow does not want to look in the `golang` folder, and you need to bring the library into the current folder. You could either move it to the project root, or you could run `ln -sf mytest.so .` to create a soft link to the library in the project root.

### FFI in the Haskell Library: The difficult scenario
What if you want to do a FFI call inside a Haskell library you're writing? This gets tricky, since `ghc-pkg` does not like relative paths in `extra-lib-dirs`, but this is only when those paths are in the `library` section. Using them in `executables` seems to work just fine, as we saw in the section above.

#### Dynamically Linking a Haskell Library with a Shared Object File
This seems to work in almost the same manner as the Linking-a-Haskell-Executable-with-A-Shared-Object. See the above section for details on how to do this.

*Fun Fact*: You do not even need to add the `extra-libraries` to the `library` section, even though the Haskell library is the one depending on shared object. Since the library is loaded at run time (dynamically), the binary will find `libmytest.so` during startup. Thus, there is no point in it knowing the dependency during compile time.

*Fun Fact 2*: Moving the `extra-libraries` and `extra-lib-dirs` to the `libraries` section seems to actually break the build. For reasons, refer to the next section.

#### Statically Linking a Haskell Library with an Archive File
This is a case which does not work nicely with relative paths. Any help in this regards would be appreciated.
Anyhow, this is what I could understand about this:

* The Haskell library depends on the archive file placed in the project root (for simplicity).
* The user wants the build program to find the archive file during build, and statically link during compile time, to keep the binary portable.
* The ideal scenario would be to add `-optl-static -optl-pthread` to ghc-options, and provide a relative path (`.`) in the `extra-lib-dirs` key of the library configuration in the build file. Or, maybe, allow us to provide a `-L.` flag to the build tool, or a similar flag to the linker. None of these works.
* Somehow, somewhere in time, someone thought that relative library paths should be forbidden during build time. As far as I could trace right now, this appears to be an issue in `ghc-pkg`.
* The above behavior is different in the cases of building a haskell executable and a haskell library. This makes me think that this behavior is buggy.
* IMHO, relative build-time library paths should be allowed, because of such a use case for FFI.

So, currently, I could not find a way to statically link an archive placed in the project folder, to a haskell library. Things may change in the future, as I plan to pursue this as a bug. If you know of a way to get this done, do point it out in the comments below.

A very relevant (but old) discussion can be found [here](https://github.com/haskell/cabal/issues/1317).
