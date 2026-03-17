project('luajit-decompiler-v2', 'cpp',
  version : '2.0.0',
  default_options : [
    'cpp_std=c++20',
    'warning_level=1'
  ]
)

cpp = meson.get_compiler('cpp')

# Compiler-specific flags for consistent behavior
# -fpermissive / -funsigned-char (GCC/Clang) vs /permissive / /J (MSVC)
cpp_args = []
if cpp.get_id() in ['gcc', 'clang']
  cpp_args += ['-fpermissive', '-funsigned-char']
elif cpp.get_id() == 'msvc'
  cpp_args += ['/permissive', '/J']  # permissive mode, unsigned char
endif

sources = [
  'main.cpp',
  'ast/ast.cpp',
  'bytecode/bytecode.cpp',
  'bytecode/prototype.cpp',
  'lua/lua.cpp',
]

inc = include_directories('.')

decompiler = executable('luajit-decompiler', sources,
  include_directories : inc,
  cpp_args : cpp_args,
  install : true
)

# Compile Lua to bytecode, decompile, recompile, and diff against original compile
# Decompiler writes to output/<basename>.lua (no path prefix)
foreach file : ['tests/fibonacci.lua']
  bc = file.replace('.lua', '.ljbc')
  name = file.split('/')[-1].replace('.lua', '')
  out_lua = 'output/' + name + '.lua'
  out_bc = 'output/' + name + '.ljbc'
  test(name,
    find_program('sh'),
    args: [
      '-c',
      'rm -rf output && mkdir -p output && luajit -bd @0@ @1@ && @2@ @1@ -o output/ && luajit -bd @3@ @4@ && diff @1@ @4@'.format(file, bc, decompiler.full_path(), out_lua, out_bc)
    ],
    workdir: meson.current_source_dir()
  )
endforeach
