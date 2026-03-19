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
