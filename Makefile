PROJECT = pkt
PROJECT_VERSION = 0.1

DEPS = cowboy
dep_cowboy = git https://github.com/ninenines/cowboy 2.1.0

DOC_DEPS = edown
EDOC_OPTS += '{doclet,edown_doclet}'

include erlang.mk
