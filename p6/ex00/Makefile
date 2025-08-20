
# BASE

NAME = people
SRC = main.ml people.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS =

SOURCES = $(shell $(CAMLDEP) -sort $(SRC))

# RULES

all: $(NAME)

$(NAME): opt byt
	@ln -sf $(NAME).byt $(NAME)
	@printf "\r$(CYAN)Executable linked$(RESET)\n"

byt: $(NAME).byt
opt: $(NAME).opt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

# LINK

$(NAME).byt:	$(OBJS)
	@$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)
	@printf "\r$(GREEN)Project compiled$(RESET)             \n"

$(NAME).opt:	$(OPTOBJS)
	@$(CAMLOPT) -o $(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

# COMPILATION

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	@printf "\r$(CYAN)Compiling $<$(RESET)               "
	@$(CAMLC) -c $<

.mli.cmi:
	@printf "\r$(CYAN)Compiling $<$(RESET)               "
	@$(CAMLC) -c $<

.ml.cmx:
	@printf "\r$(CYAN)Compiling $<$(RESET)               "
	@$(CAMLOPT) -c $<

# CLEANING

clean:
	@printf "\r$(PURPLE)Removing objects...$(RESET)"
	@rm -f *.cm[iox] *~ .*~ #*#
	@rm -f *.o
	@rm -f .depend
	@printf "\r$(PURPLE)Objects removed$(RESET)    \n"

fclean: clean
	@printf "\r$(RED)Removing executables...$(RESET)"
	@rm -f $(NAME)
	@rm -f $(NAME).byt
	@rm -f $(NAME).opt
	@printf "\r$(RED)Executables removed$(RESET)    \n"

re: fclean all

# DEPENDENCIES ORDER

depend: $(SOURCES)
	@$(CAMLDEP) $(SOURCES) > .depend
	@printf "\r$(CYAN)Dependencies created$(RESET)           \n"

-include .depend

# COLORS

RESET = \033[0m
RED = \033[31;1m
GREEN = \033[32;1m
CYAN = \033[36;1m
PURPLE = \033[35;1m
