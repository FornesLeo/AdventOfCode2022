##
## EPITECH PROJECT, 2018
## deBruijn
## File description:
## makefile
##

NAME = AdventOfCode2022

BINPATH = $(shell stack path --local-install-root)/bin/$(NAME)-exe

all:
	 stack build
	 cp $(BINPATH) $(NAME)

fclean:
	rm $(NAME)

re: fclean all

.PHONY: all fclean re
