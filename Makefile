##
## Makefile for ok in /home/garbo_r//Training/sdlocaml
## 
## Made by robin garbo
## Login   <garbo_r@epitech.net>
## 
## Started on  Sat Apr 30 16:45:04 2011 robin garbo
## Last update Sun May  1 21:48:19 2011 robin garbo
##


RESULT		= step3
SOURCES		= dispmaze.ml maze.ml main.ml
LIBS		= bigarray sdl sdlloader sdlttf sdlmixer
INCDIRS		= +sdl
RM		= rm -f

all		: nc

fclean		: clean
		$(RM) $(RESULT)

re		: fclean all

include OCamlMakefile
