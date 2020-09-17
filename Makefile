##
## EPITECH PROJECT, 2018
## MAKEFILE
## File description:
## Makefile
##

USELESS =	src/*.hi src/*~ src/*.o

SRC		=	src/Main.hs				\
			src/Options.hs			\
			src/Utils.hs			\
			src/Wolfram.hs			\
			src/Rules.hs

GHC		=	ghc -o

NAME	=	wolfram

all:		$(NAME)

$(NAME):
	$(GHC) $(NAME) $(SRC)

clean:
	rm -f $(USELESS)

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY:	clean fclean re all
