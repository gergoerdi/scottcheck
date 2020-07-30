GAMES	= t0 t4
SAOS	= $(patsubst %, %.sao, $(GAMES))

all: $(SAOS)

%.sao: %.sck
	scottkit -c $< > $@
