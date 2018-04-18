OC = ocamlc
EXEC = anim

# Edition des liens et création de l'exécutable
anim: animation.cmo 	
	$(OC) unix.cma graphics.cma -o anim animation.cmo

# Compilation du corps du module Animation
animation.cmo: animation.ml animation.cmi
	$(OC) -c animation.ml

# Compilation de l'interface du module Animation
animation.cmi: animation.mli
	$(OC) animation.mli

# Suppression des fichiers compilés et de l'exécutable
clean:
	rm -f *.cm[io] *~
	rm -f $(EXEC)
