test1 :
	agda -i ~/fotc/src/fot -i. Lemma1ATP.agda
	agda2atp -i ~/fotc/src/fot -i. --atp=equinox --time=300 Lemma1ATP.agda

test2 :
	agda -i ~/fotc/src/fot -i. PropertiesATP.agda
	agda2atp -i ~/fotc/src/fot -i. --atp=vampire --time=180 PropertiesATP.agda
