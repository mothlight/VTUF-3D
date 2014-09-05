find ./maespa -type f -exec sed -i 's/\t/    /g' {} \;

find ./maespa -type f -exec sed -i 's/CHARACTER\*256/CHARACTER(256)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*60/CHARACTER(60)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*70/CHARACTER(70)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*80/CHARACTER(70)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*10/CHARACTER(10)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*8/CHARACTER(8)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*3/CHARACTER(3)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*1/CHARACTER(1)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER\*5/CHARACTER(5)/g' {} \;


find ./maespa -type f -exec sed -i 's/CHARACTER SPECIESNAMES(MAXSP)\*30/CHARACTER(30) SPECIESNAMES(MAXSP)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER PHYFILES(MAXSP)\*30/CHARACTER(30) PHYFILES(MAXSP)/g' {} \;
find ./maespa -type f -exec sed -i 's/CHARACTER STRFILES(MAXSP)\*30/CHARACTER(30) STRFILES(MAXSP)/g' {} \;


