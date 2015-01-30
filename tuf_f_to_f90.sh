cp TUFreg3D.f TUFreg3D.f90

sed -i 's/^c/!/' TUFreg3D.f90
sed -i 's/^C/!/' TUFreg3D.f90
sed -i 's/\t/    /g' TUFreg3D.f90

sed -i 's/!DIR$ FREE//' TUFreg3D.f90
sed -i 's/!DIR$ FIXED//' TUFreg3D.f90

sed -i 's/CHARACTER\*256/CHARACTER(256)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*60/CHARACTER(60)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*70/CHARACTER(70)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*80/CHARACTER(70)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*10/CHARACTER(10)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*8/CHARACTER(8)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*3/CHARACTER(3)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*1/CHARACTER(1)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*5/CHARACTER(5)/g' TUFreg3D.f90
sed -i 's/CHARACTER\*2/CHARACTER(2)/g' TUFreg3D.f90


sed -i 's/character\*256/character(256)/g' TUFreg3D.f90
sed -i 's/character\*60/character(60)/g' TUFreg3D.f90
sed -i 's/character\*70/character(70)/g' TUFreg3D.f90
sed -i 's/character\*80/character(70)/g' TUFreg3D.f90
sed -i 's/character\*10/character(10)/g' TUFreg3D.f90
sed -i 's/character\*8/character(8)/g' TUFreg3D.f90
sed -i 's/character\*3/character(3)/g' TUFreg3D.f90
sed -i 's/character\*1/character(1)/g' TUFreg3D.f90
sed -i 's/character\*5/character(5)/g' TUFreg3D.f90
sed -i 's/character\*2/character(2)/g' TUFreg3D.f90
sed -i 's/character\*4/character(4)/g' TUFreg3D.f90



sed -i 's/real\*8/real(8)/g' TUFreg3D.f90

export CLASSPATH=$CLASSPATH:. 
/usr/bin/java -cp "/home/kerryn/workspace/TUF-3DRadiationOnly2"   Repage > /tmp/TUFreg3D.f90
cat /tmp/TUFreg3D.f90 > TUFreg3D.f90

exit 1

sed -i 's/                    //g' TUFreg3D.f90
sed -i '$!N;s/\n\s*&//;P;D' TUFreg3D.f90
sed -i 's/                    //g' TUFreg3D.f90
sed -i '$!N;s/\n\s*&//;P;D' TUFreg3D.f90
sed -i 's/                    //g' TUFreg3D.f90
sed -i '$!N;s/\n\s*&//;P;D' TUFreg3D.f90
sed -i 's/                    //g' TUFreg3D.f90
sed -i '$!N;s/\n\s*&//;P;D' TUFreg3D.f90
sed -i 's/                    //g' TUFreg3D.f90
sed -r -i 's/.{132}/&\&\n/g' TUFreg3D.f90



#sed -i ':a;N;$!ba;s/\n/ /g' TUFreg3D.f90
#perl -p -e  's/\n/ /' TUFreg3D.f90 
#sed -i 's/\n+\s\&//g' TUFreg3D.f90
#sed -i 's/\n.*&/  /g' TUFreg3D.f90
#sed -i 's/.{132}/&\&\n/g' TUFreg3D.f90
#awk '{gsub(/.{132}/,"&\\&\n")}1' TUFreg3D.f90 > /tmp/TUFreg3D.f90
#cat /tmp/TUFreg3D.f90 > TUFreg3D.f90


