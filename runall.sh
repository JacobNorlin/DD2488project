for file in *.class
do
  java $file >> results.out
done
