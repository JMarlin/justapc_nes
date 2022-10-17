ca65 -g basic.asm -o basic.o
ld65 -vm -m symbols.txt -o basic.bin -C link.cfg basic.o
cp basic.bin tools\injektor\
cd tools\injektor\
dotnet run
cd ..\..\
