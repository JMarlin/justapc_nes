ca65 basic.asm -o basic.o
ld65 -o basic.bin -C link.cfg basic.o
cp basic.bin tools\injektor\
cd tools\injektor\
dotnet run
cd ..\..\
