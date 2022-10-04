using System.IO;

using var inRomStream = File.OpenRead("smb3.nes");
using var outRomStream = File.OpenWrite("basic.nes");
using var binStream = File.OpenRead("basic.bin");

//Copy over ROM header
var headerData = new byte[16];
inRomStream.Read(headerData, 0, 16);

outRomStream.Write(headerData, 0, 16);

//Write 64k of zeroes
var zeroes = new byte[64 * 1024];

for(var i = 0; i < 3; i++)
    outRomStream.Write(zeroes, 0, 64 * 1024);

//Write the content of our assembled binary
var binContent = new byte[64 * 1024];
binStream.Read(binContent, 0, 64 * 1024);
outRomStream.Write(binContent, 0, 64 * 1024);
binStream.Close();

//Write the CHR content of the smb3 ROM
var chrContent = new byte[128 * 1024];
inRomStream.Seek(256 * 1024, SeekOrigin.Current);
inRomStream.Read(chrContent, 0 , 128 * 1024);
outRomStream.Write(chrContent, 0, 128 * 1024);

outRomStream.Close();
inRomStream.Close();


