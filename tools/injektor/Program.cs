using System.IO;

using var outRomStream = File.OpenWrite("basic.nes");
using var prgRomStream = File.OpenRead("basic.bin");
using var chrRomStream = File.OpenRead(@"..\..\..\font.chr");

//Write ines header
outRomStream.Write(new byte[16] {
    0x4E, 0x45, 0x53, 0x1A, //'NES' magic number
    0x02, // Size of PRG rom x16k
    0x01, // Size of CHR rom x8k
    0x01, // Flags 6 - Vertical mirroring, no SRAM, no Trainer, do not ignore mirroring, mapper 0 
    0x00, // Flags 7 - Mapper 0
    0x00, // Flags 8 - no PRG RAM
    0x00, // Flags 9 - NTSC
    0x10, // Flags 10 - NTSC, no PRG RAM
    0x00, 0x00, 0x00, 0x00, 0x00 // PADDING
}, 0, 16);

//Write the content of our assembled binary
var binContent = new byte[32 * 1024];
prgRomStream.Seek(32 * 1024, SeekOrigin.Begin);
prgRomStream.Read(binContent, 0, 32 * 1024);
outRomStream.Write(binContent, 0, 32 * 1024);
prgRomStream.Close();

//Write the CHR content of the smb3 ROM
var chrContent = new byte[8 * 1024];
chrRomStream.Read(chrContent, 0 , 8 * 1024);
outRomStream.Write(chrContent, 0, 8 * 1024);

outRomStream.Close();
outRomStream.Close();


