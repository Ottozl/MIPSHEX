using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;

// Small utility to convert HEX words to MIPS assembly (supports many common R/I/J instructions)
internal static class Program
{
    private static void Main()
    {
        var hexLines = new[]
        {
            @"04 00 00 11
00 00 00 00
48 02 88 97
02 00 16 11
00 00 00 00
CD 02 87 93"
        };

        // exemplo usando slus offset
        var slusStart = "00000000";
        var asmWithOffsets = ConvertHexToMipsWithOffsets(slusStart, hexLines, littleEndian: true);
        int printed = 0;
        const int pageSize = 9000;
        foreach (var a in asmWithOffsets)
        {
            Console.WriteLine(a);
            printed++;
            if (printed % pageSize == 0)
            {
                Console.WriteLine("-- Pressione Enter para continuar...");
                Console.ReadLine();
                try { Console.Clear(); } catch { /* ignore if no console available */ }
            }
        }
    }

    // Convert a sequence of hex representations to MIPS assembly lines.
    // Accepts formats: "0xXXXXXXXX", "XXXXXXXX", or bytes "AA BB CC DD".
    // If littleEndian==true the input bytes/hex are treated as little-endian and will be converted to the logical instruction word.
    public static List<string> ConvertHexToMips(IEnumerable<string> hexLines, bool littleEndian = true)
    {
        if (hexLines == null) throw new ArgumentNullException(nameof(hexLines));
        var outList = new List<string>();
        foreach (var raw in hexLines)
        {
            if (string.IsNullOrWhiteSpace(raw)) continue;
            // allow each input element to contain multiple lines
            var subLines = raw.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                              .Select(l => l.Trim())
                              .Where(l => !string.IsNullOrWhiteSpace(l));
            foreach (var line in subLines)
            {
                try
                {
                    uint word = ParseHexWord(line, littleEndian);
                    outList.Add(DecodeWord(word));
                }
                catch (Exception ex)
                {
                    outList.Add($".error \"{ex.Message} (line: {line})\"");
                }
            }
        }
        return outList;
    }

    // New: Convert hex list to MIPS assembly while also printing SLUS and RAM offsets per line.
    // slusStartHex accepts strings like "1CA140" or "0x1CA140". ramOffset = slus + 0x8000F800.
    public static List<string> ConvertHexToMipsWithOffsets(string slusStartHex, IEnumerable<string> hexLines, bool littleEndian = true)
    {
        if (hexLines == null) throw new ArgumentNullException(nameof(hexLines));
        if (string.IsNullOrWhiteSpace(slusStartHex)) throw new ArgumentNullException(nameof(slusStartHex));

        // normalize and parse slus start
        var s = slusStartHex.Trim();
        if (s.StartsWith("0x", StringComparison.OrdinalIgnoreCase)) s = s.Substring(2);
        int slusStart;
        try
        {
            slusStart = int.Parse(s, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
        }
        catch (Exception ex)
        {
            throw new FormatException($"SLUS offset inválido: {slusStartHex}", ex);
        }

        var outList = new List<string>();
        int currentSlus = slusStart;
        const uint ramBase = 0x8000F800u;

        foreach (var raw in hexLines)
        {
            if (string.IsNullOrWhiteSpace(raw)) continue;
            var subLines = raw.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                              .Select(l => l.Trim())
                              .Where(l => !string.IsNullOrWhiteSpace(l));
            foreach (var line in subLines)
            {
                try
                {
                    uint word = ParseHexWord(line, littleEndian);
                    string asm = DecodeWord(word);
                    uint ramOffset = (uint)currentSlus + ramBase;
                    // format SLUS as 6 hex digits and RAM as 8 hex digits, uppercase
                    string slusStr = currentSlus.ToString("X6");
                    string ramStr = ramOffset.ToString("X8");

                    // If this is a branch (beq or beqz represented as beq with $zero), compute and append target SLUS and RAM
                    string branchSuffix = string.Empty;
                    // detect beq (including beq ...,$zero,... which is beqz semantic)
                    if (asm.StartsWith("beq ", StringComparison.OrdinalIgnoreCase))
                    {
                        // immediate is lower 16 bits, signed, indicates number of instructions to jump relative to next instruction
                        int imm = (short)(word & 0xFFFF);
                        // current instruction RAM address
                        uint ramCurr = ramOffset;
                        uint ramNext = ramCurr + 4u;
                        long targetRamSigned = (long)ramNext + (long)imm * 4L;
                        uint targetRam = (uint)targetRamSigned;
                        uint targetSlus = targetRam - ramBase;
                        branchSuffix = $" ({targetSlus:X6}) [{targetRam:X8}]";
                    }

                    outList.Add($"{slusStr}; {ramStr}; {asm}{branchSuffix}");
                }
                catch (Exception ex)
                {
                    outList.Add($".error \"{ex.Message} (line: {line})\"");
                }
                currentSlus += 4;
            }
        }

        return outList;
    }

    // Parse a single hex line into a uint word. Supports three formats.
    private static uint ParseHexWord(string s, bool littleEndian)
    {
        // bytes like "AA BB CC DD" (4 byte tokens)
        if (Regex.IsMatch(s, "^([0-9A-Fa-f]{2}([ \t]+|$)){4}$"))
        {
            var parts = s.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            byte[] bytes = parts.Select(p => Convert.ToByte(p, 16)).ToArray();
            // Interpret the provided byte sequence according to requested endianness
            if (littleEndian)
            {
                // parts[0] is least-significant byte
                return (uint)bytes[0] | (uint)bytes[1] << 8 | (uint)bytes[2] << 16 | (uint)bytes[3] << 24;
            }
            else
            {
                // parts[0] is most-significant byte
                return (uint)bytes[0] << 24 | (uint)bytes[1] << 16 | (uint)bytes[2] << 8 | (uint)bytes[3];
            }
        }

        // hex with 0x or plain 8 hex digits; treat string as sequence of 4 bytes in the given order
        var t = s.StartsWith("0x", StringComparison.OrdinalIgnoreCase) ? s.Substring(2) : s;
        if (t.Length != 8 || !Regex.IsMatch(t, "^[0-9A-Fa-f]{8}$"))
            throw new FormatException($"Formato HEX inválido: {s}");

        // split into byte pairs in the same order they appear
        byte[] bytePairs = new byte[4];
        for (int i = 0; i < 4; i++)
        {
            string pair = t.Substring(i * 2, 2);
            bytePairs[i] = Convert.ToByte(pair, 16);
        }

        if (littleEndian)
        {
            // pair[0] is least-significant byte
            return (uint)bytePairs[0] | (uint)bytePairs[1] << 8 | (uint)bytePairs[2] << 16 | (uint)bytePairs[3] << 24;
        }
        else
        {
            // pair[0] is most-significant byte
            return (uint)bytePairs[0] << 24 | (uint)bytePairs[1] << 16 | (uint)bytePairs[2] << 8 | (uint)bytePairs[3];
        }
    }

    // Decode 32-bit instruction word into MIPS assembly (subset)
    private static string DecodeWord(uint w)
    {
        if (w == 0u) return "nop";

        uint opcode = (w >> 26) & 0x3Fu;
        int rs = (int)((w >> 21) & 0x1Fu);
        int rt = (int)((w >> 16) & 0x1Fu);
        int rd = (int)((w >> 11) & 0x1Fu);
        int shamt = (int)((w >> 6) & 0x1Fu);
        uint funct = w & 0x3Fu;
        int imm = (short)(w & 0xFFFF);
        uint uimm = w & 0xFFFFu;
        uint addr26 = w & 0x03FFFFFFu;

        switch (opcode)
        {
            case 0x00: // R-type
                switch (funct)
                {
                    case 0x20: return $"add {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x21: return $"addu {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x22: return $"sub {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x23: return $"subu {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x24: return $"and {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x25: return $"or {RegName(rd)}, {RegName(rs)}, {RegName(rt)}";
                    case 0x00: return $"sll {RegName(rd)}, {RegName(rt)}, {shamt}";
                    case 0x02: return $"srl {RegName(rd)}, {RegName(rt)}, {shamt}";
                    case 0x09: return $"jalr {RegName(rd)}, {RegName(rs)}";
                    case 0x08: return $"jr {RegName(rs)}";
                    case 0x10: return $"mfhi {RegName(rd)}";
                    case 0x12: return $"mflo {RegName(rd)}";
                    case 0x11: return $"mthi {RegName(rs)}";
                    case 0x13: return $"mtlo {RegName(rs)}";
                    case 0x18: return $"mult {RegName(rs)}, {RegName(rt)}";
                    case 0x19: return $"multu {RegName(rs)}, {RegName(rt)}";
                    case 0x1A: return $"div {RegName(rs)}, {RegName(rt)}";
                    case 0x1B: return $"divu {RegName(rs)}, {RegName(rt)}";
                    case 0x0C: return "syscall";
                    case 0x0D: return "break";
                    default:
                        return $".rtype opcode=0 funct=0x{funct:X2} rs={RegName(rs)} rt={RegName(rt)} rd={RegName(rd)} sh={shamt}";
                }

            case 0x02: // j
                {
                    uint ramAddr = ((addr26 << 2) | 0x80000000u);
                    uint slus = ramAddr - 0x8000F800u;
                    return $"j 0x{ramAddr:X8} ({slus:X6})";
                }
            case 0x03: // jal
                {
                    uint ramAddr = ((addr26 << 2) | 0x80000000u);
                    uint slus = ramAddr - 0x8000F800u;
                    return $"jal 0x{ramAddr:X8} ({slus:X6})";
                }

            case 0x04: return $"beq {RegName(rs)}, {RegName(rt)}, {imm}";
            case 0x05: return $"bne {RegName(rs)}, {RegName(rt)}, {imm}";
            case 0x06: return $"blez {RegName(rs)}, {imm}";
            case 0x07: return $"bgtz {RegName(rs)}, {imm}";

            case 0x08: return $"addi {RegName(rt)}, {RegName(rs)}, {imm} [0x{uimm:X4}]";
            case 0x09: return $"addiu {RegName(rt)}, {RegName(rs)}, {imm} [0x{uimm:X4}]";
            case 0x0A: return $"slti {RegName(rt)}, {RegName(rs)}, {imm} [0x{uimm:X4}]";
            case 0x0B: return $"sltiu {RegName(rt)}, {RegName(rs)}, {imm} [0x{uimm:X4}]";
            case 0x0C: return $"andi {RegName(rt)}, {RegName(rs)}, 0x{uimm:X4}";
            case 0x0D: return $"ori {RegName(rt)}, {RegName(rs)}, 0x{uimm:X4}";
            case 0x0F: return $"lui {RegName(rt)}, 0x{uimm:X4}";

            case 0x20: return $"lb {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x21: return $"lh {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x23: return $"lw {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x24: return $"lbu {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x25: return $"lhu {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x28: return $"sb {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x29: return $"sh {RegName(rt)}, {imm}({RegName(rs)})";
            case 0x2B: return $"sw {RegName(rt)}, {imm}({RegName(rs)})";

            case 0x01: // REGIMM
                switch (rt)
                {
                    case 0x00: return $"bltz {RegName(rs)}, {imm}";
                    case 0x01: return $"bgez {RegName(rs)}, {imm}";
                    case 0x10: return $"bltzal {RegName(rs)}, {imm}";
                    case 0x11: return $"bgezal {RegName(rs)}, {imm}";
                    default: return $"regimm rt=0x{rt:X} rs={RegName(rs)} imm={imm}";
                }

            default:
                return $".opcode 0x{opcode:X2} word=0x{w:X8}";
        }
    }

    private static string RegName(int idx)
    {
        // prefer symbolic names
        string[] names = new[]
        {
            "$zero","$at","$v0","$v1","$a0","$a1","$a2","$a3",
            "$t0","$t1","$t2","$t3","$t4","$t5","$t6","$t7",
            "$s0","$s1","$s2","$s3","$s4","$s5","$s6","$s7",
            "$t8","$t9","$k0","$k1","$gp","$sp","$fp","$ra"
        };
        if (idx >= 0 && idx < names.Length) return names[idx];
        return "$r" + idx;
    }
}
