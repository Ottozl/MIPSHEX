using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;
using System.IO;

// Small utility to convert HEX words to MIPS assembly (supports many common R/I/J instructions)
internal static class Program
{
    // Hard-coded register hex identifiers for comparison at execution time
    private static readonly Dictionary<string, uint> RegHex = new()
    {
        {"$zero", 0x00}, {"$at", 0x01}, {"$v0", 0x02}, {"$v1", 0x03},
        {"$a0", 0x04}, {"$a1", 0x05}, {"$a2", 0x06}, {"$a3", 0x07},
        {"$t0", 0x08}, {"$t1", 0x09}, {"$t2", 0x0A}, {"$t3", 0x0B},
        {"$t4", 0x0C}, {"$t5", 0x0D}, {"$t6", 0x0E}, {"$t7", 0x0F},
        {"$s0", 0x10}, {"$s1", 0x11}, {"$s2", 0x12}, {"$s3", 0x13},
        {"$s4", 0x14}, {"$s5", 0x15}, {"$s6", 0x16}, {"$s7", 0x17},
        {"$t8", 0x18}, {"$t9", 0x19}, {"$k0", 0x1A}, {"$k1", 0x1B},
        {"$gp", 0x1C}, {"$sp", 0x1D}, {"$fp", 0x1E}, {"$ra", 0x1F},
    };

    // Mutable register values representing execution-time state; update these during execution as needed
    private static readonly Dictionary<string, uint> RegValues = new()
    {
        {"$zero", 0x00000000u}, {"$at", 0x00000000u}, {"$v0", 0x00000000u}, {"$v1", 0x00000000u},
        {"$a0",   0x00000000u}, {"$a1", 0x00000000u}, {"$a2", 0x00000000u}, {"$a3", 0x00000000u},
        {"$t0",   0x00000000u}, {"$t1", 0x00000000u}, {"$t2", 0x00000000u}, {"$t3", 0x00000000u},
        {"$t4",   0x00000000u}, {"$t5", 0x00000000u}, {"$t6", 0x00000000u}, {"$t7", 0x00000000u},
        {"$s0",   0x00000000u}, {"$s1", 0x00000000u}, {"$s2", 0x00000000u}, {"$s3", 0x00000000u},
        {"$s4",   0x00000000u}, {"$s5", 0x00000000u}, {"$s6", 0x00000000u}, {"$s7", 0x00000000u},
        {"$t8",   0x00000000u}, {"$t9", 0x00000000u}, {"$k0", 0x00000000u}, {"$k1", 0x00000000u},
        {"$gp",   0x00000000u}, {"$sp", 0x00000000u}, {"$fp", 0x00000000u}, {"$ra", 0x00000000u},
    };

    private static void Main()
    {
        var hexLines = new[]
        {
            @"00 00 08 25
01 00 08 25
02 00 08 25
03 00 08 25
04 00 08 25
05 00 08 25
06 00 08 25
07 00 08 25
08 00 08 25
09 00 08 25
0A 00 08 25
0B 00 08 25
0C 00 08 25
0D 00 08 25
0E 00 08 25
0F 00 08 25
10 00 08 25
11 00 08 25
12 00 08 25
13 00 08 25
14 00 08 25
15 00 08 25
16 00 08 25
17 00 08 25
18 00 08 25
19 00 08 25
03 00 09 11
00 00 00 00
21 3E 00 08
00 00 00 00
00 00 00 00
01 00 09 24
FA FF 00 11
00 00 00 00
00 00 00 00
"
        };

        // exemplo usando slus offset
        var slusStart = "00000000";
        // routine boundaries (SLUS offsets) - set to desired values
        var routineStart = "68"; // start SLUS
        var routineFinish = "88"; // finish SLUS
        var asmWithOffsets = ConvertHexToMipsWithOffsets(slusStart, hexLines, littleEndian: true, routineStartHex: routineStart, routineFinishHex: routineFinish);

        // create timestamped output filename so each run produces a new file
        var timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss");

        // find repository root by walking up until we find a .git folder or a .sln file
        string repoRoot = FindRepoRoot(Directory.GetCurrentDirectory());
        string outputsDir = Path.Combine(repoRoot, "outputs/HEX-MIPS");
        Directory.CreateDirectory(outputsDir);

        var outPath = Path.Combine(outputsDir, $"output_{timestamp}.csv");

        // Print final register values before routine output
        Console.WriteLine("Final register values (after execution):");
        foreach (var kv in RegValues.OrderBy(k => RegHex[k.Key]))
        {
            var pad = kv.Key == "$zero" ? string.Empty : "  ";
            Console.WriteLine($"{pad}{kv.Key} = 0x{kv.Value:X8}");
        }
        Console.WriteLine();

        // temporary: print CSV header to file and console (first column = original HEX), keep identical remaining columns
        using (var sw = new StreamWriter(outPath, false, System.Text.Encoding.UTF8))
        {
            sw.WriteLine("HEX,SLUS,RAM,ASM,BranchTargetSLUS,BranchTargetRAM");

            Console.WriteLine("HEX,SLUS,RAM,ASM,BranchTargetSLUS,BranchTargetRAM");

            foreach (var a in asmWithOffsets)
            {
                // expected format now: "HEX; SLUS; RAM; ASM( maybe branchSuffix )"
                var parts = a.Split(new[] { ';' }, 4);
                string hex = parts.Length > 0 ? parts[0].Trim() : string.Empty;
                string slus = parts.Length > 1 ? parts[1].Trim() : string.Empty;
                string ram = parts.Length > 2 ? parts[2].Trim() : string.Empty;
                string asmAndBranch = parts.Length > 3 ? parts[3].Trim() : string.Empty;

                string asmField = asmAndBranch;
                string branchSlus = string.Empty;
                string branchRam = string.Empty;

                // match suffix like: " (TARGETSLUS) [TARGETRAM]" at end
                var m = Regex.Match(asmAndBranch, "\\s*\\((?<bslus>[0-9A-Fa-f]{1,8})\\)\\s*\\[(?<bram>[0-9A-Fa-f]{1,8})\\]$");
                if (m.Success)
                {
                    branchSlus = m.Groups["bslus"].Value;
                    branchRam = m.Groups["bram"].Value;
                    asmField = asmAndBranch.Substring(0, m.Index).TrimEnd();
                }

                // escape quotes for CSV and wrap ASM field in quotes
                asmField = "\"" + asmField.Replace("\"", "\"\"") + "\"";

                sw.WriteLine($"{hex},{slus},{ram},{asmField},{branchSlus},{branchRam}");
                Console.WriteLine($"{hex} | {slus} | {ram} | {asmField} | {branchSlus} | {branchRam}");
            }
        }

        Console.WriteLine($"Wrote {asmWithOffsets.Count} lines to {Path.GetFullPath(outPath)}");
        Console.WriteLine($"Wrote {asmWithOffsets.Count} lines (printed to console)");

        // local function to locate repository root
        static string FindRepoRoot(string start)
        {
            try
            {
                var dir = new DirectoryInfo(start);
                for (int i = 0; i < 20 && dir != null; i++)
                {
                    if (Directory.Exists(Path.Combine(dir.FullName, ".git")))
                        return dir.FullName;

                    if (Directory.EnumerateFiles(dir.FullName, "*.sln").Any())
                        return dir.FullName;

                    dir = dir.Parent;
                }
            }
            catch
            {
                // ignore and fallback to start
            }
            return start;
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

    // Decode 32-bit instruction word into MIPS assembly (subset) without PC context
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
                    return $"j 0x{ramAddr:X8} ({slus:X6}) [0x{ramAddr:X8}]";
                }
            case 0x03: // jal
                {
                    uint ramAddr = ((addr26 << 2) | 0x80000000u);
                    uint slus = ramAddr - 0x8000F800u;
                    return $"jal 0x{ramAddr:X8} ({slus:X6}) [0x{ramAddr:X8}]";
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

    // Decode with PC (RAM) context to compute branch targets and flag beq/bne
    private static string DecodeWord(uint w, uint currentRam)
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
            case 0x02: // j
            {
                uint ramAddr = ((addr26 << 2) | 0x80000000u);
                uint slus = ramAddr - 0x8000F800u;
                return $"j 0x{ramAddr:X8} ({slus:X6}) [0x{ramAddr:X8}]";
            }
            case 0x03: // jal
            {
                uint ramAddr = ((addr26 << 2) | 0x80000000u);
                uint slus = ramAddr - 0x8000F800u;
                return $"jal 0x{ramAddr:X8} ({slus:X6}) [0x{ramAddr:X8}]";
            }
            case 0x04: // beq
            {
                string rsn = RegName(rs);
                string rtn = RegName(rt);
                bool taken = RegValues[rsn] == RegValues[rtn];
                uint targetRam = unchecked(currentRam + 4u + (uint)(imm * 4));
                uint targetSlus = targetRam - 0x8000F800u;
                return $"{(taken ? "[true]" : "[false]")} beq {rsn}, {rtn}, {imm} ({targetSlus:X6}) [0x{targetRam:X8}]";
            }
            case 0x05: // bne
            {
                string rsn = RegName(rs);
                string rtn = RegName(rt);
                bool taken = RegValues[rsn] != RegValues[rtn];
                uint targetRam = unchecked(currentRam + 4u + (uint)(imm * 4));
                uint targetSlus = targetRam - 0x8000F800u;
                return $"{(taken ? "[true]" : "[false]")} bne {rsn}, {rtn}, {imm} ({targetSlus:X6}) [0x{targetRam:X8}]";
            }
            default:
                // Fallback to non-PC-aware decoding for other opcodes
                return DecodeWord(w);
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

    // New: Convert hex list to MIPS assembly while also printing SLUS and RAM offsets per line.
    // slusStartHex accepts strings like "1CA140" or "0x1CA140". ramOffset = slus + 0x8000F800.
    private static List<string> ConvertHexToMipsWithOffsets(
    string slusStartHex,
    IEnumerable<string> hexLines,
    bool littleEndian = true,
    string? routineStartHex = null,
    string? routineFinishHex = null)
    {
        static uint ParseAddr(string s)
        {
            if (string.IsNullOrWhiteSpace(s)) throw new ArgumentException(nameof(s));
            s = s.Trim();
            if (s.StartsWith("0x", StringComparison.OrdinalIgnoreCase)) s = s.Substring(2);
            return uint.Parse(s, NumberStyles.HexNumber, CultureInfo.InvariantCulture);
        }

        var baseAddr = ParseAddr(slusStartHex);

        // Flatten all input into per-instruction lines preserving original text per word
        var flatLines = new List<string>();
        foreach (var raw in hexLines ?? throw new ArgumentNullException(nameof(hexLines)))
        {
            if (string.IsNullOrWhiteSpace(raw)) continue;
            var sub = raw.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                         .Select(l => l.Trim())
                         .Where(l => !string.IsNullOrWhiteSpace(l));
            flatLines.AddRange(sub);
        }

        int startWordIndex = 0;
        if (!string.IsNullOrWhiteSpace(routineStartHex))
        {
            var routineAddr = ParseAddr(routineStartHex);
            if (routineAddr < baseAddr)
                throw new InvalidOperationException("routineStartHex é menor que a base (slusStartHex)");
            startWordIndex = checked((int)((routineAddr - baseAddr) / 4));
        }

        int? endWordIndex = null;
        if (!string.IsNullOrWhiteSpace(routineFinishHex))
        {
            var finishAddr = ParseAddr(routineFinishHex);
            if (finishAddr < baseAddr)
                throw new InvalidOperationException("routineFinishHex é menor que a base (slusStartHex)");
            endWordIndex = checked((int)((finishAddr - baseAddr) / 4));
        }

        if (startWordIndex < 0 || startWordIndex > flatLines.Count)
            throw new ArgumentOutOfRangeException(nameof(routineStartHex), "Índice inicial fora do intervalo das linhas HEX.");

        int takeCount = (endWordIndex is int e && e > startWordIndex && e <= flatLines.Count)
            ? (e - startWordIndex)
            : (flatLines.Count - startWordIndex);

        var result = new List<string>(takeCount);
        int i = 0;
        while (i < takeCount)
        {
            int globalIndex = startWordIndex + i;
            string originalHex = flatLines[globalIndex];

            uint slus = baseAddr + (uint)(globalIndex * 4);
            uint ram = slus + 0x8000F800u;

            uint word;
            try
            {
                word = ParseHexWord(originalHex, littleEndian);
            }
            catch (Exception ex)
            {
                string asmErr = $".error \"{ex.Message} (line: {originalHex})\"";
                result.Add($"{originalHex}; {slus:X6}; {ram:X8}; {asmErr}");
                i++;
                continue;
            }

            // Decode with PC context
            string asm = DecodeWord(word, ram);
            result.Add($"{originalHex}; {slus:X6}; {ram:X8}; {asm}");

            // Branch/Jump flow control: if beq/bne is taken or j/jal, execute delay slot, then jump to target
            uint opcode = (word >> 26) & 0x3Fu;
            if (opcode == 0x04 || opcode == 0x05 || opcode == 0x02 || opcode == 0x03)
            {
                bool shouldJump = false;
                uint targetRam = 0u;

                if (opcode == 0x04 || opcode == 0x05)
                {
                    int rs = (int)((word >> 21) & 0x1Fu);
                    int rt = (int)((word >> 16) & 0x1Fu);
                    int imm = (short)(word & 0xFFFF);
                    string rsn = RegName(rs);
                    string rtn = RegName(rt);
                    bool taken = opcode == 0x04 ? (RegValues[rsn] == RegValues[rtn]) : (RegValues[rsn] != RegValues[rtn]);
                    shouldJump = taken;
                    if (taken)
                    {
                        targetRam = unchecked(ram + 4u + (uint)(imm * 4));
                    }
                }
                else // j / jal
                {
                    uint addr26 = word & 0x03FFFFFFu;
                    targetRam = ((addr26 << 2) | 0x80000000u);
                    shouldJump = true; // unconditional
                }

                if (shouldJump)
                {
                    // Execute delay slot (next sequential instruction) if it exists
                    if (i + 1 < takeCount)
                    {
                        int delayGlobalIndex = startWordIndex + (i + 1);
                        string delayHex = flatLines[delayGlobalIndex];
                        uint delaySlus = baseAddr + (uint)(delayGlobalIndex * 4);
                        uint delayRam = delaySlus + 0x8000F800u;
                        string delayAsm;
                        try
                        {
                            uint delayWord = ParseHexWord(delayHex, littleEndian);
                            delayAsm = DecodeWord(delayWord, delayRam);
                        }
                        catch (Exception ex)
                        {
                            delayAsm = $".error \"{ex.Message} (line: {delayHex})\"";
                        }
                        result.Add($"{delayHex}; {delaySlus:X6}; {delayRam:X8}; {delayAsm}");
                    }

                    // Map target RAM to SLUS and index within flatLines
                    uint targetSlus = targetRam - 0x8000F800u;
                    int targetIndex = (int)((targetSlus - baseAddr) / 4);

                    if (targetIndex >= 0 && targetIndex < flatLines.Count)
                    {
                        // Jump: set i to target index relative to slice
                        i = targetIndex - startWordIndex;
                        continue;
                    }
                    else
                    {
                        // Target out of range; stop execution
                        break;
                    }
                }
            }

            // Normal sequential advance
            i++;
        }

        return result;
    }
}