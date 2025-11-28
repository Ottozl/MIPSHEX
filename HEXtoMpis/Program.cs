using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;
using System.IO;

// Small utility to convert HEX words to MIPS assembly (supports many common R/I/J instructions)
internal static class Program
{
    private static void Main()
    {
        var hexLines = new[]
        {
            @"01 00 17 24
09 00 D7 12
02 00 17 24
00 00 D7 12
03 00 17 24
00 00 D7 12
04 00 17 24
00 00 D7 12
00 00 00 00
5F 66 07 08
00 00 00 00
1A 80 0C 3C
1A 80 0D 3C
CD 02 87 93
             "
        };

        // exemplo usando slus offset
        var slusStart = "1CA1B8";
        var asmWithOffsets = ConvertHexToMipsWithOffsets(slusStart, hexLines, littleEndian: true);

        // create timestamped output filename so each run produces a new file
        var timestamp = DateTime.Now.ToString("yyyyMMdd_HHmmss");

        // find repository root by walking up until we find a .git folder or a .sln file
        string repoRoot = FindRepoRoot(Directory.GetCurrentDirectory());
        string outputsDir = Path.Combine(repoRoot, "outputs/HEX-MIPS");
        Directory.CreateDirectory(outputsDir);

        var outPath = Path.Combine(outputsDir, $"output_{timestamp}.csv");

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
                Console.WriteLine($"{hex},{slus},{ram},{asmField},{branchSlus},{branchRam}");
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

    // New: Convert hex list to MIPS assembly while also printing SLUS and RAM offsets per line.
    // slusStartHex accepts strings like "1CA140" or "0x1CA140". ramOffset = slus + 0x8000F800.
    public static List<string> ConvertHexToMipsWithOffsets(string slusStartHex, IEnumerable<string> hexLines, bool littleEndian = true)
    {
        if (hexLines == null) throw new ArgumentNullException(nameof(hexLines));
        if (string.IsNullOrWhiteSpace(slusStartHex)) throw new ArgumentNullException(nameof(slusStartHex));

        // normalize and parse slus start
        var s = slusStartHex.Trim();
        if (s.StartsWith("0x", StringComparison.OrdinalIgnoreCase)) s = s.Substring(2);
        if (!int.TryParse(s, NumberStyles.HexNumber, CultureInfo.InvariantCulture, out int slusStart))
            throw new FormatException($"SLUS offset inválido: {slusStartHex}");

        // initial register state (hardcoded values)
        uint reg_zero = 0;
        uint reg_at = 0;
        uint reg_v0 = 0;
        uint reg_v1 = 0;
        uint reg_a0 = 0;
        uint reg_a1 = 0;
        uint reg_a2 = 0;
        uint reg_a3 = 0;
        uint reg_t0 = 0;
        uint reg_t1 = 0;
        uint reg_t2 = 0;
        uint reg_t3 = 0;
        uint reg_t4 = 0;
        uint reg_t5 = 0;
        uint reg_t6 = 0;
        uint reg_t7 = 0;
        uint reg_s0 = 0;
        uint reg_s1 = 0;
        uint reg_s2 = 0;
        uint reg_s3 = 0;
        uint reg_s4 = 0;
        uint reg_s5 = 0;
        uint reg_s6 = 1; // as requested
        uint reg_s7 = 0;
        uint reg_t8 = 0;
        uint reg_t9 = 0;
        uint reg_k0 = 0;
        uint reg_k1 = 0;
        uint reg_gp = 0;
        uint reg_sp = 0;
        uint reg_fp = 0;
        uint reg_ra = 0;

        uint GetRegVal(int idx)
        {
            return idx switch
            {
                0 => reg_zero,
                1 => reg_at,
                2 => reg_v0,
                3 => reg_v1,
                4 => reg_a0,
                5 => reg_a1,
                6 => reg_a2,
                7 => reg_a3,
                8 => reg_t0,
                9 => reg_t1,
                10 => reg_t2,
                11 => reg_t3,
                12 => reg_t4,
                13 => reg_t5,
                14 => reg_t6,
                15 => reg_t7,
                16 => reg_s0,
                17 => reg_s1,
                18 => reg_s2,
                19 => reg_s3,
                20 => reg_s4,
                21 => reg_s5,
                22 => reg_s6,
                23 => reg_s7,
                24 => reg_t8,
                25 => reg_t9,
                26 => reg_k0,
                27 => reg_k1,
                28 => reg_gp,
                29 => reg_sp,
                30 => reg_fp,
                31 => reg_ra,
                _ => 0u,
            };
        }

        void SetRegVal(int idx, uint val)
        {
            switch (idx)
            {
                case 0: reg_zero = 0; break;
                case 1: reg_at = val; break;
                case 2: reg_v0 = val; break;
                case 3: reg_v1 = val; break;
                case 4: reg_a0 = val; break;
                case 5: reg_a1 = val; break;
                case 6: reg_a2 = val; break;
                case 7: reg_a3 = val; break;
                case 8: reg_t0 = val; break;
                case 9: reg_t1 = val; break;
                case 10: reg_t2 = val; break;
                case 11: reg_t3 = val; break;
                case 12: reg_t4 = val; break;
                case 13: reg_t5 = val; break;
                case 14: reg_t6 = val; break;
                case 15: reg_t7 = val; break;
                case 16: reg_s0 = val; break;
                case 17: reg_s1 = val; break;
                case 18: reg_s2 = val; break;
                case 19: reg_s3 = val; break;
                case 20: reg_s4 = val; break;
                case 21: reg_s5 = val; break;
                case 22: reg_s6 = val; break;
                case 23: reg_s7 = val; break;
                case 24: reg_t8 = val; break;
                case 25: reg_t9 = val; break;
                case 26: reg_k0 = val; break;
                case 27: reg_k1 = val; break;
                case 28: reg_gp = val; break;
                case 29: reg_sp = val; break;
                case 30: reg_fp = val; break;
                case 31: reg_ra = val; break;
            }
        }

        // flatten input into instruction lines
        var instrLines = hexLines
            .Where(x => !string.IsNullOrWhiteSpace(x))
            .SelectMany(x => x.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                              .Select(l => l.Trim())
                              .Where(l => !string.IsNullOrWhiteSpace(l)))
            .ToList();

        var outList = new List<string>();
        int currentSlus = slusStart;
        uint? pendingJump = null; // scheduled target SLUS to apply after delay slot
        const uint ramBase = 0x8000F800u;

        for (int i = 0; i < instrLines.Count; i++)
        {
            string line = instrLines[i];
            try
            {
                uint word = ParseHexWord(line, littleEndian);
                string asm = DecodeWord(word);
                uint ramOffset = (uint)currentSlus + ramBase;
                string slusStr = currentSlus.ToString("X6");
                string ramStr = ramOffset.ToString("X8");

                string branchSuffix = string.Empty;
                uint? newPendingJump = null;

                uint opcodeLocal = (word >> 26) & 0x3Fu;
                int rsIdx = (int)((word >> 21) & 0x1Fu);
                int rtIdx = (int)((word >> 16) & 0x1Fu);
                int rdIdx = (int)((word >> 11) & 0x1Fu);
                int shamt = (int)((word >> 6) & 0x1Fu);
                uint funct = word & 0x3Fu;
                int immSigned = (short)(word & 0xFFFF);
                uint uimm = word & 0xFFFFu;
                uint addr26 = word & 0x03FFFFFFu;

                // simulate writes for common instructions so branches see updated values
                if (opcodeLocal == 0x00)
                {
                    switch (funct)
                    {
                        case 0x20: // add
                        case 0x21: // addu
                            SetRegVal(rdIdx, GetRegVal(rsIdx) + GetRegVal(rtIdx));
                            break;
                        case 0x22: // sub
                        case 0x23: // subu
                            SetRegVal(rdIdx, GetRegVal(rsIdx) - GetRegVal(rtIdx));
                            break;
                        case 0x00: // sll
                            SetRegVal(rdIdx, GetRegVal(rtIdx) << shamt);
                            break;
                        case 0x02: // srl
                            SetRegVal(rdIdx, GetRegVal(rtIdx) >> shamt);
                            break;
                        case 0x09: // jalr rd, rs
                            // write return addr (SLUS) to rd
                            SetRegVal(rdIdx, (uint)((ramOffset + 8u) - ramBase));
                            break;
                    }
                }
                else
                {
                    switch (opcodeLocal)
                    {
                        case 0x08: // addi
                        case 0x09: // addiu
                            SetRegVal(rtIdx, (uint)((int)GetRegVal(rsIdx) + immSigned));
                            break;
                        case 0x0F: // lui
                            SetRegVal(rtIdx, (uimm << 16));
                            break;
                        case 0x0D: // ori
                            SetRegVal(rtIdx, GetRegVal(rsIdx) | uimm);
                            break;
                        case 0x0C: // andi
                            SetRegVal(rtIdx, GetRegVal(rsIdx) & uimm);
                            break;
                    }
                }

                // Branch evaluation: beq / bne
                if (opcodeLocal == 0x04 || opcodeLocal == 0x05)
                {
                    uint rsVal = GetRegVal(rsIdx);
                    uint rtVal = GetRegVal(rtIdx);
                    bool taken = opcodeLocal == 0x04 ? rsVal == rtVal : rsVal != rtVal;

                    uint ramNext = (uint)currentSlus + ramBase + 4u; // address of instruction after branch (delay slot)
                    long targetRamSigned = (long)ramNext + (long)immSigned * 4L;
                    uint targetRam = (uint)targetRamSigned;
                    uint targetSlus = targetRam - ramBase;

                    branchSuffix = $" ({targetSlus:X6}) [{targetRam:X8}]";

                    if (taken)
                        newPendingJump = targetSlus;
                }

                // REGIMM handling (bltz/bgez etc.) - treat common ones
                if (opcodeLocal == 0x01)
                {
                    int rt = (int)rtIdx;
                    int imm = immSigned;
                    bool taken = false;
                    uint rsVal = GetRegVal(rsIdx);
                    if (rt == 0x00) // bltz
                        taken = ((int)rsVal) < 0;
                    else if (rt == 0x01) // bgez
                        taken = ((int)rsVal) >= 0;
                    else if (rt == 0x10) // bltzal
                        taken = ((int)rsVal) < 0;
                    else if (rt == 0x11) // bgezal
                        taken = ((int)rsVal) >= 0;

                    if (taken)
                    {
                        uint ramNext = (uint)currentSlus + ramBase + 4u;
                        long targetRamSigned = (long)ramNext + (long)imm * 4L;
                        uint targetRam = (uint)targetRamSigned;
                        uint targetSlus = targetRam - ramBase;
                        branchSuffix = $" ({targetSlus:X6}) [{targetRam:X8}]";
                        newPendingJump = targetSlus;
                    }
                }

                // detect unconditional jumps j/jal
                if (opcodeLocal == 0x02 || opcodeLocal == 0x03)
                {
                    uint ramAddr = ((addr26 << 2) | 0x80000000u);
                    uint targetSlus = ramAddr - ramBase;
                    branchSuffix = branchSuffix.Length > 0 ? branchSuffix : $" [{ramAddr:X8}]";
                    newPendingJump = targetSlus;
                    if (opcodeLocal == 0x03)
                    {
                        // write return address to $ra (SLUS)
                        SetRegVal(31, (uint)((ramOffset + 8u) - ramBase));
                    }
                }

                // add output entry
                outList.Add($"{line}; {slusStr}; {ramStr}; {asm}{branchSuffix}");

                // Advance to next instruction (delay slot will be executed next)
                currentSlus += 4;

                // If there was a pending jump from the previous instruction, apply it now (after executing its delay slot)
                if (pendingJump.HasValue)
                {
                    currentSlus = (int)pendingJump.Value;
                    pendingJump = null;
                }

                // schedule jump from this instruction to be applied after the next instruction
                if (newPendingJump.HasValue)
                    pendingJump = newPendingJump;
            }
            catch (Exception ex)
            {
                outList.Add($".error \"{ex.Message} (line: {line})\"");
            }
        }

        // print register values after routine
        Console.WriteLine("\nRegister state after tracing:");
        var regPairs = new (string, uint)[]
        {
            ("$zero", reg_zero), ("$at", reg_at), ("$v0", reg_v0), ("$v1", reg_v1),
            ("$a0", reg_a0), ("$a1", reg_a1), ("$a2", reg_a2), ("$a3", reg_a3),
            ("$t0", reg_t0), ("$t1", reg_t1), ("$t2", reg_t2), ("$t3", reg_t3),
            ("$t4", reg_t4), ("$t5", reg_t5), ("$t6", reg_t6), ("$t7", reg_t7),
            ("$s0", reg_s0), ("$s1", reg_s1), ("$s2", reg_s2), ("$s3", reg_s3),
            ("$s4", reg_s4), ("$s5", reg_s5), ("$s6", reg_s6), ("$s7", reg_s7),
            ("$t8", reg_t8), ("$t9", reg_t9), ("$k0", reg_k0), ("$k1", reg_k1),
            ("$gp", reg_gp), ("$sp", reg_sp), ("$fp", reg_fp), ("$ra", reg_ra)
        };
        foreach (var (name, val) in regPairs)
        {
            Console.WriteLine($"{name} = 0x{val:X8} ({val})");
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
