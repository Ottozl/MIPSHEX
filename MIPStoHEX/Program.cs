using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;

internal static class Program
{
    // Exemplo de uso rápido
    private static void Main()
    {
        string source = @"
           lhu $s6, 12($s0)
        ";

        var hex = ConvertMipsToHex(source);
        foreach (var h in hex)
            Console.WriteLine(h);
    }

    // Converte um bloco de código MIPS (string com múltiplas linhas) para lista de HEX 32-bit (8 dígitos, prefixo 0x)
    public static List<string> ConvertMipsToHex(string source)
    {
        if (source is null) throw new ArgumentNullException(nameof(source));

        var lines = source.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                          .Select(l => l.Trim())
                          .ToList();

        // Remover comentários e linhas vazias
        for (int i = 0; i < lines.Count; i++)
        {
            var idx = lines[i].IndexOf('#');
            if (idx >= 0) lines[i] = lines[i].Substring(0, idx).Trim();
            lines[i] = lines[i].Trim();
        }
        lines = lines.Where(l => !string.IsNullOrWhiteSpace(l)).ToList();

        // Primeiro passe: localizar rótulos e instruções
        var labelToIndex = new Dictionary<string, int>(StringComparer.Ordinal);
        var instructions = new List<string>();
        for (int i = 0, instIndex = 0; i < lines.Count; i++)
        {
            var line = lines[i];

            // Pode haver label(s) na mesma linha: "label: instr ..." ou só "label:"
            while (true)
            {
                var m = Regex.Match(line, @"^\s*([A-Za-z_]\w*):\s*(.*)$");
                if (!m.Success) break;
                var label = m.Groups[1].Value;
                if (labelToIndex.ContainsKey(label))
                    throw new InvalidOperationException($"Rótulo duplicado: {label}");
                labelToIndex[label] = instIndex;
                line = m.Groups[2].Value.Trim();
            }

            if (string.IsNullOrEmpty(line)) continue;
            instructions.Add(line);
            instIndex++;
        }

        // Segunda passe: montar instruções
        var hexOutput = new List<string>();
        for (int i = 0; i < instructions.Count; i++)
        {
            var instLine = instructions[i];
            var hexWord = AssembleInstruction(instLine, i, labelToIndex);
            // format as little-endian bytes
            hexOutput.Add("0x" + ToLittleEndianHex(hexWord));
        }

        return hexOutput;
    }

    // Converte um uint 32-bit para string hex representando os bytes em ordem little-endian
    private static string ToLittleEndianHex(uint value)
    {
        uint b0 = (value & 0x000000FFu);
        uint b1 = (value & 0x0000FF00u) >> 8;
        uint b2 = (value & 0x00FF0000u) >> 16;
        uint b3 = (value & 0xFF000000u) >> 24;
        uint swapped = (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
        return swapped.ToString("X8");
    }

    // Monta instrução única em uint
    private static uint AssembleInstruction(string instLine, int pcIndex, Dictionary<string, int> labels)
    {
        if (string.IsNullOrWhiteSpace(instLine)) throw new InvalidOperationException("Instrução vazia.");

        // Se a linha for 4 bytes hex (ex: "00 00 F4 01") converte diretamente
        if (Regex.IsMatch(instLine, @"^\s*([0-9A-Fa-f]{2}\s+){3}[0-9A-Fa-f]{2}\s*$"))
        {
            var parts = instLine.Split(new[] { ' ', '\t' }, StringSplitOptions.RemoveEmptyEntries);
            if (parts.Length == 4)
            {
                uint b0 = Convert.ToUInt32(parts[0], 16);
                uint b1 = Convert.ToUInt32(parts[1], 16);
                uint b2 = Convert.ToUInt32(parts[2], 16);
                uint b3 = Convert.ToUInt32(parts[3], 16);
                // Combina em palavra big-endian (byte0 is most significant)
                return (b0 << 24) | (b1 << 16) | (b2 << 8) | b3;
            }
        }

        // Tokenizar: separar por espaço e vírgula (preservando parênteses para lw/sw)
        var tokens = Tokenize(instLine);
        if (tokens.Count == 0) throw new InvalidOperationException("Instrução vazia.");

        var op = tokens[0].ToLowerInvariant();
        switch (op)
        {
            // R-type: opcode 0
            case "add":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x20);
            case "addu":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x21);
            case "sub":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x22);
            case "subu":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x23);
            case "and":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x24);
            case "or":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x25);
            case "sll":
                return RTypeShift(0x00, tokens, rdIdx: 1, rtIdx: 2, shamtIdx: 3, funct: 0x00);
            case "srl":
                return RTypeShift(0x00, tokens, rdIdx: 1, rtIdx: 2, shamtIdx: 3, funct: 0x02);
            case "slt":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x2A);
            case "sltu":
                return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x2B);
            case "jr":
                return RTypeJr(tokens, rsIdx: 1);
            case "jalr":
                return JALR(tokens);

            // mult/div
            case "mult":
                return RTypeNoRd(0x00, tokens, rsIdx: 1, rtIdx: 2, funct: 0x18);
            case "multu":
                return RTypeNoRd(0x00, tokens, rsIdx: 1, rtIdx: 2, funct: 0x19);
            case "div":
                return RTypeNoRd(0x00, tokens, rsIdx: 1, rtIdx: 2, funct: 0x1A);
            case "divu":
                return RTypeNoRd(0x00, tokens, rsIdx: 1, rtIdx: 2, funct: 0x1B);
            case "mfhi":
                return RTypeMf(0x00, tokens, rdIdx: 1, funct: 0x10);
            case "mflo":
                return RTypeMf(0x00, tokens, rdIdx: 1, funct: 0x12);
            case "mthi":
                return RTypeMthiMtlo(0x00, tokens, rsIdx: 1, funct: 0x11);
            case "mtlo":
                return RTypeMthiMtlo(0x00, tokens, rsIdx: 1, funct: 0x13);

            // I-type
            case "addiu":
                // support accidental register as third operand: treat as addu rd, rs, rt
                if (tokens.Count > 3 && tokens[3].StartsWith("$"))
                    return RType(0x00, tokens, rdIdx: 1, rsIdx: 2, rtIdx: 3, funct: 0x21);
                return IType(opcode: 0x09, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex);
            case "addi":
                return IType(opcode: 0x08, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex);
            case "andi":
                return IType(opcode: 0x0C, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex, unsignedImmediate: true);
            case "ori":
                return IType(opcode: 0x0D, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex, unsignedImmediate: true);
            case "slti":
                return IType(opcode: 0x0A, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex);
            case "sltiu":
                return IType(opcode: 0x0B, tokens, rtIdx: 1, rsIdx: 2, immIdx: 3, labels, pcIndex);
            case "lw":
                return LoadStore(opcode: 0x23, tokens, rtIdx: 1, addrIdx: 2);
            case "sw":
                return LoadStore(opcode: 0x2B, tokens, rtIdx: 1, addrIdx: 2);
            case "lbu":
                return LoadStore(opcode: 0x24, tokens, rtIdx: 1, addrIdx: 2);
            case "lb":
                return LoadStore(opcode: 0x20, tokens, rtIdx: 1, addrIdx: 2);
            case "lhu":
                return LoadStore(opcode: 0x25, tokens, rtIdx: 1, addrIdx: 2);
            case "lh":
                return LoadStore(opcode: 0x21, tokens, rtIdx: 1, addrIdx: 2);
            case "sb":
                return LoadStore(opcode: 0x28, tokens, rtIdx: 1, addrIdx: 2);
            case "sh":
                return LoadStore(opcode: 0x29, tokens, rtIdx: 1, addrIdx: 2);
            case "beq":
                return Branch(opcode: 0x04, tokens, rsIdx: 1, rtIdx: 2, labelIdx: 3, labels, pcIndex);
            case "bne":
                return Branch(opcode: 0x05, tokens, rsIdx: 1, rtIdx: 2, labelIdx: 3, labels, pcIndex);
            case "beqz":
                // beqz reg, imm -> beq reg, $zero, imm
                return Branch(opcode: 0x04, tokens, rsIdx: 1, rtIdx: -1, labelIdx: 2, labels, pcIndex, zeroRt: true);
            case "bnez":
                // bnez reg, imm -> bne reg, $zero, imm
                return Branch(opcode: 0x05, tokens, rsIdx: 1, rtIdx: -1, labelIdx: 2, labels, pcIndex, zeroRt: true);

            case "bgez":
                return BranchRegImm(tokens, rsIdx: 1, rtValue: 0x01, labels: labels, pcIndex: pcIndex);
            case "bltz":
                return BranchRegImm(tokens, rsIdx: 1, rtValue: 0x00, labels: labels, pcIndex: pcIndex);
            case "bgezal":
                return BranchRegImm(tokens, rsIdx: 1, rtValue: 0x11, labels: labels, pcIndex: pcIndex);
            case "bltzal":
                return BranchRegImm(tokens, rsIdx: 1, rtValue: 0x10, labels: labels, pcIndex: pcIndex);
            case "blez":
                return BranchSimpleOpcode(tokens, opcode: 0x06, rsIdx: 1, labelIdx: 2, labels: labels, pcIndex: pcIndex);
            case "bgtz":
                return BranchSimpleOpcode(tokens, opcode: 0x07, rsIdx: 1, labelIdx: 2, labels: labels, pcIndex: pcIndex);

            // J-type
            case "j":
                return JType(opcode: 0x02, tokens, labelIdx: 1, labels);
            case "jal":
                return JType(opcode: 0x03, tokens, labelIdx: 1, labels);
            case "lui":
                return LUI(tokens, rtIdx: 1, immIdx: 2);
            case "b":
                return PseudoB(tokens, labels, pcIndex);
            case "bal":
                return PseudoBAL(tokens, labels, pcIndex);

            // system
            case "syscall":
                return (0u << 26) | 0u | 0u | 0u | 0u | 0x0Cu;
            case "break":
                return (0u << 26) | 0u | 0u | 0u | 0u | 0x0Du;

            // nop -> sll $zero, $zero, 0 (codificação 0)
            case "nop":
                return 0u;

            default:
                throw new InvalidOperationException($"Instrução não suportada: {op}");
        }
    }

    // Tokeniza linha: separa por espaço e vírgula, preservando 'offset(base)' como um token
    private static List<string> Tokenize(string line)
    {
        line = line ?? string.Empty;
        // Captura opcode (primeiro token não-space) e o resto como string de operandos
        var m = Regex.Match(line, @"^\s*(\S+)\s*(.*)$");
        if (!m.Success)
            return new List<string>();

        var opcode = m.Groups[1].Value;
        var operandsPart = m.Groups[2].Value.Trim();

        var tokens = new List<string> { opcode };

        if (string.IsNullOrEmpty(operandsPart))
            return tokens;

        // Divide operandos por vírgula, removendo espaços ao redor
        var ops = Regex.Split(operandsPart, @"\s*,\s*")
                       .Where(s => !string.IsNullOrWhiteSpace(s))
                       .Select(s => SanitizeToken(s))
                       .ToList();

        tokens.AddRange(ops);
        return tokens;
    }

    private static string SanitizeToken(string s)
    {
        s = s.Trim();
        // remove annotations like "[0x1234]" that some disassemblers append
        var idx = s.IndexOf('[');
        if (idx >= 0) s = s.Substring(0, idx).Trim();
        // normalize double 0x => 0x0x -> 0x
        s = Regex.Replace(s, @"^0x0x", "0x", RegexOptions.IgnoreCase);
        // remove trailing commas/brackets
        s = s.Trim().TrimEnd(',').Trim();
        return s;
    }

    // R-type genérico (rd, rs, rt)
    private static uint RType(uint opcode, List<string> tokens, int rdIdx, int rsIdx, int rtIdx, uint funct)
    {
        if (tokens.Count <= Math.Max(rdIdx, Math.Max(rsIdx, rtIdx)))
            throw new InvalidOperationException("Formato R-type inválido.");

        var rd = ParseRegister(tokens[rdIdx]);
        var rs = ParseRegister(tokens[rsIdx]);
        var rt = ParseRegister(tokens[rtIdx]);
        uint shamt = 0;
        return (opcode << 26) | ((uint)rs << 21) | ((uint)rt << 16) | ((uint)rd << 11) | (shamt << 6) | funct;
    }

    // R-type para shift (rd, rt, shamt)
    private static uint RTypeShift(uint opcode, List<string> tokens, int rdIdx, int rtIdx, int shamtIdx, uint funct)
    {
        if (tokens.Count <= Math.Max(rdIdx, Math.Max(rtIdx, shamtIdx)))
            throw new InvalidOperationException("Formato shift inválido.");

        var rd = ParseRegister(tokens[rdIdx]);
        var rt = ParseRegister(tokens[rtIdx]);
        var shamt = ParseImmediateNumeric(tokens[shamtIdx]);
        return (opcode << 26) | (0u << 21) | ((uint)rt << 16) | ((uint)rd << 11) | ((uint)(shamt & 0x1F) << 6) | funct;
    }

    // R-type para jr (opcode 0, funct 8) usa rs
    private static uint RTypeJr(List<string> tokens, int rsIdx)
    {
        if (tokens.Count <= rsIdx) throw new InvalidOperationException("Formato jr inválido.");
        var rs = ParseRegister(tokens[rsIdx]);
        return (0u << 26) | ((uint)rs << 21) | (0u << 16) | (0u << 11) | (0u << 6) | 0x08u;
    }

    // JALR - aceita 'jalr rd, rs' ou 'jalr rs' (rd = $ra)
    private static uint JALR(List<string> tokens)
    {
        if (tokens.Count == 2)
        {
            // jalr rs -> rd = $ra (31), rs = tokens[1]
            var rs = ParseRegister(tokens[1]);
            var rd = 31;
            return (0u << 26) | ((uint)rs << 21) | (0u << 16) | ((uint)rd << 11) | (0u << 6) | 0x09u;
        }
        if (tokens.Count >= 3)
        {
            var rd = ParseRegister(tokens[1]);
            var rs = ParseRegister(tokens[2]);
            return (0u << 26) | ((uint)rs << 21) | (0u << 16) | ((uint)rd << 11) | (0u << 6) | 0x09u;
        }
        throw new InvalidOperationException("Formato jalr inválido.");
    }

    // R-type sem rd (para mult/div) usa rs, rt
    private static uint RTypeNoRd(uint opcode, List<string> tokens, int rsIdx, int rtIdx, uint funct)
    {
        if (tokens.Count <= Math.Max(rsIdx, rtIdx)) throw new InvalidOperationException("Formato R-type inválido.");
        var rs = ParseRegister(tokens[rsIdx]);
        var rt = ParseRegister(tokens[rtIdx]);
        return (opcode << 26) | ((uint)rs << 21) | ((uint)rt << 16) | (0u << 11) | (0u << 6) | funct;
    }

    // MFHI/MFLO (rd)
    private static uint RTypeMf(uint opcode, List<string> tokens, int rdIdx, uint funct)
    {
        if (tokens.Count <= rdIdx) throw new InvalidOperationException("Formato mf inválido.");
        var rd = ParseRegister(tokens[rdIdx]);
        return (opcode << 26) | (0u << 21) | (0u << 16) | ((uint)rd << 11) | (0u << 6) | funct;
    }

    // MTHI/MTLO (rs)
    private static uint RTypeMthiMtlo(uint opcode, List<string> tokens, int rsIdx, uint funct)
    {
        if (tokens.Count <= rsIdx) throw new InvalidOperationException("Formato mt inválido.");
        var rs = ParseRegister(tokens[rsIdx]);
        return (opcode << 26) | ((uint)rs << 21) | (0u << 16) | (0u << 11) | (0u << 6) | funct;
    }

    // I-type genérico (opcode, rt, rs, immediate)
    private static uint IType(uint opcode, List<string> tokens, int rtIdx, int rsIdx, int immIdx, Dictionary<string, int> labels, int pcIndex, bool unsignedImmediate = false)
    {
        if (tokens.Count <= Math.Max(rtIdx, Math.Max(rsIdx, immIdx)))
            throw new InvalidOperationException("Formato I-type inválido.");

        var rt = ParseRegister(tokens[rtIdx]);
        var rs = ParseRegister(tokens[rsIdx]);
        var immToken = tokens[immIdx];
        int imm = ParseImmediateWithLabels(immToken, labels, pcIndex);
        uint uimm = unsignedImmediate ? (uint)imm & 0xFFFFu : (uint)(imm & 0xFFFF);
        return (opcode << 26) | ((uint)rs << 21) | ((uint)rt << 16) | uimm;
    }

    // LUI (opcode 0x0F)
    private static uint LUI(List<string> tokens, int rtIdx, int immIdx)
    {
        if (tokens.Count <= Math.Max(rtIdx, immIdx))
            throw new InvalidOperationException("Formato lui inválido.");
        var rt = ParseRegister(tokens[rtIdx]);
        var imm = ParseImmediateNumeric(tokens[immIdx]);
        uint uimm = (uint)(imm & 0xFFFF);
        return (0x0Fu << 26) | ((uint)rt << 16) | uimm;
    }

    // load/store tipo: lw rt, offset(base)
    private static uint LoadStore(uint opcode, List<string> tokens, int rtIdx, int addrIdx)
    {
        if (tokens.Count <= Math.Max(rtIdx, addrIdx))
            throw new InvalidOperationException("Formato load/store inválido.");

        var rt = ParseRegister(tokens[rtIdx]);
        var addr = tokens[addrIdx];

        // formato: OFFSET(BASE) ou apenas label (não comum)
        var m = Regex.Match(addr, @"^\s*([-+]?0x[0-9A-Fa-f]+|[-+]?\w+|[-+]?\d+)\s*\(\s*([^)]+)\s*\)\s*$");
        if (!m.Success)
            throw new InvalidOperationException($"Endereço inválido: {addr}. Use offset(base).");

        var offsetToken = m.Groups[1].Value;
        var baseRegToken = m.Groups[2].Value;

        var baseReg = ParseRegister(baseRegToken);
        int offset = ParseImmediateNumeric(offsetToken);
        uint uoffset = (uint)(offset & 0xFFFF);

        return (opcode << 26) | ((uint)baseReg << 21) | ((uint)rt << 16) | uoffset;
    }

    // Branch (beq/bne): beq rs, rt, label
    // zeroRt: quando true, usamos $zero como rt (para beqz/bnez)
    private static uint Branch(uint opcode, List<string> tokens, int rsIdx, int rtIdx, int labelIdx, Dictionary<string, int> labels, int pcIndex, bool zeroRt = false)
    {
        // rtIdx may be -1 when zeroRt==true
        int maxIdx = Math.Max(rsIdx, Math.Max(rtIdx, labelIdx));
        if (tokens.Count <= maxIdx) throw new InvalidOperationException("Formato branch inválido.");

        var rs = ParseRegister(tokens[rsIdx]);
        int rt;
        if (zeroRt) rt = 0;
        else rt = ParseRegister(tokens[rtIdx]);

        var labelToken = tokens[labelIdx];

        // Se for um rótulo
        if (labels.TryGetValue(labelToken, out var labelIndex))
        {
            int imm = labelIndex - (pcIndex + 1);
            uint uimm = (uint)((ushort)imm);
            return (opcode << 26) | ((uint)rs << 21) | ((uint)rt << 16) | uimm;
        }

        // tenta interpretar como número (pode ser decimal negativo ou hex)
        try
        {
            int raw = ParseImmediateNumeric(labelToken);
            // se for múltiplo de 4, provavelmente é offset em bytes -> converte para palavras
            int imm;
            if (raw % 4 == 0)
                imm = raw / 4;
            else
                imm = raw; // caso contrário assume que já é offset em instruções

            uint uimm = (uint)((ushort)imm);
            return (opcode << 26) | ((uint)rs << 21) | ((uint)rt << 16) | uimm;
        }
        catch
        {
            throw new InvalidOperationException($"Rótulo não encontrado: {labelToken}");
        }
    }

    // Branch REGIMM (opcode 1) para bgez/bltz/bgezal/bltzal
    private static uint BranchRegImm(List<string> tokens, int rsIdx, int rtValue, Dictionary<string, int> labels, int pcIndex)
    {
        if (tokens.Count <= rsIdx) throw new InvalidOperationException("Formato branch-regimm inválido.");
        var rs = ParseRegister(tokens[rsIdx]);
        // operand label usually next token
        if (tokens.Count <= rsIdx + 1) throw new InvalidOperationException("Formato branch-regimm inválido.");
        var labelToken = tokens[rsIdx + 1];

        // label or immediate
        int imm;
        if (labels.TryGetValue(labelToken, out var labelIndex))
        {
            imm = labelIndex - (pcIndex + 1);
        }
        else
        {
            imm = ParseImmediateNumeric(labelToken);
            if (imm % 4 == 0) imm /= 4;
        }

        uint uimm = (uint)((ushort)imm);
        return (0x01u << 26) | ((uint)rs << 21) | ((uint)rtValue << 16) | uimm;
    }

    // Branch simple for blez/bgtz opcodes 6 and 7: opcode, rs, imm
    private static uint BranchSimpleOpcode(List<string> tokens, uint opcode, int rsIdx, int labelIdx, Dictionary<string, int> labels, int pcIndex)
    {
        if (tokens.Count <= Math.Max(rsIdx, labelIdx)) throw new InvalidOperationException("Formato branch inválido.");
        var rs = ParseRegister(tokens[rsIdx]);
        var labelToken = tokens[labelIdx];

        int imm;
        if (labels.TryGetValue(labelToken, out var labelIndex)) imm = labelIndex - (pcIndex + 1);
        else
        {
            imm = ParseImmediateNumeric(labelToken);
            if (imm % 4 == 0) imm /= 4;
        }

        uint uimm = (uint)((ushort)imm);
        return (opcode << 26) | ((uint)rs << 21) | (0u << 16) | uimm;
    }

    // J-type aceita rótulos ou endereço numérico (0x...)
    private static uint JType(uint opcode, List<string> tokens, int labelIdx, Dictionary<string, int> labels)
    {
        if (tokens.Count <= labelIdx)
            throw new InvalidOperationException("Formato J-type inválido.");

        var label = tokens[labelIdx];
        // primeiro tenta rótulo
        if (labels.TryGetValue(label, out var labelIndex))
        {
            uint address26 = (uint)(labelIndex & 0x03FFFFFF);
            return (opcode << 26) | address26;
        }

        // tenta interpretar como endereço numérico
        try
        {
            int addr = ParseImmediateNumeric(label);
            uint address26 = ((uint)addr >> 2) & 0x03FFFFFFu;
            return (opcode << 26) | address26;
        }
        catch
        {
            throw new InvalidOperationException($"Rótulo não encontrado: {label}");
        }
    }

    // Pseudo 'b' -> beq $zero,$zero,label
    private static uint PseudoB(List<string> tokens, Dictionary<string, int> labels, int pcIndex)
    {
        if (tokens.Count <= 1) throw new InvalidOperationException("Formato b inválido.");
        var label = tokens[1];
        return Branch(opcode: 0x04, tokens: new List<string> { "beq", "$zero", "$zero", label }, rsIdx: 1, rtIdx: 2, labelIdx: 3, labels: labels, pcIndex: pcIndex);
    }

    // Pseudo 'bal' -> REGIMM with rt=16 (BLTZAL/BGEZAL convention) using $zero as rs
    private static uint PseudoBAL(List<string> tokens, Dictionary<string, int> labels, int pcIndex)
    {
        if (tokens.Count <= 1) throw new InvalidOperationException("Formato bal inválido.");
        var label = tokens[1];
        // use rs=$zero, rt=16 (BLTZAL) but typically bal encodes with rt=16 and rs=0
        return BranchRegImm(new List<string> { "bal", "$zero", label }, rsIdx: 1, rtValue: 0x10, labels: labels, pcIndex: pcIndex);
    }

    // Parse register name como $t0, $s1, $zero, etc.
    private static int ParseRegister(string token)
    {
        token = token.Trim();
        if (!token.StartsWith("$")) throw new InvalidOperationException($"Registrador inválido: {token}");

        var reg = token.Substring(1);
        // Mapas comuns
        var map = new Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
        {
            {"zero",0}, {"at",1},
            {"v0",2},{"v1",3},
            {"a0",4},{"a1",5},{"a2",6},{"a3",7},
            {"t0",8},{"t1",9},{"t2",10},{"t3",11},{"t4",12},{"t5",13},{"t6",14},{"t7",15},
            {"s0",16},{"s1",17},{"s2",18},{"s3",19},{"s4",20},{"s5",21},{"s6",22},{"s7",23},
            {"t8",24},{"t9",25},
            {"k0",26},{"k1",27},
            {"gp",28},{"sp",29},{"fp",30},{"ra",31}
        };

        if (map.TryGetValue(reg, out var idx)) return idx;

        // permitir números diretos como $5
        if (int.TryParse(reg, NumberStyles.Integer, CultureInfo.InvariantCulture, out var n) && n >= 0 && n <= 31) return n;

        throw new InvalidOperationException($"Registrador desconhecido: ${reg}");
    }

    // Parse imediato numérico (decimal ou 0x...). Também limpa anotações como "123 [0x7B64]".
    private static int ParseImmediateNumeric(string token)
    {
        token = token.Trim();
        // remove anotações entre colchetes
        var idx = token.IndexOf('[');
        if (idx >= 0) token = token.Substring(0, idx).Trim();

        // normaliza 0x0x para 0x
        token = Regex.Replace(token, @"^0x0x", "0x", RegexOptions.IgnoreCase);

        if (token.StartsWith("0x", StringComparison.OrdinalIgnoreCase))
        {
            return Convert.ToInt32(token.Substring(2), 16);
        }

        if (int.TryParse(token, NumberStyles.AllowLeadingSign | NumberStyles.Integer, CultureInfo.InvariantCulture, out var v))
            return v;

        // caso não seja número -> error
        throw new InvalidOperationException($"Imediato inválido: {token}");
    }

    // Parse imediato que pode ser número ou rótulo (para branches usamos labels)
    private static int ParseImmediateWithLabels(string token, Dictionary<string, int> labels, int pcIndex)
    {
        token = token.Trim();
        // número?
        try
        {
            return ParseImmediateNumeric(token);
        }
        catch
        {
            // se não for número, deve ser rótulo
            if (!labels.TryGetValue(token, out var labelIndex))
                throw new InvalidOperationException($"Rótulo/Imediato inválido: {token}");
            // para imedia-to imediato de I-type sem ser branch, interpretamos como índice de palavra
            return labelIndex;
        }
    }
}