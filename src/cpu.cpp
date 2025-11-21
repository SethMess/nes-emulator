#include "cpu.h"
#include "bus.h"
#include <cstdint>

CPU::CPU() {

  // The table is one big initialiser list of initialiser lists...
  using a = CPU;
  lookup = {
      {"BRK", &a::BRK, &a::IMM, 7}, {"ORA", &a::ORA, &a::IZX, 6},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 3}, {"ORA", &a::ORA, &a::ZP0, 3},
      {"ASL", &a::ASL, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"PHP", &a::PHP, &a::IMP, 3}, {"ORA", &a::ORA, &a::IMM, 2},
      {"ASL", &a::ASL, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ABS, 4},
      {"ASL", &a::ASL, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BPL", &a::BPL, &a::REL, 2}, {"ORA", &a::ORA, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ZPX, 4},
      {"ASL", &a::ASL, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"CLC", &a::CLC, &a::IMP, 2}, {"ORA", &a::ORA, &a::ABY, 4},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"ORA", &a::ORA, &a::ABX, 4},
      {"ASL", &a::ASL, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
      {"JSR", &a::JSR, &a::ABS, 6}, {"AND", &a::AND, &a::IZX, 6},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"BIT", &a::BIT, &a::ZP0, 3}, {"AND", &a::AND, &a::ZP0, 3},
      {"ROL", &a::ROL, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"PLP", &a::PLP, &a::IMP, 4}, {"AND", &a::AND, &a::IMM, 2},
      {"ROL", &a::ROL, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"BIT", &a::BIT, &a::ABS, 4}, {"AND", &a::AND, &a::ABS, 4},
      {"ROL", &a::ROL, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BMI", &a::BMI, &a::REL, 2}, {"AND", &a::AND, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"AND", &a::AND, &a::ZPX, 4},
      {"ROL", &a::ROL, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"SEC", &a::SEC, &a::IMP, 2}, {"AND", &a::AND, &a::ABY, 4},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"AND", &a::AND, &a::ABX, 4},
      {"ROL", &a::ROL, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
      {"RTI", &a::RTI, &a::IMP, 6}, {"EOR", &a::EOR, &a::IZX, 6},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 3}, {"EOR", &a::EOR, &a::ZP0, 3},
      {"LSR", &a::LSR, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"PHA", &a::PHA, &a::IMP, 3}, {"EOR", &a::EOR, &a::IMM, 2},
      {"LSR", &a::LSR, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"JMP", &a::JMP, &a::ABS, 3}, {"EOR", &a::EOR, &a::ABS, 4},
      {"LSR", &a::LSR, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BVC", &a::BVC, &a::REL, 2}, {"EOR", &a::EOR, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"EOR", &a::EOR, &a::ZPX, 4},
      {"LSR", &a::LSR, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"CLI", &a::CLI, &a::IMP, 2}, {"EOR", &a::EOR, &a::ABY, 4},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"EOR", &a::EOR, &a::ABX, 4},
      {"LSR", &a::LSR, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
      {"RTS", &a::RTS, &a::IMP, 6}, {"ADC", &a::ADC, &a::IZX, 6},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 3}, {"ADC", &a::ADC, &a::ZP0, 3},
      {"ROR", &a::ROR, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"PLA", &a::PLA, &a::IMP, 4}, {"ADC", &a::ADC, &a::IMM, 2},
      {"ROR", &a::ROR, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"JMP", &a::JMP, &a::IND, 5}, {"ADC", &a::ADC, &a::ABS, 4},
      {"ROR", &a::ROR, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BVS", &a::BVS, &a::REL, 2}, {"ADC", &a::ADC, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"ADC", &a::ADC, &a::ZPX, 4},
      {"ROR", &a::ROR, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"SEI", &a::SEI, &a::IMP, 2}, {"ADC", &a::ADC, &a::ABY, 4},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"ADC", &a::ADC, &a::ABX, 4},
      {"ROR", &a::ROR, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 2}, {"STA", &a::STA, &a::IZX, 6},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 6},
      {"STY", &a::STY, &a::ZP0, 3}, {"STA", &a::STA, &a::ZP0, 3},
      {"STX", &a::STX, &a::ZP0, 3}, {"???", &a::XXX, &a::IMP, 3},
      {"DEY", &a::DEY, &a::IMP, 2}, {"???", &a::NOP, &a::IMP, 2},
      {"TXA", &a::TXA, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"STY", &a::STY, &a::ABS, 4}, {"STA", &a::STA, &a::ABS, 4},
      {"STX", &a::STX, &a::ABS, 4}, {"???", &a::XXX, &a::IMP, 4},
      {"BCC", &a::BCC, &a::REL, 2}, {"STA", &a::STA, &a::IZY, 6},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 6},
      {"STY", &a::STY, &a::ZPX, 4}, {"STA", &a::STA, &a::ZPX, 4},
      {"STX", &a::STX, &a::ZPY, 4}, {"???", &a::XXX, &a::IMP, 4},
      {"TYA", &a::TYA, &a::IMP, 2}, {"STA", &a::STA, &a::ABY, 5},
      {"TXS", &a::TXS, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 5},
      {"???", &a::NOP, &a::IMP, 5}, {"STA", &a::STA, &a::ABX, 5},
      {"???", &a::XXX, &a::IMP, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"LDY", &a::LDY, &a::IMM, 2}, {"LDA", &a::LDA, &a::IZX, 6},
      {"LDX", &a::LDX, &a::IMM, 2}, {"???", &a::XXX, &a::IMP, 6},
      {"LDY", &a::LDY, &a::ZP0, 3}, {"LDA", &a::LDA, &a::ZP0, 3},
      {"LDX", &a::LDX, &a::ZP0, 3}, {"???", &a::XXX, &a::IMP, 3},
      {"TAY", &a::TAY, &a::IMP, 2}, {"LDA", &a::LDA, &a::IMM, 2},
      {"TAX", &a::TAX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"LDY", &a::LDY, &a::ABS, 4}, {"LDA", &a::LDA, &a::ABS, 4},
      {"LDX", &a::LDX, &a::ABS, 4}, {"???", &a::XXX, &a::IMP, 4},
      {"BCS", &a::BCS, &a::REL, 2}, {"LDA", &a::LDA, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 5},
      {"LDY", &a::LDY, &a::ZPX, 4}, {"LDA", &a::LDA, &a::ZPX, 4},
      {"LDX", &a::LDX, &a::ZPY, 4}, {"???", &a::XXX, &a::IMP, 4},
      {"CLV", &a::CLV, &a::IMP, 2}, {"LDA", &a::LDA, &a::ABY, 4},
      {"TSX", &a::TSX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 4},
      {"LDY", &a::LDY, &a::ABX, 4}, {"LDA", &a::LDA, &a::ABX, 4},
      {"LDX", &a::LDX, &a::ABY, 4}, {"???", &a::XXX, &a::IMP, 4},
      {"CPY", &a::CPY, &a::IMM, 2}, {"CMP", &a::CMP, &a::IZX, 6},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"CPY", &a::CPY, &a::ZP0, 3}, {"CMP", &a::CMP, &a::ZP0, 3},
      {"DEC", &a::DEC, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"INY", &a::INY, &a::IMP, 2}, {"CMP", &a::CMP, &a::IMM, 2},
      {"DEX", &a::DEX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 2},
      {"CPY", &a::CPY, &a::ABS, 4}, {"CMP", &a::CMP, &a::ABS, 4},
      {"DEC", &a::DEC, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BNE", &a::BNE, &a::REL, 2}, {"CMP", &a::CMP, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"CMP", &a::CMP, &a::ZPX, 4},
      {"DEC", &a::DEC, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"CLD", &a::CLD, &a::IMP, 2}, {"CMP", &a::CMP, &a::ABY, 4},
      {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"CMP", &a::CMP, &a::ABX, 4},
      {"DEC", &a::DEC, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
      {"CPX", &a::CPX, &a::IMM, 2}, {"SBC", &a::SBC, &a::IZX, 6},
      {"???", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"CPX", &a::CPX, &a::ZP0, 3}, {"SBC", &a::SBC, &a::ZP0, 3},
      {"INC", &a::INC, &a::ZP0, 5}, {"???", &a::XXX, &a::IMP, 5},
      {"INX", &a::INX, &a::IMP, 2}, {"SBC", &a::SBC, &a::IMM, 2},
      {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::SBC, &a::IMP, 2},
      {"CPX", &a::CPX, &a::ABS, 4}, {"SBC", &a::SBC, &a::ABS, 4},
      {"INC", &a::INC, &a::ABS, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"BEQ", &a::BEQ, &a::REL, 2}, {"SBC", &a::SBC, &a::IZY, 5},
      {"???", &a::XXX, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 8},
      {"???", &a::NOP, &a::IMP, 4}, {"SBC", &a::SBC, &a::ZPX, 4},
      {"INC", &a::INC, &a::ZPX, 6}, {"???", &a::XXX, &a::IMP, 6},
      {"SED", &a::SED, &a::IMP, 2}, {"SBC", &a::SBC, &a::ABY, 4},
      {"NOP", &a::NOP, &a::IMP, 2}, {"???", &a::XXX, &a::IMP, 7},
      {"???", &a::NOP, &a::IMP, 4}, {"SBC", &a::SBC, &a::ABX, 4},
      {"INC", &a::INC, &a::ABX, 7}, {"???", &a::XXX, &a::IMP, 7},
  };
}

CPU::~CPU() {}

uint8_t CPU::read(uint16_t a) { return bus->read(a, false); }

void CPU::write(uint16_t a, uint8_t d) { bus->write(a, d); }

void CPU::clock() {
  if (cycles == 0) {
    opcode = read(pc);

    // get starting cycles
    cycles = lookup[opcode].cycles;

    uint8_t additional_cycle1 = (this->*lookup[opcode].addrmode)();

    uint8_t additional_cycle2 = (this->*lookup[opcode].operate)();

    if (additional_cycle1 &&
        additional_cycle2) { // if both return 1 add extra cycle
      cycles++;
    }
  }
  cycles--;
}

// Addressing Modes (12)

uint8_t CPU::IMP() {
  fetched = a; // might operate on the acumulator register
  return 0;
}

uint8_t CPU::IMM() {
  addr_abs = pc++; // next memory space
  return 0;
}

uint8_t CPU::ZP0() {
  addr_abs = read(pc);
  pc++;
  // sets the first byte to be zero since it zero page Addressing
  addr_abs &= 0x00FF;
  return 0;
}

uint8_t CPU::ZPX() {
  addr_abs = (read(pc) + x);
  pc++;
  addr_abs &= 0x00FF;
  return 0;
}

uint8_t CPU::ZPY() {
  addr_abs = (read(pc) + y);
  pc++;
  addr_abs &= 0x00FF;
  return 0;
}

// understand this one more
uint8_t CPU::REL() {
  addr_rel = read(pc);
  pc++;
  if (addr_rel & 0x80) {
    addr_rel |= 0xFF00; // how does this work? does it overflow if its
                        // negative???
  }
  return 0;
}

// this should be correct but double check
uint8_t CPU::ABS() {
  addr_abs = read(pc) << 8;
  pc++;
  addr_abs |= read(pc);
  pc++;
  return 0;
}

uint8_t CPU::ABX() {
  uint16_t lo = read(pc);
  pc++;
  uint16_t hi = read(pc);
  pc++;

  addr_abs = (hi << 8) | lo;
  addr_abs += x;
  if ((0xFF00 & addr_abs) !=
      (hi << 8)) { // if the high byte changed pages take an extra clock cycle
    return 1;
  } else {
    return 0;
  }
}

uint8_t CPU::ABY() {
  uint16_t lo = read(pc);
  pc++;
  uint16_t hi = read(pc);
  pc++;

  addr_abs = (hi << 8) | lo;
  addr_abs += y;
  if ((0xFF00 & addr_abs) !=
      (hi << 8)) { // if the high byte changed pages take an extra clock cycle
    return 1;
  } else {
    return 0;
  }
}

uint8_t CPU::IND() {
  uint16_t lo_ptr = read(pc);
  pc++;
  uint16_t hi_ptr = read(pc);
  pc++;

  uint16_t ptr = (hi_ptr << 8) | lo_ptr;
  if (lo_ptr == 0x00FF) {
    addr_abs = (read(ptr & 0xFF00) << 8) |
               read(ptr + 0); // if it were to go to a new page loop instead
  } else {
    addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
  }

  return 0;
}

uint8_t CPU::IZX() {
  uint16_t addr = read(pc);
  pc++;

  uint16_t lo = read((uint16_t)(addr + (uint16_t)x) & 0x00FF);
  uint16_t hi = read((uint16_t)(addr + (uint16_t)x + 1) & 0x00FF);

  addr_abs = (hi << 8) | lo;
  return 0;
}

uint8_t CPU::IZY() {
  uint16_t addr = read(pc);
  pc++;

  uint16_t lo = read(addr & 0x00FF);
  uint16_t hi = read((addr + 1) & 0x00FF);

  addr_abs = (hi << 8) | lo;
  addr_abs += y;

  if ((addr_abs & 0xFF00) != (hi << 8)) {
    return 1;
  } else {
    return 0;
  }
}

// instruction (56)

uint8_t CPU::fetch() {
  if (lookup[opcode].addrmode != &CPU::IMP) {
    fetched = read(addr_abs);
  }
  return fetched;
}

uint8_t CPU::ADC() {
  fetch();
  temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);
  SetFlag(C, temp > 255);
  SetFlag(Z, (temp & 0x00FF) == 0);
  SetFlag(N, temp & 0x08);
  SetFlag(
      V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) &
             0x0080);
  a = temp &
      0x00FF; // sets the acumulator to the low byte of calculated variable
  return 1;
}

uint8_t CPU::SBC() {
  fetch();
  uint16_t inverted_value = ((uint16_t)fetched) ^ 0x00FF;

  temp = (uint16_t)a + inverted_value + (uint16_t)GetFlag(C);
  SetFlag(C, temp > 255); // or temp & 0xFF00
  SetFlag(Z, (temp & 0x00FF) == 0);
  SetFlag(N, temp & 0x08);
  SetFlag(
      V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) &
             0x0080);
  a = temp &
      0x00FF; // sets the acumulator to the low byte of calculated variable
  return 1;
}

uint8_t CPU::AND() {
  a = a & fetched;
  SetFlag(Z, a == 0x00); // does this \/
  // status &= (a == 0x00);
  SetFlag(N, a & 0x80); // does this \/
  // status &= (a & 0x80);
  return 1;
}

uint8_t CPU::ASL() { return 1; }

// Branch instructions ****************
// Branch if carry set
uint8_t CPU::BCS() {
  if (GetFlag(C) == 1) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if carry clear
uint8_t CPU::BCC() {
  if (GetFlag(C) == 0) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if zero bit is set
uint8_t CPU::BEQ() {
  if (GetFlag(Z) == 1) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if zero bit is not set
uint8_t CPU::BNE() {
  if (GetFlag(Z) == 0) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if the negative flag is clear
uint8_t CPU::BPL() {
  if (GetFlag(N) == 0) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if the negative flag is set
uint8_t CPU::BMI() {
  if (GetFlag(N) == 1) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if the overflow is clear
uint8_t CPU::BVC() {
  if (GetFlag(V) == 0) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Branch if the overflow is set
uint8_t CPU::BVS() {
  if (GetFlag(V) == 1) {
    cycles++;
    addr_abs = pc + addr_rel;

    if ((addr_abs & 0xFF00) != (pc & 0xFF00)) {
      cycles++;
    }
    pc = addr_abs;
  }
  return 0;
}

// Flag instructions that set and clear flags

// Clear carry bit
uint8_t CPU::CLC() {
  SetFlag(C, false);
  return 0;
}

// Set carry bit
uint8_t CPU::SEC() {
  SetFlag(C, true);
  return 0;
}

// Clear interupt bit
uint8_t CPU::CLI() {
  SetFlag(I, false);
  return 0;
}

// Set interupt bit
uint8_t CPU::SEI() {
  SetFlag(I, true);
  return 0;
}

// Clear decimal bit
uint8_t CPU::CLD() {
  SetFlag(D, false);
  return 0;
}

// Set decimal bit
uint8_t CPU::SED() {
  SetFlag(D, true);
  return 0;
}

// Clear overflow bit, no corresponding set overflow its a hardware thing
uint8_t CPU::CLV() {
  SetFlag(V, false);
  return 0;
}

// Note: the stack starts at address 0x0100
// Writes the value of the acumulator to the stack
uint8_t CPU::PHA() {
  write(0x0100 + stkp, a);
  stkp--;
  return 0;
}

// Reads the value to the acumulator from the stack
uint8_t CPU::PLA() {
  stkp++;
  a = read(0x0100 + stkp);
  SetFlag(Z, a == 0x00);
  SetFlag(N, a & 0x80);
  return 0;
}

void CPU::reset() {
  a = 0;
  x = 0;
  y = 0;
  stkp = 0xFD;
  status = 0x00 | U;

  addr_abs = 0xFFFC;
  uint16_t lo = read(addr_abs + 0);
  uint16_t hi = read(addr_abs + 1);

  pc = (hi << 8) | lo;

  addr_rel = 0x0000;
  addr_abs = 0x0000;
  fetched = 0x00;

  cycles = 8;
}

void CPU::irq() {
  if (GetFlag(I) == 0) {
    write(0x0100 + stkp, (pc >> 8) & 0x00FF); // High byte
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF); // Low byte
    stkp--;

    SetFlag(B, 0);
    SetFlag(U, 1);
    SetFlag(I, 1);
    write(0x0100 + stkp, status);
    stkp--;

    addr_abs = 0xFFFE; // fixed know address set by programmer
    uint16_t lo = read(addr_abs + 0);
    uint16_t hi = read(addr_abs + 1);
    pc = (hi << 8) | lo;

    cycles = 7;
  }
}

void CPU::nmi() {
  write(0x0100 + stkp, (pc >> 8) & 0x00FF); // High byte
  stkp--;
  write(0x0100 + stkp, pc & 0x00FF); // Low byte
  stkp--;

  SetFlag(B, 0);
  SetFlag(U, 1);
  SetFlag(I, 1);
  write(0x0100 + stkp, status);
  stkp--;

  addr_abs = 0xFFFA; // fixed know address set by programmer
  uint16_t lo = read(addr_abs + 0);
  uint16_t hi = read(addr_abs + 1);
  pc = (hi << 8) | lo;

  cycles = 8;
}

uint8_t CPU::RTI() {
  stkp++;
  status = read(0x0100 + stkp);
  status &= ~B;
  status &= ~U;
  SetFlag(I, 0);

  stkp++;
  uint16_t lo = read(0x0100 + stkp);
  stkp++;
  uint16_t hi = read(0x0100 + stkp);
  pc = (hi << 8) | lo;
  return 0;
}
