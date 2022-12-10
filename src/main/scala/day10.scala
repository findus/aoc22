object day10 extends App {

  case class Instruction(name: String, data: Option[Int], cycles: Int)
  case class CPU(cycle: Int, cyclesInCurrentInstruction: Int, instructionQueue: List[Instruction], registers: Map[String, Int], signalStrength: Int) {
    def updateRegisterValue(instruction: Instruction): Map[String, Int] = {
      instruction match {
        case Instruction("addx",data,_) => registers.map(entry => if (entry._1.equals("X")) (entry._1, entry._2 + data.get) else entry)
        case _ => registers
      }
    }
  }

  def genInstruction(input: String) = {
    val inp = input.split(" ")
    val instruction = inp.head
    val data = inp.lastOption
    instruction match {
      case "addx" => Instruction(instruction, data.map(Integer.parseInt), 2)
      case "noop" => Instruction(instruction, None, 1)
    }
  }

  def calcSignalStrength(cpu: CPU, input: Int) = {
    input match {
      case 20 | 60 | 100 | 140 | 180 | 220 => {
        val e = input * cpu.registers("X")
        cpu.signalStrength + e
      }
      case _ => cpu.signalStrength
    }
  }

  def draw(cycle: Int, registerValue: Int) = {
    val currentPosInLine = ((cycle - 1) % 40) +1
    val spritePosition = ((registerValue to registerValue + 2))
    if (spritePosition.contains(currentPosInLine)) {
      print("#")
    } else {
      print(".")
    }
    cycle match {
      case 40 | 80 | 120 | 160 | 200 | 240 => println("")
      case _ =>
    }
  }

  io.load("day10") { lines =>
    val cpu = CPU(0, 0, lines.map(genInstruction), List(("X", 1)).toMap, 0)
    val cpuState = (1 to 240).foldLeft(cpu)((prev, cycle) => {
      draw(cycle, prev.registers("X"))
      val strength = calcSignalStrength(prev, cycle)
      if (prev.instructionQueue.isEmpty) {
        prev.copy(cycle = prev.cycle + 1, signalStrength = strength)
      } else {
        val (newCyclesInCurrent, registers, newList) = if (prev.cyclesInCurrentInstruction + 1 == prev.instructionQueue.head.cycles) {
          (0, prev.updateRegisterValue(prev.instructionQueue.head), prev.instructionQueue.drop(1))
        } else {
          (prev.cyclesInCurrentInstruction + 1, prev.registers, prev.instructionQueue)
        }
        val newCpuState = prev.copy(prev.cycle + 1, newCyclesInCurrent, newList, registers, strength)
        newCpuState
      }
    })
    println("")
    println(cpuState.signalStrength)
  }
}
