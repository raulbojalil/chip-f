open System
open System.Windows.Forms
open System.IO
open System.Drawing

let SCREEN_WIDTH = 64
let SCREEN_HEIGHT = 32
let SCALE = 16

let random = new Random()

type DoubleBufferForm() =
    inherit Form()
    do base.SetStyle(ControlStyles.AllPaintingInWmPaint ||| ControlStyles.UserPaint ||| ControlStyles.DoubleBuffer, true)

let form = new DoubleBufferForm()

//http://en.wikipedia.org/wiki/CHIP-8
//CHIP-8's memory addresses range from 200h to FFFh, making for 3,584 bytes.
let memory = Array.create 4096 0uy  //4 KB
let mutable PC = 0x200us

let mutable romName = String.Empty
let openBinDialog = new OpenFileDialog()
openBinDialog.Title <- "Open Chip-8 ROM File"
openBinDialog.Filter <- "Chip-8 ROM Files|*.c8|All files|*.*"
match openBinDialog.ShowDialog() with
    | DialogResult.OK -> do let rom = File.ReadAllBytes(openBinDialog.FileName)
                            if rom.Length = 0 || rom.Length > (0xFFF - 0x200) then
                                ignore(MessageBox.Show("Invalid ROM file", "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error)) 
                                Environment.Exit(1)
                            else
                                romName <- openBinDialog.FileName
                                rom.CopyTo(memory, int PC)   
    | _ -> Environment.Exit(1)

//Characters 0-F (in hexadecimal) are represented by a 4x5 font.
let fontset = [| 
    0xF0uy; 0x90uy; 0x90uy; 0x90uy; 0xF0uy; //0
    0x20uy; 0x60uy; 0x20uy; 0x20uy; 0x70uy; //1
    0xF0uy; 0x10uy; 0xF0uy; 0x80uy; 0xF0uy; //2
    0xF0uy; 0x10uy; 0xF0uy; 0x10uy; 0xF0uy; //3
    0x90uy; 0x90uy; 0xF0uy; 0x10uy; 0x10uy; //4
    0xF0uy; 0x80uy; 0xF0uy; 0x10uy; 0xF0uy; //5
    0xF0uy; 0x80uy; 0xF0uy; 0x90uy; 0xF0uy; //6
    0xF0uy; 0x10uy; 0x20uy; 0x40uy; 0x40uy; //7
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0xF0uy; //8
    0xF0uy; 0x90uy; 0xF0uy; 0x10uy; 0xF0uy; //9
    0xF0uy; 0x90uy; 0xF0uy; 0x90uy; 0x90uy; //A
    0xE0uy; 0x90uy; 0xE0uy; 0x90uy; 0xE0uy; //B
    0xF0uy; 0x80uy; 0x80uy; 0x80uy; 0xF0uy; //C
    0xE0uy; 0x90uy; 0x90uy; 0x90uy; 0xE0uy; //D
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0xF0uy; //E
    0xF0uy; 0x80uy; 0xF0uy; 0x80uy; 0x80uy |]  //F  

fontset.CopyTo(memory, 0) 

//CHIP-8 has 35 opcodes, which are all two bytes long.
let mutable opcode = 0us

//CHIP-8 has 16 8-bit data registers named from V0 to VF. The VF register doubles as a carry flag.
let Vx = Array.create 16 0uy

//The address register, which is named I, is 16 bits wide and is used with several opcodes that involve memory operations.
let mutable I = 0us

//The stack is only used to store return addresses when subroutines are called. 
//The original 1802 version allocated 48 bytes for up to 12 levels of nesting; 
//modern implementations normally have at least 16 levels.
let stack = Array.create 16 0us
let mutable SP = 0us

//Input is done with a hex keyboard that has 16 keys which range from 0 to F. 
//The '8', '4', '6', and '2' keys are typically used for directional input. 
let keys = Array.create 16 0uy

//Display resolution is 64×32 pixels, and color is monochrome.
let screen = Array.create (SCREEN_WIDTH*SCREEN_HEIGHT) 0uy 

//CHIP-8 has two timers. They both count down at 60 hertz, until they reach 0.
//This timer is intended to be used for timing the events of games. Its value can be set and read.
let mutable delayTimer = 0uy
//This timer is used for sound effects. When its value is nonzero, a beeping sound is made.
let mutable soundTimer = 0uy

let mutable delayAndSoundTimersClock = DateTime.Now
let mutable executionClock = DateTime.Now

let OnKeyPress (args:KeyEventArgs) =

    match args.KeyCode with
    | Keys.D1 -> keys.[0x1] <- 1uy
    | Keys.D2 -> keys.[0x2] <- 1uy
    | Keys.D3 -> keys.[0x3] <- 1uy
    | Keys.D4 -> keys.[0xC] <- 1uy
    | Keys.Q -> keys.[0x4] <- 1uy
    | Keys.W -> keys.[0x5] <- 1uy
    | Keys.E -> keys.[0x6] <- 1uy
    | Keys.R -> keys.[0xD] <- 1uy
    | Keys.A -> keys.[0x7] <- 1uy
    | Keys.S -> keys.[0x8] <- 1uy
    | Keys.D -> keys.[0x9] <- 1uy
    | Keys.F -> keys.[0xE] <- 1uy
    | Keys.Z -> keys.[0xA] <- 1uy
    | Keys.X -> keys.[0x0] <- 1uy
    | Keys.C -> keys.[0xB] <- 1uy
    | Keys.V -> keys.[0xF] <- 1uy
    | _ -> ()

let OnKeyUp (args:KeyEventArgs) =
    match args.KeyCode with
    | Keys.D1 -> keys.[0x1] <- 0uy
    | Keys.D2 -> keys.[0x2] <- 0uy
    | Keys.D3 -> keys.[0x3] <- 0uy
    | Keys.D4 -> keys.[0xC] <- 0uy
    | Keys.Q -> keys.[0x4] <- 0uy
    | Keys.W -> keys.[0x5] <- 0uy
    | Keys.E -> keys.[0x6] <- 0uy
    | Keys.R -> keys.[0xD] <- 0uy
    | Keys.A -> keys.[0x7] <- 0uy
    | Keys.S -> keys.[0x8] <- 0uy
    | Keys.D -> keys.[0x9] <- 0uy
    | Keys.F -> keys.[0xE] <- 0uy
    | Keys.Z -> keys.[0xA] <- 0uy
    | Keys.X -> keys.[0x0] <- 0uy
    | Keys.C -> keys.[0xB] <- 0uy
    | Keys.V -> keys.[0xF] <- 0uy
    | _ -> ()

let Draw (args:PaintEventArgs) =
    let whiteBrush = new SolidBrush(Color.White)  
    for row in [0..(SCREEN_HEIGHT-1)] do
        for col in [0..(SCREEN_WIDTH-1)] do
            if screen.[col + (row * SCREEN_WIDTH)] <> 0uy then
                args.Graphics.FillRectangle(whiteBrush, col * SCALE, row * SCALE, SCALE, SCALE)
    whiteBrush.Dispose()

let Loop =
    async { 
    while true do
        if (DateTime.Now - delayAndSoundTimersClock).Milliseconds >= (1000/60) then do
            delayAndSoundTimersClock <- DateTime.Now
            if soundTimer > 0uy then
                if(soundTimer = 1uy) then 
                    Console.Beep()
                soundTimer <- soundTimer - 1uy

            if delayTimer > 0uy then delayTimer <- delayTimer - 1uy

        if (DateTime.Now - executionClock).Milliseconds >= 1 then do 
            executionClock <- DateTime.Now
            opcode <- uint16 (((uint16 memory.[int PC]) <<< 8) ||| (uint16 memory.[int PC + 1]))

            match opcode &&& 0xF000us with

                | 0x0000us -> match opcode &&& 0x00FFus with
                //00E0 Clears the screen
                              | 0x00E0us -> Array.iteri (fun i e -> screen.[i] <- 0uy) screen
                                            PC <- PC + 2us
                //00EE Returns from a subroutine
                              | 0x00EEus -> SP <- SP - 1us
                                            PC <- stack.[int SP]
                                            PC <- PC + 2us 
                              | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                        Environment.Exit(1)
        
                //1NNN Jumps to address NNN.
                | 0x1000us -> PC <- (opcode &&& 0x0FFFus)

                //2NNN Calls subroutine at NNN.
                | 0x2000us -> stack.[int SP] <- PC
                              SP <- SP + 1us
                              PC <- (opcode &&& 0x0FFFus)

                //3XNN Skips the next instruction if VX equals NN.
                | 0x3000us -> if Vx.[int ((opcode &&& 0x0F00us) >>> 8)] = byte (opcode &&& 0x00FFus) then PC <- PC + 4us else PC <- PC + 2us

                //4XNN Skips the next instruction if VX doesn't equal NN.
                | 0x4000us -> if Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <> byte (opcode &&& 0x00FFus) then PC <- PC + 4us else PC <- PC + 2us

                 //5XY0 Skips the next instruction if VX equals VY.
                | 0x5000us -> if Vx.[int ((opcode &&& 0x0F00us) >>> 8)] = Vx.[int ((opcode &&& 0x00F0us) >>> 4)] then PC <- PC + 4us else PC <- PC + 2us

                 //6XNN Sets VX to NN.
                | 0x6000us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- byte (opcode &&& 0x00FFus)
                              PC <- PC + 2us

                //7XNN Adds NN to VX.
                | 0x7000us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] + byte (opcode &&& 0x00FFus)
                              PC <- PC + 2us

                | 0x8000us -> match opcode &&& 0x000Fus with
                //8XY0 Sets VX to the value of VY.
                              | 0x0000us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                                            PC <- PC + 2us
                //8XY1 Sets VX to VX or VY.
                              | 0x0001us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] ||| Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                                            PC <- PC + 2us
                //8XY2 Sets VX to VX and VY.
                              | 0x0002us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] &&& Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                                            PC <- PC + 2us
                //8XY3 Sets VX to VX xor VY.
                              | 0x0003us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] ^^^ Vx.[int ((opcode &&& 0x00F0us) >>> 4)] 
                                            PC <- PC + 2us
                //8XY4 Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't.
                              | 0x0004us -> let sum = uint16 Vx.[int ((opcode &&& 0x0F00us) >>> 8)] + uint16 Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                                            if sum > 0xFFus then Vx.[0xF] <- 1uy else Vx.[0xF] <- 0uy 
                                            Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- byte sum
                                            PC <- PC + 2us
                //8XY5 VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
                              | 0x0005us -> if Vx.[int ((opcode &&& 0x00F0us) >>> 4)] > Vx.[int ((opcode &&& 0x0F00us) >>> 8)] then Vx.[0xF] <- 0uy else Vx.[0xF] <- 1uy
                                            Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] - Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                                            PC <- PC + 2us
                //8XY6 Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift.
                              | 0x0006us -> Vx.[0xF] <- (Vx.[int ((opcode &&& 0x0F00us) >>> 8)] &&& 0b00000001uy)
                                            Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] >>> 1
                                            PC <- PC + 2us
                //8XY7 Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't.
                              | 0x0007us -> if Vx.[int ((opcode &&& 0x0F00us) >>> 8)] > Vx.[int ((opcode &&& 0x00F0us) >>> 4)] then Vx.[0xF] <- 0uy else Vx.[0xF] <- 1uy
                                            Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x00F0us) >>> 4)] - Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                                            PC <- PC + 2us
                //8XYE Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift.
                              | 0x0008us -> Vx.[0xF] <- (Vx.[int ((opcode &&& 0x0F00us) >>> 8)] &&& 0b10000000uy) >>> 7
                                            Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <<< 1
                                            PC <- PC + 2us
                              | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                        Environment.Exit(1)

                //9XY0 Skips the next instruction if VX doesn't equal VY.
                | 0x9000us -> if Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <> Vx.[int ((opcode &&& 0x00F0us) >>> 4)] then PC <- PC + 4us else PC <- PC + 2us

                //ANNN Sets I to the address NNN
                | 0xA000us -> I <- opcode &&& 0x0FFFus
                              PC <- PC + 2us

                //BNNN Jumps to the address NNN plus V0.
                | 0xB000us -> PC <- (opcode &&& 0x0FFFus) + uint16 Vx.[0]
        
                //CXNN Sets VX to a random number and NN.
                | 0xC000us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- byte (random.Next()) &&& byte (opcode &&& 0x00FFus)
                              PC <- PC + 2us

                //DXYN Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels. 
                //     Each row of 8 pixels is read as bit-coded (with the most significant bit of each byte displayed on the left) 
                //     starting from memory location I; I value doesn't change after the execution of this instruction. 
                //     As described above, VF is set to 1 if any screen pixels are flipped from set to unset when the sprite is drawn, 
                //     and to 0 if that doesn't happen.
                | 0xD000us -> let drawX = Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                              let drawY = Vx.[int ((opcode &&& 0x00F0us) >>> 4)]
                              let drawHeight = byte (opcode &&& 0x000Fus)
                              //Console.WriteLine(String.Format("drawX: 0x{0:X4}, drawY: 0x{0:X4}, 0x{0:X4}", drawX, drawY, drawHeight))
                              
                              Vx.[0xF] <- 0uy

                              for row in [0..(int drawHeight-1)] do
                                  let rowdata = memory.[int I + row]
                                  let mutable bit = 0b10000000uy
                                  for bitpos in [0..7] do
                                      let pixelIndex = int drawX + bitpos + ((int drawY + row) * SCREEN_WIDTH) 
                                      if pixelIndex < screen.Length then 
                                          let screenPixel = screen.[pixelIndex]
                                          if rowdata &&& bit > 0uy then do 
                                              if screenPixel = 1uy then Vx.[0xF] <- 1uy
                                              screen.[int drawX + bitpos + ((int drawY + row) * SCREEN_WIDTH)] <- screenPixel ^^^ 1uy
                                          bit <- bit >>> 1

                              form.Invalidate()
                              PC <- PC + 2us

                | 0xE000us -> match opcode &&& 0x00FFus with
                              //EX9E Skips the next instruction if the key stored in VX is pressed.
                              | 0x009Eus -> if keys.[int Vx.[int ((opcode &&& 0x0F00us) >>> 8)]] <> 0uy then PC <- PC + 4us else PC <- PC + 2us
                              //EXA1 Skips the next instruction if the key stored in VX isn't pressed.
                              | 0x00A1us -> if keys.[int Vx.[int ((opcode &&& 0x0F00us) >>> 8)]] = 0uy then PC <- PC + 4us else PC <- PC + 2us
                              | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                        Environment.Exit(1)

                | 0xF000us -> match opcode &&& 0x00FFus with
                              //FX07 Sets VX to the value of the delay timer.
                              | 0x0007us -> Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- delayTimer
                                            PC <- PC + 2us 
                              //FX0A A key press is awaited, and then stored in VX.
                              | 0x000Aus -> for i in [0..keys.Length-1] do
                                                if keys.[i] = 1uy then 
                                                    Vx.[int ((opcode &&& 0x0F00us) >>> 8)] <- keys.[i]
                                                    PC <- PC + 2us
                              //FX15 Sets the delay timer to VX.
                              | 0x0015us -> delayTimer <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] 
                                            PC <- PC + 2us 
                              //FX18 Sets the sound timer to VX.
                              | 0x0018us -> soundTimer <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] 
                                            PC <- PC + 2us 
                              //FX1E Adds VX to I.
                              | 0x001Eus -> let sum = uint32 I + uint32 Vx.[int ((opcode &&& 0x0F00us) >>> 8)]
                                            if sum > 0xFFFFu then Vx.[0xF] <- 1uy else Vx.[0xF] <- 0uy
                                            I <- uint16 sum
                                            PC <- PC + 2us 
                              //FX29 Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font.
                              | 0x0029us -> I <- 0us + uint16 (Vx.[int ((opcode &&& 0x0F00us) >>> 8)]*5uy)
                                            PC <- PC + 2us 
                              //FX33 Stores the Binary-coded decimal representation of VX, with the most significant of three digits at the address in I, the middle digit at I plus 1, and the least significant digit at I plus 2. (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I, the tens digit at location I+1, and the ones digit at location I+2.)
                              | 0x0033us -> memory.[int I] <- Vx.[int ((opcode &&& 0x0F00us) >>> 8)] / 100uy
                                            memory.[int I + 1] <- (Vx.[int ((opcode &&& 0x0F00us) >>> 8)] / 10uy) % 10uy
                                            memory.[int I + 2] <- (Vx.[int ((opcode &&& 0x0F00us) >>> 8)] % 100uy) % 10uy
                                            PC <- PC + 2us 
                              //FX55 Stores V0 to VX in memory starting at address I.
                              | 0x0055us -> for i in [0..(int ((opcode &&& 0x0F00us) >>> 8))] do
                                                memory.[int I + i] <- Vx.[int i] 
                                            PC <- PC + 2us 
                              //FX65 Fills V0 to VX with values from memory starting at address I.
                              | 0x0065us -> for i in [0..(int ((opcode &&& 0x0F00us) >>> 8))] do
                                                Vx.[int i] <- memory.[int I + i] 
                                            PC <- PC + 2us 

                              | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                                        Environment.Exit(1)

                | _ -> do ignore(MessageBox.Show(String.Format("Unhandled opcode 0x{0:X4}", opcode), "FATAL ERROR", MessageBoxButtons.OK, MessageBoxIcon.Error))
                          Environment.Exit(1)
            ()}

form.ClientSize <- new System.Drawing.Size(SCREEN_WIDTH * SCALE, SCREEN_HEIGHT * SCALE)
form.Load.Add(fun e -> form.BackColor <- Color.Black
                       Async.Start(Loop))
form.Paint.Add(Draw)
form.KeyDown.Add(OnKeyPress)
form.KeyUp.Add(OnKeyUp)
form.Text <- String.Format("{0} - Chip-F Emulator", romName)
form.MaximizeBox <- false
form.FormBorderStyle <- FormBorderStyle.FixedSingle
Application.Run(form)