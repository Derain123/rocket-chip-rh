//==========================================================
// RAIN_RCU_runahead
package freechips.rocketchip.rocket

import chisel3._
import chisel3.experimental.BaseModule
//import freechips.rocketchip.tile._

//import scala.collection.mutable.ArrayBuffer
// import chisel3.stage.ChiselStage


//==========================================================
// Parameters
//==========================================================
case class RCU_Params (
  xLen: Int = 64
)

//==========================================================
// I/Os
//==========================================================
class RCU_IO (params: RCU_Params) extends Bundle {
  //val in = UInt(params.xLen.W)
  //val out = UInt(params.xLen.W)
  val wb_valid = Input(Bool())
  //val tag = Input(UInt)
  val l2miss = Input(Bool())
  val rf_in = Input(Vec(31,UInt(params.xLen.W)))
  // val fp_in = Input(Vec(32,UInt(params.xLen.W+1)))     //floating point register file
  val ipc = Input(UInt(40.W))
  val rf_out = Output(Vec(31,UInt(params.xLen.W)))
  val runahead_backflag = Output(Bool())
  // val fp_out = Output(Vec(32,UInt(params.xLen.W+1)))    //floating point register file
  // val stall_pipe = Output(Bool())
  // val opc = Output(UInt(40.W))
}

trait Has_RCU_IO extends BaseModule {
  val params: RCU_Params
  val io = IO(new RCU_IO(params))
}

//==========================================================
// Implementations
//==========================================================
class RCU (val params: RCU_Params) extends Module with Has_RCU_IO {
  //val rf_reg = RegInit(0.U(params.xLen.W))
  val rf_reg = RegInit(VecInit(Seq.fill(31)(0.U(params.xLen.W))))
  // val fp_reg = RegInit(VecInit(Seq.fill(32)(0.U(params.xLen.W+1))))      //floating point register file
  //val storepc = RegInit(0.U(40.W))
  //initialize
  for (i <- 0 until 31) {
    io.rf_out(i) := 0.U(params.xLen.W)
  }
  io.runahead_backflag := false.B
  dontTouch(io)
  dontTouch(rf_reg)

  //==========================================================method 2
  // 状态机逻辑
  // val sIdle :: sCopyRegs :: sWriteBack :: Nil = Enum(3)
  // val state = RegInit(sIdle)
  // switch(state) {
  //   is(sIdle) {
  //     counter := counter + 1.U
  //     when(counter === 100.U) {
  //       // 复制寄存器文件，然后转换到下一个状态
  //       // newRegFile := regFile
  //       // exit_runahead := 1.U
  //       // state := sCopyRegs
  //       // counter := 0.U // 重置计数器
  //     }
  //   }
  //   is(sCopyRegs) {
  //     // state := sWriteBack
  //   }
  //   is(sWriteBack) {
  //     // ...
  //   }
  // }
  //==========================================================method 2

  //==========================================================method 1

  //==========================================================method 1
  when(io.l2miss) {
    for (i <- 0 until 31) {
      rf_reg(i) := io.rf_in(i)
    }
    // for (i <- 0 until 32) {
    //   fp_reg(i) := io.fp_in(i)
    // }
    //storepc := io.ipc
    //io.stall_pipe := true.B
  }
//  } otherwise{
//    io.stall_pipe<>DontCare
//    io.opc <> DontCare
//  }

  when(io.wb_valid) {
    for (j <- 0 until 31) {
      io.runahead_backflag := true.B
      io.rf_out(j) := rf_reg(j)
      // rf_reg(j) := 0.U(params.xLen.W)
    }
    // for (j <- 0 until 32) {
    //   io.fp_out(j) := fp_reg(j)
    //   //fp_reg(j) := 0.U(params.xLen.W+1)
    // }
    //io.stall_pipe := false.B
    //io.opc := storepc
    //storepc:= 0.U
  } 
  // otherwise {
    //io.stall_pipe<>DontCare
    //io.opc := 0.U
    // for (j <- 0 until 31) {
      // io.rf_out(j) := 0.U(params.xLen.W)
    // }
    // for (j <- 0 until 32) {
    //   io.fp_out(j) := io.fp_out(j)
    // }
  // }


  /* for(j<-0 until 31)
  {
    io.rf_out(j) := Mux(counter===500.U, rf_reg(j), 0.U)
  }*/
  //io.rf_out := Mux(counter === 500.U, rf_reg1, 0.U)
  //==========================================================method 1
  
}
//==========================================================