module common

  use iric

  implicit none

  ! 文字列の最大長
  integer, parameter :: strMax = 1000
  !******************************************************************************************
  ! CGNSのファイルID類
  !******************************************************************************************
  !> iriclibの関数のエラーコード
  integer :: is_error

  !> 読み込計算結果のあるファイルID
  integer :: cgnsIn
  !> 計算結果を出力するファイルID
  integer :: cgnsOut

  !******************************************************************************************
  ! 計算の基本設定(時間など)
  !******************************************************************************************
  !> トレーサー追跡の時間間隔
  double precision :: time_interval_for_tracking
  !> 計算終了時刻
  double precision :: time_end_out
  !> 計算結果出力間隔倍率
  integer :: output_interval_magnification
  !> 出力する時刻に元の時刻を使用するか
  integer :: is_use_original_time

  !> 読み込む計算結果のタイムステップ数
  integer :: time_step_count_in
  !> 読み込む計算結果の初期時刻
  double precision :: time_start_in
  !> 読み込む計算結果の終了時刻
  double precision :: time_end_in

  !******************************************************************************************
  ! 計算の基本設定（シミュレーションや描画の有無）
  !******************************************************************************************
  !> センターライン描画有無
  integer :: is_draw_center_line

  ! 各トレーサーの追跡有無
  !> プライマリートレーサーの追跡有無
  integer :: is_trace_primary
  !> セカンダリートレーサーの追跡有無
  integer :: is_trace_secondary
  !> 軌跡追跡トレーサーの追跡有無
  integer :: is_trace_trajectory
  !> ウィンドマップ風描画の有無
  integer :: is_draw_wind_map
  !> 魚のシミュレーションの有無
  integer :: is_simulation_fish
  !> 樹木の描写の有無
  integer :: is_draw_vegetation
  !> 礫の描画の有無
  integer :: is_draw_gravel

  !******************************************************************************************
  ! 計算に使用する値の扱いについて
  !******************************************************************************************
  ! マニングの粗度係数の扱い
  !> マニングの粗度係数に一定の値を使用するか
  integer :: is_use_constant_roughness
  !> マニングの粗度係数の一定値
  double precision :: constant_roughness

  !> トレーサーの追跡に一定値を使用するか
  integer :: flow_conditions_used_for_tracking

  !> ランダムウォークによる拡散を考慮するか
  integer :: is_consider_Diffusion
  !> 拡散係数a
  double precision :: a_diff
  !> 拡散定数b
  double precision :: b_diff

  !******************************************************************************************
  ! 計算に使用する定数
  !******************************************************************************************
  !> 重力加速度 (m/s^2)
  double precision, parameter :: g = 9.8
  !> カーマン定数
  double precision, parameter :: karman_constant = 0.4
  !> 円周率
  double precision, parameter :: pi = 3.141592
  !> 小数誤差の許容範囲
  double precision, parameter :: tolerance = 1.0d-14

contains

  !> CGNSファイルを開く処理
  subroutine open_cgns()

    !> コマンド行引数の総数
    integer :: icount
    !> 計算結果を出力するCGNSファイル名
    character(len=strMax) :: cgnsOutName
    !> 計算結果を読み込むCGNSファイル名
    character(len=strMax) :: cgnsInName

    ! 入力 CGNS ファイル名を読み込む
    ! (Intel Fortran 用)
    icount = nargs()
    if (icount == 2) then
      call getarg(1, cgnsOutName, is_error)
    else
      write (*, *) "Input File not specified."
      stop
    end if

    ! 出力用CGNSファイルを開く
    call cg_iric_open(cgnsOutName, IRIC_MODE_MODIFY, cgnsOut, is_error)
    if (is_error /= 0) STOP "*** Open error of CGNS file for output ***"

    call cg_iRIC_Clear_Sol(cgnsOut, is_error)

    !強制終了用の割り込み処理
    call iric_initoption(IRIC_OPTION_CANCEL, is_error)

    ! 分析対象のcgnsファイル名を取得
    call cg_iric_read_string(cgnsOut, 'inputfile', cgnsInName, is_error)

    ! 指定された CGNS ファイルを開く
    call cg_iric_open(cgnsInName, IRIC_MODE_READ, cgnsIn, is_error)
    if (is_error /= 0) STOP "*** Open error of CGNS file for input ***"

  end subroutine open_cgns

  subroutine close_cgns()

    call cg_iric_close(cgnsIn, is_error)
    call cg_iric_close(cgnsOut, is_error)

  end subroutine close_cgns

  subroutine read_common_parameter()

    time_step_count_in = 0

    ! 読み込む計算結果のタイムステップ数
    call cg_iric_read_sol_count(cgnsIn, time_step_count_in, is_error)
    ! 読み込む計算結果の最初の時刻
    call cg_iric_read_sol_time(cgnsIn, 1, time_start_in, is_error)
    ! 読み込む計算結果の最後の時刻
    call cg_iric_read_sol_time(cgnsIn, time_step_count_in, time_end_in, is_error)

    ! トレーサー追跡の時間間隔
    call cg_iric_read_real(cgnsOut, "time_interval_for_tracking", time_interval_for_tracking, is_error)
    ! 計算終了時刻
    call cg_iric_read_real(cgnsOut, "time_end_out", time_end_out, is_error)
    ! 出力時間間隔倍率
    call cg_iric_read_integer(cgnsOut, "output_interval_magnification", output_interval_magnification, is_error)
    ! 元の出力時刻を使用するか
    call cg_iric_read_integer(cgnsOut, "is_use_original_time", is_use_original_time, is_error)
    ! センターラインの描画
    call cg_iric_read_integer(cgnsOut, "is_draw_center_line", is_draw_center_line, is_error)

    ! ノーマルトレーサー(プライマリー)を追跡するか
    call cg_iric_read_integer(cgnsOut, "is_trace_primary", is_trace_primary, is_error)
    ! ノーマルトレーサー(セカンダリー)を追跡するか
    call cg_iric_read_integer(cgnsOut, "is_trace_secondary", is_trace_secondary, is_error)
    ! 軌跡追跡トレーサーを追跡するか
    call cg_iric_read_integer(cgnsOut, "is_trace_trajectory", is_trace_trajectory, is_error)
    ! ウィンドマップ風描画を行うか
    call cg_iric_read_integer(cgnsOut, "is_draw_wind_map", is_draw_wind_map, is_error)
    ! 魚のシミュレーションを行うか
    call cg_iric_read_integer(cgnsOut, "is_simulation_fish", is_simulation_fish, is_error)
    ! 樹木の描画を行うか
    call cg_iric_read_integer(cgnsOut, "is_draw_vegetation", is_draw_vegetation, is_error)
    ! 礫の描画を行うか
    call cg_iric_read_integer(cgnsOut, "is_draw_gravel", is_draw_gravel, is_error)

    ! トレーサーの追跡に一定値を用いるか
    call cg_iric_read_integer(cgnsOut, "flow_conditions_used_for_tracking", flow_conditions_used_for_tracking, is_error)

    ! 一定のマニング粗度係数を使用するか
    call cg_iric_read_integer(cgnsOut, "is_use_constant_roughness", is_use_constant_roughness, is_error)
    if (is_use_constant_roughness == 1) then
      call cg_iric_read_real(cgnsOut, "constant_roughness", constant_roughness, is_error)
    end if

    ! 拡散を考慮するか
    call cg_iric_read_integer(cgnsOut, "is_consider_Diffusion", is_consider_Diffusion, is_error)
    if (is_consider_Diffusion == 1) then
      call cg_iric_read_real(cgnsOut, "a_diff", a_diff, is_error)
      call cg_iric_read_real(cgnsOut, "b_diff", b_diff, is_error)
    else
      a_diff = 0.
      b_diff = 0.
    end if

  end subroutine read_common_parameter

end module
