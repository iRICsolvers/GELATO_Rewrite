module trace
  use iric
  use grid
  use result

  implicit none

  !******************************************************************************************
  ! 通常トレーサーについて
  !******************************************************************************************

  !> 通常トレーサーの散布開始時間
  double precision:: supply_time_start_normal_tracers = 0.0
  !> 通常トレーサーの散布終了時間
  double precision:: supply_time_end_normal_tracers = 0.0
  !> 通常トレーサーの散布時間間隔
  double precision:: supply_time_interval_normal_tracers = 99999.0
  !> 通常トレーサーの周期境界条件
  integer:: is_periodic_boundary_condition_Tracers
  !> 停止したトレーサーの扱い 1:その場で留まる 2:除去する
  integer:: stopped_tracer_handling
  !> トレーサー追加のタイムカウンター
  double precision :: time_counter_add_normal_tracer

  !> 通常トレーサーの構造体
  type :: normal_tracer

    !==========================================================================================
    ! 通常トレーサー
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! GUIから読み込む値
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> 最大トレーサー数
    integer:: max_number
    !> セル内の最大トレーサー数
    integer:: max_number_in_cell
    !> 移動限界水深
    double precision:: Movable_Critical_depth
    !> 移動限界摩擦速度
    double precision:: Movable_Critical_u_star
    !> トレーサー補足壁の高さ
    double precision:: trap_wall_height
    !> トレーサー捕捉率
    double precision:: trap_rate
    !> ξ方向配置始点
    double precision:: supply_position_xi_first
    !> ξ方向配置終点
    double precision:: supply_position_xi_end
    !> ξ方向配置間隔
    double precision:: supply_interval_xi
    !> η方向配置始点
    double precision:: supply_position_eta_first
    !> η方向配置終点
    double precision:: supply_position_eta_end
    !> η方向配置間隔
    double precision:: supply_interval_eta

    !> トレーサークローニングを行うか
    integer:: is_tracer_cloning
    !> クローニング手法 0:全ての空白セル 1:トレーサーが1つしかないセル 2:指定したセル
    integer:: cloning_option
    !> 最大分割回数
    integer:: max_generation
    !> すべての空白セルにトレーサーを発生させる場合の割引係数
    integer:: cloning_reduction_factor

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサーの属性
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !> トレーサーのξ座標
    double precision, dimension(:), allocatable :: tracer_coordinate_xi
    !> トレーサーのη座標
    double precision, dimension(:), allocatable :: tracer_coordinate_eta
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_i
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_j
    !> セル内でのトレーサーのξ座標
    double precision, dimension(:), allocatable :: tracer_coordinate_xi_in_cell
    !> セル内でのトレーサーのη座標
    double precision, dimension(:), allocatable :: tracer_coordinate_eta_in_cell
    !> トレーサーの重み
    double precision, dimension(:), allocatable :: tracer_weight
    !> トレーサーの世代
    integer, dimension(:), allocatable :: tracer_generation
    !> トレーサーが動くことができるか
    integer, dimension(:), allocatable :: is_tracer_movable
    !> トレーサーがトラップにとらわれているか
    integer, dimension(:), allocatable :: is_tracer_traped
    !> トレーサーが無敵状態か
    integer, dimension(:), allocatable :: is_tracer_invincibl

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! !トレーサー数の統計
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> トレーサーの総数
    integer :: total_tracer_number
    !> セル内のトレーサー総数
    integer, dimension(:, :), allocatable :: tracer_number_in_cell
    !>セル内の重み付きトレーサー数
    double precision, dimension(:, :), allocatable ::  Weighted_number_in_cell
    !> セル内のトレーサー総数のi断面合計
    integer, dimension(:, :), allocatable :: total_tracer_number_in_cross_section
    !> セル内のトレーサー総数のi断面平均
    double precision, dimension(:, :), allocatable :: averaged_tracer_number_in_cross_section
    !> セル内の時間積算トレーサー数
    double precision, dimension(:, :), allocatable ::  time_integrated_tracer_number_in_cell
    !> セル内の時間平均トレーサー数
    double precision, dimension(:, :), allocatable ::  time_averaged_tracer_number_in_cell

  end type normal_tracer

  !> 仮通常トレーサーの構造体
  type :: normal_tracer_tmp

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 計算で使用する値
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !> トレーサーのξ座標
    double precision, dimension(:), allocatable :: tracer_coordinate_xi
    !> トレーサーのη座標
    double precision, dimension(:), allocatable :: tracer_coordinate_eta
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_i
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_j
    !> セル内でのトレーサーのξ座標
    double precision, dimension(:), allocatable :: tracer_coordinate_xi_in_cell
    !> セル内でのトレーサーのη座標
    double precision, dimension(:), allocatable :: tracer_coordinate_eta_in_cell
    !> トレーサーの重み
    double precision, dimension(:), allocatable :: tracer_weight
    !> トレーサーの世代
    integer, dimension(:), allocatable :: tracer_generation
    !> トレーサーが動くことができるか
    integer, dimension(:), allocatable :: is_tracer_movable
    !> トレーサーがトラップにとらわれているか
    integer, dimension(:), allocatable :: is_tracer_traped
    !> トレーサーが無敵状態か
    integer, dimension(:), allocatable :: is_tracer_invincibl

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! !トレーサー数の統計
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> トレーサーの総数
    integer :: total_tracer_number
    !> セル内のトレーサー総数
    integer, dimension(:, :), allocatable :: tracer_number_in_cell
    !>セル内の重み付きトレーサー数
    double precision, dimension(:, :), allocatable ::  Weighted_number_in_cell

  end type normal_tracer_tmp

  !> プライマートレーサーのパラメータ群
  type(normal_tracer) :: primary
  !> 移動後のトレーサーを格納するための一時配列
  type(normal_tracer_tmp) :: primary_tmp
  !> セカンダリートレーサーのパラメータ群
  type(normal_tracer) :: secondary
  !> 移動後のトレーサーを格納するための一時配列
  type(normal_tracer_tmp) :: secondary_tmp

  !******************************************************************************************
  ! 軌跡追跡トレーサー
  !******************************************************************************************

  !> 軌跡追跡トレーサーの最大数
  integer :: max_number_trajectory
  !> 軌跡の最大保存回数
  integer :: max_save_times_trajectory
  !> 軌跡の保存間隔
  integer :: save_interval_trajectory

contains

  !******************************************************************************************
  !> @brief 通常トレーサーの初期化を行うサブルーチン
  !******************************************************************************************
  subroutine Initialize_Normal_Tracer()

    !==========================================================================================
    ! 共通条件
    !==========================================================================================
    call cg_iric_read_real(cgnsOut, "supply_time_start_normal_tracers", supply_time_start_normal_tracers, is_error)
    call cg_iric_read_real(cgnsOut, "supply_time_end_normal_tracers", supply_time_end_normal_tracers, is_error)
    call cg_iric_read_real(cgnsOut, "supply_time_interval_normal_tracers", supply_time_interval_normal_tracers, is_error)
    call cg_iric_read_integer(cgnsOut, "is_periodic_boundary_condition_Tracers", is_periodic_boundary_condition_Tracers, is_error)
    call cg_iric_read_integer(cgnsOut, "stopped_tracer_handling", stopped_tracer_handling, is_error)

    !==========================================================================================
    ! プライマリートレーサー
    !==========================================================================================
    if (is_trace_primary) call Initialize_Normal_Tracer_type("primary", primary)
    if (is_trace_primary) call Initialize_normal_tracer_tmp_type(primary%max_number, primary_tmp)

    !==========================================================================================
    ! セカンダリートレーサー
    !==========================================================================================
    if (is_trace_secondary) call Initialize_Normal_Tracer_type("secondary", secondary)
    if (is_trace_secondary) call Initialize_Normal_Tracer_tmp_type(secondary%max_number, secondary_tmp)

  end subroutine Initialize_Normal_Tracer

  !******************************************************************************************
  !> @brief 通常トレーサーの各タイプの初期化を行うサブルーチン
  !> @brief トレーサーの設定値を外部ファイルから読み込み、メモリを確保して初期化を行う。
  !> @param[in]  suffix  トレーサーのタイプを表す文字列（例: "primary", "secondary"）
  !> @param[inout] tracer  初期化されるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine Initialize_Normal_Tracer_type(suffix, tracer)
    character(len=*), intent(in) :: suffix
    type(normal_tracer), intent(inout) :: tracer

    call cg_iric_read_integer(cgnsOut, "max_number_"//trim(suffix), tracer%max_number, is_error)
    call cg_iric_read_integer(cgnsOut, "max_number_in_cell_"//trim(suffix), tracer%max_number_in_cell, is_error)
    call cg_iric_read_real(cgnsOut, "Movable_Critical_depth_"//trim(suffix), tracer%Movable_Critical_depth, is_error)
    call cg_iric_read_real(cgnsOut, "Movable_Critical_u_star_"//trim(suffix), tracer%Movable_Critical_u_star, is_error)
    call cg_iric_read_real(cgnsOut, "trap_wall_height_"//trim(suffix), tracer%trap_wall_height, is_error)
    call cg_iric_read_real(cgnsOut, "trap_rate_"//trim(suffix), tracer%trap_rate, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_xi_first_"//trim(suffix), tracer%supply_position_xi_first, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_xi_end_"//trim(suffix), tracer%supply_position_xi_end, is_error)
    call cg_iric_read_real(cgnsOut, "supply_interval_xi_"//trim(suffix), tracer%supply_interval_xi, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_eta_first_"//trim(suffix), tracer%supply_position_eta_first, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_eta_end_"//trim(suffix), tracer%supply_position_eta_end, is_error)
    call cg_iric_read_real(cgnsOut, "supply_interval_eta_"//trim(suffix), tracer%supply_interval_eta, is_error)
    call cg_iric_read_integer(cgnsOut, "is_tracer_cloning_"//trim(suffix), tracer%is_tracer_cloning, is_error)
    call cg_iric_read_integer(cgnsOut, "cloning_option_"//trim(suffix), tracer%cloning_option, is_error)
    call cg_iric_read_integer(cgnsOut, "max_generation_"//trim(suffix), tracer%max_generation, is_error)
    call cg_iric_read_integer(cgnsOut, "cloning_reduction_factor_"//trim(suffix), tracer%cloning_reduction_factor, is_error)

    allocate (tracer%tracer_coordinate_xi(tracer%max_number))
    allocate (tracer%tracer_coordinate_eta(tracer%max_number))
    allocate (tracer%cell_index_i(tracer%max_number))
    allocate (tracer%cell_index_j(tracer%max_number))
    allocate (tracer%tracer_coordinate_xi_in_cell(tracer%max_number))
    allocate (tracer%tracer_coordinate_eta_in_cell(tracer%max_number))
    allocate (tracer%tracer_weight(tracer%max_number))
    allocate (tracer%tracer_generation(tracer%max_number))
    allocate (tracer%is_tracer_movable(tracer%max_number))
    allocate (tracer%is_tracer_traped(tracer%max_number))
    allocate (tracer%is_tracer_invincibl(tracer%max_number))

    allocate (tracer%tracer_number_in_cell(cell_count_i, cell_count_j))
    allocate (tracer%Weighted_number_in_cell(cell_count_i, cell_count_j))
    allocate (tracer%total_tracer_number_in_cross_section(cell_count_i, cell_count_j))
    allocate (tracer%averaged_tracer_number_in_cross_section(cell_count_i, cell_count_j))
    allocate (tracer%time_integrated_tracer_number_in_cell(cell_count_i, cell_count_j))
    allocate (tracer%time_averaged_tracer_number_in_cell(cell_count_i, cell_count_j))

    tracer%tracer_coordinate_xi = 0.0
    tracer%tracer_coordinate_eta = 0.0
    tracer%cell_index_i = 0
    tracer%cell_index_j = 0
    tracer%tracer_coordinate_xi_in_cell = 0.0
    tracer%tracer_coordinate_eta_in_cell = 0.0
    tracer%tracer_weight = 0.0
    tracer%tracer_generation = 0
    tracer%is_tracer_movable = 0
    tracer%is_tracer_traped = 0
    tracer%is_tracer_invincibl = 0

    tracer%total_tracer_number = 0

    tracer%tracer_number_in_cell = 0
    tracer%Weighted_number_in_cell = 0.0
    tracer%total_tracer_number_in_cross_section = 0
    tracer%averaged_tracer_number_in_cross_section = 0.0
    tracer%time_integrated_tracer_number_in_cell = 0.0
    tracer%time_averaged_tracer_number_in_cell = 0.0

  end subroutine Initialize_Normal_Tracer_type

  !******************************************************************************************
  !> @brief 通常トレーサーの仮タイプのメモリ確保を行う
  !> @param[in]  suffix  トレーサーのタイプを表す文字列（例: "primary", "secondary"）
  !> @param[inout] tracer_tmp  初期化されるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine Initialize_normal_tracer_tmp_type(max_number, tracer_tmp)
    integer, intent(in) :: max_number
    type(normal_tracer_tmp), intent(inout) :: tracer_tmp

    allocate (tracer_tmp%tracer_coordinate_xi(max_number))
    allocate (tracer_tmp%tracer_coordinate_eta(max_number))
    allocate (tracer_tmp%cell_index_i(max_number))
    allocate (tracer_tmp%cell_index_j(max_number))
    allocate (tracer_tmp%tracer_coordinate_xi_in_cell(max_number))
    allocate (tracer_tmp%tracer_coordinate_eta_in_cell(max_number))
    allocate (tracer_tmp%tracer_weight(max_number))
    allocate (tracer_tmp%tracer_generation(max_number))
    allocate (tracer_tmp%is_tracer_movable(max_number))
    allocate (tracer_tmp%is_tracer_traped(max_number))
    allocate (tracer_tmp%is_tracer_invincibl(max_number))

    allocate (tracer_tmp%tracer_number_in_cell(cell_count_i, cell_count_j))
    allocate (tracer_tmp%Weighted_number_in_cell(cell_count_i, cell_count_j))

    tracer_tmp%tracer_coordinate_xi = 0.0
    tracer_tmp%tracer_coordinate_eta = 0.0
    tracer_tmp%cell_index_i = 0
    tracer_tmp%cell_index_j = 0
    tracer_tmp%tracer_coordinate_xi_in_cell = 0.0
    tracer_tmp%tracer_coordinate_eta_in_cell = 0.0
    tracer_tmp%tracer_weight = 0.0
    tracer_tmp%tracer_generation = 0
    tracer_tmp%is_tracer_movable = 0
    tracer_tmp%is_tracer_traped = 0
    tracer_tmp%is_tracer_invincibl = 0

    tracer_tmp%total_tracer_number = 0
    tracer_tmp%tracer_number_in_cell = 0
    tracer_tmp%Weighted_number_in_cell = 0.0

  end subroutine Initialize_normal_tracer_tmp_type

  !******************************************************************************************
  !> @brief 通常トレーサーの各トレーサーの値をリセット
  !> @param[inout] tracer_tmp  リセットされるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine reset_normal_tracer(tracer_tmp)
    type(normal_tracer_tmp), intent(inout) :: tracer_tmp

    tracer_tmp%tracer_coordinate_xi = 0.0
    tracer_tmp%tracer_coordinate_eta = 0.0
    tracer_tmp%cell_index_i = 0
    tracer_tmp%cell_index_j = 0
    tracer_tmp%tracer_coordinate_xi_in_cell = 0.0
    tracer_tmp%tracer_coordinate_eta_in_cell = 0.0
    tracer_tmp%tracer_weight = 0.0
    tracer_tmp%tracer_generation = 0
    tracer_tmp%is_tracer_movable = 0
    tracer_tmp%is_tracer_traped = 0
    tracer_tmp%is_tracer_invincibl = 0

    tracer_tmp%total_tracer_number = 0
    tracer_tmp%tracer_number_in_cell = 0

  end subroutine reset_normal_tracer

  !******************************************************************************************
  !> @brief トレーサーが位置するセルのインデックスを探査してセル内での座標も計算する
  !> @param[in] tracer_coordinate_xi トレーサーのξ座標
  !> @param[in] tracer_coordinate_eta トレーサーのη座標
  !> @param[inout] cell_index_i トレーサーの存在するセルのi方向インデックス
  !> @param[inout] cell_index_j トレーサーの存在するセルのi方向インデックス
  !> @param[inout] tracer_coordinate_xi_in_cell セル内でのトレーサーのξ座標
  !> @param[inout] tracer_coordinate_eta_in_cell セル内でのトレーサーのη座標
  !******************************************************************************************
  subroutine find_tracer_cell_index(tracer_coordinate_xi, tracer_coordinate_eta, cell_index_i, cell_index_j, tracer_coordinate_xi_in_cell, tracer_coordinate_eta_in_cell)
    double precision, intent(in) :: tracer_coordinate_xi
    double precision, intent(in) :: tracer_coordinate_eta
    integer, intent(inout) :: cell_index_i
    integer, intent(inout) :: cell_index_j
    double precision, intent(inout) :: tracer_coordinate_xi_in_cell
    double precision, intent(inout) :: tracer_coordinate_eta_in_cell

    cell_index_i = int(tracer_coordinate_xi/grid_interval_xi) + 1
    cell_index_j = int(tracer_coordinate_eta/grid_interval_eta) + 1

    if (abs(tracer_coordinate_xi - 1.0d0) < tolerance) cell_index_i = cell_count_i
    if (abs(tracer_coordinate_eta - 1.0d0) < tolerance) cell_index_j = cell_count_j

    ! セル内のトレーサーのξ、η座標を計算
    tracer_coordinate_xi_in_cell = tracer_coordinate_xi - grid_interval_xi*(cell_index_i - 1)
    tracer_coordinate_eta_in_cell = tracer_coordinate_eta - grid_interval_eta*(cell_index_j - 1)

  end subroutine find_tracer_cell_index

  !******************************************************************************************
  !> @brief トレーサー位置のスカラーを周囲の格子点の値から計算
  !> @param[in] scalar スカラーの配列
  !> @param[in] cell_index_i トレーサーのあるセルのi方向インデックス
  !> @param[in] cell_index_j トレーサーのあるセルのj方向インデックス
  !> @param[in] tracer_coordinate_xi_in_cell セル内でのトレーサーのξ座標
  !> @param[in] tracer_coordinate_eta_in_cell セル内でのトレーサーのη座標
  !> @return point トレーサー位置のスカラー
  !******************************************************************************************
  function calculate_scalar_at_tracer_position(scalar, cell_index_i, cell_index_j, tracer_coordinate_xi_in_cell, tracer_coordinate_eta_in_cell) result(point)
    double precision, intent(in), dimension(:, :) :: scalar
    integer, intent(in) :: cell_index_i
    integer, intent(in) :: cell_index_j
    double precision :: tracer_coordinate_xi_in_cell
    double precision :: tracer_coordinate_eta_in_cell
    double precision :: point
    !> ξ方向で上側の2つの値(i,j+1),(i+1,j+1)の補間結果
    double precision :: interpolated_xi_top
    !> ξ方向で下側の2つの値(i,j),(i+1,j)の補間結果
    double precision :: interpolated_xi_bottom

    ! トレーサー位置のスカラーを計算
    interpolated_xi_bottom = scalar(cell_index_i, cell_index_j) + (scalar(cell_index_i + 1, cell_index_j) - scalar(cell_index_i, cell_index_j))*tracer_coordinate_xi_in_cell/grid_interval_xi
    interpolated_xi_top = scalar(cell_index_i, cell_index_j + 1) + (scalar(cell_index_i + 1, cell_index_j + 1) - scalar(cell_index_i, cell_index_j + 1))*tracer_coordinate_xi_in_cell/grid_interval_xi
    point = interpolated_xi_bottom + (interpolated_xi_top - interpolated_xi_bottom)*tracer_coordinate_eta_in_cell/grid_interval_eta

  end function calculate_scalar_at_tracer_position

  !******************************************************************************************
  !> @brief トレーサーの物理座標を計算
  !> @param[in] cell_index_i トレーサーのあるセルのi方向インデックス
  !> @param[in] cell_index_j トレーサーのあるセルのj方向インデックス
  !> @param[in] tracer_coordinate_xi_in_cell セル内でのトレーサーのξ座標
  !> @param[in] tracer_coordinate_eta_in_cell セル内でのトレーサーのη座標
  !> @param[inout] tracer_coordinate_x トレーサーx座標
  !> @param[inout] tracer_coordinate_y トレーサーy座標
  !******************************************************************************************
  subroutine transform_general_to_physical(cell_index_i, cell_index_j, tracer_coordinate_xi_in_cell, tracer_coordinate_eta_in_cell, tracer_coordinate_x, tracer_coordinate_y)
    integer, intent(in) :: cell_index_i
    integer, intent(in) :: cell_index_j
    double precision, intent(in) :: tracer_coordinate_xi_in_cell
    double precision, intent(in) :: tracer_coordinate_eta_in_cell
    double precision, intent(inout) :: tracer_coordinate_x
    double precision, intent(inout) :: tracer_coordinate_y

    ! 物理座標x、yを計算
    tracer_coordinate_x = node_coordinate_x(cell_index_i, cell_index_j) &
                          + x_gradient_in_xi(cell_index_i, cell_index_j)*tracer_coordinate_xi_in_cell &
                          + x_gradient_in_eta(cell_index_i, cell_index_j)*tracer_coordinate_eta_in_cell
    tracer_coordinate_y = node_coordinate_y(cell_index_i, cell_index_j) &
                          + y_gradient_in_xi(cell_index_i, cell_index_j)*tracer_coordinate_xi_in_cell &
                          + y_gradient_in_eta(cell_index_i, cell_index_j)*tracer_coordinate_eta_in_cell

  end subroutine transform_general_to_physical

  !******************************************************************************************
  !> @brief   入力された整数値を増加させるサブルーチン
  !> @param[inout] value  入力される元の整数値、結果として増加した値を返します
  !> @param[in] add_value  加算値
  !******************************************************************************************
  subroutine increment_integer_value(value, add_value)
    implicit none
    integer, intent(inout) :: value   ! 入力および出力: もとの整数値と増加後の値
    integer, intent(in) :: add_value

    ! 値を増やす
    value = value + add_value
  end subroutine increment_integer_value

  !******************************************************************************************
  !> @brief   入力された実数値を増加させるサブルーチン
  !> @param[inout] value  入力される元の整数値、結果として増加した値を返します
  !> @param[in] add_value  加算値
  !******************************************************************************************
  subroutine increment_real_value(value, add_value)
    implicit none
    double precision, intent(inout) :: value   ! 入力および出力: もとの整数値と増加後の値
    double precision, intent(in) :: add_value

    ! 値を増やす
    value = value + add_value

  end subroutine increment_real_value

  !******************************************************************************************
  !> @brief   入力された実数値を増加させるサブルーチン(配列用)
  !> @param[inout] array  入力される元の配列、結果として増加した値を返します
  !> @param[in] add_value  加算値の配列
  !******************************************************************************************
  subroutine increment_real_value_array(array, add_array)
    implicit none
    double precision, intent(inout) :: array(:, :)   ! 入力および出力: もとの整数値と増加後の値
    double precision, intent(in) :: add_array(:, :)

    ! 値を増やす
    array = array + add_array

  end subroutine increment_real_value_array

  !******************************************************************************************
  !> @brief ボックスミュラーによる正規分布乱数を発生させる
  !> @param[inout] z0 正規分布乱数(cos成分)
  !> @param[inout] z1 正規分布乱数(sin成分)
  !******************************************************************************************
  subroutine generate_box_muller_random(z0, z1)
    implicit none
    real(8)::drand
    double precision :: u1
    double precision :: u2
    double precision, intent(inout) :: z0
    double precision, intent(inout) :: z1

    u1 = drand(0)
    u2 = drand(0)
    z0 = sqrt(-2.*log(u1))*cos(2.*pi*u2)
    z1 = sqrt(-2.*log(u1))*sin(2.*pi*u2)

  end subroutine generate_box_muller_random

  !******************************************************************************************
  !> @brief トレーサー位置のスカラーから追加や移動をさせるかチェックする
  !> @param[in] tracer
  !> @param[in] tracer_position_xi トレーサーのξ方向座標
  !> @param[in] tracer_position_eta トレーサーのη方向座標
  !> @param[in] tracer_position_i 投入地点のセルインデックス
  !> @param[in] tracer_position_j 投入地点のセルインデックス
  !> @param[in] tracer_position_xi_in_cell セル内でのξ方向投入地点座標
  !> @param[in] tracer_position_eta_in_cell セル内でのη方向投入地点座標
  !> @param[inout] is_add_tracer トレーサーの追加、除去の対象か
  !> @param[inout] is_tracer_movable トレーサーが動くか
  ! メモ：障害物に関するチェックは移動と追加で扱いが異なるのでこの中では行わない
  !******************************************************************************************
  subroutine check_tracer(tracer, tracer_position_xi, tracer_position_eta, tracer_position_i, tracer_position_j, tracer_position_xi_in_cell, tracer_position_eta_in_cell, is_add_tracer, is_tracer_movable)
    !> 対象のトレーサー
    type(normal_tracer), intent(inout) :: tracer
    !> ξ方向トレーサー座標
    double precision, intent(in) :: tracer_position_xi
    !> η方向トレーサー座標
    double precision, intent(in) :: tracer_position_eta
    !> 投入地点のセルインデックス
    integer, intent(in) :: tracer_position_i
    !> 投入地点のセルインデックス
    integer, intent(in) :: tracer_position_j
    !> セル内でのξ方向投入地点座標
    double precision, intent(in) :: tracer_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    double precision, intent(in) :: tracer_position_eta_in_cell
    !> 投入地点の水深
    double precision :: tracer_point_depth
    !> 投入地点の摩擦速度
    double precision :: tracer_point_u_star

    !> 投入するかしないか
    integer, intent(inout) :: is_add_tracer
    !> トレーサーが動くか
    integer, intent(inout) :: is_tracer_movable

    ! 投入地点の水深と摩擦速度を調べる
    tracer_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                             tracer_position_i, &
                                                             tracer_position_j, &
                                                             tracer_position_xi_in_cell, &
                                                             tracer_position_eta_in_cell)
    tracer_point_u_star = calculate_scalar_at_tracer_position(u_star_node, &
                                                              tracer_position_i, &
                                                              tracer_position_j, &
                                                              tracer_position_xi_in_cell, &
                                                              tracer_position_eta_in_cell)

    ! 投入箇所の水深が移動可能水深以上かつ停止したトレーサーを除去するかチェック
    if (tracer%Movable_Critical_depth > tracer_point_depth) then

      is_tracer_movable = 0

      if (stopped_tracer_handling == 2) then
        is_add_tracer = 0
      end if

    end if

    ! 投入箇所の摩擦速度が移動可能水深以上かつ停止したトレーサーを除去するかチェック
    if (tracer%Movable_Critical_u_star > tracer_point_u_star) then

      is_tracer_movable = 0

      if (stopped_tracer_handling == 2) then
        is_add_tracer = 0
      end if

    end if

  end subroutine check_tracer

  !******************************************************************************************
  !> @brief トレーサーを追加する
  !> @param[inout] tracer  初期化されるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine add_tracer(tracer)

    type(normal_tracer), intent(inout) :: tracer

    !> この投入で追加されるトレーサー数
    integer :: added_tracer_number
    !> ξ方向投入地点座標
    double precision :: supply_position_xi
    !> η方向投入地点座標
    double precision :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    double precision :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    double precision :: supply_position_eta_in_cell

    !> 投入するかしないか
    integer :: is_add_tracer
    !> トレーサーが動くか
    integer :: is_tracer_movable

    !> 投入範囲ループの総数
    integer :: supply_loop_count_xi
    !> 投入範囲ループの総数
    integer :: supply_loop_count_eta
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_xi
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_eta

    added_tracer_number = 0

    !==========================================================================================
    ! 投入箇所ごとのループ
    !==========================================================================================

    ! ループ回数の計算
    supply_loop_count_xi = int((tracer%supply_position_xi_end - tracer%supply_position_xi_first + tolerance)/tracer%supply_interval_xi + tolerance)
    supply_loop_count_eta = int((tracer%supply_position_eta_end - tracer%supply_position_eta_first + tolerance)/tracer%supply_interval_eta + tolerance)

    do supply_loop_index_xi = 0, supply_loop_count_xi

      ! 投入箇所を計算
      supply_position_xi = tracer%supply_position_xi_first + tracer%supply_interval_xi*supply_loop_index_xi

      ! 範囲外ならループ終了
      if (supply_position_xi > tracer%supply_position_xi_end + tolerance) exit

      ! 誤差範囲内なら誤差を補正
      if (supply_position_xi > tracer%supply_position_xi_end) supply_position_xi = tracer%supply_position_xi_end

      do supply_loop_index_eta = 0, supply_loop_count_eta

        ! 投入箇所を計算
        supply_position_eta = tracer%supply_position_eta_first + tracer%supply_interval_eta*supply_loop_index_eta

        ! 誤差範囲内なら誤差を補正
        if (supply_position_eta > tracer%supply_position_eta_end + tolerance) exit

        ! 誤差範囲内なら誤差を補正
        if (supply_position_eta > tracer%supply_position_eta_end) supply_position_eta = tracer%supply_position_eta_end

        ! 投入フラグをリセット
        is_add_tracer = 1

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 投入箇所に投入できるかチェック
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! 投入地点のセルのインデックスを調べる
        call find_tracer_cell_index(supply_position_xi, supply_position_eta, supply_position_i, supply_position_j, supply_position_xi_in_cell, supply_position_eta_in_cell)

        ! 投入箇所が障害物セルかチェック
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) then
          is_add_tracer = 0
        end if

        ! 投入箇所セル内の格子の数が最大数に達しているかチェック
        if (tracer%tracer_number_in_cell(supply_position_i, supply_position_j) >= tracer%max_number_in_cell) then
          is_add_tracer = 0
        end if

        ! 投入箇所の水深と摩擦速度を調べる
        call check_tracer(tracer, &
                          supply_position_xi, &
                          supply_position_eta, &
                          supply_position_i, &
                          supply_position_j, &
                          supply_position_xi_in_cell, &
                          supply_position_eta_in_cell, &
                          is_add_tracer, &
                          is_tracer_movable)

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 投入箇所に問題がなければトレーサーを追加
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (is_add_tracer == 1) then

          ! カウンターを更新
          call increment_integer_value(added_tracer_number, 1)
          call increment_integer_value(tracer%total_tracer_number, 1)
          call increment_integer_value(tracer%tracer_number_in_cell(supply_position_i, supply_position_j), 1)
          call increment_real_value(tracer%Weighted_number_in_cell(supply_position_i, supply_position_j), 1.0d0)

          ! トレーサーの状態を入力
          tracer%tracer_coordinate_xi(tracer%total_tracer_number) = supply_position_xi
          tracer%tracer_coordinate_eta(tracer%total_tracer_number) = supply_position_eta
          tracer%cell_index_i(tracer%total_tracer_number) = supply_position_i
          tracer%cell_index_j(tracer%total_tracer_number) = supply_position_j
          tracer%tracer_coordinate_xi_in_cell(tracer%total_tracer_number) = supply_position_xi_in_cell
          tracer%tracer_coordinate_eta_in_cell(tracer%total_tracer_number) = supply_position_eta_in_cell
          tracer%tracer_weight(tracer%total_tracer_number) = 1.0
          tracer%tracer_generation(tracer%total_tracer_number) = 1
          tracer%is_tracer_movable(tracer%total_tracer_number) = is_tracer_movable
          tracer%is_tracer_traped(tracer%total_tracer_number) = 0
          tracer%is_tracer_invincibl(tracer%total_tracer_number) = 0

          if (tracer%total_tracer_number >= tracer%max_number) return

        end if

      end do
    end do

  end subroutine add_tracer

  !******************************************************************************************
  !> @brief トレーサーを移動させる
  !> @param[inout] tracer  移動するトレーサー構造体 (normal_tracer型)
  !> @param[inout] tracer_tmp  移動後のトレーサーを一時保持する仮トレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine move_tracer(tracer, tracer_tmp)
    !> 移動対象のトレーサーグループ
    type(normal_tracer), intent(inout) :: tracer
    !> 移動対象の仮トレーサーグループ
    type(normal_tracer_tmp), intent(inout) :: tracer_tmp
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !> ξ方向移動後座標
    double precision :: moved_position_xi
    !> η方向移動後座標
    double precision :: moved_position_eta
    !> セル内でのξ方向投入地点座標
    double precision :: moved_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    double precision :: moved_position_eta_in_cell
    !> 移動後のトレーサーのあるセルのインデックス
    integer :: moved_position_i
    !> 移動後のトレーサーのあるセルのインデックス
    integer :: moved_position_j

    !> トレーサー箇所の水深
    double precision :: tracer_point_depth
    !> トレーサー箇所の摩擦速度
    double precision :: tracer_point_u_star
    !> トレーサー箇所のxi方向速度
    double precision :: tracer_point_velocity_xi
    !> トレーサー箇所のeta方向速度
    double precision :: tracer_point_velocity_eta
    !> トレーサー箇所の渦動粘性係数
    double precision :: tracer_point_eddy_viscosity_coefficient
    !> トレーサー箇所の捕獲率
    double precision :: tracer_point_trap_rate
    !> トレーサー箇所のξ方向スケーリング係数
    double precision :: tracer_point_scale_factor_xi
    !> トレーサー箇所のη方向スケーリング係数
    double precision :: tracer_point_scale_factor_eta

    !> トレーサー捕獲判定値
    double precision :: trap_decision_value

    !> トレーサーが動くか
    integer :: is_tracer_movable
    !> トレーサを削除するか
    integer :: is_alive_tracer
    !> トレーサーがトラップされているか
    integer :: is_tracer_traped
    !> トレーサーが無敵か
    integer :: is_tracer_invincibl

    !> ランダムウォークの移動距離の標準偏差
    double precision :: diffusion_std_dev

    ! ボックスミュラーによる席分布乱数用変数
    !> 正規分布乱数
    double precision :: bm_standard_normal_cos
    !> 正規分布乱数
    double precision :: bm_standard_normal_sin

    !> 移動前のトレーサーの個数
    integer :: total_tracer_number_before

    !> 乱数
    real(8)::drand

    ! 一時配列をリセット
    call reset_normal_tracer(tracer_tmp)

    !> 移動前のトレーサー数をセット
    total_tracer_number_before = tracer%total_tracer_number

    do tracer_index = 1, total_tracer_number_before  ! 既存の全てのトレーサーのループ

      ! フラグをリセット
      is_tracer_movable = 1
      is_alive_tracer = 1
      is_tracer_traped = 0
      is_tracer_invincibl = 0

      !==========================================================================================
      ! トレーサーの水深を調べる
      !==========================================================================================
      tracer_point_depth = &
        calculate_scalar_at_tracer_position( &
        depth_node, &
        tracer%cell_index_i(tracer_index), &
        tracer%cell_index_j(tracer_index), &
        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
        tracer%tracer_coordinate_eta_in_cell(tracer_index))

      !==========================================================================================
      ! まずはトレーサーが動けるか、除去されるかをチェック
      !==========================================================================================
      call check_tracer(tracer, &
                        tracer%tracer_coordinate_xi(tracer_index), &
                        tracer%tracer_coordinate_eta(tracer_index), &
                        tracer%cell_index_i(tracer_index), &
                        tracer%cell_index_j(tracer_index), &
                        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
                        tracer%tracer_coordinate_eta_in_cell(tracer_index), &
                        is_alive_tracer, &
                        is_tracer_movable)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トラップによって動けなくなるかをチェック
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

      ! 無敵ではないトラップセルにある動けるトレーサーを対象にする
      if (trap_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) == 1 .and. is_tracer_movable == 1 .and. tracer%is_tracer_invincibl(tracer_index) == 0) then

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 捕獲率の計算
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ! トラップの高さが水深より大きい場合
        if (tracer%trap_wall_height > tracer_point_depth) then

          ! 捕獲率は入力値のまま
          tracer_point_trap_rate = tracer%trap_rate

          ! トラップの高さが水深より小さい場合
        else

          ! 捕獲率は浸水分の高さの割合が反映される
          tracer_point_trap_rate = tracer%trap_wall_height/tracer_point_depth*tracer%trap_rate

        end if

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 捕獲の判定値の乱数を発生
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        trap_decision_value = drand(0)*100

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 捕獲の判定
        ! 判例地が捕獲率よりも小さい場合捕獲される
        ! なお、トラップで動かなくなった場合は除去されない
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        if (tracer_point_trap_rate > trap_decision_value) then
          ! トラップされて二度と動かなくなる
          is_tracer_traped = 1
          is_tracer_movable = 0
        else
          is_tracer_invincibl = 1
        end if

      end if

      !==========================================================================================
      ! この時点で動けるトレーサーは移動を行う
      !==========================================================================================
      if (is_tracer_movable == 1) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! トレーサー地点の流速を計算
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        tracer_point_velocity_xi = &
          calculate_scalar_at_tracer_position( &
          velocity_xi_node, &
          tracer%cell_index_i(tracer_index), &
          tracer%cell_index_j(tracer_index), &
          tracer%tracer_coordinate_xi_in_cell(tracer_index), &
          tracer%tracer_coordinate_eta_in_cell(tracer_index))

        tracer_point_velocity_eta = &
          calculate_scalar_at_tracer_position( &
          velocity_eta_node, &
          tracer%cell_index_i(tracer_index), &
          tracer%cell_index_j(tracer_index), &
          tracer%tracer_coordinate_xi_in_cell(tracer_index), &
          tracer%tracer_coordinate_eta_in_cell(tracer_index))

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! ランダムウォークを考慮
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トレーサー地点の渦動粘性係数を計算
        tracer_point_eddy_viscosity_coefficient = &
          calculate_scalar_at_tracer_position( &
          eddy_viscosity_coefficient_node, &
          tracer%cell_index_i(tracer_index), &
          tracer%cell_index_j(tracer_index), &
          tracer%tracer_coordinate_xi_in_cell(tracer_index), &
          tracer%tracer_coordinate_eta_in_cell(tracer_index))

        ! ランダムウォークによる移動距離の標準偏差
        diffusion_std_dev = sqrt(2*(a_diff*tracer_point_eddy_viscosity_coefficient + b_diff)*time_interval_for_tracking)

        ! 縦方向横方向の正規分布乱数を取得
        call generate_box_muller_random(bm_standard_normal_cos, bm_standard_normal_sin)

        ! トレーサー地点のスケーリング係数を計算
        tracer_point_scale_factor_xi = (scale_factor_xi(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index) + 1) &
                                        *tracer%tracer_coordinate_eta_in_cell(tracer_index) &
                                        + scale_factor_xi(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) &
                                        *(grid_interval_eta - tracer%tracer_coordinate_eta_in_cell(tracer_index))) &
                                       /grid_interval_eta
        tracer_point_scale_factor_eta = (scale_factor_eta(tracer%cell_index_i(tracer_index) + 1, tracer%cell_index_j(tracer_index)) &
                                         *tracer%tracer_coordinate_xi_in_cell(tracer_index) &
                                         + scale_factor_eta(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) &
                                         *(grid_interval_xi - tracer%tracer_coordinate_xi_in_cell(tracer_index)))/grid_interval_xi

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 移動後の座標を計算
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        moved_position_xi = tracer%tracer_coordinate_xi(tracer_index) &
                            + tracer_point_velocity_xi*time_interval_for_tracking &
                            + bm_standard_normal_cos*diffusion_std_dev*tracer_point_scale_factor_xi

        moved_position_eta = tracer%tracer_coordinate_eta(tracer_index) &
                             + tracer_point_velocity_eta*time_interval_for_tracking &
                             + bm_standard_normal_sin*diffusion_std_dev*tracer_point_scale_factor_eta

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 側面の壁では反射する
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (moved_position_eta >= 1.) moved_position_eta = 1.-(moved_position_eta + - 1.)
        if (moved_position_eta <= 0.) moved_position_eta = -moved_position_eta

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 周期境界条件による判定
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (is_periodic_boundary_condition_Tracers == 1) then
          if (moved_position_xi > 1.0) moved_position_xi = moved_position_xi - 1.
          if (moved_position_xi < 0.0) moved_position_xi = moved_position_xi + 1.
        else
          ! 周期境界じゃない場合範囲外のトレーサーは除去
          if (moved_position_xi < 0.0 .or. 1.0 + tolerance < moved_position_xi) is_alive_tracer = 0
          ! 精度の誤差によりちょっぴりはみ出ている場合は修正
          if (1.0 < moved_position_xi .and. moved_position_xi < 1.0 + tolerance) moved_position_xi = 1.0
        end if

      end if

      !==========================================================================================
      ! 除去対象ではないトレーサーであれば移動後の場所のチェック
      !==========================================================================================
      if (is_alive_tracer == 1) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 移動処理後の場所について調べる
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トレーサーが存在するセルのインデックスを取得
        call find_tracer_cell_index(moved_position_xi, moved_position_eta, moved_position_i, moved_position_j, moved_position_xi_in_cell, moved_position_eta_in_cell)
        ! 移動後の場所の条件でチェック
        call check_tracer(tracer, moved_position_xi, moved_position_eta, moved_position_i, moved_position_j, moved_position_xi_in_cell, moved_position_eta_in_cell, is_alive_tracer, is_tracer_movable)

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 除去されないトレーサーのみを仮配列に入力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (is_alive_tracer == 1) then

        ! トレーサーがセルを移動していたら無敵状態解除
        if (tracer%cell_index_i(tracer_index) /= moved_position_i .or. tracer%cell_index_j(tracer_index) /= moved_position_j) is_tracer_invincibl = 0

        ! カウンターを更新
        call increment_integer_value(tracer_tmp%total_tracer_number, 1)
        call increment_integer_value(tracer_tmp%tracer_number_in_cell(moved_position_i, moved_position_j), 1)
        call increment_real_value(tracer_tmp%Weighted_number_in_cell(moved_position_i, moved_position_j), tracer%tracer_weight(tracer_index))

        ! トレーサーの状態を入力
        tracer_tmp%tracer_coordinate_xi(tracer_tmp%total_tracer_number) = moved_position_xi
        tracer_tmp%tracer_coordinate_eta(tracer_tmp%total_tracer_number) = moved_position_eta
        tracer_tmp%cell_index_i(tracer_tmp%total_tracer_number) = moved_position_i
        tracer_tmp%cell_index_j(tracer_tmp%total_tracer_number) = moved_position_j
        tracer_tmp%tracer_coordinate_xi_in_cell(tracer_tmp%total_tracer_number) = moved_position_xi_in_cell
        tracer_tmp%tracer_coordinate_eta_in_cell(tracer_tmp%total_tracer_number) = moved_position_eta_in_cell
        tracer_tmp%tracer_weight(tracer_tmp%total_tracer_number) = tracer%tracer_weight(tracer_index)
        tracer_tmp%tracer_generation(tracer_tmp%total_tracer_number) = tracer%tracer_generation(tracer_index)
        tracer_tmp%is_tracer_movable(tracer_tmp%total_tracer_number) = is_tracer_movable
        tracer_tmp%is_tracer_traped(tracer_tmp%total_tracer_number) = is_tracer_traped
        tracer_tmp%is_tracer_invincibl(tracer_tmp%total_tracer_number) = is_tracer_invincibl

      end if

    end do

    ! 仮配列を元の配列にコピー
    ! これにより除外されるトレーサーがない配列に更新される
    tracer%total_tracer_number = tracer_tmp%total_tracer_number
    tracer%tracer_number_in_cell = tracer_tmp%tracer_number_in_cell
    tracer%Weighted_number_in_cell = tracer_tmp%Weighted_number_in_cell

    tracer%tracer_coordinate_xi = tracer_tmp%tracer_coordinate_xi
    tracer%tracer_coordinate_eta = tracer_tmp%tracer_coordinate_eta
    tracer%cell_index_i = tracer_tmp%cell_index_i
    tracer%cell_index_j = tracer_tmp%cell_index_j
    tracer%tracer_coordinate_xi_in_cell = tracer_tmp%tracer_coordinate_xi_in_cell
    tracer%tracer_coordinate_eta_in_cell = tracer_tmp%tracer_coordinate_eta_in_cell
    tracer%tracer_weight = tracer_tmp%tracer_weight
    tracer%tracer_generation = tracer_tmp%tracer_generation
    tracer%is_tracer_movable = tracer_tmp%is_tracer_movable
    tracer%is_tracer_traped = tracer_tmp%is_tracer_traped
    tracer%is_tracer_invincibl = tracer_tmp%is_tracer_invincibl

  end subroutine move_tracer

  !******************************************************************************************
  !> @brief トレーサーを分裂させる
  !> @param[in]  suffix  トレーサーのタイプを表す文字列（例: "primary", "secondary"）
  !> @param[inout] tracer  移動するトレーサー構造体 (normal_tracer型)
  !> @param[inout] tracer_tmp  分裂で生じたトレーサーを一時保持する仮トレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine clone_tracer(suffix, tracer, tracer_tmp)
    !> トレーサーグループの名前
    character(len=*), intent(in) :: suffix
    !> 移動対象のトレーサーグループ
    type(normal_tracer), intent(inout) :: tracer
    !> 移動対象の仮トレーサーグループ
    type(normal_tracer_tmp), intent(inout) :: tracer_tmp
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !==========================================================================================
    ! 各トレーサーについて
    !==========================================================================================

    do tracer_index = 1, tracer%total_tracer_number

    end do

  end subroutine clone_tracer

  !******************************************************************************************
  !> @brief 空のセルにトレーサーを発生させる
  !> @param[inout] tracer  移動するトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine add_all_empty_cells(tracer)
    !> 移動対象のトレーサーグループ
    type(normal_tracer), intent(inout) :: tracer
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index
    !> セルのインデックス
    integer :: cell_index_i
    !> セルのインデックス
    integer :: cell_index_j

    !> ξ方向投入地点座標
    double precision :: supply_position_xi
    !> η方向投入地点座標
    double precision :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    double precision :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    double precision :: supply_position_eta_in_cell

    !> 投入するかしないか
    integer :: is_add_tracer = 1
    !> トレーサーが動くか
    integer :: is_tracer_movable = 1

    !> 間引き処理のための追加したトレーサー数カウント
    integer :: add_counter

    ! カウンターをリセット
    add_counter = 0

    ! 全格子をチェック
    do cell_index_i = 1, cell_count_i
      do cell_index_j = 1, cell_count_j

        ! セルに1つ以上トレーサーがある場合は次のセルへ
        if (tracer%tracer_number_in_cell(cell_index_i, cell_index_j) >= 1) cycle

        ! インデックスセット
        supply_position_i = cell_index_i
        supply_position_j = cell_index_j

        ! 座標計算
        supply_position_xi = grid_interval_xi*(dble(cell_index_i-1) + 0.5)
        supply_position_eta = grid_interval_eta*(dble(cell_index_j-1) + 0.5)

        ! セル内の座標計算
        supply_position_xi_in_cell = grid_interval_xi/2
        supply_position_eta_in_cell = grid_interval_eta/2

        ! 投入箇所が障害物セルかチェック
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) then
          is_add_tracer = 0
        end if

        ! トレーサー位置の水深、摩擦速度、などのチェック
        call check_tracer(tracer, &
                          supply_position_xi, &
                          supply_position_eta, &
                          supply_position_i, &
                          supply_position_j, &
                          supply_position_xi_in_cell, &
                          supply_position_eta_in_cell, &
                          is_add_tracer, &
                          is_tracer_movable)

        if (is_add_tracer == 1) then

          ! 間引き処理のためのカウンターを更新
          call increment_integer_value(add_counter, 1)

          if (add_counter >= tracer%cloning_reduction_factor) then

            ! カウンターをリセット
            add_counter = 0

            ! カウンターを更新
            call increment_integer_value(tracer%total_tracer_number, 1)
            call increment_integer_value(tracer%tracer_number_in_cell(supply_position_i, supply_position_j), 1)
            call increment_real_value(tracer%Weighted_number_in_cell(supply_position_i, supply_position_j), 1.0d0)

            ! トレーサーの状態を入力
            tracer%tracer_coordinate_xi(tracer%total_tracer_number) = supply_position_xi
            tracer%tracer_coordinate_eta(tracer%total_tracer_number) = supply_position_eta
            tracer%cell_index_i(tracer%total_tracer_number) = supply_position_i
            tracer%cell_index_j(tracer%total_tracer_number) = supply_position_j
            tracer%tracer_coordinate_xi_in_cell(tracer%total_tracer_number) = supply_position_xi_in_cell
            tracer%tracer_coordinate_eta_in_cell(tracer%total_tracer_number) = supply_position_eta_in_cell
            tracer%tracer_weight(tracer%total_tracer_number) = 1.0
            tracer%tracer_generation(tracer%total_tracer_number) = 1
            tracer%is_tracer_movable(tracer%total_tracer_number) = is_tracer_movable
            tracer%is_tracer_traped(tracer%total_tracer_number) = 0
            tracer%is_tracer_invincibl(tracer%total_tracer_number) = 0

          end if

        end if

        ! 全体の最大トレーサー数に達してたらサブルーチン終了
        if (tracer%total_tracer_number >= tracer%max_number) return

      end do
    end do

  end subroutine add_all_empty_cells

  !******************************************************************************************
  !> @brief トレーサー数の統計を計算する
  !> @param[in] time_since_start　出力開始からの経過時間
  !> @param[inout] tracer  トレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine calculate_tracer_statistics(time_since_start, tracer)
    double precision, intent(in) :: time_since_start
    type(normal_tracer), intent(inout) :: tracer
    integer :: tracer_index
    integer :: cell_index_i
    integer :: cell_index_j
    integer :: i, j

    ! カウンターをリセット
    tracer%total_tracer_number_in_cross_section = 0
    tracer%averaged_tracer_number_in_cross_section = 0.0

    ! 横断面毎の個数
    do i = 1, cell_count_i

      tracer%total_tracer_number_in_cross_section(i, :) = sum(tracer%tracer_number_in_cell(i, :))
      tracer%averaged_tracer_number_in_cross_section(i, :) = sum(tracer%tracer_number_in_cell(i, :))/cell_count_j

    end do

    ! 時間積算個数
    ! セル内のトレーサー数に単位時間を掛けた値を積んでいく
    call increment_real_value_array(tracer%time_integrated_tracer_number_in_cell, tracer%tracer_number_in_cell*time_interval_for_tracking)

    ! 時間平均個数
    if (time_since_start > 0.) then ! 分母が0になるので出力は2回目から
      tracer%time_averaged_tracer_number_in_cell = tracer%time_integrated_tracer_number_in_cell/time_since_start
    else    ! 初回は0を出力
      tracer%time_averaged_tracer_number_in_cell = 0.
    end if

  end subroutine calculate_tracer_statistics

  !******************************************************************************************
  !> @brief 通常トレーサーのアウトプットを行う
  !> @param[in]  suffix  トレーサーのタイプを表す文字列（例: "primary", "secondary"）
  !> @param[inout] tracer  出力されるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine write_sol_normal_tracer(suffix, tracer)
    character(len=*), intent(in) :: suffix
    type(normal_tracer), intent(in) :: tracer
    integer :: tracer_index
    double precision, dimension(:), allocatable :: tracer_coordinate_x
    double precision, dimension(:), allocatable :: tracer_coordinate_y

    allocate (tracer_coordinate_x(tracer%total_tracer_number))
    allocate (tracer_coordinate_y(tracer%total_tracer_number))

    write (*, '(a40,i10)') "total "//trim(suffix)//" tracer number : ", tracer%total_tracer_number

    !==========================================================================================
    ! トレーサーの出力
    !==========================================================================================

    ! 出力開始
    call cg_iric_write_sol_particlegroup_groupbegin(cgnsOut, trim(suffix)//" Normal Tracers", is_error)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサーの属性について
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    do tracer_index = 1, tracer%total_tracer_number

      ! トレーサーの物理座標の計算
      call transform_general_to_physical(tracer%cell_index_i(tracer_index) &
                                         , tracer%cell_index_j(tracer_index) &
                                         , tracer%tracer_coordinate_xi_in_cell(tracer_index) &
                                         , tracer%tracer_coordinate_eta_in_cell(tracer_index) &
                                         , tracer_coordinate_x(tracer_index) &
                                         , tracer_coordinate_y(tracer_index))

      ! トレーサーの属性をエクスポート
      call cg_iric_write_sol_particlegroup_pos2d(cgnsOut, tracer_coordinate_x(tracer_index), tracer_coordinate_y(tracer_index), is_error)
      call cg_iric_write_sol_particlegroup_integer(cgnsOut, 'Generation('//trim(suffix)//")", tracer%tracer_generation(tracer_index), is_error)
      call cg_iric_write_sol_particlegroup_real(cgnsOut, 'Tracer Wight('//trim(suffix)//")", tracer%tracer_weight(tracer_index), is_error)

      ! デバッグ用出力
      call cg_iric_write_sol_particlegroup_real(cgnsOut, 'coordinate_x('//trim(suffix)//")", tracer_coordinate_x(tracer_index), is_error)
      call cg_iric_write_sol_particlegroup_real(cgnsOut, 'coordinate_y('//trim(suffix)//")", tracer_coordinate_y(tracer_index), is_error)
      call cg_iric_write_sol_particlegroup_integer(cgnsOut, 'cell_index_i('//trim(suffix)//")", tracer%cell_index_i(tracer_index), is_error)
      call cg_iric_write_sol_particlegroup_integer(cgnsOut, 'cell_index_j('//trim(suffix)//")", tracer%cell_index_j(tracer_index), is_error)

    end do

    deallocate (tracer_coordinate_x)
    deallocate (tracer_coordinate_y)

    ! 出力終了
    call cg_iric_write_sol_particlegroup_groupend(cgnsOut, is_error)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! セルの属性について
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    call cg_iric_write_sol_cell_real(cgnsOut, "Numbers of tracers ("//trim(suffix)//")", real(tracer%tracer_number_in_cell, kind=8), is_error)
    call cg_iric_write_sol_cell_real(cgnsOut, "Weighted numbers of tracers ("//trim(suffix)//")", tracer%Weighted_number_in_cell, is_error)
    call cg_iric_write_sol_cell_real(cgnsOut, "Tracer numbers in each section("//trim(suffix)//")", real(tracer%total_tracer_number_in_cross_section, kind=8), is_error)
    call cg_iric_write_sol_cell_real(cgnsOut, "Cross-sectional averaged tracer numbers("//trim(suffix)//")", tracer%averaged_tracer_number_in_cross_section, is_error)
    call cg_iric_write_sol_cell_real(cgnsOut, "Time-integrated Particle Counts in Cells ("//trim(suffix)//")", tracer%time_integrated_tracer_number_in_cell, is_error)
    call cg_iric_write_sol_cell_real(cgnsOut, "Averaged Particle Counts ("//trim(suffix)//")", tracer%time_averaged_tracer_number_in_cell, is_error)

  end subroutine write_sol_normal_tracer

end module Trace
