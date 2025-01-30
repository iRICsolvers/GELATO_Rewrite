module trace
  use iric
  use common
  use grid
  use result

  implicit none

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 変数の宣言
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  ! 通常トレーサーについて
  !******************************************************************************************

  !> 通常トレーサーの散布開始時間
  real(8):: supply_time_start_normal_tracers = 0.0
  !> 通常トレーサーの散布終了時間
  real(8):: supply_time_end_normal_tracers = 0.0
  !> 通常トレーサーの散布時間間隔
  real(8):: supply_time_interval_normal_tracers = 99999.0
  !> 通常トレーサーの周期境界条件
  integer:: is_periodic_boundary_condition_Tracers
  !> 停止したトレーサーの扱い 1:その場で留まる 2:除去する
  integer:: stopped_tracer_handling
  !> トレーサー追加のタイムカウンター
  real(8) :: time_counter_add_normal_tracer

  type :: tracer_base

    !==========================================================================================
    ! トレーサーの共通パラメータ
    !==========================================================================================
    !------------------------------------------------------------------------------------------
    ! GUIから読み込む値
    !------------------------------------------------------------------------------------------
    !> 移動限界水深
    real(8):: Movable_Critical_depth
    !> 移動限界摩擦速度
    real(8):: Movable_Critical_u_star
    !> トレーサー補足壁の高さ
    real(8):: trap_wall_height
    !> トレーサー捕捉率
    real(8):: trap_rate
    !> ξ方向配置始点
    real(8):: supply_position_xi_first
    !> ξ方向配置終点
    real(8):: supply_position_xi_end
    !> ξ方向配置間隔
    real(8):: supply_interval_xi
    !> η方向配置始点
    real(8):: supply_position_eta_first
    !> η方向配置終点
    real(8):: supply_position_eta_end
    !> η方向配置間隔
    real(8):: supply_interval_eta

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサーの属性
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> トレーサーのξ座標
    real(8), dimension(:), allocatable :: tracer_coordinate_xi
    !> トレーサーのη座標
    real(8), dimension(:), allocatable :: tracer_coordinate_eta
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_i
    !> トレーサーのあるセルインデックス
    integer, dimension(:), allocatable :: cell_index_j
    !> セル内でのトレーサーのξ座標
    real(8), dimension(:), allocatable :: tracer_coordinate_xi_in_cell
    !> セル内でのトレーサーのη座標
    real(8), dimension(:), allocatable :: tracer_coordinate_eta_in_cell
    !> トレーサーが動くことができるか
    integer, dimension(:), allocatable :: is_tracer_movable
    !> トレーサーがトラップにとらわれているか
    integer, dimension(:), allocatable :: is_tracer_trapped
    !> トレーサーが無敵状態か
    integer, dimension(:), allocatable :: is_tracer_invincible
    !> トレーサーが生き残るか
    integer, dimension(:), allocatable :: is_tracer_arrived

    !> トレーサーの総数
    integer :: total_tracer_number

  end type tracer_base

  !> 通常トレーサーの構造体
  type, extends(tracer_base) :: normal_tracer

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
    !> トレーサーの重み
    real(8), dimension(:), allocatable :: tracer_weight
    !> トレーサーの世代
    integer, dimension(:), allocatable :: tracer_generation

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! !トレーサー数の統計
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> セル内のトレーサー総数
    integer, dimension(:, :), allocatable :: tracer_number_in_cell
    !>セル内の重み付きトレーサー数
    real(8), dimension(:, :), allocatable ::  Weighted_number_in_cell
    !> セル内のトレーサー総数のi断面合計
    integer, dimension(:, :), allocatable :: total_tracer_number_in_cross_section
    !> セル内のトレーサー総数のi断面平均
    real(8), dimension(:, :), allocatable :: averaged_tracer_number_in_cross_section
    !> セル内の時間積算トレーサー数
    real(8), dimension(:, :), allocatable ::  time_integrated_tracer_number_in_cell
    !> セル内の時間平均トレーサー数
    real(8), dimension(:, :), allocatable ::  time_averaged_tracer_number_in_cell

  end type normal_tracer

  !> プライマートレーサーのパラメータ群
  type(normal_tracer) :: primary
  !> セカンダリートレーサーのパラメータ群
  type(normal_tracer) :: secondary

  !******************************************************************************************
  ! 軌跡追跡トレーサー
  !******************************************************************************************

  type, extends(tracer_base) :: trajectory_tracer

    !==========================================================================================
    ! 軌跡追跡トレーサー
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! GUIから読み込む値
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> 軌跡追跡トレーサーの最大数
    integer:: max_number
    !> 軌跡の最大保存回数
    integer:: max_save_times
    !> 軌跡の保存回数カウンター
    integer:: save_counter
    !> 軌跡の保存時間間隔
    integer:: save_interval
    !> 軌跡を保存してからの移動回数タイマー
    integer:: save_timer
    !> 軌跡追跡トレーサーを既に追加したか
    integer:: is_added_trajectory_tracer

    !> 軌跡追跡トレーサーの追加時間
    real(8) :: supply_time

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサーの属性
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> 軌跡のx座標
    real(8), dimension(:, :), allocatable :: trajectory_coordinate_x
    !> 軌跡のy座標
    real(8), dimension(:, :), allocatable :: trajectory_coordinate_y
    !> 保存された軌跡を構成する点の数
    integer, dimension(:), allocatable :: seved_trajectory_point_number

  end type trajectory_tracer

  type(trajectory_tracer) :: trajectory

  !******************************************************************************************
  ! windmp風トレーサー
  !******************************************************************************************

  type:: windmap_tracer

    !==========================================================================================
    ! windmapトレーサー
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! GUIから読み込む値
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    !> windmapラインの最大数
    integer:: max_number
    !> windmapラインの最大保存回数
    integer:: max_save_times
    !> windmapラインの寿命(時間)
    real(8):: life_time
    !> windmapラインの保存間隔
    real(8):: line_save_interval

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサーの属性
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !> トレーサーのξ座標
    real(8), dimension(:), allocatable :: tracer_coordinate_xi
    !> トレーサーのη座標
    real(8), dimension(:), allocatable :: tracer_coordinate_eta

    !> windmapラインを何回保存したか
    integer, dimension(:), allocatable :: windmap_save_times
    !> windmapラインの保存用タイマー
    real(8), dimension(:), allocatable :: windmap_save_timer

    !> windmapラインのx座標
    real(8), dimension(:, :), allocatable :: windmap_coordinate_x
    !> windmapラインのy座標
    real(8), dimension(:, :), allocatable :: windmap_coordinate_y
    !> windmapラインの長さ
    real(8), dimension(:, :), allocatable :: windmap_line_length

  end type windmap_tracer

  type(windmap_tracer) :: windmap

contains

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 汎用的なサブルーチン
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  !> @brief トレーサーが位置するセルのインデックスを探査してセル内での座標も計算する
  !> @param[in] tracer_coordinate_xi トレーサーのξ座標
  !> @param[in] tracer_coordinate_eta トレーサーのη座標
  !> @param[inout] cell_index_i トレーサーの存在するセルのi方向インデックス
  !> @param[inout] cell_index_j トレーサーの存在するセルのi方向インデックス
  !> @param[inout, optional] tracer_coordinate_xi_in_cell セル内でのトレーサーのξ座標
  !> @param[inout, optional] tracer_coordinate_eta_in_cell セル内でのトレーサーのη座標
  !******************************************************************************************
  subroutine find_tracer_cell_index(tracer_coordinate_xi, tracer_coordinate_eta, cell_index_i, cell_index_j, tracer_coordinate_xi_in_cell, tracer_coordinate_eta_in_cell)
    real(8), intent(in) :: tracer_coordinate_xi
    real(8), intent(in) :: tracer_coordinate_eta
    integer, intent(inout) :: cell_index_i
    integer, intent(inout) :: cell_index_j
    double precision, intent(inout) :: tracer_coordinate_xi_in_cell
    double precision, intent(inout) :: tracer_coordinate_eta_in_cell

    cell_index_i = int(tracer_coordinate_xi/grid_interval_xi) + 1
    cell_index_j = int(tracer_coordinate_eta/grid_interval_eta) + 1

    ! 例外処理
    ! grid_interval_xi, grid_interval_etaを求める際に誤差により1付近のトレーサーのインデックスが範囲外になることがある
    ! そのため、インデックスが最大値を超えた場合のインデックスを修正する
    ! これにより格子範囲外のトレーサーでも格子範囲内に収まることになってしまうが、トレーサーの移動の際に範囲外のトレーサーは除去されるため問題ない
    if (cell_index_i > cell_count_i) cell_index_i = cell_count_i
    if (cell_index_j > cell_count_j) cell_index_j = cell_count_j
    ! 浮動小数点による誤差を考慮して、トレーサーが格子点の境界に位置する場合の処理
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
    real(8), intent(in), dimension(:, :) :: scalar
    integer, intent(in) :: cell_index_i
    integer, intent(in) :: cell_index_j
    real(8) :: tracer_coordinate_xi_in_cell
    real(8) :: tracer_coordinate_eta_in_cell
    real(8) :: point
    !> ξ方向で上側の2つの値(i,j+1),(i+1,j+1)の補間結果
    real(8) :: interpolated_xi_top
    !> ξ方向で下側の2つの値(i,j),(i+1,j)の補間結果
    real(8) :: interpolated_xi_bottom

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
    real(8), intent(in) :: tracer_coordinate_xi_in_cell
    real(8), intent(in) :: tracer_coordinate_eta_in_cell
    real(8), intent(inout) :: tracer_coordinate_x
    real(8), intent(inout) :: tracer_coordinate_y

    ! 物理座標x、yを計算
    tracer_coordinate_x = node_coordinate_x(cell_index_i, cell_index_j) &
                          + x_gradient_in_xi(cell_index_i, cell_index_j)*tracer_coordinate_xi_in_cell &
                          + x_gradient_in_eta(cell_index_i, cell_index_j)*tracer_coordinate_eta_in_cell
    tracer_coordinate_y = node_coordinate_y(cell_index_i, cell_index_j) &
                          + y_gradient_in_xi(cell_index_i, cell_index_j)*tracer_coordinate_xi_in_cell &
                          + y_gradient_in_eta(cell_index_i, cell_index_j)*tracer_coordinate_eta_in_cell

  end subroutine transform_general_to_physical

  ! TODO: サブルーチンにすることで計算が重くなる可能性があるので見直す。
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
    real(8), intent(inout) :: value   ! 入力および出力: もとの整数値と増加後の値
    real(8), intent(in) :: add_value

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
    real(8), intent(inout) :: array(:, :)   ! 入力および出力: もとの整数値と増加後の値
    real(8), intent(in) :: add_array(:, :)

    ! 値を増やす
    array = array + add_array

  end subroutine increment_real_value_array

  !******************************************************************************************
  !> @brief   トレーサーの数を更新するサブルーチン
  !> @param[inout] tracer　トレーサーの位置、速度、重み、世代などの情報を含む構造体
  !******************************************************************************************
  subroutine update_tracer_count(tracer, tracer_index)
    implicit none
    type(normal_tracer), intent(inout) :: tracer
    integer, intent(in) :: tracer_index

    call increment_integer_value(tracer%total_tracer_number, 1)
    call increment_integer_value(tracer%tracer_number_in_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)), 1)
    call increment_real_value(tracer%Weighted_number_in_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)), &
                              tracer%tracer_weight(tracer_index))

  end subroutine update_tracer_count

  !******************************************************************************************
  !> @brief ボックスミュラーによる正規分布乱数を発生させる
  !> @param[inout] z0 正規分布乱数(cos成分)
  !> @param[inout] z1 正規分布乱数(sin成分)
  !******************************************************************************************
  subroutine generate_box_muller_random(z0, z1)
    implicit none
    real(8) :: u1
    real(8) :: u2
    real(8), intent(inout) :: z0
    real(8), intent(inout) :: z1

    call random_number(u1)
    call random_number(u2)
    z0 = sqrt(-2.*log(u1))*cos(2.*pi*u2)
    z1 = sqrt(-2.*log(u1))*sin(2.*pi*u2)

  end subroutine generate_box_muller_random

  !******************************************************************************************
  !> @brief トレーサー位置のスカラーから追加や移動をさせるかチェックする
  !> @param[in] movable_critical_depth 移動限界水深
  !> @param[in] movable_critical_u_star 移動限界摩擦速度
  !> @param[in] tracer_coordinate_xi トレーサーのξ方向座標
  !> @param[in] tracer_coordinate_eta トレーサーのη方向座標
  !> @param[in] cell_index_i トレーサー位置のセルインデックス
  !> @param[in] cell_index_j トレーサー位置のセルインデックス
  !> @param[in] tracer_coordinate_xi_in_cell セル内でのξ方向投入地点座標
  !> @param[in] tracer_coordinate_eta_in_cell セル内でのη方向投入地点座標
  !> @param[inout] is_add_tracer トレーサーの追加、除去の対象か
  !> @param[inout] is_tracer_movable トレーサーが動くか
  ! メモ：障害物に関するチェックは移動と追加で扱いが異なるのでこの中では行わない
  !******************************************************************************************
  subroutine check_tracer(Movable_Critical_depth, Movable_Critical_u_star, tracer_coordinate_xi, tracer_coordinate_eta, cell_index_i, cell_index_j, tracer_coordinate_xi_in_cell, tracer_coordinate_eta_in_cell, is_add_tracer, is_tracer_movable)
    !> 移動限界水深
    real(8) :: Movable_Critical_depth
    !> 移動限界摩擦速度
    real(8) :: Movable_Critical_u_star
    !> ξ方向トレーサー座標
    real(8), intent(in) :: tracer_coordinate_xi
    !> η方向トレーサー座標
    real(8), intent(in) :: tracer_coordinate_eta
    !> 投入地点のセルインデックス
    integer, intent(in) :: cell_index_i
    !> 投入地点のセルインデックス
    integer, intent(in) :: cell_index_j
    !> セル内での移動後のξ方向座標
    real(8), intent(in) :: tracer_coordinate_xi_in_cell
    !> セル内での移動後のη方向座標
    real(8), intent(in) :: tracer_coordinate_eta_in_cell
    !> 投入地点の水深
    real(8) :: tracer_point_depth
    !> 投入地点の摩擦速度
    real(8) :: tracer_point_u_star

    !> 投入するかしないか
    integer, intent(inout) :: is_add_tracer
    !> トレーサーが動くか
    integer, intent(inout) :: is_tracer_movable

    ! 投入地点の水深と摩擦速度を調べる
    tracer_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                             cell_index_i, &
                                                             cell_index_j, &
                                                             tracer_coordinate_xi_in_cell, &
                                                             tracer_coordinate_eta_in_cell)
    tracer_point_u_star = calculate_scalar_at_tracer_position(u_star_node, &
                                                              cell_index_i, &
                                                              cell_index_j, &
                                                              tracer_coordinate_xi_in_cell, &
                                                              tracer_coordinate_eta_in_cell)

    ! 投入箇所の水深が移動可能水深以上かつ停止したトレーサーを除去するかチェック
    if (Movable_Critical_depth > tracer_point_depth) then

      is_tracer_movable = 0

      if (stopped_tracer_handling == 2) then
        is_add_tracer = 0
      end if

    end if

    ! 投入箇所の摩擦速度が移動可能水深以上かつ停止したトレーサーを除去するかチェック
    if (Movable_Critical_u_star > tracer_point_u_star) then

      is_tracer_movable = 0

      if (stopped_tracer_handling == 2) then
        is_add_tracer = 0
      end if

    end if

  end subroutine check_tracer

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 通常トレーサーのサブルーチン
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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

    !==========================================================================================
    ! セカンダリートレーサー
    !==========================================================================================
    if (is_trace_secondary) call Initialize_Normal_Tracer_type("secondary", secondary)

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
    allocate (tracer%is_tracer_trapped(tracer%max_number))
    allocate (tracer%is_tracer_invincible(tracer%max_number))
    allocate (tracer%is_tracer_arrived(tracer%max_number))

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
    tracer%is_tracer_trapped = 0
    tracer%is_tracer_invincible = 0
    tracer%is_tracer_arrived = 0

    tracer%total_tracer_number = 0

    tracer%tracer_number_in_cell = 0
    tracer%Weighted_number_in_cell = 0.0
    tracer%total_tracer_number_in_cross_section = 0
    tracer%averaged_tracer_number_in_cross_section = 0.0
    tracer%time_integrated_tracer_number_in_cell = 0.0
    tracer%time_averaged_tracer_number_in_cell = 0.0

  end subroutine Initialize_Normal_Tracer_type

  !******************************************************************************************
  !> @brief 通常トレーサーを追加する
  !> @param[inout] tracer  初期化されるトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine add_normal_tracer(tracer)

    type(normal_tracer), intent(inout) :: tracer

    !> ξ方向投入地点座標
    real(8) :: supply_position_xi
    !> η方向投入地点座標
    real(8) :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    real(8) :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    real(8) :: supply_position_eta_in_cell

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

        ! 範囲外ならループ終了
        if (supply_position_eta > tracer%supply_position_eta_end + tolerance) cycle

        ! 誤差範囲内なら誤差を補正
        if (supply_position_eta > tracer%supply_position_eta_end) supply_position_eta = tracer%supply_position_eta_end

        ! 最大数に達していたら終了
        if (tracer%total_tracer_number >= tracer%max_number) return

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 投入箇所に投入できるかチェック
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! フラグの初期化
        is_add_tracer = 1
        is_tracer_movable = 1

        ! 投入地点のセルのインデックスを調べる
        call find_tracer_cell_index(supply_position_xi, supply_position_eta, supply_position_i, supply_position_j, supply_position_xi_in_cell, supply_position_eta_in_cell)

        ! 投入箇所が障害物セルかチェック
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) cycle

        ! 投入箇所セル内の格子の数が最大数に達しているかチェック
        if (tracer%tracer_number_in_cell(supply_position_i, supply_position_j) >= tracer%max_number_in_cell) cycle

        ! 投入箇所の水深と摩擦速度を調べる
        call check_tracer(tracer%Movable_Critical_depth, &
                          tracer%Movable_Critical_u_star, &
                          supply_position_xi, &
                          supply_position_eta, &
                          supply_position_i, &
                          supply_position_j, &
                          supply_position_xi_in_cell, &
                          supply_position_eta_in_cell, &
                          is_add_tracer, &
                          is_tracer_movable)

        if (is_add_tracer == 0) cycle
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 投入箇所に問題がなければトレーサーを追加
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
        tracer%is_tracer_trapped(tracer%total_tracer_number) = 0
        tracer%is_tracer_invincible(tracer%total_tracer_number) = 0
        tracer%is_tracer_arrived(tracer%total_tracer_number) = 1

      end do
    end do

  end subroutine add_normal_tracer

  !******************************************************************************************
  !> @brief 通常トレーサーを移動させる
  !> @param[inout] tracer  移動するトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine move_normal_tracer(tracer)
    !> 移動するトレーサー構造体
    type(normal_tracer), intent(inout) :: tracer
    !> トレーサーのインデックス
    integer :: tracer_index

    !> 既存の全てのトレーサーの総数
    integer :: total_tracer_number_before

    !> 移動後のξ方向座標
    real(8) :: moved_position_xi
    !> 移動後のη方向座標
    real(8) :: moved_position_eta
    !> 移動後のセル内でのξ方向座標
    real(8) :: moved_position_xi_in_cell
    !> 移動後のセル内でのη方向座標
    real(8) :: moved_position_eta_in_cell
    !> 移動後のセルのj方向インデックス
    integer :: moved_position_i
    !> 移動後のトレーサーのあるセルのインデックス
    integer :: moved_position_j

    !> トレーサー箇所の水深
    real(8) :: tracer_point_depth
    !> トレーサー箇所の摩擦速度
    real(8) :: tracer_point_u_star
    !> トレーサー箇所のxi方向速度
    real(8) :: tracer_point_velocity_xi
    !> トレーサー箇所のeta方向速度
    real(8) :: tracer_point_velocity_eta
    !> トレーサー箇所の渦動粘性係数
    real(8) :: tracer_point_eddy_viscosity_coefficient
    !> トレーサー箇所の捕獲率
    real(8) :: tracer_point_trap_rate
    !> トレーサー箇所のξ方向スケーリング係数
    real(8) :: tracer_point_scale_factor_xi
    !> トレーサー箇所のη方向スケーリング係数
    real(8) :: tracer_point_scale_factor_eta

    !> トレーサー捕獲判定値
    real(8) :: trap_decision_value

    !> ランダムウォークの移動距離の標準偏差
    real(8) :: diffusion_std_dev

    ! ボックスミュラーによる正規分布乱数用変数
    !> 正規分布乱数
    real(8) :: bm_standard_normal_cos
    !> 正規分布乱数
    real(8) :: bm_standard_normal_sin

    !==========================================================================================
    ! トレーサーの総数をリセット
    !==========================================================================================
    total_tracer_number_before = tracer%total_tracer_number
    tracer%tracer_number_in_cell = 0
    tracer%total_tracer_number = 0
    tracer%Weighted_number_in_cell = 0.0

    do tracer_index = 1, total_tracer_number_before  ! 既存の全てのトレーサーのループ

      !==========================================================================================
      ! 移動しないトレーサーはここで抜ける
      !==========================================================================================

      ! 除去対象のトレーサーはスキップ
      ! TODO: ここのチェックは不要かもしれない
      if (tracer%is_tracer_arrived(tracer_index) == 0) then
        cycle
      end if

      ! トラップされたトレーサーは移動しない
      if (tracer%is_tracer_trapped(tracer_index) == 1) then
        ! 移動しないのでトレーサーの個数を更新して次のトレーサーへ
        call update_tracer_count(tracer, tracer_index)
        cycle
      end if

      !==========================================================================================
      ! まずはトレーサーが動けるか、除去されるかをチェック
      !==========================================================================================

      ! フラグのリセット
      tracer%is_tracer_movable(tracer_index) = 1

      ! 水深と摩擦速度によるチェック
      call check_tracer(tracer%Movable_Critical_depth, &
                        tracer%Movable_Critical_u_star, &
                        tracer%tracer_coordinate_xi(tracer_index), &
                        tracer%tracer_coordinate_eta(tracer_index), &
                        tracer%cell_index_i(tracer_index), &
                        tracer%cell_index_j(tracer_index), &
                        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
                        tracer%tracer_coordinate_eta_in_cell(tracer_index), &
                        tracer%is_tracer_arrived(tracer_index), &
                        tracer%is_tracer_movable(tracer_index))

      ! 除去対象のトレーサーはスキップ
      if (tracer%is_tracer_arrived(tracer_index) == 0) then
        cycle
      end if

      ! 水深や摩擦速度が移動限界より小さい場合は移動しないし、トラップされない
      ! 移動しないのでトレーサーの個数を更新して次のトレーサーへ
      if (tracer%is_tracer_movable(tracer_index) == 0) then
        call update_tracer_count(tracer, tracer_index)
        cycle
      end if

      !==========================================================================================
      ! トラップによって動けなくなるかをチェック
      !==========================================================================================
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トレーサーの水深を調べる
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      tracer_point_depth = &
        calculate_scalar_at_tracer_position( &
        depth_node, &
        tracer%cell_index_i(tracer_index), &
        tracer%cell_index_j(tracer_index), &
        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
        tracer%tracer_coordinate_eta_in_cell(tracer_index))

      ! 無敵ではないトラップセルにある動けるトレーサーを対象にする
      if (trap_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) == 1 .and. tracer%is_tracer_movable(tracer_index) == 1 .and. tracer%is_tracer_invincible(tracer_index) == 0) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲率の計算
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トラップの高さが水深より大きい場合
        if (tracer%trap_wall_height > tracer_point_depth) then

          ! 捕獲率は入力値のまま
          tracer_point_trap_rate = tracer%trap_rate

        else  ! トラップの高さが水深より小さい場合

          ! 捕獲率は浸水分の高さの割合が反映される
          tracer_point_trap_rate = tracer%trap_wall_height/tracer_point_depth*tracer%trap_rate

        end if

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲の判定値の乱数を発生
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        call random_number(trap_decision_value)
        trap_decision_value = trap_decision_value*100

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲の判定
        ! 判定値が捕獲率よりも小さい場合捕獲される
        ! なお、トラップで動かなくなった場合は除去されない
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        if (tracer_point_trap_rate > trap_decision_value) then
          ! トラップされて二度と動かなくなる
          tracer%is_tracer_trapped(tracer_index) = 1
          tracer%is_tracer_movable(tracer_index) = 0
        else
          tracer%is_tracer_invincible(tracer_index) = 1
        end if

      end if

      ! トラップされたトレーサーは移動しない
      if (tracer%is_tracer_trapped(tracer_index) == 1) then
        ! 移動しないのでトレーサーの個数を更新して次のトレーサーへ
        call update_tracer_count(tracer, tracer_index)
        cycle
      end if

      !==========================================================================================
      ! この時点で動けるトレーサーは移動を行う
      !==========================================================================================
      if (tracer%is_tracer_movable(tracer_index) == 1) then

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
        if (moved_position_eta >= 1.) moved_position_eta = 1.-(moved_position_eta - 1.)
        if (moved_position_eta <= 0.) moved_position_eta = -moved_position_eta

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 上下流端の周期境界条件による判定
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (is_periodic_boundary_condition_Tracers == 1) then
          ! 周期境界条件の場合は範囲外のトレーサーを移動
          if (moved_position_xi > 1.0) moved_position_xi = moved_position_xi - 1.
          if (moved_position_xi < 0.0) moved_position_xi = moved_position_xi + 1.
        else
          ! 周期境界じゃない場合範囲外のトレーサーは除去
          if (moved_position_xi < 0.0 .or. 1.0 + tolerance < moved_position_xi) then
            tracer%is_tracer_arrived(tracer_index) = 0
            cycle
          end if
          ! 精度の誤差によりちょっぴりはみ出ている場合は修正
          if (1.0 < moved_position_xi .and. moved_position_xi < 1.0 + tolerance) then
            moved_position_xi = 1.0
          end if
        end if

      end if

      !==========================================================================================
      ! 除去対象ではないトレーサーであれば移動後の場所のチェック
      !==========================================================================================
      if (tracer%is_tracer_arrived(tracer_index) == 1) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 移動処理後の場所について調べる
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トレーサーが存在するセルのインデックスを取得
        call find_tracer_cell_index( &
          moved_position_xi, &
          moved_position_eta, &
          moved_position_i, &
          moved_position_j, &
          moved_position_xi_in_cell, &
          moved_position_eta_in_cell)

        ! 移動後の場所の条件でチェック
        call check_tracer(tracer%Movable_Critical_depth, &
                          tracer%Movable_Critical_u_star, &
                          moved_position_xi, &
                          moved_position_eta, &
                          moved_position_i, &
                          moved_position_j, &
                          moved_position_xi_in_cell, &
                          moved_position_eta_in_cell, &
                          tracer%is_tracer_arrived(tracer_index), &
                          tracer%is_tracer_movable(tracer_index))

        ! 移動後のセルで最大数に達しているかチェック
        if (tracer%tracer_number_in_cell(moved_position_i, moved_position_j) >= tracer%max_number_in_cell) then
          tracer%is_tracer_arrived(tracer_index) = 0
          cycle
        end if

        ! トレーサーがセルを移動していたら無敵状態解除
        if (tracer%cell_index_i(tracer_index) /= moved_position_i .or. tracer%cell_index_j(tracer_index) /= moved_position_j) then
          tracer%is_tracer_invincible(tracer_index) = 0
        end if

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動して、かつ除去されないトレーサーの処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (tracer%is_tracer_arrived(tracer_index) == 1) then

        ! トレーサーの座標を更新
        tracer%tracer_coordinate_xi(tracer_index) = moved_position_xi
        tracer%tracer_coordinate_eta(tracer_index) = moved_position_eta
        tracer%cell_index_i(tracer_index) = moved_position_i
        tracer%cell_index_j(tracer_index) = moved_position_j
        tracer%tracer_coordinate_xi_in_cell(tracer_index) = moved_position_xi_in_cell
        tracer%tracer_coordinate_eta_in_cell(tracer_index) = moved_position_eta_in_cell

        ! カウンターを更新
        call update_tracer_count(tracer, tracer_index)
      end if

    end do

    !==========================================================================================
    ! 生き残るトレーサーのみを残す
    !==========================================================================================

    call remove_dead_tracer(tracer, total_tracer_number_before)

  end subroutine move_normal_tracer

  !******************************************************************************************
  !> @brief 生きているトレーサーだけを残すサブルーチン
  !> @param[inout] tracer  トレーサーの位置、速度、重み、世代などの情報を含む構造体
  !> @param[in] total_tracer_number_before  削除する前のトレーサーの総数
  !******************************************************************************************
  subroutine remove_dead_tracer(tracer, total_tracer_number_before)
    implicit none
    !> トレーサーの位置、速度、重み、世代などの情報を含む構造体
    type(normal_tracer), intent(inout) :: tracer
    !> 削除する前のトレーサーの総数
    integer, intent(in) :: total_tracer_number_before
    !> トレーサーのインデックス
    integer :: tracer_index
    !> 生存しているトレーサーのインデックス
    integer :: alive_tracer_index

    ! 生存しているトレーサーのインデックスを初期化
    alive_tracer_index = 0

    ! 全てのトレーサーをループして生存しているものを見つける
    do tracer_index = 1, total_tracer_number_before
      if (tracer%is_tracer_arrived(tracer_index) == 1) then
        ! 生存しているトレーサーのインデックスを増加
        alive_tracer_index = alive_tracer_index + 1
        ! 生存しているトレーサーのプロパティを新しい位置にコピー
        tracer%tracer_coordinate_xi(alive_tracer_index) = tracer%tracer_coordinate_xi(tracer_index)
        tracer%tracer_coordinate_eta(alive_tracer_index) = tracer%tracer_coordinate_eta(tracer_index)
        tracer%cell_index_i(alive_tracer_index) = tracer%cell_index_i(tracer_index)
        tracer%cell_index_j(alive_tracer_index) = tracer%cell_index_j(tracer_index)
        tracer%tracer_coordinate_xi_in_cell(alive_tracer_index) = tracer%tracer_coordinate_xi_in_cell(tracer_index)
        tracer%tracer_coordinate_eta_in_cell(alive_tracer_index) = tracer%tracer_coordinate_eta_in_cell(tracer_index)
        tracer%tracer_weight(alive_tracer_index) = tracer%tracer_weight(tracer_index)
        tracer%tracer_generation(alive_tracer_index) = tracer%tracer_generation(tracer_index)
        tracer%is_tracer_movable(alive_tracer_index) = tracer%is_tracer_movable(tracer_index)
        tracer%is_tracer_trapped(alive_tracer_index) = tracer%is_tracer_trapped(tracer_index)
        tracer%is_tracer_invincible(alive_tracer_index) = tracer%is_tracer_invincible(tracer_index)
        tracer%is_tracer_arrived(alive_tracer_index) = tracer%is_tracer_arrived(tracer_index)
      end if
    end do

    ! 残りのトレーサーのプロパティをリセット
    do tracer_index = alive_tracer_index + 1, total_tracer_number_before
      tracer%tracer_coordinate_xi(tracer_index) = 0.0
      tracer%tracer_coordinate_eta(tracer_index) = 0.0
      tracer%cell_index_i(tracer_index) = 0
      tracer%cell_index_j(tracer_index) = 0
      tracer%tracer_coordinate_xi_in_cell(tracer_index) = 0.0
      tracer%tracer_coordinate_eta_in_cell(tracer_index) = 0.0
      tracer%tracer_weight(tracer_index) = 0.0
      tracer%tracer_generation(tracer_index) = 0
      tracer%is_tracer_movable(tracer_index) = 0
      tracer%is_tracer_trapped(tracer_index) = 0
      tracer%is_tracer_invincible(tracer_index) = 0
      tracer%is_tracer_arrived(tracer_index) = 0
    end do

  end subroutine remove_dead_tracer

  !******************************************************************************************
  !> @brief トレーサーを分裂させる
  !> @param[inout] tracer  分裂するトレーサー構造体 (normal_tracer型)
  !******************************************************************************************
  subroutine clone_tracer(tracer)
    !> 移動対象のトレーサーグループ
    type(normal_tracer), intent(inout) :: tracer
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index
    !> 分裂したトレーサーの数
    integer :: cloning_counter

    cloning_counter = 0

    !==========================================================================================
    ! 各トレーサーについて
    !==========================================================================================
    do tracer_index = 1, tracer%total_tracer_number

      !全体のセルの数が最大数に達している場合は分裂しないのでサブルーチンを抜ける
      if (tracer%total_tracer_number + cloning_counter >= tracer%max_number) then
        return
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トレーサーの分裂条件をチェック
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! cloning_option=2の場合は、指定されたセルでのみ分裂する
      if (tracer%cloning_option == 2) then
        if (is_cloning_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) == 0) then
          cycle ! 分裂しない場合は次のトレーサーへ
        end if
      end if

      ! 動かないトレーサーは分裂しない
      if (tracer%is_tracer_movable(tracer_index) == 0 .or. tracer%is_tracer_trapped(tracer_index) == 1) then
        cycle ! 分裂しない場合は次のトレーサーへ
      end if

      ! セル内に2個以上トレーサーがある場合は分裂しない
      if (tracer%tracer_number_in_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) >= 2) then
        cycle ! 分裂しない場合は次のトレーサーへ
      end if

      ! 分裂回数が最大回数を超えている場合は分裂しない
      if (tracer%tracer_generation(tracer_index) >= tracer%max_generation) then
        cycle ! 分裂しない場合は次のトレーサーへ
      end if

      call increment_integer_value(cloning_counter, 1)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 既存トレーサーの分裂処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      tracer%tracer_weight(tracer_index) = tracer%tracer_weight(tracer_index)/2
      call increment_integer_value(tracer%tracer_generation(tracer_index), 1)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 分裂して追加されたトレーサーの処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call increment_integer_value(tracer%tracer_number_in_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)), 1)

      tracer%tracer_coordinate_xi(tracer%total_tracer_number + cloning_counter) = tracer%tracer_coordinate_xi(tracer_index)
      tracer%tracer_coordinate_eta(tracer%total_tracer_number + cloning_counter) = tracer%tracer_coordinate_eta(tracer_index)
      tracer%cell_index_i(tracer%total_tracer_number + cloning_counter) = tracer%cell_index_i(tracer_index)
      tracer%cell_index_j(tracer%total_tracer_number + cloning_counter) = tracer%cell_index_j(tracer_index)
      tracer%tracer_coordinate_xi_in_cell(tracer%total_tracer_number + cloning_counter) = tracer%tracer_coordinate_xi_in_cell(tracer_index)
      tracer%tracer_coordinate_eta_in_cell(tracer%total_tracer_number + cloning_counter) = tracer%tracer_coordinate_eta_in_cell(tracer_index)
      tracer%tracer_weight(tracer%total_tracer_number + cloning_counter) = tracer%tracer_weight(tracer_index)
      tracer%tracer_generation(tracer%total_tracer_number + cloning_counter) = tracer%tracer_generation(tracer_index)
      tracer%is_tracer_movable(tracer%total_tracer_number + cloning_counter) = tracer%is_tracer_movable(tracer_index)
      tracer%is_tracer_trapped(tracer%total_tracer_number + cloning_counter) = 0
      tracer%is_tracer_invincible(tracer%total_tracer_number + cloning_counter) = tracer%is_tracer_invincible(tracer_index)
      tracer%is_tracer_arrived(tracer%total_tracer_number + cloning_counter) = 1

    end do

    ! ループ内で加算すると追加されたトレーサーまで処理されてしまうので、最後に加算
    tracer%total_tracer_number = tracer%total_tracer_number + cloning_counter

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
    real(8) :: supply_position_xi
    !> η方向投入地点座標
    real(8) :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    real(8) :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    real(8) :: supply_position_eta_in_cell

    !> 投入するかしないか
    integer :: is_add_tracer = 1
    !> トレーサーが動くか
    integer :: is_tracer_movable = 1

    !> 間引き処理のための追加したトレーサー数カウント
    integer :: add_counter

    ! カウンターをリセット
    add_counter = 0

    ! 全格子をチェック
    do cell_index_j = 1, cell_count_j
      do cell_index_i = 1, cell_count_i
        ! セルに1つ以上トレーサーがある場合は次のセルへ
        if (tracer%tracer_number_in_cell(cell_index_i, cell_index_j) >= 1) cycle

        ! 投入箇所が障害物セルかチェック
        if (obstacle_cell(cell_index_i, cell_index_j) == 1) cycle

        ! インデックスセット
        supply_position_i = cell_index_i
        supply_position_j = cell_index_j

        ! 座標計算
        supply_position_xi = grid_interval_xi*(dble(cell_index_i - 1) + 0.5)
        supply_position_eta = grid_interval_eta*(dble(cell_index_j - 1) + 0.5)

        ! セル内の座標計算
        supply_position_xi_in_cell = grid_interval_xi/2
        supply_position_eta_in_cell = grid_interval_eta/2

        ! トレーサー位置の水深、摩擦速度、などのチェック
        call check_tracer(tracer%Movable_Critical_depth, &
                          tracer%Movable_Critical_u_star, &
                          supply_position_xi, &
                          supply_position_eta, &
                          supply_position_i, &
                          supply_position_j, &
                          supply_position_xi_in_cell, &
                          supply_position_eta_in_cell, &
                          is_add_tracer, &
                          is_tracer_movable)

        if (is_add_tracer == 0) cycle

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
          tracer%is_tracer_trapped(tracer%total_tracer_number) = 0
          tracer%is_tracer_invincible(tracer%total_tracer_number) = 0
          tracer%is_tracer_arrived(tracer%total_tracer_number) = 1

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
    real(8), intent(in) :: time_since_start
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
    real(8), dimension(:), allocatable :: tracer_coordinate_x
    real(8), dimension(:), allocatable :: tracer_coordinate_y

    allocate (tracer_coordinate_x(tracer%total_tracer_number))
    allocate (tracer_coordinate_y(tracer%total_tracer_number))

    write (*, '(a40,i10)') "total "//trim(suffix)//" tracer number : ", tracer%total_tracer_number

    call cg_iric_write_sol_baseiterative_integer(cgnsout, "Tracer Number("//trim(suffix)//")", tracer%total_tracer_number, is_error)

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

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 軌跡追跡のためのサブルーチン
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  !> @brief 軌跡追跡トレーサーの初期化を行うサブルーチン
  !******************************************************************************************
  subroutine Initialize_Trajectory_Tracer()

    !==========================================================================================
    ! 軌跡追跡トレーサー
    !==========================================================================================
    call cg_iric_read_integer(cgnsOut, "max_number_trajectory", trajectory%max_number, is_error)
    call cg_iric_read_integer(cgnsOut, "max_save_times_trajectory", trajectory%max_save_times, is_error)
    call cg_iric_read_integer(cgnsOut, "save_interval_trajectory", trajectory%save_interval, is_error)
    call cg_iric_read_real(cgnsOut, "movable_critical_depth_trajectory", trajectory%Movable_Critical_depth, is_error)
    call cg_iric_read_real(cgnsOut, "movable_critical_u_star_trajectory", trajectory%Movable_Critical_u_star, is_error)
    call cg_iric_read_real(cgnsOut, "trap_wall_height_trajectory", trajectory%trap_wall_height, is_error)
    call cg_iric_read_real(cgnsOut, "trap_rate_trajectory", trajectory%trap_rate, is_error)
    call cg_iric_read_real(cgnsOut, "supply_time_trajectory", trajectory%supply_time, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_xi_first_trajectory", trajectory%supply_position_xi_first, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_xi_end_trajectory", trajectory%supply_position_xi_end, is_error)
    call cg_iric_read_real(cgnsOut, "supply_interval_xi_trajectory", trajectory%supply_interval_xi, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_eta_first_trajectory", trajectory%supply_position_eta_first, is_error)
    call cg_iric_read_real(cgnsOut, "supply_position_eta_end_trajectory", trajectory%supply_position_eta_end, is_error)
    call cg_iric_read_real(cgnsOut, "supply_interval_eta_trajectory", trajectory%supply_interval_eta, is_error)

    allocate (trajectory%tracer_coordinate_xi(trajectory%max_number))
    allocate (trajectory%tracer_coordinate_eta(trajectory%max_number))
    allocate (trajectory%cell_index_i(trajectory%max_number))
    allocate (trajectory%cell_index_j(trajectory%max_number))
    allocate (trajectory%tracer_coordinate_xi_in_cell(trajectory%max_number))
    allocate (trajectory%tracer_coordinate_eta_in_cell(trajectory%max_number))
    ! fortranは列優先なので、次元の順番を逆にしている
    allocate (trajectory%trajectory_coordinate_x(trajectory%max_save_times, trajectory%max_number))
    allocate (trajectory%trajectory_coordinate_y(trajectory%max_save_times, trajectory%max_number))
    allocate (trajectory%is_tracer_movable(trajectory%max_number))
    allocate (trajectory%is_tracer_trapped(trajectory%max_number))
    allocate (trajectory%is_tracer_invincible(trajectory%max_number))
    allocate (trajectory%is_tracer_arrived(trajectory%max_number))
    allocate (trajectory%seved_trajectory_point_number(trajectory%max_number))

    trajectory%save_counter = 0
    trajectory%save_timer = 0
    trajectory%total_tracer_number = 0
    trajectory%is_added_trajectory_tracer = 0
    trajectory%tracer_coordinate_xi = 0.0
    trajectory%tracer_coordinate_eta = 0.0
    trajectory%cell_index_i = 0
    trajectory%cell_index_j = 0
    trajectory%tracer_coordinate_xi_in_cell = 0.0
    trajectory%tracer_coordinate_eta_in_cell = 0.0
    trajectory%trajectory_coordinate_x = 0.0
    trajectory%trajectory_coordinate_y = 0.0
    trajectory%is_tracer_movable = 0
    trajectory%is_tracer_trapped = 0
    trajectory%is_tracer_invincible = 0
    trajectory%is_tracer_arrived = 0
    trajectory%total_tracer_number = 0
    trajectory%seved_trajectory_point_number = 0

  end subroutine Initialize_Trajectory_Tracer

  !******************************************************************************************
  !> @brief 軌跡追跡トレーサーの追加を行うサブルーチン
  !> @param[in] tracer  軌跡追跡トレーサーの情報を含む構造体
  !******************************************************************************************
  subroutine add_trajectory_tracer(tracer)
    !> 軌跡追跡トレーサーの情報を含む構造体
    type(trajectory_tracer), intent(inout) :: tracer
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index
    !> トレーサーの数
    integer :: tracer_number

    !> 投入範囲ループの総数
    integer :: supply_loop_count_xi
    !> 投入範囲ループの総数
    integer :: supply_loop_count_eta
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_xi
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_eta

    !> ξ方向投入地点座標
    real(8) :: supply_position_xi
    !> η方向投入地点座標
    real(8) :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    real(8) :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    real(8) :: supply_position_eta_in_cell

    !> 投入するかしないか
    integer :: is_add_tracer = 1
    !> トレーサーが動くか
    integer :: is_tracer_movable = 1

    !> 間引き処理のための追加したトレーサー数カウント
    integer :: add_counter

    !==========================================================================================
    ! 投入箇所ごとのループ
    !==========================================================================================

    ! 投入したかどうかのフラグを更新、一度投入したら二度と投入しない
    tracer%is_added_trajectory_tracer = 1

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

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 投入箇所に投入できるかチェック
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! フラグの初期化
        is_add_tracer = 1
        is_tracer_movable = 1

        ! 投入地点のセルのインデックスを調べる
        call find_tracer_cell_index(supply_position_xi, supply_position_eta, supply_position_i, supply_position_j, supply_position_xi_in_cell, supply_position_eta_in_cell)

        ! 投入箇所が障害物セルかチェック
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) cycle

        ! 投入箇所の水深と摩擦速度を調べる
        call check_tracer(tracer%Movable_Critical_depth, &
                          tracer%Movable_Critical_u_star, &
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
          call increment_integer_value(tracer%total_tracer_number, 1)

          ! トレーサーの状態を入力
          tracer%tracer_coordinate_xi(tracer%total_tracer_number) = supply_position_xi
          tracer%tracer_coordinate_eta(tracer%total_tracer_number) = supply_position_eta
          tracer%cell_index_i(tracer%total_tracer_number) = supply_position_i
          tracer%cell_index_j(tracer%total_tracer_number) = supply_position_j
          tracer%tracer_coordinate_xi_in_cell(tracer%total_tracer_number) = supply_position_xi_in_cell
          tracer%tracer_coordinate_eta_in_cell(tracer%total_tracer_number) = supply_position_eta_in_cell
          tracer%is_tracer_movable(tracer%total_tracer_number) = is_tracer_movable
          tracer%is_tracer_trapped(tracer%total_tracer_number) = 0
          tracer%is_tracer_invincible(tracer%total_tracer_number) = 0
          tracer%is_tracer_arrived(tracer%total_tracer_number) = 1

          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 軌跡の保存
          ! トレーサー追加時の位置が軌跡の初期位置となる
          ! 軌跡の一般座標は計算で使用しなので、保存の時点で物理座標に変換しておく
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tracer%save_counter = 1
          tracer%save_timer = 0

          call transform_general_to_physical(supply_position_i, &
                                             supply_position_j, &
                                             supply_position_xi_in_cell, &
                                             supply_position_eta_in_cell, &
                                             tracer%trajectory_coordinate_x(1, tracer%total_tracer_number), &
                                             tracer%trajectory_coordinate_y(1, tracer%total_tracer_number))

          if (tracer%total_tracer_number >= tracer%max_number) return

        end if

      end do
    end do
  end subroutine add_trajectory_tracer

  !******************************************************************************************
  !> @brief 軌跡追跡トレーサーの移動を行うサブルーチン
  !> @param[inout] tracer  軌跡追跡トレーサーの情報を含む構造体
  !******************************************************************************************
  subroutine move_trajectory_tracer(tracer)
    !> 軌跡追跡トレーサーの情報を含む構造体
    type(trajectory_tracer), intent(inout) :: tracer
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !> 移動後のξ方向座標
    real(8) :: moved_position_xi
    !> 移動後のη方向座標
    real(8) :: moved_position_eta
    !> 移動後のセル内でのξ方向座標
    real(8) :: moved_position_xi_in_cell
    !> 移動後のセル内でのη方向座標
    real(8) :: moved_position_eta_in_cell
    !> 移動後のセルのi方向インデックス
    integer :: moved_position_i
    !> 移動後のセルのj方向インデックス
    integer :: moved_position_j

    !> トレーサー箇所の水深
    real(8) :: tracer_point_depth
    !> トレーサー箇所の摩擦速度
    real(8) :: tracer_point_u_star
    !> トレーサー箇所のxi方向速度
    real(8) :: tracer_point_velocity_xi
    !> トレーサー箇所のeta方向速度
    real(8) :: tracer_point_velocity_eta
    !> トレーサー箇所の渦動粘性係数
    real(8) :: tracer_point_eddy_viscosity_coefficient
    !> トレーサー箇所の捕獲率
    real(8) :: tracer_point_trap_rate
    !> トレーサー箇所のξ方向スケーリング係数
    real(8) :: tracer_point_scale_factor_xi
    !> トレーサー箇所のη方向スケーリング係数
    real(8) :: tracer_point_scale_factor_eta

    !> トレーサー捕獲判定値
    real(8) :: trap_decision_value

    !> ランダムウォークの移動距離の標準偏差
    real(8) :: diffusion_std_dev

    ! ボックスミュラーによる正規分布乱数用変数
    !> 正規分布乱数
    real(8) :: bm_standard_normal_cos
    !> 正規分布乱数
    real(8) :: bm_standard_normal_sin

    do tracer_index = 1, tracer%total_tracer_number ! 既存の全てのトレーサーのループ

      !==========================================================================================
      ! 移動しないトレーサーはここで抜ける
      !==========================================================================================

      ! 除去対象のトレーサーはスキップ
      if (tracer%is_tracer_arrived(tracer_index) == 0) then
        cycle
      end if

      ! トラップされたトレーサーは移動しない
      if (tracer%is_tracer_trapped(tracer_index) == 1) then
        cycle
      end if

      !==========================================================================================
      ! まずはトレーサーが動けるか、除去されるかをチェック
      !==========================================================================================

      ! フラグのリセット
      tracer%is_tracer_movable(tracer_index) = 1

      ! 水深と摩擦速度によるチェック
      call check_tracer(tracer%Movable_Critical_depth, &
                        tracer%Movable_Critical_u_star, &
                        tracer%tracer_coordinate_xi(tracer_index), &
                        tracer%tracer_coordinate_eta(tracer_index), &
                        tracer%cell_index_i(tracer_index), &
                        tracer%cell_index_j(tracer_index), &
                        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
                        tracer%tracer_coordinate_eta_in_cell(tracer_index), &
                        tracer%is_tracer_arrived(tracer_index), &
                        tracer%is_tracer_movable(tracer_index))

      ! 除去対象のトレーサーはスキップ
      if (tracer%is_tracer_arrived(tracer_index) == 0) then
        cycle
      end if

      ! 水深や摩擦速度が移動限界より小さい場合は移動しないし、トラップされない
      if (tracer%is_tracer_movable(tracer_index) == 0) then
        cycle
      end if

      !==========================================================================================
      ! トラップによって動けなくなるかをチェック
      !==========================================================================================
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トレーサーの水深を調べる
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      tracer_point_depth = &
        calculate_scalar_at_tracer_position( &
        depth_node, &
        tracer%cell_index_i(tracer_index), &
        tracer%cell_index_j(tracer_index), &
        tracer%tracer_coordinate_xi_in_cell(tracer_index), &
        tracer%tracer_coordinate_eta_in_cell(tracer_index))

      ! 無敵ではないトラップセルにある動けるトレーサーを対象にする
      if (trap_cell(tracer%cell_index_i(tracer_index), tracer%cell_index_j(tracer_index)) == 1 .and. tracer%is_tracer_movable(tracer_index) == 1 .and. tracer%is_tracer_invincible(tracer_index) == 0) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲率の計算
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トラップの高さが水深より大きい場合
        if (tracer%trap_wall_height > tracer_point_depth) then

          ! 捕獲率は入力値のまま
          tracer_point_trap_rate = tracer%trap_rate

        else  ! トラップの高さが水深より小さい場合

          ! 捕獲率は浸水分の高さの割合が反映される
          tracer_point_trap_rate = tracer%trap_wall_height/tracer_point_depth*tracer%trap_rate

        end if

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲の判定値の乱数を発生
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        call random_number(trap_decision_value)
        trap_decision_value = trap_decision_value*100

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 捕獲の判定
        ! 判定値が捕獲率よりも小さい場合捕獲される
        ! なお、トラップで動かなくなった場合は除去されない
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        if (tracer_point_trap_rate > trap_decision_value) then
          ! トラップされて二度と動かなくなる
          tracer%is_tracer_trapped(tracer_index) = 1
          tracer%is_tracer_movable(tracer_index) = 0
        else
          tracer%is_tracer_invincible(tracer_index) = 1
        end if

      end if

      ! トラップされたトレーサーは移動しない
      if (tracer%is_tracer_trapped(tracer_index) == 1) then
        cycle
      end if

      !==========================================================================================
      ! この時点で動けるトレーサーは移動を行う
      !==========================================================================================
      if (tracer%is_tracer_movable(tracer_index) == 1) then

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
        if (moved_position_eta >= 1.) moved_position_eta = 1.-(moved_position_eta - 1.)
        if (moved_position_eta <= 0.) moved_position_eta = -moved_position_eta

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 上下流端の周期境界条件による判定
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        if (is_periodic_boundary_condition_Tracers == 1) then
          ! 周期境界条件の場合は範囲外のトレーサーを移動
          if (moved_position_xi > 1.0) moved_position_xi = moved_position_xi - 1.
          if (moved_position_xi < 0.0) moved_position_xi = moved_position_xi + 1.
        else
          ! 周期境界じゃない場合範囲外のトレーサーは除去
          if (moved_position_xi < 0.0 .or. 1.0 + tolerance < moved_position_xi) then
            tracer%is_tracer_arrived(tracer_index) = 0
            cycle
          end if
          ! 精度の誤差によりちょっぴりはみ出ている場合は修正
          if (1.0 < moved_position_xi .and. moved_position_xi < 1.0 + tolerance) then
            moved_position_xi = 1.0
          end if
        end if

      end if

      !==========================================================================================
      ! 除去対象ではないトレーサーであれば移動後の場所のチェック
      !==========================================================================================
      if (tracer%is_tracer_arrived(tracer_index) == 1) then

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 移動処理後の場所について調べる
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! トレーサーが存在するセルのインデックスを取得
        call find_tracer_cell_index( &
          moved_position_xi, &
          moved_position_eta, &
          moved_position_i, &
          moved_position_j, &
          moved_position_xi_in_cell, &
          moved_position_eta_in_cell)

        ! 移動後の場所の条件でチェック
        call check_tracer(tracer%Movable_Critical_depth, &
                          tracer%Movable_Critical_u_star, &
                          moved_position_xi, &
                          moved_position_eta, &
                          moved_position_i, &
                          moved_position_j, &
                          moved_position_xi_in_cell, &
                          moved_position_eta_in_cell, &
                          tracer%is_tracer_arrived(tracer_index), &
                          tracer%is_tracer_movable(tracer_index))

        ! トレーサーがセルを移動していたら無敵状態解除
        if (tracer%cell_index_i(tracer_index) /= moved_position_i .or. tracer%cell_index_j(tracer_index) /= moved_position_j) then
          tracer%is_tracer_invincible(tracer_index) = 0
        end if

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動して、かつ除去されないトレーサーの処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (tracer%is_tracer_arrived(tracer_index) == 1) then

        ! トレーサーの座標を更新
        tracer%tracer_coordinate_xi(tracer_index) = moved_position_xi
        tracer%tracer_coordinate_eta(tracer_index) = moved_position_eta
        tracer%cell_index_i(tracer_index) = moved_position_i
        tracer%cell_index_j(tracer_index) = moved_position_j
        tracer%tracer_coordinate_xi_in_cell(tracer_index) = moved_position_xi_in_cell
        tracer%tracer_coordinate_eta_in_cell(tracer_index) = moved_position_eta_in_cell
      end if

    end do  ! トレーサー毎のループ

    ! 追跡回数カウンターを更新
    call increment_integer_value(trajectory%save_timer, 1)

    ! 追跡回数カウンターが保存間隔を超えたら保存
    if (tracer%save_timer >= tracer%save_interval) then
      tracer%save_timer = 0
      call save_trajectory_tracer(tracer)
    end if

  end subroutine move_trajectory_tracer

  !******************************************************************************************
  !> @brief 軌跡追跡トレーサーの保存を行うサブルーチン
  !> @param[inout] tracer  軌跡追跡トレーサーの情報を含む構造体
  !******************************************************************************************
  subroutine save_trajectory_tracer(tracer)
    !> 軌跡追跡トレーサーの情報を含む構造体
    type(trajectory_tracer), intent(inout) :: tracer
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    ! 保存回数カウンターを更新
    call increment_integer_value(tracer%save_counter, 1)

    ! 保存回数が最大保存回数を超えたら保存しない
    if (tracer%save_counter > tracer%max_save_times) return

    ! トレーサー毎のループ
    do tracer_index = 1, tracer%total_tracer_number
      ! 生存判定のトレサーのみ軌跡を保存
      if (tracer%is_tracer_arrived(tracer_index) == 1) then

        ! このトレーサーは何回保存されたかを記録
        tracer%seved_trajectory_point_number(tracer_index) = tracer%save_counter

        ! tracer_index番目のトレーサーのtracer%save_counter回目の保存位置を記録
        call transform_general_to_physical(tracer%cell_index_i(tracer_index), &
                                           tracer%cell_index_j(tracer_index), &
                                           tracer%tracer_coordinate_xi_in_cell(tracer_index), &
                                           tracer%tracer_coordinate_eta_in_cell(tracer_index), &
                                           tracer%trajectory_coordinate_x(tracer%save_counter, tracer_index), &
                                           tracer%trajectory_coordinate_y(tracer%save_counter, tracer_index))

      end if
    end do

  end subroutine save_trajectory_tracer

  !******************************************************************************************
  !> @brief 軌跡追跡トレーサーの出力を行うサブルーチン
  !> @param[in] tracer  軌跡追跡トレーサーの情報を含む構造体
  !******************************************************************************************
  subroutine write_sol_trajectory_tracer(trace)
    !> 軌跡追跡トレーサーの情報を含む構造体
    type(trajectory_tracer), intent(in) :: trace
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    write (*, '(a40, i10)') "total Special tracer number : ", count(trace%is_tracer_arrived == 1)
    ! is_tracer_arrivedが1のトレーサーの数を出力
    call cg_iric_write_sol_baseiterative_integer(cgnsOut, 'TotalSpecialTracerNumber', count(trace%is_tracer_arrived == 1), is_error)

    ! トレーサーが存在しない場合はスキップ
    if (trace%total_tracer_number == 0) return

    ! ポリゴンデータの書き込み開始の宣言
    call cg_iric_write_sol_polydata_groupbegin(cgnsOut, 'Trajectory', is_error)

    ! トレーサー毎のループ
    do tracer_index = 1, trace%total_tracer_number

      ! 軌跡の点数が2点未満の場合はスキップ
      if (trace%seved_trajectory_point_number(tracer_index) < 2) cycle

      ! ポリラインの書き込み
      call cg_iric_write_sol_polydata_polyline( &
        cgnsOut, &
        trace%seved_trajectory_point_number(tracer_index), &
        trace%trajectory_coordinate_x(1:trace%seved_trajectory_point_number(tracer_index), tracer_index), &
        trace%trajectory_coordinate_y(1:trace%seved_trajectory_point_number(tracer_index), tracer_index), &
        is_error)
      call cg_iric_write_sol_polydata_integer(cgnsOut, 'TracerID', tracer_index, is_error)

    end do

    ! ポリゴンデータの書き込み終了の宣言
    call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)

  end subroutine write_sol_trajectory_tracer

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! Windmapのサブルーチン郡
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  !> @brief windmapの初期化を行うサブルーチン
  !> @param[inout] windmap  windmapの情報を含む構造体
  !******************************************************************************************
  subroutine initialize_windmap()

    integer :: i

    !==========================================================================
    ! windmapトレーサー
    !==========================================================================
    call cg_iric_read_integer(cgnsOut, "max_display_number_windmap_line", windmap%max_number, is_error)
    call cg_iric_read_real(cgnsOut, "display_life_span_windmap_line", windmap%life_time, is_error)
    call cg_iric_read_real(cgnsOut, "save_interval_windmap_line", windmap%line_save_interval, is_error)

    ! windmapの最大保存回数
    windmap%max_save_times = int(windmap%life_time/windmap%line_save_interval)

    allocate (windmap%tracer_coordinate_xi(windmap%max_number))
    allocate (windmap%tracer_coordinate_eta(windmap%max_number))
    allocate (windmap%windmap_save_times(windmap%max_number))
    allocate (windmap%windmap_save_timer(windmap%max_number))
    allocate (windmap%windmap_coordinate_x(windmap%max_save_times, windmap%max_number))
    allocate (windmap%windmap_coordinate_y(windmap%max_save_times, windmap%max_number))
    allocate (windmap%windmap_line_length(windmap%max_save_times - 1, windmap%max_number))

    windmap%tracer_coordinate_xi = 0.0
    windmap%tracer_coordinate_eta = 0.0
    windmap%windmap_save_times = 0
    windmap%windmap_save_timer = 0.0
    windmap%windmap_coordinate_x = 0.0
    windmap%windmap_coordinate_y = 0.0
    windmap%windmap_line_length = 0.0

  end subroutine initialize_windmap

  !******************************************************************************************
  !> @brief windmapの更新を行うサブルーチン
  !******************************************************************************************
  subroutine update_windmap()
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !==========================================================================
    ! 全トレーサーが対象のループ
    ! 寿命内及び移動後の位置が生存可能であれば場所を更新、そうでなければ再配置
    !==========================================================================
    do tracer_index = 1, windmap%max_number

      if (windmap%windmap_save_times(tracer_index) > 0) call move_windmap_tracer(tracer_index)
      if (windmap%windmap_save_times(tracer_index) == 0) call add_windmap_tracer(tracer_index)

    end do
  end subroutine update_windmap

  !******************************************************************************************
  !> @brief windmapの追加を行うサブルーチン
  !> @param[in] tracer_index  何個目のトレーサーかのインデックス
  !******************************************************************************************
  subroutine add_windmap_tracer(tracer_index)
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !> 投入するかしないか
    integer :: is_add_tracer = 0

    !> xi方向投入地点座標
    real(8) :: supply_position_xi
    !> eta方向投入地点座標
    real(8) :: supply_position_eta
    !> 投入地点のセルインデックス
    integer :: supply_position_i
    !> 投入地点のセルインデックス
    integer :: supply_position_j
    !> セル内でのξ方向投入地点座標
    real(8) :: supply_position_xi_in_cell
    !> セル内でのη方向投入地点座標
    real(8) :: supply_position_eta_in_cell

    integer:: i

    !==========================================================================================
    ! トレーサー追加先の座標を障害物ではないランダムな場所から探す
    !==========================================================================================
    do while (is_add_tracer == 0)

      ! 仮の投入地点を計算
      call random_number(supply_position_xi)
      call random_number(supply_position_eta)

      ! 投入箇所のセルのインデックスを調べる
      call find_tracer_cell_index(supply_position_xi, &
                                  supply_position_eta, &
                                  supply_position_i, &
                                  supply_position_j, &
                                  supply_position_xi_in_cell, &
                                  supply_position_eta_in_cell)

      ! 投入箇所が障害物セルかチェック
      if (obstacle_cell(supply_position_i, supply_position_j) == 1) cycle

      is_add_tracer = 1

    end do

    ! フラグをリセット
    is_add_tracer = 0

    ! ヘッドの位置を更新
    windmap%tracer_coordinate_xi(tracer_index) = supply_position_xi
    windmap%tracer_coordinate_eta(tracer_index) = supply_position_eta

    !==========================================================================================
    ! トレーサーもつ情報を初期化
    !==========================================================================================
    ! windmap_save_timesを更新
    windmap%windmap_save_times(tracer_index) = 1
    ! windmap_save_timerをリセット
    windmap%windmap_save_timer(tracer_index) = 0.0
    ! windmap_coordinate_x, windmap_coordinate_yをリセット
    windmap%windmap_coordinate_x(:, tracer_index) = 0.0
    windmap%windmap_coordinate_y(:, tracer_index) = 0.0
    ! windmap_line_lengthをリセット
    windmap%windmap_line_length(:, tracer_index) = 0.0

    !==========================================================================================
    ! 軌跡の保存
    !==========================================================================================
    call transform_general_to_physical( &
      supply_position_i, &
      supply_position_j, &
      supply_position_xi_in_cell, &
      supply_position_eta_in_cell, &
      windmap%windmap_coordinate_x(1, tracer_index), &
      windmap%windmap_coordinate_y(1, tracer_index))

  end subroutine add_windmap_tracer

  !******************************************************************************************
  !> @brief windmapの移動を行うサブルーチン
  !> @param[in] tracer_index  何個目のトレーサーかのインデックス
  !******************************************************************************************
  subroutine move_windmap_tracer(tracer_index)
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index

    !> トレーサーのあるセルのインデックス
    integer :: cell_index_i
    !> トレーサーのあるセルのインデックス
    integer :: cell_index_j
    !> トレーサーのセル内でのξ方向座標
    real(8) :: tracer_coordinate_xi_in_cell
    !> トレーサーのセル内でのη方向座標
    real(8) :: tracer_coordinate_eta_in_cell

    !> 移動後のξ方向座標
    real(8) :: moved_position_xi
    !> 移動後のη方向座標
    real(8) :: moved_position_eta
    !> 移動後のセル内でのξ方向座標
    real(8) :: moved_position_xi_in_cell
    !> 移動後のセル内でのη方向座標
    real(8) :: moved_position_eta_in_cell
    !> 移動後のセルのi方向インデックス
    integer :: moved_position_i
    !> 移動後のセルのj方向インデックス
    integer :: moved_position_j

    !> トレーサー地点の流速
    real(8) :: tracer_point_velocity_xi
    !> トレーサー地点の流速
    real(8) :: tracer_point_velocity_eta
    !> トレーサー地点の渦動粘性係数
    real(8) :: tracer_point_eddy_viscosity_coefficient

    !> トレーサー箇所のξ方向スケーリング係数
    real(8) :: tracer_point_scale_factor_xi
    !> トレーサー箇所のη方向スケーリング係数
    real(8) :: tracer_point_scale_factor_eta

    !> ランダムウォークの移動距離の標準偏差
    real(8) :: diffusion_std_dev

    !> ボックスミュラーによる正規分布乱数用変数
    !> 正規分布乱数
    real(8) :: bm_standard_normal_cos
    !> 正規分布乱数
    real(8) :: bm_standard_normal_sin

    !==========================================================================================
    ! トレーサーの寿命をチェックして寿命を超えたら再配置
    !==========================================================================================
    ! windmap_save_timerを更新
    call increment_real_value(windmap%windmap_save_timer(tracer_index), time_interval_for_tracking)

    ! windmap_save_timerが寿命を超えたら再配置
    if (windmap%windmap_save_timer(tracer_index) > windmap%life_time) then
      windmap%windmap_save_times(tracer_index) = 0
      return
    end if

    !==========================================================================================
    ! トレーサーのあるセルのインデックスを取得
    !==========================================================================================
    call find_tracer_cell_index( &
      windmap%tracer_coordinate_xi(tracer_index), &
      windmap%tracer_coordinate_eta(tracer_index), &
      cell_index_i, &
      cell_index_j, &
      tracer_coordinate_xi_in_cell, &
      tracer_coordinate_eta_in_cell)

    !==========================================================================================
    ! トレーサー位置の各種パラメータを計算
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサー地点の流速を計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    tracer_point_velocity_xi = &
      calculate_scalar_at_tracer_position( &
      velocity_xi_node, &
      cell_index_i, &
      cell_index_j, &
      tracer_coordinate_xi_in_cell, &
      tracer_coordinate_eta_in_cell)

    tracer_point_velocity_eta = &
      calculate_scalar_at_tracer_position( &
      velocity_eta_node, &
      cell_index_i, &
      cell_index_j, &
      tracer_coordinate_xi_in_cell, &
      tracer_coordinate_eta_in_cell)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! ランダムウォークを考慮
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! トレーサー地点の渦動粘性係数を計算
    tracer_point_eddy_viscosity_coefficient = &
      calculate_scalar_at_tracer_position( &
      eddy_viscosity_coefficient_node, &
      cell_index_i, &
      cell_index_j, &
      tracer_coordinate_xi_in_cell, &
      tracer_coordinate_eta_in_cell)

    ! ランダムウォークによる移動距離の標準偏差
    diffusion_std_dev = sqrt(2*(a_diff*tracer_point_eddy_viscosity_coefficient + b_diff)*time_interval_for_tracking)

    ! 縦方向横方向の正規分布乱数を取得
    call generate_box_muller_random(bm_standard_normal_cos, bm_standard_normal_sin)

    ! トレーサー地点のスケーリング係数を計算
    tracer_point_scale_factor_xi = (scale_factor_xi(cell_index_i, cell_index_j + 1)*tracer_coordinate_eta_in_cell &
                                    + scale_factor_xi(cell_index_i, cell_index_j)*(grid_interval_eta - tracer_coordinate_eta_in_cell)) &
                                   /grid_interval_eta

    tracer_point_scale_factor_eta = (scale_factor_eta(cell_index_i + 1, cell_index_j)*tracer_coordinate_xi_in_cell &
                                     + scale_factor_eta(cell_index_i, cell_index_j)*(grid_interval_xi - tracer_coordinate_xi_in_cell)) &
                                    /grid_interval_xi

    !==========================================================================================
    ! 移動後の座標を計算
    !==========================================================================================
    moved_position_xi = windmap%tracer_coordinate_xi(tracer_index) &
                        + tracer_point_velocity_xi*time_interval_for_tracking &
                        + bm_standard_normal_cos*diffusion_std_dev*tracer_point_scale_factor_xi

    moved_position_eta = windmap%tracer_coordinate_eta(tracer_index) &
                         + tracer_point_velocity_eta*time_interval_for_tracking &
                         + bm_standard_normal_sin*diffusion_std_dev*tracer_point_scale_factor_eta

    !==========================================================================================
    ! 境界条件によるチェック
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 側面の壁では反射する
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (moved_position_eta >= 1.) moved_position_eta = 1.-(moved_position_eta - 1.)
    if (moved_position_eta <= 0.) moved_position_eta = -moved_position_eta

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 上下流端の周期境界条件による判定
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (is_periodic_boundary_condition_Tracers == 1) then
      ! 周期境界条件の場合は範囲外のトレーサーを移動
      if (moved_position_xi > 1.0) moved_position_xi = moved_position_xi - 1.
      if (moved_position_xi < 0.0) moved_position_xi = moved_position_xi + 1.
    else
      ! 周期境界じゃない場合範囲外のトレーサーは除去
      if (moved_position_xi < 0.0 .or. 1.0 + tolerance < moved_position_xi) then
        windmap%windmap_save_times(tracer_index) = 0
        return
      end if
      ! 精度の誤差によりちょっぴりはみ出ている場合は修正
      if (1.0 < moved_position_xi .and. moved_position_xi < 1.0 + tolerance) then
        moved_position_xi = 1.0
      end if
    end if

    !==========================================================================================
    ! 移動後の場所でのチェック
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 移動後の座標をセルのインデックスに変換
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    call find_tracer_cell_index( &
      moved_position_xi, &
      moved_position_eta, &
      moved_position_i, &
      moved_position_j, &
      moved_position_xi_in_cell, &
      moved_position_eta_in_cell)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 移動後が障害物セルの場合は除去
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (obstacle_cell(moved_position_i, moved_position_j) == 1) then
      windmap%windmap_save_times(tracer_index) = 0
      return
    end if

    !==========================================================================================
    ! 問題ないので移動後の座標に更新
    !==========================================================================================
    windmap%tracer_coordinate_xi(tracer_index) = moved_position_xi
    windmap%tracer_coordinate_eta(tracer_index) = moved_position_eta

    !==========================================================================================
    ! 軌跡保存のタイマーを更新、軌跡保存間隔を超えていたら保存＆ラインの長さ計算
    !==========================================================================================
    ! windmap_save_timerが保存間隔を超えたら保存
    if (int(windmap%windmap_save_timer(tracer_index)/windmap%line_save_interval) > windmap%windmap_save_times(tracer_index)) then

      ! 保存回数を更新
      call increment_integer_value(windmap%windmap_save_times(tracer_index), 1)

      ! 座標変換して軌跡を保存
      call transform_general_to_physical( &
        moved_position_i, &
        moved_position_j, &
        moved_position_xi_in_cell, &
        moved_position_eta_in_cell, &
        windmap%windmap_coordinate_x(windmap%windmap_save_times(tracer_index), tracer_index), &
        windmap%windmap_coordinate_y(windmap%windmap_save_times(tracer_index), tracer_index))

      ! ラインの長さを計算
      windmap%windmap_line_length(windmap%windmap_save_times(tracer_index) - 1, tracer_index) = &
        sqrt((windmap%windmap_coordinate_x(windmap%windmap_save_times(tracer_index), tracer_index) - &
              windmap%windmap_coordinate_x(windmap%windmap_save_times(tracer_index) - 1, tracer_index))**2 + &
             (windmap%windmap_coordinate_y(windmap%windmap_save_times(tracer_index), tracer_index) - &
              windmap%windmap_coordinate_y(windmap%windmap_save_times(tracer_index) - 1, tracer_index))**2)
    end if

  end subroutine move_windmap_tracer

  !******************************************************************************************
  !> @brief windmapの出力を行うサブルーチン
  !******************************************************************************************
  subroutine write_sol_windmap()
    !> 何個目のトレーサーかのインデックス
    integer :: tracer_index
    !> 何個目の保存かのインデックス
    integer :: saved_point_index
    !> ポリラインの長さの最小値
    real(8) :: polyline_length_min
    !> ポリラインの長さの最大値
    real(8) :: polyline_length_max
    !> 正規化された速度
    real(8) :: normalized_velocity

    !==========================================================================
    ! ポリラインの長さの最小値と最大値を計算
    !==========================================================================
    ! ポリラインが一つでもある場合にのみ計算
    if (minval(windmap%windmap_save_times) >= 2) then
      polyline_length_min = minval(windmap%windmap_line_length, mask=windmap%windmap_line_length /= 0.0)
      polyline_length_max = maxval(windmap%windmap_line_length, mask=windmap%windmap_line_length /= 0.0)
    end if

    !==========================================================================
    ! Headとwindmapの出力
    !==========================================================================
    ! 出力開始
    call cg_iric_write_sol_particlegroup_groupbegin(cgnsOut, "Windmap Head", is_error)
    call cg_iric_write_sol_polydata_groupbegin(cgnsOut, 'Windmap', is_error)

    ! トレーサー毎のループ
    do tracer_index = 1, windmap%max_number

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! Headの出力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トレーサーが存在しない場合はスキップ、ないとは思うが保険として
      if (windmap%windmap_save_times(tracer_index) == 0) cycle

      ! トレーサー座標の出力
      call cg_iric_write_sol_particlegroup_pos2d( &
        cgnsOut, &
        windmap%windmap_coordinate_x(windmap%windmap_save_times(tracer_index), tracer_index), &
        windmap%windmap_coordinate_y(windmap%windmap_save_times(tracer_index), tracer_index), &
        is_error)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! Polylineの出力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (windmap%windmap_save_times(tracer_index) <= 1) cycle
      do saved_point_index = 1, windmap%windmap_save_times(tracer_index) - 1
        ! ポリラインの書き込み
        call cg_iric_write_sol_polydata_polyline( &
          cgnsOut, &
          2, &
          windmap%windmap_coordinate_x(saved_point_index:saved_point_index + 1, tracer_index), &
          windmap%windmap_coordinate_y(saved_point_index:saved_point_index + 1, tracer_index), &
          is_error)

        ! ポリラインの長さ出力
        call cg_iric_write_sol_polydata_real(cgnsOut, 'Length', windmap%windmap_line_length(saved_point_index, tracer_index), is_error)
        ! 正規化された速度の計算・出力
        normalized_velocity = (windmap%windmap_line_length(saved_point_index, tracer_index) - polyline_length_min) &
                              /(polyline_length_max - polyline_length_min)
        call cg_iric_write_sol_polydata_real(cgnsOut, 'Normalized Velocity', normalized_velocity, is_error)
      end do

    end do

    ! 出力終了処理
    call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)
    call cg_iric_write_sol_particlegroup_groupend(cgnsOut, is_error)

  end subroutine write_sol_windmap

end module Trace
