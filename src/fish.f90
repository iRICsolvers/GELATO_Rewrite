module fish_module

  use iric
  use common
  use timer_module
  use grid
  use result
  use trace

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  ! Memo
  ! 魚の移動については他のトレーサーと異なり物理座標系での移動を行う。
  ! そのため、物理座標系での流速やランダムウォーク、魚自身の運動による速度を求め、一般座標系に戻してから移動を行う。
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  implicit none

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 変数の宣言
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !******************************************************************************************
  ! Fishトレーサーの共通パラメータ
  !******************************************************************************************
  !> 魚の済周期境界条件を有効にするかどうか
  integer :: is_periodic_boundary_condition_fish
  !> 魚がジャンプするかどうか
  integer :: is_jump_fish
  !> 魚の表示サイズ倍率
  real(8) :: fish_display_size_magnification

  !==========================================================================================
  ! 任意断面を通過した魚をカウントする機能に関する変数
  !==========================================================================================
  !> 魚の計測を行うかどうか
  integer :: is_count_fish
  !> 魚の計測を行う断面の位置
  integer :: count_section_position
  !> 計測開始時間
  real(8) :: count_time_start
  !> 計測終了時間
  real(8) :: count_time_end
  !> 通過した魚体数のカウンター
  integer, save :: passed_fish_count = 0
  !> 通過した魚の数をカウントする任意断面ポリラインの座標
  real(8), dimension(:), allocatable :: count_fish_crossing_section_x
  !> 通過した魚の数をカウントする任意断面ポリラインの座標
  real(8), dimension(:), allocatable :: count_fish_crossing_section_y

  !******************************************************************************************
  ! 魚のパラメーター
  !******************************************************************************************
  !> 魚の数
  integer :: fish_count
  !> 魚の最大数
  integer :: fish_count_max
  !> 魚グループの数
  integer :: fish_group_count

  !> 各グループの魚の数
  integer, dimension(:), allocatable :: fish_count_in_group
  !> グループの割合
  integer, dimension(:), allocatable :: fish_group_ratio
  !> 体長
  real(8), dimension(:), allocatable :: fish_body_length
  !> 遊泳速度
  real(8), dimension(:), allocatable :: fish_cruise_speed
  !> 遊泳継続時間
  real(8), dimension(:), allocatable :: fish_cruise_time
  !> 突進速度
  real(8), dimension(:), allocatable :: fish_rush_speed
  !> 突進継続時間
  real(8), dimension(:), allocatable :: fish_rush_time
  !> 魚の遊泳、突進のサイクル時間
  real(8), dimension(:), allocatable :: fish_cycle_time
  !> 移動限界水深
  real(8), dimension(:), allocatable :: movable_critical_depth_fish
  !> @brief 移動限界水深でのさかなの挙動
  !> @param 0: その場に留まる
  !> @param 1: 逆方向に泳ぐ
  !> @param 2: 流れに身を任せる
  !> @param 3: ランダムな方向に泳ぐ
  !> @param 4: 除去される
  integer, dimension(:), allocatable :: fish_handling_in_critical_depth
  !> 移動限界水深での行動時間
  real(8), dimension(:), allocatable :: behavior_time_in_critical_depth
  !> ジャンプに挑戦する高さ
  real(8), dimension(:), allocatable :: jump_try_height
  !> ジャンプ可能な高さ
  real(8), dimension(:), allocatable :: jumpable_height
  !> ジャンプ可能な距離
  real(8), dimension(:), allocatable :: jumpable_distance
  !> @brief ジャンプ失敗時の挙動
  !> @param 0: その場に留まる
  !> @param 1: 泳ぎによる遡上を試みる
  integer, dimension(:), allocatable :: fish_handling_when_jumping_failed

  !******************************************************************************************
  ! 魚のポリゴン形状等に関するパラメータ
  !******************************************************************************************
  !> 魚の中心線のセグメント数
  integer, parameter :: centerline_segments = 20
  !> 魚の形状を表す頂点の配列のサイズ
  integer, parameter :: fish_outline_point_count = 43 ! centerline_segments*2+3

  !> 魚の形状をプロットするためのx座標
  real(8) :: fish_outline_x(fish_outline_point_count)
  !> 魚の形状をプロットするためのy座標
  real(8) :: fish_outline_y(fish_outline_point_count)

  !******************************************************************************************
  ! 魚トレーサーの持つ変数
  !******************************************************************************************
  !> 魚のξ方向座標
  real(8), dimension(:), allocatable :: fish_coordinate_xi
  !> 魚のη方向座標
  real(8), dimension(:), allocatable :: fish_coordinate_eta
  !> 魚のアングル(radian)(0=x軸方向)(0~2π)
  real(8), dimension(:), allocatable :: fish_angle
  !> 魚の最小水深以下での行動経過時間
  real(8), dimension(:), allocatable :: fish_stay_time_in_critical_depth
  !> どのグループに属するか
  integer, dimension(:), allocatable :: fish_group
  !> 魚の生存フラグ
  integer, dimension(:), allocatable :: is_fish_alive
  !> 魚の固有タイマー
  real(8), dimension(:), allocatable :: fish_timer
  !> @brief 魚がジャンプしたか、失敗したかのフラグ、一度出力したらリセットされる
  !> @param 0: 魚はジャンプしていない
  !> @param 1: 魚はジャンプした
  !> @param -1: 魚はジャンプに失敗した
  integer, dimension(:), allocatable :: fish_jump_flag

contains

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! 汎用的なサブルーチン
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  !> @brief 配列を大きい順に見たときのインデックスを取得する、ソート対象の配列は変更されない
  !> @param[in] array ソート対象の配列
  !> @param[out] indices 大きい順で並べた際のインデックスを格納する配列
  !******************************************************************************************
  subroutine get_sorted_indices(array, indices)
    implicit none
    !> ソート対象の配列、real(8)またはinteger型のみ対応、この配列は変更されない
    integer, intent(in) :: array(:)
    !> 大きい順で並べた際のインデックスを格納する配列
    integer, intent(out) :: indices(:)
    !> ループ用の変数
    integer :: i
    !> ループ用の変数
    integer :: j
    !> 配列のサイズを格納する変数
    integer :: n
    !> 一時的にインデックスを格納する変数
    integer :: temp_index

    ! 配列のサイズを取得
    n = size(array)
    ! インデックスを初期化
    indices = [(i, i=1, n)]

    ! バブルソートアルゴリズムを使用してインデックスをソート
    do i = 1, n - 1
      do j = i + 1, n
        ! 大きい順に並べ替える
        if (array(indices(i)) < array(indices(j))) then
          ! インデックスを交換
          temp_index = indices(i)
          indices(i) = indices(j)
          indices(j) = temp_index
        end if
      end do
    end do

  end subroutine get_sorted_indices

  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !
  ! Fishトレーサーのサブルーチン
  !
  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  !******************************************************************************************
  !> @brief 魚トレーサーの初期化
  !******************************************************************************************
  subroutine initialize_fish_tracer()

    !> ループ用の変数
    integer :: i

    !==========================================================================================
    ! パラメータの読み込み
    !==========================================================================================
    !------------------------------------------------------------------------------------------
    ! Fishトレーサーの共通パラメータ
    !------------------------------------------------------------------------------------------
    call cg_iric_read_integer(cgnsOut, 'is_periodic_boundary_condition_fish', is_periodic_boundary_condition_fish, is_error)
    call cg_iric_read_integer(cgnsOut, 'is_jump_fish', is_jump_fish, is_error)
    call cg_iric_read_real(cgnsOut, 'fish_display_size_magnification', fish_display_size_magnification, is_error)
    call cg_iric_read_integer(cgnsOut, 'is_count_fish', is_count_fish, is_error)
    call cg_iric_read_integer(cgnsOut, 'count_section_position', count_section_position, is_error)
    call cg_iric_read_real(cgnsOut, 'count_time_start', count_time_start, is_error)
    call cg_iric_read_real(cgnsOut, 'count_time_end', count_time_end, is_error)

    !------------------------------------------------------------------------------------------
    ! トレーサーグループ毎のパラメータ
    !------------------------------------------------------------------------------------------
    call cg_iric_read_complex_count(cgnsOut, 'fish_information_list', fish_group_count, is_error)

    allocate (fish_count_in_group(fish_group_count))
    allocate (fish_group_ratio(fish_group_count))
    allocate (fish_body_length(fish_group_count))
    allocate (fish_cruise_speed(fish_group_count))
    allocate (fish_cruise_time(fish_group_count))
    allocate (fish_rush_speed(fish_group_count))
    allocate (fish_rush_time(fish_group_count))
    allocate (fish_cycle_time(fish_group_count))
    allocate (movable_critical_depth_fish(fish_group_count))
    allocate (fish_handling_in_critical_depth(fish_group_count))
    allocate (behavior_time_in_critical_depth(fish_group_count))
    allocate (jump_try_height(fish_group_count))
    allocate (jumpable_height(fish_group_count))
    allocate (jumpable_distance(fish_group_count))
    allocate (fish_handling_when_jumping_failed(fish_group_count))

    ! 各グループの魚のパラメーターを読み込む
    do i = 1, fish_group_count
      call cg_iric_read_complex_integer(cgnsOut, 'fish_information_list', i, 'fish_group_ratio', fish_group_ratio(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'fish_body_length', fish_body_length(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'fish_cruise_speed', fish_cruise_speed(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'fish_cruise_time', fish_cruise_time(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'fish_rush_speed', fish_rush_speed(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'fish_rush_time', fish_rush_time(i), is_error)
      fish_cycle_time(i) = fish_cruise_time(i) + fish_rush_time(i)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'movable_critical_depth_fish', movable_critical_depth_fish(i), is_error)
      call cg_iric_read_complex_integer(cgnsOut, 'fish_information_list', i, 'fish_handling_in_critical_depth', fish_handling_in_critical_depth(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'behavior_time_in_critical_depth', behavior_time_in_critical_depth(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'jump_try_height', jump_try_height(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'jumpable_height', jumpable_height(i), is_error)
      call cg_iric_read_complex_real(cgnsOut, 'fish_information_list', i, 'jumpable_distance', jumpable_distance(i), is_error)
      call cg_iric_read_complex_integer(cgnsOut, 'fish_information_list', i, 'fish_handling_when_jumping_failed', fish_handling_when_jumping_failed(i), is_error)
    end do

    ! 魚の表示サイズをかけておく
    ! 体長によって変化するパラメーターは描画関係のみなので問題ない
    fish_body_length = fish_body_length*fish_display_size_magnification

  end subroutine initialize_fish_tracer

  !******************************************************************************************
  !> @brief 魚トレーサーの初期配置メモリ確保と初期化
  !> @param[in] fish_count_max 魚の最大数
  !******************************************************************************************
  subroutine allocate_fish_tracer()

    allocate (fish_coordinate_xi(fish_count_max))
    allocate (fish_coordinate_eta(fish_count_max))
    allocate (fish_angle(fish_count_max))
    allocate (fish_stay_time_in_critical_depth(fish_count_max))
    allocate (fish_group(fish_count_max))
    allocate (is_fish_alive(fish_count_max))
    allocate (fish_timer(fish_count_max))

    fish_coordinate_xi = 0.0
    fish_coordinate_eta = 0.0
    fish_angle = 0.0
    fish_stay_time_in_critical_depth = 0.0
    fish_group = 0
    is_fish_alive = 1
    fish_timer = 0.0

    ! ジャンプする場合はフラグのメモリを確保
    if (is_jump_fish == 1) then
      allocate (fish_jump_flag(fish_count_max))
      fish_jump_flag = 0
    end if

  end subroutine allocate_fish_tracer

  !******************************************************************************************
  !> @brief 魚をカウントする断面のポリラインの作成する、ポリラインは指定されたインデックスiの格子を通る
  !******************************************************************************************
  subroutine create_fish_counting_section()

    !> ループ用の変数
    integer :: i

    !==========================================================================================
    ! メモリを確保
    !==========================================================================================
    allocate (count_fish_crossing_section_x(node_count_j))
    allocate (count_fish_crossing_section_y(node_count_j))

    ! 断面iの格子をj方向に通るポリラインを作成
    ! 格子点の座標の配列をスライスして取得
    count_fish_crossing_section_x = node_coordinate_x(count_section_position, :)
    count_fish_crossing_section_y = node_coordinate_y(count_section_position, :)

  end subroutine create_fish_counting_section

  !******************************************************************************************
  !> @brief 魚をカウントする断面のポリラインの出力
  !******************************************************************************************
  subroutine output_fish_counting_section()

    ! ポリライン出力を通知
    call cg_iric_write_sol_polydata_groupbegin(cgnsOut, 'Fish Count Crossing Section', is_error)

    call cg_iric_write_sol_polydata_polyline(cgnsOut, node_count_j, count_fish_crossing_section_x, count_fish_crossing_section_y, is_error)

    call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)

  end subroutine output_fish_counting_section

  !******************************************************************************************
  !> @brief 魚トレーサーの初期配置
  !******************************************************************************************
  subroutine add_fish_tracer()

    !==========================================================================================
    ! 初期位置での魚のアングルを求めるための変数
    !==========================================================================================
    !> 魚のアングルを求める用の微小変位(一般座標)
    real(8) :: displacement_xi = -0.0001
    !> 魚のアングルを求める用の微小変位(一般座標)
    real(8) :: displacement_eta = 0.0
    !> 魚のアングルを求める用の微小変位(物理座標)
    real(8) :: displacement_x
    !> 魚のアングルを求める用の微小変位(物理座標)
    real(8) :: displacement_y

    !> 初期位置の魚の存在するセルのインデックス
    integer :: fish_position_i
    !> 初期位置の魚の存在するセルのインデックス
    integer :: fish_position_j
    !> 初期位置の魚の存在するセル内のξ方向座標
    real(8) :: fish_position_xi_in_cell
    !> 初期位置の魚の存在するセル内のη方向座標
    real(8) :: fish_position_eta_in_cell

    ! 魚の位置でのヤコビ行列の行列式の逆数（1/det(J)
    real(8) :: fish_point_inverse_jacobian
    ! !> x方向の変位を ξ方向に変換する逆ヤコビ行列の要素（∂ξ/∂x = ∂y/∂η / det(J)）
    real(8) :: fish_point_x_to_xi_component
    !> y方向の変位を ξ方向に変換する逆ヤコビ行列の要素（∂ξ/∂y = -∂x/∂η / det(J)）
    real(8) :: fish_point_y_to_xi_component
    !> x方向の変位を η方向に変換する逆ヤコビ行列の要素（∂η/∂x = -∂y/∂ξ / det(J)）
    real(8) :: fish_point_x_to_eta_component
    !> y方向の変位を η方向に変換する逆ヤコビ行列の要素（∂η/∂y = ∂x/∂ξ / det(J)）
    real(8) :: fish_point_y_to_eta_component

    !==========================================================================================
    ! Fishトレーサー配置関係のパラメータ
    !==========================================================================================
    !> @brief 魚の配置方法
    !> @param 1: 無次元座標で配置する
    !> @param 2: 総数を与えてランダムに配置する
    integer :: fish_arrangement_method

    !==========================================================================================
    !> ループ用の変数
    !==========================================================================================
    integer :: fish_index

    !==========================================================================================
    ! Fishトレーサー配置方法の読み込み
    !==========================================================================================
    call cg_iric_read_integer(cgnsOut, 'fish_arrangement_method', fish_arrangement_method, is_error)

    !==========================================================================================
    ! 配置方法によって処理を分岐、まずは魚を配置する
    !==========================================================================================
    if (fish_arrangement_method == 1) then
      call add_fish_tracer_with_range()
    else if (fish_arrangement_method == 2) then
      call add_fish_tracer_random()
    end if

    !=========================================================================================
    ! 魚のグループ割り当て、ここでグループに応じた魚の属性を与える。
    !=========================================================================================
    call set_fish_group()

    !==========================================================================================
    ! 初期配置での条件チェック、再配置および除去
    !==========================================================================================
    call check_fish_initial_position()

    !==========================================================================================
    ! 初期配置時の魚のアングルを設定
    ! 初期条件では流速が定まっていない可能性もあるため、ξ方向(上流方向)に向かうように設定
    ! 魚のアングルは物理座標に置ける角度であるため、魚の位置からξ方向の変位とη方向(=0)の変位を微小な仮値で与え物理座標に直した後、角度に変換する。
    !==========================================================================================
    do fish_index = 1, fish_count
      if (is_fish_alive(fish_index) == 0) cycle ! 生存フラグが立っていない場合はスキップ

      ! 魚の位置のセルインデックス等を取得
      call find_tracer_cell_index(fish_coordinate_xi(fish_index), fish_coordinate_eta(fish_index), &
                                  fish_position_i, fish_position_j, &
                                  fish_position_xi_in_cell, fish_position_eta_in_cell)

      ! 魚の位置でのヤコビ行列の行列式の逆数（1/det(J)
      fish_point_inverse_jacobian = calculate_scalar_at_tracer_position(inverse_jacobian, &
                                                                        fish_position_i, &
                                                                        fish_position_j, &
                                                                        fish_position_xi_in_cell, &
                                                                        fish_position_eta_in_cell)

      ! x方向の変位を ξ方向に変換するための変換行列の要素
      fish_point_x_to_xi_component = calculate_scalar_at_tracer_position(x_to_xi_component, &
                                                                         fish_position_i, &
                                                                         fish_position_j, &
                                                                         fish_position_xi_in_cell, &
                                                                         fish_position_eta_in_cell)
      ! y方向の変位を ξ方向に変換するための変換行列の要素
      fish_point_y_to_xi_component = calculate_scalar_at_tracer_position(y_to_xi_component, &
                                                                         fish_position_i, &
                                                                         fish_position_j, &
                                                                         fish_position_xi_in_cell, &
                                                                         fish_position_eta_in_cell)
      ! x方向の変位を η方向に変換するための変換行列の要素
      fish_point_x_to_eta_component = calculate_scalar_at_tracer_position(x_to_eta_component, &
                                                                          fish_position_i, &
                                                                          fish_position_j, &
                                                                          fish_position_xi_in_cell, &
                                                                          fish_position_eta_in_cell)
      ! y方向の変位を η方向に変換するための変換行列の要素
      fish_point_y_to_eta_component = calculate_scalar_at_tracer_position(y_to_eta_component, &
                                                                          fish_position_i, &
                                                                          fish_position_j, &
                                                                          fish_position_xi_in_cell, &
                                                                          fish_position_eta_in_cell)

      ! 物理座標系での変位を計算
      displacement_x = (fish_point_y_to_eta_component*displacement_xi - fish_point_y_to_xi_component*displacement_eta)/fish_point_inverse_jacobian
      displacement_y = (-fish_point_x_to_eta_component*displacement_xi + fish_point_x_to_xi_component*displacement_eta)/fish_point_inverse_jacobian

      ! 魚の初期位置でのアングルを設定(0~2π)
      fish_angle(fish_index) = mod(atan2(displacement_y, displacement_x) + 2*pi, 2*pi)
    end do

  end subroutine add_fish_tracer

  !******************************************************************************************
  !> @brief 魚トレーサーの初期配置(無次元座標の範囲から)
  ! @note とりあえず投入先については障害物のみ考慮、魚をグループ分けしてから限界水深などを考慮する
  !******************************************************************************************
  subroutine add_fish_tracer_with_range()

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

    !> 投入範囲ループの総数
    integer :: supply_count_xi
    !> 投入範囲ループの総数
    integer :: supply_count_eta
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_xi
    !> 投入範囲ループのインデックス
    integer :: supply_loop_index_eta
    !> Fishトレーサーのインデックス
    integer :: fish_index = 0

    !> ξ方向投入地点座標
    real(8) :: supply_position_xi
    !> η方向投入地点座標
    real(8) :: supply_position_eta

    !==========================================================================================
    ! GUIから条件の読み込み
    !==========================================================================================
    call cg_iric_read_real(cgnsOut, 'supply_position_xi_first_fish', supply_position_xi_first, is_error)
    call cg_iric_read_real(cgnsOut, 'supply_position_xi_end_fish', supply_position_xi_end, is_error)
    call cg_iric_read_real(cgnsOut, 'supply_interval_xi_fish', supply_interval_xi, is_error)
    call cg_iric_read_real(cgnsOut, 'supply_position_eta_first_fish', supply_position_eta_first, is_error)
    call cg_iric_read_real(cgnsOut, 'supply_position_eta_end_fish', supply_position_eta_end, is_error)
    call cg_iric_read_real(cgnsOut, 'supply_interval_eta_fish', supply_interval_eta, is_error)

    !==========================================================================================
    ! 投入範囲ループの総数を計算
    !==========================================================================================
    supply_count_xi = int( &
                      (supply_position_xi_end - supply_position_xi_first + tolerance) &
                      /supply_interval_xi + tolerance)
    supply_count_eta = int( &
                       (supply_position_eta_end - supply_position_eta_first + tolerance) &
                       /supply_interval_eta + tolerance)

    fish_count_max = (supply_count_xi + 1)*(supply_count_eta + 1)

    !==========================================================================================
    ! Fishトレーサーのパラメーターの初期化
    !==========================================================================================
    call allocate_fish_tracer()

    !==========================================================================================
    ! 投入箇所ごとのループ
    !==========================================================================================
    do supply_loop_index_xi = 0, supply_count_xi

      ! ξ方向の投入位置を計算
      supply_position_xi = supply_position_xi_first + supply_interval_xi*supply_loop_index_xi

      ! 投入箇所が範囲外の場合は終了
      if (supply_position_xi > supply_position_xi_end + tolerance) exit

      ! 許容誤差範囲内の場合は補正
      if (abs(supply_position_xi - supply_position_xi_end) < tolerance) supply_position_xi = supply_position_xi_end

      do supply_loop_index_eta = 0, supply_count_eta

        ! η方向の投入位置を計算
        supply_position_eta = supply_position_eta_first + supply_interval_eta*supply_loop_index_eta

        ! 投入箇所が範囲外の場合はスキップ
        if (supply_position_eta > supply_position_eta_end + tolerance) cycle

        ! 許容誤差範囲内の場合は補正
        if (abs(supply_position_eta - supply_position_eta_end) < tolerance) supply_position_eta = supply_position_eta_end

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 魚トレーサーの座標を設定
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! Fishトレーサーのインデックスを計算
        fish_index = fish_index + 1
        fish_coordinate_xi(fish_index) = supply_position_xi
        fish_coordinate_eta(fish_index) = supply_position_eta

      end do
    end do

    fish_count = fish_index

    ! 多分ないとは思うが、誤差などで魚の数が最大数より少ない場合は警告をして、足りない分の魚は除去判定とする。
    if (fish_count < fish_count_max) then
      print *, 'Warning: The number of fish tracers is less than the maximum number of fish tracers.'
      do fish_index = fish_count + 1, fish_count_max
        ! 除去された魚のインデックス、理由を表示
        print '(A, I0, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' is removed. Because out of range.'
        fish_coordinate_xi(fish_index) = 0.0
        fish_coordinate_eta(fish_index) = 0.0
        fish_angle(fish_index) = 0.0
        fish_stay_time_in_critical_depth(fish_index) = 0.0
        fish_group(fish_index) = 0
        fish_timer(fish_index) = 0.0
        is_fish_alive(fish_index) = 0
      end do
    end if

  end subroutine add_fish_tracer_with_range

  !******************************************************************************************
  !> @brief 魚トレーサーの初期配置(総数を与えてランダムに配置)
  !******************************************************************************************
  subroutine add_fish_tracer_random()

    !> Fishトレーサーのインデックス
    integer :: fish_index
    !> 投入可能かどうかのフラグ
    integer :: is_supplyable = 0

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

    ! 魚の最大数を読み込む
    call cg_iric_read_integer(cgnsOut, 'fish_number', fish_count_max, is_error)

    !==========================================================================================
    ! Fishトレーサーのパラメーターの初期化
    !==========================================================================================

    call allocate_fish_tracer()

    !==========================================================================================
    ! 魚の座標をランダムに設定
    !==========================================================================================
    do fish_index = 1, fish_count_max

      ! 投入地点をランダムに決定
      call random_number(fish_coordinate_xi(fish_index))
      call random_number(fish_coordinate_eta(fish_index))

    end do

    fish_count = fish_count_max

  end subroutine add_fish_tracer_random

  !******************************************************************************************
  !> @brief グループ毎に魚を割り当てる
  !******************************************************************************************
  subroutine set_fish_group()
    implicit none
    !> 魚のインデックス
    integer :: fish_index
    !> ループ用の変数
    integer :: group_index
    !> 割当済みの魚と合計の差分
    integer :: difference
    !> 比率の大きい順にソートするためのインデックス配列
    integer, dimension(:), allocatable :: sorted_indices

    !> 割り当て済みの魚の数
    integer :: assigned_fish_count
    !> 一時的なカウント
    integer :: temp_count
    !> 乱数
    real(8) :: random_num

    !==========================================================================================
    ! 魚の座標をランダムに設定
    !==========================================================================================

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 魚の総数に基づいて各グループの魚の数を計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    do group_index = 1, fish_group_count

      ! 割合が0の場合は0を設定
      if (fish_group_ratio(group_index) == 0) then
        fish_count_in_group(group_index) = 0
        cycle
      end if
      fish_count_in_group(group_index) = int(fish_count*fish_group_ratio(group_index)/sum(fish_group_ratio) + tolerance)
    end do

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 差分を計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    difference = fish_count - sum(fish_count_in_group)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 割り振られなかった魚がいる場合、差分を割り当てる
    ! グループの占める割合の大きい順に余った数を1匹づつ割り当てる
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (difference /= 0) then

      ! 比率の大きい順に並べ替えたインデックスを格納する配列を初期化
      allocate (sorted_indices(fish_group_count))

      ! 比率の大きい順に並べ替えたインデックスを取得（元の配列の順番は変更されない）
      call get_sorted_indices(fish_group_ratio, sorted_indices)

      ! 差分が0になるまで繰り返す
      do while (difference /= 0)
        ! 比率の大きい順に差分を割り当て
        do group_index = 1, fish_group_count
          fish_count_in_group(sorted_indices(group_index)) = fish_count_in_group(sorted_indices(group_index)) + 1
          difference = difference - 1
          if (difference == 0) exit
        end do
      end do

      ! ソートしたインデックスのメモリを解放
      deallocate (sorted_indices)

    end if

    ! それぞれのグループの魚の数を表示
    print *, 'Fish count in each group:'
    do group_index = 1, fish_group_count
      print *, 'Group', group_index, ':', fish_count_in_group(group_index)
    end do

    !==========================================================================================
    ! 魚をランダムでグループに割り当てる
    !==========================================================================================
    ! 割り当て済みの魚の数を初期化
    assigned_fish_count = 0

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 各グループの魚を割り当てる
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    do group_index = 1, fish_group_count
      temp_count = 0
      do while (temp_count < fish_count_in_group(group_index))

        ! ランダムに魚のインデックスを選択
        call random_number(random_num)
        fish_index = int(random_num*fish_count) + 1

        ! すでにグループが割り当てられているかチェック、割り当てられていない場合は割り当てる
        if (fish_group(fish_index) == 0) then
          fish_group(fish_index) = group_index

          ! このグループでの魚の数をカウント
          temp_count = temp_count + 1
          ! 全体での割り当て済みの魚の数をカウント
          assigned_fish_count = assigned_fish_count + 1

          ! 魚の固有タイマーの初期値をランダムで設定
          call random_number(random_num)
          fish_timer(fish_index) = random_num*fish_cruise_time(group_index)
        end if

        ! 全ての魚にグループが割り当てられたら終了
        if (assigned_fish_count == fish_count) exit
      end do
    end do

  end subroutine set_fish_group

  !******************************************************************************************
  !> @brief 投入箇所のチェックおよび再配置
  !******************************************************************************************
  subroutine check_fish_initial_position()

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
    !> 投入箇所の水深
    real(8) :: supply_position_depth

    !> @brief 魚が配置できない場合の処理
    !> @param 0: 障害物セルと水深に関して配置不可の箇所で除去を行う
    !> @param 1: 障害物セルに関して配置不可の箇所で除去を行う
    !> @param 2: 障害物セルと水深に関して配置不可の箇所で再配置を行う
    !> @param 3: 障害物セルに関して配置不可の箇所で再配置を行う
    integer :: fish_handling_unable_to_place
    !> 投入可能かどうかのフラグ
    integer :: is_supplyable = 0
    !> 各グループで最小水深による再配置が可能かどうかのフラグ
    integer, dimension(:), allocatable :: is_replacable_by_critical_depth

    !> Fishトレーサーのインデックス
    integer :: fish_index
    !> Fishグループのインデックス
    integer :: fish_group_index

    !==========================================================================================
    ! GUIから条件の読み込み
    !==========================================================================================
    ! 魚の配置方法を読み込む
    ! 0: 障害物セルと水深に関して配置不可の箇所で除去を行う
    ! 1: 障害物セルに関して配置不可の箇所で除去を行う
    ! 2: 障害物セルと水深に関して配置不可の箇所で再配置を行う
    ! 3: 障害物セルに関して配置不可の箇所で再配置を行う
    call cg_iric_read_integer(cgnsOut, 'fish_handling_unable_to_place', fish_handling_unable_to_place, is_error)

    !=========================================================================================
    ! 再配置する場合で、すべて障害物セルの場合は警告をして、プログラムを終了する
    ! この処理をいれておかないと無限ループに陥る
    !=========================================================================================
    if (fish_handling_unable_to_place >= 2) then

      if (minval(obstacle_cell) == 1) then
        print *, 'Warning: There is no place to place the fish tracers.'
        write (*, '(a81)') '*********************************** Finish !! ***********************************'
        call close_cgns()
        call end_timer()
        stop
      end if
    end if

    !=========================================================================================
    ! 水深を考慮する場合は初期水深の最小値と各グループの存在可能最小水深を比較し、フラグを立てる
    ! 全部の箇所で水深が足りない場合は魚の箇所の計算をするのが無駄なので
    !=========================================================================================
    if (fish_handling_unable_to_place == 0 .or. fish_handling_unable_to_place == 2) then
      ! 配列のメモリ確保
      allocate (is_replacable_by_critical_depth(fish_group_count))
      do fish_group_index = 1, fish_group_count
        if (movable_critical_depth_fish(fish_group_index) > maxval(depth_node)) then
          print '(A,I4)', 'Warning: The minimum depth of the initial water level is less than the critical depth of the fish group ', fish_group_index
          is_replacable_by_critical_depth(fish_group_index) = 0
        end if
      end do
    end if

    !==========================================================================================
    ! 投入箇所のチェック
    !==========================================================================================
    do fish_index = 1, fish_count

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 水深を考慮する場合で、当該グループが配置可能箇所がない場合は投入箇所の水深の計算などせず除去
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (fish_handling_unable_to_place == 0 .or. fish_handling_unable_to_place == 2) then
        if (is_replacable_by_critical_depth(fish_group(fish_index)) == 0) then

          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、理由を表示
          print '(A, I4, A, I4, A)', 'Fish ', fish_index, ' in group ', fish_group(fish_index), ' is removed. Because there was no place with sufficient depth for relocation.'
          cycle

        end if
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 投入箇所のセルのインデックス、セル内の座標を計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call find_tracer_cell_index(fish_coordinate_xi(fish_index), &
                                  fish_coordinate_eta(fish_index), &
                                  supply_position_i, &
                                  supply_position_j, &
                                  supply_position_xi_in_cell, &
                                  supply_position_eta_in_cell)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 投入箇所の水深を計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      supply_position_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                  supply_position_i, &
                                                                  supply_position_j, &
                                                                  supply_position_xi_in_cell, &
                                                                  supply_position_eta_in_cell)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 条件でチェック、再配置および除去
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (fish_handling_unable_to_place == 0) then
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 障害物セルまたは最小水深以下の位置に配置された場合は除去
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 障害物による除去
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) then

          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A)', 'Fish ', fish_index, ' is removed because it is placed in the obstacle cell.'
          print '(A, F8.6, A, F8.6)', 'i = ', supply_position_i, 'j = ', supply_position_j
          cycle
        end if

        ! 最小水深以下による除去
        if (supply_position_depth < movable_critical_depth_fish(fish_group(fish_index))) then

          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A, I4,A)', 'Fish ', fish_index, 'in group', fish_group(fish_index), ' is removed because it is placed in the critical depth.'
          print '(A, F5.2, A, F5.2)', 'Fish point depth = ', supply_position_depth, 'Critical depth = ', movable_critical_depth_fish(fish_group(fish_index))
          cycle
        end if

      else if (fish_handling_unable_to_place == 1) then
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 障害物の箇所に配置された場合のみ除去する（最低水深でもそのまま配置する）
        ! ただし最小水深以下での魚の挙動が除去の場合は除去
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        ! 障害物による除去
        if (obstacle_cell(supply_position_i, supply_position_j) == 1) then

          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A)', 'Fish ', fish_index, ' is removed because it is placed in the obstacle cell.'
          print '(A, F8.6, A, F8.6)', 'i = ', supply_position_i, 'j = ', supply_position_j
          cycle
        end if

        ! 最小水深以下での挙動が除去の場合は水深でチェックして除去
        if ((supply_position_depth < movable_critical_depth_fish(fish_group(fish_index))) .and. fish_handling_in_critical_depth(fish_group(fish_index)) == 4) then

          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A, I4,A)', 'Fish ', fish_index, 'in group', fish_group(fish_index), ' is removed because it is placed in the critical depth.'
          print '(A, F5.2, A, F5.2)', 'Fish point depth = ', supply_position_depth, 'Critical depth = ', movable_critical_depth_fish(fish_group(fish_index))
          cycle
        end if

      else if (fish_handling_unable_to_place == 2) then
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 初期位置が障害物セルまたは最小水深未満の場合は再配置
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if ((obstacle_cell(supply_position_i, supply_position_j) == 1) .or. (supply_position_depth < movable_critical_depth_fish(fish_group(fish_index)))) then

          ! 条件を満たす箇所が見つかるまでランダムで再配置
          do while (is_supplyable == 0)

            ! 仮の投入地点を計算
            call random_number(supply_position_xi)
            call random_number(supply_position_eta)

            ! 新しい投入箇所のセルのインデックスを調べる
            call find_tracer_cell_index(supply_position_xi, &
                                        supply_position_eta, &
                                        supply_position_i, &
                                        supply_position_j, &
                                        supply_position_xi_in_cell, &
                                        supply_position_eta_in_cell)

            ! 新しい投入箇所の水深を計算
            supply_position_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                        supply_position_i, &
                                                                        supply_position_j, &
                                                                        supply_position_xi_in_cell, &
                                                                        supply_position_eta_in_cell)

            ! 投入箇所が障害物セルではなく、最小水深以上の場合はフラグを立ててループを抜ける
            if ((obstacle_cell(supply_position_i, supply_position_j) == 0) .and. (supply_position_depth >= movable_critical_depth_fish(fish_group(fish_index)))) then
              is_supplyable = 1
            end if

          end do

          ! 次の魚のためにフラグをリセット
          is_supplyable = 0

          ! 新しい投入地点を設定
          fish_coordinate_xi(fish_index) = supply_position_xi
          fish_coordinate_eta(fish_index) = supply_position_eta

        end if

      else if (fish_handling_unable_to_place == 3) then
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 初期位置が障害物セルの場合は再配置(最低水深はそのまま配置)
        ! ただし最小水深以下での魚の挙動が除去の場合は除去
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (obstacle_cell(supply_position_i, supply_position_j) == 1 .or. ((supply_position_depth < movable_critical_depth_fish(fish_group(fish_index))) .and. fish_handling_in_critical_depth(fish_group(fish_index)) == 4)) then

          ! 条件を満たす箇所が見つかるまでランダムで再配置
          do while (is_supplyable == 0)

            ! 仮の投入地点を計算
            call random_number(supply_position_xi)
            call random_number(supply_position_eta)

            ! 新しい投入箇所のセルのインデックスを調べる
            call find_tracer_cell_index(supply_position_xi, &
                                        supply_position_eta, &
                                        supply_position_i, &
                                        supply_position_j, &
                                        supply_position_xi_in_cell, &
                                        supply_position_eta_in_cell)

            ! 投入箇所が障害物セルではなく、最小水深以上の場合はフラグを立ててループを抜ける
            if ((obstacle_cell(supply_position_i, supply_position_j) == 0)) then

              ! 最小水深未満での魚の挙動が除去の場合以外はフラグを立ててループを抜ける、除去の場合は水位によるチェックを行う
              if (fish_handling_in_critical_depth(fish_group(fish_index)) /= 4) then
                is_supplyable = 1
              else

                ! 新しい投入箇所の水深を計算
                supply_position_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                            supply_position_i, &
                                                                            supply_position_j, &
                                                                            supply_position_xi_in_cell, &
                                                                            supply_position_eta_in_cell)

                if (supply_position_depth >= movable_critical_depth_fish(fish_group(fish_index))) then
                  is_supplyable = 1
                end if

              end if
            end if

          end do

          ! 次の魚のためにフラグをリセット
          is_supplyable = 0

          ! 新しい投入地点を設定
          fish_coordinate_xi(fish_index) = supply_position_xi
          fish_coordinate_eta(fish_index) = supply_position_eta

        end if

      end if

    end do

  end subroutine check_fish_initial_position

  !******************************************************************************************
  !> @brief 魚が障害物に入った時の処理
  !> @param[in] i セルのインデックス
  !> @param[in] j セルのインデックス
  !> @param[in] xi 魚のξ方向座標
  !> @param[in] eta 魚のη方向座標
  !> @param[in] xi_in_cell 魚のセル内ξ方向座標
  !> @param[in] eta_in_cell 魚のセル内η方向座標
  !******************************************************************************************
  subroutine fish_obstacle_handling(fish_index, i, j, xi, eta, xi_in_cell, eta_in_cell)

    !> 魚のインデックス
    integer :: fish_index
    !> 魚のいるセルのインデックス
    integer :: i
    !> 魚のいるセルのインデックス
    integer :: j
    !> 魚のξ方向座標
    real(8) :: xi
    !> 魚のη方向座標
    real(8) :: eta
    !> 魚のセル内ξ方向座標
    real(8) :: xi_in_cell
    !> 魚のセル内η方向座標
    real(8) :: eta_in_cell
    !> 魚が移動出来たことを示すフラグ
    integer :: is_moved

    ! フラグを初期化
    is_moved = 0

    if (xi_in_cell < 0.5) then
      !==========================================================================================
      ! ξ方向のセルの中心より左側にある場合
      if (eta_in_cell < 0.5) then
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! η方向のセルの中心より下側にある場合
        if (xi_in_cell > eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域1にある場合

          if (j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i, j - 1) == 0) then
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (i > 1 .and. j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j - 1) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        else if (xi_in_cell <= eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域2にある場合
          if (i > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i, j - 1) == 0) then
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i > 1 .and. j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j - 1) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        end if
      else if (eta_in_cell >= 0.5) then
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! η方向のセルの中心より上側にある場合
        if ((1 - xi_in_cell) > eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域3にある場合
          if (i > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i, j + 1) == 0) then
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i > 1 .and. j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j + 1) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        else if ((1 - xi_in_cell) <= eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域4にある場合

          if (j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i, j + 1) == 0) then
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i > 1 .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (i > 1 .and. j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i - 1, j + 1) == 0) then
              i = i - 1
              xi_in_cell = 0.08
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        end if
      end if
    else if (xi_in_cell >= 0.5) then
      !==========================================================================================
      ! ξ方向のセルの中心より右側にある場合
      if (eta_in_cell < 0.5) then
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! η方向のセルの中心より下側にある場合
        if ((xi_in_cell - 1) > eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域5にある場合

          if (j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i, j - 1) == 0) then
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j - 1) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        else if ((xi_in_cell - 1) <= eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域6にある場合

          if (i < cell_count_i .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i, j - 1) == 0) then
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. j > 1 .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j - 1) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j - 1
              eta_in_cell = 0.08
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        end if
      else if (eta_in_cell >= 0.5) then
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! η方向のセルの中心より上側にある場合
        if (xi_in_cell > eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域7にある場合

          if (i < cell_count_i .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i, j + 1) == 0) then
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j + 1) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        else if (xi_in_cell <= eta_in_cell) then
          !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          ! 領域8にある場合

          if (j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i, j + 1) == 0) then
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              is_moved = 1
            end if
          end if

          if (i < cell_count_i .and. j < cell_count_j .and. is_moved == 0) then
            if (obstacle_cell(i + 1, j + 1) == 0) then
              i = i + 1
              xi_in_cell = 0.02
              xi = (i - 1 + xi_in_cell)*grid_interval_xi
              j = j + 1
              eta_in_cell = 0.02
              eta = (j - 1 + eta_in_cell)*grid_interval_eta
              is_moved = 1
            end if
          end if

          if (is_moved == 0) then
            call remove_fish_in_obstacle(fish_index, i, j, xi, eta)
          end if

        end if
      end if
    end if

  end subroutine fish_obstacle_handling

  !******************************************************************************************
  !> @brief 魚が障害物に入り除去される場合の処理
  !******************************************************************************************
  subroutine remove_fish_in_obstacle(fish_index, i, j, xi, eta)

    !> 魚のインデックス
    integer :: fish_index
    !> 魚のいるセルのインデックス
    integer :: i
    !> 魚のいるセルのインデックス
    integer :: j
    !> 魚のξ方向座標
    real(8) :: xi
    !> 魚のη方向座標
    real(8) :: eta

    !> 魚の座標を更新
    fish_coordinate_xi(fish_index) = xi
    fish_coordinate_eta(fish_index) = eta

    !> 魚の生存フラグを更新
    is_fish_alive(fish_index) = 0

    ! 除去された魚のインデックス、座標、理由を表示
    print '(A, I4, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' is removed because it entered an obstacle and could not escape.'
    print '(A, I6, A, I6)', 'i = ', i, 'j = ', j

  end subroutine remove_fish_in_obstacle

  !******************************************************************************************
  !> @brief 魚の最小水深以下滞在時間タイマーの更新
  !> @param[in] fish_index 魚のインデックス
  !******************************************************************************************
  subroutine update_fish_timer(fish_index)

    !> 魚のインデックス
    integer :: fish_index

    ! 固有タイマーを更新
    fish_stay_time_in_critical_depth(fish_index) = fish_stay_time_in_critical_depth(fish_index) + time_interval_for_tracking

    ! 固有タイマーが最小水深での挙動をする時間を超えた場合は通常時に戻す
    if (fish_stay_time_in_critical_depth(fish_index) >= behavior_time_in_critical_depth(fish_group(fish_index))) then
      fish_stay_time_in_critical_depth(fish_index) = 0.0
    end if

  end subroutine update_fish_timer

  !******************************************************************************************
  !> @brief 任意断面を通過した魚の数を増減させるサブルーチン
  !> @param[in] previous_i 移動前の魚が存在するセルのインデックス
  !> @param[in] moved_i 移動後の魚が存在するセルのインデックス
  !> @param[in] is_periodic_moved 魚が周期境界条件による移動をしたかのフラグ
  !******************************************************************************************
  subroutine count_fish_crossing_section(previous_i, moved_i, is_periodic_moved)

    !> 移動前の魚が存在するセルのインデックス
    integer, intent(in) :: previous_i
    !> 移動後の魚が存在するセルのインデックス
    integer, intent(in) :: moved_i
    !> 魚が周期境界条件による移動をしたかのフラグ
    integer, intent(in) :: is_periodic_moved

    ! 下流から上流へ通過した場合は増加する(ただし、上流端から下流端へ通過した場合は除く)
    if (previous_i >= count_section_position .and. moved_i < count_section_position) then
      if (is_periodic_moved == 0) then
        passed_fish_count = passed_fish_count + 1
      end if
    end if

    ! 上流から下流へ通過した場合は減少する(ただし、下流端から上流端へ通過した場合は除く)
    if (previous_i < count_section_position .and. moved_i >= count_section_position) then
      if (is_periodic_moved == 0) then
        passed_fish_count = passed_fish_count - 1
      end if
    end if

  end subroutine count_fish_crossing_section

  !******************************************************************************************
  !> @brief 魚の周期境界条件による移動を処理する
  !> @param[in] fish_index 魚のインデックス
  !> @param[inout] fish_position_xi 魚のξ方向座標
  !> @param[inout] fish_position_eta 魚のη方向座標
  !> @param[out] is_periodic_moved 周期境界条件による移動をしたかのフラグ
  !******************************************************************************************
  subroutine fish_periodic_handling(fish_index, fish_position_xi, fish_position_eta, is_periodic_moved)

    !> 魚のインデックス
    integer, intent(in) :: fish_index
    !> 魚のξ方向座標
    real(8), intent(inout) :: fish_position_xi
    !> 魚のη方向座標
    real(8), intent(inout) :: fish_position_eta
    !> 魚が周期境界条件による移動をしたかのフラグ
    integer, intent(out) :: is_periodic_moved

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 側面の壁では反射する
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (fish_position_eta >= 1.0) fish_position_eta = 1.0 - (fish_position_eta - 1.0)
    if (fish_position_eta <= 0.0) fish_position_eta = -fish_position_eta

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 上下流端の周期境界条件による判定
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (is_periodic_boundary_condition_fish == 1) then
      ! 周期境界条件による移動をしたかのフラグをリセット
      is_periodic_moved = 0
      ! 周期境界条件の場合は範囲外のトレーサーを移動
      if (fish_position_xi > 1.0) then
        fish_position_xi = fish_position_xi - 1.
        is_periodic_moved = 1
      else if (fish_position_xi < 0.0) then
        fish_position_xi = fish_position_xi + 1.
        is_periodic_moved = 1
      end if
    else
      ! 周期境界じゃない場合範囲外のトレーサーは除去
      if (fish_position_xi < 0.0 .or. 1.0 + tolerance < fish_position_xi) then
        is_fish_alive(fish_index) = 0

        ! 座標を更新しておく
        fish_coordinate_xi(fish_index) = fish_position_xi
        fish_coordinate_eta(fish_index) = fish_position_eta
        ! 除去された魚のインデックス、座標、理由を表示
        print '(A, I4, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' was removed because it went out of range.'

      else if (fish_position_xi > 1.0 .and. fish_position_xi < 1.0 + tolerance) then
        ! 精度の誤差によりちょっぴりはみ出ている場合は修正
        fish_position_xi = 1.0
      end if

    end if

  end subroutine fish_periodic_handling

  !******************************************************************************************
  !> @brief 魚トレーサーの位置を更新する
  !> @param[in] time_trace 一度トレーサーが移動した後の時刻
  !******************************************************************************************
  subroutine move_fish_tracer(time_trace)

    !> 一度トレーサーが移動した後の時刻
    real(8), intent(in) :: time_trace

    !> 魚のインデックス
    integer :: fish_index

    !> 移動前の魚の存在するセルのインデックス
    integer :: fish_position_i
    !> 移動前の魚の存在するセルのインデックス
    integer :: fish_position_j
    !> 移動前の魚の存在するセル内のξ方向座標
    real(8) :: fish_position_xi_in_cell
    !> 移動前の魚の存在するセル内のη方向座標
    real(8) :: fish_position_eta_in_cell
    !> 移動前の魚の位置の水深
    real(8) :: fish_point_depth
    !> 移動前の魚の位置の水位
    real(8) :: fish_point_water_level
    !> 移動前の魚の位置の物理座標での流速
    real(8) :: fish_point_velocity_x
    !> 移動前の魚の位置の物理座標での流速
    real(8) :: fish_point_velocity_y
    !> 移動前の魚の位置の流速の絶対値
    real(8) :: fish_point_velocity_magunitude
    !> x方向の変位を ξ方向に変換するための変換行列の要素
    real(8) :: fish_point_x_to_xi_component
    !> y方向の変位を ξ方向に変換するための変換行列の要素
    real(8) :: fish_point_y_to_xi_component
    !> x方向の変位を η方向に変換するための変換行列の要素
    real(8) :: fish_point_x_to_eta_component
    !> y方向の変位を η方向に変換するための変換行列の要素
    real(8) :: fish_point_y_to_eta_component
    !> 魚のポイントの流向
    real(8) :: fish_point_flow_angle

    !> 魚の移動後の座標
    real(8) :: moved_position_xi
    !> 魚の移動後の座標
    real(8) :: moved_position_eta
    !> 移動後の魚の存在するセルのインデックス
    integer :: moved_position_i
    !> 移動後の魚の存在するセルのインデックス
    integer :: moved_position_j
    !> 移動後の魚の存在するセル内のξ方向座標
    real(8) :: moved_position_xi_in_cell
    !> 移動後の魚の存在するセル内のη方向座標
    real(8) :: moved_position_eta_in_cell
    !> 移動後の魚の位置の水深
    real(8) :: moved_fish_point_depth

    !> ひとつ上のセルの水位
    real(8) :: upper_point_water_level

    !> トレーサー地点の渦動粘性係数
    real(8) :: tracer_point_eddy_viscosity_coefficient
    !> ランダムウォークによる移動の速度
    real(8) :: diffusion_move_speed_x
    !> ランダムウォークによる移動の速度
    real(8) :: diffusion_move_speed_y
    !> ランダムウォークによる移動距離の標準偏差
    real(8) :: diffusion_std_dev
    !> 魚の泳ぐ速度
    real(8) :: fish_swim_speed
    !> 魚の泳ぐ速度
    real(8) :: fish_swim_speed_x
    !> 魚の泳ぐ速度
    real(8) :: fish_swim_speed_y
    !> 魚の移動に用いられる合計速度
    real(8) :: fish_net_speed_x
    !> 魚の移動に用いられる合計速度
    real(8) :: fish_net_speed_y
    !> 魚の移動に用いられる合計速度
    real(8) :: fish_net_speed_xi
    !> 魚の移動に用いられる合計速度
    real(8) :: fish_net_speed_eta
    !> 魚が最小水深以下での挙動をするかのフラグ
    integer :: is_fish_in_critical_depth

    ! ボックスミュラーによる正規分布乱数用変数
    !> 正規分布乱数
    real(8) :: bm_standard_normal_cos
    !> 正規分布乱数
    real(8) :: bm_standard_normal_sin

    !> 乱数
    real(8) :: rand_num

    !> 魚の遊泳サイクル内での時間
    real(8) :: fish_timer_in_cycle

    !> 周期境界条件による移動をしたかのフラグ
    integer :: is_periodic_moved

    !> 魚が突進モードかのフラグ
    integer :: is_fish_rush_mode

    do fish_index = 1, fish_count

      !==========================================================================================
      ! 魚の移動前に魚の状態を確認する
      !==========================================================================================

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の生存フラグが立っていない場合はスキップ
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (is_fish_alive(fish_index) == 0) cycle

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の挙動が前回から引き続き最小水深以下での挙動をするかのフラグをチェック
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 一度フラグをリセット
      is_fish_in_critical_depth = 0

      ! 最小水深以下での行動タイマーがスタートしていて行動継続時間内の場合フラグを立てる
      if (fish_stay_time_in_critical_depth(fish_index) > 0.0 .and. fish_stay_time_in_critical_depth(fish_index) < behavior_time_in_critical_depth(fish_group(fish_index))) then
        is_fish_in_critical_depth = 1
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の位置のインデックスを計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動前の魚の存在するセルのインデックス、セル内の座標を計算
      call find_tracer_cell_index(fish_coordinate_xi(fish_index), &
                                  fish_coordinate_eta(fish_index), &
                                  fish_position_i, &
                                  fish_position_j, &
                                  fish_position_xi_in_cell, &
                                  fish_position_eta_in_cell)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の位置の水深や流速を計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動前の魚の位置の水深を計算
      fish_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                             fish_position_i, &
                                                             fish_position_j, &
                                                             fish_position_xi_in_cell, &
                                                             fish_position_eta_in_cell)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動前の魚の位置で最小水深以下の場合の処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (fish_point_depth < movable_critical_depth_fish(fish_group(fish_index))) then

        if (fish_handling_in_critical_depth(fish_group(fish_index)) == 4) then ! 除去される場合

          ! 魚の生存フラグを更新
          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' was removed because it stayed in the critical depth.'

          ! 魚は除去されるので次の魚へ
          cycle

        else ! その他の挙動の場合

          ! 魚の固有タイマーを更新
          call update_fish_timer(fish_index)

          ! 魚は動かないので次の魚へ
          cycle

        end if

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の遊泳・突進サイクル内での時間を計算　突進モードかどうかを判定
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      fish_timer_in_cycle = mod(fish_timer(fish_index), fish_cycle_time(fish_group(fish_index))) + tolerance

      if (fish_timer_in_cycle > fish_cruise_time(fish_group(fish_index))) then ! ! 突進状態の場合
        is_fish_rush_mode = 1
      else ! 遊泳状態の場合
        is_fish_rush_mode = 0
      end if

      ! fish_timer(fish_index)を使う部分は終わったので更新しておく
      fish_timer(fish_index) = fish_timer(fish_index) + time_interval_for_tracking

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の泳ぐ速度の基準値を決定
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (is_fish_rush_mode == 1) then ! 突進状態の場合

        fish_swim_speed = fish_rush_speed(fish_group(fish_index))

      else ! 遊泳状態の場合

        fish_swim_speed = fish_cruise_speed(fish_group(fish_index))

      end if

      !==========================================================================================
      ! ジャンプするかどうかの判定
      ! 0. 魚がジャンプモードでない場合はスキップ
      ! 1. 魚が突進モードかつ突進モードに入ってから2回目の移動までの間
      ! 2. 魚の現在位置とひとつ上のセルの水位差がジャンプ検討水位差以上かつ、ジャンプ可能水位差以内
      ! 3. 最小水深以下の動作をしない場合
      ! 4. 一番上流のセルにいる場合はジャンプしない
      !==========================================================================================
      if (is_jump_fish == 1 .and. is_fish_in_critical_depth == 0 .and. fish_position_i > 1) then
        if (is_fish_rush_mode == 1 .and. fish_timer_in_cycle <= fish_cruise_time(fish_group(fish_index)) + time_interval_for_tracking*2 + tolerance) then

          ! 魚の現在位置での水位を計算
          fish_point_water_level = calculate_scalar_at_tracer_position(water_level_node, &
                                                                       fish_position_i, &
                                                                       fish_position_j, &
                                                                       fish_position_xi_in_cell, &
                                                                       fish_position_eta_in_cell)

          ! ひとつ上のセルの水位を計算
          upper_point_water_level = calculate_scalar_at_tracer_position(water_level_node, &
                                                                        fish_position_i - 1, &
                                                                        fish_position_j, &
                                                                        fish_position_xi_in_cell, &
                                                                        fish_position_eta_in_cell)

          ! 魚の現在位置とひとつ上のセルの水位差がジャンプ検討水位差以上
          if (upper_point_water_level - fish_point_water_level >= jump_try_height(fish_group(fish_index))) then

            ! ジャンプ検討に入ったのでジャンプ検討フラグを立てる
            ! ジャンプしたら1になるが、検討してジャンプしなければ-1のまま
            fish_jump_flag(fish_index) = -1

            !ジャンプ可能水位差以内の場合ジャンプする、水位差が大きい場合はジャンプせずその場に留まる
            if (upper_point_water_level - fish_point_water_level <= jumpable_height(fish_group(fish_index))) then

              ! ジャンプしたのでジャンプ成功フラグを立てる
              fish_jump_flag(fish_index) = 1
              ! ジャンプの処理
              call jump_fish(time_trace, fish_index, fish_position_i, fish_position_j, fish_position_xi_in_cell, fish_position_eta_in_cell)
              ! ジャンプした後に移動しない場合はこのcycleで終了、ジャンプ後に移動する場合はcycleをコメントアウト
              cycle

            else if (fish_handling_when_jumping_failed(fish_group(fish_index)) == 0) then

              ! ジャンプに失敗した場合、その場に留まる
              fish_angle(fish_index) = fish_angle(fish_index)
              ! 移動しないので次の魚へ
              cycle

            end if

          end if
        end if
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動前の魚の位置での流速(物理座標)を計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の位置の流速を計算(物理座標)
      fish_point_velocity_x = calculate_scalar_at_tracer_position(velocity_x_node, &
                                                                  fish_position_i, &
                                                                  fish_position_j, &
                                                                  fish_position_xi_in_cell, &
                                                                  fish_position_eta_in_cell)
      fish_point_velocity_y = calculate_scalar_at_tracer_position(velocity_y_node, &
                                                                  fish_position_i, &
                                                                  fish_position_j, &
                                                                  fish_position_xi_in_cell, &
                                                                  fish_position_eta_in_cell)

      ! 魚の位置の流速の絶対値を計算
      fish_point_velocity_magunitude = sqrt(fish_point_velocity_x**2 + fish_point_velocity_y**2)

      ! 物理座標における魚の位置の流向を計算(0~2π)
      if (fish_point_velocity_magunitude /= 0.0) then
        fish_point_flow_angle = mod(atan2(fish_point_velocity_y, fish_point_velocity_x) + 2*pi, 2*pi)
      else
        fish_point_flow_angle = fish_angle(fish_index)
      end if

      ! x方向の変位を ξ方向に変換するための変換行列の要素
      fish_point_x_to_xi_component = calculate_scalar_at_tracer_position(x_to_xi_component, &
                                                                         fish_position_i, &
                                                                         fish_position_j, &
                                                                         fish_position_xi_in_cell, &
                                                                         fish_position_eta_in_cell)
      ! y方向の変位を ξ方向に変換するための変換行列の要素
      fish_point_y_to_xi_component = calculate_scalar_at_tracer_position(y_to_xi_component, &
                                                                         fish_position_i, &
                                                                         fish_position_j, &
                                                                         fish_position_xi_in_cell, &
                                                                         fish_position_eta_in_cell)
      ! x方向の変位を η方向に変換するための変換行列の要素
      fish_point_x_to_eta_component = calculate_scalar_at_tracer_position(x_to_eta_component, &
                                                                          fish_position_i, &
                                                                          fish_position_j, &
                                                                          fish_position_xi_in_cell, &
                                                                          fish_position_eta_in_cell)
      ! y方向の変位を η方向に変換するための変換行列の要素
      fish_point_y_to_eta_component = calculate_scalar_at_tracer_position(y_to_eta_component, &
                                                                          fish_position_i, &
                                                                          fish_position_j, &
                                                                          fish_position_xi_in_cell, &
                                                                          fish_position_eta_in_cell)

      !==========================================================================================
      ! ランダムウォークによる変位量、速度を計算
      !==========================================================================================
      ! トレーサー地点の渦動粘性係数を計算
      tracer_point_eddy_viscosity_coefficient = calculate_scalar_at_tracer_position( &
                                                eddy_viscosity_coefficient_node, &
                                                fish_position_i, &
                                                fish_position_j, &
                                                fish_position_xi_in_cell, &
                                                fish_position_eta_in_cell)

      ! ランダムウォークによる移動距離の標準偏差
      diffusion_std_dev = sqrt(2*(a_diff*tracer_point_eddy_viscosity_coefficient + b_diff)*time_interval_for_tracking)

      ! 縦方向横方向の正規分布乱数を取得
      call generate_box_muller_random(bm_standard_normal_cos, bm_standard_normal_sin)

      ! ランダムウォークによる移動の流速を計算(移動距離/タイムステップ)
      diffusion_move_speed_x = bm_standard_normal_cos*diffusion_std_dev/time_interval_for_tracking
      diffusion_move_speed_y = bm_standard_normal_sin*diffusion_std_dev/time_interval_for_tracking

      !==========================================================================================
      ! 魚の状態によって魚のアングルと最終的な移動速度を決定
      !==========================================================================================
      if (is_fish_in_critical_depth == 1) then
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 最小水深以下での挙動をする場合
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! 魚の固有タイマーを更新
        call update_fish_timer(fish_index)

        if (fish_handling_in_critical_depth(fish_group(fish_index)) == 0) then ! その場で動かない場合

          ! 魚は動かないので次の魚へ
          cycle

        else if (fish_handling_in_critical_depth(fish_group(fish_index)) == 1) then  ! 逆方向に泳ぐ場合

          ! 魚の向きを流向と同じ方向に設定（本来は逆方向に泳ぐが、ここでは逆方向に泳ぐという意味を持たせる）
          fish_angle(fish_index) = fish_point_flow_angle

        else if (fish_handling_in_critical_depth(fish_group(fish_index)) == 2) then   ! 流れに身を任せる場合

          ! 魚の向きは通常時と同様に流向と逆方向に設定
          fish_angle(fish_index) = mod(fish_point_flow_angle + pi, 2*pi)

          ! 魚は流れに身を任せるため、泳ぐ速度は0
          fish_swim_speed = 0.0

        else if (fish_handling_in_critical_depth(fish_group(fish_index)) == 3) then   ! ランダムに泳ぐ場合

          ! 魚の向きは発動時に変更し、最小水深行動中は維持なので更新しない
          ! fish_angle(fish_index) = fish_angle(fish_index)

        end if

      else
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 通常時
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        ! 魚は遡上するので向きを流向と逆方向に設定
        fish_angle(fish_index) = mod(fish_point_flow_angle + pi, 2*pi)

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の向きに基づいて魚の遊泳速度を設定
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      fish_swim_speed_x = fish_swim_speed*cos(fish_angle(fish_index))
      fish_swim_speed_y = fish_swim_speed*sin(fish_angle(fish_index))

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の移動に関する各成分を合成
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      fish_net_speed_x = fish_point_velocity_x + diffusion_move_speed_x + fish_swim_speed_x
      fish_net_speed_y = fish_point_velocity_y + diffusion_move_speed_y + fish_swim_speed_y

      !==========================================================================================
      ! 魚の位置を更新
      !==========================================================================================

      ! 一般座標における魚の速度を計算
      fish_net_speed_xi = fish_point_x_to_xi_component*fish_net_speed_x + fish_point_y_to_xi_component*fish_net_speed_y
      fish_net_speed_eta = fish_point_x_to_eta_component*fish_net_speed_x + fish_point_y_to_eta_component*fish_net_speed_y

      ! 移動後の魚の一般座標を計算
      moved_position_xi = fish_coordinate_xi(fish_index) + fish_net_speed_xi*time_interval_for_tracking
      moved_position_eta = fish_coordinate_eta(fish_index) + fish_net_speed_eta*time_interval_for_tracking

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 周期境界条件での移動のチェック、移動
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call fish_periodic_handling(fish_index, moved_position_xi, moved_position_eta, is_periodic_moved)
      ! 周期境界条件の中で魚が除去された場合は次の魚へ
      if (is_fish_alive(fish_index) == 0) cycle

      !==========================================================================================
      ! 移動後の魚の位置のセルのインデックス、セル内の座標を計算
      !==========================================================================================

      call find_tracer_cell_index(moved_position_xi, &
                                  moved_position_eta, &
                                  moved_position_i, &
                                  moved_position_j, &
                                  moved_position_xi_in_cell, &
                                  moved_position_eta_in_cell)

      !==========================================================================================
      ! 移動後の魚の位置の水深による処理
      !==========================================================================================

      ! 移動後の魚の位置の水深を計算
      moved_fish_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                   moved_position_i, &
                                                                   moved_position_j, &
                                                                   moved_position_xi_in_cell, &
                                                                   moved_position_eta_in_cell)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 移動後の魚の位置が最小水深以下の場合、魚の位置を移動前に戻して挙動に応じた位置に再移動する
      ! ただし、再配置後の位置が最小水深以下の場合はそのまま移動しない
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (moved_fish_point_depth < movable_critical_depth_fish(fish_group(fish_index))) then

        ! 魚の位置を移動前に戻す
        moved_position_xi = fish_coordinate_xi(fish_index)
        moved_position_eta = fish_coordinate_eta(fish_index)
        call find_tracer_cell_index(moved_position_xi, &
                                    moved_position_eta, &
                                    moved_position_i, &
                                    moved_position_j, &
                                    moved_position_xi_in_cell, &
                                    moved_position_eta_in_cell)

        ! すでに限界水深以下での動きをしている場合でも再度行動し直すのでタイマーをリセットする。
        fish_stay_time_in_critical_depth(fish_index) = 0.0

        ! 魚の固有タイマーを更新
        call update_fish_timer(fish_index)

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 再移動のために挙動に応じて魚の向きを変える
        ! 除去、停止の場合の処理もここで行う
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (fish_handling_in_critical_depth(fish_group(fish_index)) == 0) then ! その場で動かない場合

          ! 魚は動かないので次の魚へ
          cycle

        else if (fish_handling_in_critical_depth(fish_group(fish_index)) == 3) then ! ランダムに泳ぐ場合

          ! 魚の方向を±90度の範囲を標準偏差とする正規分布に従う乱数で変更(標準偏差pi/2.0)
          call random_number(rand_num)
          fish_angle(fish_index) = fish_angle(fish_index) + pi/2.0*sqrt(-2.0*log(rand_num))*cos(2.0*pi*rand_num)

        else if (fish_handling_in_critical_depth(fish_group(fish_index)) == 4) then   ! 除去される場合

          ! 魚の生存フラグを更新
          is_fish_alive(fish_index) = 0

          ! 除去された魚のインデックス、座標、理由を表示
          print '(A, I4, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' was removed because it stayed in the critical depth.'

          ! 魚は除去されるので次の魚へ
          cycle

        end if

      end if

      !==========================================================================================
      ! 移動後の魚の位置が障害物セルにある場合は近くに移動する
      !==========================================================================================
      if (obstacle_cell(moved_position_i, moved_position_j) == 1) then

        call fish_obstacle_handling(fish_index, &
                                    moved_position_i, moved_position_j, &
                                    moved_position_xi, moved_position_eta, &
                                    moved_position_xi_in_cell, moved_position_eta_in_cell)

        ! 障害物処理で除去された場合は次の魚へ
        if (is_fish_alive(fish_index) == 0) cycle

        ! 障害物処理で移動した場合かつ最小水深以下での魚の挙動が除去の場合はチェックを行う
        if (fish_handling_in_critical_depth(fish_group(fish_index)) == 4) then

          ! 移動後の魚の位置の水深を計算
          moved_fish_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                       moved_position_i, &
                                                                       moved_position_j, &
                                                                       moved_position_xi_in_cell, &
                                                                       moved_position_eta_in_cell)

          ! 移動後の魚の位置が最小水深以下の場合は除去
          if (moved_fish_point_depth < movable_critical_depth_fish(fish_group(fish_index))) then
            ! 魚の生存フラグを更新
            is_fish_alive(fish_index) = 0

            ! 除去された魚のインデックス、座標、理由を表示
            print '(A, I4, A, F10.5, A, F10.5)', 'Fish ', fish_index, ' was removed because it stayed in the critical depth.'

            ! 魚は除去されるので次の魚へ
            cycle
          end if

        end if

      end if

      !==========================================================================================
      ! 魚の位置を更新
      !==========================================================================================
      fish_coordinate_xi(fish_index) = moved_position_xi
      fish_coordinate_eta(fish_index) = moved_position_eta

      !==========================================================================================
      ! 任意断面を通過した魚の数をカウントする場合の処理
      !==========================================================================================
      if (is_count_fish == 1) then
        if (count_time_start <= time_trace + tolerance .and. time_trace <= count_time_end + tolerance) then
          call count_fish_crossing_section(fish_position_i, moved_position_i, is_periodic_moved)
        end if
      end if
    end do

  end subroutine move_fish_tracer

  !******************************************************************************************
  !> @brief 魚のジャンプ処理
  !> @param[in] time_trace トレーサー追跡用の現在の時間
  !> @param[in] fish_index 魚のインデックス
  !> @param[inout] fish_position_i 魚の存在するセルのインデックス
  !> @param[inout] fish_position_j 魚の存在するセルのインデックス
  !> @param[inout] fish_position_xi_in_cell 魚の存在するセル内のξ方向座標
  !> @param[inout] fish_position_eta_in_cell 魚の存在するセル内のη方向座標
  !******************************************************************************************
  subroutine jump_fish(time_trace, fish_index, fish_position_i, fish_position_j, fish_position_xi_in_cell, fish_position_eta_in_cell)

    !> 一度トレーサーが移動した後の時刻
    real(8), intent(in) :: time_trace

    !> 魚のインデックス
    integer, intent(in) :: fish_index
    !> 魚の存在するセルのインデックス
    integer, intent(inout) :: fish_position_i
    !> 魚の存在するセルのインデックス
    integer, intent(inout) :: fish_position_j
    !> 魚の存在するセル内のξ方向座標
    real(8), intent(inout) :: fish_position_xi_in_cell
    !> 魚の存在するセル内のη方向座標
    real(8), intent(inout) :: fish_position_eta_in_cell

    !> 魚のジャンプ後の位置の一般座標
    real(8) :: jumped_position_xi
    !> 魚のジャンプ後の位置の一般座標
    real(8) :: jumped_position_eta
    !> 魚のジャンプ後の位置のセルのインデックス
    integer :: jumped_position_i
    !> 魚のジャンプ後の位置のセルのインデックス
    integer :: jumped_position_j
    !> 魚のジャンプ後の位置のセル内のξ方向座標
    real(8) :: jumped_position_xi_in_cell
    !> 魚のジャンプ後の位置のセル内のη方向座標
    real(8) :: jumped_position_eta_in_cell
    !> 魚のジャンプ後の位置の水深
    real(8) :: jumped_fish_point_depth

    !> 魚の位置のxi方向スケーリングファクタ
    real(8) :: fish_point_scale_factor_xi

    !> 魚のジャンプ一般座標系距離
    real(8) :: jumpable_distance_xi

    !> 境界条件による移動のフラグ
    integer :: is_periodic_moved

    !==========================================================================================
    ! 魚のジャンプによる移動距離を計算
    ! 物理座標系距離(jumpable_distance)を一般座標系距離に変換
    !==========================================================================================

    ! 魚の位置でのスケーリングファクタを計算
    fish_point_scale_factor_xi = (scale_factor_xi(fish_position_i, fish_position_j + 1)* &
                                  fish_position_eta_in_cell + &
                                  scale_factor_xi(fish_position_i, fish_position_j)* &
                                  (grid_interval_eta - fish_position_eta_in_cell)) &
                                 *inverse_grid_interval_eta

    ! 魚の地点でのxi方向ジャンプ距離を計算
    jumpable_distance_xi = jumpable_distance(fish_group(fish_index))*fish_point_scale_factor_xi

    !==========================================================================================
    ! 魚のジャンプ後の位置を計算
    !==========================================================================================
    jumped_position_xi = fish_coordinate_xi(fish_index) - jumpable_distance_xi
    jumped_position_eta = fish_coordinate_eta(fish_index)

    !==========================================================================================
    ! 境界条件による処理
    !==========================================================================================
    call fish_periodic_handling(fish_index, jumped_position_xi, jumped_position_eta, is_periodic_moved)
    ! 周期境界条件の中で魚が除去された場合は次の魚へ
    if (is_fish_alive(fish_index) == 0) return

    !==========================================================================================
    ! 移動後の場所でのチェック・処理
    !==========================================================================================

    ! 魚のジャンプ後の位置のセルのインデックス、セル内の座標を計算
    call find_tracer_cell_index(jumped_position_xi, &
                                jumped_position_eta, &
                                jumped_position_i, &
                                jumped_position_j, &
                                jumped_position_xi_in_cell, &
                                jumped_position_eta_in_cell)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 障害物セルに関する処理
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 魚のジャンプ後の位置が障害物セルにある場合は近くに移動する
    if (obstacle_cell(jumped_position_i, jumped_position_j) == 1) then

      call fish_obstacle_handling(fish_index, &
                                  jumped_position_i, jumped_position_j, &
                                  jumped_position_xi, jumped_position_eta, &
                                  jumped_position_xi_in_cell, jumped_position_eta_in_cell)

      ! 障害物処理で除去された場合は次の魚へ
      if (is_fish_alive(fish_index) == 0) return

    end if

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 水深による処理
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 最小水深以下での挙動が除去された場合は水深によるチェックを行う
    if (fish_handling_in_critical_depth(fish_group(fish_index)) == 4) then

      ! 魚のジャンプ後の位置の水深を計算
      jumped_fish_point_depth = calculate_scalar_at_tracer_position(depth_node, &
                                                                    jumped_position_i, &
                                                                    jumped_position_j, &
                                                                    jumped_position_xi_in_cell, &
                                                                    jumped_position_eta_in_cell)

      ! 魚のジャンプ後の位置が最小水深以下の場合は除去
      if (jumped_fish_point_depth < movable_critical_depth_fish(fish_group(fish_index))) then
        is_fish_alive(fish_index) = 0
        return
      end if

    end if

    !==========================================================================================
    ! 任意位置での魚のカウントを行う場合、カウンターを更新
    !==========================================================================================
    if (is_count_fish == 1) then
      if (count_time_start <= time_trace + tolerance .and. time_trace <= count_time_end + tolerance) then
        call count_fish_crossing_section(fish_position_i, jumped_position_i, is_periodic_moved)
      end if
    end if

    !==========================================================================================
    ! 最終的に決定した魚のジャンプ後の位置を更新
    !==========================================================================================
    fish_coordinate_xi(fish_index) = jumped_position_xi
    fish_coordinate_eta(fish_index) = jumped_position_eta

  end subroutine jump_fish

  !******************************************************************************************
  !> @brief 魚のポリゴン形状、各魚の持つ属性の出力
  !******************************************************************************************
  subroutine output_fish()

    !> 魚のインデックス
    integer :: fish_index
    !> 魚の位置の物理座標
    real(8) :: fish_position_x
    !> 魚の位置の物理座標
    real(8) :: fish_position_y
    !> 魚の存在するセルのインデックス
    integer :: fish_position_i
    !> 魚の存在するセルのインデックス
    integer :: fish_position_j
    !> 魚の存在するセル内のξ方向座標
    real(8) :: fish_position_xi_in_cell
    !> 魚の存在するセル内のη方向座標
    real(8) :: fish_position_eta_in_cell
    !> 生存可能な魚の数
    integer :: fish_count_alive

    !==========================================================================================
    ! 魚のポリゴン形状、各魚の持つ属性の出力
    !==========================================================================================
    call cg_iric_write_sol_polydata_groupbegin(cgnsOut, "Fish", is_error)

    ! 生存魚数のカウンターをリセット
    fish_count_alive = 0

    do fish_index = 1, fish_count

      ! 魚の生存フラグが立っていない場合はスキップ
      if (is_fish_alive(fish_index) == 0) cycle

      ! 生存魚数をカウント
      fish_count_alive = fish_count_alive + 1

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の物理座標を計算してポリゴンの座標を計算・出力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の存在するセルのインデックス、セル内の座標を計算
      call find_tracer_cell_index(fish_coordinate_xi(fish_index), &
                                  fish_coordinate_eta(fish_index), &
                                  fish_position_i, &
                                  fish_position_j, &
                                  fish_position_xi_in_cell, &
                                  fish_position_eta_in_cell)

      ! 魚の位置の物理座標を計算
      call transform_general_to_physical(fish_position_i, &
                                         fish_position_j, &
                                         fish_position_xi_in_cell, &
                                         fish_position_eta_in_cell, &
                                         fish_position_x, &
                                         fish_position_y)

      ! 魚のポリゴンの形状を計算
      call make_fish_outline(fish_index, fish_position_x, fish_position_y)

      ! 魚のポリゴンを出力
      call cg_iric_write_sol_polydata_polygon(cgnsOut, fish_outline_point_count, fish_outline_x, fish_outline_y, is_error)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 魚の属性を出力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚の泳ぎのモードを計算して出力
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚の泳ぎのモードを判定
      ! 遊泳状態の場合は魚の泳ぎのモードを0、突進状態の場合は1
      if (mod(fish_timer(fish_index), fish_cycle_time(fish_group(fish_index))) + tolerance <= fish_cruise_time(fish_group(fish_index))) then
        call cg_iric_write_sol_polydata_integer(cgnsOut, "Swim Mode", 0, is_error)
      else
        call cg_iric_write_sol_polydata_integer(cgnsOut, "Swim Mode", 1, is_error)
      end if

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚が移動後の時点で最小水深以下での挙動を行うかのフラグを出力
      ! 魚が最小水深以下の挙動を行う場合は1、そうでない場合は0
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (fish_stay_time_in_critical_depth(fish_index) > 0) then
        call cg_iric_write_sol_polydata_integer(cgnsOut, "under critical depth mode", 1, is_error)
      else
        call cg_iric_write_sol_polydata_integer(cgnsOut, "under critical depth mode", 0, is_error)
      end if

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚のグループインデックスを出力
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call cg_iric_write_sol_polydata_integer(cgnsOut, "Fish Group", fish_group(fish_index), is_error)

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚のインデックスを出力（とりあえずデバッグ用として出しておく）
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      call cg_iric_write_sol_polydata_integer(cgnsOut, "Fish Index", fish_index, is_error)

      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ! 魚のジャンプ状態を出力
      ! 魚がジャンプした場合は1、ジャンプに失敗したら-1、そうでない場合は0
      !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (is_jump_fish == 1) then
        call cg_iric_write_sol_polydata_integer(cgnsOut, "Jump Mode", fish_jump_flag(fish_index), is_error)
        ! 出力したらフラグをリセット
        fish_jump_flag(fish_index) = 0
      end if

    end do

    ! 魚のポリゴンのグループの出力終了を宣言
    call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)

    !==========================================================================================
    ! 統計など時系列で単一の属性を出力
    !==========================================================================================
    ! 生存魚数を出力
    call cg_iric_write_sol_baseiterative_integer(cgnsOut, "Number of Alive Fish", fish_count_alive, is_error)
    ! 任意断面を通過した魚の数を出力
    if (is_count_fish == 1) then
      call cg_iric_write_sol_baseiterative_integer(cgnsOut, "Number of Fish passed through the section", passed_fish_count, is_error)
    end if

    !==========================================================================================
    ! コンソールに魚の数を出力
    !==========================================================================================
    write (*, *) "Number of Alive Fish: ", fish_count_alive

  end subroutine output_fish

  !******************************************************************************************
  !> @brief 魚のポリゴン形状を計算する
  !******************************************************************************************
  subroutine make_fish_outline(fish_index, fish_position_x, fish_position_y)

    !> 魚のインデックス（個別の魚を識別するため）
    integer, intent(in) :: fish_index
    !> 魚の基準位置（物理座標）X座標
    real(8), intent(inout) :: fish_position_x
    !> 魚の基準位置（物理座標）Y座標
    real(8), intent(inout) :: fish_position_y

    !==========================================================================================
    ! 魚のアウトライン基本形状を決定する波の基本パラメータ
    !==========================================================================================
    !> 魚のアウトラインに使用される波の波長λ（魚が1波長分くねるのに必要な長さ）
    real(8) :: body_wavelength
    !> 魚のアウトラインに使用される波の振幅（波の振れ幅）
    real(8) :: body_amplitude
    !> 中心線からの左右方向の変位（魚の体の形状を作る基準）
    real(8) :: outline_offset_body

    !==========================================================================================
    ! 魚のアウトライン形状を時間で変化させるための波のパラメータ
    !==========================================================================================
    !> 時間周期のスケール調整係数（魚の体長によって時間周期を調整する際の係数）
    real(8) :: time_period_scaling_factor = 2.2
    !> 時間で変化する波の時間周期T（1回のくねりにかかる時間）(魚の体長×time_period_scaling_factor)
    real(8) :: oscillation_time_period
    !> 時間で変化する波の基本振幅
    real(8) :: time_dependent_amplitude
    !> 身体の位置によって振幅を変化させるための補正係数（魚の体の後方ほど振幅が大きくなる）
    real(8) :: wave_attenuation_factor
    !> 中心線からの左右方向の時間による変位（魚の基準形状に変化を与える）
    real(8) :: outline_offset_time

    !==========================================================================================
    ! 魚のアウトライン形状を計算するためのパラメータ
    !==========================================================================================
    !> 魚の体の中心線上の位置(中心線からのアウトラインまでの距離を求める際の基準位置)
    real(8) :: position_on_centerline
    !> 魚の中心線の尻尾側の位置
    real(8) :: centerline_tail
    !> 魚の中心線の頭側の位置
    real(8) :: centerline_head
    !> 中心線の分割間隔
    real(8) :: centerline_segment_length
    !> 中心線の頭側からposition_on_centerlineまでの距離（波の振幅を減衰させるために使用）
    real(8) :: distance_from_head
    !> 最終的なアウトラインの変位（左右のアウトラインを決定する）
    real(8) :: outline_adjustment
    !> 魚の体を分割して各セグメントの位置を計算するためのループカウンタ
    integer :: centerline_segment_index
    !> アウトラインの頂点インデックス
    integer :: outline_point_index

    !==========================================================================================
    ! 計算に使用するパラメータを魚の体長から決定
    !==========================================================================================
    ! 魚の波打つ動きの時間周期を決定（魚の体長にスケール係数をかける）
    oscillation_time_period = fish_body_length(fish_group(fish_index))*time_period_scaling_factor
    ! 魚の波打つ動きの空間周期（魚の体全体が波形を描く長さ）
    body_wavelength = fish_body_length(fish_group(fish_index))*9.0/5.0
    ! 波の最大振幅（魚のくねりの最大幅）
    body_amplitude = body_wavelength/20.0
    ! 時間による振幅変化（時間依存の変位は基準の振幅に対して60%の範囲で行われる）
    time_dependent_amplitude = body_amplitude*0.6
    ! 波の開始位置（魚の体の中心線上の最も後ろ側）
    centerline_tail = body_wavelength*(-3.0/8.0)
    ! 波の終了位置（魚の体の中心線上の最も前側）
    centerline_head = body_wavelength*(1.0/4.0)
    ! 中心線の分割間隔（開始から終了までの距離を分割数で割る）
    centerline_segment_length = (centerline_head - centerline_tail)/centerline_segments

    !==========================================================================================
    ! 魚のアウトライン形状を計算
    !==========================================================================================
    ! 頂点インデックスを初期化
    outline_point_index = 0

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 魚の尾から頭に向かって右側のアウトラインを計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    do centerline_segment_index = 0, centerline_segments

      ! 頂点インデックスを更新
      outline_point_index = outline_point_index + 1

      ! 中心線上の現在位置を計算
      position_on_centerline = centerline_tail + centerline_segment_length*centerline_segment_index
      ! 中心線の頭側からの距離を計算
      distance_from_head = centerline_head - position_on_centerline
      ! 現在の位置での波の減衰率を計算（体の後方ほど小さくなる）
      wave_attenuation_factor = distance_from_head/fish_body_length(fish_group(fish_index))

      ! 魚のアウトラインの形状を決定するための基本形状（波の振幅）
      outline_offset_body = abs(body_amplitude*cos(2.0*pi/body_wavelength*position_on_centerline))

      ! 時間変化による波の変位（この値によって魚のうねりを表現する）
      ! sin(2.0*pi/oscillation_time_period*fish_timer(fish_index))によってこの時間での振幅を決定しているが、
      ! - time_period_scaling_factor*distance_from_head によって位相をずらすことで空間的な振幅の変化を与えている
      ! また、wave_attenuation_factorによって尻尾側ほど振幅が大きくなるようにしている
      outline_offset_time = time_dependent_amplitude &
                            *sin(2.0*pi/oscillation_time_period*(fish_timer(fish_index) - time_period_scaling_factor*distance_from_head)) &
                            *wave_attenuation_factor

      ! 最終的な中心線からの変位の計算
      outline_adjustment = outline_offset_time - outline_offset_body

      ! 魚のアングルによる回転を考慮したアウトラインの座標を計算
      fish_outline_x(outline_point_index) = fish_position_x &
                                            + cos(fish_angle(fish_index))*position_on_centerline &
                                            - sin(fish_angle(fish_index))*outline_adjustment
      fish_outline_y(outline_point_index) = fish_position_y &
                                            + sin(fish_angle(fish_index))*position_on_centerline &
                                            + cos(fish_angle(fish_index))*outline_adjustment
    end do

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 魚の頭で折り返して尾に向かって左側のアウトラインを計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    do centerline_segment_index = 0, centerline_segments
      ! 頂点インデックスを更新
      outline_point_index = outline_point_index + 1

      ! 中心線上の現在位置を計算
      position_on_centerline = centerline_head - centerline_segment_length*centerline_segment_index
      ! 中心線の頭側からの距離を計算
      distance_from_head = centerline_head - position_on_centerline
      ! 現在の位置での波の減衰率を計算（体の後方ほど小さくなる）
      wave_attenuation_factor = distance_from_head/fish_body_length(fish_group(fish_index))

      ! 魚のアウトラインの形状を決定するための基本形状（波の振幅）
      outline_offset_body = abs(body_amplitude*cos(2.0*pi/body_wavelength*position_on_centerline))

      ! 時間変化による波の変位（この値によって魚のうねりを表現する）
      ! sin(2.0*pi/oscillation_time_period*fish_timer(fish_index))によってこの時間での振幅を決定しているが、
      ! - time_period_scaling_factor*distance_from_head によって位相をずらすことで空間的な振幅の変化を与えている
      ! また、wave_attenuation_factorによって尻尾側ほど振幅が大きくなるようにしている
      outline_offset_time = time_dependent_amplitude &
                            *sin(2.0*pi/oscillation_time_period*(fish_timer(fish_index) - time_period_scaling_factor*distance_from_head)) &
                            *wave_attenuation_factor

      ! 最終的な中心線からの変位の計算
      outline_adjustment = outline_offset_time + outline_offset_body

      ! 魚のアングルによる回転を考慮したアウトラインの座標を計算
      fish_outline_x(outline_point_index) = fish_position_x &
                                            + cos(fish_angle(fish_index))*position_on_centerline &
                                            - sin(fish_angle(fish_index))*outline_adjustment
      fish_outline_y(outline_point_index) = fish_position_y &
                                            + sin(fish_angle(fish_index))*position_on_centerline &
                                            + cos(fish_angle(fish_index))*outline_adjustment
    end do

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !  最初の頂点の座標を最後の頂点にコピーしてポリゴンを閉じる
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    outline_point_index = outline_point_index + 1
    fish_outline_x(outline_point_index) = fish_outline_x(1)
    fish_outline_y(outline_point_index) = fish_outline_y(1)

  end subroutine make_fish_outline

end module fish_module
