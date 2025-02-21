module landscape_poly

  use iric
  use common
  use grid
  use result
  use trace

  implicit none

  type :: object_poly
    !==========================================================================================
    ! オブジェクトの共通情報
    !==========================================================================================
    !> @brief オブジェクトを描画するセルの基準
    !> @param 1: 指定した水深より小さいセルにオブジェクトを描画
    !> @param 2: 指定したセルにオブジェクトを描画
    integer :: drawing_target
    !> オブジェクトを描画するセルの水深
    real(8) :: drawable_depth
    !> オブジェクトの基準サイズ
    real(8) :: base_size
    !> @brief オブジェクトのサイズにバリエーションを与えるかどうか
    !> @param 0: 与えない
    !> @param 1: 与える
    integer :: is_apply_variation_size
    !> オブジェクトの描画間隔
    integer :: draw_interval_i
    !> オブジェクトの描画間隔
    integer :: draw_interval_j

    !> オブジェクトの最大数
    integer :: max_object_count
    !> オブジェクトの描画位置のセルのインデックス
    integer, dimension(:), allocatable :: draw_point_index_i
    !> オブジェクトの描画位置のセルのインデックス
    integer, dimension(:), allocatable :: draw_point_index_j
    !> オブジェクトの描画位置の座標
    real(8), dimension(:), allocatable :: draw_point_coordinate_x
    !> オブジェクトの描画位置の座標
    real(8), dimension(:), allocatable :: draw_point_coordinate_y
    !> オブジェクトサイズ
    real(8), dimension(:), allocatable :: object_size
  end type object_poly

  ! 樹木のオブジェクトの構造体を宣言
  type(object_poly) :: tree
  ! 礫のオブジェクトの構造体を宣言
  type(object_poly) :: gravel

  !******************************************************************************************
  ! 樹木に関するパラメータ
  !******************************************************************************************
  !> 樹木のアスペクト比
  real(8) :: tree_aspect_ratio

  !> 葉部分のアウトライン頂点数
  integer :: leaf_1_outline_point_count = 4
  !> 葉部分のアウトライン頂点数
  integer :: leaf_2_outline_point_count = 4
  !> 葉部分のアウトライン頂点数
  integer :: leaf_3_outline_point_count = 3
  !> 幹部分のアウトライン頂点数
  integer :: trunk_outline_point_count = 4

  !> 樹木の葉のアウトラインの最終座標(樹木の数, 葉の頂点数)の2次元配列
  real(8), dimension(:, :), allocatable :: tree_leaf_1_outline_x
  real(8), dimension(:, :), allocatable :: tree_leaf_2_outline_x
  real(8), dimension(:, :), allocatable :: tree_leaf_3_outline_x
  real(8), dimension(:, :), allocatable :: tree_leaf_1_outline_y
  real(8), dimension(:, :), allocatable :: tree_leaf_2_outline_y
  real(8), dimension(:, :), allocatable :: tree_leaf_3_outline_y
  !> 樹木の幹のアウトラインの最終座標(樹木の数, 幹の頂点数)の2次元配列
  real(8), dimension(:, :), allocatable :: tree_trunk_outline_x
  real(8), dimension(:, :), allocatable :: tree_trunk_outline_y

  !******************************************************************************************
  ! 礫に関するパラメータ
  !******************************************************************************************
  !> アウトラインの頂点数
  integer :: gravel_outline_point_count = 25

  !> 礫のアウトラインの最終座標(礫の数, 礫の頂点数)の2次元配列
  real(8), dimension(:, :), allocatable :: gravel_outline_x
  real(8), dimension(:, :), allocatable :: gravel_outline_y

contains

  !******************************************************************************************
  !> @brief オブジェクトの描画に関するパラメータの読み込み
  !> @param[in]  suffix  トレーサーのタイプを表す文字列（例: "tree", "gravel"）
  !> @param[inout] object  初期化されるオブジェクト構造体 (object_poly型)
  !******************************************************************************************
  subroutine initialize_object_type_parameters(suffix, object)

    !> トレーサーのタイプを表す文字列（例: "tree", ""）
    character(len=*), intent(in) :: suffix
    !> 初期化されるオブジェクト構造体 (object_poly型)
    type(object_poly), intent(inout) :: object

    !==========================================================================================
    ! オブジェクトの描画に関するパラメータの読み込み
    !==========================================================================================
    call cg_iric_read_integer(cgnsOut, "drawing_target_"//trim(suffix), object%drawing_target, is_error)
    if (object%drawing_target == 1) call cg_iric_read_real(cgnsOut, "drawable_depth_"//trim(suffix), object%drawable_depth, is_error)
    call cg_iric_read_real(cgnsOut, "base_size_"//trim(suffix), object%base_size, is_error)
    call cg_iric_read_integer(cgnsOut, "is_apply_variation_size_"//trim(suffix), object%is_apply_variation_size, is_error)
    call cg_iric_read_integer(cgnsOut, "draw_interval_i_"//trim(suffix), object%draw_interval_i, is_error)
    call cg_iric_read_integer(cgnsOut, "draw_interval_j_"//trim(suffix), object%draw_interval_j, is_error)

  end subroutine initialize_object_type_parameters

  !******************************************************************************************
  !> @brief オブジェクトを描画する位置や基本条件を決定するサブルーチン
  !> @param[inout] object  オブジェクトのタイプ（例: "tree", "gravel"）
  !******************************************************************************************
  subroutine calculate_object_base_info(object)

    !> オブジェクトのタイプ（例: "tree", "gravel"）
    type(object_poly), intent(inout) :: object

    !> ループ用変数
    integer :: i, j

    !> 仮のオブジェクトの最大数
    integer :: max_object_count
    !> 仮のオブジェクト配置先のセルのインデックス
    integer :: draw_point_index_i_tmp
    !> 仮のオブジェクト配置先のセルのインデックス
    integer :: draw_point_index_j_tmp
    !> 仮のオブジェクト配置先のセルのインデックスを格納する配列
    integer, dimension(:), allocatable :: draw_point_index_i
    !> 仮のオブジェクト配置先のセルのインデックスを格納する配列
    integer, dimension(:), allocatable :: draw_point_index_j

    !> オブジェクトのインデックス
    integer :: object_index
    !> 配置間隔内でどこにオブジェクトを配置するかのオフセット
    integer :: offset_i, offset_j
    !> セル内のオブジェクトの一般座標
    real(8) :: draw_point_coordinate_xi_in_cell
    !> セル内のオブジェクトの一般座標
    real(8) :: draw_point_coordinate_eta_in_cell

    !> 乱数
    real(8) :: rand_num

    ! 仮のオブジェクト最大数を計算
    max_object_count = ceiling(real(cell_count_i, 8)/real(object%draw_interval_i, 8), kind=8)* &
                       ceiling(real(cell_count_j, 8)/real(object%draw_interval_j, 8), kind=8)
    ! 仮のオブジェクト配置先のセルのインデックスのメモリ確保
    allocate (draw_point_index_i(max_object_count), draw_point_index_j(max_object_count))

    !==========================================================================================
    ! オブジェクトを配置するセルのインデックスを計算
    ! 仮の最大数分の計算をするが、指定したセルにのみオブジェクトを配置する場合はカウントしない
    !==========================================================================================

    ! オブジェクトのインデックスの初期化
    object_index = 0

    do j = 1, cell_count_j, object%draw_interval_j
      do i = 1, cell_count_i, object%draw_interval_i

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! オブジェクトを配置するセルのインデックスを決定
        ! オブジェクトは配置間隔内でランダムな位置に配置されるように処理する
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! rand_numは[0, 1)の範囲なので、[0, draw_interval_i_object)の範囲に変換
        call random_number(rand_num)
        offset_i = int(rand_num*object%draw_interval_i)  ! 0 〜 draw_interval_i_object-1 の範囲
        call random_number(rand_num)
        offset_j = int(rand_num*object%draw_interval_j)  ! 0 〜 draw_interval_j_object-1 の範囲

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 境界処理を適用して安全な座標を確保
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! i + offset_i が cell_count_i を超えないようにする
        draw_point_index_i_tmp = min(i + offset_i, cell_count_i)
        ! j + offset_j が cell_count_j を超えないようにする
        draw_point_index_j_tmp = min(j + offset_j, cell_count_j)

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! 指定したセルにオブジェクトを配置する場合、配置先のインデックスが指定セルかチェック、指定セル以外であればスキップ
        ! 水深で描画判定をする場合は全ての配置位置で描画の可能性があるのでここでは間引かずタイムステップ毎に描画判定を行う
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        if (object%drawing_target == 2) then
          if (is_tree_cell(draw_point_index_i_tmp, draw_point_index_j_tmp) == 0) then
            cycle
          end if
        end if

        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        ! object_indexを更新して仮配列に配置先のセルのインデックスを格納
        !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        object_index = object_index + 1
        draw_point_index_i(object_index) = draw_point_index_i_tmp
        draw_point_index_j(object_index) = draw_point_index_j_tmp

      end do
    end do

    !==========================================================================================
    ! オブジェクトの最大数、メモリ確保、仮配列から実配列にコピー
    !==========================================================================================
    ! 実際のオブジェクトの最大数を更新
    object%max_object_count = object_index

    ! オブジェクトポリゴンの基準点のインデックスのメモリ確保
    allocate (object%draw_point_index_i(object%max_object_count), object%draw_point_index_j(object%max_object_count))
    allocate (object%draw_point_coordinate_x(object%max_object_count), object%draw_point_coordinate_y(object%max_object_count))
    allocate (object%object_size(object%max_object_count))

    ! 仮配列から実配列にコピー(配列の長さが違うので仮配列からobject%max_object_countまでコピー)
    object%draw_point_index_i = draw_point_index_i(1:object%max_object_count)
    object%draw_point_index_j = draw_point_index_j(1:object%max_object_count)

    ! 仮配列のメモリ解放
    deallocate (draw_point_index_i, draw_point_index_j)

    !==========================================================================================
    ! オブジェクトの座標とサイズの計算
    !==========================================================================================
    do object_index = 1, object%max_object_count
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! オブジェクトの座標の計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! ランダムでセル内の座標(0~1)を求め物理座標に変換
      call random_number(rand_num)
      draw_point_coordinate_xi_in_cell = rand_num*grid_interval_xi

      call random_number(rand_num)
      draw_point_coordinate_eta_in_cell = rand_num*grid_interval_eta

      ! セル内の座標を物理座標に変換
      call transform_general_to_physical(object%draw_point_index_i(object_index), &
                                         object%draw_point_index_j(object_index), &
                                         draw_point_coordinate_xi_in_cell, &
                                         draw_point_coordinate_eta_in_cell, &
                                         object%draw_point_coordinate_x(object_index), &
                                         object%draw_point_coordinate_y(object_index))

      !==========================================================================================
      ! オブジェクトサイズの設定
      !==========================================================================================

      if (object%is_apply_variation_size == 1) then
        ! オブジェクトサイズにバリエーションを与える
        call random_number(rand_num)
        object%object_size(object_index) = object%base_size*(rand_num + 0.5d0) ! 0.5 〜 1.5 の範囲の倍率を与える
      else
        ! オブジェクトサイズにバリエーションを与えない
        object%object_size(object_index) = object%base_size
      end if
    end do

  end subroutine calculate_object_base_info

  !******************************************************************************************
  !> @brief 樹木オブジェクトの形状を作成、基準点からポリゴンの頂点の座標までのオフセットを計算する
  !******************************************************************************************
  subroutine make_tree_polygon_outline()

    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_1_outline_base_offset_x
    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_2_outline_base_offset_x
    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_3_outline_base_offset_x
    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_1_outline_base_offset_y
    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_2_outline_base_offset_y
    !> 樹木の葉部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_leaf_3_outline_base_offset_y

    !> 樹木の幹部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_trunk_outline_base_offset_x
    !> 樹木の幹部分のベースのアウトラインオフセット
    real(8), dimension(:), allocatable :: tree_trunk_outline_base_offset_y

    !> 樹木のベースのアウトラインオフセットの高さ
    real(8) :: tree_base_height

    !> 樹木のインデックス
    integer :: tree_index
    !> ループ用変数
    integer :: i

    ! 樹木のアウトラインオフセットのメモリ確保
    allocate (tree_leaf_1_outline_base_offset_x(leaf_1_outline_point_count))
    allocate (tree_leaf_2_outline_base_offset_x(leaf_2_outline_point_count))
    allocate (tree_leaf_3_outline_base_offset_x(leaf_3_outline_point_count))
    allocate (tree_leaf_1_outline_base_offset_y(leaf_1_outline_point_count))
    allocate (tree_leaf_2_outline_base_offset_y(leaf_2_outline_point_count))
    allocate (tree_leaf_3_outline_base_offset_y(leaf_3_outline_point_count))
    allocate (tree_trunk_outline_base_offset_x(trunk_outline_point_count))
    allocate (tree_trunk_outline_base_offset_y(trunk_outline_point_count))

    ! 樹木のアウトラインの最終座標のメモリ確保
    allocate (tree_leaf_1_outline_x(tree%max_object_count, leaf_1_outline_point_count))
    allocate (tree_leaf_2_outline_x(tree%max_object_count, leaf_2_outline_point_count))
    allocate (tree_leaf_3_outline_x(tree%max_object_count, leaf_3_outline_point_count))
    allocate (tree_leaf_1_outline_y(tree%max_object_count, leaf_1_outline_point_count))
    allocate (tree_leaf_2_outline_y(tree%max_object_count, leaf_2_outline_point_count))
    allocate (tree_leaf_3_outline_y(tree%max_object_count, leaf_3_outline_point_count))
    allocate (tree_trunk_outline_x(tree%max_object_count, trunk_outline_point_count))
    allocate (tree_trunk_outline_y(tree%max_object_count, trunk_outline_point_count))

    !==========================================================================================
    ! ベースのアウトラインオフセットの計算
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 樹木葉部分のアウトラインオフセット
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 1段目の葉のアウトラインオフセット
    tree_leaf_1_outline_base_offset_x(1) = -1.0000d0; tree_leaf_1_outline_base_offset_y(1) = 1.0d0
    tree_leaf_1_outline_base_offset_x(2) = -0.4333d0; tree_leaf_1_outline_base_offset_y(2) = 2.0d0
    tree_leaf_1_outline_base_offset_x(3) = 0.4333d0; tree_leaf_1_outline_base_offset_y(3) = 2.0d0
    tree_leaf_1_outline_base_offset_x(4) = 1.0000d0; tree_leaf_1_outline_base_offset_y(4) = 1.0d0

    ! 2段目の葉のアウトラインオフセット
    tree_leaf_2_outline_base_offset_x(1) = -0.7666d0; tree_leaf_2_outline_base_offset_y(1) = 2.0d0
    tree_leaf_2_outline_base_offset_x(2) = -0.1665d0; tree_leaf_2_outline_base_offset_y(2) = 3.0d0
    tree_leaf_2_outline_base_offset_x(3) = 0.1665d0; tree_leaf_2_outline_base_offset_y(3) = 3.0d0
    tree_leaf_2_outline_base_offset_x(4) = 0.7666d0; tree_leaf_2_outline_base_offset_y(4) = 2.0d0

    ! 3段目の葉のアウトラインオフセット
    tree_leaf_3_outline_base_offset_x(1) = -0.3333d0; tree_leaf_3_outline_base_offset_y(1) = 3.0d0
    tree_leaf_3_outline_base_offset_x(2) = 0.0000d0; tree_leaf_3_outline_base_offset_y(2) = 4.0d0
    tree_leaf_3_outline_base_offset_x(3) = 0.3333d0; tree_leaf_3_outline_base_offset_y(3) = 3.0d0

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 樹木幹部分のアウトラインオフセット
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    !幅が0.4、高さが1.0の矩形を作成
    tree_trunk_outline_base_offset_x(1) = -0.2d0; tree_trunk_outline_base_offset_y(1) = 0.0d0
    tree_trunk_outline_base_offset_x(2) = -0.2d0; tree_trunk_outline_base_offset_y(2) = 1.0d0
    tree_trunk_outline_base_offset_x(3) = 0.2d0; tree_trunk_outline_base_offset_y(3) = 1.0d0
    tree_trunk_outline_base_offset_x(4) = 0.2d0; tree_trunk_outline_base_offset_y(4) = 0.0d0

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 基準点(y=0)からアウトラインのy最大値までの高さを設定
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    tree_base_height = 4.0d0

    !==========================================================================================
    ! 樹木のアウトラインの最終座標を計算
    ! tree_leaf_outline_x, tree_leaf_outline_y は(樹木の数, 葉の頂点数)の2次元配列なので
    ! draw_point_coordinate_x, draw_point_coordinate_yは(樹木の数)の1次元配列を2次元配列に拡張、
    ! tree_leaf_outline_base_offset_x, tree_leaf_outline_base_offset_yは(葉の頂点数)の1次元配列を2次元配列に拡張、
    ! object_sizeは(樹木の数)の1次元配列を2次元配列に拡張
    !==========================================================================================

    ! 葉のアウトラインの最終座標を計算(1段目)
    tree_leaf_1_outline_x(:, :) = spread(tree%draw_point_coordinate_x(:), 2, leaf_1_outline_point_count) + &
                                  spread(tree_leaf_1_outline_base_offset_x(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_1_outline_point_count)/tree_base_height*tree_aspect_ratio

    tree_leaf_1_outline_y(:, :) = spread(tree%draw_point_coordinate_y(:), 2, leaf_1_outline_point_count) + &
                                  spread(tree_leaf_1_outline_base_offset_y(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_1_outline_point_count)/tree_base_height
    ! 葉のアウトラインの最終座標を計算(2段目)
    tree_leaf_2_outline_x(:, :) = spread(tree%draw_point_coordinate_x(:), 2, leaf_2_outline_point_count) + &
                                  spread(tree_leaf_2_outline_base_offset_x(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_2_outline_point_count)/tree_base_height*tree_aspect_ratio

    tree_leaf_2_outline_y(:, :) = spread(tree%draw_point_coordinate_y(:), 2, leaf_2_outline_point_count) + &
                                  spread(tree_leaf_2_outline_base_offset_y(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_2_outline_point_count)/tree_base_height
    ! 葉のアウトラインの最終座標を計算(3段目)
    tree_leaf_3_outline_x(:, :) = spread(tree%draw_point_coordinate_x(:), 2, leaf_3_outline_point_count) + &
                                  spread(tree_leaf_3_outline_base_offset_x(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_3_outline_point_count)/tree_base_height*tree_aspect_ratio

    tree_leaf_3_outline_y(:, :) = spread(tree%draw_point_coordinate_y(:), 2, leaf_3_outline_point_count) + &
                                  spread(tree_leaf_3_outline_base_offset_y(:), 1, tree%max_object_count)* &
                                  spread(tree%object_size(:), 2, leaf_3_outline_point_count)/tree_base_height

    ! 幹のアウトラインの最終座標を計算
    tree_trunk_outline_x(:, :) = spread(tree%draw_point_coordinate_x(:), 2, trunk_outline_point_count) + &
                                 spread(tree_trunk_outline_base_offset_x(:), 1, tree%max_object_count)* &
                                 spread(tree%object_size(:), 2, trunk_outline_point_count)/tree_base_height*tree_aspect_ratio
    tree_trunk_outline_y(:, :) = spread(tree%draw_point_coordinate_y(:), 2, trunk_outline_point_count) + &
                                 spread(tree_trunk_outline_base_offset_y(:), 1, tree%max_object_count)* &
                                 spread(tree%object_size(:), 2, trunk_outline_point_count)/tree_base_height

    ! メモリ解放
    deallocate (tree_leaf_1_outline_base_offset_x, tree_leaf_1_outline_base_offset_y)
    deallocate (tree_leaf_2_outline_base_offset_x, tree_leaf_2_outline_base_offset_y)
    deallocate (tree_leaf_3_outline_base_offset_x, tree_leaf_3_outline_base_offset_y)
    deallocate (tree_trunk_outline_base_offset_x, tree_trunk_outline_base_offset_y)
    deallocate (tree%draw_point_coordinate_x, tree%draw_point_coordinate_y) ! もう使わないので解放
    deallocate (tree%object_size) ! もう使わないので解放

  end subroutine make_tree_polygon_outline

  !******************************************************************************************
  !> @brief 礫オブジェクトの形状を作成、基準点からポリゴンの頂点の座標までのオフセットを計算する
  !******************************************************************************************
  subroutine make_gravel_polygon_outline()

    !> 礫のアウトラインオフセット
    real(8), dimension(:), allocatable :: gravel_outline_base_offset_x
    !> 礫のアウトラインオフセット
    real(8), dimension(:), allocatable :: gravel_outline_base_offset_y

    !> ループ用変数
    integer :: i
    !> 頂点間の角度の間隔
    real(8) :: angle_interval
    !> 角度
    real(8) :: angle

    ! 礫のアウトラインオフセットのメモリ確保
    allocate (gravel_outline_base_offset_x(gravel_outline_point_count))
    allocate (gravel_outline_base_offset_y(gravel_outline_point_count))

    ! 礫のアウトラインの最終座標のメモリ確保
    allocate (gravel_outline_x(gravel%max_object_count, gravel_outline_point_count))
    allocate (gravel_outline_y(gravel%max_object_count, gravel_outline_point_count))

    !==========================================================================================
    ! 礫のアウトラインオフセットを計算
    ! 基準点を中心に半径1.0の円周上にoutline_point_count個の頂点を配置
    !==========================================================================================
    ! 除算は負荷がかかるので、ループ外で事前に計算しておく
    angle_interval = 2.0d0*pi/gravel_outline_point_count

    ! outline_point_count個の頂点の座標を計算
    do i = 1, gravel_outline_point_count
      angle = angle_interval*(i - 1)
      gravel_outline_base_offset_x(i) = cos(angle)
      gravel_outline_base_offset_y(i) = sin(angle)
    end do

    !==========================================================================================
    ! 礫のアウトラインの最終座標を計算
    ! 基準の直径が2.0なので、オフセットにobject_size/2を掛けて直径object_sizeの円を作成
    !==========================================================================================
    ! 礫のアウトラインの最終座標を計算
    gravel_outline_x(:, :) = spread(gravel%draw_point_coordinate_x(:), 2, gravel_outline_point_count) + &
                             spread(gravel_outline_base_offset_x(:), 1, gravel%max_object_count)* &
                             spread(gravel%object_size(:), 2, gravel_outline_point_count)/2.0d0
    gravel_outline_y(:, :) = spread(gravel%draw_point_coordinate_y(:), 2, gravel_outline_point_count) + &
                             spread(gravel_outline_base_offset_y(:), 1, gravel%max_object_count)* &
                             spread(gravel%object_size(:), 2, gravel_outline_point_count)/2.0d0

    ! メモリ解放
    deallocate (gravel_outline_base_offset_x, gravel_outline_base_offset_y)
    deallocate (gravel%draw_point_coordinate_x, gravel%draw_point_coordinate_y) ! もう使わないので解放
    deallocate (gravel%object_size) ! もう使わないので解放

  end subroutine make_gravel_polygon_outline

  !******************************************************************************************
  !> @brief オブジェクトの描画に関するパラメータの読み込み、初期化
  !******************************************************************************************
  subroutine initialize_object()

    !==========================================================================================
    ! 樹木オブジェクトの初期化
    !==========================================================================================
    if (is_draw_tree == 1) then

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 樹木のオブジェクトのパラメーターの読み込みとメモリ確保
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call initialize_object_type_parameters("tree", tree)
      ! 樹木のアスペクト比は樹木だけのパラメータなのでここで個別に読み込む
      call cg_iric_read_real(cgnsOut, "tree_aspect_ratio", tree_aspect_ratio, is_error)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 樹木の描画位置、サイズの計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call calculate_object_base_info(tree)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 樹木の形状を作成、基準点からポリゴンの頂点の座標までのオフセットを計算する
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call make_tree_polygon_outline()

    end if

    !==========================================================================================
    ! 礫オブジェクトの初期化
    !==========================================================================================
    if (is_draw_gravel == 1) then

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 礫のオブジェクトのパラメーターの読み込みとメモリ確保
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call initialize_object_type_parameters("gravel", gravel)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 礫の描画位置、サイズの計算
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call calculate_object_base_info(gravel)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 礫の形状を作成、基準点からポリゴンの頂点の座標までのオフセットを計算する
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call make_gravel_polygon_outline()

    end if

  end subroutine initialize_object

  !******************************************************************************************
  !> @brief オブジェクトの出力
  !******************************************************************************************
  subroutine output_landscape_object()

    !> ループ用変数
    integer :: object_index

    !==========================================================================================
    ! 樹木オブジェクトの出力
    !==========================================================================================
    if (is_draw_tree == 1) then

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 樹木の葉部分の描画
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call cg_iric_write_sol_polydata_groupbegin(cgnsOut, "tree_leaf", is_error)
      do object_index = 1, tree%max_object_count

        if (tree%drawing_target == 1) then
          ! 指定した水深より大きいセルではスキップ
          if (depth_cell(tree%draw_point_index_i(object_index), tree%draw_point_index_j(object_index)) > tree%drawable_depth) then
            cycle
          end if
        end if

        ! 指定したセルにオブジェクトを描画
        ! 1段目の葉の描画
        call cg_iric_write_sol_polydata_polygon(cgnsOut, leaf_1_outline_point_count, &
                                                tree_leaf_1_outline_x(object_index, :), &
                                                tree_leaf_1_outline_y(object_index, :), is_error)
        ! 2段目の葉の描画
        call cg_iric_write_sol_polydata_polygon(cgnsOut, leaf_2_outline_point_count, &
                                                tree_leaf_2_outline_x(object_index, :), &
                                                tree_leaf_2_outline_y(object_index, :), is_error)
        ! 3段目の葉の描画
        call cg_iric_write_sol_polydata_polygon(cgnsOut, leaf_3_outline_point_count, &
                                                tree_leaf_3_outline_x(object_index, :), &
                                                tree_leaf_3_outline_y(object_index, :), is_error)

      end do
      call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 樹木の幹部分の描画
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call cg_iric_write_sol_polydata_groupbegin(cgnsOut, "tree_trunk", is_error)
      do object_index = 1, tree%max_object_count

        if (tree%drawing_target == 1) then
          ! 指定した水深以下のセルにオブジェクトを描画
          if (depth_cell(tree%draw_point_index_i(object_index), tree%draw_point_index_j(object_index)) <= tree%drawable_depth) then
            call cg_iric_write_sol_polydata_polygon(cgnsOut, trunk_outline_point_count, &
                                                    tree_trunk_outline_x(object_index, :), &
                                                    tree_trunk_outline_y(object_index, :), is_error)
          end if
        else
          ! 指定したセルにオブジェクトを描画
          call cg_iric_write_sol_polydata_polygon(cgnsOut, trunk_outline_point_count, &
                                                  tree_trunk_outline_x(object_index, :), &
                                                  tree_trunk_outline_y(object_index, :), is_error)
        end if

      end do
      call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)
    end if

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 礫の描画
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (is_draw_gravel == 1) then
      call cg_iric_write_sol_polydata_groupbegin(cgnsOut, "gravel", is_error)
      do object_index = 1, gravel%max_object_count

        if (gravel%drawing_target == 1) then
          ! 指定した水深以下のセルにオブジェクトを描画
          if (depth_cell(gravel%draw_point_index_i(object_index), gravel%draw_point_index_j(object_index)) <= gravel%drawable_depth) then
            call cg_iric_write_sol_polydata_polygon(cgnsOut, gravel_outline_point_count, &
                                                    gravel_outline_x(object_index, :), &
                                                    gravel_outline_y(object_index, :), is_error)
          end if
        else
          ! 指定したセルにオブジェクトを描画
          call cg_iric_write_sol_polydata_polygon(cgnsOut, gravel_outline_point_count, &
                                                  gravel_outline_x(object_index, :), &
                                                  gravel_outline_y(object_index, :), is_error)
        end if

      end do
      call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)
    end if

  end subroutine output_landscape_object

end module landscape_poly
