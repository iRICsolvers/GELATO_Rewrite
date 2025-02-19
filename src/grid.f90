!******************************************************************************************
! 格子関係
!******************************************************************************************
module grid

  use iric
  use cell2node_m
  use Common

  implicit none

  !******************************************************************************************
  ! 格子情報
  !******************************************************************************************
  !> i方向の格子点の数
  integer :: node_count_i
  !> j方向の格子点の数
  integer :: node_count_j
  !> i方向のセルの数
  integer :: cell_count_i
  !> j方向のセルの数
  integer :: cell_count_j

  !> 格子のy座標
  real(8), dimension(:, :), allocatable :: node_coordinate_x
  !> 格子のy座標
  real(8), dimension(:, :), allocatable :: node_coordinate_y

  !> 水路長
  real(8) :: channel_length

  !> j方向中央のインデックス
  integer :: j_center

  !******************************************************************************************
  ! 計算格子属性
  !******************************************************************************************
  !> 障害物セル
  integer, dimension(:, :), allocatable :: obstacle_cell
  !> クローンニングセル
  integer, dimension(:, :), allocatable :: is_cloning_cell
  !> マニング粗度(セル)
  real(8), dimension(:, :), allocatable :: roughness_cell
  !> マニング粗度(格子点)
  real(8), dimension(:, :), allocatable :: roughness_node
  !> 植生発生セル
  integer, dimension(:, :), allocatable :: is_vegetation_cell
  !> 礫発生セル
  integer, dimension(:, :), allocatable :: is_gravel_cell
  !> トレーサートラップセル
  integer, dimension(:, :), allocatable :: trap_cell

  !******************************************************************************************
  ! 一般化座標系関係
  !******************************************************************************************
  !> ξ方向一般座標系格子
  real(8) :: grid_interval_xi
  !> ξ方向物理座標での格子点間距離の最小値
  real(8) :: grid_physical_distance_xi_min
  !> ξ方向スケーリング係数
  real(8), dimension(:, :), allocatable :: scale_factor_xi
  !> ξ方向でのxの勾配(中央差分法)
  real(8), dimension(:, :), allocatable :: x_gradient_in_xi
  !> ξ方向でのyの勾配(中央差分法)
  real(8), dimension(:, :), allocatable :: y_gradient_in_xi
  !> x方向の変位を ξ方向に変換する逆ヤコビ行列の要素（∂ξ/∂x = ∂y/∂η / det(J)）
  real(8), dimension(:, :), allocatable :: x_to_xi_component
  !> y方向の変位を ξ方向に変換する逆ヤコビ行列の要素（∂ξ/∂y = -∂x/∂η / det(J)）
  real(8), dimension(:, :), allocatable :: y_to_xi_component

  !> η方向一般座標系格子間隔
  real(8) :: grid_interval_eta
  !> η方向物理座標での格子点間距離の最小値
  real(8) :: grid_physical_distance_eta_min
  !> η方向スケーリング係数
  real(8), dimension(:, :), allocatable :: scale_factor_eta
  !> η方向でのxの勾配(中央差分法)
  real(8), dimension(:, :), allocatable :: x_gradient_in_eta
  !> η方向でのyの勾配(中央差分法)
  real(8), dimension(:, :), allocatable :: y_gradient_in_eta
  !> x方向の変位を η方向に変換する逆ヤコビ行列の要素（∂η/∂x = -∂y/∂ξ / det(J)）
  real(8), dimension(:, :), allocatable :: x_to_eta_component
  !> y方向の変位を η方向に変換する逆ヤコビ行列の要素（∂η/∂y = ∂x/∂ξ / det(J)）
  real(8), dimension(:, :), allocatable :: y_to_eta_component

  !> ヤコビアン行列の逆数 det(J)
  real(8), dimension(:, :), allocatable :: inverse_jacobian

contains

  !******************************************************************************************
  !> @brief 出力用格子の読み込みの計算
  !******************************************************************************************
  subroutine Load_Grid()

    implicit none

    integer :: i, j

    !==========================================================================================
    ! 格子点の数の読み込み
    !==========================================================================================
    call cg_iric_read_grid2d_str_size(cgnsOut, node_count_i, node_count_j, is_error)
    cell_count_i = node_count_i - 1
    cell_count_j = node_count_j - 1

    j_center = ceiling(real(node_count_j)/2.0d0)

    !==========================================================================================
    ! メモリ確保
    !==========================================================================================

    ! 格子点の座標
    allocate (node_coordinate_x(node_count_i, node_count_j))
    allocate (node_coordinate_y(node_count_i, node_count_j))

    ! 計算格子の属性
    allocate (obstacle_cell(cell_count_i, cell_count_j))
    allocate (is_cloning_cell(cell_count_i, cell_count_j))
    allocate (roughness_cell(cell_count_i, cell_count_j))
    allocate (roughness_node(node_count_i, node_count_j))
    allocate (is_vegetation_cell(cell_count_i, cell_count_j))
    allocate (is_gravel_cell(cell_count_i, cell_count_j))
    allocate (trap_cell(cell_count_i, cell_count_j))

    !==========================================================================================
    ! 格子属性の読み込み
    !==========================================================================================

    call cg_iric_read_grid2d_coords(cgnsOut, node_coordinate_x, node_coordinate_y, is_error)
    call cg_iric_read_grid_integer_cell(cgnsOut, "obstacle", obstacle_cell, is_error)
    call cg_iric_read_grid_integer_cell(cgnsOut, "is_tracer_cloning", is_cloning_cell, is_error)
    call cg_iric_read_grid_real_cell(cgnsOut, "roughness_cell", roughness_cell, is_error)
    call cg_iric_read_grid_integer_cell(cgnsOut, "is_vegetation_cell", is_vegetation_cell, is_error)
    call cg_iric_read_grid_integer_cell(cgnsOut, "is_gravel_cell", is_gravel_cell, is_error)
    call cg_iric_read_grid_integer_cell(cgnsOut, "trap_cell", trap_cell, is_error)

    !==========================================================================================
    ! 摩擦係数一定の場合の処理
    !==========================================================================================
    if (is_use_constant_roughness == 1) then
      roughness_cell = constant_roughness
    end if

    !==========================================================================================
    ! 摩擦係数をセルからノードに変換
    !==========================================================================================
    call cell2node(roughness_cell, roughness_node)

    call compute_transform_metrics()

  end subroutine Load_Grid

  !******************************************************************************************
  !> @brief 物理座標(x,y)と一般座標(ξ,η)を変換するためのパラメータ計算
  !******************************************************************************************
  subroutine compute_transform_metrics()

    implicit none

    !> ξ方向一般座標系格子間隔
    real(8) :: grid_physical_distance_xi
    !> η方向一般座標系格子間隔
    real(8) :: grid_physical_distance_eta
    !> xi, eta方向の変位を x, y方向に変換するための変換行列の要素(∂x/∂ξ)(x方向の変位をxi方向の変位で除した傾き)
    real(8) :: xi_to_x_component
    !> xi, eta方向の変位を x, y方向に変換するための変換行列の要素(∂y/∂ξ)(y方向の変位をxi方向の変位で除した傾き)
    real(8) :: xi_to_y_component
    !> xi, eta方向の変位を x, y方向に変換するための変換行列の要素(∂x/∂η)(x方向の変位をeta方向の変位で除した傾き)
    real(8) :: eta_to_x_component
    !> xi, eta方向の変位を x, y方向に変換するための変換行列の要素(∂y/∂η)(y方向の変位をeta方向の変位で除した傾き)
    real(8) :: eta_to_y_component

    !> i方向ループ用の変数
    integer :: i
    !> j方向ループ用の変数
    integer :: j

    channel_length = 0.0
    grid_physical_distance_xi_min = 10000.0  ! 最小値の初期値
    grid_physical_distance_eta_min = 10000.0 ! 最小値の初期値

    !==========================================================================================
    ! 平均格子間隔を計算
    !==========================================================================================
    grid_interval_xi = 1./(cell_count_i)
    grid_interval_eta = 1./(cell_count_j)

    !==========================================================================================
    ! メモリ確保
    !==========================================================================================
    allocate (scale_factor_xi(cell_count_i, node_count_j))
    allocate (scale_factor_eta(node_count_i, cell_count_j))
    allocate (x_gradient_in_xi(cell_count_i, cell_count_j))
    allocate (y_gradient_in_xi(cell_count_i, cell_count_j))
    allocate (x_gradient_in_eta(cell_count_i, cell_count_j))
    allocate (y_gradient_in_eta(cell_count_i, cell_count_j))

    allocate (inverse_jacobian(node_count_i, node_count_j))
    allocate (x_to_xi_component(node_count_i, node_count_j))
    allocate (y_to_xi_component(node_count_i, node_count_j))
    allocate (x_to_eta_component(node_count_i, node_count_j))
    allocate (y_to_eta_component(node_count_i, node_count_j))

    !==========================================================================================
    ! 水路延長、ξ,η方向のスケーリング係数の計算
    ! grid_physical_distance_xi: ξ方向の物理座標での格子点間距離
    ! 物理距離 dsに掛けることで、dxiが求まる
    ! grid_physical_distance_eta: η方向の物理座標での格子点間距離
    ! 物理距離 dsに掛けることで、detaが求まる
    !==========================================================================================
    !$omp parallel do collapse(2) private(i, j, grid_physical_distance_xi) reduction(min:grid_physical_distance_xi_min) reduction(+:channel_length)
    do j = 1, node_count_j
      do i = 1, cell_count_i
        grid_physical_distance_xi = sqrt((node_coordinate_x(i + 1, j) - node_coordinate_x(i, j))**2 + (node_coordinate_y(i + 1, j) - node_coordinate_y(i, j))**2)
        grid_physical_distance_xi_min = min(grid_physical_distance_xi, grid_physical_distance_xi_min)
        scale_factor_xi(i, j) = grid_interval_xi/grid_physical_distance_xi
        if (j == j_center) channel_length = channel_length + grid_physical_distance_xi
      end do
    end do
    ! $omp end parallel do

    !$omp parallel do collapse(2) private(i, j, grid_physical_distance_eta) reduction(min:grid_physical_distance_eta_min)
    do j = 1, cell_count_j
      do i = 1, node_count_i
        grid_physical_distance_eta = sqrt((node_coordinate_x(i, j + 1) - node_coordinate_x(i, j))**2 + (node_coordinate_y(i, j + 1) - node_coordinate_y(i, j))**2)
        grid_physical_distance_eta_min = min(grid_physical_distance_eta, grid_physical_distance_eta_min)
        scale_factor_eta(i, j) = grid_interval_eta/grid_physical_distance_eta
      end do
    end do
    !$omp end parallel do

    !==========================================================================================
    ! ξ、η方向でのx,yの勾配(中央差分方法)
    ! このコンポーネントは、一般座標の位置を物理座標の位置に変換する際に必要
    !==========================================================================================
    !$omp parallel do collapse(2) private(i, j) shared(node_coordinate_x, node_coordinate_y, x_gradient_in_xi, x_gradient_in_eta, y_gradient_in_xi, y_gradient_in_eta)
    do j = 1, cell_count_j
      do i = 1, cell_count_i
        x_gradient_in_xi(i, j) = (node_coordinate_x(i + 1, j + 1) + node_coordinate_x(i + 1, j) - node_coordinate_x(i, j + 1) - node_coordinate_x(i, j))/(2.*grid_interval_xi)
        x_gradient_in_eta(i, j) = (node_coordinate_x(i + 1, j + 1) + node_coordinate_x(i, j + 1) - node_coordinate_x(i + 1, j) - node_coordinate_x(i, j))/(2.*grid_interval_eta)
        y_gradient_in_xi(i, j) = (node_coordinate_y(i + 1, j + 1) + node_coordinate_y(i + 1, j) - node_coordinate_y(i, j + 1) - node_coordinate_y(i, j))/(2.*grid_interval_xi)
        y_gradient_in_eta(i, j) = (node_coordinate_y(i + 1, j + 1) + node_coordinate_y(i, j + 1) - node_coordinate_y(i + 1, j) - node_coordinate_y(i, j))/(2.*grid_interval_eta)
      end do
    end do
    !$omp end parallel do

    !$omp parallel do collapse(2) private(i, j, xi_to_x_component, xi_to_y_component, eta_to_x_component, eta_to_y_component) shared(node_coordinate_x, node_coordinate_y, inverse_jacobian, x_to_xi_component, y_to_xi_component, x_to_eta_component, y_to_eta_component)
    do j = 1, node_count_j
      do i = 1, node_count_i
        !==========================================================================================
        ! ξ、η方向でのx,yの勾配(2点差分方法)
        ! xi, eta方向の変位を x, y方向に変換するための変換行列の要素を計算
        !==========================================================================================
        if (i == 1) then
          xi_to_x_component = (node_coordinate_x(i + 1, j) - node_coordinate_x(i, j))/grid_interval_xi
          xi_to_y_component = (node_coordinate_y(i + 1, j) - node_coordinate_y(i, j))/grid_interval_xi
        else if (i == node_count_i) then
          xi_to_x_component = (node_coordinate_x(i, j) - node_coordinate_x(i - 1, j))/grid_interval_xi
          xi_to_y_component = (node_coordinate_y(i, j) - node_coordinate_y(i - 1, j))/grid_interval_xi
        else
          xi_to_x_component = (node_coordinate_x(i + 1, j) - node_coordinate_x(i - 1, j))/(2.*grid_interval_xi)
          xi_to_y_component = (node_coordinate_y(i + 1, j) - node_coordinate_y(i - 1, j))/(2.*grid_interval_xi)
        end if

        if (j == 1) then
          eta_to_x_component = (node_coordinate_x(i, j + 1) - node_coordinate_x(i, j))/grid_interval_eta
          eta_to_y_component = (node_coordinate_y(i, j + 1) - node_coordinate_y(i, j))/grid_interval_eta
        else if (j == node_count_j) then
          eta_to_x_component = (node_coordinate_x(i, j) - node_coordinate_x(i, j - 1))/grid_interval_eta
          eta_to_y_component = (node_coordinate_y(i, j) - node_coordinate_y(i, j - 1))/grid_interval_eta
        else
          eta_to_x_component = (node_coordinate_x(i, j + 1) - node_coordinate_x(i, j - 1))/(2.*grid_interval_eta)
          eta_to_y_component = (node_coordinate_y(i, j + 1) - node_coordinate_y(i, j - 1))/(2.*grid_interval_eta)
        end if

        !==========================================================================================
        ! x, y方向の変位を ξ, η方向に変換する逆ヤコビ行列の要素を計算する
        ! GELATOの計算では、x, y方向の変位を ξ, η方向に変換する方がおおいので共通変数には逆ヤコビ行列を格納する
        !
        ! ξ, η方向の変位を x, y方向に変換するための変換行列＝ヤコビ行列J
        ! ヤコビ行列 J:
        ! J = [ xi_to_x_component   eta_to_x_component ]
        !     [ xi_to_y_component   eta_to_y_component ]
        !
        ! x=ξ方向の変位*xi_to_x_component + η方向の変位*eta_to_x_component
        ! y=ξ方向の変位*xi_to_y_component + η方向の変位*eta_to_y_component
        !
        ! x, y方向の変位を ξ, η方向に変換するには逆ヤコビ行列を使う
        !
        ! J⁻¹ = 1/det(J) * [ eta_to_y_component  -eta_to_x_component ]
        !                  [ -xi_to_y_component    xi_to_x_component ]
        !
        ! ヤコビ行列の行列式:
        ! det(J) = xi_to_x_component * eta_to_y_component - eta_to_x_component * xi_to_y_component
        !
        ! 逆ヤコビ行列 J⁻¹を以下のようにすると
        ! J⁻¹ = [ x_to_xi_component   y_to_xi_component  ]
        !       [ x_to_eta_component  y_to_eta_component ]
        !
        ! 以下のような式になる
        !==========================================================================================
        inverse_jacobian(i, j) = 1./(xi_to_x_component*eta_to_y_component - eta_to_x_component*xi_to_y_component)
        x_to_xi_component(i, j) = inverse_jacobian(i, j)*eta_to_y_component
        y_to_xi_component(i, j) = -inverse_jacobian(i, j)*eta_to_x_component
        x_to_eta_component(i, j) = -inverse_jacobian(i, j)*xi_to_y_component
        y_to_eta_component(i, j) = inverse_jacobian(i, j)*xi_to_x_component
      end do
    end do
    !$omp end parallel do

  end subroutine compute_transform_metrics

end module grid
