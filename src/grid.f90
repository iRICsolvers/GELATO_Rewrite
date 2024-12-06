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
  double precision, dimension(:, :), allocatable :: node_coordinate_x
  !> 格子のy座標
  double precision, dimension(:, :), allocatable :: node_coordinate_y

  !> 水路長
  double precision :: channel_length

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
  double precision, dimension(:, :), allocatable :: roughness_cell
  !> マニング粗度(格子点)
  double precision, dimension(:, :), allocatable :: roughness_node
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
  double precision :: grid_interval_xi
  !> ξ方向物理座標での格子点間距離の最小値
  double precision :: grid_physical_distance_xi_min
  !> ξ方向スケーリング係数
  double precision, dimension(:, :), allocatable :: scale_factor_xi
  !> ξ方向でのxの勾配(中央差分法)
  double precision, dimension(:, :), allocatable :: x_gradient_in_xi
  !> ξ方向でのyの勾配(中央差分法)
  double precision, dimension(:, :), allocatable :: y_gradient_in_xi
  !> ξ方向の変位を x方向に変換するための変換行列の要素
  double precision, dimension(:, :), allocatable :: xi_to_x_component
  !> ξ方向の変位を y方向に変換するための変換行列の要素
  double precision, dimension(:, :), allocatable :: xi_to_y_component

  !> η方向一般座標系格子間隔
  double precision :: grid_interval_eta
  !> η方向物理座標での格子点間距離の最小値
  double precision :: grid_physical_distance_eta_min
  !> η方向スケーリング係数
  double precision, dimension(:, :), allocatable :: scale_factor_eta
  !> ξ方向でのxの勾配(中央差分法)
  double precision, dimension(:, :), allocatable :: x_gradient_in_eta
  !> ξ方向でのyの勾配(中央差分法)
  double precision, dimension(:, :), allocatable :: y_gradient_in_eta
  !> ξ方向の変位を x方向に変換するための変換行列の要素
  double precision, dimension(:, :), allocatable :: eta_to_x_component
  !> ξ方向の変位を y方向に変換するための変換行列の要素
  double precision, dimension(:, :), allocatable :: eta_to_y_component

  !> ヤコビアンの逆数
  double precision, dimension(:, :), allocatable :: inverse_jacobian

contains

  !******************************************************************************************
  !> 出力用格子の読み込みの計算
  subroutine Load_Grid()

    implicit none

    integer :: i, j

    !==========================================================================================
    ! 格子点の数の読み込み
    !==========================================================================================
    call cg_iric_read_grid2d_str_size(cgnsOut, node_count_i, node_count_j, is_error)
    cell_count_i = node_count_i - 1
    cell_count_j = node_count_j - 1

    j_center = node_count_j/2

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

    call compute_transform_metrics

  end subroutine Load_Grid

  !******************************************************************************************
  !> 物理座標(x,y)と一般座標(ξ,η)を変換するためのパラメータ計算
  subroutine compute_transform_metrics()

    implicit none

    !> ξ方向一般座標系格子間隔
    double precision :: grid_physical_distance_xi
    !> η方向一般座標系格子間隔
    double precision :: grid_physical_distance_eta
    !> 特定の格子でのξ方向におけるxの勾配
    double precision :: x_base_gradient_in_xi
    !> 特定の格子でのξ方向におけるyの勾配
    double precision :: y_base_gradient_in_xi
    !> 特定の格子でのη方向におけるxの勾配
    double precision :: x_base_gradient_in_eta
    !> 特定の格子でのη方向におけるyの勾配
    double precision :: y_base_gradient_in_eta

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
    allocate (xi_to_x_component(node_count_i, node_count_j))
    allocate (xi_to_y_component(node_count_i, node_count_j))
    allocate (eta_to_x_component(node_count_i, node_count_j))
    allocate (eta_to_y_component(node_count_i, node_count_j))

    !==========================================================================================
    ! 水路延長、ξ,η方向のスケーリング係数の計算
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

    !$omp parallel do collapse(2) private(i, j, x_base_gradient_in_xi, y_base_gradient_in_xi, x_base_gradient_in_eta, y_base_gradient_in_eta) shared(node_coordinate_x, node_coordinate_y, inverse_jacobian, xi_to_x_component, xi_to_y_component, eta_to_x_component, eta_to_y_component)
    do j = 1, node_count_j
      do i = 1, node_count_i
        !==========================================================================================
        ! ξ、η方向でのx,yの勾配(2点差分方法)
        !==========================================================================================
        if (i == 1) then
          x_base_gradient_in_xi = (node_coordinate_x(i + 1, j) - node_coordinate_x(i, j))/grid_interval_xi
          y_base_gradient_in_xi = (node_coordinate_y(i + 1, j) - node_coordinate_y(i, j))/grid_interval_xi
        else if (i == node_count_i) then
          x_base_gradient_in_xi = (node_coordinate_x(i, j) - node_coordinate_x(i - 1, j))/grid_interval_xi
          y_base_gradient_in_xi = (node_coordinate_y(i, j) - node_coordinate_y(i - 1, j))/grid_interval_xi
        else
          x_base_gradient_in_xi = (node_coordinate_x(i + 1, j) - node_coordinate_x(i - 1, j))/(2.*grid_interval_xi)
          y_base_gradient_in_xi = (node_coordinate_y(i + 1, j) - node_coordinate_y(i - 1, j))/(2.*grid_interval_xi)
        end if

        if (j == 1) then
          x_base_gradient_in_eta = (node_coordinate_x(i, j + 1) - node_coordinate_x(i, j))/grid_interval_eta
          y_base_gradient_in_eta = (node_coordinate_y(i, j + 1) - node_coordinate_y(i, j))/grid_interval_eta
        else if (j == node_count_j) then
          x_base_gradient_in_eta = (node_coordinate_x(i, j) - node_coordinate_x(i, j - 1))/grid_interval_eta
          y_base_gradient_in_eta = (node_coordinate_y(i, j) - node_coordinate_y(i, j - 1))/grid_interval_eta
        else
          x_base_gradient_in_eta = (node_coordinate_x(i, j + 1) - node_coordinate_x(i, j - 1))/(2.*grid_interval_eta)
          y_base_gradient_in_eta = (node_coordinate_y(i, j + 1) - node_coordinate_y(i, j - 1))/(2.*grid_interval_eta)
        end if

        !==========================================================================================
        ! ξ、η方向の変位を x,y方向に変換するための変換行列の要素
        !==========================================================================================

        inverse_jacobian(i, j) = 1./(x_base_gradient_in_xi*y_base_gradient_in_eta - x_base_gradient_in_eta*y_base_gradient_in_xi)
        xi_to_x_component(i, j) = inverse_jacobian(i, j)*y_base_gradient_in_eta
        xi_to_y_component(i, j) = -inverse_jacobian(i, j)*x_base_gradient_in_eta
        eta_to_x_component(i, j) = -inverse_jacobian(i, j)*y_base_gradient_in_xi
        eta_to_y_component(i, j) = inverse_jacobian(i, j)*x_base_gradient_in_xi
      end do
    end do
    !$omp end parallel do

  end subroutine compute_transform_metrics

end module grid
