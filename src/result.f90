module result
  !******************************************************************************************
  ! 計算結果を管理するモジュール
  !******************************************************************************************
  use iric
  use cell2node_m
  use common
  use grid

  implicit none
  !******************************************************************************************
  ! モジュール内変数
  !******************************************************************************************
  !==========================================================================================
  ! 計算に必要な計算結果
  !==========================================================================================

  !> X軸流速が含まれている配列の名前
  character(len=strMax) :: velocity_x_name
  !> Y軸流速が含まれている配列の名前
  character(len=strMax) :: velocity_y_name
  !> 水深が含まれる計算結果の配列の名前
  character(len=strMax) :: depth_name
  !> 河床高が含まれる計算結果の配列の名前
  character(len=strMax) :: elevation_name

  !> X軸流速を格納する配列
  real(8), dimension(:, :), allocatable :: velocity_x_node
  !> Y軸流速を格納する配列
  real(8), dimension(:, :), allocatable :: velocity_y_node
  !> 水深を格納する配列
  real(8), dimension(:, :), allocatable :: depth_node
  !> 標高を格納する配列
  real(8), dimension(:, :), allocatable :: elevation_node

  !> 水深を格納する配列
  real(8), dimension(:, :), allocatable :: depth_cell
  !> 標高を格納する配列
  real(8), dimension(:, :), allocatable :: elevation_cell

  !==========================================================================================
  ! 参考として読み込む計算結果
  !==========================================================================================
  !> 流量を読み込むかどうか
  integer :: is_load_discharge
  !> 河床変動量を読み込むかどうか
  integer :: is_load_elevation_change
  !> 渦度を読み込むかどうか
  integer :: is_load_vorticity
  !> 染料濃度を読み込むかどうか
  integer :: is_load_dye_concentration

  !> 流量が含まれる計算結果の値の名前
  character(len=strMax) :: discharge_name
  !> 河床変動量が含まれる計算結果の配列の名前
  character(len=strMax) :: elevation_change_name
  !> 渦度が含まれる計算結果の配列名称
  character(len=strMax) :: vorticity_name
  !> 染料濃度が含まれる計算結果の配列名称
  character(len=strMax) :: dye_concentration_name

  !> 流量を格納する配列
  real(8), dimension(:), allocatable :: discharge
  !> 河床変動量を格納する配列
  real(8), dimension(:, :), allocatable :: elevation_change_node
  !> 河床変動量を格納する配列
  real(8), dimension(:, :), allocatable :: vorticity_node
  !> 河床変動量を格納する配列
  real(8), dimension(:, :), allocatable :: dye_concentration_node

  !==========================================================================================
  ! 計算して求める値
  !==========================================================================================
  !> 水位を格納する配列(格子点)
  real(8), dimension(:, :), allocatable :: water_surface_elevation_node
  !> 水位を格納する配列(セル)
  real(8), dimension(:, :), allocatable :: water_surface_elevation_cell
  !> 渦動粘性係数を格納する配列
  real(8), dimension(:, :), allocatable :: eddy_viscosity_coefficient_node
  !> 摩擦速度
  real(8), dimension(:, :), allocatable :: u_star_node
  !> 摩擦速度
  real(8), dimension(:, :), allocatable :: u_star_cell
  !> ξ方向流速
  real(8), dimension(:, :), allocatable :: velocity_xi_node
  !> η方向流速
  real(8), dimension(:, :), allocatable :: velocity_eta_node

contains

  !******************************************************************************************
  !> 読み込みに必要な計算結果の名前を取得
  subroutine read_result_name()

    implicit none

    ! 計算に必要な計算結果読み込み
    call cg_iric_read_string(cgnsOut, "velocity_x_name", velocity_x_name, is_error)
    call cg_iric_read_string(cgnsOut, "velocity_y_name", velocity_y_name, is_error)
    call cg_iric_read_string(cgnsOut, "depth_name", depth_name, is_error)
    call cg_iric_read_string(cgnsOut, "elevation_name", elevation_name, is_error)

    ! 参考の条件を読み込み有無
    call cg_iric_read_integer(cgnsOut, "is_load_discharge", is_load_discharge, is_error)
    call cg_iric_read_integer(cgnsOut, "is_load_elevation_change", is_load_elevation_change, is_error)
    call cg_iric_read_integer(cgnsOut, "is_load_vorticity", is_load_vorticity, is_error)
    call cg_iric_read_integer(cgnsOut, "is_load_dye_concentration", is_load_discharge, is_error)

    ! 有無に応じて名前読み込み
    if (is_load_discharge == 1) call cg_iric_read_string(cgnsOut, "discharge_name", discharge_name, is_error)
    if (is_load_elevation_change == 1) call cg_iric_read_string(cgnsOut, "elevation_change_name", elevation_change_name, is_error)
    if (is_load_vorticity == 1) call cg_iric_read_string(cgnsOut, "vorticity_name", vorticity_name, is_error)
    if (is_load_dye_concentration == 1) call cg_iric_read_string(cgnsOut, "dye_concentration_name", dye_concentration_name, is_error)

  end subroutine read_result_name

  !******************************************************************************************
  !> 変数のメモリを確保
  subroutine allocate_result_value()

    implicit none

    allocate (velocity_x_node(node_count_i, node_count_j))
    allocate (velocity_y_node(node_count_i, node_count_j))
    allocate (depth_node(node_count_i, node_count_j))
    allocate (depth_cell(cell_count_i, cell_count_j))
    allocate (elevation_node(node_count_i, node_count_j))
    allocate (elevation_cell(cell_count_i, cell_count_j))

    if (flow_conditions_used_for_tracking == 0) then
      allocate (velocity_xi_node(node_count_i, node_count_j))
      allocate (velocity_eta_node(node_count_i, node_count_j))
    end if

    ! hoshino memo
    ! flow_conditions_used_for_trackingに関わらずallocate_result_valueを呼ぶので流量などcgnsから読み込むとき限定の値だけは条件分岐させる
    if (flow_conditions_used_for_tracking == 0) then
      if (is_load_discharge == 1) allocate (discharge(time_step_count_in))
      if (is_load_elevation_change == 1) allocate (elevation_change_node(node_count_i, node_count_j))
      if (is_load_vorticity == 1) allocate (vorticity_node(node_count_i, node_count_j))
      if (is_load_dye_concentration == 1) allocate (dye_concentration_node(node_count_i, node_count_j))
    end if

    allocate (water_surface_elevation_node(node_count_i, node_count_j))
    allocate (water_surface_elevation_cell(cell_count_i, cell_count_j))
    allocate (eddy_viscosity_coefficient_node(node_count_i, node_count_j))
    allocate (u_star_node(node_count_i, node_count_j))
    allocate (u_star_cell(cell_count_i, cell_count_j))

  end subroutine allocate_result_value

  !******************************************************************************************
  !> 指定されたタイムステップの計算結果を読み込み
  !> @param step タイムステップ
  subroutine read_sol_result(step)
    implicit none
    !> タイムステップ
    integer :: step

    call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(velocity_x_name), velocity_x_node, is_error)
    call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(velocity_y_name), velocity_y_node, is_error)
    call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(depth_name), depth_node, is_error)
    call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(elevation_name), elevation_node, is_error)

    if (is_load_discharge == 1) call cg_iric_read_sol_baseiterative_real(cgnsIn, step, trim(discharge_name), discharge(step), is_error)
    if (is_load_elevation_change == 1) call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(elevation_change_name), elevation_change_node, is_error)
    if (is_load_vorticity == 1) call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(vorticity_name), vorticity_node, is_error)
    if (is_load_dye_concentration == 1) call cg_iric_read_sol_node_real_withgridid(cgnsIn, 1, step, trim(dye_concentration_name), dye_concentration_node, is_error)

  end subroutine read_sol_result

  !******************************************************************************************
  !> GUIで入力した一定値から計算に使用する値を計算
  subroutine read_parameter_for_Trace_from_gui()
    implicit none
    !> GUIから読み込んだ一定流速
    real(8) :: Constant_velocity
    !> GUIから読み込んだ一定水深
    real(8) :: Constant_Depth
    !> 一般座標系に変換した一定流速
    real(8) :: Constant_velocity_xi

    !> i方向ループ用の変数
    integer :: i
    !> j方向ループ用の変数
    integer :: j

    !==========================================================================================
    ! 必要なパラメーターの読み込み
    !==========================================================================================
    call cg_iric_read_real(cgnsOut, "Constant_velocity", Constant_velocity, is_error)
    call cg_iric_read_real(cgnsOut, "Constant_Depth", Constant_Depth, is_error)
    call cg_iric_read_grid_real_node(cgnsOut, "elevation", elevation_node, is_error)

    !==========================================================================================
    ! 各格子点での一般座標系の流速を計算
    !==========================================================================================
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 水路上流端中心部(j=j_center)の流速を一般座標系に
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    Constant_velocity_xi = x_to_xi_component(1, j_center)*Constant_velocity

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 水路上流端中心部(j=j_center)の流速を全ての格子点に適用
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    velocity_xi_node(i, j) = Constant_velocity_xi
    velocity_eta_node(i, j) = 0.

    !==========================================================================================
    ! 各格子点での物理座標系の流速、水深を計算
    ! 逆ヤコビ行列を用いて一般座標系から物理座標系に変換
    ! 本来の変換式：
    ! velocity_x_node = xi_to_x_component * velocity_xi_node + eta_to_x_component * velocity_eta_node
    ! velocity_y_node = xi_to_y_component * velocity_xi_node + eta_to_y_component * velocity_eta_node
    ! 逆ヤコビ行列の定義より：
    ! x_to_xi_component  = inverse_jacobian * eta_to_y_component
    ! y_to_xi_component  = -inverse_jacobian * eta_to_x_component
    ! x_to_eta_component = -inverse_jacobian * xi_to_y_component
    ! y_to_eta_component = inverse_jacobian * xi_to_x_component
    !==========================================================================================

    velocity_x_node = (y_to_eta_component*velocity_xi_node - y_to_xi_component*velocity_eta_node)/inverse_jacobian
    velocity_y_node = (-x_to_eta_component*velocity_xi_node + x_to_xi_component*velocity_eta_node)/inverse_jacobian
    depth_node = Constant_Depth

  end subroutine read_parameter_for_Trace_from_gui

  !******************************************************************************************
  !> 計算に使用する値を計算
  subroutine cal_parameter_for_Trace()
    implicit none

    integer :: i, j
    !> 流速ベクトルの大きさ
    real(8) :: velocity_magnitude
    !> エネルギー勾配
    real(8) :: energy_slope

    !==========================================================================================
    ! 水位の計算
    !==========================================================================================
    water_surface_elevation_node = elevation_node + depth_node

    !==========================================================================================
    ! 摩擦速度、渦動粘性係数の計算
    !==========================================================================================
    do j = 1, node_count_j
      do i = 1, node_count_i
        if (depth_node(i, j) > 1e-8) then
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          ! 格子点での水深がある場合
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          velocity_magnitude = sqrt(velocity_x_node(i, j)**2 + velocity_y_node(i, j)**2)            ! 格子点での流速ベクトルの大きさ
          energy_slope = velocity_magnitude**2*roughness_node(i, j)**2/depth_node(i, j)             ! エネルギー勾配の計算
          u_star_node(i, j) = sqrt(g*depth_node(i, j)*energy_slope)                                       ! 摩擦速度の計算
          eddy_viscosity_coefficient_node(i, j) = karman_constant/6.*u_star_node(i, j)*depth_node(i, j)  ! 渦動粘性係数の計算

        else
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          ! 格子点での水深がない場合
          !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          u_star_node(i, j) = 0.                        ! 摩擦速度
          eddy_viscosity_coefficient_node(i, j) = 0.    ! 渦動粘性係数

        end if
      end do
    end do

    !==========================================================================================
    ! 流速（一般座標系)の計算
    !==========================================================================================
    if (flow_conditions_used_for_tracking == 0) then
      velocity_xi_node = x_to_xi_component*velocity_x_node + y_to_xi_component*velocity_y_node
      velocity_eta_node = x_to_eta_component*velocity_x_node + y_to_eta_component*velocity_y_node
    end if

    !==========================================================================================
    ! セルでの河床高、水位、水深、摩擦速度の計算
    !==========================================================================================
    call node2cell(elevation_node, elevation_cell)
    call node2cell(water_surface_elevation_node, water_surface_elevation_cell)
    call node2cell(depth_node, depth_cell)
    call node2cell(u_star_node, u_star_cell)

  end subroutine cal_parameter_for_Trace

  !******************************************************************************************
  !> 読み込んだ計算結果を出力
  !> @parm time_step タイムステップ
  subroutine write_sol_result(time_step_in)
    !> メインループで使うループ用変数（読み込む計算結果）
    integer :: time_step_in

    ! 流速
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'VelocityX', velocity_x_node, is_error)
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'VelocityY', velocity_y_node, is_error)
    ! 水深
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'Depth', depth_node, is_error)
    ! 河床高
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'Elevation', elevation_node, is_error)
    ! 水位
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'WaterSurface', water_surface_elevation_node, is_error)
    !摩擦速度
    call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'ShearVelocity', u_star_node, is_error)
    ! 流量
    if (is_load_discharge == 1) call cg_iric_write_sol_baseiterative_real(cgnsOut, 'Discharge', discharge(time_step_in), is_error)
    ! 渦度
    if (is_load_vorticity == 1) call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'Vorticity', vorticity_node, is_error)
    ! 染料濃度
    if (is_load_dye_concentration == 1) call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'DyeConcentration', dye_concentration_node, is_error)
    ! 河床変動量
    if (is_load_elevation_change == 1) call cg_iRIC_Write_Sol_Node_Real(cgnsOut, 'ElevationChange', elevation_change_node, is_error)

  end subroutine write_sol_result

end module
