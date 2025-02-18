module cell2node_m

  implicit none

contains

  !******************************************************************************************
  !> @brief セルの配列の値をノード配列の値に変換する
  !> @param cell_array セルの値を持つ配列(cell_count_i, cell_count_j)
  !> @return node_array ノードの値を持つ配列(node_count_i, node_count_j)
  !******************************************************************************************
  subroutine cell2node(cell_array, node_array)

    !> i方向のセルの数
    integer :: cell_count_i
    !> j方向のセルの数
    integer :: cell_count_j
    integer :: i, j
    !> セルの配列
    real(8), dimension(:, :), intent(in)    :: cell_array
    !> 格子点の配列
    real(8), dimension(:, :), intent(inout) :: node_array

    ! セル配列とノード配列のサイズを取得
    cell_count_i = size(cell_array, 1)
    cell_count_j = size(cell_array, 2)

    ! 初期化
    node_array = 0.0d0

    ! 内部ノード: 周囲4つのセルの平均をノードに割り当てる
    do j = 2, cell_count_j
      do i = 2, cell_count_i
        node_array(i, j) = (cell_array(i - 1, j - 1) + cell_array(i - 1, j) + &
                            cell_array(i, j - 1) + cell_array(i, j))*0.25d0
      end do
    end do

    ! 左・右境界
    do j = 2, cell_count_j
      node_array(1, j) = (cell_array(1, j - 1) + cell_array(1, j))*0.5d0
      node_array(cell_count_i + 1, j) = (cell_array(cell_count_i, j - 1) + cell_array(cell_count_i, j))*0.5d0
    end do

    ! 上・下境界
    do i = 2, cell_count_i
      node_array(i, 1) = (cell_array(i - 1, 1) + cell_array(i, 1))*0.5d0
      node_array(i, cell_count_j + 1) = (cell_array(i - 1, cell_count_j) + cell_array(i, cell_count_j))*0.5d0
    end do

    ! コーナー処理
    node_array(1, 1) = cell_array(1, 1)
    node_array(cell_count_i + 1, 1) = cell_array(cell_count_i, 1)
    node_array(1, cell_count_j + 1) = cell_array(1, cell_count_j)
    node_array(cell_count_i + 1, cell_count_j + 1) = cell_array(cell_count_i, cell_count_j)

  end subroutine cell2node

  !******************************************************************************************
  !> @brief ノード配列の値をセルの配列の値に変換する
  !> @param node_array ノードの値を持つ配列(node_count_i, node_count_j)
  !> @return cell_array セルの値を持つ配列(cell_count_i, cell_count_j)
  !******************************************************************************************
  subroutine node2cell(node_array, cell_array)

    !> i方向の格子点の数
    integer :: node_count_i
    !> j方向の格子点の数
    integer :: node_count_j
    integer :: i, j
    !> 格子点の配列
    real(8), dimension(:, :), intent(in) :: node_array
    !> セルの配列
    real(8), dimension(:, :), intent(inout) :: cell_array

    ! ノード配列とセル配列のサイズを取得
    node_count_i = size(node_array, 1)
    node_count_j = size(node_array, 2)

    ! 初期化
    cell_array = 0.0d0

    ! セルの値を周囲4つのノードの平均で計算
    do j = 1, node_count_j - 1
      do i = 1, node_count_i - 1
        cell_array(i, j) = (node_array(i, j) + node_array(i + 1, j) + &
                            node_array(i, j + 1) + node_array(i + 1, j + 1))*0.25d0
      end do
    end do

  end subroutine node2cell

end module cell2node_m
