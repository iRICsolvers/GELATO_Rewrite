program gelate

  use iric
  use common
  use grid
  use result
  use trace
  use fish_module
  use timer_module

  implicit none

  !> メインループで使うループ用変数（読み込む計算結果）
  integer :: time_step
  !> 出力結果のタイムステップ
  integer :: time_step_out
  !> 出力のループで使うループ用変数(time_stepの1回のループでoutput_interval_magnification回ループ)
  integer :: time_step_out_in_magnification
  !> 魚とトレーサーの追跡のループ変数
  integer :: time_step_trace

  !> 読み込んだタイムステップの時刻
  real(8) :: time_in_origin
  !> ひとつ前のタイムステップの時刻
  real(8) :: time_in_previous_origin
  !> 読み込んだタイムステップの時刻
  real(8) :: time_in
  !> ひとつ前のタイムステップの時刻
  real(8) :: time_in_previous
  !> ひとつ前のタイムステップからの経過時間
  real(8) :: delta_time_in

  !> 出力用の時刻
  real(8) :: time_out
  !> 追跡用の時刻
  real(8) :: time_trace
  !> 初期状態の出力用の時刻
  real(8) :: time_out_initial
  !> ひとつ前のタイムステップの時刻
  real(8) :: time_out_previous
  !> 出力時間間隔
  real(8) :: delta_time_out

  ! 何回追跡するか
  integer :: tracking_count

  !> 経過時間
  real(8) :: time_since_start

  !> 停止ボタンが押されたか
  integer :: is_pressed_stop_button

  !> 中心線ポリラインの座標
  real(8), dimension(:), allocatable :: center_line_coordinate_x
  !> 中心線ポリラインの座標
  real(8), dimension(:), allocatable :: center_line_coordinate_y

  !******************************************************************************************
  ! メインプログラムスタート　CGNSファイルを開く
  !******************************************************************************************

  write (*, '(a81)') '======================================Start======================================'
  call start_timer()
  write (*, *) '>>Start Opening CGNS File'

  call open_cgns()

  write (*, '(a)') '    Completed Opening CGNS File'

  !******************************************************************************************
  ! 計算格子読み込み
  !******************************************************************************************

  write (*, *) '>>Start loading Grid conditions'
  call Load_Grid()
  write (*, '(a)') '    Completed loading Grid conditions'

  !******************************************************************************************
  ! 計算条件読み込み
  !******************************************************************************************
  !==========================================================================================
  ! 基本情報の読み込み
  !==========================================================================================

  write (*, *) '>>Start loading Common Parameter'

  call read_common_parameter()
  call write_msg_Common_Parameter()

  write (*, '(a)') '    Completed loading Common Parameter'

  !==========================================================================================
  ! 計算結果の名前の読み込み、メモリ確保
  !==========================================================================================

  if (flow_conditions_used_for_tracking == 0) call read_result_name()
  call allocate_result_value()

  !==========================================================================================
  ! センターラインの座標のメモリ確保、形状作成
  !==========================================================================================
  if (is_draw_center_line == 1) then
    allocate (center_line_coordinate_x(node_count_i), center_line_coordinate_y(node_count_i))
    call create_center_line()
  end if

  !==========================================================================================
  ! トレーサー・windmap・魚の設定読み込み、初期化
  !==========================================================================================

  if (is_trace_primary == 1 .or. is_trace_secondary == 1) call Initialize_Normal_Tracer()
  if (is_trace_trajectory == 1) call Initialize_Trajectory_Tracer()
  if (is_draw_windmap == 1) call Initialize_windmap()
  if (is_simulation_fish == 1) call initialize_fish_tracer()
  ! 魚の数をカウントするセクションのポリゴン形状を作成
  if (is_simulation_fish == 1 .and. is_count_fish == 1) call create_fish_counting_section()

  !******************************************************************************************
  ! 初期状態の計算、アウトプット
  !******************************************************************************************
  !==========================================================================================
  !タイマー等をリセット
  !==========================================================================================

  ! 最初のタイムステップの時刻をセット
  if (is_use_original_time == 1) then
    ! オリジナルの時刻を使う場合
    time_out_initial = time_start_in
  else
    ! 0からスタートする場合
    time_out_initial = 0.0
  end if

  ! 最初のタイムステップなのでとりあえず初期値入れておく
  time_out = time_out_initial
  time_out_previous = time_out_initial
  time_trace = time_out_initial

  ! 初期化
  time_since_start = 0.
  delta_time_in = 0.
  time_step = 1
  time_step_out = 1
  time_step_out_in_magnification = 1

  !==========================================================================================
  ! 流速、水深等を読み込み
  !==========================================================================================

  if (flow_conditions_used_for_tracking == 0) then
    call read_sol_result(1)
    write (*, '(" >>Read flow calculation result CGNS           solID :     1/",i5)') time_step_count_in
  else
    ! 計算に一定流速、水深、標高を使用する場合の処理
    ! 時系列で変化しないのでここで1度だけ実行する
    call read_parameter_for_Trace_from_gui()
  end if

  call cal_parameter_for_Trace()

  !==========================================================================================
  ! 通常トレーサーの初期散布（散布開始時間内であれば）
  !==========================================================================================

  ! 散布開始時間が来たら散布できるようにタイムカウンターをセット
  time_counter_add_normal_tracer = supply_time_interval_normal_tracers

  ! トレーサーの投入
  if ((supply_time_end_normal_tracers + tolerance >= time_trace .and. time_trace + tolerance >= supply_time_start_normal_tracers .and. time_counter_add_normal_tracer + tolerance >= supply_time_interval_normal_tracers)) then

    if (is_trace_primary == 1) call add_normal_tracer(primary)
    if (is_trace_secondary == 1) call add_normal_tracer(secondary)

    ! カウンターリセット
    time_counter_add_normal_tracer = 0

  end if

  !==========================================================================================
  ! 軌跡追跡トレーサーの初期散布（散布開始時間内であれば）
  !==========================================================================================
  if (is_trace_trajectory == 1 .and. trajectory%supply_time <= time_trace + tolerance) then
    call add_trajectory_tracer(trajectory)
  end if

  !==========================================================================================
  ! WindMapの初期散布
  !==========================================================================================
  if (is_draw_windmap == 1) call update_windmap()

  !==========================================================================================
  ! 魚を投入
  !==========================================================================================
  if (is_simulation_fish == 1) call add_fish_tracer()

  !==========================================================================================
  ! 初期状態アウトプット
  !==========================================================================================
  call cg_iric_write_sol_start(cgnsOut, is_error)

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! 時間を出力
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  call cg_iric_write_sol_time(cgnsOut, time_out_initial, is_error)

  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ! 初期状態のスカラー・パーティクルを出力
  !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  call write_sol_result(1)

  write (*, '("  >>Output tracer calculation results   output solID : ",i5,"  output time: ",f8.2)') time_step_out, time_out

  ! 通常トレーサの出力
  if (is_trace_primary == 1) call write_sol_normal_tracer("primary", primary)
  if (is_trace_secondary == 1) call write_sol_normal_tracer("secondary", secondary)
  ! 軌跡追跡トレーサの出力
  if (is_trace_trajectory == 1) call write_sol_trajectory_tracer(trajectory)
  ! windmapの出力
  if (is_draw_windmap == 1) call write_sol_windmap()
  ! 魚の出力
  if (is_simulation_fish == 1) call output_fish()
  ! 魚の数をカウントするセクションのポリゴン形状を出力
  if (is_simulation_fish == 1 .and. is_count_fish == 1) call output_fish_counting_section()
  ! センターラインの出力
  if (is_draw_center_line == 1) call output_center_line()

  call cg_iric_write_sol_end(cgnsOut, is_error)

  !******************************************************************************************
  ! メインループ
  !******************************************************************************************
  !==========================================================================================
  ! 読み込んだ計算結果のタイムステップ毎のループ
  !==========================================================================================
  do time_step = 2, time_step_count_in

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! このタイムステップでの入力結果の時刻を読み込む
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    call cg_iric_read_sol_time(cgnsIn, time_step, time_in_origin, is_error)
    call cg_iric_read_sol_time(cgnsIn, time_step - 1, time_in_previous_origin, is_error)

    if (is_use_original_time == 1) then
      time_in = time_in_origin
      time_in_previous = time_in_previous_origin
    else
      time_in = time_in_origin - time_start_in
      time_in_previous = time_in_previous_origin - time_start_in
    end if

    if (time_in_previous >= time_end_out) then
      write (*, *) "The output end time has been reached!!"
      write (*, '(a81)') '*********************************** Finish !! ***********************************'
      call close_cgns()
      call end_timer()
      stop
    end if

    ! 前回のタイムステップからの時間
    delta_time_in = time_in - time_in_previous
    ! 出力時間間隔
    delta_time_out = delta_time_in/output_interval_magnification
    ! 出力までに何回追跡するか
    tracking_count = int(delta_time_out/time_interval_for_tracking + .5)

    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    ! 計算結果の読み込みと水位、摩擦速度などの計算
    !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    if (flow_conditions_used_for_tracking == 0) then
      write (*, '(" >>Read flow calculation result CGNS           solID : ",i5,"/",i5)') time_step, time_step_count_in
      call read_sol_result(time_step)
      call cal_parameter_for_Trace()
    end if

    !==========================================================================================
    ! 出力毎のタイムステップ
    !==========================================================================================
    do time_step_out_in_magnification = 1, output_interval_magnification

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! ソルバーの中断処理
      ! iRIC GUIで停止ボタンが押されたかチェック 押された場合は計算を停止する。
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      call iric_check_cancel(is_pressed_stop_button)
      if (is_pressed_stop_button == 1) then
        write (*, *) "Solver is stopped because the STOP button was clicked."
        write (*, '(a81)') '*********************************** Finish !! ***********************************'
        call close_cgns()
        call end_timer()
        stop
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 出力用の時刻の計算
      ! 入力タイムステップ毎での出力時に小数点誤差で変な値になられるのが嫌なので安全策
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (time_step_out_in_magnification == output_interval_magnification) then
        time_out = time_in
      else
        time_out = time_in_previous + delta_time_out*real(time_step_out_in_magnification)
      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! 計算終了時刻を超えたらソルバーを終了する
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (time_out > time_end_out) then
        write (*, *) "The output end time has been reached!!"
        write (*, '(a81)') '*********************************** Finish !! ***********************************'
        call close_cgns()
        call end_timer()
        stop
      end if

      ! 出力タイムステップを増加
      call increment_integer_value(time_step_out, 1)

      ! 計算結果出力開始を宣言
      call cg_iric_write_sol_start(cgnsOut, is_error)
      ! 時刻を出力
      call cg_iric_write_sol_time(cgnsOut, time_out, is_error)
      ! 読み込んだ計算結果の出力
      call write_sol_result(time_step)
      write (*, '("  >>Output tracer calculation results   output solID : ",i5,"  output time: ",f8.2)') time_step_out, time_out

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! トレーサー、魚の処理
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (is_trace_primary == 1 .or. is_trace_secondary == 1 .or. is_trace_trajectory == 1 .or. is_simulation_fish == 1 .or. is_draw_windmap == 1) then

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 移動、追加
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        do time_step_trace = 1, tracking_count

          ! トレーサーを1回追跡した後の時刻
          time_trace = time_out_previous + time_interval_for_tracking*time_step_trace
          ! 経過時間
          time_since_start = time_trace - time_out_initial

          ! トレーサー追加のカウンター更新
          call increment_real_value(time_counter_add_normal_tracer, time_interval_for_tracking)

          !------------------------------------------------------------------------------------------
          ! トレーサーの追跡
          !------------------------------------------------------------------------------------------
          ! 通常トレーサーの移動
          if (is_trace_primary == 1 .and. primary%total_tracer_number > 0) call move_normal_tracer(primary)
          if (is_trace_secondary == 1 .and. secondary%total_tracer_number > 0) call move_normal_tracer(secondary)

          ! 軌跡追跡トレーサーの移動
          if (is_trace_trajectory == 1 .and. trajectory%total_tracer_number > 0) call move_trajectory_tracer(trajectory)

          !------------------------------------------------------------------------------------------
          ! トレーサーの投入
          !------------------------------------------------------------------------------------------
          ! 通常トレーサーの投入
          if ((supply_time_end_normal_tracers + tolerance >= time_trace .and. time_trace + tolerance >= supply_time_start_normal_tracers .and. time_counter_add_normal_tracer + tolerance >= supply_time_interval_normal_tracers)) then
            if (is_trace_primary == 1 .and. primary%total_tracer_number < primary%max_number) call add_normal_tracer(primary)
            if (is_trace_secondary == 1 .and. secondary%total_tracer_number < secondary%max_number) call add_normal_tracer(secondary)
            ! カウンターリセット
            time_counter_add_normal_tracer = 0.0
          end if

          ! 軌跡追跡トレーサーの投入
          if (trajectory%is_added_trajectory_tracer == 0) then
            if (trajectory%supply_time <= time_trace + tolerance) then
              call add_trajectory_tracer(trajectory)
            end if
          end if

          !------------------------------------------------------------------------------------------
          ! WindMapの更新
          !------------------------------------------------------------------------------------------
          if (is_draw_windmap == 1) call update_windmap()

          !------------------------------------------------------------------------------------------
          ! 魚の移動
          !------------------------------------------------------------------------------------------
          if (is_simulation_fish == 1) call move_fish_tracer(time_trace)

        end do !time_step_trace = 1, tracking_count

        !------------------------------------------------------------------------------------------
        ! トレーサーのクローン
        !------------------------------------------------------------------------------------------
        if (is_trace_primary == 1 .and. primary%is_tracer_cloning == 1 .and. primary%total_tracer_number < primary%max_number) then

          if (primary%cloning_option == 0) call add_all_empty_cells(primary)
          if (primary%cloning_option == 1 .or. primary%cloning_option == 2) call clone_tracer(primary)

        end if
        if (is_trace_secondary == 1 .and. secondary%is_tracer_cloning == 1 .and. secondary%total_tracer_number < secondary%max_number) then

          if (secondary%cloning_option == 0) call add_all_empty_cells(secondary)
          if (secondary%cloning_option == 1 .or. secondary%cloning_option == 2) call clone_tracer(secondary)

        end if

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! トレーサー数の統計を計算
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (primary%total_tracer_number > 0) call calculate_tracer_statistics(time_since_start, primary)
        if (secondary%total_tracer_number > 0) call calculate_tracer_statistics(time_since_start, secondary)

        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! トレーサーの出力
        !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        ! 通常トレーサの出力
        if (is_trace_primary == 1) call write_sol_normal_tracer("primary", primary)
        if (is_trace_secondary == 1) call write_sol_normal_tracer("secondary", secondary)
        ! 軌跡追跡トレーサの出力
        if (is_trace_trajectory == 1) call write_sol_trajectory_tracer(trajectory)
        ! windmapの出力
        if (is_draw_windmap == 1) call write_sol_windmap()
        ! 魚の出力
        if (is_simulation_fish == 1) call output_fish()
        ! 魚の数をカウントするセクションのポリゴン形状を出力
        if (is_simulation_fish == 1 .and. is_count_fish == 1) call output_fish_counting_section()

      end if

      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      ! センターラインの出力
      !^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      if (is_draw_center_line == 1) call output_center_line()

      ! 出力終了を宣言
      call cg_iric_write_sol_end(cgnsOut, is_error)

      ! ひとつ前の時間を更新
      time_out_previous = time_out

    end do !do time_step_out_in_magnification = 1, output_interval_magnification

  end do ! time_step = 2, time_step_count_in

  write (*, '(a81)') '*********************************** Finish !! ***********************************'
  call close_cgns()
  call end_timer()
  stop

contains
  !==========================================================================================
  ! サブルーチン群
  !==========================================================================================

  !******************************************************************************************
  !> @brief 基本情報のコンソール出力
  !******************************************************************************************
  subroutine write_msg_Common_Parameter()
    !==========================================================================================
    ! 基本的なパラーメータをコンソールに出力する
    !==========================================================================================

    write (*, '("  Delta T for Tracer Tracking       : ", f10.4)') time_interval_for_tracking
    write (*, '("  Computation Finishing Time(sec)   : ", f10.4)') time_end_out
    write (*, '("  Output frequency increase factor  : ", i14)') output_interval_magnification
    if (is_draw_center_line == 0) then
      write (*, '(a)') "  Drawing Center Line               :        disable"
    else
      write (*, '(a)') "  Drawing Center Line               :         enable"
    end if
    if (is_trace_primary == 0) then
      write (*, '(a)') "  Trace primary tracer              :        disable"
    else
      write (*, '(a)') "  Trace primary tracer              :         enable"
    end if
    if (is_trace_secondary == 0) then
      write (*, '(a)') "  Trace secondary tracer            :        disable"
    else
      write (*, '(a)') "  Trace secondary tracer            :         enable"
    end if
    if (is_trace_trajectory == 0) then
      write (*, '(a)') "  Trace Trajectory tracer           :        disable"
    else
      write (*, '(a)') "  Trace Trajectory tracer           :         enable"
    end if
    if (is_draw_windmap == 0) then
      write (*, '(a)') "  Drawing wind map                  :        disable"
    else
      write (*, '(a)') "  Drawing wind map                  :         enable"
    end if
    if (is_simulation_fish == 0) then
      write (*, '(a)') "  Fish simulation                   :        disable"
    else
      write (*, '(a)') "  Fish simulation                   :         enable"
    end if
    if (is_draw_vegetation == 0) then
      write (*, '(a)') "  Drawing vegetation                :        disable"
    else
      write (*, '(a)') "  Drawing vegetation                :         enable"
    end if
    if (is_draw_gravel == 0) then
      write (*, '(a)') "  Drawing gravel                    :        disable"
    else
      write (*, '(a)') "  Drawing gravel                    :         enable"
    end if

  end subroutine write_msg_Common_Parameter

  !******************************************************************************************
  !> @brief センターラインのポリゴンを作成する
  !******************************************************************************************
  subroutine create_center_line()

    !> j方向の格子点数が奇数か
    integer :: is_odd

    ! node_count_jが偶数か奇数かを計算
    is_odd = mod(node_count_j, 2)

    !==========================================================================================
    ! node_count_jが奇数の時の処理
    !==========================================================================================
    if (is_odd == 1) then
      ! node_coordinate_x, node_coordinate_yのインデックスj_centerの値をcenter_line_codinate_x, center_line_codinate_yにコピー
      center_line_coordinate_x(:) = node_coordinate_x(:, j_center)
      center_line_coordinate_y(:) = node_coordinate_y(:, j_center)
    else
      !==========================================================================================
      ! node_count_jが偶数の時の処理
      !==========================================================================================
      ! node_codinate_x, node_codinate_yはjcenterとjcenter+1の値の平均をとる
      center_line_coordinate_x(:) = (node_coordinate_x(:, j_center) + node_coordinate_x(:, j_center + 1))*0.5d0
      center_line_coordinate_y(:) = (node_coordinate_y(:, j_center) + node_coordinate_y(:, j_center + 1))*0.5d0
    end if
  end subroutine create_center_line

  !******************************************************************************************
  !> @brief センターラインのポリラインの出力
  !******************************************************************************************
  subroutine output_center_line()

    call cg_iric_write_sol_polydata_groupbegin(cgnsOut, "Center Line", is_error)

    call cg_iric_write_sol_polydata_polyline(cgnsOut, node_count_i, center_line_coordinate_x, center_line_coordinate_y, is_error)

    call cg_iric_write_sol_polydata_groupend(cgnsOut, is_error)

  end subroutine output_center_line

end program gelate

