module timer_module
  implicit none
  integer :: start_clock, clock_rate
  real(8) :: start_cpu_time

contains

  ! 開始時刻とCPU時間を記録するサブルーチン
  subroutine start_timer()
    call system_clock(count_rate=clock_rate)  ! クロックの単位時間を取得
    call system_clock(start_clock)  ! スタート時点のクロックを取得
    call cpu_time(start_cpu_time)  ! スタート時点のCPU時間を取得
    print *, 'Program started...'
  end subroutine start_timer

  ! 終了時に経過時間とCPU時間を計算して出力するサブルーチン
  subroutine end_timer()
    integer :: end_clock, elapsed_hours, elapsed_minutes, fraction_seconds_int
    real(8) :: end_cpu_time, elapsed_time, cpu_elapsed_time
    real(8) :: elapsed_seconds
    integer :: int_elapsed_time, int_cpu_elapsed_time, int_seconds

    ! 実時間の終了時刻を取得
    call system_clock(end_clock)

    ! CPU時間の終了時刻を取得
    call cpu_time(end_cpu_time)

    ! 実時間の経過時間を計算
    elapsed_time = real(end_clock - start_clock) / real(clock_rate)

    ! CPU時間の経過時間を計算
    cpu_elapsed_time = end_cpu_time - start_cpu_time

    ! 実時間を整数秒に変換
    int_elapsed_time = int(elapsed_time)

    ! 実時間をhh:mm:ss形式に変換 (整数部分)
    elapsed_hours = int_elapsed_time / 3600
    elapsed_minutes = mod(int_elapsed_time, 3600) / 60
    int_seconds = mod(int(elapsed_time), 60)
    fraction_seconds_int = int(mod(elapsed_time, 1.0d0) * 100.0d0)  ! 小数部分を整数に変換

    ! hh:mm:ss.ss形式で出力 (秒部分は02.12のようにゼロ埋め)
    write(*,'(A, I2.2, A, I2.2, A, I2.2, ".", I2.2)') 'Elapsed time (real time): ', elapsed_hours, ':', elapsed_minutes, ':', int_seconds, fraction_seconds_int

    ! CPU時間も整数秒に変換
    int_cpu_elapsed_time = int(cpu_elapsed_time)

    ! CPU時間をhh:mm:ss形式に変換 (整数部分)
    elapsed_hours = int_cpu_elapsed_time / 3600
    elapsed_minutes = mod(int_cpu_elapsed_time, 3600) / 60
    int_seconds = mod(int(cpu_elapsed_time), 60)
    fraction_seconds_int = int(mod(cpu_elapsed_time, 1.0d0) * 100.0d0)  ! 小数部分を整数に変換

    ! hh:mm:ss.ss形式で出力 (秒部分は02.12のようにゼロ埋め)
    write(*,'(A, I2.2, A, I2.2, A, I2.2, ".", I2.2)') 'Elapsed time (CPU time):  ', elapsed_hours, ':', elapsed_minutes, ':', int_seconds, fraction_seconds_int

  end subroutine end_timer

end module timer_module
