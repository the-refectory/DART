program test_ensemble_manager

use ensemble_manager_mod, only: get_my_num_copies, &
                                get_my_num_vars,     &
                                ensemble_type,          &
                                init_ensemble_manager, &
                                end_ensemble_manager
                                
use types_mod, only: i8
use mpi_utilities_mod, only: my_task_id, initialize_mpi_utilities, finalize_mpi_utilities

use test

implicit none

!TODO HK layout_type, transpose_type_in, distribution_type_in, transpose_type_in all need tests

! HK Test
!   - large number of vars (bigger than 2,147,483,647 (2^31-1))
!   - num_vars > num_pes
!   - num_vars < num_pes

! public :: ensemble_manager,      end_ensemble_manager,     get_ensemble_time,          &
! ensemble_type,              duplicate_ens,            get_var_owner_index,        &
! get_my_num_copies,          get_my_copies,            get_my_num_vars,            &
! get_my_vars,                compute_copy_mean,        compute_copy_mean_sd,       &
! get_copy,                   put_copy,                 all_vars_to_all_copies,     &
! all_copies_to_all_vars,     allocate_vars,            deallocate_vars,            &
! compute_copy_mean_var,      get_copy_owner_index,     set_ensemble_time,          &
! broadcast_copy,             print_ens_handle,         set_current_time,           &
! map_task_to_pe,             map_pe_to_task,           get_current_time,           &
! allocate_single_copy,       put_single_copy,          get_single_copy,            &
! deallocate_single_copy
!
! public :: copies_in_window, mean_row, set_num_extra_copies,   &
! get_allow_transpose


! test inputs:
!   num_vars, num_copes, distribution_type, layout_type, transpose_type, transpose_type_in
! test inputs namelist: communication_configuration, &
!layout, tasks_per_node

type(ensemble_type) :: ens_handle
integer(i8) :: num_vars
integer :: num_copies
integer :: correct(7), idx

call initialize_mpi_utilities()

call plan(61)


! regular 
num_vars = 1289
num_copies = 80
! fix task id at X
call init_ensemble_manager(ens_handle, num_copies, num_vars)

! check calculations on each PE
idx = my_task_id() + 1 ! my_pe

! For each task id, check the my_num_copies, my_num_vars
correct = [ 1, 1, 1, 1, 1, 1, 1 ]
call ok(get_my_num_copies(ens_handle)==correct(idx), 'get_my_num_copies')

correct = [ 10, 10, 10, 10, 10, 10, 10 ]
call ok(get_my_num_vars(ens_handle)==correct(idx), 'get_my_num_vars')


call pass()
call fail()
call ok(2 == 2)

call end_ensemble_manager(ens_handle)

call finalize_mpi_utilities()

end program test_ensemble_manager
