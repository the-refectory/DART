module netcdf_variables_mod

use types_mod, only: r8, r4, MISSING_I, MISSING_R4, MISSING_R8

implicit none

public :: get_missing_value, &
          get_FillValue, &
          set_clamping, &
          interface get_missing_value
   module procedure get_missing_value_r8
   module procedure get_missing_value_r4
   module procedure get_missing_value_int
end interface

interface get_FillValue
   module procedure get_spval_r8
   module procedure get_spval_r4
   module procedure get_spval_int
end interface

type io_information
   private

   ! netcdf variable id
   integer :: varid ! HK Do you want one for reading, one for writing?
   integer :: xtype     ! netCDF variable type (NF90_double, etc.)

   ! clamping variables
   logical  :: clamping = .false.    ! does variable need to be range-restricted before
   real(r8) :: minvalue = missing_r8 ! min value for clamping
   real(r8) :: maxvalue = missing_r8 ! max value for clamping
   ! logical  :: out_of_range_fail = .false. ! is out of range fatal if range-checking?

   ! dimension information, including unlimited dimensions
   integer :: io_numdims = 0
   integer, dimension(NF90_MAX_VAR_DIMS) :: io_dimIds
   integer, dimension(NF90_MAX_VAR_DIMS) :: dimlens

   ! update information
   logical :: update = .true. ! default to update variables

   ! CF-Conventions
   character(len=NF90_MAX_NAME) :: units = ' '
   character(len=NF90_MAX_NAME) :: short_name = ' '
   character(len=NF90_MAX_NAME) :: long_name = ' '
   logical  :: has_missing_value = .false.
   logical  :: has_FillValue = .false.
   integer  :: missingINT = MISSING_I   ! missing values
   real(r4) :: missingR4 = MISSING_R4
   real(r8) :: missingR8 = MISSING_R8
   integer  :: spvalINT = MISSING_I   ! fill values
   real(r4) :: spvalR4 = MISSING_R4
   real(r8) :: spvalR8 = MISSING_R8
   real(r8) :: scale_factor = MISSING_R8
   real(r8) :: add_offset = MISSING_R8

end type io_information

contains

!-------------------------------------------------------------------------------
!> Return the number of dimensions in a domain
!> Repeat dimensions are allowed
function get_domain_num_dims(dom_id)

integer, intent(in) :: dom_id ! domain identifier
integer :: get_domain_num_dims

integer :: ivar, num_vars

num_vars = state%domain(dom_id)%num_variables

get_domain_num_dims = 0

do ivar = 1, num_vars
   get_domain_num_dims =  get_domain_num_dims + &
                              state%domain(dom_id)%variable(ivar)%io_info%io_numdims
enddo

end function get_domain_num_dims

!-------------------------------------------------------------------------------
!> Return unlimited dimension id
function get_unlimited_dimid(dom_id)

integer, intent(in) :: dom_id
integer :: get_unlimited_dimid

get_unlimited_dimid = state%domain(dom_id)%unlimDimId

end function get_unlimited_dimid

!-------------------------------------------------------------------------------
!> Return the unique dimension names
function get_io_unique_dim_name(dom_id, jdim)

integer, intent(in) :: dom_id ! domain identifier
integer, intent(in) :: jdim ! index into array, not connected to dimId
character(len=NF90_MAX_NAME) :: get_io_unique_dim_name

get_io_unique_dim_name = state%domain(dom_id)%unique_dim_names(jdim)

end function get_io_unique_dim_name


!-------------------------------------------------------------------------------
!> Return the unique dimension lengths
function get_io_unique_dim_length(dom_id, jdim)

integer, intent(in) :: dom_id ! domain identifier
integer, intent(in) :: jdim ! index into array, not connected to dimId
integer :: get_io_unique_dim_length

get_io_unique_dim_length = state%domain(dom_id)%unique_dim_length(jdim)

end function get_io_unique_dim_length

!-------------------------------------------------------------------------------
!> Return the number of unique dimensions
function get_io_num_unique_dims(dom_id)

integer, intent(in) :: dom_id ! domain identifier
integer :: get_io_num_unique_dims

get_io_num_unique_dims = state%domain(dom_id)%num_unique_dims

end function get_io_num_unique_dims

!-------------------------------------------------------------------------------
!> Returns whether a variable should be clamped or not
function do_io_clamping(dom_id, var_id)

integer, intent(in) :: dom_id ! domain identifier
integer, intent(in) :: var_id
logical :: do_io_clamping

do_io_clamping = state%domain(dom_id)%variable(var_id)%io_info%clamping

end function do_io_clamping
   
!-------------------------------------------------------------------------------
!> Return clamping minimum for a given variable
function get_io_clamping_minval(dom_id, var_id)

integer, intent(in) :: dom_id
integer, intent(in) :: var_id
real(r8) :: get_io_clamping_minval

get_io_clamping_minval= state%domain(dom_id)%variable(var_id)%io_info%minvalue

end function get_io_clamping_minval
   

!-------------------------------------------------------------------------------
!> Return clamping maximum for a given variable
function get_io_clamping_maxval(dom_id, var_id)

integer, intent(in) :: dom_id
integer, intent(in) :: var_id
real(r8) :: get_io_clamping_maxval

get_io_clamping_maxval= state%domain(dom_id)%variable(var_id)%io_info%maxvalue

end function get_io_clamping_maxval

!-------------------------------------------------------------------------------
!> Return io dimension length
function get_io_dim_length(dom_id, ivar, jdim)

integer, intent(in) :: jdim ! dimension
integer :: get_io_dim_length

integer, intent(in) :: dom_id ! domain
integer, intent(in) :: ivar ! variable

get_io_dim_length = state%domain(dom_id)%variable(ivar)%io_info%dimlens(jdim)

end function get_io_dim_length

!-------------------------------------------------------------------------------
!> Return whether the domain has an unlimited dimension
! HK @todo ulimited dimension is by variable
function has_unlimited_dim(dom_id)

integer, intent(in) :: dom_id
logical :: has_unlimited_dim

has_unlimited_dim = state%domain(dom_id)%has_unlimited

end function has_unlimited_dim

!-------------------------------------------------------------------------------
!> Set clamping bounds for domain variables.
!>   missing_r8 values are used to set no-clamping
!>   clamp_vals(ivar,1) must be the minimum value
!>   clamp_vals(ivar,2) must be the maximum value
subroutine set_clamping(dom_id, num_vars, clamp_vals)

integer, intent(in) :: dom_id
integer, intent(in) :: num_vars
real(r8), intent(in) :: clamp_vals(num_vars, 2)

real(r8) :: min_value, max_value
integer  :: ivar

do ivar = 1, num_vars

   min_value = clamp_vals(ivar, 1)
   if (min_value /= missing_r8) then
      state%domain(dom_id)%variable(ivar)%io_info%clamping = .true.
      state%domain(dom_id)%variable(ivar)%io_info%minvalue = min_value
   end if

   max_value = clamp_vals(ivar, 2)
   if (max_value /= missing_r8) then
      state%domain(dom_id)%variable(ivar)%io_info%clamping = .true.
      state%domain(dom_id)%variable(ivar)%io_info%maxvalue = max_value
   end if

end do

end subroutine set_clamping

!-------------------------------------------------------------------------------
!> Check to see if the template file has any of the common cf-conventions
!>
!>  * units
!>  * short_name
!>  * long_name
!>  * short_name
!>  * _FillValue
!>  * missing_value
!>  * add_offset
!>  * scale_factor
!>
!> If they exist, load them up into the state structure.
!-------------------------------------------------------------------------------
! HK @todo why is this by domain?
subroutine load_common_cf_conventions(domain)

type(domain_type), intent(inout) :: domain

integer :: ivar
integer :: nvars

! netcdf variables
integer  :: ret, ncid, VarID
integer  :: var_xtype
integer  :: cf_spvalINT
real(r4) :: cf_spvalR4
real(digits12) :: cf_spvalR8
real(digits12) :: cf_scale_factor, cf_add_offset
character(len=512) :: ncFilename
character(len=NF90_MAX_NAME) :: var_name
character(len=NF90_MAX_NAME) :: cf_long_name, cf_short_name, cf_units

character(len=*), parameter :: routine = 'load_common_cf_conventions'

ncFilename = domain%info_file

ret = nf90_open(ncFilename, NF90_NOWRITE, ncid)
call nc_check(ret, routine, 'nf90_open '//trim(ncFilename))

! determine attributes of each variable in turn

nvars = domain%num_variables

do ivar = 1, nvars
   var_name = domain%variable(ivar)%varname

   ret = nf90_inq_varid(ncid, trim(var_name), VarID)
   call nc_check(ret, routine, 'inq_varid '//trim(var_name))

   ! If the short_name, long_name and/or units attributes are set, get them.
   ! They are not REQUIRED by DART but are nice to keep around if they are present.

   if (nf90_inquire_attribute(ncid, VarID, 'long_name') == NF90_NOERR) then
      ret = nf90_get_att(ncid, VarID, 'long_name', cf_long_name)
      call nc_check(ret, routine, 'get_att long_name '//trim(var_name))
      domain%variable(ivar)%io_info%long_name = cf_long_name
   end if

   if (nf90_inquire_attribute(ncid, VarID, 'short_name') == NF90_NOERR) then
      ret = nf90_get_att(ncid, VarID, 'short_name', cf_short_name)
      call nc_check(ret, routine, 'get_att short_name '//trim(var_name))
      domain%variable(ivar)%io_info%short_name = cf_short_name
   end if

   if (nf90_inquire_attribute(ncid, VarID, 'units') == NF90_NOERR) then
      ret = nf90_get_att(ncid, VarID, 'units', cf_units)
      call nc_check(ret, routine, 'get_att units '//trim(var_name))
      domain%variable(ivar)%io_info%units = cf_units
   end if

   ! Saving any FillValue, missing_value attributes ...
   ! Also stuff them into the 'r8' slots to facilitate simpler clamp_variable
   ! implementation. (Since we coerce the DART state to 'r8')

   var_xtype = domain%variable(ivar)%io_info%xtype
   select case (var_xtype)
   case (NF90_INT)
      ! Sometimes the attributes are specified as GLOBAL attributes
      ret = nf90_get_att(ncid, NF90_GLOBAL, '_FillValue', cf_spvalINT)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalINT = cf_spvalINT
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalINT, r8)
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, NF90_GLOBAL, 'missing_value', cf_spvalINT)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingINT = cf_spvalINT
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalINT, r8)
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if
      ! Usually the attributes are specified as variable attributes
      ret = nf90_get_att(ncid, VarID, '_FillValue', cf_spvalINT)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalINT = cf_spvalINT
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalINT, r8)
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, VarID, 'missing_value', cf_spvalINT)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingINT = cf_spvalINT
         domain%variable(ivar)%io_info%missingR8 = real(cf_spvalINT, r8)
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if

   case (NF90_FLOAT)
      ret = nf90_get_att(ncid, NF90_GLOBAL, '_FillValue', cf_spvalR4)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalR4 = cf_spvalR4
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalR4, r8)
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, NF90_GLOBAL, 'missing_value', cf_spvalR4)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingR4 = cf_spvalR4
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalR4, r8)
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if
      ret = nf90_get_att(ncid, VarID, '_FillValue', cf_spvalR4)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalR4 = cf_spvalR4
         domain%variable(ivar)%io_info%spvalR8 = real(cf_spvalR4, r8)
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, VarID, 'missing_value', cf_spvalR4)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingR4 = cf_spvalR4
         domain%variable(ivar)%io_info%missingR8 = real(cf_spvalR4, r8)
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if

   case (NF90_DOUBLE)

      ! If r8 = r4,
      ! the missing_value must be present in both missingR4 and missingR8
      ! ditto for _FillValue.
      ! This satisfies the overloaded operator 'get_missing_value, get_FillValue'

      ret = nf90_get_att(ncid, NF90_GLOBAL, '_FillValue', cf_spvalR8)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalR4 = cf_spvalR8
         domain%variable(ivar)%io_info%spvalR8 = cf_spvalR8
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, NF90_GLOBAL, 'missing_value', cf_spvalR8)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingR4 = cf_spvalR8
         domain%variable(ivar)%io_info%missingR8 = cf_spvalR8
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if
      ret = nf90_get_att(ncid, VarID, '_FillValue', cf_spvalR8)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%spvalR4 = cf_spvalR8
         domain%variable(ivar)%io_info%spvalR8 = cf_spvalR8
         domain%variable(ivar)%io_info%has_FillValue = .true.
      end if
      ret = nf90_get_att(ncid, VarID, 'missing_value', cf_spvalR8)
      if (ret == NF90_NOERR) then
         domain%variable(ivar)%io_info%missingR4 = cf_spvalR8
         domain%variable(ivar)%io_info%missingR8 = cf_spvalR8
         domain%variable(ivar)%io_info%has_missing_value = .true.
      end if

   case DEFAULT
      write (string1, *) ' unsupported netcdf variable type : ', var_xtype
      call error_handler(E_ERR, routine, string1, source)
   end select

   ! If the variable has one or the other, no problem.
   ! If the variable has both _FillValue and missing_value attributes, the
   ! values must be the same or we are lost. DART only supports one missing
   ! value code. When we go to write, we have no way of knowing which value
   ! to use as a replacement for the DART missing code.

   if (domain%variable(ivar)%io_info%has_missing_value .and. &
         domain%variable(ivar)%io_info%has_FillValue) then

      if (domain%variable(ivar)%io_info%missingR8 /= &
            domain%variable(ivar)%io_info%spvalR8) then

         write (string1, *) trim(var_name)//' missing_value /= _FillValue '
         write (string2, *) 'missing_value is ', domain%variable(ivar)%io_info%missingR8
         write (string3, *) '_FillValue    is ', domain%variable(ivar)%io_info%spvalR8
         call error_handler(E_ERR, 'set_dart_missing_value:', string1, &
                              source, text2=string2, text3=string3)
      end if

   end if

   !>@todo FIXME : Not supporting scale factor or offset at the moment, so just error.
   !>              To fully support netCDF I/O we need to
   !>              pack and unpack the variable if these attributes exist.
   if (nf90_get_att(ncid, VarID, 'scale_factor', cf_scale_factor) == NF90_NOERR) then
      domain%variable(ivar)%io_info%scale_factor = cf_scale_factor
      write (string1, *) 'scale_factor not supported at the moment'
      write (string2, *) 'contact DART if you would like to get this to work'
      call error_handler(E_ERR, routine, string1, source, text2=string2)
   end if

   if (nf90_get_att(ncid, VarID, 'add_offset', cf_add_offset) == NF90_NOERR) then
      domain%variable(ivar)%io_info%add_offset = cf_add_offset
      write (string1, *) 'add_offset not supported at the moment'
      write (string2, *) 'contact DART if you would like to get this to work'
      call error_handler(E_ERR, routine, string1, source, text2=string2)
   end if

end do

! close netcdf file
ret = nf90_close(ncid)
call nc_check(ret, routine, 'nf90_close', trim(ncFilename))

end subroutine load_common_cf_conventions

end module netcdf_variables_mod
