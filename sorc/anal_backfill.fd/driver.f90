 program test

 use netcdf

 implicit none

 integer, parameter :: num_recs = 9
 character(len=128) :: outfile, infile
 character(len=11)  :: records(num_recs) 

 integer :: i, j, mi, iret, mo, km, rec
 integer :: lon_in, lat_in
 integer :: lon_out, lat_out
 integer :: lev, ilev, lev_in
 integer :: ncid_in, id_var
 integer :: ncid_out, error
 integer :: dim_lon_out, dim_lat_out
 integer :: dim_lev_out, dim_ilev_out
 integer :: id_u_inc_out, id_v_inc_out
 integer :: id_lon_out, id_lat_out, id_lev_out
 integer :: id_pfull_out, id_ilev_out
 integer :: id_hyai_out, id_hybi_out
 integer :: id_delp_inc_out, id_delz_inc_out
 integer :: id_t_inc_out, id_sphum_inc_out
 integer :: id_liq_wat_inc_out, id_o3mr_inc_out
 integer :: id_icmr_inc_out, id_dim
 integer :: fsize=65536, initial = 0
 integer :: header_buffer_val = 16384
 integer :: kgds_in(200), kgds_out(200)
 integer :: ip, ipopt(20), no
 integer, allocatable :: ibi(:), ibo(:), levs(:)

 logical*1, allocatable :: li(:,:), lo(:,:)

 real, allocatable :: dummy_in(:,:,:), latitude_in(:)
 real, allocatable :: longitude_in(:)
 real, allocatable :: latitude_out(:), longitude_out(:)
 real, allocatable :: dummy_out(:,:,:)
 real, allocatable :: slat(:), wlat(:)
 real, allocatable :: rlat(:), rlon(:)
 real, allocatable :: gi(:,:), go(:,:)

 data records /'u_inc', 'v_inc', 'delp_inc', 'delz_inc', 'T_inc', &
               'sphum_inc', 'liq_wat_inc', 'o3mr_inc', 'icmr_inc' /

!-----------------------------------------------------------------
! open and create output file.
!-----------------------------------------------------------------

 lon_out=1536
 lat_out=768
 lev=127
 ilev=128

 outfile="./out.nc"

 error=nf90_create(outfile, IOR(NF90_NETCDF4,NF90_CLASSIC_MODEL), &
                   ncid_out, initialsize=initial, chunksize=fsize)
 call netcdf_err(error, 'CREATING FILE='//trim(outfile) )

 error = nf90_def_dim(ncid_out, 'lon', lon_out, dim_lon_out)
 call netcdf_err(error, 'define dim lon_out for file='//trim(outfile) )

 error = nf90_def_dim(ncid_out, 'lat', lat_out, dim_lat_out)
 call netcdf_err(error, 'define dim lat_out for file='//trim(outfile) )

 error = nf90_def_dim(ncid_out, 'lev', lev, dim_lev_out)
 call netcdf_err(error, 'define dim lev_out for file='//trim(outfile) )

 error = nf90_def_dim(ncid_out, 'ilev', ilev, dim_ilev_out)
 call netcdf_err(error, 'define dim lev_out for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'lon', nf90_float, (/dim_lon_out/), id_lon_out)
 call netcdf_err(error, 'define var lon for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'lat', nf90_float, (/dim_lat_out/), id_lat_out)
 call netcdf_err(error, 'define var lat for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'lev', nf90_float, (/dim_lev_out/), id_lev_out)
 call netcdf_err(error, 'define var lev for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'pfull', nf90_float, (/dim_lev_out/), id_pfull_out)
 call netcdf_err(error, 'define var pfull for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'ilev', nf90_float, (/dim_ilev_out/), id_ilev_out)
 call netcdf_err(error, 'define var ilev for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'hyai', nf90_float, (/dim_ilev_out/), id_hyai_out)
 call netcdf_err(error, 'define var hyai for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'hybi', nf90_float, (/dim_ilev_out/), id_hybi_out)
 call netcdf_err(error, 'define var hybi for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'u_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_u_inc_out)
 call netcdf_err(error, 'define var u_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'v_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_v_inc_out)
 call netcdf_err(error, 'define var v_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'delp_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_delp_inc_out)
 call netcdf_err(error, 'define var delp_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'delz_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_delz_inc_out)
 call netcdf_err(error, 'define var delz_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'T_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_t_inc_out)
 call netcdf_err(error, 'define var t_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'sphum_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_sphum_inc_out)
 call netcdf_err(error, 'define var sphum_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'liq_wat_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_liq_wat_inc_out)
 call netcdf_err(error, 'define var liq_wat_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'o3mr_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_o3mr_inc_out)
 call netcdf_err(error, 'define var liq_wat_inc for file='//trim(outfile) )

 error = nf90_def_var(ncid_out, 'icmr_inc', nf90_float, (/dim_lon_out,dim_lat_out,dim_lev_out/), id_icmr_inc_out)
 call netcdf_err(error, 'define var icmr_inc for file='//trim(outfile) )

 error = nf90_enddef(ncid_out, header_buffer_val, 4,0,4)
 call netcdf_err(error, 'end meta define for file='//trim(outfile) )

 allocate(latitude_out(lat_out))
 allocate(slat(lat_out))
 allocate(wlat(lat_out))

 call splat(4, lat_out, slat, wlat)
 do j = 1, lat_out
   latitude_out(j) = -( 90.0 - (acos(slat(j))* 180.0 / (4.*atan(1.))) )
 enddo
 deallocate(slat, wlat)

 print*,'lat out ',latitude_out(1), latitude_out(lat_out)

 allocate(longitude_out(lon_out))

 do i = 1, lon_out
   longitude_out(i) = float(i-1) * 360.0 / float(lon_out)
 enddo

 print*,'lon out ',longitude_out(1), longitude_out(lon_out)

 kgds_out = 0
 kgds_out(1) = 4
 kgds_out(2) = lon_out
 kgds_out(3) = lat_out
 kgds_out(4) = nint(latitude_out(1)*1000.0)
 kgds_out(5) = 0  ! lon of origin
 kgds_out(6) = 128 ! res flag
 kgds_out(7) = nint(latitude_out(lat_out)*1000.)
 kgds_out(8) = nint(longitude_out(lon_out)*1000.)
 kgds_out(9) = nint((360.0 / float(lon_out))*1000.0)
 kgds_out(10) = lat_out / 2
 kgds_out(11) = 64  ! scan mode
 kgds_out(12) = 255
 kgds_out(19) = 0
 kgds_out(20) = 255

 print*,'kgds out ',kgds_out(1:20)

!----------------------------------------------------
! open read input file
!----------------------------------------------------

 infile="/scratch2/NCEPDEV/stmp1/Cory.R.Martin/GFSv16_ncio/gfs.20190612/00/gfs.t00z.atminc.nc"
 error = nf90_open(trim(infile), nf90_nowrite, ncid_in)
 call netcdf_err(error, 'opening file='//trim(infile) )

 error = nf90_inq_dimid(ncid_in, 'lon', id_dim)
 call netcdf_err(error, 'inq lon dim for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lon_in)
 call netcdf_err(error, 'reading lon dim for file='//trim(infile) )

 print*,'lon of input file is ',lon_in

 error = nf90_inq_dimid(ncid_in, 'lat', id_dim)
 call netcdf_err(error, 'inq lat dim for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lat_in)
 call netcdf_err(error, 'reading lat dim for file='//trim(infile) )

 print*,'lat of input file is ',lat_in

 error = nf90_inq_dimid(ncid_in, 'lev', id_dim)
 call netcdf_err(error, 'inq lev dim for file='//trim(infile) )
 error = nf90_inquire_dimension(ncid_in, id_dim, len=lev_in)
 call netcdf_err(error, 'reading lev dim for file='//trim(infile) )

 print*,'lev of input file is ',lev_in

! cant use lat in file, it is not correct.
 allocate(latitude_in(lat_in))
 allocate(slat(lat_in))
 allocate(wlat(lat_in))

 call splat(4, lat_in, slat, wlat)
 do j = 1, lat_in
   latitude_in(j) = -( 90.0 - (acos(slat(j))* 180.0 / (4.*atan(1.))) )
 enddo
 print*,'lat in ',latitude_in(1), latitude_in(lat_in)

 allocate(longitude_in(lon_in))

 do i = 1, lon_in
   longitude_in(i) = float(i-1) * 360.0 / float(lon_in)
 enddo
 print*,'lon in ',longitude_in(1), longitude_in(lon_in)

 kgds_in = 0
 kgds_in(1) = 4
 kgds_in(2) = lon_in
 kgds_in(3) = lat_in
 kgds_in(4) = nint(latitude_in(1)*1000.0)
 kgds_in(5) = 0  ! lon of origin
 kgds_in(6) = 128 ! res flag
 kgds_in(7) = nint(latitude_in(lat_in)*1000.)
 kgds_in(8) = nint(longitude_in(lon_in)*1000.)
 kgds_in(9) = nint((360.0 / float(lon_in))*1000.0)
 kgds_in(10) = lat_in / 2
 kgds_in(11) = 64  ! scan mode
 kgds_in(12) = 255
 kgds_in(19) = 0
 kgds_in(20) = 255

 print*,'kgds in ',kgds_in(1:20)

 if (lev /= lev_in) then
   print*,'error: input output levs dont match'
   stop
 endif

 mi = lon_in * lat_in
 mo = lon_out * lat_out
 km = lev_in
 allocate(dummy_out(lon_out,lat_out,lev))
 allocate(dummy_in(lon_in, lat_in, lev_in))
 allocate(ibi(km))
 allocate(li(mi,km))
 allocate(gi(mi,km))
 allocate(rlat(mo))
 allocate(rlon(mo))
 allocate(ibo(km))
 allocate(lo(mo,km))
 allocate(go(mo,km))

 do rec = 1, num_recs

   print*,'- PROCESS RECORD: ', trim(records(rec))

   error = nf90_inq_varid(ncid_in, trim(records(rec)), id_var)
   call netcdf_err(error, 'inq ' // trim(records(rec)) // ' id for file='//trim(infile) )
   error = nf90_get_var(ncid_in, id_var, dummy_in)
   call netcdf_err(error, 'in get_var for file='//trim(infile) )

   print*,'dummy in ',maxval(dummy_in), minval(dummy_in)

!----------------------------------------------------
! call ipolates
!----------------------------------------------------

   ip = 0 ! bilinear
   ipopt = 0
   ibi = 0
   li = 0
   gi = 0
   rlat = 0.
   rlon = 0.
   ibo = 0
   lo = 0
   go = 0
   gi = reshape (dummy_in, (/mi, km/))

   call ipolates(ip, ipopt, kgds_in, kgds_out, mi, mo, &
              km, ibi, li, gi, no, rlat, rlon, ibo, &
              lo, go, iret)

   if (iret /= 0) then
     print*,'error in ipolates ',iret
     stop
   endif

   if (no /= mo) then
     print*,'ipolates returned wrong number of pts ',no
     stop
   endif

   dummy_out = reshape(go, (/lon_out,lat_out,lev/))

   error = nf90_inq_varid(ncid_out, trim(records(rec)), id_var)
   call netcdf_err(error, 'inq ' // trim(records(rec)) // ' id for file='//trim(outfile) )
   error = nf90_put_var(ncid_out, id_var, dummy_out)
   call netcdf_err(error, 'writing t_inc for file='//trim(outfile) )

 enddo  ! records

 allocate(levs(lev))
 do j = 1, lev
   levs(j) = j
 enddo

 error = nf90_put_var(ncid_out, id_lev_out, levs)
 call netcdf_err(error, 'writing levs for file='//trim(infile) )

 error = nf90_close(ncid_out)
 error = nf90_close(ncid_in)

 print*,'done'

 end program test

 subroutine netcdf_err( err, string )

 use netcdf

 implicit none
 integer, intent(in) :: err
 character(len=*), intent(in) :: string
 character(len=256) :: errmsg

 if( err.EQ.NF90_NOERR )return
 errmsg = NF90_STRERROR(err)
 print*,''
 print*,'FATAL ERROR: ', trim(string), ': ', trim(errmsg)
 print*,'STOP.'
 stop 999

 return
 end subroutine netcdf_err

