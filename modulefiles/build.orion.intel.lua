help([[
Load environment to compile UFS_UTILS on Orion
]])

cmake_ver=os.getenv("cmake_ver") or "3.22.1"
load(pathJoin("cmake", cmake_ver))

prepend_path("MODULEPATH", "/work/noaa/epic-ps/role-epic-ps/miniconda3/modulefiles")

miniconda3_ver=os.getenv("miniconda3_ver") or "4.12.0"
load(pathJoin("miniconda3", miniconda3_ver))

prepend_path("MODULEPATH", "/work/noaa/epic-ps/role-epic-ps/hpc-stack/libs/intel-2022.1.2/modulefiles/stack")

hpc_ver=os.getenv("hpc_ver") or "1.2.0"
load(pathJoin("hpc", hpc_ver))

hpc_intel_ver=os.getenv("hpc_intel_ver") or "2022.1.2"
load(pathJoin("hpc-intel", hpc_intel_ver))

hpc_impi_ver=os.getenv("hpc_impi_ver") or "2022.1.2"
load(pathJoin("hpc-impi", hpc_impi_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

ip_ver=os.getenv("ip_ver") or "3.3.3"
load(pathJoin("ip", ip_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
load(pathJoin("nemsio", nemsio_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3nco_ver=os.getenv("w3nco_ver") or "2.4.1"
load(pathJoin("w3nco", w3nco_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.12"
load(pathJoin("zlib", zlib_ver))

png_ver=os.getenv("png_ver") or "1.6.37"
load(pathJoin("libpng", png_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.14.0"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.9.1"
load(pathJoin("netcdf", netcdf_ver))

nccmp_ver=os.getenv("nccmp_ver") or "1.8.9.0"
load(pathJoin("nccmp", nccmp_ver))

esmf_ver=os.getenv("esmf_ver") or "8.3.0b09"
load(pathJoin("esmf", esmf_ver))

nco_ver=os.getenv("nco_ver") or "5.0.6"
load(pathJoin("nco", nco_ver))

whatis("Description: UFS_UTILS build environment")
