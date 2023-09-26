module mod_grid

    use iso_fortran_env
    implicit none

    !Some Constants
    real(kind=real32),parameter::x_can=23.27 !Latitude of Tropic of Cancer
    real(kind=real32),parameter::x_cap=-23.27 !Latitude of Tropic of Capricorn
    real(kind=real32),parameter::R=6378160  !Radius(equatorial) of earth assuming it is a sphere
    real(kind=real32),parameter::Pi=16*atan(1./5.)-4*atan(1./239.)!atan requires float values as argument so added . after each number to get float fraction

    type, public :: Grid
        !Specifies space domain
        real(kind=real32) :: x0
        real(kind=real32) :: x1
        real(kind=real32) :: y0
        real(kind=real32) :: y1
        real(kind=real32) :: z0
        real(kind=real32) :: z1
        !Specifies time domain
        real(kind=real32) :: t0
        real(kind=real32) :: t1
        !For space discreatization
        real(kind=real32) :: dx
        real(kind=real32) :: dy
        real(kind=real32) :: dz
        !For time discreatization
        real(kind=real32) :: dt
        !Parameters for the GRID
        !integer(kind=int32)::Nx
        !integer(kind=int32)::Ny
        !integer(kind=int32)::Nz
        contains
            procedure :: calculate => cal
    end type Grid
contains
    !Calculates all the parameters required for the GRID
    subroutine cal(this, Nx, Ny, Nz, Nt)
        class(Grid), intent(in) :: this
        !No. of points in each spatial dimension
        integer(kind=int32), intent(in out) :: Nx
        integer(kind=int32), intent(in out) :: Ny
        integer(kind=int32), intent(in out) :: Nz
        !No. of points in time
        integer(kind=int32), intent(in out) :: Nt
        Nt = int((this%t1-this%t0)/this%dt)
        Nx = int((this%x0-this%x1)/this%dx)
        Ny = int((this%y0-this%y1)/this%dy)
        Nz = int((this%z0-this%z1)/this%dz)
        
    end subroutine cal
    
    function lat_to_x(lat) result(x)
        
        real(kind=real32),intent(in)::lat
        real(kind=real32)::x
    


        !x=((lat-int(lat))*100/60+(int(lat))*R*Pi)/180
        x=(lat*R*Pi)/180  

    end function lat_to_x

    function x_to_lat(x) result(lat)

        real(kind=real32),intent(in)::x
        real(kind=real32)::lat
    
        !lat=int(x/R)+((x/R)-int(x/R))/100! Doubt2
        !Doubt2: The above gives lat in radians while lat_to_x function you have assumed lat to have degrees shouldn't it be like what is given below for consistency.
        !Note: This is if you implement the format recommended in Doubt1 
        lat=(x*180)/(R*Pi)
    end function x_to_lat
    
    function long_to_x(long,lat) result(x)
        real(kind=real32),intent(in)::long,lat
        real(kind=real32)::x
    
        !x=((long-int(long))*100/60+(int(long))*R*Pi)/180! Doubt 1
        x=(long*R*cos(lat*Pi/180)*Pi)/180
    end function long_to_x


    function x_to_long(x,lat) result(long)
        real(kind=real32),intent(in)::x,lat
        real(kind=real32)::long
    
        !long=int(x/R)+((x/R)-int(x/R))/100! Doubt 2
        long=(x*180)/(R*cos(lat*Pi/180)*Pi)
    end function x_to_long
end module mod_grid