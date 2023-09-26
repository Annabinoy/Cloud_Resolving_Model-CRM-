module mod_initial

    use iso_fortran_env, only: int32,real32
    implicit none

contains
    !! Subroutine to initialize the input array to a Gaussian shape
    pure subroutine set_gaussian(x,icenter,decay)
        real(real32),intent(in out)::x(:) !1D array as input(and output)argument
        !!input parameters for pertubation position and shape
        integer(int32) ,intent(in)::icenter
        real(real32),intent(in)::decay
        integer(int32)::i
        do concurrent( i=1:size(x) )        !loops over all elements,from index1 to grid_size
            x(i)=exp(-decay*(i-icenter)**2)
        end do 
    end subroutine set_gaussian

     !! Subroutine to initialize the input array to a Gaussian shape
    pure subroutine set_gaussian_2d(x,icenter,jcenter,decay)
        real(real32),intent(in out)::x(:,:) !1D array as input(and output)argument
        !!input parameters for pertubation psition and shape
        integer(int32) ,intent(in)::icenter,jcenter
        real(real32),intent(in)::decay
        integer(int32)::i,j
        integer(int32)::d(2)
        d=shape(x)
        do concurrent( i=1:d(1) )        !loops over all elements,from index  1 to grid_size in x-direction
            do concurrent( j=1:d(2) )     !loops over all elements,from index 1 to grid_size in y-direction
                x(i,j)=exp(-decay*(((i-icenter)**2)+((j-jcenter)**2)))
            end do
        end do 
    end subroutine set_gaussian_2d

     !! Subroutine to initialize the input array to a Gaussian shape
    pure subroutine set_gaussian_3d(x,icenter,jcenter,kcenter,decay)
        real(real32),intent(in out)::x(:,:,:) !3D array as input(and output)argument
        !!input parameters for pertubation psition and shape
        integer(int32) ,intent(in)::icenter,jcenter,kcenter
        real(real32),intent(in)::decay
        integer(int32)::i,j,k
        integer(int32)::d(3)
        d=shape(x)
        do concurrent( i=1:d(1) )        !loops over all elements,from index  1 to grid_size in x-direction
            do concurrent( j=1:d(2) )     !loops over all elements,from index  1 to grid_size in y-direction
                do concurrent( k=1:d(3) )     !loops over all elements,from index  1 to grid_size in z-direction
                    x(i,j,k)=exp(-decay*(((i-icenter)**2)+((j-jcenter)**2)+((k-kcenter)**2)))
                end do
            end do
        end do 
    end subroutine set_gaussian_3d

    pure function initial3(x,y,z) result(h)
        integer(int32), intent(in) :: x,y,z
        integer(int32)::i,j,k
        real(real32) :: h(x,y,z) 
        do i=1,x
            do j=1,y
                do k=1,z
                    h(i,j,k)=0
                end do
            end do
        end do  
    end function initial3

    pure function initial4(x,y,z,t) result(h)
        integer, intent(in) :: x,y,z,t
        integer(int32)::i,j,k,l
        integer :: h(x,y,z,t)
        do i=1,x
            do j=1,y
                do k=1,z
                    do l=1,t
                        h(i,j,k,l)=0
                end do
            end do
        end do
    end function initial4

end module mod_initial
