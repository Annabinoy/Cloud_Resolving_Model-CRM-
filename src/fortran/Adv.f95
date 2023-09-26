program Advection

    use iso_fortran_env, only : int32,real32
    use mod_diff,only : centered_diff
    use mod_initial,only : set_gaussian_3d,initial3,initial4
    use mod_grid
    use mod_misc, only : file_retrieve,file_write
    implicit none
    
    !Loop variables
    integer(int32)::n,i,j,k
    !dimensions of matrices
    integer(int32)::Nx=0,Ny=0,Nz=0,Nt=0

    !c is (background flow)speed[m/s]
    real(real32),allocatable::c(:),g(:)

    !object of class grid(don't know if the terms apply fortran)   
    type(Grid)::fg
    !Declares h as a real array with the number of elements equal to grid size
    real(real32),allocatable::h(:,:,:),diffx(:,:,:),diffy(:,:,:),diffz(:,:,:)
    real(real32),allocatable::adres(:,:,:,:)
    
    !!!SETTING THE INITIAL WATER HEIGHT VALUES
    !central index and decay factor of the shape
    integer(int32),parameter::icenter=2!TODO: Should be moved to parameters
    integer(int32),parameter::jcenter=2
    integer(int32),parameter::kcenter=2
    real(real32),parameter::decay=0.7!TODO: Should be moved to parameters
    
    !if(Nx<=0 .or. Ny<=0 .or. Nz<=0) stop 'grid_size must be > 0'
    !if(dt<=0) stop 'time step dt must be > 0'
    !if(dx<=0 .or. dy<=0 .or. dz<=0) stop 'grid spacing dx must be > 0'
    
    
    
    
    !Retrieving background flow as a parameter from the parameter file
    c=file_retrieve('advparam.txt')
    fg=Grid(c(4),c(5),c(6),c(7),c(8),c(9),c(10),c(11),c(12),c(13),c(14),c(15))
    call fg%calculate(Nx,Ny,Nz,Nt)
    h=initial3(Nx,Ny,Nz)
    diffx=initial3(Nx,Ny,Nz)
    diffy=initial3(Nx,Ny,Nz)
    diffz=initial3(Nx,Ny,Nz)
    adres=initial4(Nt,Nx,Ny,Nz)

    call set_gaussian_3d(h,icenter,jcenter,kcenter,decay)  !Calls the subroutine to initialize the water height
    !Adding initial function as data at t=1 of final matrix
    
    do i=1,Nx
        do j=1,Ny
            do k=1,Nz
                adres(1,i,j,k)=h(i,j,k)
            end do
        end do
    end do
    !!!PREDICTING THE MOVEMENT OF THE OBJECT
    Time_loop: do n=2,Nt  !iterates over num_time_steps time steps
    !We store the    
        do i=1,Nx
            do j=1,Ny
                do k=1,Nz
                    h(i,j,k)=adres(n-1,i,j,k)
                end do
            end do
        end do

        call centered_diff(h,diffx,diffy,diffz,"open")
        do i=1,Nx
            do j=1,Ny
                do k=1,Nz
                    adres(n,i,j,k)=h(i,j,k)-((c(1)*diffx(i,j,k)/fg%dx)+(c(2)*diffy(i,j,k)/fg%dy)+(c(3)*diffz(i,j,k)/fg%dz))*fg%dt
                end do
            end do
        end do
    end do Time_loop
    !prints values n and h to the terminal using default formatting
    call file_write('test.csv',adres)
    

end program Advection