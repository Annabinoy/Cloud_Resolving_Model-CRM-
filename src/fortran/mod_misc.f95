module mod_misc

    use iso_fortran_env ,only:real32,int32
    implicit none

contains
    function file_retrieve(file_name) result(n)

        character(len=*),intent(in)::file_name
        character(len=256)::x,k
        character(len=256),allocatable::content(:)
        real(real32),allocatable::n(:)
        integer::i,s,f
        integer::ierr=0,num_lines=0,fid=1

        !Counting number of lines

        open(fid,file=file_name)
        do while (ierr==0)
            read(fid,*,iostat=ierr)x
            num_lines=num_lines+1
        end do
        num_lines=num_lines-1

        !Allocating the array to match content size

        allocate(content(num_lines))
        allocate(n(num_lines))

        !Reading content in the file and saving the values in an array n
        rewind(1)

        do i = 1, num_lines
            read(fid,'(A)')content(i)
            s=index(content(i),'=')
            f=index(content(i),',')
            k=content(i)
            read(k(s+1:f-1),*)n(i)
        end do

        close(1)

    end function

    subroutine file_write(file_name, matrix)
        real(real32), intent(in) :: matrix(:,:,:,:)
        character(len=*), intent(in)::file_name
        integer(int32)::n1,n2,n3,n4,i,j,k
        n1 = SIZE(matrix, 1)
        n2 = SIZE(matrix, 2)
        n3 = SIZE(matrix, 3)
        n4 = SIZE(matrix, 4)

        ! Formatting for CSV
        101 format(1x, *(g0, ", ")) 
        
        ! Open connection (i.e. create file where to write)
        OPEN(unit = 10, access = "sequential", action = "write", &
             status = "replace", file = file_name, form = "formatted") 
        !Writing the dimensions of the 4d matrix. Here n1 stands for time
        WRITE(10,101) n1,n2,n3,n4
        ! Loop across rows
        do i=1,n2
            do j=1,n3
                do k=1,n4
                    WRITE(10, 101) matrix(:,i,j,k)
                end do
            end do
        end do 
        ! Close connection
        CLOSE(10)
    
        
    end subroutine file_write

end module mod_misc