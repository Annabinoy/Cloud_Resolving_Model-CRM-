module mod_diff

    use iso_fortran_env, only:int32,real32
    implicit none

contains
    !Fuction returns finite difference by forward scheme
    pure subroutine forward_diff(x,dx,dy,dz,bound_condition)
        real(real32), intent(in out) :: x(:,:,:),dx(:,:,:),dy(:,:,:),dz(:,:,:)
        character(len=*), intent(in) :: bound_condition 
        integer(int32) :: len(3)      !Stores the length of the input array
        
        len=shape(x)

        !For the case of open boundary
        if ( bound_condition .eq. "open" ) then
            dx(len(1),:,:)=0
            dy(:,len(2),:)=0
            dz(:,:,len(3))=0
        
        
        !For the case of closed boundary
        else if (bound_condition .eq. "closed") then
            dx(len(1),:,:)=-x(len(1),:,:)
            dy(:,len(2),:)=-x(:,len(2),:)
            dz(:,:,len(3))=-x(:,:,len(3))

            
        !For the case of periodic boundary
        else if (bound_condition .eq. "periodic") then
            dx(len(1),:,:)=x(1,:,:)-x(len(1),:,:)
            dy(:,len(2),:)=x(:,1,:)-x(:,len(2),:)
            dz(:,:,len(3))=x(:,:,1)-x(:,:,len(3))
        end if
        
        !For values not on the boundary
        dx(1:len(1)-1,:,:)=x(2:len(1),:,:)-x(1:len(1)-1,:,:)
        dy(:,1:len(1)-1,:)=x(:,2:len(1),:)-x(:,1:len(1)-1,:)
        dz(:,:,1:len(1)-1)=x(:,:,2:len(1))-x(:,:,1:len(1)-1)
    
    end subroutine forward_diff
    
    pure subroutine centered_diff(x,dx,dy,dz,bound_condition)
        real(real32), intent(in out) :: x(:,:,:),dx(:,:,:),dy(:,:,:),dz(:,:,:)
        character(len=*), intent(in) :: bound_condition 
        integer(int32) :: len(3)      !Stores the length of the input array
        
        len=shape(x)

        !For the case of open boundary
        if ( bound_condition .eq. "open" ) then
            dx(len(1),:,:)=0
            dy(:,len(2),:)=0
            dz(:,:,len(3))=0
            dx(1,:,:)=0
            dy(:,1,:)=0
            dz(:,:,1)=0
        
        !For the case of closed boundary
        else if (bound_condition .eq. "closed") then
            dx(len(1),:,:)=-2*x(len(1)-1,:,:)
            dy(:,len(2),:)=-2*x(:,len(2)-1,:)
            dz(:,:,len(3))=-2*x(:,:,len(3)-1)
            dx(1,:,:)=2*x(2,:,:)
            dy(:,1,:)=2*x(:,2,:)
            dz(:,:,1)=2*x(:,:,2)

            
        !For the case of periodic boundary
        else if (bound_condition .eq. "periodic") then
            dx(len(1),:,:)=(x(1,:,:)-x(len(1)-1,:,:))*0.5
            dy(:,len(2),:)=(x(:,1,:)-x(:,len(2)-1,:))*0.5
            dz(:,:,len(3))=(x(:,:,1)-x(:,:,len(3)-1))*0.5
            dx(1,:,:)=(x(2,:,:)-x(len(1),:,:))*0.5
            dy(:,1,:)=(x(:,2,:)-x(:,len(2),:))*0.5
            dz(:,:,1)=(x(:,:,2)-x(:,:,len(3)))*0.5
            
        end if
        
        !For values not on the boundary
        dx(2:len(1)-1,:,:)=(x(3:len(1),:,:)-x(1:len(1)-2,:,:))*0.5
        dy(:,2:len(1)-1,:)=(x(:,3:len(1),:)-x(:,1:len(1)-2,:))*0.5
        dz(:,:,2:len(1)-1)=(x(:,:,3:len(1))-x(:,:,1:len(1)-2))*0.5
    
    end subroutine centered_diff
end module mod_diff
