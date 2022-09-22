!  Simple self-test based on the driver*.f90 examples. Returns 1 in
!  the incorrect value is reached. Only other change is the removal of
!  long headers and comments.
!
!  Copyright Ciyou Zhu, Richard Byrd, Jorge Nocedal, Jose Luis
!  Morales. 3-clause BSD license.

      program driver


      implicit none
 
 
      integer,  parameter    :: n = 25, m = 5, iprint = -1
      integer,  parameter    :: dp = kind(1.0d0)
      real(dp), parameter    :: factr  = 0.0d0, pgtol  = 0.0d0

      character(len=60)      :: task, csave
      logical                :: lsave(4)
      integer                :: isave(44)
      real(dp)               :: f
      real(dp)               :: dsave(29)
      integer,  allocatable  :: nbd(:), iwa(:)
      real(dp), allocatable  :: x(:), l(:), u(:), g(:), wa(:)
!
      real(dp)               :: t1, t2
      integer                :: i

      allocate ( nbd(n), x(n), l(n), u(n), g(n) )
      allocate ( iwa(3*n) )
      allocate ( wa(2*m*n + 5*n + 11*m*m + 8*m) )

      do 10 i=1, n,2
         nbd(i)=2
         l(i)=1.0d0
         u(i)=1.0d2
  10  continue

!    Next set bounds on the even numbered variables.

      do 12 i=2, n,2
         nbd(i)=2
         l(i)=-1.0d2
         u(i)=1.0d2
  12   continue

!     We now define the starting point.

      do 14 i=1, n
         x(i)=3.0d0
  14  continue
 
!     We now write the heading of the output.

      write (6,16)
  16  format(/,5x, 'Solving sample problem.', &
             /,5x, ' (f = 0.0 at the optimal solution.)',/)               
               

!     We start the iteration by initializing task.
! 
      task = 'START'

!        ------- the beginning of the loop ----------
 
      do while( task(1:2).eq.'FG'.or.task.eq.'NEW_X'.or. &
                task.eq.'START') 
      
!     This is the call to the L-BFGS-B code.

         call setulb(n,m,x,l,u,nbd,f,g,factr,pgtol,wa,iwa,task,iprint, &
                     csave,lsave,isave,dsave)
 
         if (task(1:2) .eq. 'FG') then

!        the minimization routine has returned to request the
!        function f and gradient g values at the current x.

!        Compute function value f for the sample problem.

            f =.25d0*(x(1) - 1.d0)**2
            do 20 i=2,n
               f = f + (x(i) - x(i-1)**2)**2
  20        continue
            f = 4.d0*f

!        Compute gradient g for the sample problem.

            t1 = x(2) - x(1)**2
            g(1) = 2.d0*(x(1) - 1.d0) - 1.6d1*x(1)*t1
            do 22 i= 2,n-1
               t2 = t1
               t1 = x(i+1) - x(i)**2
               g(i) = 8.d0*t2 - 1.6d1*x(i)*t1
  22        continue
            g(n)=8.d0*t1
!
      else
!
         if (task(1:5) .eq. 'NEW_X') then   
!    

             if (isave(34) .ge. 99)  &
                task='STOP: TOTAL NO. of f AND g EVALUATIONS EXCEEDS LIMIT'

             if (dsave(13) .le. 1.d-10*(1.0d0 + abs(f))) &
               task='STOP: THE PROJECTED GRADIENT IS SUFFICIENTLY SMALL'

         
            write (6,'(2(a,i5,4x),a,1p,d12.5,4x,a,1p,d12.5)') 'Iterate' &
               , isave(30),'nfg =',isave(34),'f =',f,'|proj g| =',dsave(13)

!        If the run is to be terminated, we print also the information
!        contained in task as well as the final value of x.

            if (task(1:4) .eq. 'STOP') then
               write (6,*) task  
               write (6,*) 'Final X='
               write (6,'((1x,1p, 6(1x,d11.4)))') (x(i),i = 1,n)
            end if
 
        end if
      end if
      
   end do

   if (abs(f) < 1e-6) then
      stop 0
   else
      stop 1
   end if
!           ---------- the end of the loop -------------
 
!     If task is neither FG nor NEW_X we terminate execution.
 
      end program driver

!======================= The end of driver2 ============================

