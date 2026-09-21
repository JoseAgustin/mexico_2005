!
!	celda_pob2.f90
!	
!
!	Created by Agustin on 23/11/12.
!	Copyright 2012 CCA-UNAM. All rights reserved.
!
!  Reads celda poblacion and group in a cel and sums
!
module vars
integer :: nl  ! line numbers in Pob_TOTAL_x_celda
integer,allocatable:: grid(:),grid2(:)
integer,allocatable::pr(:),pt(:),prT(:),puT(:)
integer,allocatable::pr2(:),pt2(:)
real,allocatable ::pu(:),pu2(:)
character(len=5),allocatable::cemun(:),cemun2(:),cemun3(:)
common /var/ nl
end module
!
program celda
use vars

	call lee
	
	call calcula
	
	call guarda
contains
subroutine lee
	implicit none
	integer i,j,idum
	character(len=10)::cdum
    character (len=25):: fname
    fname='Pob_TOTAL_x_celda.csv'
	open (unit=10,file=fname,status='old',action='read')
    read(10,'(A)') cdum
	i=0
	do
	read(10,*,END=100) cdum
	i=i+1
	end do
100 continue
	rewind(10)
	nl=i
	print *,'file ',fname,' has',nl,'lines'
	allocate(grid(nl),pr(nl),pu(nl),pt(nl),cemun(nl))
	read(10,*)cdum
	do i=1,nl
	read(10,*)grid(i),cemun(i),pr(i),pu(i),pt(i)
    !print *,i,grid(i)
	end do
end subroutine lee
subroutine calcula
	implicit none
	integer ::i,j
	call count
    do j=1,nl
        do i=1,size(grid2)
			if(grid2(i).eq.grid(j))then
				pr2(i)=pr2(i)+pr(j)
				pu2(i)=pu2(i)+pu(j)
				pt2(i)=pt(j)
			end if
		end do
        do i=1,size(cemun3)
            if(cemun3(i).eq.cemun(j))then
                puT(i)=puT(i) + pu(j)
                prT(i)=prT(i) + pr(j)
            end if
        end do
	end do
    
end subroutine calcula
!
subroutine count
!  Identifies the different elements in the array
  integer i,j,k
logical,allocatable::xl(:),xlm(:)
  allocate(xl(size(grid)),xlm(size(grid)))
  xl=.true.
  xlm=.true.
  do i=1,nl-1
   do j=i+1,nl
   if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
   if(xlm(j).and.cemun(j).eq.cemun(i)) xlm(j)=.false.
   end do
  end do
  
  j=0
  do i=1,nl
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j),pr2(j),pu2(j),pt2(j),cemun2(j))
  k=0
    do i=1,nl
    if(xlm(i)) k=k+1
    end do
    allocate(prT(k),puT(k),cemun3(k))
    prT=0
    puT=0
  j=0
  do i=1,nl
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
	cemun2(j)=cemun(i)
	end if
  end do
    k=0
    do i=1,nl
      if(xlm(i)) then
        k=k+1
        cemun3(k)=cemun(i)
      end if
    end do
  print *,'Number of different cells',j
  print *,'Number of different muni',size(cemun3)
  deallocate(xl)
end subroutine count
subroutine guarda
	implicit none
	integer:: i,j
	real :: fu,fr
    real :: fu2,fr2
	open(unit=20,file='gri_pob2.txt')
	write(20,'(A)')'GRIDCODE,ID,furb,frural,fpob, PobUrb, PobR, puT,prT, PobT'
	write(20,'(A)')'0			fraction'
	do i=1,size(grid2)
        if (pt2(i).eq.0) then
        fu=0
        fr=0
        else
		fu= real(pu2(i))/real(pt2(i))
        fr= real(pr2(i))/real(pt2(i))
        end if
        MUNI: do j=1,size(cemun3)
          if(cemun2(i).eq.cemun3(j)) then
            if(puT(j).eq.0) then
                fu2=0
            else
                fu2=real(pu2(i))/real(puT(j))
            end if
            if(prT(j).ne.0) then
                fr2= real(pr2(i))/real(prT(j))
            else
                fr2=0
            end if
            if(fu2.eq.1)fu2=fu
            if(fr2.eq.1)fr2=fr
            exit MUNI
          end if
        end do MUNI
		write(20,300) grid2(i),cemun2(i),fu2,fr2,fu+fr,int(pu2(i)),pr2(i),puT(j),prT(j),pt2(i)
	end do
300 format(i6,",",A5,2(",",E0.7),",",(E0.7,","),2(I6,","),2(I7,","),I7)
end subroutine guarda
end program celda
