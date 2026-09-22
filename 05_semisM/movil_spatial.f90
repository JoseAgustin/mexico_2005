!  movil_spatial.f90
!
!  ifort -O3 movil_spatial.f90
!
!  Creado por Jose Agustin Garcia Reynoso el 25/05/2012
!
! Proposito
!          Distribución espacial de las emisiones de fuentes moviles
!          Program that reads EI2008 and spatial allocation
!
module vars
integer nl   !  Number of lines in M_E2008.csv
integer nl2  !  Number of lines in grid_pob.txt
integer,allocatable :: id(:),id2(:) !State Mun code in emis and grid files
integer,allocatable ::grid(:),grid2(:) ! gridcode in gri_pob
integer,allocatable :: im(:),im2(:)  ! time lag emis and grid files
! scc code in emis and subset of different scc codes.
integer*8,allocatable ::iscc(:),jscc(:)
character(len=4),allocatable::pol(:) ! pollutant name
! ei emission in emissfile (nl dimension)
! uf, rf urban and rural population fraction
! pemi emission in grid cell,pollutan,scc category
real,allocatable:: ei(:),uf(:),rf(:),pemi(:,:,:)
common /vari/ nl,nl2
end module vars

program movil_spatial
use vars

	call lee
  
    call computations 
  
	call imprime
	
contains
subroutine imprime
    integer i,j,k
	character(len=15) ::name
	do i=1,7
	name='M_'//trim(pol(i))//'.csv'
	open(10,file=name)
	write(10,*)'GRIDCODE emissions in Mg per year'
	write(10,210)size(jscc),(jscc(j),j=1,size(jscc))
	do k=1,size(grid2)
	  write(10,220) grid2(k),(1000*pemi(k,i,j),j=1,size(jscc)),im2(k)
	end do
	close(10)
	end do
   print *,"Tamaño ",size(jscc)
210 format(i6,x,35I11,x)
220 format(i6,x,35E12.4,x,I2)
end subroutine imprime
!
subroutine computations
implicit none
	integer i,j,ii,l,k
	print *,' Start doing computations'
!	print *,(pol(i),i=1,7)
	call count  ! counts grids and scc different values
	print *,'end count'
	ii=1
	do i=1,nl2
		do j=1,nl-1
		  if (id2(i).eq.id(j)) then
		    if(pol(1).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,1,l)=pemi(k,1,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol1
		    if(pol(2).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,2,l)=pemi(k,2,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol2
		    if(pol(3).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,3,l)=pemi(k,3,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol3
		    if(pol(4).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,4,l)=pemi(k,4,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol4
		    if(pol(5).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,5,l)=pemi(k,5,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol5
		    if(pol(6).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,6,l)=pemi(k,6,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol6
		    if(pol(7).eq.pol(j)) then
			  do l=1,size(jscc)
			   if(jscc(l).eq.iscc(j))then
			    do k=1,size(grid2)
			    if(grid2(k).eq.grid(i))pemi(k,7,l)=pemi(k,7,l)+&
				&(uf(i)+rf(i))*ei(j)
				end do! k
			   end if!scc
			  end do! l
			end if !pol7
		  end if! id2
		end do!j
	end do !i
end subroutine computations
!
subroutine lee
	implicit none
	integer:: i
	character(len=10):: cdum
	print *,'Starts reading files'
	open(10,file='M_E2005.csv',status='old',action='read')
	read(10,'(A)') cdum !read header
	i=0
	do 
	 read(10,*,END=100)cdum
	 i=1+i
	end do
100 continue
    print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum ! read header
	allocate(id(i),iscc(i),pol(i),ei(i),im(i))
	nl=i
	do i=1,nl
	read(10,*,ERR=140) id(i),iscc(i),pol(i),ei(i),im(i)
	end do
	print *,'End reading file M_E2005.csv'
	close(10)
!
	open(10,file='gri_movil.csv',status='old',action='read')
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	i=0
	do 
	 read(10,*,END=110)cdum
	 i=1+i
	end do
110 continue
    !print *,'number of lines',i
	rewind(10)
	read(10,'(A)') cdum !read header line 1
	read(10,'(A)') cdum !read header line 2
	allocate(grid(i),id2(i),uf(i),rf(i))
	nl2=i
	do i=1,nl2
	read(10,*) grid(i),id2(i),uf(i),rf(i)
	!print *,i,grid(i),id2(i),uf(i),rf(i)
	end do
	print *,'End reading file gri_movil.csv'
	close(10)
!
!	Se considera que el 10 % va en carretera 
!   y el 90% en ciudad
!
	uf=uf*0.90
	rf=rf*0.10
	return
140 print *,"Error in reading file M_E2008",i
end subroutine lee
subroutine count
  integer i,j
  logical,allocatable::xl(:)
  allocate(xl(size(grid)))
  xl=.true.
  do i=1,nl2-1
   do j=i+1,nl2
   if(grid(j).eq.grid(i).and.xl(j)) xl(j)=.false.
   end do
  end do
  j=0
  do i=1,nl2
    if(xl(i)) j=j+1
  end do
  allocate(grid2(j),im2(j))
  j=0
  do i=1,nl2
    if(xl(i)) then
	j=j+1
	grid2(j)=grid(i)
    im2(j) = im(i)
	end if
  end do

!  print *,'Number of different cells',j
  deallocate(xl)
  allocate(xl(size(iscc)))

  xl=.true.
  
  do ii=1,nl-1
    do i=ii+1,nl
    if(iscc(ii).eq.iscc(i).and.xl(i)) xl(i)=.false.
	end do
  end do
  ii=0
  do i=1,nl
   if(xl(i)) then
   ii=ii+1
   end if
  end do
!  print *,'scc different',ii
  allocate(jscc(ii))
  allocate(pemi(j,7,ii))
  pemi=0
   ii=0
    do i=1,nl
     if(xl(i)) then
	 ii=ii+1
	 jscc(ii)=iscc(i)
	 end if
  end do
!  print *,(jscc(i),i=1,ii)
  deallocate(xl)
end subroutine count

end program
