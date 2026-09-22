!
!	t_puntal.f90
!	
!
!  Creado por Jose Agustin Garcia Reynoso el 06/06/12.
! Proposito
!          Distribución temporal de las emisiones de fuentes puntuales
!
! ifort -O3 t_puntal.f90 -o Puntual.exe
!
!   modificado
!   14/08/2012  nombre archivos de PM
!   02/10/2012  Ajuste en horas dia previo subroutina lee

module vars
integer :: nsp,nh    !number of compounds & hours
integer ::juliano
parameter (nsp=6, nh=24,juliano=365)
integer :: month,daytype
integer*8,allocatable:: iscc(:)
integer,allocatable :: capa(:),ict(:),jct(:),idcg(:,:)
integer,allocatable :: profile(:,:),mcst(:,:)
integer :: nl,nx,ny
real,allocatable :: lat(:),lon(:),pf(:,:)
real,allocatable :: e_mis(:,:),emis(:,:,:)! line compounds nsp
real,allocatable :: mes(:),dia(:),diap(:),hCST(:,:),hMST(:,:),hPST(:,:)
character (len=7) :: cvar(nsp)
character (len=19) :: current_date

common /dat/ nl,nx,ny,month,daytype,cvar,current_date
end module vars
!
program t_puntual
use vars
	call lee
	
	call calculos
	
	call guarda

contains
subroutine lee
implicit none
	integer :: i,j,k,l,m
	integer :: idum, imon,iwk,ipdy,idia
	integer*8:: jscc
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	integer,dimension(12) :: daym ! days in a month
	real,allocatable ::xlat(:,:),xlon(:,:)
	real rdum
	logical fil1,fil2
	character(len=10)::cdum
	character(len=18):: nfile,nfilep
	! number of day in a month 
	!          jan feb mar apr may jun jul aug sep oct nov dec
	data daym /31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/

	print *,"READING fecha.txt file"
	open (unit=10,file='fecha.txt',status='OLD',action='read')
	read (10,*)month  
	read (10,*)idia
	month=abs(month)
	idia=abs(idia)
	if (month.lt.1 .or. month.gt.12) then
	print '(A,I3)','Error in month (from 1 to 12) month= ',month
	stop
	end if
	if (idia.gt.daym(month))then
	print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
	Stop
	end if
	close(10)
	if(month.lt.10) then
	write(current_date,'(A6,I1,A12)')'2005-0',month,'-01_00:00:00'
	else
	write(current_date,'(A5,I2,A12)')'2005-0',month,'-01_00:00:00'	
	end if
    if(idia.lt.10) then
        write(current_date(10:10),'(I1)') idia
        else 
        write(current_date( 9:10),'(I2)') idia
    end if
	print *,'Done fecha.txt ',current_date
!
!   Days in 2005 year
!
    print *,"READING anio2005.csv file"
    open (unit=10,file='anio2005.csv',status='OLD',action='read')
    daytype=0
    read(10,*)cdum
        do
        read(10,*,end=95)imon,ipdy,idum,cdum
		if(imon.eq.month.and. ipdy.eq.idia) then
		 daytype=idum
		 print *,'Day type :',daytype,cdum
		 exit
		 end if
        end do
95  continue
    close(10)
	if(daytype.eq.0) STOP 'Error in daytype=0'

	open(unit=10,file='puntual.csv',status='old',action='read')
	i=0
	read (10,*) cdum
	do
		read(10,*,end=100) cdum
		i=i+1
	end do
100 continue
	nl=i
	print *,'numero de lineas ',nl
	allocate(iscc(i),capa(i),lat(i),lon(i),e_mis(i,nsp))
	allocate(mes(nl),dia(nl),diap(nl),hCST(nl,nh),hMST(nl,nh),hPST(nl,nh))
	allocate(emis(i,nsp,nh))
	allocate(profile(3,nl))
	allocate(ict(nl),jct(nl))

	do i=1,nl  ! Defaul values for temproal profile
		profile(1,i)=262
		profile(2,i)=7
		profile(3,i)=24
	end do
	rewind(10)
	read (10,*) cdum,cdum,cdum,(cvar(i),i=1,nsp)
	do i=1,nl
	 read(10,*,err=110)lat(i),lon(i),iscc(i),(e_mis(i,j),j=1,nsp),capa(i)
	end do
	close(10)
    e_mis=e_mis*1000 !para g desde TON
		print *,'Done puntual.csv ',cvar,maxval(e_mis)
!
!	temporal_01.txt
!
	write(6,*)' >>>> Reading file -  localiza.csv ---------'

	open (unit=10,file='localiza.csv',status='old',action='read')
	read (10,*) cdum  !Header
	read (10,*) nx,ny  !Header
	allocate(idcg(nx,ny),xlon(nx,ny),xlat(nx,ny),mcst(nx,ny))
	do j=1,ny
		do i=1,nx
			read(10,*) idcg(i,j),xlon(i,j),xlat(i,j),mcst(i,j)
		end do
	end do
	!print *,ncel
	close(10)
    print *,'   >>>>>  Finding i,j for each cell localization'
     call localization(xlat,xlon,nx,ny,lat,lon,ict,jct,nl)   ! Point Sources
    print *,'   >>>>>  Finding emissions in grid'


!  REading and findig monthly, week and houry code profiles
    inquire(15,opened=fil1)
    if(.not.fil1) then
	  open(unit=15,file='temporal_01.txt',status='OLD',action='read')
	else
	  rewind(15)
	end if
	read (15,'(A)') cdum
      do
	  read(15,*,END=200)jscc,imon,iwk,ipdy
	    do i=1,nl
		  if(iscc(i).eq.jscc) then
		    profile(1,i)=imon
		    profile(2,i)=iwk
		    profile(3,i)=ipdy
		   end if
		end do
	  end do
 200 continue
      !print '(A3,<nl>(I5))','mon',(profile(1,i),i=1,nl)
      !print '(A3,<nl>(I3,x))','day',(profile(2,i),i=1,nl)
	  !print '(A3,<nl>(I3,x))','hr ',(profile(3,i),i=1,nl)
	 print *,'   Done Temporal_01'
!  REading and findig monthly  profile
    inquire(16,opened=fil1)
    if(.not.fil1) then
	  open(unit=16,file='temporal_mon.txt',status='OLD',action='read')
	else
	  rewind(16)
	end if
	read (16,'(A)') cdum
     do
	    read(16,*,END=210)jscc,(itfrc(l),l=1,13)
	    do i=1,nl
	      if(jscc.eq.profile(1,i)) then
	        mes(i)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
	 end do
	 mes=mes/daym(month)! days per month
 210 continue
    ! print '(A3,<nl>(f6.3))','mon',(mes(i),i=1,nl)
	 print *,'   Done Temporal_mon'
!  REading and findig weekely  profile
    inquire(17,opened=fil1)
    if(.not.fil1) then
	  open(unit=17,file='temporal_week.txt',status='OLD',action='read')
	else
	  rewind(17)
	end if
	read (17,'(A)') cdum
     do
	    read(17,*,END=220)jscc,(itfrc(l),l=1,8)
	    do i=1,nl
	      if(jscc.eq.profile(2,i)) then
	        dia(i)=real(itfrc(daytype))/real(itfrc(8))
            if(daytype.eq.1) then
                diap(i)=real(itfrc(daytype+6))/real(itfrc(8))
            else
                diap(i)=real(itfrc(daytype-1))/real(itfrc(8))
            end if
	      end if
		end do !i
	 end do
 220 continue
     !print '(A3,<nl>(f6.3))','day',(dia(i),i=1,nl)
	 print *,'   Done Temporal_week'
	 nfilep='temporal_wkend.txt'
	 nfile='temporal_wkday.txt'
!  REading and findig houlry  profile
    inquire(18,opened=fil1)
    if(.not.fil1) then
	  open(unit=18,file=nfile,status='OLD',action='read')
	else
	  rewind(18)
	end if
	read (18,'(A)') cdum
     do
	    read(18,*,END=230)jscc,(itfrc(l),l=1,25)
	    do i=1,nl
	      if(jscc.eq.profile(3,i)) then
		    m=5
		    do l=1,nh
              if(m+l.gt.nh) then
                hCST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
			  else
                hCST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
			  end if
			end do
		    m=6
		    do l=1,nh
              if(m+l.gt.nh) then
                hMST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
              else
                hMST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
              end if
			end do
		    m=7
		    do l=1,nh
              if(m+l.gt.nh) then
                hPST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
			  else
                hPST(i,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i)
			  end if
			end do
	      end if
		end do !i
	 end do
 230 continue
!   do l=1,nh
!   print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!   end do
    if(daytype.eq.1) then
        inquire(19,opened=fil2)
        if(.not.fil2) then
            open(unit=19,file=nfilep,status='OLD',action='read')
        else
            rewind(19)
        end if
        read (19,'(A)') cdum
        do
          read(19,*,END=240)jscc,(itfrc(l),l=1,25)
          do i=1,nl
            if(jscc.eq.profile(3,i)) then
              m=5
              do l=1,nh
              if(m+l.gt.nh) then
                  hCST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
              end if
              end do
              m=6
              do l=1,nh
                if(m+l.gt.nh) then
                    hMST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
                end if
              end do
              m=7
              do l=1,nh
                if(m+l.gt.nh) then
                    hPST(i,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i)
                end if
              end do
            end if
          end do !i
        end do ! File 19
240 continue
!
!    do l=1,nh
!    print '(A3,x,I3,x,<nscc(k)>(f7.4))','hr',l,(hCST(i,k,l),i=1,nscc(k))
!    end do
!
end if

    print *,'   Done ',nfile,daytype

	close(15)
	close(16)
	close(17)
	close(18)
    close(19)


	return
110	print *,'Error en ',i
end subroutine lee
subroutine calculos
	implicit none
	integer i,j,k,l,ival,ii
!
	print *,'Calculos'
	do i=1,nl
		do k=1,nsp
			do l=1,nh
	if(mcst(ict(i),jct(i)).eq.6 )emis(i,k,l)=e_mis(i,k)*mes(i)*hCST(i,l)
	if(mcst(ict(i),jct(i)).eq.7 )emis(i,k,l)=e_mis(i,k)*mes(i)*hMST(i,l)
	if(mcst(ict(i),jct(i)).eq.8 )emis(i,k,l)=e_mis(i,k)*mes(i)*hPST(i,l)
			end do
		end do
	end do

	

end subroutine calculos
subroutine guarda
	implicit none
	integer:: i,j,k,l
	character(len=13) ::fname
	character(len=3):: cdia(7)
	data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/
	do k=1,nsp
		fname='T_'//trim(cvar(k))//'.csv'
		open(unit=10,file=fname,action='write')
		if(k.ne.nsp .and. k.ne.4) then
		write(10,*)cvar(k),'Lat,Lon,Capa, H1,H2,H3,H4,H5,H6,H7,H8,H9,to Hr24'
		else
		write(10,*)cvar(k),'SCC,Lat,Lon,Capa, H1,H2,H3,H4,H5,H6,H7,H8,H9,to Hr24'
		end if
		write(10,'(I6,4A)') nl,', ',current_date,', ',cdia(daytype) 
			do i=1,nl
			if(ict(i).ne.0 .or.jct(i).ne.0)then
				if(k.ne.nsp.and.k.ne.4) then
					!write(10,220)lat(i),lon(i),capa(i),(emis(i,k,l),l=1,nh)
					write(10,210) idcg(ict(i),jct(i)),capa(i),(emis(i,k,l),l=1,nh)
				else
					!write(10,300)iscc(i),lat(i),lon(i),capa(i),(emis(i,k,l),l=1,nh)
					write(10,310)iscc(i),idcg(ict(i),jct(i)),capa(i),(emis(i,k,l),l=1,nh)
				end if
			end if
			end do
		close(unit=10)
	end do
210 format(I8,',',I3,',',23(ES0.4,","),ES0.4)
220 format(f10.6,',',f10.4,',',I3,',',23(ES0.4,","),ES0.4)
300 format(I10,',',f10.6,',',f10.4,',',I3,',',23(ES0.4,","),ES0.4)
310 format(I10,',',I8,',',I3,',',23(ES0.4,","),ES0.4)
end subroutine guarda
!
   Subroutine localization(xlat,xlon,mi,mj,clat,clon,ist,jst,nst)
   implicit none
   integer :: mi,mj,nst,i,j,l
   integer,dimension(nst):: ist,jst
   real,dimension(mi,mj):: xlat,xlon
   real,dimension(nst):: clat,clon
   do l=1,nst
		! Out of the region 
   ist(l)=0
   jst(l)=0
	  do i = 1,mi-1
	    do j= 1,mj-1
        if(clon(l) .ge. xlon(i,j)  .and. clon(l) .le. xlon(i+1,j).and.&
		  &clat(l) .ge. xlat(i,j)  .and. clat(l) .le. xlat(i,j+1))then
		   ist(l)= i
		   jst(l)= j

		end if
		end do
	  end do
   end do
   RETURN
   end subroutine localization
end program t_puntual
