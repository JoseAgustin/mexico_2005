!
!	atemporal.f90
!	
!
!	 Creado por Jose Agustin Garcia Reynoso 25/05/12.
!
! Proposito:
!          Realiza la distribucion temporal de las emisiones de area
!   ifort -O3 atemporal.f90 -o Atemporal.exe
!
!   modificado
!   14/08/2012  nombre archivos de PM 
!   02/10/2012  Ajuste en horas dia previo subroutina lee
!
module variables
integer :: month,daytype
integer :: nf !number of emission files
integer :: nnscc !max number of scc descriptors in input files
integer :: nm ! line number in emissions file
integer :: nh ! number of hour per day
integer :: lh ! line number in uso horario
integer ::juliano
parameter (nf=7,nh=24, nnscc=35,juliano=365)
integer,dimension(nf) :: nscc ! number of scc codes per file
integer*8,dimension(nnscc) ::iscc 
integer, allocatable :: idcel(:),idcel2(:)
integer, allocatable :: idsm(:),idsmh(:) ! state municipality IDs emiss and usoH
integer, allocatable :: mst(:)  ! Difference in number of hours (CST, PST, MST)
real,allocatable ::emiA(:,:,:) !Area emisions from files cel,ssc,file
real,allocatable :: emis(:,:,:) ! Emission by cel,file and hour (inorganic)
real,allocatable :: epm2(:,:,:) ! PM25 emissions cel,scc and hour
real,allocatable :: evoc(:,:,:) ! VOC emissions cel,scc and hour
real,dimension(nnscc,nf) :: mes,dia,diap ! dia currentday, diap previous day
real,dimension(nnscc,nf,nh):: hCST,hMST,hPST
integer,dimension(3,nnscc,nf):: profile  ! 1=mon 2=weekday 3=hourly
character(len=3),dimension(juliano):: cdia
character (len=19) :: current_date

character(len=14),dimension(nf) ::efile,casn

 data efile/'ASO2_2005.csv','ANOx_2005.csv','ANH3_2005.csv',&
&           'ACO__2005.csv','APM10_2005.csv','APM25_2005.csv',&
&           'AVOC_2005.csv'/
 data casn /'TASO2_2005.txt','TANOx_2005.txt','TANH3_2005.txt',&
&           'TACO__2005.txt','TAPM102005.txt','TAPM2_2005.txt',&
&           'TAVOC_2005.txt'/

common /vars/ nscc,lh,month,daytype,mes,dia,hora,current_date
end module
!
!  Progran  atemporal.f90
!
!  Make the temporal distribution of emissions
!

program atemporal 
   use variables
   
   call lee
   
   call compute
   
   call storage
   
contains

subroutine lee
	implicit none 
	integer i,j,k,l,m
	integer idum,imon,iwk,ipdy,idia
	integer*8 jscc ! scc code from temporal file
	integer,dimension(25) :: itfrc  !montly,weekely and hourly values and total
	integer,dimension(12) :: daym ! days in a month
	real rdum
	logical fil1,fil2
	character(len=4):: cdum
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
	STOP
	end if
	if (idia.gt.daym(month))then
	print '(A,I2,A,I2)','Error in day value: ',idia,' larger than days in month ',daym(month)
	STOP
	end if
	close(10)
    if(month.lt.10) then
        write(current_date,'(A6,I1,A12)')'2005-0',month,'-01_00:00:00'
        else
        write(current_date,'(A5,I2,A12)')'2005-',month,'-01_00:00:00'
    end if
    if(idia.lt.10) then
        write(current_date(10:10),'(I1)') idia
        else 
        write(current_date( 9:10),'(I2)') idia
    end if
    print *,'Done fecha.txt : ',current_date,month,idia
!
    print *,"READING uso_horario.csv file"
    open (unit=10,file='uso_horario.csv',status='OLD',action='read')
    lh=0
    read(10,*)cdum
        do
        read(10,*,end=90)cdum
        lh=lh+1
        end do
90  continue
    print *,'Line number in uso hor',lh
    allocate(idsmh(lh),mst(lh))
    rewind(10)
    read (10,'(A)') cdum
    do i=1,lh
    read (10,*) idsmh(i),mst(i)
    end do
    print *,'Done uso_horario.csv :'
    close(10)
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
!
	do k=1,nf
	open (unit=10,file=efile(k),status='OLD',action='read')
	read (10,'(A)') cdum
	read (10,*) nscc(k),cdum,(iscc(i),i=1,nscc(k))
	!print '(5(I10,x))',(iscc(i),i=1,nscc(k))
	!print *,cdum,nscc(k)
	nm=0
	do
	   read(10,*,end=100) cdum
	   nm=nm+1
	end do
100	 continue
     !print *,nm
	 rewind(10)
	 if(k.eq.1) then
	allocate(idcel(nm),idcel2(nm),idsm(nm))
	allocate(emiA(nf,nm,nnscc))
	end if
	read (10,'(A)') cdum
	read (10,'(A)') cdum
	do i=1,nm
		read(10,*) idcel(i),idsm(i),rdum,rdum,(emiA(k,i,j),j=1,nscc(k))
	        !print *,idcel(i),idsm(i),(emiA(k,i,j),j=1,16),nscc(k),k
	end do
	close(10)
	print *,"Done reading: ",efile(k)
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
	    do i=1,nscc(k)
		  if(iscc(i).eq.jscc) then
		    profile(1,i,k)=imon
		    profile(2,i,k)=iwk
		    profile(3,i,k)=ipdy
		   end if
		end do
	  end do
 200 continue
      !print '(A3,<nscc(k)>(I5))','mon',(profile(1,i,k),i=1,nscc(k))      
      !print '(A3,<nscc(k)>(I3,x))','day',(profile(2,i,k),i=1,nscc(k))      
	  !print '(A3,<nscc(k)>(I3,x))','hr ',(profile(3,i,k),i=1,nscc(k))
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
	    do i=1,nscc(k)
	      if(jscc.eq.profile(1,i,k)) then
	        mes(i,k)=real(itfrc(month))/real(itfrc(13))
	      end if
		end do !i
	 end do
	 mes=mes/daym(month)! days per month
 210 continue
	! print '(A3,<nscc(k)>(f6.3))','mon',(mes(i,k),i=1,nscc(k))
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
	    do i=1,nscc(k)
	      if(jscc.eq.profile(2,i,k)) then
	        dia(i,k)=real(itfrc(daytype))/real(itfrc(8))
            if(daytype.eq.1) then
             diap(i,k)=real(itfrc(daytype+6))/real(itfrc(8))
            else
             diap(i,k)=real(itfrc(daytype-1))/real(itfrc(8))
            end if
	      end if
		end do !i
	 end do
 220 continue
     !print '(A5,<nscc(k)>(f6.3))','day  ',(dia(i,k),i=1,nscc(k))
     !print '(A5,<nscc(k)>(f6.3))','day p',(diap(i,k),i=1,nscc(k))
	 print *,'   Done Temporal_week'
     nfile='temporal_wkday.txt'
	 nfilep='temporal_wkend.txt'
!  Reading and findig houlry  profile
    inquire(18,opened=fil1)

    if(.not.fil1) then
	  open(unit=18,file=nfile,status='OLD',action='read')
	else
	  rewind(18)
	end if
!
	read (18,'(A)') cdum
     do
	    read(18,*,END=230)jscc,(itfrc(l),l=1,25)
	    do i=1,nscc(k)
	      if(jscc.eq.profile(3,i,k)) then
		    m=5
		    do l=1,nh
			if(m+l.gt.nh) then
              hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             else
              hCST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
            m=6
            do l=1,nh
            if(m+l.gt.nh) then
              hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hMST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
            end do
            m=7
            do l=1,nh
            if(m+l.gt.nh) then
              hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
            else
              hPST(i,k,m+l)=real(itfrc(l))/real(itfrc(25))*dia(i,k)
            end if
			end do
	      end if
		end do !i
     end do    ! File 18
230 continue

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
        do i=1,nscc(k)
         if(jscc.eq.profile(3,i,k)) then
           m=5
           do l=1,nh
             if(m+l.gt.nh) then
                hCST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             end if
           end do
           m=6
           do l=1,nh
             if(m+l.gt.nh) then
                hMST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             end if
           end do
           m=7
           do l=1,nh
             if(m+l.gt.nh) then
                hPST(i,k,m+l-nh)=real(itfrc(l))/real(itfrc(25))*diap(i,k)
             end if
           end do
         end if
        end do !i 
        end do ! File 19
     end if
 240 continue
     !do l=1,nh
     ! print '(A3,x,I2,x,<nscc(k)>(f6.3))','hr',l,(hCST(i,k,l),i=1,nscc(k))
	 !end do
	 print *,'   Done ',nfile,daytype
	end do ! K
	close(15)
	close(16)
	close(17)
	close(18)
    close(19)
end subroutine lee
!
subroutine compute
	implicit none
	integer i,j,k,l,ival,ii
!
    call count ! computes the number of different cells
    call uso_horario ! identifies the time lag in each cell
!
! For inorganics
!	
	do k=1,nf-2
	  ival=idcel(1)
	  ii=1
	  do i=1,nm
		 if(ival.eq.idcel(i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(i).eq.6) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(i).eq.7) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(i).eq.8) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
		  else
		  ival=idcel(i)
		  ii=ii+1
		   if(ii+1 .le.size(emis,dim=1)) then
		    do l=1,nh
	         do j=1,nscc(k)
    if(idsm(i).eq.6) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(i).eq.7) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(i).eq.8) emis(ii,k,l)=emis(ii,k,l)+emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		     end do
			end do

		   end if
		  end if
	  end do
	end do
!
!  For PM2.5
!  
	k=nf-1
   ii=1
   ival=idcel(1)
   do i=1,nm
   if(ival.eq.idcel(i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(i).eq.6) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(i).eq.7) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(i).eq.8) epm2(ii,j,l)=epm2(ii,j,l)+emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		    end do
		  end do
		  else
		  ival=idcel(i)
		  ii=ii+1
		   if(ii+1 .le.size(emis,dim=1)) then
		    do l=1,nh
	         do j=1,nscc(k)
    if(idsm(i).eq.6) epm2(ii,j,l)=emiA(k,i,j)*mes(j,k)*hCST(j,k,l)
    if(idsm(i).eq.7) epm2(ii,j,l)=emiA(k,i,j)*mes(j,k)*hMST(j,k,l)
    if(idsm(i).eq.8) epm2(ii,j,l)=emiA(k,i,j)*mes(j,k)*hPST(j,k,l)
		     end do
			end do
		   end if
		  end if
	  end do
!	
!  For VOCs
!  
	k=nf
   ii=1
   ival=idcel(1)
   do i=1,nm
   if(ival.eq.idcel(i))then
		  do l=1,nh
	        do j=1,nscc(k)
    if(idsm(i).eq.6) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hCST(j,nf,l)
    if(idsm(i).eq.7) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hMST(j,nf,l)
    if(idsm(i).eq.8) evoc(ii,j,l)=evoc(ii,j,l)+emiA(nf,i,j)*mes(j,nf)*hPST(j,nf,l)
		    end do
		  end do
		  else
		  ival=idcel(i)
		  ii=ii+1
		   if(ii+1 .le.size(emis,dim=1)) then
		    do l=1,nh
	         do j=1,nscc(k)
    if(idsm(i).eq.6) evoc(ii,j,l)=emiA(nf,i,j)*mes(j,nf)*hCST(j,nf,l)
    if(idsm(i).eq.7) evoc(ii,j,l)=emiA(nf,i,j)*mes(j,nf)*hMST(j,nf,l)
    if(idsm(i).eq.8) evoc(ii,j,l)=emiA(nf,i,j)*mes(j,nf)*hPST(j,nf,l)
		     end do
			end do
		   end if
		  end if
	  end do
	end subroutine compute
!
subroutine storage
  implicit none
  integer i,j,k,l
  character(len=3):: cdia(7)
  data cdia/'MON','TUE','WND','THR','FRD','SAT','SUN'/

  do k=1,nf-2
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),'ID, Hr to Hr24'
   write(10,'(I8,4A)')size(emis,dim=1),",",current_date,', ',cdia(daytype)
   do i=1,size(emis,dim=1)
     write(10,100)idcel2(i),(emis(i,k,l),l=1,nh)
   end do
   close(unit=10)
  end do
100 format(I7,x,24E0.4)
   k=nf-1
! WARNING iscc and pm25 must be the before last one to be read.
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(10,'(I8,4A)')size(epm2,dim=1)*nscc(k),",",current_date,', ',cdia(daytype)
   do i=1,size(epm2,dim=1)
     do j=1,nscc(k)
     write(10,110)idcel2(i),iscc(j),(epm2(i,j,l),l=1,nh)
     end do
   end do
	close(10)
	k=nf
! WARNING iscc and voc must be the last one to be read.
   open(unit=10,file=casn(k),action='write')
   write(10,*)casn(k),'ID, SCC,  Hr to Hr24'
   write(10,'(I8,4A)')size(evoc,dim=1)*nscc(k),",",current_date,', ',cdia(daytype)
   do i=1,size(evoc,dim=1)
     do j=1,nscc(k)
     write(10,110)idcel2(i),iscc(j),(evoc(i,j,l),l=1,nh)
     end do
   end do
	close(10)
110 format(I7,x,I10,x,24E0.4)
end subroutine storage
subroutine count
  integer i,j
  idcel2(1)=idcel(1)
  j=1
  do i=2,nm
   if(idcel2(j).ne.idcel(i)) then
    j=j+1
	idcel2(j)=idcel(i)
	end if
	 
  end do
  print *,'Number of different cells',j
  allocate(emis(j,nf-2,nh))
  allocate( epm2(j,nscc(nf-1),nh),evoc(j,nscc(nf),nh))
   emis=0
   evoc=0
end subroutine count
subroutine uso_horario
    integer ::i,j
    !print *,'Start uso horario'
    do i=1,nm
        do j=1,lh
        if(idsm(i).eq.idsmh(j) )idsm(i)=mst(j)
        end do
    end do
    if(maxval(idsm).gt.8 .or.minval(idsm).lt.6) then
        print *,'Error item:', MAXLOC(idsm),'value:', maxval(idsm)
        STOP 'Value must be between 6 to 8'
    end if
end subroutine uso_horario
end program atemporal
