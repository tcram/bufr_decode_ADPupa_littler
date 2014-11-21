        PARAMETER       ( MXMN = 8 )
        PARAMETER       ( MXLV = 86 )
        PARAMETER       ( NVAR = 17 )
        PARAMETER       ( NSTR = 4 )

        COMMON /BITBUF/ MAXBYT,IBIT,IBAY(5000),MBYT(32),MBAY(5000,32)

        REAL*8          r8arr ( MXMN, MXLV ), r8arr2(MXMN, MXLV ),
     +                  r8arr3 ( MXMN, MXLV ), r8arr4(MXMN, MXLV ) 

        PARAMETER       ( MXBF = 16000 )
        PARAMETER       (iu=9,iou=10,nz=999999)
 
        dimension pr(nz),tt(nz),td(nz)         

        integer  xht,nlev,iargc, n,minu,k
        real  xpr,xu,xv
        real  temp,v(nz),zx(nz),d(nz)
        real  lat(nz), lon(nz)
        real xt,xtd
        character*20 fin,fout
        character*10  date_tag,date(nz)
        character*6 dname
        character   argv*300,minute*2,M11*2,mins(nz)*2
        character*12 ilev,xy,xm,xd,xh,xmin,M5,M6,M7,M8
        character*12 M10,M1,M2,min,M3,M4,xn1,xn2,xn3,xn4,M9
        real wlon,elon,slat,nlat

        CHARACTER       cbfmsg*(MXBF),
     +                  csubset*8, inf*200, outstg*200

        CHARACTER*80   ostr(NSTR)

        INTEGER         ibfmsg ( MXBF / 4 ), ln, code,y,z,i,idate

        LOGICAL         msgok

        EQUIVALENCE     ( cbfmsg (1:4), ibfmsg (1) )

        ostr(1)='ACID ACRN ARST'
        ostr(2)='YEAR MNTH DAYS HOUR MINU'
        ostr(3)='CLAT CLON PRLC IALT'
        ostr(4)='MIXR REHU TMDB WDIR WSPD'

C*-----------------------------------------------------------------------
c*    Read the command-line arguments
c*      
        n = iargc()
        IF (n .GE. 2) THEN
          call getarg( 1, argv )
          inf=argv
          call getarg(2,argv)
          date_tag=argv
          IF (n .eq. 6) THEN  ! User-specified lat/lon boundaries
            call getarg(3,argv)
            read(argv,*) wlon
            call getarg(4,argv)
            read(argv,*) elon
            call getarg(5,argv)
            read(argv,*) slat
            call getarg(6,argv)
            read(argv,*) nlat
            write(*,*) 'Lon/lat boundaries: ',wlon,elon,slat,nlat
          ELSE  ! Default lon/lat boundaries
            slat = -90.
            nlat = 90.
            wlon = -180.
            elon = 180.
          END IF
        ELSE
          write(*,*) 'Usage: bufr_aircar2ob.x gdas.adpsfc.t<HH>z.
     +<YYYYMMDD>.bufr.be <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
          STOP
        END IF

!************************************
        !processing start from below

C*-----------------------------------------------------------------------

C*      Open the BUFR messages file.

        call getarg( 1, argv )
        inf=argv
        call getarg(2,argv)
        date_tag=argv

c*        write(*,*) 'enter input BUFR file?'
c*        read(*,'(a)') inf 
c*        write(*,*) 'Date_tag (YYYYMMDDHH) : '
c*        read(*,fmt='(a10)') date_tag

        dname=' AIREP'
        fout= "Airca"//date_tag//'.obs'
     
        OPEN  ( UNIT = 11, FILE =inf,form='unformatted' )
        open(iou,file=fout,status='unknown',form='formatted')
 
        iflag = 0
      nlev = 1
        dumm=99999.9

        isurf = 0
        ibogus = 0
        ter = dumm
        dslp= dumm
        do k=1,nz
         date(k)='MMMMMMMMMM'
         mins(k)='MM'
          pr(k)=dumm
          zx(k)=dumm
          tt(k)=dumm
          td(k)=dumm
          d(k)=dumm
          v(k)=dumm
        enddo
!********************************************

C*      Open the BUFR tables file.

C*        OPEN  ( UNIT = 12, FILE = 'bufrtab.example' )

C*      Open output file



C*      Associate the tables file with the messages file, and identify
C*      the latter to the BUFRLIB software.

        CALL OPENBF  ( 11, 'IN', 11 )

C*      Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHH ).

        CALL DATELEN  ( 10 )
     

        DO WHILE  ( .true. )

C*          Read the next BUFR message.

           call readns(11,csubset,idate,ierr)
C*           code = IUPBS1(MBAY,33) 
C*            write(*,*)' idate: ',idate,'  ',csubset,' ',code
c            write(*,*)' idate: ',idate,'  ',csubset
            IF  ( ierr .eq.  -1 )  THEN
                write(*,*) '....all records read.'
                CALL CLOSBF  ( 11 )
                Goto 1000  
            END IF

            msgok = .true.


            DO WHILE  ( msgok )


C*            At this point, we have a data subset within the
C*            internal arrays of BUFRLIB, and we can now begin
C*            reading actual data values:


              CALL UFBINT  ( 11, r8arr, MXMN, MXLV, nlv, ostr(1))
              CALL UFBINT  ( 11, r8arr2, MXMN, MXLV, nlv, ostr(2))
              CALL UFBINT  ( 11, r8arr3, MXMN, MXLV, nlv, ostr(3))
              CALL UFBINT  ( 11, r8arr4, MXMN, MXLV, nlv, ostr(4))
            minu=int(r8arr2(5,1))
            write (unit=minute, FMT='(I2)') minu
            DO k=1,2
               IF ( minute (k:k) .eq. ' ') THEN
                 minute (k:k) = '0'
               ENDIF
            ENDDO 
            DO z = 1, nlv
              WRITE (UNIT=outstg, FMT='(I10,1x,A8,2(1X,A8),
     +          1X,A4,1X,F6.1, 
     +          4(1X,F4.1),1X,F6.1,1x,F5.1,2(1X,F8.1),2(1X,F4.1),
     +          3(1X,F5.1))') idate,csubset,
     +          (r8arr(i,z), i = 1,3),(r8arr2(i,z), i = 1,5),
     +          (r8arr3(i,z), i = 1,4),(r8arr4(i,z), i = 1,5)
              DO y = 1,200
               IF ( outstg (y:y) .eq. '*') THEN
                 outstg (y:y) = 'm'
               ENDIF
              ENDDO
              read(outstg,21)M10,xn1,xn2,xn3,xn4,xy,xm,xd,
     &        xh,xmin,M1,M2,M3,M4,M5,M6,M7,M8,M9
              read(minute,22) M11
21            format(A10,1X,3(A8,1X),A4,1X,A6,
     &         4(1X,A4),1X,A6,1X,A5,
     &         2(1X,A8),2(1X,A4),2(1X,A5),1X,A5)
22            format(A2)
!              write(*,*)M10,M1,M2,M3,M4,M5,M6,M7,M8,M9
! *****************************************************
               !preparing variable for out put
               iflag =iflag+1
               j=iflag

               CALL READMval(M1,lat(j))
               CALL READMval(M2,lon(j))
               CALL READMval(M3,pr(j))
               CALL READMval(M7,tt(j))
               CALL READMval(M8,d(j))
               CALL READMval(M9,v(j))
               
!               write(*,*)'PR',pr(j)
               if(pr(j) .ne. 0 .and. pr(j) .ne. 99999.9 ) then
                  pr(j)= pr(j)/100
               end if
!               write(*,*)'PRM',pr(j)
               date(j)=M10
               mins(j)=M11 
!               write(*,*)iflag
            ENDDO

            
            

              CALL READSB  ( 11, ierrsb )

              IF  ( ierrsb .ne. 0 )  THEN

                  msgok = .false.

              ELSE
              END IF

            END DO

        END DO

1000   if (iflag .ne. 0) then

          write(iou,fmt='(a10)') date_tag
       do k = 1,iflag
        if(slat <= lat(k) .and. nlat >= lat(k) .and.
     &   wlon <= lon(k) .and. elon >= lon(k)) then
         write(iou,111)isurf,dname,dname,date(k),mins(k),
     &     lat(k),lon(k),ter,dslp,nlev,ibogus
         write(iou,112)pr(k),zx(k),tt(k),td(k),d(k),v(k)
        endif
       enddo
111    format(i1,2(1x,a6),1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112    format(6(f7.1,1x))

       endif
        write(*,*)'nlev ', nlev
2000  stop 99999

      END
      
      SUBROUTINE READMval(M1,fl)
           character*8 M1
           dumm=99999.9
           if(M1(1:1) ==  'm') then
               fl = dumm
           else
               read(M1,*)fl
           endif
       RETURN
         END
