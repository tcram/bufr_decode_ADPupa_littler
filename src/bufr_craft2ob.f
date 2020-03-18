      PARAMETER ( MXMN = 8 )
      PARAMETER ( MXLV = 86 )
      PARAMETER ( NVAR = 20 )
      PARAMETER ( NSTR = 6 )

      REAL*8 r8arr(MXMN, MXLV),  r8arr2(MXMN, MXLV),
     +       r8arr3(MXMN, MXLV), r8arr4(MXMN, MXLV),
     +       r8arr5(MXMN, MXLV), r8arr6(MXMN, MXLV) 

      parameter(iu=9,iou=10)

      real pr,tt,td,zx1,zx2
      integer xht,nlev,iargc,n,minu,k
      real xpr,xu,xv
      real temp,v,zx,d
      real lat, lon
      real xt,xtd
      character*30 fin,fout
      character*10 date_tag, date
      character*6 dname
      character argv*300,minute*2,M11*2,mins*2
      character*12 ilev,xy,xm,xd,xh,xmin,M5,M6,M7,M8
      character*12 M10,M1,M2,min,M3,M4,xn1,xn2,xn3,xn4,M9
      real wlon,elon,slat,nlat

      CHARACTER csubset*8, inf*200, outstg*200
      INTEGER y, z, i, idate, iflag

      CHARACTER*80   ostr(NSTR)

      ostr(1)='RPID YEAR MNTH DAYS'
      ostr(2)='HOUR MINU CLAT CLON FLVL PSAL'
      ostr(3)='CLATH CLONH HMSL'
      ostr(4)='TMDB WDIR WSPD'
      ostr(5)='DGOT HBOT HTOP'
      ostr(6)='HOCB HOCT'

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

C*-----------------------------------------------------------------------
C*    Open the BUFR messages file.
      OPEN(UNIT=11, FILE=inf, form='unformatted')

      dname=' AIREP'
      fout= "Aircraft"//date_tag//'.obs'

c     Open output file
      open(iou, file=fout, status='unknown', form='formatted')

      iflag=0
      nlev=1
      dumm=99999.9

      isurf=0
      ibogus=0
      ter=dumm
      dslp=dumm
      date='MMMMMMMMMM'
      mins='MM'
      pr=dumm
      zx=dumm
      tt=dumm
      td=dumm
      d=dumm
      v=dumm

C*-----------------------------------------------------------------------
C*      Associate the tables file with the messages file, and identify
C*      the latter to the BUFRLIB software.

      CALL OPENBF(11, 'IN', 11)

C*      Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHH ).

      CALL DATELEN(10)
     
C*-----------------------------------------------------------------------
      DO WHILE ( .true. )

C*      Read the next BUFR message.

        CALL READNS(11, csubset, idate, ierr)
c            write(*,*)' idate: ',idate,'  ',csubset
        IF (ierr .eq.  -1) THEN
          WRITE(*,*) '[bufr_craft2ob]....all records read, Exit'
          CALL CLOSBF(11)
          GOTO 2000 
        END IF

C*      At this point, we have a data subset within the
C*      internal arrays of BUFRLIB, and we can now begin
C*      reading actual data values:

        CALL UFBINT(11, r8arr,  MXMN, MXLV, nlv, ostr(1))
        CALL UFBINT(11, r8arr2, MXMN, MXLV, nlv, ostr(2))
        CALL UFBINT(11, r8arr3, MXMN, MXLV, nlv, ostr(3))
        CALL UFBINT(11, r8arr4, MXMN, MXLV, nlv1, ostr(4))
        CALL UFBINT(11, r8arr5, MXMN, MXLV, nlv, ostr(5))
        CALL UFBINT(11, r8arr6, MXMN, MXLV, nlv, ostr(6))

        minu=int(r8arr2(2,1))
        write (unit=minute, FMT='(I2)') minu

        DO k=1,2
          IF (minute (k:k) .eq. ' ') THEN
            minute (k:k) = '0'
          ENDIF
        ENDDO

        DO z = 1,nlv1
          WRITE (UNIT=outstg, FMT='(I10,1x,A8,1X,A6,
     +           1X,F6.1,4(1X,F4.1), 
     +           2(1X,F6.1),2(1X,F7.1),2(1X,F6.1),1X,F7.1,
     +           2(1X,F5.1),2(1X,F5.1),
     +           2(1X,F7.1),2(1X,F7.1))')
     +           idate,csubset,
     +           (r8arr(i,z), i = 1,4),(r8arr2(i,z), i = 1,6),
     +           (r8arr3(i,z), i = 1,3), (r8arr4(i,z), i = 1,3),
     +           (r8arr5(i,z), i = 1,3),(r8arr6(i,z), i = 1,2)
          DO y = 1,200
            IF ( outstg (y:y) .eq. '*') THEN
              outstg (y:y) = 'm'
            ENDIF
          ENDDO

          read(outstg,21) M10,M1,M2,M3,M4,M5,M6,M7
          read(minute,22) M11
21        format(A10,44X,A6,1X,A6,1X,A7,1X,A7,23X,A5,1X,A5,1X,A5)         
22        format(A2)          
 
          CALL READMval(M1,lat)
          CALL READMval(M2,lon)
          CALL READMval(M3,zx1)
          CALL READMval(M4,zx2)
          CALL READMval(M5,tt)
          CALL READMval(M6,d)
          CALL READMval(M7,v)
 
          date=M10
          mins=M11
          zx=99999.9
          if(zx1.ne.0 .and. zx1<99999) then
            zx=zx1
          end if
          if(zx>99999 .and. zx2.ne. 0 .and. zx2<99999) then 
            zx=zx2
          end if

c         Write output
          if (iflag.eq.0) then
            write(iou,fmt='(a10)') date_tag
            iflag=1
          endif

          if(slat<=lat .and. nlat>=lat .and.
     &       wlon<=lon .and. elon>=lon) then
             write(iou,111) isurf,dname,dname,date,mins,
     &       lat,lon,ter,dslp,nlev,ibogus
             write(iou,112) pr,zx,tt,td,d,v
           endif
111       format(i1,2(1x,a6),1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112       format(6(f7.1,1x))

        ENDDO
      END DO

C*-----------------------------------------------------------------------
2000  stop 99999

      END

C*-----------------------------------------------------------------------
      SUBROUTINE READMval(M1,fl)
      character*8 M1
      dumm=99999.9
      if(M1(1:1) == 'm') then
        fl = dumm
      else
        read(M1,*)fl
      endif

      RETURN
      END
