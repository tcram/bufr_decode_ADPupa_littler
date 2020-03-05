        PARAMETER       ( MXMN = 8 )
        PARAMETER       ( MXLV = 220 )
        PARAMETER       ( NVAR = 17 )
        PARAMETER       ( NSTR = 4 )

        COMMON /BITBUF/ MAXBYT,IBIT,IBAY(5000),MBYT(32),MBAY(5000,32)

        REAL*8          r8arr ( MXMN, MXLV ), r8arr2(MXMN, MXLV ),
     +                  r8arr3 ( MXMN, MXLV ), r8arr4(MXMN, MXLV ) 

        PARAMETER       ( MXBF = 16000 )

        parameter(iu=9,iou=10,nz=9999999)

        dimension pr(nz),tt(nz),td(nz)
        integer  xht,nlev,i, iargc, n,minu,k
        real  xpr,xu,xv
        real  temp,v(nz),zx(nz),d(nz)
        real  lat(nz), lon(nz)
        real xt,xtd
        character*30 fin,fout
        character*10  date_tag,date(nz)
        character*6 dname
        character   argv*300,minute*2,M11*2,mins(nz)*2
        character*12 ilev
        character*12 M10,M1,M2,M3,M4,M5,M6
        real wlon,elon,slat,nlat

        CHARACTER       cbfmsg*(MXBF),
     +                  csubset*8, inf*200, outstg*200

        CHARACTER*80   ostr(NSTR)

        INTEGER         ibfmsg ( MXBF / 4 ), ln, code,y,z,idate

        LOGICAL         msgok

        EQUIVALENCE     ( cbfmsg (1:4), ibfmsg (1) )

        ostr(1)='SAID GCLONG SCLF RPID'
        ostr(2)='SIDP SWCM YEAR MNTH DAYS'
        ostr(3)='HOUR MINU CLAT CLON'
        ostr(4)='TMDBST PRLC WDIR WSPD'

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
          write(*,*) 'Usage: bufr_sat2ob.x gdas.satwnd.t<HH>z.
     +<YYYYMMDD>.bufr <YYYYMMDDHH> west_lon east_lon 
     +south_lat north_lat'
          STOP
        END IF

C*-----------------------------------------------------------------------

C*      Open the BUFR messages file
        OPEN (UNIT=11, FILE=inf, form='unformatted')

c*        write(*,*) 'Date_tag (YYYYMMDDHH) : '
c*        read(*,fmt='(a10)') date_tag

        dname=' SATOB'
        fout= "Satob"//date_tag//'.obs'

C*      Open output file
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

C*      Identify BUFR file to the BUFRLIB software.  DX BUFR tables
C*      are embedded within the first few messages of the BUFR file
C*      itself, thus we logical unit for the BUFR tables file is the 
C*      same as the BUFR file itself.

        CALL OPENBF  (11, 'IN', 11)

C*      Specify that we would like IDATE values returned using 10 digits
C*      (i.e. YYYYMMDDHHMM ).

        CALL DATELEN  ( 10 )
     
        ln=0 

C*-----------------------------------------------------------------------
        DO WHILE  ( .true. )

C*          Read the next BUFR message.

           call readns(11,csubset,idate,ierr)
c            write(*,*)' idate: ',idate,'  ',csubset
            IF  ( ierr .eq.  -1 )  THEN
                write(*,*) '[bufr_sat2ob]....all records read, Exit'
                CALL CLOSBF  ( 11 )
                Goto 1000 
            END IF

            msgok = .true.

            DO WHILE  ( msgok )

C*		    Read the next data subset from the BUFR message.
		    IF (IREADSB (11) .ne. 0) THEN
		        msgok = .false.
		    ELSE

C*            At this point, we have a data subset within the
C*            internal arrays of BUFRLIB, and we can now begin
C*            reading actual data values:

              CALL UFBINT  ( 11, r8arr, MXMN, MXLV, nlv, ostr(1))
              CALL UFBINT  ( 11, r8arr2, MXMN, MXLV, nlv, ostr(2))
              CALL UFBINT  ( 11, r8arr3, MXMN, MXLV, nlv, ostr(3))
              CALL UFBINT  ( 11, r8arr4, MXMN, MXLV, nlv, ostr(4))

            minu=int(r8arr3(2,1))
            write (unit=minute, FMT='(I2)') minu

            DO k=1,2
               IF ( minute (k:k) .eq. ' ') THEN
                 minute (k:k) = '0'
               ENDIF
            ENDDO

            DO z = 1,nlv
              WRITE (UNIT=outstg, FMT='(I10,1x,A8, 
     +          3(1X,F5.1),1X,A8,1X,F5.1, 
     +          1X,F4.1,1X,F6.1,4(1x,F4.1),2(1X,F7.2),1X,F6.2,
     +          1X,F7.1,1X,F5.1,1x,F6.2)') idate,csubset,
     +          (r8arr(i,z), i = 1,4),(r8arr2(i,z), i = 1,5),
     +          (r8arr3(i,z), i = 1,4),(r8arr4(i,z), i = 1,4)
              DO y = 1,200
               IF ( outstg (y:y) .eq. '*') THEN
                 outstg (y:y) = 'm'
               ENDIF
              ENDDO

              read(outstg,21,end=1000) M10,M1,M2,M3,M4,M5,M6
              read(minute,22) M11

21            format(A10,76X,A6,1X,A7,1X,A6,1X,A7,1X,A5,2X,A5)            
22            format(A2)
 
              iflag =iflag+1
              j=iflag

              CALL READMval(M1,lat(j))
              CALL READMval(M2,lon(j))
              CALL READMval(M3,tt(j))
              CALL READMval(M4,pr(j))
              CALL READMval(M5,d(j))
              CALL READMval(M6,v(j))

              date(j)=M10
              mins(j)=M11
              if(pr(j) .ne. 0 .and. pr(j) < 99999 ) then
                pr(j)= pr(j)/100
              end if

            END DO           
            END IF
            END DO
        END DO

C*-----------------------------------------------------------------------
C* Write to output file
C*-----------------------------------------------------------------------

1000    if (iflag .ne. 0) then
          write(iou,fmt='(a10)') date_tag
 
          do k = 1,iflag
           if(slat <= lat(k) .and. nlat >= lat(k) .and.
     &      wlon <= lon(k) .and. elon >= lon(k)) then
            write(iou,111)isurf,dname,dname,date(k),mins(k),
     &        lat(k),lon(k),ter,dslp,nlev,ibogus
            write(iou,112)pr(k),zx(k),tt(k),td(k),d(k),v(k)
           endif
          enddo
111      format(i1,2(1x,a6),1x,a10,a2,4(f7.1,1x),i3,1x,i1)
112      format(6(f7.1,1x))

        endif

2000    stop 99999        

        END

C*-----------------------------------------------------------------------

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
