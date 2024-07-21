      **************************************************************
      *
      *     EINGABE: UPMA5041   FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5129
      *
      *     GENERIERUNG      :  02.10.2017/14:05:04
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00217!19.02.16!2.1  ! NEUERSTELLUNG             ! WOLFF
      * AMA00217!25.04.16!2.2  ! BUCHDAT ERGÄNZT           ! WOLFF
      * AMA00263!12.05.16!2.3  ! UST-PFLICHTIG-KZN ERGÄNZT ! WOLFF
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DGMA5007 EINBEH. ! WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5129.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 VBNR                   pic 9(9).
              07 FACH-KENNUNG           pic X(10).
              07 DV-KENNUNG             pic X(10).
              07 BUCHDAT                pic X(10).
              07 PG                     pic 99.
              07 ERWERB-FOLGE-KZN       pic X(8).
              07 BNGBETR-1              pic S9(9)V99   packed-decimal.
              07 BNGBETR-2              pic S9(9)V99   packed-decimal.
              07 UST-PFLICHTIG-KZN      pic X.
              07 DGMA5007.
                 09 UST-BERECH-KZN      pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PROV-AUS-EINB-UST   pic S9(9)V99   packed-decimal.
                 09 PROV-AUS-EINB       pic S9(9)V99   packed-decimal.
                 09 PROV-AUS            pic S9(9)V99   packed-decimal.
                 09 PROV-EINB           pic S9(9)V99   packed-decimal.
                 09 PROV-UST            pic S9(9)V99   packed-decimal.
      *
      **************************************************************

