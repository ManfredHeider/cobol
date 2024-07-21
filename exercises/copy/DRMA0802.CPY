      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0802.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0802.CPv  $
      *
      *   Rev 2.2   Oct 01 2015 17:27:18   t13ac18
      *AMA00199: USt: KOMA0505-VB-Abrechnung - Teil 2 - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:04:36   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Referenz-Tabelle zu Gesellschaften
      *
      *     SOURCE-NAME      :  DRMA0802
      *
      *     GENERIERUNG      :  30.09.2015/14:59:27
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00199!30.09.15!2.2  ! PG-UST-RELEVANT EINGEFÜGT ! D.WILHELM
      *         !        !     ! DVAG-GKTO-EIN             !
      *         !        !     ! DVAG-GKTO-AUS             !
      *         !        !     ! AVBG-GKTO-EIN             !
      *         !        !     ! AVBG-GKTO-AUS             !
      *         !        !     ! gelöscht                  !
      *         !        !     ! FILLER erweitert          !
      *         !        !     !                           !
      *         !        !     !                           !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0802.
              07 GESKZ                  pic 99.
              07 GESBEZ-KURZ            pic X(10).
              07 GESBEZ-LANG            pic X(40).
              07 LKZ                    pic X(4).
              07 BEW-ART-KZ             pic X(10).
              07 DVAG-KONTO             pic X(10).
              07 AVBG-KONTO             pic X(10).
              07 PG-UST-RELEVANT        pic X.
              07 FILLER                 occurs 13
                                        pic X.
      *
      **************************************************************

