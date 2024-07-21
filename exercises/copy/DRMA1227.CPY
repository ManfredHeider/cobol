      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1227.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1227.CPv  $
      *
      *   Rev 2.2   Jul 03 2015 13:56:04   t13ac20
      *AMA00183: USt: KOMA0506-Druck - Programme/Copies
      *
      *   Rev 2.1   Jun 28 2013 16:19:08   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DRUCKZUS - DATENSATZ DRUCKINFODATEI
      *
      *     SOURCE-NAME      :  DRMA1227
      *
      *     GENERIERUNG      :  02.06.2015/13:30:17
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00183!01.06.15!2.2  ! DUPLEXKZN HINTER EFILE    ! WOLFF
      *         !        !     ! VERSCHOBEN; OVERLAY       !
      *         !        !     ! GELÖSCHT; COPYGROUP UND   !
      *         !        !     ! PAGEFORMAT JETZT DREIFACH;!
      *         !        !     ! FILLER AUF SATZLÄNGE 80   !
      *         !        !     ! BYTE ANGEPASST            !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1227.
              07 FACH-KENNUNG           pic X(10).
              07 DRUCKREIHENFOLGE       pic S9(4)      binary.
              07 DATEIKZN               pic X.
              07 EFILE                  pic S9(4)      binary.
              07 DUPLEXKZN              pic X.
              07 COPYGROUP-1            pic X(8).
              07 PAGEFORMAT-1           pic X(8).
              07 COPYGROUP-2            pic X(8).
              07 PAGEFORMAT-2           pic X(8).
              07 COPYGROUP-3            pic X(8).
              07 PAGEFORMAT-3           pic X(8).
              07 FILLER                 occurs 16
                                        pic X.
      *
      **************************************************************

