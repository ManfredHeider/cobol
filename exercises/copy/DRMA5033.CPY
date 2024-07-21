      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5033.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5033.CPv  $
      *
      *   Rev 2.4   Jun 23 2015 10:53:50   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - Programme/Copies
      *
      *   Rev 2.3   Oct 17 2013 11:07:20   t13ac13
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.2   Oct 17 2013 07:46:14   t13ac13
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.1   Oct 11 2013 09:17:20   t13ac13
      *AMA00027: Anpassung Daisy - VB-Abrechnung
      *
      *   Rev 2.0   Aug 22 2013 12:41:16   t13ac13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE: UPMA5011 FUNKTION V1
      *
      *     SOURCE-NAME      :  DRMA5033
      *
      *     GENERIERUNG      :  23.06.2015/10:39:06
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      *----------------------------------------------------------------
      * AMA00027!22.08.13!2.1  ! NEUERSTELLUNG             ! BOOST
      * AMA00027!17.10.13!2.2  ! PFLEGE LOGBUCH            ! BOOST
      * AMA00027!17.10.13!2.3  ! PFLEGE LOGBUCH            ! BOOST
      * AMA00148!23.06.15!2.4  ! VERSION, VG EINGEFÜGT     ! WOLFF
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5033.
              07 DGAT0100.
                 09 FUNKTION            pic X(8).
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 DYN-KZN1               pic 9.
              07 DYN-KZN2               pic 9.
      *
      **************************************************************

