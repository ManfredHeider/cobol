      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5024.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRMA5024.CPv  $
      *
      *   Rev 2.7   Nov 17 2020 10:56:18   t13ac18
      *AMA00486: Layoutänderung SPNP 
      *
      *   Rev 2.6   Oct 19 2017 14:24:02   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.5   Feb 23 2015 11:15:38   t13ac13
      *FMA00008: Korrektur Ausweis "Zugang" in der SPnP - Copies
      *
      *   Rev 2.4   Apr 02 2014 10:57:24   t13ex75
      *AMA00019: Anpassung Daisy - Auswertungen / Listen
      *
      *   Rev 2.3   Mar 31 2014 11:18:52   t13ex75
      *AMA00019: Anpassung Daisy - Auswertungen / Listen
      *
      *   Rev 2.2   Oct 17 2013 11:27:50   t13ex75
      *AMA00019: Anpassung Daisy - Auswertungen / Listen
      *
      *   Rev 2.1   Oct 14 2013 07:13:56   t13ex75
      *AMA00019: Anpassung Daisy - Auswertungen / Listen
      *
      *   Rev 2.0   Jul 11 2013 13:40:20   t13ex75
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz norm. Arbeitsbestand SPnP/AE-Statistik
      *
      *     SOURCE-NAME      :  DRMA5024
      *
      *     GENERIERUNG      :  02.10.2017/13:38:22
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00019!14.10.13!2.1  ! NEUERSTELLUNG             ! J. Niehus
      * AMA00019!17.10.13!2.2  !                           ! J. Niehus
      * AMA00019!31.03.14!2.3  !                           ! J. Niehus
      * AMA00019!02.04.14!2.4  !                           ! J. Niehus
      * FMA00008!11.02.15!2.5  ! Änderung in DGMA0107      ! Boost
      * AMA00360!20.09.17!2.6  ! ÄNDERUNG DGMA0107         ! S.WARZ
      * AMA00486!29.10.20!2.7  ! ÄNDERUNG DGMA0107         ! D.Wilhelm
      *         !        !     !                           !
      *         !        !     !                           !
      * --------------------------------------------------------------
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5024.
              07 DGMA0107.
                 09 PG                  pic 99.
                 09 VERTRAGS-ID         pic X(15).
                 09 PRODSCHL            pic 999.
                 09 DGV                 pic 9(5).
                 09 DYN-KZN2            pic 9.
                 09 PRODART-VG          pic 99.
                 09 PRODART-PG          pic 99.
                 09 GEVO-TYP-VG         pic X(04).
                 09 GEVO-TYP-PG         pic X(04).
                 09 BEWERTUNGS-BETR     pic S9(11)V99  packed-decimal.
                 09 BEWERTUNGS-BETR-DYN pic S9(11)V99  packed-decimal.
                 09 AE                  pic S9(10)V9   packed-decimal.
                 09 AE-EING-GESCH       pic S9(10)V9   packed-decimal.
      *
      **************************************************************


