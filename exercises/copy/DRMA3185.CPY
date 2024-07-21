      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3185.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRMA3185.CPv  $
      *
      *   Rev 2.6   Nov 14 2013 15:27:26   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.5   Oct 15 2013 10:07:22   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.4   Oct 09 2013 14:15:18   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 19 2013 15:20:38   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.1   Jul 01 2013 11:41:46   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     SORTIERFELDER FÜR SALDENVERÄNDERUNGSSATZ
      *
      *     SOURCE-NAME      :  DRMA3185
      *
      *     GENERIERUNG      :  14.11.2013/15:19:25
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!20.06.13!2.2  ! ANPASSUNGEN DAISY         !VON HELDEN
      * AMA00018!23.07.13!2.3  ! ANPASSUNGEN DAISY         !VON HELDEN
      * AMA00018!09.10.13!2.4  ! ANPASSUNGEN DAISY         !VON HELDEN
      * AMA00018!15.10.13!2.5  ! ANPASSUNGEN LOGBUCH       !VON HELDEN
      * AMA00018!14.11.13!2.6  ! AKTUALISIEREN LOGBUCH     !VON HELDEN
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA3185.
              07 PG                     pic 99.
              07 PRODSCHL-SORT          pic 999.
              07 VERTRAGS-ID-SORT       pic X(15).
              07 TARIF-KZN              pic 99.
      *
      **************************************************************
