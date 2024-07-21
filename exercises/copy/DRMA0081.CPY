      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0081.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0081.CPv  $
      *
      *   Rev 2.2   Dec 17 2013 10:26:52   t13ac20
      *AMA00018: Anpassung Daisy - SST-Übernahme: Copies
      *
      *   Rev 2.1   Jun 28 2013 16:00:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     BESTAND PROV. JJJJ VBNR/PG/PRODSCHL/VERTRAGS-ID
      *
      *     SOURCE-NAME      :  DRMA0081
      *
      *     GENERIERUNG      :  17.12.2013/09:55:14
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00018!16.12.13!2.2  ! ANPASSUNG ANS GEÄNDERTE   ! WOLFF
      *         !        !     ! DATENMODELL               !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0081.
              07 VBNR                   pic 9(9).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 VERTRAGS-ID            pic X(15).
              07 JAHR                   pic 9(4).
              07 PROVISION              occurs 13
                                        indexed by IND
                                        pic S9(9)V99   packed-decimal.
      *
      **************************************************************
