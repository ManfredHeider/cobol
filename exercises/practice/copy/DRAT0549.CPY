      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0549.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAT0549.CPv  $
      *
      *   Rev 2.3   Jul 23 2013 17:50:02   t13ac14
      *AMA00018: Anpassung Daisy - SST-Übernahme MA-Daten
      *
      *   Rev 2.2   Jul 08 2013 16:13:34   t13ac18
      *AMA00018: Anpassung Daisy - SST-Übernahme
      *
      *   Rev 2.1   Jun 28 2013 15:06:32   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ALLGEMEINER STATUS-BEREICH
      *
      *     SOURCE-NAME      :  DRAT0549
      *
      *     GENERIERUNG      :  08.07.2013/16:11:18
      *     DURCH            :  PMMA5004/01.00.00
      *
      * LOGBUCH
      * AUFTRAG ! DATUM  ! AENDERUNG                     ! NAME
      * --------+--------+-------------------------------+-------------
      * AMA00018!08.07.13! NEUERSTELLUNG                 ! VON HELDEN/
      *         !        !                               ! D.WILHELM
      * --------+--------+-------------------------------+-------------
      *         !        !                               !
      * --------+--------+-------------------------------+-------------
      *         !        !                               !
      * --------+--------+-------------------------------+-------------
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 STATUS-BEREICH.
              07 PROGRAMMNAME           pic X(8).
              07 RC                     pic 99.
              07 MELDUNGS-BESCHREIBUNG.
                 09 MELDUNGS-NR         pic 9(8).
                 09 SUB-VAR-1           pic X(20).
                 09 SUB-VAR-2           pic X(20).
              07 FEHL-BER-ALT           pic X(45).
              07 FILLER                 occurs 47
                                        pic X.
      *
      **************************************************************
