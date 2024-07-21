      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1216.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA1216.CPv  $
      *
      *   Rev 2.3   Jun 08 2020 14:26:58   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.2   Nov 30 2015 09:53:52   t13ac20
      *AMA00024: SEPA-Nacharbeiten - Copies/Programme Datenmodell
      *
      *   Rev 2.1   Jun 28 2013 16:18:22   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINGABE-BEREICH UPMA1211 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1216
      *
      *     GENERIERUNG      :  19.05.2020/12:24:05
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00024!19.11.15!2.2  ! ÄNDERUNG DRMA1102         ! WOLFF
      * AMA00476!05.05.20!2.3  ! ÄNDERUNG DRMA1102         ! T13AC18
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1216.
              07 FUNKTION               pic X(8).
              07 BANKVERBINDUNG.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 BKTO-INH            pic X(40).
                 09 KREDITINST          pic X(40).
                 09 IBAN                pic X(34).
                 09 BIC                 pic X(11).
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************
