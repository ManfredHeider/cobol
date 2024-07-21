      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.07.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1102.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA1102.CPv  $
      *
      *   Rev 2.3   Jun 08 2020 14:24:16   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.2   Nov 30 2015 09:52:50   t13ac20
      *AMA00024: SEPA-Nacharbeiten - Copies/Programme Datenmodell
      *
      *   Rev 2.1   Jul 22 2013 12:17:16   p13sp14
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Jul 22 2013 12:14:06   p13sp14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************
      *
      *     MITCOPY: TABELLE OVBANKVERBINDUNG00
      *
      *     SOURCE-NAME      :  DRMA1102
      *
      *     GENERIERUNG      :  19.05.2020/12:01:15
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00024!19.11.15!2.2  ! BKTO-NR UND BLZ ENTFERNT  ! WOLFF
      * AMA00476!05.05.20!2.3  ! ERWEITERN BKTO-INH UND    ! T13AC18
      *    MA577!        !     ! KREDITINST AUF 40 STELLEN !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 BANKVERBINDUNG.
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 VBNR                   pic S9(9)      binary.
              07 HIST-KZN               pic X.
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 BKTO-INH               pic X(40).
              07 KREDITINST             pic X(40).
              07 IBAN                   pic X(34).
              07 BIC                    pic X(11).
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************
