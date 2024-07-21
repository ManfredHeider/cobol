      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA2183.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA2183.CPv  $
      *
      *   Rev 2.3   Jun 08 2020 14:28:58   t13ac18
      *AMA00476: Erweiterung der Stellenanzahl für Anzeige Kreditin
      *stitut/ Hauptstelle in den Abrechnungsunterlagen - Copies
      *
      *   Rev 2.2   Nov 30 2015 12:18:24   t13ac20
      *AMA00024: SEPA-Nacharbeiten - Copies/Programme ZPA-Umstellung
      *
      *   Rev 2.1   Jul 01 2013 11:29:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MA-SÄTZE FÜR ZPA
      *
      *     SOURCE-NAME      :  DRMA2183
      *
      *     GENERIERUNG      :  19.05.2020/12:31:03
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00024!20.11.15!2.2  ! ÜBERARBEITET UND NUR NOCH ! WOLFF
      *         !        !     ! BENÖTIGTE FELDER BLEIBEN  !
      *         !        !     ! IM SATZ                   !
      * AMA00476!05.05.20!2.3  ! ERWEITERN BKTO-INH AUF    ! T13AC18
      *    MA577!        !     ! 40 STELLEN UND LÖSCHEN    !
      *         !        !     ! KREDITINST                !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA2183.
              07 VBNR                   pic X(10).
              07 ABRDATUM-TTMMJJJJ      pic 9(8).
              07 IBAN                   pic X(34).
              07 BIC                    pic X(11).
              07 BKTO-INH               pic X(40).
              07 UEBERW                 pic S9(9)V99   packed-decimal.
      *
      **************************************************************
