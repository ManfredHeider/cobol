      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.06.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1920.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI1920.CPv  $
      *
      *   Rev 2.2   Sep 12 2016 10:30:22   t13ex27
      *AKI01420: Option E* für KdBV - Umsetzung
      *
      *   Rev 2.1   Sep 14 2006 10:49:18   t13ex48
      *AKI00531: Kundenbetreuungsvereinbarung PGM für KÜ
      *
      *   Rev 2.0   Jun 26 2006 08:28:46   T13EX48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI1920                                     *
      *    Vorlaufkarte fuer PGM zur Kundenuebertragung aus KdBV     *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00531!16.06.06!1.0 ! neu                       ! Sebbach  *
      * AKI01420!03.08.16!2.2 ! Kundennummer entfernt     !I. Frenzel*
      *         !        !    ! EVM-KZ eingefügt          !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1920-SATZ.
           05  DRKI1920-SA                  PIC 9(002).
           05  DRKI1920-SA00.
            10 DRKI1920-SA00-DATUM          PIC X(008).
            10 Filler                       PIC X(070).
           05  DRKI1920-SA01 REDEFINES DRKI1920-SA00.
      *  VB-Nummer des VB, dessen Kunden zu uebertragen sind
            10 DRKI1920-SA01-VB-abg         PIC 9(007).
      *  VB-Nummer des VB auf den die Kunden zu uebertragen sind
            10 DRKI1920-SA01-VB-ueb         PIC 9(007).
      *  Fachliches Gueltigkeitsdatum der Kundenuebertragung
            10 DRKI1920-SA01-Datum.
             15 DRKI1920-SA01-Datum-JJJJ    PIC 9(004).
             15 DRKI1920-SA01-Datum-MM      PIC 9(002).
             15 DRKI1920-SA01-Datum-TT      PIC 9(002).
030816      10 DRKI1920-SA01-EIGEN-VERM-KZ  PIC X(001).
            10 Filler                       PIC X(055).
      *    05  DRKI1920-SA02 REDEFINES DRKI1920-SA00.
      *     10 DRKI1920-SA02-VB-abg         PIC 9(007).
      *     10 DRKI1920-SA02-KDNR           PIC 9(009).
      *     10 Filler                       PIC X(062).
           05  DRKI1920-SA99 REDEFINES DRKI1920-SA00.
            10 DRKI1920-SA99-DATUM          PIC 9(008).
      *  Anzahl der Vorlaufkarten
            10 DRKI1920-Anz-Saetze          PIC 9(005).
            10 Filler                       PIC X(065).

