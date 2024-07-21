      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0014.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVD0014.CPv  $
      *
      *   Rev 2.4   Jun 05 2012 11:08:14   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.3   Jun 05 2012 09:48:16   t13ex19
      *AKI01124: Erweiterung VBI-SST STVD0060
      *
      *   Rev 2.2   Jul 22 2010 20:02:56   t13ex51
      *AKI00914: Anpassen VBI-Schnittstelle STVD0060 neues Feld KI_
      *B_V_BETREUUNGS_KZ
      *
      *   Rev 2.1   Nov 23 2004 16:48:50   t13ex49
      *AKI00422: VBI-DWH Schnittstelle KI-Kunden- und Vertragsbestand
      *
      *   Rev 2.0   Aug 25 2004 16:28:24   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI00422!25.08.04!    ! Neu                       ! P. Pohl   *
      * --------------------------------------------------------------*
      * AKI00914!20.07.10!    ! Neues Feld in DRVD0014    ! HRO       *
      *         !        !    ! KI-B-V-BETREUUNGS-KZ      !           *
      *         !        !    !                           !           *
      * AKI01124!08.03.12!    ! Neue Felder KI-B-V-ABL-DAT! BST       *
      *         !        !    ! KI-B-P-BTG-GEMZW          !           *
      *         !        !    ! KI-B-V-ZAHLWEISE          !           *
      *****************************************************************
      *
      * Loadfile für VBI-Schnittstelle STVD0060
      *
      * Satzlänge 178 Byte
      *
       01 DRVD0014.

      * Erstelldatum der Schnittstelle
           05 KI-B-Jahr-Monat            PIC S9(9) COMP.
      *
      * VB-Nummer des Kundenbetreuers (WUKI0068)
           05 KI-B-VB-KDBETR             PIC S9(9) COMP.
      *
      * VB-Nummer des ersten aktiven Überstellten
           05 KI-B-VB-BETR               PIC S9(9) COMP.
           05 NIL-KI-B-VB-BETR           PIC X(01).
      *
      * KIKundennummer (WUKI0031)
           05 KI-B-KD-NR                 PIC S9(9) COMP.
      *
      * KundenID
           05 KI-B-KD-ID                 PIC X(13).
      *
      * Kundenart DVAG
           05 KI-B-KD-ART-DVAG           PIC X(01).
      *
      * Kundenart VB
           05 KI-B-KD-ART-VB             PIC X(01).
      *
      * Art der Betreuung des Kunden-DVAG
           05 KI-B-KD-BETR-ART-DVAG      PIC X(01).
           05 NIL-KI-B-KD-BETR-ART-DVAG  PIC X(01).
      *
      * Art der Betreuung des Kunden-VB
           05 KI-B-KD-BETR-ART-VB        PIC X(01).
           05 NIL-KI-B-KD-BETR-ART-VB    PIC X(01).
      *
      * Einfach oder Mehrfachbetreuung des Kunden
           05 KI-B-KD-EINF-MEHR-BETR     PIC X(01).
           05 NIL-KI-B-KD-EINF-MEHR-BETR PIC X(01).
      *
      * Kunde-VB-Betreuung
           05 KI-B-KD-VB-BETR            PIC X(01).
           05 NIL-KI-B-KD-VB-BETR        PIC X(01).
      *
      *------------------------------------------------------
      *
      * Produktgebernummer
           05 KI-B-V-PG-NR              PIC S9(4) COMP.
      *
      * Vertragsnummer
           05 KI-B-V-NR                 PIC X(15).
      *
      * Vertrags-ID
           05 KI-B-V-ID                 PIC X(13).
      *
      * Vertragsstatus
           05 KI-B-V-Status             PIC S9(4) COMP.
           05 NIL-KI-B-V-Status         PIC X(01).
      *
      * Art der Betreuung des Vertrags
           05 KI-B-V-BETR-ART           PIC X(01).
      *
      * Datum des Vertragsbeginns
           05 KI-B-V-BEG-DAT            PIC X(10).
           05 NIL-KI-B-V-BEG-DAT        PIC X(01).
      *
      * Dynamikerhöhung
           05 KI-B-V-DYN                PIC X(01).
           05 NIL-KI-B-V-DYN            PIC X(01).
      *
      * Betragsfrei
           05 KI-B-V-BTGFREI            PIC X(01).
           05 NIL-KI-B-V-BTGFREI        PIC X(01).
      *
      * Vermögenswirksam
           05 KI-B-V-VERMW              PIC X(01).
           05 NIL-KI-B-V-VERMW          PIC X(01).
      *
      *-----------------------------------------------------
      *
      * Produktnummer
           05 KI-B-P-NR                   PIC S9(4) COMP.
      *    10 VETPRODUKTID                       PIC S9(4) COMP.
      *    10 NIL-VETPRODUKTID                   PIC X(01).
      *    10 GHPPRODUKTID                       PIC S9(4) COMP.
      *
      * Unter-Nr. Dt.Bank
           05 KI-B-P-UNR                  PIC S9(4) COMP.
           05 NIL-KI-B-P-UNR              PIC X(01).
      *
      * Eigenvermittlung
           05 KI-B-P-EIGEN                PIC X(01).
      *
      * VB-Nummer des Abschlussvermittlers
           05 KI-B-P-AVERM                PIC S9(9) COMP.
           05 NIL-KI-B-P-AVERM            PIC X(01).
      *
      * Anzahl versicherter Personen bei KV / AMV-Unfall
           05 KI-B-P-PERS                 PIC 999.
           05 NIL-KI-B-P-PERS             PIC X(01).
      *
      * Währungsschlüssel-Konstante
           05 KI-B-P-WAE-KONSTANTE        PIC S9(4) COMP.
      *
      * Versicherungssumme/Anlagebetrag
           05 KI-B-P-SUMME                PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-SUMME            PIC X(01).
      *
      * Währung für Versicherungssumme
           05 KI-B-P-SUMME-Waehrung       PIC S9(4) COMP.
           05 NIL-KI-B-P-SUMME-Waehrung   PIC X(01).

      * Versicherungssumme nur aus Dynamikerhöhung
           05 KI-B-P-DYNSUM               PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-DYNSUM           PIC X(01).
      *
      * Währung für Versicherungssumme
           05 KI-B-P-DYNSUM-Waehrung      PIC S9(4) COMP.
           05 NIL-KI-B-P-DYNSUM-Waehrung  PIC X(01).

      * Jahresversicherungsbeitrag - Brutto
           05 KI-B-P-JBTG-Brutto          PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JBTG-Brutto      PIC X(01).
      *
      * Jahresversicherungsbeitrag - Netto 1
           05 KI-B-P-JBTG-Netto1          PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JBTG-Netto1      PIC X(01).
      *
      * Jahresversicherungsbeitrag - Netto 2
           05 KI-B-P-JBTG-Netto2          PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JBTG-Netto2      PIC X(01).

      * Währung für Jahresversicherungsbeitrag
           05 KI-B-P-JBTG-Waehrung        PIC S9(4) COMP.
           05 NIL-KI-B-P-JBTG-Waehrung    PIC X(01).
      *
      * Jahresversicherungsbeitrag nur Dyn-Erhöhung - Brutto
           05 KI-B-P-JDYNBTG-Brutto       PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JDYNBTG-Brutto   PIC X(01).
      *
      * Jahresversicherungsbeitrag nur Dyn-Erhöhung - Netto 1
           05 KI-B-P-JDYNBTG-Netto1       PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JDYNBTG-Netto1   PIC X(01).
      *
      * Jahresversicherungsbeitrag nur Dyn-Erhöhung - Netto 2
           05 KI-B-P-JDYNBTG-Netto2       PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-JDYNBTG-Netto2   PIC X(01).
      *
      * Währung für Jahresversicherungsbeitrag nur Dyn-Erhöhung
           05 KI-B-P-JDYNBTG-Waehrung     PIC S9(4) COMP.
           05 NIL-KI-B-P-JDYNBTG-Waehrung PIC X(01).
<<
200710* Entspricht VRBKOMMBETRKZ aus Tabelle VERTRAGROLLEBEZ
           05 KI-B-V-BETREUUNGS-KZ        PIC X(01).

      * Datum Vertragsablauf
           05 KI-B-V-ABL-DAT              PIC X(10).
           05 NIL-KI-B-V-ABL-DAT          PIC X(01).

      * Beitrag gem. Zahlweise
           05 KI-B-P-BTG-GEMZW            PIC S9(9)V9(2) COMP-3.
           05 NIL-KI-B-P-BTG-GEMZW        PIC X(01).

      * Zahlweise
           05 KI-B-V-ZAHLWEISE            PIC S9(04) COMP.
           05 NIL-KI-B-V-ZAHLWEISE        PIC X(01).

>>         05 FILLER                      PIC X(28).
      *----------- Ende DRVD0014 ---------------------------
