      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0020.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKW0020.CPv  $
      *
      *   Rev 2.2   Nov 11 2010 11:55:12   t13ex14
      *AKI00966: DVAG Österreich: Sach-DTA-Aktualisierung
      *
      *   Rev 2.1   Sep 21 2010 14:46:58   t13ex14
      *AKI00966: DVAG Österreich: Sach-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 19 2010 15:43:42   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aktualisierungsdatei DVAG Wien f Tab. SACHWIEN          *
      *                                                               *
      *    SATZLÄNGE   350                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!20.08.10! 01 ! Neu                       ! Misgeld  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0020.

           05  SWI-AKTUAL-KZ                  PIC  X(001).
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE

           05  SWI-GES                        PIC  9(002).
           05  SWI-VNR                        PIC  X(015).
           05  SWI-VERTRAGSID                 PIC  X(013).
           05  SWI-LFDNR                      PIC  9(002).
      *        Subnummer (eindeutige laufende Nummer)
           05  SWI-HOSTID                     PIC  X(015).
      *        enthält den gleichen Wert wie LFDNR mit führenden
      *        Nullen auf 4 Stellen linksbündig aufgefüllt

      *        Die Produktid ist zwar kein Attribut in der Tabelle
      *        SACHWIEN, sie wird trotzdem an die Aktualisierung
      *        übergeben, um dort die Verarbeitung der Unfall-Kap-
      *        Variante zu steuern.
           05  SWI-PRODUKTID                  PIC 9(003).

           05 SWI-BUENDELBEZEICHNUNG          PIC X(040).
           05 SWI-BUENDELBEZ-AENDKZ           PIC X(001).

           05 SWI-UNFALLRENTE                 PIC 9(011)V99.
           05 SWI-UNFALLRENTE-AENDKZ          PIC X(001).
      *
           05 SWI-RENTENART                   PIC X(030).
      *       KTY = 4451 /KNR 2
           05 SWI-RENTENART-AENDKZ            PIC X(001).

           05 SWI-UNFKAPINVPROZ               PIC 9(003).
           05 SWI-UNFKAPINVPROZ-AENDKZ        PIC X(001).
      *
           05 SWI-UNFKAPINVSUM                PIC 9(011)V99.
           05 SWI-UNFKAPINVSUM-AENDKZ         PIC X(001).
      *
           05 SWI-UNFALLTODPR                 PIC 9(011)V99.
      *       Prämie für Unfalltod
           05 SWI-UNFALLTODPR-AENDKZ          PIC X(001).

           05 SWI-VERSSUMME                   PIC 9(011)V99.
           05 SWI-VERSSUMME-AENDKZ            PIC X(001).
      *
           05 SWI-TAGEGELD-BETRAG             PIC 9(011)V99.
           05 SWI-TAGEGELD-AENDKZ             PIC X(001).
      *
           05 SWI-TAGEGELD-TT                 PIC 9(003).
      *       Tagegeld nach x-Tagen
           05 SWI-TAGEGELD-TT-AENDKZ          PIC X(001).

           05 SWI-UNFKAP-VARIANTE             PIC 9(002).
      *       KTY = 78 /KNR 2
           05 SWI-UNFKAP-VARIAN-AENDKZ        PIC X(001).

           05 SWI-KFZ-KENNZEICHEN             PIC X(012).
           05 SWI-KFZ-KENNZEICH-AENDKZ        PIC X(001).
      *
           05 SWI-GESAMTBEITRAG               PIC 9(011)V99.
      *       Jahresbruttoprämie
           05 SWI-GESAMTBEITRAG-AENDKZ        PIC X(001).

           05 SWI-RATENBEITRAG                PIC 9(011)V99.
      *       Bruttoprämie lt. ZW
           05 SWI-RATENBEITRAG-AENDKZ         PIC X(001).

           05 SWI-NETTOGESBEITRAG             PIC 9(011)V99.
      *       Jahresnettoprämie
           05 SWI-NETTOGESBEITR-AENDKZ        PIC X(001).

           05 FILLER                          PIC X(091).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0020                       *
      ******************************************************************
