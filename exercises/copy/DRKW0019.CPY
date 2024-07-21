      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0019.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKW0019.CPv  $
      *
      *   Rev 2.1   Nov 11 2010 11:55:04   t13ex14
      *AKI00966: DVAG Österreich: Sach-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 19 2010 15:43:26   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Aufbereitungsbestand DVAG Wien SUH-Daten                *
      *                                                               *
      *    SATZLÄNGE   410                                            *
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
       01  DRKW0019.

           05 AUFB-SUH-VERARB-KZ                   PIC X(001).
      *
           05 AUFB-SUH-KEY.
              10 AUFB-SUH-GESELLSCHAFT             PIC 9(002).
              10 AUFB-SUH-VERTRAGSNUMMER           PIC 9(015).
              10 AUFB-SUH-LFDNR                    PIC 9(002).
      *       Subnummer (eindeutige laufende Nummer)

           05 AUFB-SUH-SPARTE                      PIC 9(002).
      *       KTY = 55 / KNR 2
           05 AUFB-SUH-SPARTE-AEND-KZ              PIC X(001).

           05 AUFB-SUH-BUENDELBEZEICHNUNG          PIC X(040).
           05 AUFB-SUH-BUENDELBEZ-AEND-KZ          PIC X(001).

           05 AUFB-SUH-LETZTE-AEND                 PIC 9(008).
      *       JJJJMMTT
           05 AUFB-SUH-LETZTE-AEND-AEND-KZ         PIC X(001).

           05 AUFB-SUH-AEND-GRUND                  PIC X(050).
           05 AUFB-SUH-AEND-GRUND-AEND-KZ          PIC X(001).

           05 AUFB-SUH-UNFALLRENTE                 PIC 9(011)V99.
           05 AUFB-SUH-UNFALLRENTE-AEND-KZ         PIC X(001).
      *
           05 AUFB-SUH-RENTENART                   PIC X(030).
      *       KTY = 4451 /KNR 2
           05 AUFB-SUH-RENTENART-AEND-KZ           PIC X(001).

           05 AUFB-SUH-UNFKAPINVPROZ               PIC 9(003).
           05 AUFB-SUH-UNFKAPINVPROZ-AEND-KZ       PIC X(001).
      *
           05 AUFB-SUH-UNFKAPINVSUM                PIC 9(011)V99.
           05 AUFB-SUH-UNFKAPINVSUM-AEND-KZ        PIC X(001).
      *
           05 AUFB-SUH-UNFALLTODPR                 PIC 9(011)V99.
      *       Prämie für Unfalltod
           05 AUFB-SUH-UNFALLTODPR-AEND-KZ         PIC X(001).

           05 AUFB-SUH-VERSSUMME                   PIC 9(011)V99.
           05 AUFB-SUH-VERSSUMME-AEND-KZ           PIC X(001).
      *
           05 AUFB-SUH-TAGEGELD-BETRAG             PIC 9(011)V99.
           05 AUFB-SUH-TAGEGELD-AEND-KZ            PIC X(001).
      *
           05 AUFB-SUH-TAGEGELD-TT                 PIC 9(003).
      *       Tagegeld nach x-Tagen
           05 AUFB-SUH-TAGEGELD-TT-AEND-KZ         PIC X(001).

           05 AUFB-SUH-UNFKAP-VARIANTE             PIC 9(002).
      *       KTY = NEU /KNR 2
           05 AUFB-SUH-UNFKAP-VARIAN-AEND-KZ       PIC X(001).

           05 AUFB-SUH-KFZ-KENNZEICHEN             PIC X(012).
           05 AUFB-SUH-KFZ-KENNZEICH-AEND-KZ       PIC X(001).
      *
           05 AUFB-SUH-GESAMTBEITRAG               PIC 9(011)V99.
      *       Jahresbruttoprämie
           05 AUFB-SUH-GESAMTBEITRAG-AEND-KZ       PIC X(001).

           05 AUFB-SUH-RATENBEITRAG                PIC 9(011)V99.
      *       Bruttoprämie lt. ZW
           05 AUFB-SUH-RATENBEITRAG-AEND-KZ        PIC X(001).

           05 AUFB-SUH-NETTOGESBEITRAG             PIC 9(011)V99.
      *       Jahresnettoprämie
           05 AUFB-SUH-NETTOGESBEITR-AEND-KZ       PIC X(001).

           05 FILLER                               PIC X(119).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0019                       *
      ******************************************************************
