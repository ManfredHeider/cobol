      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.07.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKW0010.CPv  $
      *
      *   Rev 2.1   Nov 11 2010 11:15:34   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *
      *   Rev 2.0   Jul 14 2010 13:58:06   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Eingabedatei DVAG Wien SUH-Daten mit Vorsatz            *
      *                                                               *
      *    SATZLÄNGE   400                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!12.08.10! 01 ! Neu                       ! Misgeld  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0010.

           05  DVW-SUH-VORSATZ.
      *
               10 DVW-SUH-V-SATZART                   PIC  X.
      *           'A'
               10 DVW-SUH-ABSENDER                    PIC  X(005).
      *           'DWIEN'
               10 DVW-SUH-DTADATUM                    PIC  9(008).
      *           JJJJMMTT  (alle DTAs von einem Tag müssen in
      *           diesem Feld den gleichen Wert haben)
               10 DVW-SUH-EMPFAENGER                  PIC  X(004).
      *           'DVAG'
               10 DVW-SUH-DATENTEXT                   PIC  X(025).
      *           'SACH'
               10 DVW-SUH-VERABKZ                     PIC  X(004).
      *           'INIT'
               10 FILLER                              PIC  X(353).


           05  DVW-SUH-DATEN REDEFINES DVW-SUH-VORSATZ.
      *
               10 DVW-SUH-SATZART                     PIC X(001).
      *           'D'
               10 DVW-SUH-KEY.
                  15 DVW-SUH-GESELLSCHAFT             PIC 9(002).
                  15 DVW-SUH-VERTRAGSNUMMER           PIC 9(015).
                  15 DVW-SUH-LFDNR                    PIC 9(002).
      *           Subnummer (eindeutige laufende Nummer)
      *
               10 DVW-SUH-SPARTE                      PIC 9(002).
      *           KTY = 55 / KNR 2
               10 DVW-SUH-BUENDELBEZEICHNUNG          PIC X(040).

               10 DVW-SUH-LETZTE-AEND                 PIC 9(008).
      *           JJJJMMTT
               10 DVW-SUH-AEND-GRUND                  PIC X(050).

               10 DVW-SUH-UNFALLRENTE                 PIC 9(011)V99.
      *
               10 DVW-SUH-RENTENART                   PIC X(030).
      *           KTY = 4451 /KNR 2
               10 DVW-SUH-UNFKAPINVPROZ               PIC 9(003).
      *
               10 DVW-SUH-UNFKAPINVSUM                PIC 9(011)V99.
      *
               10 DVW-SUH-UNFALLTODPR                 PIC 9(011)V99.
      *           Prämie für Unfalltod
               10 DVW-SUH-VERSSUMME                   PIC 9(011)V99.
      *
               10 DVW-SUH-TAGEGELD-BETRAG             PIC 9(011)V99.
      *
               10 DVW-SUH-TAGEGELD-TT                 PIC 9(003).
      *           Tagegeld nach x-Tagen
               10 DVW-SUH-UNFKAP-VARIANTE             PIC 9(002).
      *           KTY = NEU /KNR 2
               10 DVW-SUH-KFZ-KENNZEICHEN             PIC X(012).
      *
               10 DVW-SUH-GESAMTBEITRAG               PIC 9(011)V99.
      *           Jahresbruttoprämie
               10 DVW-SUH-RATENBEITRAG                PIC 9(011)V99.
      *           Bruttoprämie lt. ZW
               10 DVW-SUH-NETTOGESBEITRAG             PIC 9(011)V99.
      *           Jahresnettoprämie

               10 FILLER                              PIC X(126).

           05  DVW-SUH-NACHSATZ REDEFINES DVW-SUH-VORSATZ.
      *
               10 DVW-SUH-N-SATZART                   PIC X(001).
      *           'E'
               10 DVW-SUH-ANZ                         PIC 9(009).
      *           Satzanzahl incl. Vor- und Nachsatz
               10 FILLER                              PIC X(390).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0010                       *
      ******************************************************************

