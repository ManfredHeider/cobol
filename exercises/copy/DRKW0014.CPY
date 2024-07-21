      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKW0014.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKW0014.CPv  $
      *
      *   Rev 2.1   Nov 11 2010 11:54:58   t13ex14
      *AKI00966: DVAG Österreich: Sach-DTA-Aktualisierung
      *
      *   Rev 2.0   Aug 12 2010 14:51:24   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *                                                               *
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   *
      *     - Komplettbestand KSDS DVAG Wien SUH-Daten                *
      *                                                               *
      *    SATZLÄNGE   440                                            *
      *                                                               *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00966!13.08.10! 01 ! Neu                       ! Misgeld  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKW0014.

           05  KOMPL-SUH-AEND                           PIC  9(002).

           05  KOMPL-SUH-DATEN.
      *
               10 KOMPL-SUH-SATZART                     PIC X(001).
      *           'D'
               10 KOMPL-SUH-KEY.
                  15 KOMPL-SUH-GESELLSCHAFT             PIC 9(002).
                  15 KOMPL-SUH-VERTRAGSNUMMER           PIC 9(015).
                  15 KOMPL-SUH-LFDNR                    PIC 9(002).
      *           Subnummer (eindeutige laufende Nummer)
      *
               10 KOMPL-SUH-SPARTE                      PIC 9(002).
      *           KTY = 55 / KNR 2
               10 KOMPL-SUH-BUENDELBEZEICHNUNG          PIC X(040).

               10 KOMPL-SUH-LETZTE-AEND                 PIC 9(008).
      *           JJJJMMTT
               10 KOMPL-SUH-AEND-GRUND                  PIC X(050).

               10 KOMPL-SUH-UNFALLRENTE                 PIC 9(011)V99.
      *
               10 KOMPL-SUH-RENTENART                   PIC X(030).
      *           KTY = 4451 /KNR 2
               10 KOMPL-SUH-UNFKAPINVPROZ               PIC 9(003).
      *
               10 KOMPL-SUH-UNFKAPINVSUM                PIC 9(011)V99.
      *
               10 KOMPL-SUH-UNFALLTODPR                 PIC 9(011)V99.
      *           Prämie für Unfalltod
               10 KOMPL-SUH-VERSSUMME                   PIC 9(011)V99.
      *
               10 KOMPL-SUH-TAGEGELD-BETRAG             PIC 9(011)V99.
      *
               10 KOMPL-SUH-TAGEGELD-TT                 PIC 9(003).
      *           Tagegeld nach x-Tagen
               10 KOMPL-SUH-UNFKAP-VARIANTE             PIC 9(002).
      *           KTY = NEU /KNR 2
               10 KOMPL-SUH-KFZ-KENNZEICHEN             PIC X(012).
      *
               10 KOMPL-SUH-GESAMTBEITRAG               PIC 9(011)V99.
      *           Jahresbruttoprämie
               10 KOMPL-SUH-RATENBEITRAG                PIC 9(011)V99.
      *           Bruttoprämie lt. ZW
               10 KOMPL-SUH-NETTOGESBEITRAG             PIC 9(011)V99.
      *           Jahresnettoprämie

               10 FILLER-1                              PIC X(126).

             05 KOMPL-SUH-TIMESTAMP                     PIC X(026).
      *         JJJJ-MM-TT-hh.mm.ss.abcdef
             05 FILLER-2                                PIC X(012).

      ******************************************************************
      *          E n d e    Copy-Member DRKW0014                       *
      ******************************************************************
