      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.09.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1714.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1714.CPv  $
      *
      *   Rev 2.1   Dec 22 2004 17:08:34   t13ex12
      *FKI00492: Neue Copy für Fehler Akt. Rücknahmepreise
      *
      *   Rev 2.0   Sep 02 2004 11:31:42   t13ex12
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 1 7 1 4    ########                    *
      *              #####################                             *
      *                                                                *
      *     Copy-Strecke für neue Preissätze zu dem es noch kein Fonds *
      *     in der Supporttabelle FOND gibt. Diese Datei wird nur für  *
      *     den Mail wegen den neuen Fonds benutzt anstatt DRKI0561.   *
      *
      *    SATZLÄNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         !NAME     !VERS.*
      *AUFTRAG !                                       !         !     *
      *--------!---------------------------------------!---------!-----*
      *02.09.04!Neu-Erstellung                         !v.d.Goltz! 01  *
      *FKI00492!                                       !         !     *
      * -------------------------------------------------------------*
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI1714.
           05  FEHLER-DATUM               PIC  X(010).
      *                 FEHLER-DATUM IM DB2-FORMAT
      *                 (JHJJ-MM-TT) EINSTELLEN:

           05  FILLER                     PIC  X(001).

           05  FEHLER-PGM-BEZEICHNUNG     PIC  X(024).

           05  FEHLER-PGMNAME             PIC  X(008).

           05  FILLER                     PIC  X(001).

           05  FEHLER-GESELLSCHAFT        PIC  9(002).
      *              Gesellschafts-ID

           05  FILLER                     PIC  X(009).
      *

           05  FEHLER-WKN                 PIC  X(006).
      *              Wertpapiernr.

           05  FILLER                     PIC  X(007).

           05  FEHLER-FEHLERNUMMER        PIC  X(004).

           05  FILLER                     PIC  X(004).

           05  FEHLER-FEHLERTEXT          PIC  X(045).

           05  FILLER                     PIC  X(004).

           05  FEHLER-ISIN                PIC  X(012).
      *              ISIN-Nr.
      *
           05  FILLER                     PIC  X(013).

      ******************************************************************
      *          E n d e    Copy-Buch   DRKI1714                       *
      ******************************************************************

