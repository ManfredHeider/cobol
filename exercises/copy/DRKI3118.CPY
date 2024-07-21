      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3118.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3118.CPv  $
      *
      *   Rev 2.1   Mar 13 2018 09:54:22   t13ex27
      *AKI01574: Tuning für Frischer Wind
      *
      *   Rev 2.0   Jan 31 2017 16:16:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01574!09.03.18! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI3118.
           05  PI-DATEN.

               10  PI-DRKI3118-KNDPERSONENID          PIC X(013).
               10  PI-DRKI3118-KVBVBPERSONENID        PIC X(013).

               10  PI-DRKI3118-PGMNAME                PIC X(008).
               10  PI-DRKI3118-NKDEREIGNISKZ          PIC S9(004) COMP.
               10  PI-DRKI3118-VERPARTGESELLSCHID     PIC S9(004) COMP.
               10  PI-DRKI3118-VERVERTRAGSNUMMER      PIC X(015).
               10  PI-DRKI3118-NKDTAGESDATUM          PIC X(010).

       01  PO-DRKI3118.
           05  PO-DRKI3118-RC               PIC  X(003).
               88 PO-DRKI3118-RC-OK              VALUE 'OK '.
               88 PO-DRKI3118-RC-NOK             VALUE 'NOK'.

           05  PO-DRKI3118-RC-TICKET        PIC  S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI3118-FEHLERSATZ.
               10 PO-DRKI3118-FUNKTION           PIC  X(10).
               10 PO-DRKI3118-TABELLE            PIC  X(20).
               10 PO-DRKI3118-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI3118-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI3118-KEY.
                  15 PO-DRKI3118-GES             PIC  9(002).
                  15 PO-DRKI3118-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************

