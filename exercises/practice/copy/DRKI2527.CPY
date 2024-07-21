      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2527.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2527.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 18:21:58   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Kundensta
      *tus und Anzahl Betreuer bzw. NEUKUNDE
      *
      *   Rev 2.0   Aug 25 2011 10:59:06   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00886!23.08.11! 2.1! NEU                       !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI2527.
           05  PI-DATEN.

               10  PI-DRKI2527-KNDPERSONENID          PIC X(013).
               10  PI-DRKI2527-KVBVBPERSONENID        PIC X(013).

               10  PI-DRKI2527-PGMNAME                PIC X(008).
               10  PI-DRKI2527-NKDEREIGNISKZ          PIC S9(004) COMP.
               10  PI-DRKI2527-VERPARTGESELLSCHID     PIC S9(004) COMP.
               10  PI-DRKI2527-VERVERTRAGSNUMMER      PIC X(015).
               10  PI-DRKI2527-NKDTAGESDATUM          PIC X(010).

       01  PO-DRKI2527.
           05  PO-DRKI2527-RC               PIC  X(003).
               88 PO-DRKI2527-RC-OK              VALUE 'OK '.
               88 PO-DRKI2527-RC-NOK             VALUE 'NOK'.

           05  PO-DRKI2527-RC-TICKET        PIC  S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2527-FEHLERSATZ.
               10 PO-DRKI2527-FUNKTION           PIC  X(10).
               10 PO-DRKI2527-TABELLE            PIC  X(20).
               10 PO-DRKI2527-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2527-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2527-KEY.
                  15 PO-DRKI2527-GES             PIC  9(002).
                  15 PO-DRKI2527-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
