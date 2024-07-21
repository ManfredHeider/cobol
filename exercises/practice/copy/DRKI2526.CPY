      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2526.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2526.CPv  $
      *
      *   Rev 2.1   Sep 18 2012 18:21:48   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung-Kundensta
      *tus und Anzahl Betreuer bzw. NEUKUNDE
      *
      *   Rev 2.0   Aug 25 2011 10:58:58   t13ex27
      *Archiverstellung
      *
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
       01  PI-DRKI2526.
           05  PI-DATEN.

               10  PI-DRKI2526-VERVERTRAGSID          PIC  X(013).
               10  PI-DRKI2526-KNDPERSONENID          PIC  X(013).
               10  PI-DRKI2526-KVBVBPERSONENID        PIC  X(013).
               10  PI-DRKI2526-KNDSTATUSID            PIC  X(001).

               10  PI-DRKI2526-PGMNAME                PIC  X(008).
               10  PI-DRKI2526-NKDEREIGNISKZ          PIC S9(004) COMP.
               10  PI-DRKI2526-KNDNUMMER              PIC S9(009) COMP.
               10  PI-DRKI2526-VERPARTGESELLSCHID     PIC S9(004) COMP.
               10  PI-DRKI2526-VERVERTRAGSNUMMER      PIC  X(015).
               10  PI-DRKI2526-NKDTAGESDATUM          PIC  X(010).
      *
      *  PI-DRKI2526-NKDVBNR-JA VBNR und Direktion in NEUKUNDE
      *        Kundenaktualisierung
      *        Kundenzusammenführung
      *
      *  PI-DRKI2526-NKDVBNR-NEIN keine VBNR und Direktion in NEUKUNDE
      *        Vertragsbereinigung
      *        Kundenstatuskorrektur
      *        Mandantenwechsel
      *
               10  PI-DRKI2526-NKDVBNR                PIC  X(001).
                   88   PI-DRKI2526-NKDVBNR-JA       VALUE 'J'.
                   88   PI-DRKI2526-NKDVBNR-NEIN     VALUE 'N'.


       01  PO-DRKI2526.
           05  PO-DRKI2526-RC               PIC  X(003).
               88 PO-DRKI2526-RC-OK              VALUE 'OK '.
               88 PO-DRKI2526-RC-NOK             VALUE 'NOK'.

           05  PO-DRKI2526-RC-TICKET        PIC  S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2526-FEHLERSATZ.
               10 PO-DRKI2526-FUNKTION           PIC  X(10).
               10 PO-DRKI2526-TABELLE            PIC  X(20).
               10 PO-DRKI2526-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2526-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2526-KEY.
                  15 PO-DRKI2526-GES             PIC  9(002).
                  15 PO-DRKI2526-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
