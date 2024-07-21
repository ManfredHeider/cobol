      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2525.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2525.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:28:50   t13ex27
      *AKI00886: Host - neues Design Kundenaktualisierung: VN-Wechsel
      *
      *   Rev 2.0   Aug 25 2011 10:58:48   t13ex27
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
       01  PI-DRKI2525.
           05  PI-DATEN.


               10  PI-DRKI2525-VERVERTRAGSID          PIC X(013).
               10  PI-DRKI2525-KNDPERSONENID          PIC X(013).
               10  PI-DRKI2525-VRBPERSONENID-V        PIC X(013).
               10  PI-DRKI2525-VRBPABADRESSEID        PIC X(013).

               10  PI-DRKI2525-VERPARTGESELLSCHID     PIC  9(002).
               10  PI-DRKI2525-VERVERTRAGSNUMMER      PIC  9(015).
               10  PI-DRKI2525-VERVERTRZUSTANDID      PIC S9(004) COMP.

               10  PI-DRKI2525-VERVERTRAGSBEGINN      PIC  X(010).
               10  PI-DRKI2525-VERDATVERTRAEND        PIC  X(010).
               10  PI-DRKI2525-VERMANDANTNR           PIC S9(009) COMP.

               10  PI-DRKI2525-KNDGEANDERTPGM         PIC  X(008).
               10  PI-DRKI2525-KNDNEUKUNDE            PIC  X(010).

       01  PO-DRKI2525.
           05  PO-DRKI2525-RC               PIC  X(003).
               88 PO-DRKI2525-RC-OK              VALUE 'OK '.
               88 PO-DRKI2525-RC-NOK             VALUE 'NOK'.

           05  PO-DATEN.
               10  PO-DRKI2525-KNDPERSONENID          PIC X(013).

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2525-FEHLERSATZ.
               10 PO-DRKI2525-FUNKTION           PIC  X(10).
               10 PO-DRKI2525-TABELLE            PIC  X(20).
               10 PO-DRKI2525-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2525-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2525-KEY.
                  15 PO-DRKI2525-GES             PIC  9(002).
                  15 PO-DRKI2525-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
