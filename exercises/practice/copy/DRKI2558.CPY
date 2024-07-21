      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2558.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2558.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:14:02   T13ex52
      *AKI00886: NKDA-Loeschen und Umhängen Freistellungsauftrag
      *
      *   Rev 2.0   Feb 03 2012 12:30:46   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      * Umhängen(Delete alte Personen-ID Insert mit neuer Personen-ID) *
      * in der Tabelle TCKI0405 FSAEIGENVERTRAEGE)                     *
      *                                                                *
      ******************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * AKI00886!03.02.12! 2.1! NEU                       !H. Schlenther
      *    UE026!        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  PI-DRKI2558.
           05  PI-DATEN.

               10  PI-DRKI2558-VERVERTRAGSID         PIC  X(013).
               10  PI-DRKI2558-PERSONENID-ALT        PIC  X(013).
               10  PI-DRKI2558-PERSONENID-NEU        PIC  X(013).
      *
       01  PO-DRKI2558.
           05  PO-DRKI2558-RC               PIC  X(003).
               88 PO-DRKI2558-RC-OK              VALUE 'OK '.
               88 PO-DRKI2558-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2558-RC-NFD             VALUE 'NFD'.

           05  PO-DATEN.
               10  PO-DRKI2558-PERSONENID          PIC X(013).
               10  PO-DRKI2558-RC-TICKET           PIC S9(004) COMP.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2558-FEHLERSATZ.
               10 PO-DRKI2558-FUNKTION           PIC  X(10).
               10 PO-DRKI2558-TABELLE            PIC  X(20).
               10 PO-DRKI2558-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2558-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2558-KEY.
                  15 PO-DRKI2558-GES             PIC  9(002).
                  15 PO-DRKI2558-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER **************************************
