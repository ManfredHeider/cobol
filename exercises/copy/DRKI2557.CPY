      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.02.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2557.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2557.CPv  $
      *
      *   Rev 2.1   Sep 20 2012 10:13:54   T13ex52
      *AKI00886: NKDA-Loeschen und Umhängen Freistellungsauftrag
      *
      *   Rev 2.0   Feb 03 2012 11:38:24   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      * Löschen mit vorangehenden Prüfen auf Vorhandensein des zu      *
      * loeschenden Eintrags in der Tabelle TCKI0405 FSAEIGENVERTRAEGE)*
      *                                                                *
      ******************************************************************
      *    VERSION
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      * ---------------------------------------------------------------*
      * AKI00886!06.02.12! 2.1! NEU                       !H.Schlenther*
      *    UE026!        !    !                           !            *
      *         !        !    !                           !            *
      *         !        !    !                           !            *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  PI-DRKI2557.
           05  PI-DATEN.

               10  PI-DRKI2557-VERVERTRAGSID         PIC  X(013).
      * VN zu diesem Vertrag
               10  PI-DRKI2557-PERSONENID            PIC  X(013).
      *
       01  PO-DRKI2557.
           05  PO-DRKI2557-RC               PIC  X(003).
               88 PO-DRKI2557-RC-OK              VALUE 'OK '.
               88 PO-DRKI2557-RC-NOK             VALUE 'NOK'.
               88 PO-DRKI2557-RC-NFD             VALUE 'NFD'.

      *    für Log-Datei des Hauptprogramms:
           05  PO-DRKI2557-FEHLERSATZ.
               10 PO-DRKI2557-FUNKTION           PIC  X(10).
               10 PO-DRKI2557-TABELLE            PIC  X(20).
               10 PO-DRKI2557-SQL-FEHLER         PIC  X(04).
               10 PO-DRKI2557-KEY                PIC  X(40).
               10 FILLER REDEFINES PO-DRKI2557-KEY.
                  15 PO-DRKI2557-GES             PIC  9(002).
                  15 PO-DRKI2557-VNR             PIC  X(015).
      *
      ******* ENDE COPY-MEMBER *****************************************
