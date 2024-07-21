      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2721.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2721.CPv  $
      *
      *   Rev 2.1   Mar 25 2013 13:10:52   t13ex19
      *AKI01210: Ermitteln Anzahl Kunden/vers. Personen
      *
      *   Rev 2.0   Feb 26 2013 16:52:16   t13ex19
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AKI01210!06.03.13!    ! Neu                       ! B. Stoldt *
      * --------------------------------------------------------------*
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      *****************************************************************
      *
      * Ausgabe f. Auswertung versicherte Personen
      *
      * Satzlänge  103
      *
       01 DRKI2721.
           05  VP-JAHR-MONAT                     PIC 9(06).
      * Auswertungsdatum in der Form JJJJMM
           05  VP-VBNUMMER                       PIC 9(07).
           05  VP-GESELLSCHID                    PIC 9(02).
           05  VP-PRODUKTID                      PIC 9(04).
           05  VP-NAME                           PIC X(60).
           05  VP-GEBDATUM                       PIC X(10).
           05  VP-PERSONENID                     PIC X(13).
      *  nur gefüllt wenn Kunde
           05  VP-STATUS                         PIC X(01).
      *  'K' für Kunde
      *  'A' für Altkunde
      *  'U' für unbekannt
      *----------------Ende DRKI2721 ----------------
