      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  20.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1707.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI1707.CPv  $
      *
      *   Rev 2.1   Apr 26 2006 16:05:06   t13ex27
      *AKI00520: Adressqualifizierung für das VB-Magazin
      *
      *   Rev 2.0   Jul 20 2004 11:00:32   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR Rücklaufdaten VB-Magazin Deutsche Post      *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    Länge =  587 Bytes                                          *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00520!10.11.05!01  ! NEU                       !I.Frenzel *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI1707.
      *
           10  DP-VORSATZ.
               20  DP-V-BETREFF       PIC X(004).
      *            BETREFF = 'ADRU'
               20  DP-V-ABSENDER      PIC X(020).
      *            ABSENDER = 'Post Direct'
               20  FILLER             PIC X(020).
      *
               20  DP-V-ANLASS        PIC X(030).
      *            Versandanlass = 'ADRU001V2'
               20  DP-V-TESTSCHALTER  PIC X(001).
      *            TESTSCHALTER 'T' oder ' '
               20  DP-V-UMFANG        PIC X(001).
      *            Nachrichtenumfang
      *            1 - nur Mitteilung
      *            2 - nur Anhang
      *            3 - Mitteilung und Anhang
               20  DP-V-DATUM.
      *            Datum der Übertragung JJMMTTHHMM
                   25 DP-V-DATUM-DATE PIC X(006).
                   25 DP-V-DATUM-TIME PIC X(004).
               20  DP-V-ANZAHL        PIC 9(008).
      *            Anzahl Datensätze (ohne Vorsatz)
               20  DP-V-CODE          PIC X(001).
      *            Schalter 'A'SCII oder 'E'BCDIC
               20  DP-V-FORMAT        PIC X(001).
      *            Satzformat F, T, U oder V
               20  DP-V-LAENGE        PIC 9(004).
      *            Satzlänge 0587
               20  FILLER             PIC X(028).
               20  FILLER             PIC X(459).
      *
           10  DP-SATZ REDEFINES DP-VORSATZ.

               20  LFDNR              PIC 9(006).
      *            Laufende Nummer
               20  VKZNR              PIC X(006).
      *
               20  KDNR.
      *            Kundennummer
                   25 VBNR            PIC X(007).
                   25 TZ              PIC X(001).
                   25 KUNDENNR        PIC X(017).
               20  FILLER             PIC X(001).
      *
               20  HEFTNR             PIC X(003).
      *            Ausgabe
               20  FILLER             PIC X(001).
               20  KENNZIFFER         PIC 9(002).
      *            Kennung der Unzustellbarkeitsziffer
               20  ADRESS-LABEL.
      *            Angaben des Adressetikettes
                   25  L-VORNAME      PIC X(030).
      *                Vorname
                   25  L-NACHNAME     PIC X(030).
      *                Nachname
                   25  L-ZUSATZ       PIC X(030).
      *                Zusatzfeld
                   25  L-STRASSE      PIC X(022).
      *                Straße
                   25  L-HAUSNR       PIC X(010).
      *                Hausnummer
                   25  L-PLZ          PIC 9(005).
      *                Postleitzahl
                   25  L-ORT          PIC X(040).
      *                Ort
               20  ADRESS-ALT.
      *            Alte Adresse des Nachsendeauftrages
                   25  A-VORNAME      PIC X(030).
      *                Vorname
                   25  A-NACHNAME     PIC X(030).
      *                Nachname
                   25  A-ZUSATZ       PIC X(030).
      *                Zusatzfeld
                   25  A-STRASSE      PIC X(022).
      *                Straße
                   25  A-HAUSNR       PIC X(010).
      *                Hausnummer
                   25  A-PLZ          PIC 9(005).
      *                Postleitzahl
                   25  A-ORT          PIC X(040).
      *                Ort
               20  ADRESS-NEU.
      *            Neue Adresse des Nachsendeauftrages
                   25  N-VORNAME      PIC X(030).
      *                Vorname
                   25  N-NACHNAME     PIC X(030).
      *                Nachname
                   25  N-ZUSATZ       PIC X(030).
      *                Zusatzfeld
                   25  N-STRASSE      PIC X(022).
      *                Straße
                   25  N-HAUSNR       PIC X(010).
      *                Hausnummer
                   25  N-PLZ          PIC 9(005).
      *                Postleitzahl
                   25  N-ORT          PIC X(040).
      *                Ort
               20  ABGLEICH1.
      *            Abgleichklassifikatoren Teil 1
                   25  AB101          PIC X(001).
                   25  AB102          PIC X(001).
                   25  AB103          PIC X(001).
                   25  AB104          PIC X(001).
                   25  AB105          PIC X(001).
                   25  AB106          PIC X(001).
                   25  AB107          PIC X(001).
                   25  AB108          PIC X(001).
                   25  AB109          PIC X(001).
                   25  AB110          PIC X(001).
                   25  AB111          PIC X(001).
                   25  AB112          PIC X(001).
                   25  AB113          PIC X(001).
                   25  AB114          PIC 9(003).
               20  ABGLEICH2.
      *            Abgleichklassifikatoren Teil 2
                   25  AB201          PIC 9(003).
                   25  AB202          PIC 9(003).
                   25  AB203          PIC 9(003).
                   25  AB204          PIC 9(003).
                   25  AB205          PIC 9(003).
                   25  AB206          PIC 9(003).
                   25  FILLER         PIC 9(003).
                   25  FILLER         PIC 9(003).

