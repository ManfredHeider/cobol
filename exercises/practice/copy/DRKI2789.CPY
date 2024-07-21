      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2789.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2789.CPv  $
      *
      *   Rev 2.2   Apr 04 2014 11:25:22   T13Ex77
      *FKI01460: Fehlerdateien und Fehlermails DWS
      *
      *   Rev 2.1   Mar 11 2014 15:48:28   t13ex77
      *AKI01269: Tabelle DBPRODUKT -> Analog OKI.Kreditkarten_Eigen
      *
      *   Rev 2.0   Nov 12 2013 13:15:46   t13ex77
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *
      * Eingabedaten
      *
       01 PI-DRKI2789.
          05 PI-DRKI2789-DATEN.
      *      Zu verarbeitende Gesellschaft
             10 PI-DRKI2789-GESELLSCHAFT            PIC  9(02).
      *         Deutsche Bank
                88 PI-DRKI2789-GES-DEUBA              VALUE 48.
      *      Kennzeichen, welche Verarbeitung durchgeführt wird
             10 PI-DRKI2789-VERARB-KZ               PIC  X(01).
      *         Insert eines neuen Vertrags
                88 PI-DRKI2789-INSERT                 VALUE 'I'.
      *         Delete eines bestehenden Vertrags
                88 PI-DRKI2789-DELETE                 VALUE 'D'.
      *      Zu verarbeitende Vertragsnummer
             10 PI-DRKI2789-VERTRAGSID              PIC  X(13).
      *      Tagesdatum als einzusetzendes Löschdatum
             10 PI-DRKI2789-TAGESDATUM              PIC  X(10).
      *      Bisheriger Inhalt des Bündel-Kennzeichens
             10 PI-DRKI2789-BUENDEL-KZ-ALT          PIC  X(01).
      *      Bisheriger Inhalt des Löschdatums
             10 PI-DRKI2789-LOESCH-DAT-ALT          PIC  X(10).
      *
      * Ausgabedaten
      *
       01 PO-DRKI2789.
      * Rückgabe-Returncode
          05 PO-DRKI2789-RC                         PIC  X(003).
      *      Verarbeitung war erfolgreich
             88 PO-DRKI2789-RC-OK                     VALUE 'OK '.
      *      Es wurden keine zu verarbeitende Daten gefunden
             88 PO-DRKI2789-RC-NFD                    VALUE 'NFD'.
      *      Schwerer Fehler aus UPRO
             88 PO-DRKI2789-RC-ERR                    VALUE 'ERR'.
      * Rückgabedaten
          05 PO-DRKI2789-DATEN.
      *      Kennzeichen, ob sich das Bündel-Kz geändert hat
             10 PO-DRKI2789-AEND-BUENDEL-KZ         PIC  X(001).
                88 PO-DRKI2789-AEND-BUENDEL-JA        VALUE 'J'.
                88 PO-DRKI2789-AEND-BUENDEL-NEIN      VALUE 'N'.
      *      Kennzeichen, ob sich das Lösch-Datum geändert hat
             10 PO-DRKI2789-AEND-LOESCH-DAT         PIC  X(001).
                88 PO-DRKI2789-AEND-LOESCH-JA         VALUE 'J'.
                88 PO-DRKI2789-AEND-LOESCH-NEIN       VALUE 'N'.
      *      Anzahl der Produkte des Vertrags
             10 PO-DRKI2789-ANZ-PRODUKTE            PIC S9(004).
