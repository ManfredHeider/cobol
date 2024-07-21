      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2349.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2349.CPv  $
      *
      *   Rev 2.7   Nov 24 2020 09:03:52   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.6   Dec 08 2015 12:52:04   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens - DWS
      *
      *   Rev 2.5   Nov 02 2009 08:33:38   t13ex39
      *AKI00844: DWS-Datenspiegel für DWS-Vorsorgeprodukte
      *
      *   Rev 2.4   Oct 27 2009 12:38:36   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.3   Oct 27 2009 12:05:52   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.2   Aug 12 2009 16:11:00   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 11 2009 09:45:42   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2349                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:49 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *---------!--------!--------------------------------!----------*
      * AKI01805!13.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2349
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2349.
           05  PI-DRKI2349-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2349-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2349-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2349-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2349-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2349-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2349-GHPANLAGENAME.
                49 PI-DRKI2349-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI2349-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI2349-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI2349-GHPBRPERFGESBEG   PIC  X(010).
             10 PI-DRKI2349-GHPBRPERFGESEND   PIC  X(010).
             10 PI-DRKI2349-GHPBRPERFLFJBEG   PIC  X(010).
             10 PI-DRKI2349-GHPBRPERFLFJEND   PIC  X(010).
             10 PI-DRKI2349-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI2349-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2349-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI2349-GHPESAFE   PIC  X(0001).
             10 PI-DRKI2349-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI2349-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2349-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI2349-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2349-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI2349-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI2349-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI2349-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI2349-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI2349-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI2349-GHPPERFDAT   PIC  X(010).
             10 PI-DRKI2349-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2349-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2349-GHPPORTFOLIONR   PIC  X(0010).
             10 PI-DRKI2349-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2349-GHPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2349-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2349-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2349-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI2349-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI2349-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI2349-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI2349-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI2349-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2349-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2349-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2349-NI1.
           05 PI-DRKI2349-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI2349-NI7.
           05 PI-DRKI2349-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI2349.
           05  PO-DRKI2349-RC                     PIC  X(003).
               88 PO-DRKI2349-RC-OK               VALUE 'OK '.
               88 PO-DRKI2349-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
