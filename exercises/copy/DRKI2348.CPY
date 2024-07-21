      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2348.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2348.CPv  $
      *
      *   Rev 2.8   Nov 24 2020 09:03:36   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.7   Dec 08 2015 12:51:58   t13ex73
      *AKI01385: Korrektur der Darstellung des Eigenvermitteltkennz
      *eichens - DWS
      *
      *   Rev 2.6   Nov 02 2009 08:33:30   t13ex39
      *AKI00844: DWS-Datenspiegel für DWS-Vorsorgeprodukte
      *
      *   Rev 2.5   Oct 27 2009 12:38:30   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.4   Oct 27 2009 12:05:44   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.3   Aug 12 2009 16:10:52   t13ex39
      *AKI00844: DWS-Datenspiegel D E P O T
      *
      *   Rev 2.0   Aug 11 2009 09:45:26   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2348                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2348                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:47 / U130685
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
      *    FÜR DIE FUNKTION DRKI2348
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2348.
           05  PI-DRKI2348-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2348-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2348-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2348-NI.

ANF   *        %M_PI_DATEN_NI%
             15 FILLER                          PIC S9(4) COMP.
END   *

           05  PI-DRKI2348-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2348-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI2348-GHPANLAGENAME.
                49 PI-DRKI2348-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI2348-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI2348-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI2348-GHPBRPERFGESBEG   PIC  X(010).
             10 PI-DRKI2348-GHPBRPERFGESEND   PIC  X(010).
             10 PI-DRKI2348-GHPBRPERFLFJBEG   PIC  X(010).
             10 PI-DRKI2348-GHPBRPERFLFJEND   PIC  X(010).
             10 PI-DRKI2348-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI2348-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2348-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI2348-GHPESAFE   PIC  X(0001).
             10 PI-DRKI2348-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI2348-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2348-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI2348-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2348-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI2348-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI2348-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI2348-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI2348-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI2348-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI2348-GHPPERFDAT   PIC  X(010).
             10 PI-DRKI2348-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2348-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI2348-GHPPORTFOLIONR   PIC  X(0010).
             10 PI-DRKI2348-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI2348-GHPPROVISIONKZ   PIC  X(0001).
             10 PI-DRKI2348-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI2348-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2348-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI2348-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI2348-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI2348-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI2348-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI2348-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI2348-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2348-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2348-NI1.
           05 PI-DRKI2348-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI2348-NI7.
           05 PI-DRKI2348-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI2348.
           05  PO-DRKI2348-RC                     PIC  X(003).
               88 PO-DRKI2348-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2348-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
