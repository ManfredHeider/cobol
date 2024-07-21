      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2323.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2323.CPv  $
      *
      *   Rev 2.4   Dec 09 2019 13:54:34   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.3   Jan 11 2012 09:09:14   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.2   Nov 11 2011 14:21:52   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.1   Jul 21 2009 13:33:52   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 11:02:58   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2323                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2323                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.11.2019 / 10:12:09 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2323
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2323.
           05  PI-DRKI2323-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2323-VRTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2323-VRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2323-VRTVERTART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2323-VRTVERTART-KTY   PIC S9(4) COMP.
             10 PI-DRKI2323-VRTVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2323-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2323-N-VRTRSKBRTTWKZ     PIC S9(4) COMP.
             15 PI-DRKI2323-N-VRTRSKBTGBRTT     PIC S9(4) COMP.
             15 PI-DRKI2323-NI-VRTRISKBTGNETTO  PIC S9(4) COMP.
             15 PI-DRKI2323-NI-VRTRISKNETTOWKZ  PIC S9(4) COMP.
             15 PI-DRKI2323-NI-VRTSELBSTBETEIL  PIC S9(4) COMP.
             15 PI-DRKI2323-NI-VRTSFRSTUFE      PIC S9(4) COMP.
             15 PI-DRKI2323-NI-VRTSFRVAR        PIC S9(4) COMP.
             15 PI-DRKI2323-N-VRTVRTRGWGNSKZ    PIC S9(4) COMP.
END   *

           05  PI-DRKI2323-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2323-VRTRISKBRUTTOWKZ   PIC  X(0001).
             10 PI-DRKI2323-VRTRISKBTGBRUTTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2323-VRTRISKBTGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2323-VRTRISKNETTOWKZ   PIC  X(0001).
             10 PI-DRKI2323-VRTSELBSTBETEIL   PIC S9(7)V COMP-3.
             10 PI-DRKI2323-VRTSFRSTUFE   PIC  X(0001).
             10 PI-DRKI2323-VRTSFRVAR   PIC  X(0001).
             10 PI-DRKI2323-VRTVERTRAGWAGNISKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2323-NI1.
           05 PI-DRKI2323-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 8.
       01  PI-DRKI2323-NI7.
           05 PI-DRKI2323-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 8.
      *
       01  PO-DRKI2323.
           05  PO-DRKI2323-RC                     PIC  X(003).
               88 PO-DRKI2323-RC-OK               VALUE 'OK '.
               88 PO-DRKI2323-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************

