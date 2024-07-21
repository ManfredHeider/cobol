      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2322.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2322.CPv  $
      *
      *   Rev 2.4   Dec 09 2019 13:53:50   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.3   Jan 11 2012 09:09:06   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.2   Nov 11 2011 14:21:44   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.1   Jul 21 2009 13:33:46   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 11:02:50   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2322                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2322                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.11.2019 / 10:12:07 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2322
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2322.
           05  PI-DRKI2322-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI2322-VRTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2322-VRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2322-VRTVERTART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2322-VRTVERTART-KTY   PIC S9(4) COMP.
             10 PI-DRKI2322-VRTVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI2322-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI2322-N-VRTRSKBRTTWKZ     PIC S9(4) COMP.
             15 PI-DRKI2322-N-VRTRSKBTGBRTT     PIC S9(4) COMP.
             15 PI-DRKI2322-NI-VRTRISKBTGNETTO  PIC S9(4) COMP.
             15 PI-DRKI2322-NI-VRTRISKNETTOWKZ  PIC S9(4) COMP.
             15 PI-DRKI2322-NI-VRTSELBSTBETEIL  PIC S9(4) COMP.
             15 PI-DRKI2322-NI-VRTSFRSTUFE      PIC S9(4) COMP.
             15 PI-DRKI2322-NI-VRTSFRVAR        PIC S9(4) COMP.
             15 PI-DRKI2322-N-VRTVRTRGWGNSKZ    PIC S9(4) COMP.
END   *

           05  PI-DRKI2322-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI2322-VRTRISKBRUTTOWKZ   PIC  X(0001).
             10 PI-DRKI2322-VRTRISKBTGBRUTTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2322-VRTRISKBTGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI2322-VRTRISKNETTOWKZ   PIC  X(0001).
             10 PI-DRKI2322-VRTSELBSTBETEIL   PIC S9(7)V COMP-3.
             10 PI-DRKI2322-VRTSFRSTUFE   PIC  X(0001).
             10 PI-DRKI2322-VRTSFRVAR   PIC  X(0001).
             10 PI-DRKI2322-VRTVERTRAGWAGNISKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI2322-NI1.
           05 PI-DRKI2322-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 8.
       01  PI-DRKI2322-NI7.
           05 PI-DRKI2322-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 8.

      *
       01  PO-DRKI2322.
           05  PO-DRKI2322-RC                     PIC  X(003).
               88 PO-DRKI2322-RC-OK                  VALUE 'OK '.
               88 PO-DRKI2322-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

