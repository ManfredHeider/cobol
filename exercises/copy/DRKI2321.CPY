      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2321.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2321.CPv  $
      *
      *   Rev 2.4   Dec 09 2019 13:53:06   t13ae31
      *AKI01734: G81 Advocard Nettopreise Stornodatum UPKI0050 OZP
      *
      *   Rev 2.3   Jan 11 2012 09:08:58   t13ex14
      *AKI01079: normalisieren VERTRAGSARTBEZ; DB-Objekte
      *
      *   Rev 2.2   Nov 11 2011 14:21:36   t13ex14
      *AKI01079: DB-Objekte: TBAT0039 / VERTRAGSARTBEZ
      *
      *   Rev 2.1   Jul 21 2009 13:33:40   t13ex14
      *AKI00817: Ausweis von Zusatzinformationen zu Advocard; OZP-O
      *bjekte
      *
      *   Rev 2.0   May 22 2009 11:02:44   t13ex14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2321                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2321                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         12.11.2019 / 10:12:05 / T13AE31
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
      *    FÜR DIE FUNKTION DRKI2321
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2321.
           05  PI-DRKI2321-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2321-VRTLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2321-VRTVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2321-VRTVERTART-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI2321-VRTVERTART-KTY   PIC S9(4) COMP.
             10 PI-DRKI2321-VRTVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2321.
           05  PO-DRKI2321-RC                     PIC  X(003).
               88 PO-DRKI2321-RC-OK               VALUE 'OK '.
               88 PO-DRKI2321-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI2321-NI.
               10 NI-DRKI2321-TAB.
                  15 DRKI2321-ELE
                     PIC S9(04) COMP OCCURS 8 TIMES.
               10 NI-DRKI2321 REDEFINES NI-DRKI2321-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI2321-N-VRTRSKBRTTWKZ   PIC S9(4) COMP.
             15 PO-DRKI2321-N-VRTRSKBTGBRTT   PIC S9(4) COMP.
             15 PO-DRKI2321-NI-VRTRISKBTGNETTO   PIC S9(4) COMP.
             15 PO-DRKI2321-NI-VRTRISKNETTOWKZ   PIC S9(4) COMP.
             15 PO-DRKI2321-NI-VRTSELBSTBETEIL   PIC S9(4) COMP.
             15 PO-DRKI2321-NI-VRTSFRSTUFE   PIC S9(4) COMP.
             15 PO-DRKI2321-NI-VRTSFRVAR   PIC S9(4) COMP.
             15 PO-DRKI2321-N-VRTVRTRGWGNSKZ   PIC S9(4) COMP.
END   *

           05  PO-DRKI2321-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2321-VRTRISKBRUTTOWKZ   PIC  X(0001).
             10 PO-DRKI2321-VRTRISKBTGBRUTTO   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2321-VRTRISKBTGNETTO   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2321-VRTRISKNETTOWKZ   PIC  X(0001).
             10 PO-DRKI2321-VRTSELBSTBETEIL   PIC S9(7)V COMP-3.
             10 PO-DRKI2321-VRTSFRSTUFE   PIC  X(0001).
             10 PO-DRKI2321-VRTSFRVAR   PIC  X(0001).
             10 PO-DRKI2321-VRTVERTRAGWAGNISKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

