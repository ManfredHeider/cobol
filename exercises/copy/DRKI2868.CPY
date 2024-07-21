      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2868.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2868.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:40:20   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:02:04   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 11:26:25 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2868                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2868                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:01:21 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2868
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2868.
          05 PI-DRKI2868-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2868-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2868-KEY.
            10 PI-DRKI2868-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2868.
           05  PO-DRKI2868-RC                     PIC  X(003).
             88 PO-DRKI2868-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2868-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2868-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2868-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2868-DATEN.

           10 PO-DRKI2868-UNFVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFBEDINGUNG PIC  X(0016)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFRISIKO PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFKLAUSELTARIF PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFANZVERSPERSON PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFLEISTFREIVDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2868-N-NFLSTFRVDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFLEISTFREIBDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2868-N-NFLSTFRBDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFSELBSTBKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFSELBSTBPROZ PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2868-UNFSELBSTBFEST PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFSELBSTBTEXT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFMINDBEITRAGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2868-N-NFMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2868-UNFGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2868-UNFKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2868-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
