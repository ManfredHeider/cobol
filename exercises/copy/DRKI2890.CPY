      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2890.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2890.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:42:26   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:04:10   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:42:09 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2890                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2890                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:14:58 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2890
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2890.
          05 PI-DRKI2890-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2890-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2890-KEY.
            10 PI-DRKI2890-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2890.
           05  PO-DRKI2890-RC                     PIC  X(003).
             88 PO-DRKI2890-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2890-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2890-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2890-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2890-DATEN.

           10 PO-DRKI2890-VSBVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2890-VSBVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2890-VSBRISIGRUPPEID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2890-VSBSELBSTBETID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2890-VSBEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2890-VSBBEZEICHNUNG PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2890-VSBWERTPROZENT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2890-VSBWERT PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2890-VSBMINDESTBEITR PIC S9(5)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2890-VSBHOECHSTBEITR PIC S9(5)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2890-VSBSELBSTBTEXT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2890-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
