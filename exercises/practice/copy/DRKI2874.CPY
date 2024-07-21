      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2874.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2874.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:40:54   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:02:36   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:28:33 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2874                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2874                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:05:34 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2874
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2874.
          05 PI-DRKI2874-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2874-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2874-KEY.
            10 PI-DRKI2874-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2874.
           05  PO-DRKI2874-RC                     PIC  X(003).
             88 PO-DRKI2874-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2874-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2874-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2874-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2874-DATEN.

           10 PO-DRKI2874-VRAVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2874-VRARISIKOHOSTID PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2874-VRARISIKOSTRASSE PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2874-VRARISIKOPLZ PIC  X(0006)
                                   OCCURS 100.
           10 PO-DRKI2874-VRARISIKOORT PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2874-VRARISIKOLKZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2874-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
