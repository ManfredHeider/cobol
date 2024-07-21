      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2892.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2892.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:42:38   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:04:20   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 14:35:43 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2892                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2892                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:15:47 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2892
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2892.
          05 PI-DRKI2892-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2892-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2892-KEY.
            10 PI-DRKI2892-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2892.
           05  PO-DRKI2892-RC                     PIC  X(003).
             88 PO-DRKI2892-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2892-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2892-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2892-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2892-DATEN.

           10 PO-DRKI2892-VRMVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2892-VRMVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2892-VRMRISIGRUPPEID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2892-VRMRISIMERKMALID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2892-VRMEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2892-VRMBEZEICHNUNG PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2892-VRMRISMERKMWERT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2892-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
