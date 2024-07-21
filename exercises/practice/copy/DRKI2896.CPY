      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2896.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2896.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:43:00   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:04:42   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:48:47 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2896                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2896                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         19.05.2014 / 10:38:59 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2896
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2896.
          05 PI-DRKI2896-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2896-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2896-KEY.
            10 PI-DRKI2896-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2896.
           05  PO-DRKI2896-RC                     PIC  X(003).
             88 PO-DRKI2896-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2896-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2896-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2896-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2896-DATEN.

           10 PO-DRKI2896-UBRVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2896-UBRVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRRGID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRJAHR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRMONAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRMONNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRMONNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRLFDMON PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRLFDMON PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRSTATUS PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRSTATUS PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRERSTDATUM PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRERSTDATUM PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRGARRUEWERT PIC S9(11)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRGARRUEWERT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRGARTODLEIST PIC S9(11)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRGARTODLEIST PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRRUEWERTUEB PIC S9(11)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRRUEWERTUEB PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRTODLEIUEB PIC S9(11)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRTODLEIUEB PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRSTILLERES PIC S9(11)V9(4) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRSTILLERES PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-UBRRECHZINS PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2896-NI-UBRRECHZINS PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2896-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
