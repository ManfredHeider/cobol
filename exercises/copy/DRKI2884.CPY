      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2884.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2884.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 16:37:36   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow OZP 
      *ZMKI0431
      *
      *   Rev 2.2   Oct 08 2014 13:41:52   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:03:34   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:38:15 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2884                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2884                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.02.2015 / 07:49:57 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2884
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2884.
          05 PI-DRKI2884-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2884-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2884-KEY.
            10 PI-DRKI2884-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2884-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2884.
           05  PO-DRKI2884-RC                     PIC  X(003).
             88 PO-DRKI2884-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2884-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2884-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2884-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2884-DATEN.
 
           10 PO-DRKI2884-VZAVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2884-VZAVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2884-VZAUNIVLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2884-VZAEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2884-VZAZUABSCHLPROZ PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2884-VZAZUABSCHLWERT PIC S9(7)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2884-VZAZUABSCHLKZ PIC  X(0003)
                                   OCCURS 100.
           10 PO-DRKI2884-VZAZUABSCHLAGTXT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2884-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
           10 PO-DRKI2884-VZANUMMER PIC S9(9)V COMP-3
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

