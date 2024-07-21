      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2953.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2953.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 15:00:52   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow-OZP 
      *für TCKI0473
      *
      *   Rev 2.0   Jan 19 2015 11:53:01 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2953                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2953                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.02.2015 / 12:10:00 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2953
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2953.
          05 PI-DRKI2953-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2953-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2953-KEY.
            10 PI-DRKI2953-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2953-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2953.
           05  PO-DRKI2953-RC                     PIC  X(003).
             88 PO-DRKI2953-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2953-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2953-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2953-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2953-DATEN.
 
           10 PO-DRKI2953-PVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2953-PVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2953-PVBEDINGUNG-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVBEDINGUNG-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-PVMAILING-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVMAILING-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-PVZUSATZINFO-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVZUSATZINFO-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-PVINFOQUELLE-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVINFOQUELLE-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-PVBAUJAHR PIC  X(0004)
                                   OCCURS 100.
           10 PO-DRKI2953-NI-PVBAUJAHR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVLEISTUNG PIC S9(4)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-NI-PVLEISTUNG PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVBETREIBER PIC  X(0035)
                                   OCCURS 100.
           10 PO-DRKI2953-NI-PVBETREIBER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVBATTERIEKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2953-NI-PVBATTERIEKZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVGEBART-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2953-PVGEBART-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2953-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

