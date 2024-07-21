      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2878.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2878.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 16:40:20   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow OZP 
      *ZMKI0422
      *
      *   Rev 2.2   Oct 08 2014 13:41:16   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:03:00   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:34:11 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2878                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2878                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.02.2015 / 07:48:18 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2878
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2878.
          05 PI-DRKI2878-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2878-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2878-KEY.
            10 PI-DRKI2878-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2878-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2878.
           05  PO-DRKI2878-RC                     PIC  X(003).
             88 PO-DRKI2878-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2878-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2878-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2878-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2878-DATEN.
 
           10 PO-DRKI2878-SGWVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2878-SGWVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2878-SGWRGID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2878-SGWPSGBLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2878-SGWEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2878-SGWBEDKLAUSELNR PIC  X(0020)
                                   OCCURS 100.
           10 PO-DRKI2878-SGWVERTBTEILTXT PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2878-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

