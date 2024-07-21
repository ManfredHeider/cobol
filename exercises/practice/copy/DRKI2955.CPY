      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.01.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2955.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2955.CPv  $
      *
      *   Rev 2.1   Mar 18 2015 15:05:00   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 - MultiRow-OZP 
      *für TCKI0474
      *
      *   Rev 2.0   Jan 19 2015 11:53:35 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2955                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2955                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         04.02.2015 / 12:10:46 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2955
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2955.
          05 PI-DRKI2955-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2955-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2955-KEY.
            10 PI-DRKI2955-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2955-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2955.
           05  PO-DRKI2955-RC                     PIC  X(003).
             88 PO-DRKI2955-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2955-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2955-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2955-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2955-DATEN.
 
           10 PO-DRKI2955-FRVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2955-FRVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVRSKGRUPPEID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVTHEMGSCHL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVTHEMGSCHL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2955-FRVUNIVLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2955-FRVDIFFGEF-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVDIFFGEF-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2955-FRVDIFFGES-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVDIFFGES-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2955-FRVVERSNR PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2955-NI-FRVVERSNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVUMSTELLTERMIN PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2955-N-FRVMSTLLTRMN PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2955-NI-FRVKUENDIGUNGKZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-FRVRSKMERKMAL PIC S9(9)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2955-NI-FRVRSKMERKMAL PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2955-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

