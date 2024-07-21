      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2862.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2862.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:39:46   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:01:30   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 11:20:32 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2862                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2862                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 13:59:23 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2862
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2862.
          05 PI-DRKI2862-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2862-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2862-KEY.
            10 PI-DRKI2862-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2862.
           05  PO-DRKI2862-RC                     PIC  X(003).
             88 PO-DRKI2862-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2862-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2862-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2862-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2862-DATEN.

           10 PO-DRKI2862-GLVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRANSCHRIFTPLZ PIC  X(0006)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRANSCHRIFTSTR PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRANSCHRIFTORT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRANSCHRHAUSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRANSCHRIFTLKZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVKLAUSELTARIF PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVBEDINGUNG PIC  X(0016)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVGEBAEUTYP-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVGEBAEUTYP-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2862-GLVRISIKO PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVMINBEITRAGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVSBPROZENT PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2862-GLVSBFEST PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVLFREIVONDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2862-NI-GLVLFREIVONDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVLFREIBISDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2862-NI-GLVLFREIBISDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVDIFDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2862-N-GLVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2862-GLVGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2862-GLVKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2862-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
