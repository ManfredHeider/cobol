      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2876.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2876.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:41:06   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:02:48   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:33:10 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2876                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2876                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:06:20 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2876
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2876.
          05 PI-DRKI2876-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2876-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2876-KEY.
            10 PI-DRKI2876-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2876.
           05  PO-DRKI2876-RC                     PIC  X(003).
             88 PO-DRKI2876-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2876-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2876-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2876-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2876-DATEN.

           10 PO-DRKI2876-VRGVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2876-VRGVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGRISIGRUPPEID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2876-VRGRISIKOHOSTID PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-NI-VRGRISIKOHOSTID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGRISIGRTYP-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGRISIGRTYP-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2876-VRGBEZEICHNUNG PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2876-VRGBEGINNRISGRUP PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2876-N-VRGBGNNRSGRP PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGABLAUFRISGRUP PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2876-N-VRGBLFRSGRP PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGNETTOBEITRAG PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2876-VRGUBERGEOGRUPID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
           10 PO-DRKI2876-VRGDIFDECKKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2876-NI-VRGDIFDECKKZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGGESELLSCH-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGGESELLSCH-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2876-VRGVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2876-NI-VRGVERSNUMMER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2876-N-VRGMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2876-VRGKUENDIGUNGKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2876-NI-VRGKUENDIGUNGKZ PIC S9(4) COMP
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
