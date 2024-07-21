      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2888.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2888.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:42:16   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:03:58   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:40:55 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2888                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2888                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:14:15 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2888
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2888.
          05 PI-DRKI2888-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2888-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2888-KEY.
            10 PI-DRKI2888-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2888.
           05  PO-DRKI2888-RC                     PIC  X(003).
             88 PO-DRKI2888-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2888-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2888-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2888-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2888-DATEN.

           10 PO-DRKI2888-VLTVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2888-VLTVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2888-VLTRISIGRUPPEID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2888-VLTLEISTUNGID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2888-VLTEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2888-VLTTHEMGSCHL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2888-VLTTHEMGSCHL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2888-VLTBEZEICHNUNG PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2888-VLTWERT PIC S9(9)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2888-VLTSONSTWERT PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2888-VLTSONSTWERTEINH PIC  X(0020)
                                   OCCURS 100.
           10 PO-DRKI2888-VLTLEISTUNGSTXT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2888-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
