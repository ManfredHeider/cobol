      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2882.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2882.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:41:40   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:03:24   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:36:42 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2882                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2882                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:12:12 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2882
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2882.
          05 PI-DRKI2882-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2882-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2882-KEY.
            10 PI-DRKI2882-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2882.
           05  PO-DRKI2882-RC                     PIC  X(003).
             88 PO-DRKI2882-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2882-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2882-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2882-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2882-DATEN.

           10 PO-DRKI2882-VGSVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2882-VGSVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2882-VGSPVGSBLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2882-VGSEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBART PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBBEZ PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBPRZ PIC  X(0014)
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBMIN PIC S9(5)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBMAX PIC S9(5)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBFEST PIC S9(5)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2882-VGSSELBSTBTEXT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2882-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
