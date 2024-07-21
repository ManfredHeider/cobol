      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2860.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2860.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:39:34   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:01:20   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 10:53:10 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2860                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2860                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 13:56:42 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2860
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2860.
          05 PI-DRKI2860-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2860-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2860-KEY.
            10 PI-DRKI2860-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2860.
           05  PO-DRKI2860-RC                     PIC  X(003).
             88 PO-DRKI2860-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2860-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2860-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2860-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2860-DATEN.

           10 PO-DRKI2860-FKVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2860-FKVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVTARIFPRODUKT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVBEDINGUNG PIC  X(0020)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2860-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
           10 PO-DRKI2860-FKVGESELLSCH-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2860-FKVGESELLSCH-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2860-FKVVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2860-NI-FKVVERSNUMMER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2860-FKVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2860-N-FKVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2860-FKVKUENDIGUNGKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2860-NI-FKVKUENDIGUNGKZ PIC S9(4) COMP
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
