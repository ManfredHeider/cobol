      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2864.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2864.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:39:58   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:01:42   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 11:23:14 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2864                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2864                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:00:00 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2864
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2864.
          05 PI-DRKI2864-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2864-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2864-KEY.
            10 PI-DRKI2864-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2864.
           05  PO-DRKI2864-RC                     PIC  X(003).
             88 PO-DRKI2864-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2864-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2864-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2864-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2864-DATEN.

           10 PO-DRKI2864-HPVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVBEDINGUNG PIC  X(0016)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRISIKO PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVKLAUSELTARIF PIC  X(0080)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVZUSATZINFO PIC  X(0100)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRANSCHRIFTPLZ PIC  X(0006)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRANSCHRIFTSTR PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRANSCHRIFTORT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRANSCHRIFTLKZ PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVMITVERSPERSON PIC  X(0035)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVRANSCHRHAUSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVREGULIERUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVMINDBEITRKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVLFREIVONDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2864-NI-HPVLFREIVONDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVLFREIBISDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2864-NI-HPVLFREIBISDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVSELBSTBKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVSELBSTBPROZ PIC S9(2)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2864-HPVSELBSTBFEST PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVSELBSTBTEXT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVAUSLSCHADKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVVERWSCHADKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVPRODUKT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVNAMELTERNVJKI PIC  X(0045)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVNAMVJKINDERN PIC  X(0045)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVABWEICHENDEVP PIC  X(0035)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVMAILINGAKTION PIC  X(0070)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVDIFFDECKKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2864-N-HPVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2864-HPVGESELLSCHAFT PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVKUENDIGUNGKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2864-HPVZIELGRUPPE PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2864-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
