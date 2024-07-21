      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2872.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2872.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:40:42   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:02:26   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:27:17 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2872                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2872                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:04:53 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2872
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2872.
          05 PI-DRKI2872-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2872-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2872-KEY.
            10 PI-DRKI2872-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2872.
           05  PO-DRKI2872-RC                     PIC  X(003).
             88 PO-DRKI2872-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2872-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2872-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2872-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2872-DATEN.

           10 PO-DRKI2872-RGPVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2872-RGPVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2872-RGPBEDINGUNG-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPBEDINGUNG-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2872-RGPVERSSUMME PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPMAILING-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPMAILING-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2872-RGPZUSINFO-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPZUSINFO-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2872-RGPDIFDECKKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2872-NI-RGPDIFDECKKZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPGESELLSCH-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPGESELLSCH-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2872-RGPVERSNUMMER PIC  X(0040)
                                   OCCURS 100.
           10 PO-DRKI2872-NI-RGPVERSNUMMER PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPVUMSTELLUNGDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2872-N-RGPVMSTLLNGDT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-RGPKUENDIGUNGKZ PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2872-NI-RGPKUENDIGUNGKZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2872-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
