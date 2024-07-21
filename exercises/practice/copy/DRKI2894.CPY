      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2894.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2894.CPv  $
      *
      *   Rev 2.2   Oct 08 2014 13:42:50   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme 
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:04:32   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:47:16 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2894                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2894                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         08.05.2014 / 14:16:27 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2894
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2894.
          05 PI-DRKI2894-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2894-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2894-KEY.
            10 PI-DRKI2894-VERVERTRAGSNUMMER      PIC  X(015).

      *
       01  PO-DRKI2894.
           05  PO-DRKI2894-RC                     PIC  X(003).
             88 PO-DRKI2894-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2894-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2894-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2894-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2894-DATEN.

           10 PO-DRKI2894-UMVVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2894-UMVVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVPUFVPLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVPUFMVPLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITVPNAME PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITVPVORNAME PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITGESCHL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITGESCHL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITGEBDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2894-NI-UMVMITGEBDAT PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVBEGINN PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2894-NI-UMVBEGINN PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVENDE PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2894-NI-UMVENDE PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2894-UMVMITPARTHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2894-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************
