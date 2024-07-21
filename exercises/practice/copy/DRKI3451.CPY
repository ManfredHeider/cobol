      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.09.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3451.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3451.CPv  $
      *
      *   Rev 2.2   Nov 21 2019 08:31:50   t13ex27
      *AKI01674: DSGVO: Mappen der DB2-Tabelle zu gesperrten/gelösc
      *hten Verträge in die OKI
      *
      *   Rev 2.1   Nov 27 2018 11:54:12   t13ex27
      *AKI01600: M_9000_2a - Prio A - Sperren & Löschen von Verträg
      *en in KI, DBO & VBI-List-OZP für SPERRARCHIV
      *
      *   Rev 2.0   Sep 10 2018 10:51:06 T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3451                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3451                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.09.2018 / 10:51:06 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI3451
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI3451.
          05 PI-DRKI3451-PROGRAMM                 PIC  X(008).
          05 PI-DRKI3451-UFKT-CODE                PIC  9(002).

          05  PI-DRKI3451-KEY.
           10 PI-DRKI3451-SPASTATUS-KNR           PIC S9(4) COMP.
      *
       01  PO-DRKI3451.
           05  PO-DRKI3451-RC                     PIC  X(003).
             88 PO-DRKI3451-RC-OK                        VALUE 'OK '.
             88 PO-DRKI3451-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI3451-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI3451-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI3451-DATEN.

           10 PO-DRKI3451-SPAVERTRAGSID      PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI3451-SPAVERTRAGSNR      PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI3451-SPAPARTGESELLID    PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPALFDNR           PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPASTATUS-KTY      PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPASTATUS-KNR      PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPAGRUND           PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI3451-SPASPERRDAT        PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI3451-SPASPERRDAT-ISO    PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI3451-SPAARCHIVTST       PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI3451-NI-SPAARCHIVTST    PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPALOESCHTST       PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI3451-NI-SPALOESCHTST    PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-SPAENTSPERRTST     PIC  X(0026)
                                   OCCURS 100.
           10 PO-DRKI3451-NI-SPAENTSPERRTST  PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI3451-LETZTEAENDERUNG    PIC  X(0026)
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************


