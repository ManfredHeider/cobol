      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.12.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3218.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI3218.CPv  $
      *
      *   Rev 2.2   Mar 21 2018 13:52:50   t13ac17
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.1   Feb 19 2018 07:41:50   t13ac20
      *AKI01534: G20 - Projekt LENA: DB2-Zugriffe
      *
      *   Rev 2.0   Dec 06 2017 09:08:26   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI3218                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3218                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01534!06.12.17!2.1 ! NEUERSTELLUNG             ! SWO      *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI3218
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: SELECT (PRODUKTID UND STVD0067)
      *
      ****************************************************************
      *
       01  PI-DRKI3218.
           05 PI-DRKI3218-KEY.
              10 PI-DRKI3218-PKTPRODKLAS-KTY
                                            PIC S9(004) BINARY.
              10 PI-DRKI3218-PKTPRODKLAS-KNR
                                            PIC S9(015) PACKED-DECIMAL.
              10 PI-DRKI3218-PKTGSMTTARIF-BEZ
                                            PIC  X(025).
      *
       01  PO-DRKI3218.
           05 PO-DRKI3218-RC                PIC  X(003).
              88 PO-DRKI3218-RC-OK                      VALUE 'OK '.
              88 PO-DRKI3218-RC-NOT-FOUND               VALUE 'NFD'.
      *
           05 PO-DRKI3218-NI.
              10 NI-DRKI3218-TAB.
                 15 DRKI3218-ELE            PIC S9(004) BINARY
                                            OCCURS 2 TIMES.
              10 NI-DRKI3218 REDEFINES NI-DRKI3218-TAB.
                 15 PO-DRKI3218-NI-PKTSTVD0067-KTY
                                            PIC S9(004) BINARY.
                 15 PO-DRKI3218-NI-PKTSTVD0067-KNR
                                            PIC S9(004) BINARY.
      *
           05 PO-DRKI3218-DATEN.
              10 PO-DRKI3218-PKTPRODUKTID   PIC S9(004) BINARY.
              10 PO-DRKI3218-PKTSTVD0067-KTY
                                            PIC S9(004) BINARY.
              10 PO-DRKI3218-PKTSTVD0067-KNR
                                            PIC S9(015) PACKED-DECIMAL.
      *
      ******* ENDE COPY-MEMBER ***************************************
 

