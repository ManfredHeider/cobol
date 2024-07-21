      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.10.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1064.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRVB1064.CPv  $
      *
      *   Rev 2.7   Nov 20 2012 14:49:30   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.6   Nov 20 2012 13:28:02   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.5   Nov 20 2012 12:44:42   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.4   Nov 20 2012 12:31:32   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.3   Nov 20 2012 12:07:24   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.2   Nov 20 2012 11:56:22   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.1   Nov 20 2012 11:32:22   t13ae14
      *ASL00462: Versorgungswerk 2012 - F+A-SST
      *
      *   Rev 2.0   Oct 05 2012 15:29:12   t13ae30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1064                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0115                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    05.10.2012                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00466!05.10.12!2.1 ! NEU                       ! WILLIG    *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0915 - ERMITTELN VSWVERS-SATZ                 *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1064.
           05  PI-DRVB1064-DATEN.
               10  PI-DRVB1064-MA-ST-NR         PIC S9(09)       BINARY.
               10  PI-DRVB1064-VERS-NR          PIC S9(15)       COMP-3.
      *
       01  PO-DRVB1064.
         03  PO-DRVB1064-RC                     PIC  X(03).
               88  PO-DRVB1064-RC-OK                 VALUE 'OK '.
               88  PO-DRVB1064-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB1064-DATEN.
           05  PO-DRVB1064-NI-VIVBA115.
               10  NI-DRVB1064-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB1064-VERS-NR          PIC S9(04)       BINARY.
               10  NI-DRVB1064-STATUS-KZ        PIC S9(04)       BINARY.
               10  NI-DRVB1064-VSWV-LAST-UPDATE PIC S9(04)       BINARY.
               10  NI-DRVB1064-KTY-VERS-KAT     PIC S9(04)       BINARY.
               10  NI-DRVB1064-KNR-VERS-KAT     PIC S9(04)       BINARY.
           05  PO-DRVB1064-DCL-VIVBA115.
               10  PO-DRVB1064-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB1064-VERS-NR          PIC S9(15)       COMP-3.
               10  PO-DRVB1064-STATUS-KZ        PIC  X(02).
               10  PO-DRVB1064-VSWV-LAST-UPDATE PIC  X(26).
               10  PO-DRVB1064-KTY-VERS-KAT     PIC S9(04)       BINARY.
               10  PO-DRVB1064-KNR-VERS-KAT     PIC S9(04)       BINARY.
      ****
      **** ENDE COPY DRVB1064 ****************************************
      ****
