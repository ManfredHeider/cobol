      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.05.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0938.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0938.CPv  $
      *
      *   Rev 2.1   May 27 2009 14:16:42   t13ex43
      *AAS00815: PGM - Neukundenbonus
      *
      *   Rev 2.0   May 06 2009 14:53:12   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0938                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0305                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.05.2009                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AAS00815!15.05.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0938 - ERMITTELN NKWBA-SATZ                   *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0938.
           05  PI-DRVB0938-DATEN.
               10  PI-DRVB0938-MA-ST-NR         PIC S9(09)       BINARY.
               10  PI-DRVB0938-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  PI-DRVB0938-NKWBA-ANSPR-VOM  PIC  X(10).
               10  PI-DRVB0938-NKWBA-PART-VBNR  PIC S9(09)       BINARY.
               10  PI-DRVB0938-KNR-ANSPR-GRUND  PIC S9(04)       BINARY.
      *
       01  PO-DRVB0938.
         03  PO-DRVB0938-RC                     PIC  X(03).
               88  PO-DRVB0938-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0938-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0938-DATEN.
           05  PO-DRVB0938-NI-VIVB0305.
               10  NI-DRVB0938-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB0938-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-ANSPR-VOM  PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-PART-VBNR  PIC S9(04)       BINARY.
               10  NI-DRVB0938-KNR-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  NI-DRVB0938-KTY-WB-TYP       PIC S9(04)       BINARY.
               10  NI-DRVB0938-KTY-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-LAST-UPDAT PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-ANZ        PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-EH-EING    PIC S9(04)       BINARY.
               10  NI-DRVB0938-KTY-WAEHRUNG     PIC S9(04)       BINARY.
               10  NI-DRVB0938-KNR-WAEHRUNG     PIC S9(04)       BINARY.
               10  NI-DRVB0938-NKWBA-ANSPR-BETR PIC S9(04)       BINARY.
               10  NI-DRVB0938-KTY-ANSPR-STATUS PIC S9(04)       BINARY.
               10  NI-DRVB0938-KNR-ANSPR-STATUS PIC S9(04)       BINARY.
           05  PO-DRVB0938-DCL-VIVB0305.
               10  PO-DRVB0938-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB0938-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  PO-DRVB0938-NKWBA-ANSPR-VOM  PIC  X(10).
               10  PO-DRVB0938-NKWBA-PART-VBNR  PIC S9(09)       BINARY.
               10  PO-DRVB0938-KNR-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  PO-DRVB0938-KTY-WB-TYP       PIC S9(04)       BINARY.
               10  PO-DRVB0938-KTY-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  PO-DRVB0938-NKWBA-LAST-UPDAT PIC  X(26).
               10  PO-DRVB0938-NKWBA-ANZ        PIC S9(04)       BINARY.
               10  PO-DRVB0938-NKWBA-EH-EING    PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0938-KTY-WAEHRUNG     PIC S9(04)       BINARY.
               10  PO-DRVB0938-KNR-WAEHRUNG     PIC S9(04)       BINARY.
               10  PO-DRVB0938-NKWBA-ANSPR-BETR PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB0938-KTY-ANSPR-STATUS PIC S9(04)       BINARY.
               10  PO-DRVB0938-KNR-ANSPR-STATUS PIC S9(04)       BINARY.
      ****
      **** ENDE COPY DRVB0938 ****************************************
      ****
