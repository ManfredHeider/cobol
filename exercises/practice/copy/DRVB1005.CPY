      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.01.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1005.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1005.CPv  $
      *
      *   Rev 2.1   Feb 01 2011 16:50:34   t13ex13
      *ASL00367: SL-Dialog für Leistungszuschuss Allfinanz
      *
      *   Rev 2.0   Jan 31 2011 16:56:56   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1005                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0305                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    31.01.2011                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00367!31.01.11!2.2 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1005 - ERMITTELN NKWBA-SATZ. DIE ERMITTLUNG   *
      *    ERFOLGT OHNE BERÜCKSICHTIGUNG DES ANSPR_GRUND (KEY)        *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1005.
           05  PI-DRVB1005-DATEN.
               10  PI-DRVB1005-MA-ST-NR         PIC S9(09)       BINARY.
               10  PI-DRVB1005-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  PI-DRVB1005-NKWBA-ANSPR-VOM  PIC  X(10).
               10  PI-DRVB1005-NKWBA-PART-VBNR  PIC S9(09)       BINARY.
      *
       01  PO-DRVB1005.
         03  PO-DRVB1005-RC                     PIC  X(03).
               88  PO-DRVB1005-RC-OK                 VALUE 'OK '.
               88  PO-DRVB1005-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB1005-DATEN.
           05  PO-DRVB1005-NI-VIVB0305.
               10  NI-DRVB1005-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB1005-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-ANSPR-VOM  PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-PART-VBNR  PIC S9(04)       BINARY.
               10  NI-DRVB1005-KNR-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  NI-DRVB1005-KTY-WB-TYP       PIC S9(04)       BINARY.
               10  NI-DRVB1005-KTY-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-LAST-UPDAT PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-ANZ        PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-EH-EING    PIC S9(04)       BINARY.
               10  NI-DRVB1005-KTY-WAEHRUNG     PIC S9(04)       BINARY.
               10  NI-DRVB1005-KNR-WAEHRUNG     PIC S9(04)       BINARY.
               10  NI-DRVB1005-NKWBA-ANSPR-BETR PIC S9(04)       BINARY.
               10  NI-DRVB1005-KTY-ANSPR-STATUS PIC S9(04)       BINARY.
               10  NI-DRVB1005-KNR-ANSPR-STATUS PIC S9(04)       BINARY.
           05  PO-DRVB1005-DCL-VIVB0305.
               10  PO-DRVB1005-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB1005-KNR-WB-TYP       PIC S9(04)       BINARY.
               10  PO-DRVB1005-NKWBA-ANSPR-VOM  PIC  X(10).
               10  PO-DRVB1005-NKWBA-PART-VBNR  PIC S9(09)       BINARY.
               10  PO-DRVB1005-KNR-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  PO-DRVB1005-KTY-WB-TYP       PIC S9(04)       BINARY.
               10  PO-DRVB1005-KTY-ANSPR-GRUND  PIC S9(04)       BINARY.
               10  PO-DRVB1005-NKWBA-LAST-UPDAT PIC  X(26).
               10  PO-DRVB1005-NKWBA-ANZ        PIC S9(04)       BINARY.
               10  PO-DRVB1005-NKWBA-EH-EING    PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1005-KTY-WAEHRUNG     PIC S9(04)       BINARY.
               10  PO-DRVB1005-KNR-WAEHRUNG     PIC S9(04)       BINARY.
               10  PO-DRVB1005-NKWBA-ANSPR-BETR PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1005-KTY-ANSPR-STATUS PIC S9(04)       BINARY.
               10  PO-DRVB1005-KNR-ANSPR-STATUS PIC S9(04)       BINARY.
      ****
      **** ENDE COPY DRVB1005 ****************************************
      ****
