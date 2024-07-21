      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.05.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1173.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1173.CPv  $
      *
      *   Rev 2.1   Jun 01 2017 15:01:30   t13ex57
      *ASL00569: Förderprovision - PGSL0014 (UBO)
      *
      *   Rev 2.0   May 22 2017 15:50:40   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB1173                                    *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0225                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U. Boll                                         *
      *    DATUM:    22.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00569!22.05.17! 2.1! NEU, FPR-ANSPRÜCHE        ! U.Boll   *
      *         !        !    ! JE MONAT                  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB1173                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB1173.
          05 PI-DRVB1173-UFKT-CODE          PIC  9(002).
          05 PI-DRVB1173-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB1173-ANSPR-STATUS       PIC  S9(004) BINARY.
          05 PI-DRVB1173-FPRA-ST-NR         PIC  S9(009) BINARY.
      *
       01 PO-DRVB1173.

          05 PO-DRVB1173-RC                 PIC  X(003).
             88 PO-DRVB1173-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1173-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1173-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1173-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB1173-DATEN.
             10 PO-DRVB1173-FPRA-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB1173-FPRA-ANSPR-BETRAG  PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1173-KNR-WAEHRUNG       PIC  X(003).
             10 PO-DRVB1173-FPRA-ANSP-BET-UPFL PIC S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB1173 *****************************


