      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.10.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1124.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1124.CPv  $
      *
      *   Rev 2.1   Nov 11 2015 15:41:00   t13ae35
      *ASL00544: VSW 2015 - ZWVB0020
      *
      *   Rev 2.0   Oct 22 2015 10:49:22   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1124                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1124                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    22.10.2015                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00544!22.10.15! 01 ! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1124
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1124.
           05  PI-DRVB1124-BES-KEY.
               10 PI-DRVB1124-BES-VB-NR       PIC  S9(009) COMP.
               10 PI-DRVB1124-BES-GRUND-1     PIC  S9(004) COMP.
               10 PI-DRVB1124-BES-GRUND-2     PIC  S9(004) COMP.
               10 PI-DRVB1124-DAT-VON         PIC  X(10).
      *
       01  PO-DRVB1124.
           05  PO-DRVB1124-RC                 PIC  X(003).
               88 PO-DRVB1124-RC-OK                        VALUE 'OK '.
               88 PO-DRVB1124-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB1124-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB1124-BES-BESCHRAENKUNG  PIC  X.
               88  PO-DRVB1124-BESCHRAENKUNG               VALUE 'J'.
               88  PO-DRVB1124-K-BESCHRAENKUNG             VALUE 'N'.
      ******* ENDE COPY-MEMBER **************************************

