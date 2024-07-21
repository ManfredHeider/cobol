      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0477.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRVB0477.CPv  $
      *
      *   Rev 2.2   29 Nov 2000 11:42:46   T13AE28
      *ASL00055: Füllen des Gesamttupels beim Fetch
      *
      *   Rev 2.1   22 May 2000 14:09:52   T13AE27
      *neue Funktion für ZWVB0076
      *
      *   Rev 2.0   Apr 03 2000 14:11:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0477                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0076                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH. WILLIG                                       *
      *    DATUM:    05.05.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00054!05.05.00! 01 ! NEUERSTELLUNG             ! TH.WILLIG*
      * ASL00055!18.10.00! 02 ! Änderung Tupel-Länge      ! S.WALB   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0477:                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0477.
           05  PI-DRVB0477-KEY.
               10 PI-DRVB0477-MA-ST-NR          PIC S9(009) BINARY.
               10 PI-DRVB0477-OBAZ-ANSPR-VOM    PIC  X(007).
      *
       01  PO-DRVB0477.
           05  PO-DRVB0477-RC                   PIC  X(003).
               88 PO-DRVB0477-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0477-RC-NOT-FOUND                 VALUE 'NFD'.
      *
          05 PO-DRVB0477-DATEN.
181000*      10 PO-DRVB0477-OBAZ-TUPEL-GESAMT   PIC  X(54).
181000       10 PO-DRVB0477-OBAZ-TUPEL-GESAMT   PIC  X(70).
      *
      ***** ENDE COPY-MEMBER DRVB0477 *****************************
