      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0488.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0488.CPv  $
      *
      *   Rev 2.1   16 Jun 2000 15:01:08   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   19 May 2000 09:49:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0488                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0076                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. DUERR                                         *
      *    DATUM:    15.06.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00061!15.06.00! 01 ! NEUERSTELLUNG             ! P.DUERR  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0488:                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0488.
           05  PI-DRVB0488-KEY.
               10 PI-DRVB0488-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0488-OBAZ-ANSPR-VOM  PIC  X(010).
      *
       01  PO-DRVB0488.
           05  PO-DRVB0488-RC                 PIC  X(003).
               88 PO-DRVB0488-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0488-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0488-OBAZ-ANSPR-BETRAG
                                        PIC S999999999V99 USAGE COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0476 *****************************

