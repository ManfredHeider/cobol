      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0476.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0476.CPv  $
      *
      *   Rev 2.1   22 May 2000 14:09:30   T13AE27
      *neuer Funktionencopy für ZWVB0076
      *
      *   Rev 2.0   Apr 03 2000 14:11:04   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0476                                      *
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
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0476:                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0476.
           05  PI-DRVB0476-KEY.
               10 PI-DRVB0476-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0476-OBAZ-ANSPR-VOM  PIC  X(007).
      *
       01  PO-DRVB0476.
           05  PO-DRVB0476-RC                 PIC  X(003).
               88 PO-DRVB0476-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0476-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0476-OBAZ-ANSPR-BETRAG
                                        PIC S999999999V99 USAGE COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0476 *****************************
