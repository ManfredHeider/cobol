      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0510.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0510.CPv  $
      *
      *   Rev 2.1   06 Nov 2000 14:59:54   T13AE30
      *ASL00048 - Neue Zusatzvereinb. wg. "Neuer VB-Vertrag"
      *
      *   Rev 2.0   07 Sep 2000 11:32:12   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0510                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    06.10.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00048!06.11.00!2.1 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0510:                                *
      *   LIEFERT DEN GESAMTEN TUPEL DES AKTUELL GÜLTIGEN EINTRAGS   *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0510.
           05  PI-DRVB0510-KEY.
               10 PI-DRVB0510-MA-ST-NR        PIC S9(009) BINARY.
      *
       01  PO-DRVB0510.
           05  PO-DRVB0510-RC                 PIC  X(003).
               88 PO-DRVB0510-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0510-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0510-AV-TUPEL-GESAMT    PIC  X(131).
      *
      ***** ENDE COPY-MEMBER DRVB0510 *****************************
