      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0470.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0470.cpv  $
      *
      *   Rev 2.1   Apr 18 2000 12:43:22   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Apr 03 2000 14:10:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0468                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: T. Moll                                          *
      *    DATUM:    30.03.2000                                       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00046!30.03.00!2.1 ! NEUERSTELLUNG             ! T. Moll  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0470:                                *
      *   Lesen Anzahl der eingetragenen Förderbonusansprüche eines  *
      *   Unterstellten zu einer FBonusstufe                         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0470.
           05  PI-DRVB0470-KEY.
               10 PI-DRVB0470-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0470-KNR-FBONUS      PIC S9(004) BINARY.
      *
       01  PO-DRVB0470.
           05  PO-DRVB0470-RC                 PIC  X(003).
               88 PO-DRVB0470-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0470-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0470-FBAZ-ANSPR-ANZAHL  PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0470 *****************************
