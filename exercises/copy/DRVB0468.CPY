      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0468.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0468.cpv  $
      *
      *   Rev 2.1   Apr 18 2000 12:43:18   T13AE26
      *ASL00046 Einführung Förderbonus (Dimension 7)
      *
      *   Rev 2.0   Apr 03 2000 14:10:52   P13SP20
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
      *    FÜR DIE FUNKTION DRVB0468:                                *
      *   Datum des Tätigkeitsbeginns als VB und Zeitraum (in Monaten)
      *   ausgehend von übergebenem Stichtag                         *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0468.
           05  PI-DRVB0468-KEY.
               10 PI-DRVB0468-MA-ST-NR        PIC S9(009) BINARY.
               10 PI-DRVB0468-STICHTAG-ISO    PIC  X(010).
      *
       01  PO-DRVB0468.
           05  PO-DRVB0468-RC                 PIC  X(003).
               88 PO-DRVB0468-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0468-RC-NOT-FOUND                 VALUE 'NFD'.
      *
           05  PO-DRVB0468-AV-DAT-VON-ISO     PIC  X(010).
           05  PO-DRVB0468-ANZ-MONATE-VB      PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0468 *****************************
