      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0857.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0857.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Aug 1998 09:30:52   T13EX16
      *APA00032 - PEW R2.0
      *
      *   Rev 1.0   14 Jan 1998 12:46:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0857                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0857                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    16.02.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !16.02.98!    ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0857
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    FUNKTION: DATUM SETZEN
      ****************************************************************
      *
       01  PI-DRPA0857.
           05  PI-DRPA0857-DATEN.
               10 PI-DRPA0857-DATUM           PIC  X(010).
      *
       01  PO-DRPA0857.
           05  PO-DRPA0857-RC                 PIC  X(003).
               88 PO-DRPA0857-RC-OK                        VALUE 'OK '.
      *
      ******* ENDE COPY-MEMBER **************************************
