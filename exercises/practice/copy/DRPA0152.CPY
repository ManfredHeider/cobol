      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0152.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0152.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:28:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   27 Aug 1997 17:38:28   T13EX23
      *AFPA0022 - ANPASSUNG RAHMEN\DRIVER
      *
      *   Rev 1.1   Mar 20 1997 17:00:30   T13AE31
      *DRIVER-ANPASSUNG DAISY
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0152                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0152                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BORK                                             *
      *    DATUM:    24.10.1996                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !24.10.96!    ! NEU                       ! BORK     *
      * AFPA0002!20.01.97! 02 ! FELDÄNDERUNGEN, VTSATUS=4 ! AUERBACH *
      *         !        !    ! TEXT = 50 BYTE            !          *
      * AFPA0022!27.08.97!    !NUR PVST-TEXT BEHALTEN     !OPRITZ    *
      *                                                              *
      *                                                              *
      *                                                              *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0152: LESEN TBPA0015  PVST
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0152.
           05  PI-DRPA0152-DATEN.
               10 PI-DRPA0152-PVST-VSTAT       PIC  X(004).
      *
       01  PO-DRPA0152.
           05  PO-DRPA0152-RC                  PIC  X(003).
               88 PO-DRPA0152-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0152-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0152-DATEN.
               10 PO-DRPA0152-PVST-MELDUNG.
                  15 PO-DRPA0152-PVST-TEXT        PIC  X(050).
      *
