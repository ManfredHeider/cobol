      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0788.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0788.CPv  $
      *
      *   Rev 2.0   May 26 1999 13:15:54   T13EX21
      *APA00012 - Neuerstellung
      *                                   y\
      *   Rev 1.0   12 Dec 1997 09:47:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0788                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  PROVISIONSDATEN EINFüGEN                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    10.01.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!10.01.98! 01 ! NEU                       ! STORCH   *
      *         !        !    !                           !          *
      * Apa00012!15.08.98! 1.1! Buchungsgrund & KZ-Einheit! Parent   *
      *         !        !    !                           !          *
      * apa00012!25.08.98! 1.1! PDPE-POSITIONSNR entfernt ! Parent   *
      *         !        !    ! PDPE-gruppe eingefügt     !          *
      *         !        !    !                           !          *
      * apa00012!29.08.98! 2.0! allgemeine Copystrecke für! Parent   *
      *         !        !    ! PDPT-PDPE DRPA0451 hinzu  !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0788                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0788.
        03 PI-DRPA0788-KOPF.
          05 PI-DRPA0788-PROGRAMM              PIC  X(008).
          05 PI-DRPA0788-UFKT                  PIC  9(002).
        03 PI-DRPA0788-PDPT-PDPE-DATEN.
      *
290898     COPY DRPA0451.
      *
       01 PO-DRPA0788.
          05 PO-DRPA0788-RC                         PIC  X(003).
             88 PO-DRPA0788-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0788-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
