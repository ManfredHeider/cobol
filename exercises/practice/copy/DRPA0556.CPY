      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0556.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0556.CPv  $
      *
      *   Rev 2.1   Mar 05 1999 10:30:50   T13EX21
      *Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:29:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   01 Oct 1997 11:31:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0556 FUNKTION PE-STRUKTUR VERGLEICHEN    *
      *                          FÜR UWPA0005 (FKPAV042)             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    04.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  ! VER ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * APA00012!04.03.99! 01  ! NEU                       ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0556                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0556.
          05 PI-DRPA0556-PEST-NEU.
             COPY DRPA0433.
          05 PI-DRPA0556-VERGLEICHS-PEST.
             COPY DRPA0433.

       01 PO-DRPA0556.
          05 PO-DRPA0556-RC                    PIC  X(003).
             88 PO-DRPA0556-RC-PEST-IDENTISCH           VALUE 'PID'.
             88 PO-DRPA0556-RC-PEST-GLEICH              VALUE 'PGL'.
             88 PO-DRPA0556-RC-PEST-UNGLEICH            VALUE 'PUG'.


