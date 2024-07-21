      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0475.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0475.CPv  $
      *
      *   Rev 2.1   Mar 15 1999 16:37:52   T13EX22
      *MFE-UMSTELLUNG
      *
      *   Rev 2.0   Feb 27 1999 14:29:04   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:41:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0475                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PMPT AUS VORH. PMPT >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    21.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!21.11.97! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0475                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA0475.
          05 PI-DRPA0475-DATEN.
             10 PI-DRPA0475-PMPR.
                COPY DRPA0412.
             10 PI-DRPA0475-PMPT.
                COPY DRPA0572.
      *
       01 PO-DRPA0475.
          05 PO-DRPA0475-RC                         PIC  X(003).
             88 PO-DRPA0475-RC-OK                         VALUE 'OK '.
          05 PO-DRPA0475-DATEN.
             10 PO-DRPA0475-PMPT.
                COPY DRPA0572.
      *
      ******* ENDE COPY-MEMBER **************************************
