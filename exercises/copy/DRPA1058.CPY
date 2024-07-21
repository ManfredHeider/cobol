      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1058.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1058.CPv  $
      *
      *   Rev 2.2   Nov 03 2015 09:45:10   T13EX67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e MSS
      *
      *   Rev 2.1   Aug 12 2003 10:01:10   t13ex21
      *APA00245 - Erweiterung PI-Bereich
      *
      *   Rev 2.0   Mar 25 1999 14:09:58   T13AE18
      *APA00012 Einführung EVS-Umbuchungs-
      *dialog
      *
      *   Rev 1.0   14 Jul 1998 14:01:28   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1058 FUNKTION PUT PE-SRUKTUR             *
      *                          FüR PROVISIONSDATENMODUL UGPA0002   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    07.09.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!07.09.98! 01 !NEU                        !BOHLINGER *
      *         !05.03.99!1.02!NEUER RC                   !BOHLINGER *
      * APA00245!05.08.03!2.01!ERWEITERUNG PI-BEREICH     ! STORCH   *
      * APA01932!15.09.15!2.02!ERWEITERUNG PI-BEREICH     ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1058                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1058.
             10 PI-DRPA1058-PRINTE                 PIC S9(015)
                                                   PACKED-DECIMAL.
             10 PI-DRPA1058-S-PE-STRUKTUR-NEU      PIC  X(001).
050803       10 PI-DRPA1058-PVPT-KDBETR-REDUZ      PIC  X(001).
150915       10 PI-DRPA1058-KZ-VB-O-STAT-ENTF      PIC  X(001).

          05 PI-DRPA1058-DATEN.
             COPY DRPA0433.

       01 PO-DRPA1058.
          05 PO-DRPA1058-RC                    PIC  X(003).
             88 PO-DRPA1058-RC-OK                       VALUE 'OK '.
             88 PO-DRPA1058-RC-PE-S-VORHANDEN           VALUE 'PEH'.

          05 PO-DRPA1058-DATEN.
             10 PO-DRPA1058-PEST-ID            PIC S9(015)
                                               PACKED-DECIMAL.
      *
      ******** Ende Copy-Member **************************************

