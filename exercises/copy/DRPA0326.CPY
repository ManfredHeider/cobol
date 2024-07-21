      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0326.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0326.CPv  $
      *
      *   Rev 2.1   Nov 06 2006 10:39:40   t13ex25
      *APA00452: Erweiterung des PS auf 3 Stellen
      *
      *   Rev 2.0   Feb 27 1999 14:28:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:52:20   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:37:38   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPa0326  Funktion DELETE Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Opritz                                          *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! NEUERSTELLUNG             ! Opritz   *
      * AFPA0022!03.09.97!1.3 !-PROV-SATZ ->-PG-PROV-STZ  !JAN OPRITZ*
      *         !        !    !-PROV-GESAMT->-PG-GSMT-PROV!          *
      * APA00452!06.11.06!2.1 ! Umstellung 3-stell. Prod. ! Jäger    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0326                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE      TBPA0103 PMPP         *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0326.
          05 PI-DRPA0326-PMPP-LAST-UPDATE     PIC X(026).
          05 PI-DRPA0326-PMPP-KEY.
             10 PI-DRPA0326-PMPP-ID           PIC S9(015) COMP-3.
             10 PI-DRPA0326-PMPG-ID           PIC S9(015) COMP-3.
             10 PI-DRAP0326-PGEB-GES          PIC S9(004) COMP-3.
061106*      10 PI-DRPA0326-PROD-PROD-SCHL    PIC S9(004) COMP-3.
061106       10 PI-DRPA0326-PROD-PROD-SCHL    PIC S9(004) COMP.
      *
       01 PO-DRPA0326.
          05 PO-DRPA0326-RC                   PIC  X(003).
             88 PO-DRPA0326-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0326-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA0326-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
