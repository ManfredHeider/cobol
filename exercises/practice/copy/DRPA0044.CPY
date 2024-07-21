      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0044.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0044.CPv  $
      *
      *   Rev 2.1   Jan 04 2000 13:04:46   T13AE18
      *APA00068
      *
      *   Rev 2.0   Feb 27 1999 14:27:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:51:04   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0044  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    14.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !14.08.96! 01 ! Neuerstellung             ! Priggen  *
      * APA00068!18.11.99!2.01!NEUES KEY-FELD: PROD-BEGINN! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0044                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PROD TBPA0004                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0044.
          05 PI-DRPA0044-LAST-UPDATE         PIC  X(026).
          05 PI-DRPA0044-PROD-KEY.
             10 PI-DRPA0044-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0044-PROD-PROD-SCHL   PIC S9(004) BINARY.
181199       10 PI-DRPA0044-PROD-BEGINN      PIC  X(010).
      *
       01 PO-DRPA0044.
          05 PO-DRPA0044-RC                  PIC  X(003).
             88 PO-DRPA0044-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0044-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0044-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende-Copy-Member ****************************************
