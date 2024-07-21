      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0024.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0024.CPv  $
      *
      *   Rev 2.2   Sep 19 2006 15:51:18   t13ae34
      *AAN00614: Anpassung PEW an Laufzeit-Produkte
      *
      *   Rev 2.1   Jun 21 2006 10:51:10   t13ae34
      *AAN00614: Anpassung PEW an Laufzeit-Produkte
      *
      *   Rev 2.0   Feb 27 1999 14:27:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 14:48:30   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0024  Funktion Delete Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    13.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * Daisy 1 !13.08.96! 01 ! Neuerstellung             ! Priggen  *
      * AAN00614!24.04.06!2.1 ! LAUFZEITABH. PRODUKTE     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0024                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       PEWP TBPA0002                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0024.
          05 PI-DRPA0024-LAST-UPDATE         PIC  X(026).
          05 PO-DRPA0024-PEWP-KEY.
             10 PI-DRPA0024-PEWA-ID          PIC S9(015) PACKED-DECIMAL.
             10 PI-DRPA0024-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA0024-PROD-PROD-SCHL   PIC S9(004) BINARY.
240406       10 PI-DRPA0024-PEWP-TARIF       PIC  X(012).
240406       10 PI-DRPA0024-PEWP-LAUFZEIT    PIC S9(004) BINARY.
      *
       01 PO-DRPA0024.
          05 PO-DRPA0024-RC                  PIC  X(003).
             88 PO-DRPA0024-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0024-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0024-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
