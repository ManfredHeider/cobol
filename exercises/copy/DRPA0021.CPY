      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0021.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0021.CPv  $
      *
      *   Rev 2.2   Sep 19 2006 15:46:58   t13ae34
      *AAN00614: Anpassung PEW an Laufzeit-Produkte
      *
      *   Rev 2.1   Jun 21 2006 10:49:58   t13ae34
      *AAN00614: Anpassung PEW an Laufzeit-Produkte
      *
      *   Rev 2.0   Feb 27 1999 14:27:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 11:35:42   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 14:48:16   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0021  Funktion Read Standard-OZP         *
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
      *         !        !    !                           !          *
      * PEW-REL.!19.05.98! 02 ! ÜBERARBEITUNG PEW-RELEASE ! WOLFF    *
      * 2.0     !        !    ! 2.0                       !          *
      * AAN00614!24.04.06!2.1 ! LAUFZEITABH. PRODUKTE     ! HEIDER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0021                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT-ALL <<<    PEWP TBPA0002                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0021.
          05 PO-DRPA0021-PEWP-KEY.
             10 PI-DRPA0021-PEWA-ID                PIC S9(015)
                                                       PACKED-DECIMAL.
             10 PI-DRPA0021-PGEB-GES               PIC S9(004) BINARY.
             10 PI-DRPA0021-PROD-PROD-SCHL         PIC S9(004) BINARY.
240406       10 PI-DRPA0021-PEWP-TARIF             PIC  X(012).
240406       10 PI-DRPA0021-PEWP-LAUFZEIT          PIC S9(004) BINARY.
      *
       01 PO-DRPA0021.
          05 PO-DRPA0021-RC                        PIC  X(003).
             88 PO-DRPA0021-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0021-RC-NOT-FOUND                 VALUE 'NFD'.
190598    05 PO-DRPA0021-DATEN.
             10 PO-DRPA0021-PEWP-DATEN.
190598          15 PO-DRPA0021-PEW-PRODUKT.
190598             COPY DRPA0960.
                15 PO-DRPA0021-PEWP-LAST-UPDATE    PIC  X(026).
                15 PO-DRPA0021-PEWP-KENNUNG        PIC  X(008).
      *
      ****** Ende Copy-Member ****************************************
