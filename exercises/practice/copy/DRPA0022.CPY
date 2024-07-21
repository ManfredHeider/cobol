      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0022.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0022.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 11:50:22   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 14:48:20   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0022  Funktion Update Standard-OZP       *
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
      *         !        !    !                           !          *
      * PEW-REL.!19.05.98! 02 ! ÜBERARBEITUNG PEW-RELEASE ! WOLFF    *
      * 2.0     !        !    ! 2.0                       !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0022                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       PEWP TBPA0002                      *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA0022.
190598    05 PI-DRPA0022-DATEN.
             10 PI-DRPA0022-LAST-UPDATE            PIC  X(026).
             10 PI-DRPA0022-PEWA-ID                PIC S9(015)
                                                       PACKED-DECIMAL.
             10 PI-DRPA0022-PGEB-GES               PIC S9(004) BINARY.
             10 PI-DRPA0022-PEWP-DATEN.
190598          15 PI-DRPA0022-PEW-PRODUKT.
190598             COPY DRPA0960.
      *
       01 PO-DRPA0022.
          05 PO-DRPA0022-RC                        PIC  X(003).
             88 PO-DRPA0022-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0022-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA0022-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** Ende CopyMember *****************************************
