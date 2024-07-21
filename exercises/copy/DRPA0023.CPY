      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0023.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0023.CPv  $
      *
      *   Rev 2.1   Aug 23 1999 11:32:28   T13EX21
      *APA00049 - Unterfunktion eingefügt
      *
      *   Rev 2.0   Feb 27 1999 14:27:18   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 14:38:00   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 14:48:24   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0023  Funktion Insert Standard-OZP       *
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
      *         !        !    !                           !          *
      * APA00049!16.08.99! 03 ! UFKT. (FÜR INSERT-QSAM)   ! STORCH   *
      *         !        !    ! EINGEFÜGT                 !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0023                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PEWP TBPA0002              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0023.
160899    05 PI-DRPA0023-UFKT                      PIC  X(002).
190598    05 PI-DRPA0023-DATEN.
             10 PI-DRPA0023-PEWA-ID                PIC S9(015)
                                                       PACKED-DECIMAL.
             10 PI-DRPA0023-PGEB-GES               PIC S9(004) BINARY.
             10 PI-DRPA0023-PEWP-DATEN.
190598          15 PI-DRPA0023-PEW-PRODUKT.
190598             COPY DRPA0960.
      *
       01 PO-DRPA0023.
          05 PO-DRPA0023-RC                        PIC  X(003).
             88 PO-DRPA0023-RC-OK                        VALUE 'OK '.
             88 PO-DRPA0023-RC-DUPLICATE-VALUE           VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
