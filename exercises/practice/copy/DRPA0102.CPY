      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0102.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0102.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 14:45:10   T01JM2S
      *APAA0032 - PEW-Release 2.0
      *
      *   Rev 1.1   Mar 06 1997 14:58:14   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0102  Funktion Antrag-TDS Umsetzen       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    21.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !21.08.96! 01 ! Neuerstellung             ! Priggen  *
      *         !        !    !                           !          *
      * Daisy 1 !        !    !                           !          *
      * PEW-Rel.!23.01.97! 02 ! Überarbeitung PEW-Release ! Wolff    *
      *         !        !    !                           !          *
      * PEW-REL.!14.05.98! 03 ! ÜBERARBEITUNG PEW-RELEASE ! WOLFF    *
      * 2.0     !        !    ! 2.0                       !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0102                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            DEN ALLGEMEINEN ANTRAGS-TDS-DATEN UU021S          *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0102.
          05 PI-DRPA0102-PZVP-VERSION             PIC S9(004) BINARY.
      *
       01 PO-DRPA0102.
          05 PO-DRPA0102-RC                       PIC  X(003).
             88 PO-DRPA0102-RC-OK                      VALUE 'OK '.
             88 PO-DRPA0102-RC-NICHT-RELEVANT          VALUE 'NRV'.
          05 PO-DRPA0102-DATEN.
             10 PO-DRPA0102-PEW-ANTRAG.
140598          COPY DRPA0954.
      *
      ******** Ende Copy-Member **************************************
