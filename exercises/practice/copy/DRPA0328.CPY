      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0328.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0328.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 14:27:24   u130076
      *APA02838: Programm zur Automatisierung von Daueranforderungen 
      *
      *   Rev 2.0   Feb 27 1999 14:28:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:53:32   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   06 Jun 1997 09:38:02   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0323  Funktionen Standard-OZP            *
      *                           PDIF TBPA0104                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J. Opritz                                       *
      *    DATUM:    09.06.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.06.97! 01 ! Neuerstellung             ! Opritz   *
      * APA02838!26.08.19! 02 ! NEUE FUNKTIONEN DEL, INS  ! KÖPRÜCÜ  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ztpa0104.
           05 PI-ztpa0104-FUNKTION            PIC X(008).
              88 PI-ztpa0104-DRPA0329-INS         VALUE 'DRPA0329'.
              88 PI-ztpa0104-DRPA0330-UPD         VALUE 'DRPA0330'.
              88 PI-ztpa0104-DRPA0331-DEL         VALUE 'DRPA0331'.
              88 PI-ztpa0104-DRPA0332-SEL-ALL     VALUE 'DRPA0332'.
              88 PI-ztpa0104-DRPA1934-DEL-DTA     VALUE 'DRPA1934'.
              88 PI-ztpa0104-DRPA1935-INS-DTA     VALUE 'DRPA1935'.

