      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *     DATUM   :  NOVEMBER 1995
      *     KOPF AUS:  N:PROFILEKOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0030.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0030.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:18:20   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   FEB 27 1999 14:27:20   P13SP20
      *NT-UMSTELLUNG
      *
      *   REV 1.1   MAR 06 1997 14:49:08   P13PVCS
      *NEUERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0030  FUNKTIONEN STANDARD-OZP            *
      *                           PKON TBPA0007                      *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: PRIGGEN                                         *
      *    DATUM:    20.08.1996                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !20.08.96! 01 ! NEUERSTELLUNG             ! PRIGGEN  *
      * APA00820!17.09.09!2.1 ! DRPA1680 HINZU            ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZTPA0007.
           05 PI-ZTPA0007-FUNKTION                PIC X(008).
              88 PI-ZTPA0007-DRPA0054-INS         VALUE 'DRPA0054'.
              88 PI-ZTPA0007-DRPA0055-UPD         VALUE 'DRPA0055'.
              88 PI-ZTPA0007-DRPA0056-DEL         VALUE 'DRPA0056'.
              88 PI-ZTPA0007-DRPA0057-SEL-ALL     VALUE 'DRPA0057'.
              88 PI-ZTPA0007-DRPA1680-SEL-RUC     VALUE 'DRPA1680'.
