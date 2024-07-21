      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0812.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0812.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 09:21:20   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   14 Jan 1998 12:43:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0812  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    23.01.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.01.98! 01 ! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0812                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE PLOG TBPA0024              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0812.
          05 PI-DRPA0812-PLOG-DATEN.
           10 PI-DRPA0812-KNR-OBJEKT-HKF-TAB      PIC S9(4) USAGE COMP.
           10 PI-DRPA0812-PLOG-VERS-HKF-TAB       PIC S9(4) USAGE COMP.
           10 PI-DRPA0812-PLOG-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA0812-PLOG-VERSION            PIC S9(4) USAGE COMP.
           10 PI-DRPA0812-PLOG-KENNUNG            PIC X(8).
           10 PI-DRPA0812-PLOG-DIALOG-ID          PIC X(4).
           10 PI-DRPA0812-PLOG-AEND-TYP           PIC S9(4) USAGE COMP.
           10 PI-DRPA0812-PLOG-LOG-SATZ.
              49 PI-DRPA0812-PLOG-LOG-SATZ-LEN    PIC S9(4) USAGE COMP.
              49 PI-DRPA0812-PLOG-LOG-SATZ-TEXT   PIC X(3946).
      *
       01 PO-DRPA0812.
          05 PO-DRPA0812-RC                   PIC  X(003).
             88 PO-DRPA0812-RC-OK                         VALUE 'OK '.
             88 PO-DRPA0812-RC-NOK                        VALUE 'NOK'.

