      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0191.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0191.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:53:36   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:22:06   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0191  Funktion Insert Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Bohlinger                                       *
      *    DATUM:    08.11.1997                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!08.11.97! 01  !NEUERSTELLUNG             ! BOHLINGER*
      * APA00012!06.02.98!01.02!EINFüHRUNG DRAT0191-UFKT  ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0191                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE TBAT0021 NTZZ              *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0191.
060298    05 PI-DRAT0191-UFKT               PIC X(002).
          05 PI-DRAT0191-NTZZ-KEY.
             10 PI-DRAT0191-NTZ-ID           PIC S9(015) PACKED-DECIMAL.
             10 PI-DRAT0191-NTZZ-ZEILEN-NR   PIC S9(004) BINARY.
          05 PI-DRAT0191-NTZZ-DATEN.
             10 PI-DRAT0191-NTZZ-ZEILE       PIC  X(080).
      *
       01 PO-DRAT0191.
          05 PO-DRAT0191-RC                   PIC  X(003).
             88 PO-DRAT0191-RC-OK                        VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
