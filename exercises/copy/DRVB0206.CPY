      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0206.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0206.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:24   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 10:39:40   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:15:30   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0206  Funktion Kleinstes Verarbeitungs-  *
      *                           Datum zu Steuer-Info aus ZPKT erm. *
      *    --------------------------------------------------------- *
      *    ERSTELLT: JÄGER-SOKOLUK                                   *
      *    DATUM:    05.12.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !05.12.98!01.01! NEUERSTELLUNG            ! JÄGER    *
      *         !        !     !                          !          *
      * APA00012!20.05.98!01.02! PI/PO-Bereich umbenamt   ! JÄGER    *
      *         !        !     ! RC-NFD eingefügt         !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0206                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0206.
          05 PI-DRVB0206-DATEN.
             10 PI-DRVB0206-KNR-STEUER-INFO  PIC S9(4) BINARY.
      *
       01 PO-DRVB0206.
          05 PO-DRVB0206-RC                  PIC  X(003).
             88 PO-DRVB0206-RC-OK            VALUE 'OK '.
             88 PO-DRVB0206-RC-NOT-FOUND     VALUE 'NFD'.
          05 PO-DRVB0206-DATEN.
             10 PO-DRVB0206-MIN-STEUER-DAT   PIC X(010).
      *
      ******** Ende Copy-Member **************************************
