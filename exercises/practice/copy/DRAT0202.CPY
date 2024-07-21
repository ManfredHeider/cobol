      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0202.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0202.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:54:02   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0202  FUNKTION DELETE ALLER NOTIZZEILEN  *
      *                           ZU EINER ART VON OBJEKT            *
      *                           STANDARD-OZP                       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                       *
      *    DATUM:    27.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  !AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!27.11.97! 01  !NEUERSTELLUNG             ! BOHLINGER*
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0202                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE-ALL <<< TBAT0021 NTZ                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0202.
          05 PI-DRAT0202-KEY.
             10 PI-DRAT0202-NTZ-ID         PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRAT0202.
          05 PO-DRAT0202-RC                 PIC  X(003).
             88 PO-DRAT0202-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0202-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0202-RC-NOK                       VALUE 'NOK'.
      *
      ****** Ende Copy-Member ****************************************
