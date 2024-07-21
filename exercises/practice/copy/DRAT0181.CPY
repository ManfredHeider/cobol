      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0181.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0181.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:30   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:52:58   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   05 Sep 1997 09:21:26   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0181  FUNKTION DELETE EINER NOTIZ        *
      *                           ZU EINER ART VON OBJEKT            *
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
      *    FÜR DIE FUNKTION DRAT0181                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  NOTIZ <<<                                    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRAT0181.
          05 PI-DRAT0181-NTZ-KEY.
             10 PI-DRAT0181-NTZ-ID         PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRAT0181.
          05 PO-DRAT0181-RC                 PIC  X(003).
             88 PO-DRAT0181-RC-OK                        VALUE 'OK '.
      *
      ****** Ende Copy-Member ****************************************
