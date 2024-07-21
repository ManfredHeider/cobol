      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0010.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA0010.CPv  $
      *
      *   Rev 2.1   Sep 30 1999 08:41:54   T13EX21
      *FPA00070 - Neuerstellung
      *
      *   Rev 2.0   Feb 27 1999 14:27:12   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Feb 19 1997 10:00:50   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0010  PRÜFFUNKTION, OB PNAP-EINTRAG ZU   *
      *                           EINGEGEBENER PRINTE UND FÄLLIG-VON *
      *                           MIT ZUSTAND FEHLERHAFT VORHANDEN   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    01.09.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * FPA00070!01.09.99!01.01! NEUERSTELLUNG             ! STORCH  *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0010                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0010.
          05 PI-DRPA0010-PRINTE            PIC  S9(015) PACKED-DECIMAL.
          05 PI-DRPA0010-FAELLIG-VON       PIC  X(010).
      *
       01 PO-DRPA0010.
          05 PO-DRPA0010-RC                PIC  X(003).
             88 PO-DRPA0010-RC-OK               VALUE 'OK '.
             88 PO-DRPA0010-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
