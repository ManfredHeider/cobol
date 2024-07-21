      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0635.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0635.CPv  $
      *
      *   Rev 2.0   May 21 1999 13:07:26   T13EX26
      *NT Umstellung und PNAP Änderungen
      *
      *   Rev 1.0   11 Nov 1997 16:56:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0635  FUNKTION PR?FE, OB NOCH EINTR„GE   *
      *                                    IN PNAP ZU EINER PRINTE   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: J„GER-SOKOLUK                                   *
      *    DATUM:    15.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME    *
      * -------------------------------------------------------------*
      * AFPA0012!15.12.97!01.01! NEUERSTELLUNG             ! JÄGER   *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FŠR DIE FUNKTION DRPA0635                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0635.
          05 PI-DRPA0635-PRINTE            PIC S9(015) PACKED-DECIMAL.
      *
       01 PO-DRPA0635.
          05 PO-DRPA0635-RC                PIC  X(003).
             88 PO-DRPA0635-RC-OK               VALUE 'OK '.
          05 PO-DRPA0635-NULL-SCHALTER     PIC X.
      *
      ****** ENDE COPY-MEMBER ****************************************
