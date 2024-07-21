      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0516.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0516.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:10   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.2   06 Aug 1998 15:09:38   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.1   20 Oct 1997 17:23:42   T13EX22
      *AFPA0018 - Ein- und Ausgabeparameter
      *
      *   Rev 1.0   12 Sep 1997 16:59:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0516  INSERT                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: T. MUND                                         *
      *    DATUM:    22.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA....!22.09.97! 01 ! Neuerstellung             ! T. MUND  *
      *         !        !    !                           !          *
      * PEW-REL.!25.05.98! 02 ! ÄNDERUNG TABELLE PEWAH    ! WOLFF    *
      * 2.0     !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0516                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT <<<                                           *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0516.
          05 PI-DRPA0516-UFKT               PIC  9(002).
          05 PI-DRPA0516-DATEN              PIC  X(587).
      *
       01 PO-DRPA0516.
          05 PO-DRPA0516-RC                 PIC  X(003).
             88 PO-DRPA0516-RC-OK           VALUE 'OK '.
      *
      ***** Ende-Copy-Member *****************************************
