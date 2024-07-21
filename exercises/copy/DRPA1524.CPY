      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1524.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1524.CPv  $
      *
      *   Rev 2.1   Feb 18 2002 14:25:54   t13ex43
      *FPA00352 : Korrekturbuchung für halbe Einheiten
      *
      *   Rev 2.0   04 Jul 2001 11:11:12   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1524  FUNKTION UPDATE EINHEITEN IN PMPT  *
      *                                       FÜR STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    14.02.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00352!14.02.02!2.1 ! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1524                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  PMPT-EINH   <<<     TABELLE PMPT TBPA0117    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1524.
          05 PI-DRPA1524-PMPT-LAST-UPDATE  PIC  X(026).
          05 PI-DRPA1524-PMPT-KEY.
             10 PI-DRPA1524-PMPT-ID        PIC S9(015)V  PACKED-DECIMAL.
             10 PI-DRPA1524-PMPT-VERSION   PIC S9(004)   BINARY.
          05 PI-DRPA1524-PMPT-DATEN.
             10 PI-DRPA1524-PMPT-EINH      PIC S9(06)V99 PACKED-DECIMAL.
      *
       01 PO-DRPA1524.
          05 PO-DRPA1524-RC                PIC  X(003).
             88 PO-DRPA1524-RC-OK                        VALUE 'OK '.
             88 PO-DRPA1524-RC-CHANGED                   VALUE 'CHA'.
             88 PO-DRPA1524-RC-NOT-FOUND                 VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
