      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1535.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1535.CPv  $
      *
      *   Rev 2.1   Apr 16 2002 15:04:28   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.0   04 Jul 2001 11:11:26   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1535  FUNKTION DELETE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: MUND                                          *
      *    DATUM:    11.03.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!11.03.02! 01 ! NEUERSTELLUNG             ! MUND   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1535                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DELETE  <<<       TABELLE PMBT TBPA0117              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1535.
          05 PI-DRPA1535-LAST-UPDATE          PIC  X(026).
          05 PI-DRPA1535-PMPT-KEY.
           10 PI-DRPA1535-PMPT-ID             PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA1535-PMPT-VERSION        PIC S9(4) USAGE COMP.
      *
       01 PO-DRPA1535.
          05 PO-DRPA1535-RC                   PIC  X(003).
             88 PO-DRPA1535-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1535-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1535-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************

