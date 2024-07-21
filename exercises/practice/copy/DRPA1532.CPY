      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1532.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRPA1532.CPv  $
      *
      *   Rev 2.1   Apr 16 2002 15:04:24   T13ex22
      *FPA00346: Änderungen für CRPA0108
      *
      *   Rev 2.0   04 Jul 2001 11:11:22   p13sp13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1532                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG LÖSCHEN OVW
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                           *
      *    DATUM:    11.03.2002                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00346!11.03.02! 01 ! NEU                       ! MUND   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1532                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1532.
          05 PI-DRPA1532-PVER-KEY.
             10 PI-DRPA1532-PRINTE             PIC S9(15)V USAGE COMP-3.
             10 PI-DRPA1532-PVER-VERSION       PIC S9(4) USAGE COMP.
             10 PI-DRPA1532-PVER-LAST-UPDATE   PIC X(26).
      *
       01 PO-DRPA1532.
          05 PO-DRPA1532-RC                         PIC  X(003).
             88 PO-DRPA1532-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1532-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1532-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
