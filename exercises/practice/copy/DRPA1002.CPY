      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1002.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1002.CPv  $
      *
      *   Rev 2.1   Mar 19 1999 15:47:18   T13EX21
      *APA00012 - MFE-Umstellung 
      *
      *   Rev 2.0   Feb 27 1999 14:29:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   08 Jul 1998 08:51:20   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1002                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF:  VERTRAG LÖSCHEN  VON ZTPA0128            *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GRÜNER                                           *
      *    DATUM:    02.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98! 01 ! NEU                       ! GRÜNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0983                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1002.
          05 PI-DRPA1002-PAEV-KEY.
          COPY DRPA0988.
      *
      *      10 PI-DRPA1002-PRINTE             PIC S9(15)V USAGE COMP-3.
      *      10 PI-DRPA1002-PVER-VERSION       PIC S9(4) USAGE COMP.
      *      10 PI-DRPA1002-PVER-LAST-UPDATE   PIC X(26).
      *
       01 PO-DRPA1002.
          05 PO-DRPA1002-RC                         PIC  X(003).
             88 PO-DRPA1002-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1002-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1002-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER **************************************
