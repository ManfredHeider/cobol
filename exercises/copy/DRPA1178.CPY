      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1178.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1178.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:03:48   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:03:44   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1178  Funktion Update Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: LÜTTGES                                         *
      *    DATUM:    24.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!24.02.98! 01 ! NEUERSTELLUNG             ! LÜTTGES  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1178                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  VSTATUS  <<<        TABELLE PMPT TBPA0117    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1178.
          05 PI-DRPA1178-pmpt-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1178-PMPT-KEY.
           10 PI-DRPA1178-PMPT-ID           PIC S9(15)V USAGE COMP-3.
           10 PI-DRPA1178-PMPT-VERSION      PIC S9(4) USAGE COMP.
          05 PI-DRPA1178-PMPR-DATEN.
           10 PI-DRPA1178-PMPT-VSTAT        PIC X(4).
      *
       01 PO-DRPA1178.
          05 PO-DRPA1178-RC                 PIC  X(003).
             88 PO-DRPA1178-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1178-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1178-RC-NOT-FOUND                  VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
