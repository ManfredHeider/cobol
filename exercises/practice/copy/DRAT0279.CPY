      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0279.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0279.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   21 Jan 1999 16:15:28   T13AE34
      *AKI00005 - Restart
      *
      *   Rev 1.0   13 Jul 1998 09:28:42   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAT0279                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<JOBNAME LESEN       >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    07.09.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!07.09.98! 01 ! NEU                       ! WOLFF    *
      * AKI00005!21.01.99!1.1 !                           ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0279                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRAT0279.
          05 PI-DRAT0279-DATEN.
             10 PI-DRAT0279-FUNKTION                PIC  X(001).
      *
       01 PO-DRAT0279.
          05 PO-DRAT0279-DATEN.
             10 PO-DRAT0279-JOBNAME                 PIC  X(008).
             10 PO-DRAT0279-JOBNAME-R REDEFINES PO-DRAT0279-JOBNAME.
                15 PO-DRAT0279-JOB-HV               PIC  X(001).
                15 FILLER                           PIC  X(007).
             10 PO-DRAT0279-PROCNAME                PIC  X(008).
             10 PO-DRAT0279-STEPNAME                PIC  X(008).
      *
      ******* ENDE COPY-MEMBER **************************************
