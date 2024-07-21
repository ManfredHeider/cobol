      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1163.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA1163.CPv  $
      *
      *   Rev 2.0   May 26 1999 16:02:08   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   21 Oct 1998 14:02:22   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1163  Funktion Update pver/pnap-kz       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: LÜttges                                         *
      *    DATUM:    08.02.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!08.02.99! 01 ! NEUERSTELLUNG             ! LÜttges  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1163                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE PNAP-KZ <<<    TABELLE PVER TBPA0106              *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRPA1163.
          05 PI-DRPA1163-PVER-LAST-UPDATE   PIC  X(026).
          05 PI-DRPA1163-PVER-KEY.
           10 PI-DRPA1163-PRINTE            PIC S9(15)  USAGE COMP-3.
           10 PI-DRPA1163-PVER-VERSION      PIC S9(04)  USAGE COMP.
          05 PI-DRPA1163-PVER-DATEN.
           10 PI-DRPA1163-PVER-PNAP-EINTRAG PIC X(4).
      *
       01 PO-DRPA1163.
          05 PO-DRPA1163-RC                 PIC  X(003).
             88 PO-DRPA1163-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1163-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRPA1163-RC-NOT-FOUND                  VALUE 'NFD'.
             88 PO-DRPA1163-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
