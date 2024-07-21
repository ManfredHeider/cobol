      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0242.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0242.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:54:02   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Oct 1998 17:46:24   T13EX19
      *FKI00010 - Korrektion
      *
      *   Rev 1.0   23 Jun 1998 13:50:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    VERSION   01
      *    Copy Strecke für Restart des PMKI0092 Programmes
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!21.10.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  DRKI0242-RESTART.
           05 DRKI0242-COPY-NAME      PIC X(008)   VALUE 'DRKI0242'.
           05 DRKI0242-RESTART-VB-NR  PIC S9(7) COMP-3 VALUE ZERO.

      * Nummer der VBs deren Betreuer zu korrigieren ist
           05 DRKI0242-KORR-ANZAHL    PIC S9(9) COMP  VALUE ZERO.

      * In Array sind die VB Personen-id und Betreuer-nummer
           05 DRKI0242-ARRAY-KORR.
              10 DRKI0242-ARRAY-KORR-OCC   OCCURS 230.
                 20 DRKI0242-KORR-SATZ              PIC X(17).
